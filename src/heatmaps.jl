"""
Graphs for showing a 2D matrix.
"""
module Heatmaps

export AverageLinkage
export CompleteLinkage
export heatmap_graph
export heatmap_order
export HeatmapAxisConfiguration
export HeatmapAxisData
export HeatmapBottomLeft
export HeatmapBottomRight
export HeatmapGraph
export HeatmapGraphConfiguration
export HeatmapGraphData
export HeatmapGraphOrder
export HeatmapLinkage
export HeatmapOrigin
export HeatmapReorder
export HeatmapTopLeft
export HeatmapTopRight
export OptimalHclust
export RCompatibleHclust
export reset_order!
export ReorderHclust
export SameOrder
export SingleLinkage
export SlantedHclust
export SlantedOrder
export SlantedPreSquaredHclust
export SlantedPreSquaredOrder
export WardLinkage
export WardPreSquaredLinkage

using ..Common
using ..Utilities
using ..Validations

using Clustering
using Distances
using PlotlyBase
using Slanter

import ..Bars.push_annotations_traces!
import ..Bars.push_plotly_annotation!
import ..Bars.expand_vector
import ..Validations.Maybe

"""
Specify how to reorder the rows and/or columns.

  - `OptimalHclust` orders `hclust` branches using the (better) Bar-Joseph method.
  - `RCompatibleHclust` orders `hclust` branches in the same (bad) way that `R` does.
  - `ReorderHclust` reorders `hclust` branches to be as close as possible to a given order (using `reorder_hclust`).
  - `SlantedHclust` and `SlantedPreSquaredHclust` orders `hclust` branches using `Slanter` (using `slanted_orders`
    and `reorder_hclust`).
  - `SlantedOrder` and `SlantedPreSquaredOrder` uses `slanted_orders` (if a tree is needed, uses `ehclust` to create
    a tree preserving this order).
  - `SameOrder` orders the rows/columns in the same way as the other axis. This can only be applied to square matrices
    and can't be specified for both axes.
"""
@enum HeatmapReorder RCompatibleHclust OptimalHclust ReorderHclust SlantedHclust SlantedPreSquaredHclust SlantedOrder SlantedPreSquaredOrder SameOrder

"""
Specify the linkage to use when performing hierarchical clustering (`hclust` / `ehclust`). The default is `WardLinkage`.
"""
@enum HeatmapLinkage SingleLinkage AverageLinkage CompleteLinkage WardLinkage WardPreSquaredLinkage

"""
Specify where the origin (row 1 column 1) should be displayed. The Plotly default is `HeatmapBottomLeft`.
"""
@enum HeatmapOrigin HeatmapTopLeft HeatmapTopRight HeatmapBottomLeft HeatmapBottomRight

"""
    struct HeatmapGraphOrder
        rows_order::AbstractVector{<:Integer}
        rows_hclust::Maybe{Hclust}
        columns_order::AbstractVector{<:Integer}
        columns_hclust::Maybe{Hclust}
    end

The computed final order and clustering of the rows and the columns of a heatmap graph, as returned by
[`heatmap_order`](@ref).

  - `rows_order` is the order of the rows of the data, that is, the index of the original row shown at each position.
    This is always a permutation of `1:n_rows`, which for an axis that isn't reordered at all is the identity.
  - `rows_hclust` is the tree the rows were clustered by, or `nothing` if they weren't clustered (they were left alone,
    given an explicit order, or slanted without a tree).
  - `columns_order` and `columns_hclust` are the same for the columns.

These describe the order of the data, not the order it is displayed in; applying the `origin` is up to whoever shows
the graph, as is skipping the hidden rows and columns (the order and the tree include them).
"""
struct HeatmapGraphOrder
    rows_order::AbstractVector{<:Integer}
    rows_hclust::Maybe{Hclust}
    columns_order::AbstractVector{<:Integer}
    columns_hclust::Maybe{Hclust}
end

"""
    @kwdef mutable struct HeatmapAxisConfiguration <: Validated
        title::Maybe{AbstractString} = nothing
        annotations::AnnotationSize = AnnotationSize()
        reorder::Maybe{HeatmapReorder} = nothing
        linkage::Maybe{HeatmapLinkage} = nothing
        metric::Maybe{PreMetric} = nothing
        groups_gap::Maybe{Integer} = 1
        subgroups_gap::Maybe{Integer} = nothing
        dendogram_size::Maybe{Real} = nothing
        dendogram_line::LineConfiguration = LineConfiguration()
    end

Configure one axis (the rows or the columns) of a heatmap. The `title` is the title of the axis. The `annotations` are
the sizes of the annotations shown to the side of the axis.

You can use `reorder` to reorder the entries of the axis. When specifying `linkage`, by default, the clustering uses the
`Euclidean` distance metric. You can override this by specifying the `metric`.

If groups are specified for the entries in the [`HeatmapAxisData`](@ref), they can be used to constrain the clustering,
and/or to create visible gaps in the heatmap (between entries of different groups). The `groups_gap` is the number of
fake entries to added between the separated entries. That is, the default gap of 1 will add a blank gap of one entry
between adjacent entries of different groups. A gap of `nothing` will not be shown.

If subgroups are also specified, they are a second, finer level of grouping nested in the groups; each group is
contiguous, and within it each subgroup is contiguous. Their `subgroups_gap` works the same way, and defaults to
`nothing` because the usual reason to specify subgroups is to constrain the clustering rather than to show gaps.

Each level is placed independently: a level specified by numbers is laid out in the order of these numbers, and a level
specified by names is laid out by the clustering. Numbering both levels therefore lays the entries out in the order of
their (group, subgroup) pair, and numbering just the groups keeps the groups in a fixed order while clustering the
subgroups inside each of them.

If you specify `dendogram_size`, then you should either specify linkage (for computing a clustering) or must specify
`Hclust` order in the data. The dendogram tree will be shown to the side of the data. The size is specified in the usual
inconvenient units (fractions of the total graph size) because Plotly.

If a dendogram tree is shown, the `dendogram_line` can be used to control it. The default color is black. The
`is_filled` field shouldn't be set as it has no meaning here.
"""
@kwdef mutable struct HeatmapAxisConfiguration <: Validated
    title::Maybe{AbstractString} = nothing
    annotations::AnnotationSize = AnnotationSize()
    reorder::Maybe{HeatmapReorder} = nothing
    linkage::Maybe{HeatmapLinkage} = nothing
    metric::Maybe{PreMetric} = nothing
    groups_gap::Maybe{Integer} = 1
    subgroups_gap::Maybe{Integer} = nothing
    dendogram_size::Maybe{Real} = nothing
    dendogram_line::LineConfiguration = LineConfiguration()
end

function Validations.validate(context::ValidationContext, configuration::HeatmapAxisConfiguration)::Nothing
    validate_field(context, "annotations", configuration.annotations)
    validate_field(context, "dendogram_line", configuration.dendogram_line)

    validate_in(context, "groups_gap") do
        return validate_is_above(context, configuration.groups_gap, 0)
    end
    validate_in(context, "subgroups_gap") do
        return validate_is_above(context, configuration.subgroups_gap, 0)
    end
    validate_in(context, "dendogram_size") do
        return validate_is_above(context, configuration.dendogram_size, 0)
    end

    dendogram_line = configuration.dendogram_line
    if dendogram_line.is_filled
        throw(ArgumentError("can't specify heatmap $(location(context)).dendogram_line.is_filled"))
    end

    if configuration.dendogram_size === nothing &&
       (dendogram_line.width !== nothing || dendogram_line.style != SolidLine || dendogram_line.color !== nothing)
        throw(ArgumentError(chomp("""
                                  can't specify heatmap $(location(context)).dendogram_line.*
                                  without $(location(context)).dendogram_size
                                  """)))
    end

    return nothing
end

"""
    @kwdef mutable struct HeatmapGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        entries_colors::ColorsConfiguration = ColorsConfiguration()
        rows::HeatmapAxisConfiguration = HeatmapAxisConfiguration()
        columns::HeatmapAxisConfiguration = HeatmapAxisConfiguration()
        origin::HeatmapOrigin = HeatmapBottomLeft
        final_order::Maybe{HeatmapGraphOrder} = nothing
    end

Configure a graph showing a heatmap.

This displays a matrix of values using a rectangle at each position. Due to Plotly's limitations, you still to manually
tweak the graph size for best results; there's no way to directly control the width and height of the rectangles. In
addition, the only supported color configurations are using continuous color palettes.

The `rows` and `columns` configure each axis (see [`HeatmapAxisConfiguration`](@ref)).

The `final_order` caches the computed order of the rows and the columns; access it through the graph's `order` (e.g.,
for generating other graphs in an identical order). It is computed once, whether the graph's figure is generated or its
order is asked for first.

!!! note

    Nothing detects that the cache went stale. Call [`reset_order!`](@ref) if anything it was computed from is changed
    after it was computed - that is, the `reorder`, `linkage` and `metric` of the axes configuration, and the
    `entries.values` and the `order`, `arrange_by`, `groups` and `subgroups` of the axes data. The groups are easy to
    forget: they constrain the clustering, so saving the same graph twice, grouped differently each time, silently
    reuses the order of the first grouping unless the cache is reset in between.
"""
@kwdef mutable struct HeatmapGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    entries_colors::ColorsConfiguration = ColorsConfiguration()
    rows::HeatmapAxisConfiguration = HeatmapAxisConfiguration()
    columns::HeatmapAxisConfiguration = HeatmapAxisConfiguration()
    origin::HeatmapOrigin = HeatmapBottomLeft
    final_order::Maybe{HeatmapGraphOrder} = nothing
end

function Validations.validate(context::ValidationContext, configuration::HeatmapGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "entries_colors", configuration.entries_colors)
    validate_field(context, "rows", configuration.rows)
    validate_field(context, "columns", configuration.columns)

    if configuration.entries_colors.fixed !== nothing
        throw(ArgumentError("can't specify heatmap $(location(context)).entries_colors.fixed"))
    end

    if configuration.entries_colors.palette isa CategoricalColors
        throw(ArgumentError("can't specify heatmap categorical $(location(context)).entries_colors.palette"))
    end

    if configuration.rows.reorder == SameOrder && configuration.columns.reorder == SameOrder
        throw(ArgumentError(chomp("""
                                  can't specify both heatmap $(location(context)).rows.reorder: SameOrder
                                  and heatmap $(location(context)).columns.reorder: SameOrder
                                  """)))
    end

    return nothing
end

"""
    @kwdef mutable struct HeatmapAxisData
        names::ValuesData = ValuesData()
        entities::EntitiesData = EntitiesData()
        order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
        groups::Maybe{AbstractVector} = nothing
        subgroups::Maybe{AbstractVector} = nothing
        arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
        annotations::AbstractVector{AnnotationData} = AnnotationData[]
    end

The data of one axis (the rows or the columns) of a [`HeatmapGraphData`](@ref). The `names` are strings, one per entry,
shown as the tick labels; their title is the axis title. The `entities` hold the hovers and mask of the entries. The
`annotations` are shown to the side of the axis.

By default, if reordering the entries, this is based on the `entries.values` of the graph. You can override this by
specifying an `arrange_by` matrix. Only the reordered dimension needs to match the `entries.values` (the rows
`arrange_by` must have the same number of rows, and the columns `arrange_by` the same number of columns); the other
dimension holds whatever features you want to cluster by, and need not match. For efficiency the rows `arrange_by`
matrix should be in row-major layout, but that's not critical.

Alternatively you can force the order of the entries by specifying the `order` permutation. You can also specify an
`Hclust` object as the order. If you ask for a dendogram and did not specify such a clustering, one will be computed.

If `groups` are specified, then a gap can be added between entries of different groups. Groups can also be used to
constrain the computed clustering. The `subgroups` are a second, finer level of grouping nested in the groups.

Hidden entries (see the mask of [`EntitiesData`](@ref)) are not drawn, but they are still part of the data: the
clustering sees them, and the `order` (a permutation or a tree) always describes all the entries, hidden ones included.
This way the order computed for one graph (see [`heatmap_order`](@ref)) can be given to another graph of the same data,
whether or not the two hide the same entries. At least one entry must be shown.
"""
@kwdef mutable struct HeatmapAxisData
    names::ValuesData = ValuesData()
    entities::EntitiesData = EntitiesData()
    order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
    groups::Maybe{AbstractVector} = nothing
    subgroups::Maybe{AbstractVector} = nothing
    arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
    annotations::AbstractVector{AnnotationData} = AnnotationData[]
end

# Validate the data of the `name` (rows or columns) axis of a heatmap with `n_entries`. The context is that of the whole
# graph data, so the messages can refer to the entries.
function Validations.validate(
    context::ValidationContext,
    axis::HeatmapAxisData,
    name::AbstractString,
    n_entries::Integer,
)::Nothing
    base = "entries.values.$(name)"

    validate_string_values(context, "$(name).names.values", axis.names.values)
    validate_vector_length(context, "$(name).names.values", axis.names.values, base, n_entries)

    validate_vector_length(context, "$(name).entities.hovers", axis.entities.hovers, base, n_entries)
    validate_vector_length(context, "$(name).entities.mask", axis.entities.mask, base, n_entries)
    if axis.entities.mask !== nothing && !any(axis.entities.mask)
        throw(ArgumentError("all entries hidden by $(location(context)).$(name).entities.mask"))
    end

    if axis.order isa Hclust
        order = axis.order.order
    else
        order = axis.order
    end
    validate_vector_length(context, "$(name).order", order, base, n_entries)

    validate_vector_length(context, "$(name).groups", axis.groups, base, n_entries)
    validate_vector_length(context, "$(name).subgroups", axis.subgroups, base, n_entries)

    # The subgroups of an axis are a second, finer level of grouping, so they only make sense together with the groups.
    # A subgroup is nested in its group, so the same subgroup in two different groups is two different subgroups;
    # there's no need for the subgroups to be unique.
    if axis.subgroups !== nothing && axis.groups === nothing
        throw(ArgumentError("can't specify heatmap $(location(context)).$(name).subgroups without $(name).groups"))
    end

    validate_matrix_dimension(context, "$(name).arrange_by", axis.arrange_by, name == "rows" ? 1 : 2, base, n_entries)

    validate_vector_entries(context, "$(name).annotations", axis.annotations) do _, annotation
        validate(context, annotation, base, n_entries)
        return nothing
    end

    return nothing
end

"""
    @kwdef mutable struct HeatmapGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        entries::MatrixData = MatrixData()
        cells::MatrixEntitiesData = MatrixEntitiesData()
        rows::HeatmapAxisData = HeatmapAxisData()
        columns::HeatmapAxisData = HeatmapAxisData()
    end

The data for a graph showing a heatmap (matrix) of entries.

This is shown as a 2D image where each matrix entry is a small rectangle with some color. Due to Plotly limitation,
colors must be continuous. The `entries` values are required; their title is the title of the colors scale. The `cells`
hold the hovers and mask of the entries. The hover for each rectangle is a combination of the hovers of the cell, of its
row and of its column. Hidden cells are drawn as gaps; their values are still part of the data (they must be valid, they
take part in the clustering, and in the range of the colors scale unless `include_hidden` is disabled in the
`entries_colors.axis`). At least one cell must be shown.

The `rows` and `columns` hold the data of each axis (see [`HeatmapAxisData`](@ref)).

Valid combinations of the fields controlling order and clustering are:

| data `order`                | data `arrange_by`                   | data `groups` | config `reorder`                           | config `dendogram_size` | config `linkage` | config `metric` | result tree                                                                                      | result order                         | notes                                                  |
|:--------------------------- |:----------------------------------- |:------------- |:------------------------------------------ |:----------------------- |:---------------- |:--------------- |:------------------------------------------------------------------------------------------------ |:------------------------------------ |:------------------------------------------------------ |
| `nothing`                   | `nothing`                           | ignored       | `nothing`                                  | `nothing`               | `nothing`        | `nothing`       | Not computed                                                                                     | Original data order                  | Do not cluster, use the original data order (default)  |
| `nothing`                   | `nothing`/ `AbstractMatrix{<:Real}` | ignored       | `nothing`                                  | Any                     | `nothing`/ Any   | `nothing`/ Any  | `ehclust` of original order with `linkage` or `WardLinkage`                                      | Original data order                  | Cluster, preserving the original order                 |
| `nothing`                   | `nothing`                           | ignored       | `SameOrder`                                | `nothing`/ Any          | `nothing`        | `nothing`       | Same as other axis                                                                               | Same as other axis                   | Square matrices only                                   |
| `nothing`                   | `nothing`/ `AbstractMatrix{<:Real}` | `nothing`     | `OptimalHclust`/ `RCompatibleHclust`       | `nothing`/ Any          | `nothing`/ Any   | `nothing`/ Any  | `hclust` with `linkage` or `WardLinkage`                                                         | `hclust` with `reorder`              | Cluster using `linkage` and branch `reorder`           |
| `nothing`                   | `nothing`/ `AbstractMatrix{<:Real}` | Any           | `OptimalHclust`/ `RCompatibleHclust`       | `nothing`/ Any          | `nothing`/ Any   | `nothing`/ Any  | `ehclust` with `groups` and `linkage` or `WardLinkage`                                           | `hclust` with `groups` and `reorder` | Cluster using `groups`, `linkage` and branch `reorder` |
| `nothing`                   | `nothing`/ `AbstractMatrix{<:Real}` | `nothing`     | `SlantedHclust`/ `SlantedPreSquaredHclust` | `nothing`/ Any          | `nothing`/ Any   | `nothing`/ Any  | `hclust` with `linkage` or `WardLinkage`, then `reorder_hclust` by `slanted_orders`              | `reorder_hclust` by `slanted_orders` | Cluster, then slant preserving the tree                |
| `nothing`                   | `nothing`/ `AbstractMatrix{<:Real}` | Any           | `SlantedHclust`/ `SlantedPreSquaredHclust` | `nothing`/ Any          | `nothing`/ Any   | `nothing`/ Any  | `hclust` with `groups` and `linkage` or `WardLinkage`, then `reorder_hclust` by `slanted_orders` | `reorder_hclust` by `slanted_orders` | Cluster using `groups`, then slant preserving the tree |
| `nothing`                   | `nothing`/ `AbstractMatrix{<:Real}` | ignored       | `SlantedOrder`/ `SlantedPreSquaredOrder`   | `nothing`/ Any          | `nothing`/ Any   | `nothing`/ Any  | `ehclust` of `slanted_orders` with `linkage` or `WardLinkage`                                    | `slanted_orders`                     | Slant, then cluster preserving the slanted order       |
| `Hclust`                    | `nothing`                           | ignored       | `nothing`                                  | `nothing`/ Any          | `nothing`        | `nothing`       | `Hclust` tree                                                                                    | `Hclust` order                       | Force a specific tree and order on the data            |
| `Hclust`                    | `nothing`/ `AbstractMatrix{<:Real}` | ignored       | `SlantedHclust`/ `SlantedPreSquaredHclust` | `nothing`/ Any          | `nothing`        | `nothing`       | `reorder_hclust` by `slanted_orders`                                                             | `reorder_hclust` by `slanted_orders` | Slant, preserving a given tree                         |
| `AbstractVector{<:Integer}` | `nothing`                           | ignored       | `nothing`                                  | `nothing`               | `nothing`        | `nothing`       | Not computed                                                                                     | `order` permutation                  | Do not cluster, use the specified order                |
| `AbstractVector{<:Integer}` | `nothing`                           | ignored       | `nothing`                                  | Any                     | `nothing`/ Any   | `nothing`/ Any  | `ehclust` of `order` with `linkage` or `WardLinkage`                                             | `order` permutation                  | Cluster, preserving the specified order                |
| `AbstractVector{<:Integer}` | `nothing`                           | `nothing`     | `ReorderHclust`                            | `nothing`/ Any          | `nothing`/ Any   | `nothing`/ Any  | `hclust` with `linkage` or `WardLinkage`                                                         | `reorder_hclust` by data `order`     | Cluster, then reorder branches to be close to `order`  |
| `AbstractVector{<:Integer}` | `nothing`                           | Any           | `ReorderHclust`                            | `nothing`/ Any          | `nothing`/ Any   | `nothing`/ Any  | `ehclust` with `groups` and `linkage` or `WardLinkage`                                           | `reorder_hclust` by data `order`     | Cluster, then reorder branches to be close to `order`  |

All other combinations are invalid. Note:

  - When calling `hclust` and/or `ehclust` and/or `slanted_orders`, then specifying `arrange_by` will use it instead of
    the displayed data matrix.

  - When calling `hclust` and/or `ehclust`, then specifying a `metric` will be used instead of `Euclidean` to compute
    the distances matrix.

  - Specifying `groups` only impacts the tree and order when computing a new clustering without other order constraints.
    They can still be specified to denote gaps in the heatmap, even when they do not impact the tree and/or order.
"""
@kwdef mutable struct HeatmapGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    entries::MatrixData = MatrixData()
    cells::MatrixEntitiesData = MatrixEntitiesData()
    rows::HeatmapAxisData = HeatmapAxisData()
    columns::HeatmapAxisData = HeatmapAxisData()
end

function Validations.validate(context::ValidationContext, data::HeatmapGraphData)::Nothing
    values = data.entries.values
    if values === nothing
        throw(ArgumentError("must specify $(location(context)).entries.values"))
    end
    n_rows, n_columns = size(values)

    validate_matrix_size(context, "cells.hovers", data.cells.hovers, "entries.values", size(values))
    validate_matrix_size(context, "cells.mask", data.cells.mask, "entries.values", size(values))
    if data.cells.mask !== nothing && !any(data.cells.mask)
        throw(ArgumentError("all cells hidden by $(location(context)).cells.mask"))
    end

    validate(context, data.rows, "rows", n_rows)
    validate(context, data.columns, "columns", n_columns)

    return nothing
end

"""
A graph showing a heatmap. See [`HeatmapGraphData`](@ref) and [`HeatmapGraphConfiguration`](@ref).
"""
HeatmapGraph = Graph{HeatmapGraphData, HeatmapGraphConfiguration}

"""
    function heatmap_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        entries::MatrixData = MatrixData(),
        cells::MatrixEntitiesData = MatrixEntitiesData(),
        rows::HeatmapAxisData = HeatmapAxisData(),
        columns::HeatmapAxisData = HeatmapAxisData(),
        configuration::HeatmapGraphConfiguration = HeatmapGraphConfiguration()]
    )::HeatmapGraph

Create a [`HeatmapGraph`](@ref) by initializing only the [`HeatmapGraphData`](@ref) fields (with an optional
[`HeatmapGraphConfiguration`](@ref)).
"""
function heatmap_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    entries::MatrixData = MatrixData(),
    cells::MatrixEntitiesData = MatrixEntitiesData(),
    rows::HeatmapAxisData = HeatmapAxisData(),
    columns::HeatmapAxisData = HeatmapAxisData(),
    configuration::HeatmapGraphConfiguration = HeatmapGraphConfiguration(),
)::HeatmapGraph
    return HeatmapGraph(HeatmapGraphData(; figure_title, entries, cells, rows, columns), configuration)
end

# The entries values of a validated heatmap graph.
function entries_values(graph::HeatmapGraph)::AbstractMatrix{<:Real}
    values = graph.data.entries.values
    @assert values !== nothing
    return values
end

function Common.validate_graph(graph::HeatmapGraph)::Nothing
    values = entries_values(graph)

    validate_colors(
        ValidationContext(["graph.data.entries.values"]),
        values,
        ValidationContext(["graph.configuration.entries_colors"]),
        graph.configuration.entries_colors,
    )

    validate_axis_sizes(;
        axis_name = "columns",
        annotation_size = graph.configuration.columns.annotations,
        n_annotations = length(graph.data.columns.annotations),
        dendogram_size = graph.configuration.rows.dendogram_size,
    )

    validate_axis_sizes(;
        axis_name = "rows",
        annotation_size = graph.configuration.rows.annotations,
        n_annotations = length(graph.data.rows.annotations),
        dendogram_size = graph.configuration.columns.dendogram_size,
    )

    n_rows, n_columns = size(values)
    if n_rows != n_columns
        for (name, axis_configuration) in (("rows", graph.configuration.rows), ("columns", graph.configuration.columns))
            if axis_configuration.reorder == SameOrder
                throw(ArgumentError(chomp("""
                                          can't specify heatmap graph.configuration.$(name).reorder: SameOrder
                                          for a non-square matrix: $(n_rows) rows x $(n_columns) columns
                                          """)))
            end
        end
    end

    for (name, axis_data, axis_configuration, other_name, other_axis_data, other_axis_configuration) in (
        ("columns", graph.data.columns, graph.configuration.columns, "rows", graph.data.rows, graph.configuration.rows),
        ("rows", graph.data.rows, graph.configuration.rows, "columns", graph.data.columns, graph.configuration.columns),
    )
        data_order = axis_data.order
        data_arrange_by = axis_data.arrange_by
        configuration_reorder = axis_configuration.reorder
        configuration_linkage = axis_configuration.linkage
        configuration_metric = axis_configuration.metric
        configuration_dendogram_size = axis_configuration.dendogram_size
        other_data_order = other_axis_data.order

        is_clustered = false
        is_using_groups = axis_configuration.groups_gap !== nothing
        if data_order === nothing
            if configuration_reorder === nothing
                if configuration_dendogram_size === nothing
                    if data_arrange_by !== nothing
                        throw(ArgumentError(chomp("""
                                                  can't specify heatmap graph.data.$(name).arrange_by
                                                  without graph.configuration.$(name).dendogram_size
                                                  or graph.configuration.$(name).reorder
                                                  """)))
                    end
                    if configuration_linkage !== nothing
                        throw(ArgumentError(chomp("""
                                                  can't specify heatmap graph.configuration.$(name).linkage
                                                  without graph.configuration.$(name).dendogram_size
                                                  or graph.configuration.$(name).reorder
                                                  """)))
                    end
                    if configuration_metric !== nothing
                        throw(ArgumentError(chomp("""
                                                  can't specify heatmap graph.configuration.$(name).metric
                                                  without graph.configuration.$(name).dendogram_size
                                                  or graph.configuration.$(name).reorder
                                                  """)))
                    end
                end

            elseif configuration_reorder == SameOrder
                if data_arrange_by !== nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.data.$(name).arrange_by
                                              for graph.configuration.$(name).reorder: $(configuration_reorder)
                                              """)))
                end
                if configuration_linkage !== nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.configuration.$(name).linkage
                                              for graph.configuration.$(name).reorder: $(configuration_reorder)
                                              """)))
                end
                if configuration_metric !== nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.configuration.$(name).metric
                                              for graph.configuration.$(name).reorder: $(configuration_reorder)
                                              """)))
                end
                if other_data_order === nothing && other_axis_configuration.reorder === nothing
                    throw(
                        ArgumentError(
                            chomp("""
                                  can't specify heatmap graph.configuration.$(name).reorder: $(configuration_reorder)
                                  without an order to copy from the $(other_name)
                                  """),
                        ),
                    )
                end
                if configuration_dendogram_size !== nothing &&
                   !(other_data_order isa Hclust) &&
                   other_axis_configuration.reorder === nothing &&
                   other_axis_configuration.dendogram_size === nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.configuration.$(name).dendogram_size
                                              with graph.configuration.$(name).reorder: $(configuration_reorder)
                                              without a tree to copy from the $(other_name)
                                              """)))
                end

            elseif configuration_reorder in (OptimalHclust, RCompatibleHclust, SlantedHclust, SlantedPreSquaredHclust)
                is_using_groups = is_clustered = true

            elseif !(configuration_reorder in (SlantedOrder, SlantedPreSquaredOrder))
                throw(
                    ArgumentError(
                        chomp("""
                              can't specify heatmap graph.configuration.$(name).reorder: $(configuration_reorder)
                              without explicit vector graph.data.$(name).order
                              """),
                    ),
                )
            end

        elseif data_order isa Hclust
            if configuration_linkage !== nothing
                throw(ArgumentError(chomp("""
                                          can't specify heatmap graph.configuration.$(name).linkage
                                          for explicit hclust graph.data.$(name).order
                                          """)))
            end
            if configuration_metric !== nothing
                throw(ArgumentError(chomp("""
                                          can't specify heatmap graph.configuration.$(name).metric
                                          for explicit hclust graph.data.$(name).order
                                          """)))
            end
            if !(configuration_reorder in (nothing, SlantedHclust, SlantedPreSquaredHclust))
                throw(
                    ArgumentError(
                        chomp("""
                              can't specify heatmap graph.configuration.$(name).reorder: $(configuration_reorder)
                              for explicit hclust graph.data.$(name).order
                              """),
                    ),
                )
            end
            if configuration_reorder === nothing && data_arrange_by !== nothing
                throw(ArgumentError(chomp("""
                                          can't specify heatmap graph.data.$(name).arrange_by
                                          without graph.configuration.$(name).reorder
                                          for explicit hclust graph.data.$(name).order
                                          """)))
            end

        elseif data_order isa AbstractVector
            if data_arrange_by !== nothing
                throw(ArgumentError(chomp("""
                                          can't specify heatmap graph.data.$(name).arrange_by
                                          for explicit vector graph.data.$(name).order
                                          """)))
            end

            if configuration_reorder == ReorderHclust
                is_using_groups = is_clustered = true

            elseif configuration_reorder !== nothing
                throw(
                    ArgumentError(
                        chomp("""
                              can't specify heatmap graph.configuration.$(name).reorder: $(configuration_reorder)
                              for explicit vector graph.data.$(name).order
                              """),
                    ),
                )
            end

            if configuration_dendogram_size === nothing
                if configuration_linkage !== nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.configuration.$(name).linkage
                                              for explicit vector graph.data.$(name).order
                                              without graph.configuration.$(name).dendogram_size
                                              """)))
                end
                if configuration_metric !== nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.configuration.$(name).metric
                                              for explicit vector graph.data.$(name).order
                                              without graph.configuration.$(name).dendogram_size
                                              """)))
                end
            end

        else
            @assert false
        end

        if !is_using_groups && axis_data.groups !== nothing
            throw(ArgumentError("no effect for specified graph.data.$(name).groups"))
        end

        ## Unlike the groups, the subgroups have their own gap, so they are of use if either the axis is clustered (they
        ## constrain the clustering) or they are gapped.
        if !is_clustered && axis_configuration.subgroups_gap === nothing && axis_data.subgroups !== nothing
            throw(ArgumentError("no effect for specified graph.data.$(name).subgroups"))
        end

        if axis_configuration.subgroups_gap !== nothing && axis_data.subgroups === nothing
            throw(ArgumentError(chomp("""
                                      can't specify heatmap graph.configuration.$(name).subgroups_gap
                                      without graph.data.$(name).subgroups
                                      """)))
        end
    end

    return nothing
end

function Common.graph_to_figure(graph::HeatmapGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    next_colors_scale_index = [1]
    cells_mask = graph.data.cells.mask
    colors = configured_colors(;
        colors_configuration = graph.configuration.entries_colors,
        colors_title = prefer_data(graph.data.entries.title, graph.configuration.entries_colors.title),
        colors_values = entries_values(graph),
        next_colors_scale_index,
        mask = cells_mask,
    )

    final_order = heatmap_order(graph)

    # The order is that of the data; the `origin` decides which end of each axis the first entry is shown at.
    rows_mask = graph.data.rows.entities.mask
    columns_mask = graph.data.columns.entities.mask
    rows_order = displayed_order(
        final_order.rows_order,
        rows_mask,
        graph.configuration.origin in (HeatmapTopLeft, HeatmapTopRight),
    )
    columns_order = displayed_order(
        final_order.columns_order,
        columns_mask,
        graph.configuration.origin in (HeatmapBottomRight, HeatmapTopRight),
    )

    reordered_values = colors.final_colors_values[rows_order, columns_order]
    if cells_mask !== nothing
        # Hidden cells are `missing` (serialized as JSON `null`) rather than `NaN`: Plotly renders both as blank, but the
        # JSON writer used by `to_html` rejects `NaN`.
        reordered_values = Matrix{Union{eltype(reordered_values), Missing}}(reordered_values)
        reordered_values[.!cells_mask[rows_order, columns_order]] .= missing
    end

    n_rows_annotations = length(graph.data.rows.annotations)
    n_columns_annotations = length(graph.data.columns.annotations)

    columns_sub_graph = SubGraph(;
        index = 1,
        n_graphs = 1,
        graphs_gap = nothing,
        n_annotations = n_rows_annotations,
        annotation_size = graph.configuration.rows.annotations,
        dendogram_size = graph.configuration.rows.dendogram_size,
    )

    rows_sub_graph = SubGraph(;
        index = 1,
        n_graphs = 1,
        graphs_gap = nothing,
        n_annotations = n_columns_annotations,
        annotation_size = graph.configuration.columns.annotations,
        dendogram_size = graph.configuration.columns.dendogram_size,
    )

    xaxis_index, _, yaxis_index, _ = plotly_sub_graph_axes(;
        basis_sub_graph = columns_sub_graph,
        values_sub_graph = rows_sub_graph,
        values_orientation = VerticalValues,
    )

    expanded_rows_mask = compute_expansion_mask(
        rows_order,
        graph.data.rows.groups,
        graph.configuration.rows.groups_gap,
        graph.data.rows.subgroups,
        graph.configuration.rows.subgroups_gap,
    )
    expanded_columns_mask = compute_expansion_mask(
        columns_order,
        graph.data.columns.groups,
        graph.configuration.columns.groups_gap,
        graph.data.columns.subgroups,
        graph.configuration.columns.subgroups_gap,
    )

    expanded_z = expand_z_matrix(reordered_values, rows_order, expanded_rows_mask, columns_order, expanded_columns_mask)

    n_expanded_rows, n_expanded_columns = size(expanded_z)

    rows_hovers = graph.data.rows.entities.hovers
    if rows_hovers !== nothing
        rows_hovers = rows_hovers[rows_order]
    end

    columns_hovers = graph.data.columns.entities.hovers
    if columns_hovers !== nothing
        columns_hovers = columns_hovers[columns_order]
    end

    entries_hovers = graph.data.cells.hovers
    if entries_hovers !== nothing
        entries_hovers = entries_hovers[rows_order, columns_order]
    end

    hovers = expand_hovers(;
        n_expanded_rows,
        n_expanded_columns,
        expanded_rows_mask,
        expanded_columns_mask,
        rows_hovers,
        columns_hovers,
        entries_hovers,
    )
    if hovers !== nothing
        hovers = permutedims(hovers)
    end

    push!(
        traces,
        heatmap(;
            name = "",
            x = collect(1:n_expanded_columns),
            y = collect(1:n_expanded_rows),
            z = expanded_z,
            xaxis = plotly_axis("x", xaxis_index; short = true),
            yaxis = plotly_axis("y", yaxis_index; short = true),
            text = hovers,
            coloraxis = plotly_axis("color", 1),
        ),
    )

    has_legend_only_traces = [false]

    columns_annotations_colors = push_annotations_traces!(;
        traces,
        names = nothing,
        basis_sub_graph = columns_sub_graph,
        value_axis = graph.configuration.entries_colors.axis,
        values_orientation = VerticalValues,
        next_colors_scale_index,
        has_legend_only_traces,
        annotations_data = graph.data.columns.annotations,
        annotation_size = graph.configuration.columns.annotations,
        entries_hovers = graph.data.columns.entities.hovers,
        order = columns_order,
        expanded_mask = expanded_columns_mask,
    )

    rows_annotations_colors = push_annotations_traces!(;
        traces,
        names = nothing,
        basis_sub_graph = rows_sub_graph,
        value_axis = graph.configuration.entries_colors.axis,
        values_orientation = HorizontalValues,
        next_colors_scale_index,
        has_legend_only_traces,
        annotations_data = graph.data.rows.annotations,
        annotation_size = graph.configuration.rows.annotations,
        entries_hovers = graph.data.rows.entities.hovers,
        order = rows_order,
        expanded_mask = expanded_rows_mask,
    )

    if graph.configuration.rows.dendogram_size !== nothing
        rows_max_height = push_dendogram_trace!(;
            traces,
            clusters = displayed_hclust(final_order.rows_hclust, rows_mask),
            values_orientation = HorizontalValues,
            dendogram_line = graph.configuration.rows.dendogram_line,
            expanded_mask = expanded_rows_mask,
            basis_sub_graph = rows_sub_graph,
            values_sub_graph = SubGraph(;
                index = 0,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_rows_annotations,
                annotation_size = graph.configuration.rows.annotations,
                dendogram_size = graph.configuration.rows.dendogram_size,
            ),
        )
    else
        rows_max_height = 0
    end

    if graph.configuration.columns.dendogram_size !== nothing
        columns_max_height = push_dendogram_trace!(;
            traces,
            clusters = displayed_hclust(final_order.columns_hclust, columns_mask),
            values_orientation = VerticalValues,
            dendogram_line = graph.configuration.columns.dendogram_line,
            expanded_mask = expanded_columns_mask,
            basis_sub_graph = columns_sub_graph,
            values_sub_graph = SubGraph(;
                index = 0,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_columns_annotations,
                annotation_size = graph.configuration.columns.annotations,
                dendogram_size = graph.configuration.columns.dendogram_size,
            ),
        )
    else
        columns_max_height = 0
    end

    has_legend =
        (
            n_rows_annotations > 0 &&
            any([annotation_colors.show_in_legend for annotation_colors in rows_annotations_colors])
        ) || (
            n_columns_annotations > 0 &&
            any([annotation_colors.show_in_legend for annotation_colors in columns_annotations_colors])
        )
    has_hovers =
        graph.data.cells.hovers !== nothing ||
        graph.data.rows.entities.hovers !== nothing ||
        graph.data.columns.entities.hovers !== nothing

    layout = plotly_layout(graph.configuration.figure; title = graph.data.figure_title, has_legend, has_hovers)

    expanded_rows_names = expand_vector(string_values(graph.data.rows.names), rows_order, expanded_rows_mask, "")
    set_layout_axis!(
        layout,
        plotly_axis("y", yaxis_index),
        AxisConfiguration(; show_grid = false, show_ticks = graph.data.rows.names.values !== nothing);
        title = prefer_data(graph.data.rows.names.title, graph.configuration.rows.title),
        ticks_values = expanded_rows_names === nothing ? nothing : collect(1:n_expanded_rows),
        ticks_labels = expanded_rows_names,
        range = Range(; minimum = 0.5, maximum = n_expanded_rows + 0.5),
        domain = plotly_sub_graph_domain(
            SubGraph(;
                index = 1,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_columns_annotations,
                annotation_size = graph.configuration.columns.annotations,
                dendogram_size = graph.configuration.columns.dendogram_size,
            ),
        ),
        is_zeroable = false,
    )

    expanded_columns_names =
        expand_vector(string_values(graph.data.columns.names), columns_order, expanded_columns_mask, "")
    set_layout_axis!(
        layout,
        plotly_axis("x", xaxis_index),
        AxisConfiguration(; show_grid = false, show_ticks = graph.data.columns.names.values !== nothing);
        title = prefer_data(graph.data.columns.names.title, graph.configuration.columns.title),
        ticks_values = expanded_columns_names === nothing ? nothing : collect(1:n_expanded_columns),
        ticks_labels = expanded_columns_names,
        range = Range(; minimum = 0.5, maximum = n_expanded_columns + 0.5),
        domain = plotly_sub_graph_domain(
            SubGraph(;
                index = 1,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_rows_annotations,
                annotation_size = graph.configuration.rows.annotations,
                dendogram_size = graph.configuration.rows.dendogram_size,
            ),
        ),
        is_zeroable = false,
    )

    next_colors_scale_offset_index = [Int(has_legend)]

    if colors !== nothing && colors.colors_scale_index !== nothing
        set_layout_colorscale!(;
            layout,
            colors_scale_index = colors.colors_scale_index,
            colors_configuration = colors.colors_configuration,
            scaled_colors_palette = colors.scaled_colors_palette,
            range = colors.final_colors_range,
            title = colors.colors_title,
            show_scale = colors.show_scale,
            next_colors_scale_offset_index,
            colors_scale_offsets = graph.configuration.figure.colors_scale_offsets,
        )
    end

    layout["annotations"] = plotly_annotations = []
    for (
        axis_letter,
        values_orientation,
        annotations_data,
        annotations_colors,
        annotation_size,
        dendogram_size,
        max_height,
    ) in (
        (
            "y",
            VerticalValues,
            graph.data.columns.annotations,
            columns_annotations_colors,
            graph.configuration.columns.annotations,
            graph.configuration.columns.dendogram_size,
            columns_max_height,
        ),
        (
            "x",
            HorizontalValues,
            graph.data.rows.annotations,
            rows_annotations_colors,
            graph.configuration.rows.annotations,
            graph.configuration.rows.dendogram_size,
            rows_max_height,
        ),
    )
        n_annotations = 0
        if annotations_colors !== nothing
            n_annotations = length(annotations_colors)
            for (annotation_index, annotation_colors) in enumerate(annotations_colors)
                annotation_data = annotations_data[annotation_index]
                sub_graph = SubGraph(;
                    index = -annotation_index,
                    n_graphs = 1,
                    graphs_gap = nothing,
                    n_annotations,
                    annotation_size,
                    dendogram_size,
                )
                push_plotly_annotation!(;
                    plotly_annotations,
                    values_sub_graph = sub_graph,
                    values_orientation,
                    title = annotation_data.values.title,
                )
                set_layout_axis!(  # NOJET
                    layout,
                    plotly_axis(axis_letter, annotation_index),
                    AxisConfiguration(; show_grid = false, show_ticks = false);
                    range = Range(; minimum = 0, maximum = 1),
                    domain = plotly_sub_graph_domain(sub_graph),
                    is_tick_axis = false,
                    is_zeroable = false,
                )
                if annotation_colors.colors_scale_index !== nothing
                    set_layout_colorscale!(;
                        layout,
                        colors_scale_index = annotation_colors.colors_scale_index,
                        colors_configuration = annotation_data.colors,
                        scaled_colors_palette = annotation_colors.scaled_colors_palette,
                        range = nothing,
                        title = annotation_data.values.title,
                        show_scale = annotation_colors.show_scale,
                        next_colors_scale_offset_index,
                        colors_scale_offsets = graph.configuration.figure.colors_scale_offsets,
                    )
                end
            end
        end

        if dendogram_size !== nothing
            set_layout_axis!(  # NOJET
                layout,
                plotly_axis(axis_letter, n_annotations + 1 + 1),
                AxisConfiguration(; show_grid = false, show_ticks = false);
                title = nothing,
                range = Range(0, max_height),
                domain = plotly_sub_graph_domain(
                    SubGraph(;
                        index = 0,
                        n_graphs = 1,
                        graphs_gap = nothing,
                        n_annotations,
                        annotation_size,
                        dendogram_size,
                    ),
                ),
                is_tick_axis = false,
                is_zeroable = false,
            )
        end
    end

    if has_legend_only_traces[1]
        layout["xaxis99"] = Dict(:domain => [0, 0.001], :showgrid => false, :showticklabels => false)
        layout["yaxis99"] = Dict(:domain => [0, 0.001], :showgrid => false, :showticklabels => false)
    end

    if n_rows_annotations > 0 || n_columns_annotations > 0
        layout["bargap"] = 0
    end

    return plotly_figure(traces, layout)
end

function compute_heatmap_order(graph::HeatmapGraph)::HeatmapGraphOrder
    data_rows_arrange_by = prefer_data(graph.data.rows.arrange_by, entries_values(graph))
    data_columns_arrange_by = prefer_data(graph.data.columns.arrange_by, entries_values(graph))
    @assert data_rows_arrange_by !== nothing
    @assert data_columns_arrange_by !== nothing

    slant_rows = (
        graph.configuration.rows.reorder in
        (SlantedHclust, SlantedPreSquaredHclust, SlantedOrder, SlantedPreSquaredOrder)
    )
    slant_columns = (
        graph.configuration.columns.reorder in
        (SlantedHclust, SlantedPreSquaredHclust, SlantedOrder, SlantedPreSquaredOrder)
    )

    slant_rows_is_pre_squared = graph.configuration.rows.reorder in (SlantedPreSquaredHclust, SlantedPreSquaredOrder)
    slant_columns_is_pre_squared =
        graph.configuration.columns.reorder in (SlantedPreSquaredHclust, SlantedPreSquaredOrder)

    if slant_rows &&
       slant_columns &&
       slant_rows_is_pre_squared == slant_columns_is_pre_squared &&
       data_rows_arrange_by === data_columns_arrange_by
        slant_rows_order, slant_columns_order =
            slanted_orders(data_rows_arrange_by; squared_order = !slant_rows_is_pre_squared)
    else
        slant_rows_order = nothing
        slant_columns_order = nothing

        if slant_rows
            if graph.configuration.columns.reorder == SameOrder
                slant_rows_order, slant_columns_order =
                    slanted_orders(data_rows_arrange_by; same_order = true, squared_order = !slant_rows_is_pre_squared)
            else
                slant_rows_order, _ =
                    slanted_orders(data_rows_arrange_by; order_cols = false, squared_order = !slant_rows_is_pre_squared)
            end
        end

        if slant_columns
            if graph.configuration.rows.reorder == SameOrder
                slant_rows_order, slant_columns_order = slanted_orders(
                    data_columns_arrange_by;
                    same_order = true,
                    squared_order = !slant_columns_is_pre_squared,
                )
            else
                _, slant_columns_order = slanted_orders(
                    data_columns_arrange_by;
                    order_rows = false,
                    squared_order = !slant_columns_is_pre_squared,
                )
            end
        end
    end

    data_columns_order, data_columns_hclust = finalize_order(;
        data_order = graph.data.columns.order,
        data_arrange_by = data_columns_arrange_by,
        data_groups = graph.data.columns.groups,
        data_subgroups = graph.data.columns.subgroups,
        slant_order = slant_columns_order,
        configuration_reorder = graph.configuration.columns.reorder,
        configuration_dendogram_size = graph.configuration.columns.dendogram_size,
        configuration_linkage = graph.configuration.columns.linkage,
        configuration_metric = graph.configuration.columns.metric,
    )

    data_rows_order, data_rows_hclust = finalize_order(;
        data_order = graph.data.rows.order,
        data_arrange_by = PermutedDimsArray(data_rows_arrange_by, (2, 1)),
        data_groups = graph.data.rows.groups,
        data_subgroups = graph.data.rows.subgroups,
        slant_order = slant_rows_order,
        configuration_reorder = graph.configuration.rows.reorder,
        configuration_dendogram_size = graph.configuration.rows.dendogram_size,
        configuration_linkage = graph.configuration.rows.linkage,
        configuration_metric = graph.configuration.rows.metric,
    )

    if graph.configuration.rows.reorder == SameOrder
        @assert data_rows_order === nothing
        @assert data_rows_hclust === nothing
        data_rows_order = data_columns_order
        data_rows_hclust = data_columns_hclust
    end

    if graph.configuration.columns.reorder == SameOrder
        @assert data_columns_order === nothing
        @assert data_columns_hclust === nothing
        data_columns_order = data_rows_order
        data_columns_hclust = data_rows_hclust
    end

    n_rows, n_columns = size(entries_values(graph))  # NOJET

    # An axis that wasn't reordered is still reported as an explicit permutation, so the order can be used as-is.
    if data_rows_order === nothing
        data_rows_order = collect(1:n_rows)
    end
    if data_columns_order === nothing
        data_columns_order = collect(1:n_columns)
    end

    return HeatmapGraphOrder(data_rows_order, data_rows_hclust, data_columns_order, data_columns_hclust)
end

"""
    heatmap_order(graph::HeatmapGraph)::HeatmapGraphOrder

Return the [`HeatmapGraphOrder`](@ref) of a heatmap `graph`, that is, the final order of its rows and columns and the
trees they were clustered by, without rendering it.

You can just write `graph.order` instead of `heatmap_order(graph)`. Either way the order is only computed once; showing
the graph will reuse it, and vice versa.

Use this to list the entries in the order they are shown:

```julia
ordered_rows_names = graph.data.rows.names.values[graph.order.rows_order]
```

Use it to show several graphs in the same order, so they can be compared. Cluster one of them, then give the rest its
order (and, if they use the same groups, they will also have the same gaps):

```julia
graph.configuration.columns.reorder = OptimalHclust
other_graph.data.columns.order = graph.order.columns_order
```

If the graphs also show a dendogram, give them the tree instead of the order. This arranges them in the same order
*and* draws the same tree above each of them (this only makes sense if the graphs share the same columns, as the tree
refers to the original column indices):

```julia
graph.configuration.columns.reorder = OptimalHclust
graph.configuration.columns.dendogram_size = 0.1
other_graph.data.columns.order = graph.order.columns_hclust
other_graph.configuration.columns.dendogram_size = 0.1
```
"""
function heatmap_order(graph::HeatmapGraph)::HeatmapGraphOrder
    final_order = graph.configuration.final_order  # NOJET
    if final_order === nothing
        graph.configuration.final_order = final_order = compute_heatmap_order(graph)  # NOJET
    end
    return final_order
end

"""
    reset_order!(graph::HeatmapGraph)::Nothing

Forget the [`HeatmapGraphOrder`](@ref) cached in the graph's `final_order`, so that asking for the graph's `order` (or
showing it) will compute it again. Call this after changing anything the order was computed from.
"""
function reset_order!(graph::HeatmapGraph)::Nothing
    graph.configuration.final_order = nothing
    return nothing
end

# Only a heatmap has a computed order, so only a heatmap has this property; any other graph will complain there's no
# such field.
function Base.getproperty(graph::HeatmapGraph, property::Symbol)::Any
    if property == :order
        return heatmap_order(graph)
    else
        return invoke(Base.getproperty, Tuple{Graph, Symbol}, graph, property)
    end
end

# The entries of an axis which are shown, in the order they are shown in: the order of the data without the hidden
# entries, reversed if the `origin` places the first entry at the far end of the axis.
function displayed_order(
    order::AbstractVector{<:Integer},
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
    is_reversed::Bool,
)::AbstractVector{<:Integer}
    if mask !== nothing
        order = [index for index in order if mask[index]]
    end
    return is_reversed ? reverse(order) : order
end

# The tree of the shown entries of an axis: the tree of the data with the hidden entries pruned out of it, and the
# remaining leaves renumbered to the positions of the shown entries. A merge left with a single subtree is dropped, and
# that subtree takes its place.
function displayed_hclust(clusters::Hclust, ::Nothing)::Hclust
    return clusters
end

function displayed_hclust(clusters::Hclust, mask::Union{AbstractVector{Bool}, BitVector})::Hclust
    shown_positions = cumsum(mask)
    n_merges = size(clusters.merges, 1)

    # The pruned node each merge maps to: a leaf (negative) or a merge (positive) of the pruned tree, or `nothing` if it
    # held only hidden leaves.
    pruned_node_per_merge = Vector{Maybe{Int}}(undef, n_merges)

    function pruned_node(node::Integer)::Maybe{Int}
        if node < 0
            return mask[-node] ? -shown_positions[-node] : nothing
        else
            return pruned_node_per_merge[node]
        end
    end

    pruned_merges = Int[]
    pruned_heights = eltype(clusters.heights)[]
    for merge_index in 1:n_merges
        left_node = pruned_node(clusters.merges[merge_index, 1])
        right_node = pruned_node(clusters.merges[merge_index, 2])
        if left_node === nothing
            pruned_node_per_merge[merge_index] = right_node
        elseif right_node === nothing
            pruned_node_per_merge[merge_index] = left_node
        else
            push!(pruned_merges, left_node, right_node)
            push!(pruned_heights, clusters.heights[merge_index])
            pruned_node_per_merge[merge_index] = length(pruned_heights)
        end
    end

    pruned_order = [shown_positions[index] for index in clusters.order if mask[index]]

    return Hclust(permutedims(reshape(pruned_merges, 2, :)), pruned_heights, pruned_order, clusters.linkage)
end

function finalize_order(;
    data_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}},
    data_arrange_by::AbstractMatrix{<:Real},
    data_groups::Maybe{Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}}},
    data_subgroups::Maybe{Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}}},
    slant_order::Maybe{AbstractVector{<:Integer}},
    configuration_reorder::Maybe{HeatmapReorder},
    configuration_dendogram_size::Maybe{Real},
    configuration_linkage::Maybe{HeatmapLinkage},
    configuration_metric::Maybe{PreMetric},
)::Tuple{Maybe{AbstractVector{<:Integer}}, Maybe{Hclust}}
    if configuration_linkage === nothing
        configuration_linkage = WardLinkage
    end

    if configuration_metric === nothing
        configuration_metric = Euclidean()
    end

    if data_order === nothing
        if configuration_reorder === nothing
            if configuration_dendogram_size === nothing
                return (nothing, nothing)
            else
                distances = pairwise(configuration_metric, data_arrange_by; dims = 2)
                clusters = ehclust(
                    distances;
                    order = collect(1:size(distances, 1)),
                    linkage = hclust_linkage(configuration_linkage),
                )
                return (clusters.order, clusters)
            end

        elseif configuration_reorder === SameOrder
            return (nothing, nothing)

        elseif configuration_reorder in (OptimalHclust, RCompatibleHclust)
            distances = pairwise(configuration_metric, data_arrange_by; dims = 2)
            clusters = ehclust(  # NOJET
                distances;
                linkage = hclust_linkage(configuration_linkage),
                groups = data_groups,
                subgroups = data_subgroups,
                branchorder = hclust_branchorder(configuration_reorder),
            )
            return (clusters.order, clusters)

        elseif configuration_reorder in (SlantedHclust, SlantedPreSquaredHclust)
            @assert slant_order !== nothing
            distances = pairwise(configuration_metric, data_arrange_by; dims = 2)
            clusters = ehclust(
                distances;
                linkage = hclust_linkage(configuration_linkage),
                groups = data_groups,
                subgroups = data_subgroups,
            )
            clusters = reorder_hclust(clusters, slant_order)
            return (clusters.order, clusters)

        elseif configuration_reorder in (SlantedOrder, SlantedPreSquaredOrder)
            distances = pairwise(configuration_metric, data_arrange_by; dims = 2)
            clusters = ehclust(distances; order = slant_order, linkage = hclust_linkage(configuration_linkage))
            return (slant_order, clusters)

        else
            @assert false
        end

    elseif data_order isa Hclust
        if configuration_reorder === nothing
            return (data_order.order, data_order)

        elseif configuration_reorder in (SlantedHclust, SlantedPreSquaredHclust)
            @assert slant_order !== nothing
            clusters = reorder_hclust(data_order, slant_order)
            return (clusters.order, clusters)

        else
            @assert false
        end

    elseif data_order isa AbstractVector
        if configuration_reorder === nothing
            if configuration_dendogram_size === nothing
                return (data_order, nothing)
            else
                distances = pairwise(configuration_metric, data_arrange_by; dims = 2)
                clusters = ehclust(distances; order = data_order, linkage = hclust_linkage(configuration_linkage))
                return (clusters.order, clusters)
            end

        elseif configuration_reorder === ReorderHclust
            distances = pairwise(configuration_metric, data_arrange_by; dims = 2)
            clusters = ehclust(distances; groups = data_groups, subgroups = data_subgroups)
            clusters = reorder_hclust(clusters, data_order)
            return (clusters.order, clusters)

        else
            @assert false
        end

    else
        @assert false
    end
end

function hclust_linkage(linkage::HeatmapLinkage)::Symbol
    if linkage == SingleLinkage
        return :single
    elseif linkage == AverageLinkage
        return :average
    elseif linkage == CompleteLinkage
        return :complete
    elseif linkage == WardLinkage
        return :ward
    elseif linkage == WardPreSquaredLinkage
        return :ward_presquared
    else
        @assert false
    end
end

function hclust_branchorder(reorder::HeatmapReorder)::Symbol
    if reorder == RCompatibleHclust
        return :r
    elseif reorder == OptimalHclust
        return :optimal
    else
        @assert false
    end
end

function push_dendogram_trace!(;
    traces::Vector{GenericTrace},
    clusters::Hclust,
    values_orientation::ValuesOrientation,
    dendogram_line::LineConfiguration,
    expanded_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
    basis_sub_graph::SubGraph,
    values_sub_graph::SubGraph,
)::Real
    values, heights = dendogram_coordinates(clusters, expanded_mask)

    if values_orientation == VerticalValues
        xs = values
        ys = heights
    elseif values_orientation == HorizontalValues
        ys = values
        xs = heights
    else
        @assert false
    end

    xaxis_index, _, yaxis_index, _ = plotly_sub_graph_axes(; basis_sub_graph, values_sub_graph, values_orientation)

    push!(
        traces,
        scatter(;
            x = xs,
            y = ys,
            x0 = nothing,
            y0 = nothing,
            xaxis = plotly_axis("x", xaxis_index; short = true),
            yaxis = plotly_axis("y", yaxis_index; short = true),
            mode = "lines",
            name = "",
            line_width = dendogram_line.width,
            line_color = prefer_data(dendogram_line.color, "black"),
            line_dash = plotly_line_dash(prefer_data(dendogram_line.style, SolidLine)),
            showlegend = false,
        ),
    )

    return maximum(skipmissing(heights))
end

function dendogram_coordinates(
    clusters::Hclust,
    expanded_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
)::Tuple{AbstractVector{<:Union{AbstractFloat, Missing}}, AbstractVector{<:Union{AbstractFloat, Missing}}}
    # The separators between the line segments are `missing` (serialized as JSON `null`, which breaks the line) rather
    # than `NaN`, which the JSON writer used by `to_html` rejects.
    values = Union{Float32, Missing}[]
    heights = Union{Float32, Missing}[]

    n_values = length(clusters.order)
    @assert size(clusters.merges, 1) == n_values - 1
    value_per_node = Vector{Float32}(undef, n_values * 2 - 1)
    height_per_node = Vector{Float32}(undef, n_values * 2 - 1)
    height_per_node[1:n_values] .= 0

    if expanded_mask === nothing
        expanded_positions = nothing
    else
        expanded_positions = findall(expanded_mask)
        @assert length(expanded_positions) == n_values
    end

    for (position, index) in enumerate(clusters.order)
        if expanded_positions !== nothing
            position = expanded_positions[position]
        end
        value_per_node[index] = position
    end

    for merge_index in 1:(n_values - 1)
        left_merge_index, right_merge_index = clusters.merges[merge_index, :]
        height = clusters.heights[merge_index]

        @assert left_merge_index != 0
        @assert right_merge_index != 0
        @assert height >= 0

        left_node_index = left_merge_index < 0 ? -left_merge_index : left_merge_index + n_values
        right_node_index = right_merge_index < 0 ? -right_merge_index : right_merge_index + n_values

        left_value = value_per_node[left_node_index]
        right_value = value_per_node[right_node_index]

        left_height = height_per_node[left_node_index]
        right_height = height_per_node[right_node_index]

        middle_value = (left_value + right_value) / 2

        push!(values, left_value, left_value, right_value, right_value, missing)
        push!(heights, left_height, height, height, right_height, missing)

        value_per_node[merge_index + n_values] = middle_value
        height_per_node[merge_index + n_values] = height
    end

    return (values, heights)
end

function compute_expansion_mask(
    order::Maybe{AbstractVector{<:Integer}},
    groups::Maybe{AbstractVector},
    groups_gap::Maybe{Integer},
    subgroups::Maybe{AbstractVector} = nothing,
    subgroups_gap::Maybe{Integer} = nothing,
)::Maybe{Union{BitVector, AbstractVector{Bool}}}
    has_groups_gap = groups !== nothing && groups_gap !== nothing
    has_subgroups_gap = subgroups !== nothing && subgroups_gap !== nothing
    if !has_groups_gap && !has_subgroups_gap
        return nothing
    end

    @assert groups_gap === nothing || groups_gap > 0
    @assert subgroups_gap === nothing || subgroups_gap > 0

    if order === nothing
        order = 1:length(groups === nothing ? subgroups : groups)  # UNTESTED
    end

    expanded_mask = Bool[]

    ## A boundary between the groups is also a boundary between the subgroups, and is gapped as the wider of the two.
    prev_group = has_groups_gap ? groups[order[1]] : nothing
    prev_subgroup = has_subgroups_gap ? subgroups[order[1]] : nothing
    for entry_index in order
        gap = 0
        if has_groups_gap && groups[entry_index] != prev_group
            gap = groups_gap
        elseif has_subgroups_gap && subgroups[entry_index] != prev_subgroup
            gap = subgroups_gap
        end
        has_groups_gap && (prev_group = groups[entry_index])
        has_subgroups_gap && (prev_subgroup = subgroups[entry_index])

        for _ in 1:gap
            push!(expanded_mask, false)
        end
        push!(expanded_mask, true)
    end

    return expanded_mask
end

function expand_z_matrix(
    z::AbstractMatrix{<:Union{Real, Missing}},
    rows_order::Maybe{AbstractVector{<:Integer}},
    expanded_rows_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
    columns_order::Maybe{AbstractVector{<:Integer}},
    expanded_columns_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
)::AbstractMatrix{<:Union{Real, Missing}}
    if rows_order === nothing &&
       expanded_rows_mask === nothing &&
       columns_order === nothing &&
       expanded_columns_mask === nothing
        return z  # UNTESTED
    end

    if expanded_rows_mask === nothing && expanded_columns_mask === nothing
        return z
    end

    n_rows, n_columns = size(z)

    if expanded_rows_mask === nothing
        n_expanded_rows = n_rows
        expanded_rows_mask = 1:n_rows
    else
        n_expanded_rows = length(expanded_rows_mask)
    end

    if expanded_columns_mask === nothing
        n_expanded_columns = n_columns
        expanded_columns_mask = 1:n_columns
    else
        n_expanded_columns = length(expanded_columns_mask)
    end

    # The gap entries are `missing` (serialized as JSON `null`) rather than `NaN`: Plotly renders both as blank gaps, but
    # the JSON writer used by `to_html` rejects `NaN`.
    expanded_z = Matrix{Union{eltype(z), Missing}}(undef, n_expanded_rows, n_expanded_columns)
    expanded_z .= missing
    expanded_z[expanded_rows_mask, expanded_columns_mask] .= z

    return expanded_z
end

function expand_hovers(;
    n_expanded_rows::Integer,
    n_expanded_columns::Integer,
    expanded_rows_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
    expanded_columns_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
    rows_hovers::Maybe{AbstractVector{<:AbstractString}},
    columns_hovers::Maybe{AbstractVector{<:AbstractString}},
    entries_hovers::Maybe{AbstractMatrix{<:AbstractString}},
)::Maybe{AbstractMatrix{<:AbstractString}}
    if columns_hovers === nothing &&
       rows_hovers === nothing &&
       (entries_hovers === nothing || (expanded_rows_mask === nothing && expanded_columns_mask === nothing))
        return entries_hovers
    end

    expanded_hovers = Matrix{AbstractString}(undef, n_expanded_rows, n_expanded_columns)
    expanded_hovers .= ""

    if expanded_rows_mask === nothing
        expanded_rows_indices = 1:n_expanded_rows
    else
        expanded_rows_indices = findall(expanded_rows_mask)
    end

    if expanded_columns_mask === nothing
        expanded_columns_indices = 1:n_expanded_columns
    else
        expanded_columns_indices = findall(expanded_columns_mask)
    end

    for (column_index, column_position) in enumerate(expanded_columns_indices)
        if columns_hovers !== nothing
            column_hover = columns_hovers[column_index]
        else
            column_hover = ""
        end

        for (row_index, row_position) in enumerate(expanded_rows_indices)
            text = String[]
            if entries_hovers !== nothing
                entry_hover = entries_hovers[row_index, column_index]
                if entry_hover != ""
                    push!(text, entry_hover)
                end
            end

            if rows_hovers !== nothing
                row_hover = rows_hovers[row_index]
                if row_hover != ""
                    push!(text, row_hover)
                end
            end

            if column_hover != ""
                push!(text, column_hover)
            end

            expanded_hovers[row_position, column_position] = join(text, "<br>")
        end
    end

    return expanded_hovers
end

# The data of an axis, moved to the other axis of the graph (that is, with its `arrange_by` matrix transposed).
function flipped_axis_data(axis::HeatmapAxisData)::HeatmapAxisData
    return HeatmapAxisData(;
        names = axis.names,
        entities = axis.entities,
        order = axis.order,
        groups = axis.groups,
        subgroups = axis.subgroups,
        arrange_by = axis.arrange_by === nothing ? nothing : transpose(axis.arrange_by),
        annotations = axis.annotations,
    )
end

function Common.flip_axes(graph::HeatmapGraph)::HeatmapGraph
    entries = graph.data.entries
    cells = graph.data.cells
    return HeatmapGraph(  # NOJET
        HeatmapGraphData(;
            figure_title = graph.data.figure_title,
            entries = MatrixData(;
                values = entries.values === nothing ? nothing : transpose(entries.values),
                title = entries.title,
            ),
            cells = MatrixEntitiesData(;
                hovers = cells.hovers === nothing ? nothing : PermutedDimsArray(cells.hovers, (2, 1)),
                mask = cells.mask === nothing ? nothing : transpose(cells.mask),
            ),
            rows = flipped_axis_data(graph.data.columns),
            columns = flipped_axis_data(graph.data.rows),
        ),
        HeatmapGraphConfiguration(;
            figure = graph.configuration.figure,
            entries_colors = graph.configuration.entries_colors,
            rows = graph.configuration.columns,
            columns = graph.configuration.rows,
            origin = graph.configuration.origin,
        ),
    )
end

function Common.flip_axes!(graph::HeatmapGraph)::HeatmapGraph
    data = graph.data
    data.entries.values = data.entries.values === nothing ? nothing : transpose(data.entries.values)
    data.cells.hovers = data.cells.hovers === nothing ? nothing : PermutedDimsArray(data.cells.hovers, (2, 1))  # NOJET
    data.cells.mask = data.cells.mask === nothing ? nothing : transpose(data.cells.mask)
    data.rows, data.columns = data.columns, data.rows
    for axis in (data.rows, data.columns)
        if axis.arrange_by !== nothing
            axis.arrange_by = transpose(axis.arrange_by)
        end
    end

    configuration = graph.configuration
    configuration.rows, configuration.columns = configuration.columns, configuration.rows

    return graph
end

end
