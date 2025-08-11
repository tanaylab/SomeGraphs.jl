"""
Graphs for showing a 2D matrix.
"""
module Heatmaps

export AverageLinkage
export CompleteLinkage
export heatmap_graph
export HeatmapBottomLeft
export HeatmapBottomRight
export HeatmapGraph
export HeatmapGraphConfiguration
export HeatmapGraphData
export HeatmapLinkage
export HeatmapOrigin
export HeatmapReorder
export HeatmapTopLeft
export HeatmapTopRight
export OptimalHclust
export RCompatibleHclust
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
using PlotlyJS
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
    @kwdef mutable struct HeatmapGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        entries_colors::ColorsConfiguration = ColorsConfiguration()
        rows_annotations::AnnotationSize = AnnotationSize()
        columns_annotations::AnnotationSize = AnnotationSize()
        rows_reorder::Maybe{HeatmapReorder} = nothing
        columns_reorder::Maybe{HeatmapReorder} = nothing
        rows_linkage::Maybe{HeatmapLinkage} = nothing
        columns_linkage::Maybe{HeatmapLinkage} = nothing
        rows_metric::Maybe{PreMetric} = nothing
        columns_metric::Maybe{PreMetric} = nothing
        rows_groups_gap::Maybe{Integer} = 1
        columns_groups_gap::Maybe{Integer} = 1
        rows_dendogram_size::Real = nothing
        columns_dendogram_size::Maybe{Real} = nothing
        rows_dendogram_line::LineConfiguration = LineConfiguration()
        columns_dendogram_line::LineConfiguration = LineConfiguration()
        origin::HeatmapOrigin = HeatmapBottomLeft
    end

Configure a graph showing a heatmap.

This displays a matrix of values using a rectangle at each position. Due to Plotly's limitations, you still to manually
tweak the graph size for best results; there's no way to directly control the width and height of the rectangles. In
addition, the only supported color configurations are using continuous color palettes.

You can use `..._reorder` reorder the data. When specifying `..._linkage`, by default, the clustering uses the
`Euclidean` distance metric. You can override this by specifying the `..._metric`.

If groups are specified for some entries (rows and/or columns), they can be used to constrain the clustering, and/or to
create visible gaps in the heatmap (between entries of different groups). groups. The size of the gaps is the number of
fake entries to added between the separated entries. That is, the default gap of 1 will add a blank gap of one entry
between adjacent entries of different groups. A gap of `nothing` will not be shown.

If you specify `..._dendogram_size`, then you should either specify linkage (for computing a clustering) or must specify
`Hclust` order in the data. The dendogram tree will be shown to the side of the data. The size is specified in the usual
inconvenient units (fractions of the total graph size) because Plotly.

If a dendogram tree is shown, the `..._dendogram_line` can be used to control it. The default color is black. The
`is_filled` field shouldn't be set as it has no meaning here.
"""
@kwdef mutable struct HeatmapGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    x_axis_title::Maybe{AbstractString} = nothing
    y_axis_title::Maybe{AbstractString} = nothing
    entries_colors::ColorsConfiguration = ColorsConfiguration()
    rows_annotations::AnnotationSize = AnnotationSize()
    columns_annotations::AnnotationSize = AnnotationSize()
    rows_reorder::Maybe{HeatmapReorder} = nothing
    columns_reorder::Maybe{HeatmapReorder} = nothing
    rows_linkage::Maybe{HeatmapLinkage} = nothing
    columns_linkage::Maybe{HeatmapLinkage} = nothing
    rows_metric::Maybe{PreMetric} = nothing
    columns_metric::Maybe{PreMetric} = nothing
    rows_groups_gap::Maybe{Integer} = 1
    columns_groups_gap::Maybe{Integer} = 1
    rows_dendogram_size::Maybe{Real} = nothing
    columns_dendogram_size::Maybe{Real} = nothing
    rows_dendogram_line::LineConfiguration = LineConfiguration()
    columns_dendogram_line::LineConfiguration = LineConfiguration()
    origin::HeatmapOrigin = HeatmapBottomLeft
end

function Validations.validate(context::ValidationContext, configuration::HeatmapGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "entries_colors", configuration.entries_colors)
    validate_field(context, "rows_annotations", configuration.rows_annotations)
    validate_field(context, "columns_annotations", configuration.columns_annotations)

    if configuration.entries_colors.fixed !== nothing
        throw(ArgumentError("can't specify heatmap $(location(context)).entries_colors.fixed"))
    end

    if configuration.entries_colors.palette isa CategoricalColors
        throw(ArgumentError("can't specify heatmap categorical $(location(context)).entries_colors.palette"))
    end

    validate_in(context, "rows_groups_gap") do
        return validate_is_above(context, configuration.rows_groups_gap, 0)
    end
    validate_in(context, "columns_groups_gap") do
        return validate_is_above(context, configuration.columns_groups_gap, 0)
    end

    validate_in(context, "rows_dendogram_size") do
        return validate_is_above(context, configuration.rows_dendogram_size, 0)
    end
    validate_in(context, "columns_dendogram_size") do
        return validate_is_above(context, configuration.columns_dendogram_size, 0)
    end

    for (name, dendogram_line, dendogram_size) in (
        ("rows", configuration.rows_dendogram_line, configuration.rows_dendogram_size),
        ("columns", configuration.columns_dendogram_line, configuration.columns_dendogram_size),
    )
        if dendogram_line.is_filled
            throw(ArgumentError("can't specify heatmap $(location(context)).$(name)_dendogram_line.is_filled"))
        end

        if dendogram_size === nothing &&
           (dendogram_line.width !== nothing || dendogram_line.style != SolidLine || dendogram_line.color !== nothing)
            throw(ArgumentError(chomp("""
                                      can't specify heatmap $(location(context)).$(name)_dendogram_line.*
                                      without $(location(context)).$(name)_dendogram_size
                                      """)))
        end
    end

    if configuration.rows_reorder == SameOrder && configuration.columns_reorder == SameOrder
        throw(ArgumentError(chomp("""
                                  can't specify both heatmap $(location(context)).rows_reorder: SameOrder
                                  and heatmap $(location(context)).columns_reorder: SameOrder
                                  """)))
    end

    return nothing
end

"""
    @kwdef mutable struct HeatmapGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        x_axis_title::Maybe{AbstractString} = nothing
        y_axis_title::Maybe{AbstractString} = nothing
        entries_colors_title::Maybe{AbstractString} = nothing
        entries_values::Maybe{AbstractMatrix{<:Real}} = Float32[;;]
        rows_names::Maybe{AbstractVector{<:AbstractString}} = nothing
        columns_names::Maybe{AbstractVector{<:AbstractString}} = nothing
        entries_hovers::Maybe{AbstractMatrix{<:AbstractString}} = nothing
        rows_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
        columns_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
        rows_annotations::AbstractVector{AnnotationData} = AnnotationData[]
        columns_annotations::AbstractVector{AnnotationData} = AnnotationData[]
        rows_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
        columns_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
        rows_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
        columns_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
        rows_groups::Maybe{AbstractVector} = nothing
        columns_groups::Maybe{AbstractVector} = nothing
    end

The data for a graph showing a heatmap (matrix) of entries.

This is shown as a 2D image where each matrix entry is a small rectangle with some color. Due to Plotly limitation,
colors must be continuous. The hover for each rectangle is a combination of the `entries_hovers`, `rows_hovers`
and `columns_hovers` for the entry.

By default, if reordering the data, this is based on the `entries_values`. You can override this by specifying an an
`..._arrange_by` matrix of the same size. For efficiency the `rows_arrange_by` matrix should be in row-major layout, but
that's not critical.

Alternatively you can force the order of the data by specifying the `..._order` permutation. You can also specify an
`Hclust` object as the order. If you ask for a dendogram and did not specify such a clustering, one will be computed.

If `..._groups` are specified, then a gap can be added between entries of different groups. Groups can also be
used to constrain the computed clustering.

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
    x_axis_title::Maybe{AbstractString} = nothing
    y_axis_title::Maybe{AbstractString} = nothing
    entries_colors_title::Maybe{AbstractString} = nothing
    entries_values::Maybe{AbstractMatrix{<:Real}} = Float32[;;]
    rows_names::Maybe{AbstractVector{<:AbstractString}} = nothing
    columns_names::Maybe{AbstractVector{<:AbstractString}} = nothing
    entries_hovers::Maybe{AbstractMatrix{<:AbstractString}} = nothing
    rows_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
    columns_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
    rows_annotations::AbstractVector{AnnotationData} = AnnotationData[]
    columns_annotations::AbstractVector{AnnotationData} = AnnotationData[]
    rows_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
    columns_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
    rows_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
    columns_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
    rows_groups::Maybe{AbstractVector} = nothing
    columns_groups::Maybe{AbstractVector} = nothing
end

function Validations.validate(context::ValidationContext, data::HeatmapGraphData)::Nothing
    n_rows, n_columns = size(data.entries_values)

    validate_matrix_size(context, "rows_arrange_by", data.rows_arrange_by, "entries_values", size(data.entries_values))
    validate_matrix_size(
        context,
        "columns_arrange_by",
        data.columns_arrange_by,
        "entries_values",
        size(data.entries_values),
    )

    validate_vector_length(context, "rows_names", data.rows_names, "entries_values.rows", n_rows)
    validate_vector_length(context, "columns_names", data.columns_names, "entries_values.columns", n_columns)

    validate_matrix_size(context, "entries_hovers", data.entries_hovers, "entries_values", size(data.entries_values))
    validate_vector_length(context, "rows_hovers", data.rows_hovers, "entries_values.rows", n_rows)
    validate_vector_length(context, "columns_hovers", data.columns_hovers, "entries_values.columns", n_columns)

    validate_vector_length(context, "rows_groups", data.rows_groups, "entries_values.rows", n_rows)
    validate_vector_length(context, "columns_groups", data.columns_groups, "entries_values.columns", n_columns)

    if data.rows_order isa Hclust
        rows_order = data.rows_order.order  # UNTESTED
    else
        rows_order = data.rows_order
    end
    validate_vector_length(context, "rows_order", rows_order, "entries_values.rows", n_rows)

    if data.columns_order isa Hclust
        columns_order = data.columns_order.order
    else
        columns_order = data.columns_order
    end
    validate_vector_length(context, "columns_order", columns_order, "entries_values.columns", n_columns)

    validate_vector_entries(context, "rows_annotations", data.rows_annotations) do _, rows_annotation
        validate(context, rows_annotation, "entries_values.rows", n_rows)
        return nothing
    end

    validate_vector_entries(context, "columns_annotations", data.columns_annotations) do _, columns_annotation
        validate(context, columns_annotation, "entries_values.columns", n_columns)
        return nothing
    end

    return nothing
end

"""
A graph showing a heatmap. See [`HeatmapGraphData`](@ref) and [`HeatmapGraphConfiguration`](@ref).
"""
HeatmapGraph = Graph{HeatmapGraphData, HeatmapGraphConfiguration}

"""
    function heatmap_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        x_axis_title::Maybe{AbstractString} = nothing,
        y_axis_title::Maybe{AbstractString} = nothing,
        entries_colors_title::Maybe{AbstractString} = nothing,
        entries_values::Maybe{AbstractMatrix{<:Real}} = Float32[;;],
        rows_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
        columns_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
        entries_hovers::Maybe{AbstractMatrix{<:AbstractString}} = nothing,
        rows_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
        columns_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
        rows_annotations::AbstractVector{AnnotationData} = AnnotationData[],
        columns_annotations::AbstractVector{AnnotationData} = AnnotationData[],
        rows_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing,
        columns_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing,
        rows_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing,
        columns_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing,
        rows_groups::Maybe{AbstractVector} = nothing,
        columns_groups::Maybe{AbstractVector} = nothing,
        configuration::HeatmapGraphConfiguration = HeatmapGraphConfiguration()]
    )::HeatmapGraph

Create a [`HeatmapGraph`](@ref) by initializing only the [`HeatmapGraphData`](@ref) fields (with an optional
[`HeatmapGraphConfiguration`](@ref)).
"""
function heatmap_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    x_axis_title::Maybe{AbstractString} = nothing,
    y_axis_title::Maybe{AbstractString} = nothing,
    entries_colors_title::Maybe{AbstractString} = nothing,
    entries_values::Maybe{AbstractMatrix{<:Real}} = Float32[;;],
    rows_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    columns_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    entries_hovers::Maybe{AbstractMatrix{<:AbstractString}} = nothing,
    rows_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    columns_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    rows_annotations::AbstractVector{AnnotationData} = AnnotationData[],
    columns_annotations::AbstractVector{AnnotationData} = AnnotationData[],
    rows_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing,
    columns_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing,
    rows_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing,
    columns_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing,
    rows_groups::Maybe{AbstractVector} = nothing,
    columns_groups::Maybe{AbstractVector} = nothing,
    configuration::HeatmapGraphConfiguration = HeatmapGraphConfiguration(),
)::HeatmapGraph
    return HeatmapGraph(
        HeatmapGraphData(;
            figure_title,
            x_axis_title,
            y_axis_title,
            entries_colors_title,
            entries_values,
            rows_names,
            columns_names,
            entries_hovers,
            rows_hovers,
            columns_hovers,
            rows_annotations,
            columns_annotations,
            rows_arrange_by,
            columns_arrange_by,
            rows_order,
            columns_order,
            rows_groups,
            columns_groups,
        ),
        configuration,
    )
end

function Common.validate_graph(graph::HeatmapGraph)::Nothing
    validate_colors(
        ValidationContext(["graph.data.entries_values"]),
        graph.data.entries_values,
        ValidationContext(["graph.configuration.entries_colors"]),
        graph.configuration.entries_colors,
    )

    validate_axis_sizes(;
        axis_name = "columns",
        annotation_size = graph.configuration.columns_annotations,
        n_annotations = length(graph.data.columns_annotations),
        dendogram_size = graph.configuration.rows_dendogram_size,
    )

    validate_axis_sizes(;
        axis_name = "rows",
        annotation_size = graph.configuration.rows_annotations,
        n_annotations = length(graph.data.rows_annotations),
        dendogram_size = graph.configuration.columns_dendogram_size,
    )

    n_rows, n_columns = size(graph.data.entries_values)
    if n_rows != n_columns
        for (name, value) in
            (("rows", graph.configuration.rows_reorder), ("columns", graph.configuration.columns_reorder))
            if value == SameOrder
                throw(ArgumentError(chomp("""
                                          can't specify heatmap graph.configuration.$(name)_reorder: SameOrder
                                          for a non-square matrix: $(n_rows) rows x $(n_columns) columns
                                          """)))
            end
        end
    end

    for (
        name,
        data_order,
        data_arrange_by,
        data_groups,
        configuration_reorder,
        configuration_linkage,
        configuration_metric,
        configuration_groups_gap,
        configuration_dendogram_size,
        other_name,
        other_data_order,
        other_configuration_reorder,
        other_configuration_dendogram_size,
    ) in (
        (
            "columns",
            graph.data.columns_order,
            graph.data.columns_arrange_by,
            graph.data.columns_groups,
            graph.configuration.columns_reorder,
            graph.configuration.columns_linkage,
            graph.configuration.columns_metric,
            graph.configuration.columns_groups_gap,
            graph.configuration.columns_dendogram_size,
            "rows",
            graph.data.rows_order,
            graph.configuration.rows_reorder,
            graph.configuration.rows_dendogram_size,
        ),
        (
            "rows",
            graph.data.rows_order,
            graph.data.rows_arrange_by,
            graph.data.rows_groups,
            graph.configuration.rows_reorder,
            graph.configuration.rows_linkage,
            graph.configuration.rows_metric,
            graph.configuration.rows_groups_gap,
            graph.configuration.rows_dendogram_size,
            "columns",
            graph.data.columns_order,
            graph.configuration.columns_reorder,
            graph.configuration.columns_dendogram_size,
        ),
    )
        is_using_groups = configuration_groups_gap !== nothing
        if data_order === nothing
            if configuration_reorder === nothing
                if configuration_dendogram_size === nothing
                    if data_arrange_by !== nothing
                        throw(ArgumentError(chomp("""
                                                  can't specify heatmap graph.data.$(name)_arrange_by
                                                  without graph.configuration.$(name)_dendogram_size
                                                  or graph.configuration.$(name)_reorder
                                                  """)))
                    end
                    if configuration_linkage !== nothing
                        throw(ArgumentError(chomp("""
                                                  can't specify heatmap graph.configuration.$(name)_linkage
                                                  without graph.configuration.$(name)_dendogram_size
                                                  or graph.configuration.$(name)_reorder
                                                  """)))
                    end
                    if configuration_metric !== nothing
                        throw(ArgumentError(chomp("""
                                                  can't specify heatmap graph.configuration.$(name)_metric
                                                  without graph.configuration.$(name)_dendogram_size
                                                  or graph.configuration.$(name)_reorder
                                                  """)))
                    end
                end

            elseif configuration_reorder == SameOrder
                if data_arrange_by !== nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.data.$(name)_arrange_by
                                              for graph.configuration.$(name)_reorder: $(configuration_reorder)
                                              """)))
                end
                if configuration_linkage !== nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.configuration.$(name)_linkage
                                              for graph.configuration.$(name)_reorder: $(configuration_reorder)
                                              """)))
                end
                if configuration_metric !== nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.configuration.$(name)_metric
                                              for graph.configuration.$(name)_reorder: $(configuration_reorder)
                                              """)))
                end
                if other_data_order === nothing && other_configuration_reorder === nothing
                    throw(
                        ArgumentError(
                            chomp("""
                                  can't specify heatmap graph.configuration.$(name)_reorder: $(configuration_reorder)
                                  without an order to copy from the $(other_name)
                                  """),
                        ),
                    )
                end
                if configuration_dendogram_size !== nothing &&
                   !(other_data_order isa Hclust) &&
                   other_configuration_reorder === nothing &&
                   other_configuration_dendogram_size === nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.configuration.$(name)_dendogram_size
                                              with graph.configuration.$(name)_reorder: $(configuration_reorder)
                                              without a tree to copy from the $(other_name)
                                              """)))
                end

            elseif configuration_reorder in (OptimalHclust, RCompatibleHclust, SlantedHclust, SlantedPreSquaredHclust)
                is_using_groups = true

            elseif !(configuration_reorder in (SlantedOrder, SlantedPreSquaredOrder))
                throw(
                    ArgumentError(
                        chomp("""
                              can't specify heatmap graph.configuration.$(name)_reorder: $(configuration_reorder)
                              without explicit vector graph.data.$(name)_order
                              """),
                    ),
                )
            end

        elseif data_order isa Hclust
            if configuration_linkage !== nothing
                throw(ArgumentError(chomp("""
                                          can't specify heatmap graph.configuration.$(name)_linkage
                                          for explicit hclust graph.data.$(name)_order
                                          """)))
            end
            if configuration_metric !== nothing
                throw(ArgumentError(chomp("""
                                          can't specify heatmap graph.configuration.$(name)_metric
                                          for explicit hclust graph.data.$(name)_order
                                          """)))
            end
            if !(configuration_reorder in (nothing, SlantedHclust, SlantedPreSquaredHclust))
                throw(
                    ArgumentError(
                        chomp("""
                              can't specify heatmap graph.configuration.$(name)_reorder: $(configuration_reorder)
                              for explicit hclust graph.data.$(name)_order
                              """),
                    ),
                )
            end
            if configuration_reorder === nothing && data_arrange_by !== nothing
                throw(ArgumentError(chomp("""
                                          can't specify heatmap graph.data.$(name)_arrange_by
                                          without graph.configuration.$(name)_reorder
                                          for explicit hclust graph.data.$(name)_order
                                          """)))
            end

        elseif data_order isa AbstractVector
            if data_arrange_by !== nothing
                throw(ArgumentError(chomp("""
                                          can't specify heatmap graph.data.$(name)_arrange_by
                                          for explicit vector graph.data.$(name)_order
                                          """)))
            end

            if configuration_reorder == ReorderHclust
                is_using_groups = true

            elseif configuration_reorder !== nothing
                throw(
                    ArgumentError(
                        chomp("""
                              can't specify heatmap graph.configuration.$(name)_reorder: $(configuration_reorder)
                              for explicit vector graph.data.$(name)_order
                              """),
                    ),
                )
            end

            if configuration_dendogram_size === nothing
                if configuration_linkage !== nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.configuration.$(name)_linkage
                                              for explicit vector graph.data.$(name)_order
                                              without graph.configuration.$(name)_dendogram_size
                                              """)))
                end
                if configuration_metric !== nothing
                    throw(ArgumentError(chomp("""
                                              can't specify heatmap graph.configuration.$(name)_metric
                                              for explicit vector graph.data.$(name)_order
                                              without graph.configuration.$(name)_dendogram_size
                                              """)))
                end
            end

        else
            @assert false
        end

        if !is_using_groups && data_groups !== nothing
            throw(ArgumentError("no effect for specified graph.data.$(name)_groups"))
        end
    end

    return nothing
end

function Common.graph_to_figure(graph::HeatmapGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    next_colors_scale_index = [1]
    colors = configured_colors(;
        colors_configuration = graph.configuration.entries_colors,
        colors_title = prefer_data(graph.data.entries_colors_title, graph.configuration.entries_colors.title),
        colors_values = graph.data.entries_values,
        next_colors_scale_index,
    )

    rows_order, rows_hclust, columns_order, columns_hclust, z = reorder_data(graph, colors)

    n_rows_annotations = length(graph.data.rows_annotations)
    n_columns_annotations = length(graph.data.columns_annotations)

    columns_sub_graph = SubGraph(;
        index = 1,
        n_graphs = 1,
        graphs_gap = nothing,
        n_annotations = n_rows_annotations,
        annotation_size = graph.configuration.rows_annotations,
        dendogram_size = graph.configuration.rows_dendogram_size,
    )

    rows_sub_graph = SubGraph(;
        index = 1,
        n_graphs = 1,
        graphs_gap = nothing,
        n_annotations = n_columns_annotations,
        annotation_size = graph.configuration.columns_annotations,
        dendogram_size = graph.configuration.columns_dendogram_size,
    )

    xaxis_index, _, yaxis_index, _ = plotly_sub_graph_axes(;
        basis_sub_graph = columns_sub_graph,
        values_sub_graph = rows_sub_graph,
        values_orientation = VerticalValues,
    )

    expanded_rows_mask = compute_expansion_mask(rows_order, graph.data.rows_groups, graph.configuration.rows_groups_gap)
    expanded_columns_mask =
        compute_expansion_mask(columns_order, graph.data.columns_groups, graph.configuration.columns_groups_gap)

    expanded_z = expand_z_matrix(z, rows_order, expanded_rows_mask, columns_order, expanded_columns_mask)

    n_expanded_rows, n_expanded_columns = size(expanded_z)

    rows_hovers = graph.data.rows_hovers
    if rows_hovers !== nothing && rows_order !== nothing
        rows_hovers = rows_hovers[rows_order]  # UNTESTED
    end

    columns_hovers = graph.data.columns_hovers
    if columns_hovers !== nothing && columns_order !== nothing
        columns_hovers = columns_hovers[columns_order]  # UNTESTED
    end

    entries_hovers = graph.data.entries_hovers
    if entries_hovers !== nothing
        if rows_order !== nothing && columns_order !== nothing
            entries_hovers = entries_hovers[rows_order, columns_order]  # UNTESTED
        elseif rows_order !== nothing
            entries_hovers = entries_hovers[rows_order, :]  # UNTESTED
        elseif columns_order !== nothing
            entries_hovers = entries_hovers[:, columns_order]  # UNTESTED
        end
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
        annotations_data = graph.data.columns_annotations,
        annotation_size = graph.configuration.columns_annotations,
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
        annotations_data = graph.data.rows_annotations,
        annotation_size = graph.configuration.rows_annotations,
        order = rows_order,
        expanded_mask = expanded_rows_mask,
    )

    if graph.configuration.rows_dendogram_size !== nothing
        rows_max_height = push_dendogram_trace!(;
            traces,
            clusters = rows_hclust,
            values_orientation = HorizontalValues,
            dendogram_line = graph.configuration.rows_dendogram_line,
            expanded_mask = expanded_rows_mask,
            basis_sub_graph = rows_sub_graph,
            values_sub_graph = SubGraph(;
                index = 0,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_rows_annotations,
                annotation_size = graph.configuration.rows_annotations,
                dendogram_size = graph.configuration.rows_dendogram_size,
            ),
        )
    else
        rows_max_height = 0
    end

    if graph.configuration.columns_dendogram_size !== nothing
        columns_max_height = push_dendogram_trace!(;
            traces,
            clusters = columns_hclust,
            values_orientation = VerticalValues,
            dendogram_line = graph.configuration.columns_dendogram_line,
            expanded_mask = expanded_columns_mask,
            basis_sub_graph = columns_sub_graph,
            values_sub_graph = SubGraph(;
                index = 0,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_columns_annotations,
                annotation_size = graph.configuration.columns_annotations,
                dendogram_size = graph.configuration.columns_dendogram_size,
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
        graph.data.entries_hovers !== nothing ||
        graph.data.rows_hovers !== nothing ||
        graph.data.columns_hovers !== nothing

    layout = plotly_layout(graph.configuration.figure; title = graph.data.figure_title, has_legend, has_hovers)

    expanded_rows_names = expand_vector(graph.data.rows_names, rows_order, expanded_rows_mask, "")
    set_layout_axis!(
        layout,
        plotly_axis("y", yaxis_index),
        AxisConfiguration(; show_grid = false, show_ticks = graph.data.rows_names !== nothing);
        title = prefer_data(graph.data.y_axis_title, graph.configuration.y_axis_title),
        ticks_values = expanded_rows_names === nothing ? nothing : collect(1:n_expanded_rows),
        ticks_labels = expanded_rows_names,
        range = Range(; minimum = 0.5, maximum = n_expanded_rows + 0.5),
        domain = plotly_sub_graph_domain(
            SubGraph(;
                index = 1,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_columns_annotations,
                annotation_size = graph.configuration.columns_annotations,
                dendogram_size = graph.configuration.columns_dendogram_size,
            ),
        ),
        is_zeroable = false,
    )

    expanded_columns_names = expand_vector(graph.data.columns_names, columns_order, expanded_columns_mask, "")
    set_layout_axis!(
        layout,
        plotly_axis("x", xaxis_index),
        AxisConfiguration(; show_grid = false, show_ticks = graph.data.columns_names !== nothing);
        title = prefer_data(graph.data.x_axis_title, graph.configuration.x_axis_title),
        ticks_values = expanded_columns_names === nothing ? nothing : collect(1:n_expanded_columns),
        ticks_labels = expanded_columns_names,
        range = Range(; minimum = 0.5, maximum = n_expanded_columns + 0.5),
        domain = plotly_sub_graph_domain(
            SubGraph(;
                index = 1,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_rows_annotations,
                annotation_size = graph.configuration.rows_annotations,
                dendogram_size = graph.configuration.rows_dendogram_size,
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
            graph.data.columns_annotations,
            columns_annotations_colors,
            graph.configuration.columns_annotations,
            graph.configuration.columns_dendogram_size,
            columns_max_height,
        ),
        (
            "x",
            HorizontalValues,
            graph.data.rows_annotations,
            rows_annotations_colors,
            graph.configuration.rows_annotations,
            graph.configuration.rows_dendogram_size,
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
                    title = annotation_data.title,
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
                        annotation_data.title,
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

function reorder_data(
    graph::HeatmapGraph,
    colors::ConfiguredColors,
)::Tuple{
    Maybe{AbstractVector{<:Integer}},
    Maybe{Hclust},
    Maybe{AbstractVector{<:Integer}},
    Maybe{Hclust},
    AbstractMatrix{<:Real},
}
    data_rows_arrange_by = prefer_data(graph.data.rows_arrange_by, graph.data.entries_values)
    data_columns_arrange_by = prefer_data(graph.data.columns_arrange_by, graph.data.entries_values)
    @assert data_rows_arrange_by !== nothing
    @assert data_columns_arrange_by !== nothing

    slant_rows = (
        graph.configuration.rows_reorder in
        (SlantedHclust, SlantedPreSquaredHclust, SlantedOrder, SlantedPreSquaredOrder)
    )
    slant_columns = (
        graph.configuration.columns_reorder in
        (SlantedHclust, SlantedPreSquaredHclust, SlantedOrder, SlantedPreSquaredOrder)
    )

    slant_rows_is_pre_squared = graph.configuration.rows_reorder in (SlantedPreSquaredHclust, SlantedPreSquaredOrder)
    slant_columns_is_pre_squared =
        graph.configuration.columns_reorder in (SlantedPreSquaredHclust, SlantedPreSquaredOrder)

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
            if graph.configuration.columns_reorder == SameOrder
                slant_rows_order, slant_columns_order =
                    slanted_orders(data_rows_arrange_by; same_order = true, squared_order = !slant_rows_is_pre_squared)
            else
                slant_rows_order, _ =
                    slanted_orders(data_rows_arrange_by; order_cols = false, squared_order = !slant_rows_is_pre_squared)
            end
        end

        if slant_columns
            if graph.configuration.rows_reorder == SameOrder
                slant_rows_order, slant_columns_order = slanted_orders(
                    data_columns_arrange_by;
                    same_order = true,
                    squared_order = slant_columns_is_pre_squared,
                )
            else
                _, slant_columns_order = slanted_orders(
                    data_columns_arrange_by;
                    order_rows = false,
                    squared_order = slant_columns_is_pre_squared,
                )
            end
        end
    end

    data_columns_order, data_columns_hclust = finalize_order(;
        data_order = graph.data.columns_order,
        data_arrange_by = data_columns_arrange_by,
        data_groups = graph.data.columns_groups,
        slant_order = slant_columns_order,
        configuration_reorder = graph.configuration.columns_reorder,
        configuration_dendogram_size = graph.configuration.columns_dendogram_size,
        configuration_linkage = graph.configuration.columns_linkage,
        configuration_metric = graph.configuration.columns_metric,
    )

    data_rows_order, data_rows_hclust = finalize_order(;
        data_order = graph.data.rows_order,
        data_arrange_by = PermutedDimsArray(data_rows_arrange_by, (2, 1)),
        data_groups = graph.data.rows_groups,
        slant_order = slant_rows_order,
        configuration_reorder = graph.configuration.rows_reorder,
        configuration_dendogram_size = graph.configuration.rows_dendogram_size,
        configuration_linkage = graph.configuration.rows_linkage,
        configuration_metric = graph.configuration.rows_metric,
    )

    if graph.configuration.rows_reorder == SameOrder
        @assert data_rows_order === nothing
        @assert data_rows_hclust === nothing
        data_rows_order = data_columns_order
        data_rows_hclust = data_columns_hclust
    end

    if graph.configuration.columns_reorder == SameOrder
        @assert data_columns_order === nothing
        @assert data_columns_hclust === nothing
        data_columns_order = data_rows_order
        data_columns_hclust = data_rows_hclust
    end

    n_rows, n_columns = size(graph.data.entries_values)

    if graph.configuration.origin in (HeatmapTopLeft, HeatmapTopRight)
        if data_rows_order === nothing
            data_rows_order = collect(1:n_rows)
        end
        data_rows_order = reverse(data_rows_order)
    end

    if graph.configuration.origin in (HeatmapBottomRight, HeatmapTopRight)
        if data_columns_order === nothing
            data_columns_order = collect(1:n_columns)
        end
        data_columns_order = reverse(data_columns_order)
    end

    if data_rows_order === nothing
        if data_columns_order === nothing
            z = colors.final_colors_values
        else
            z = colors.final_colors_values[:, data_columns_order]
        end
    else
        if data_columns_order === nothing
            z = colors.final_colors_values[data_rows_order, :]
        else
            z = colors.final_colors_values[data_rows_order, data_columns_order]
        end
    end

    return (data_rows_order, data_rows_hclust, data_columns_order, data_columns_hclust, z)
end

function finalize_order(;
    data_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}},
    data_arrange_by::AbstractMatrix{<:Real},
    data_groups::Maybe{Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}}},
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
                branchorder = hclust_branchorder(configuration_reorder),
            )
            return (clusters.order, clusters)

        elseif configuration_reorder in (SlantedHclust, SlantedPreSquaredHclust)
            @assert slant_order !== nothing
            distances = pairwise(configuration_metric, data_arrange_by; dims = 2)
            clusters = ehclust(distances; linkage = hclust_linkage(configuration_linkage), groups = data_groups)
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
            clusters = ehclust(distances; groups = data_groups)
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
        return :single  # UNTESTED
    elseif linkage == AverageLinkage
        return :average  # UNTESTED
    elseif linkage == CompleteLinkage
        return :complete  # UNTESTED
    elseif linkage == WardLinkage
        return :ward
    elseif linkage == WardPreSquaredLinkage  # UNTESTED
        return :ward_presquared  # UNTESTED
    else
        @assert false
    end
end

function hclust_branchorder(reorder::HeatmapReorder)::Symbol
    if reorder == RCompatibleHclust
        return :r  # UNTESTED
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

    return maximum(heights)
end

function dendogram_coordinates(
    clusters::Hclust,
    expanded_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
)::Tuple{AbstractVector{<:AbstractFloat}, AbstractVector{<:AbstractFloat}}
    values = Float32[]
    heights = Float32[]

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

        push!(values, left_value, left_value, right_value, right_value, NaN)
        push!(heights, left_height, height, height, right_height, NaN)

        value_per_node[merge_index + n_values] = middle_value
        height_per_node[merge_index + n_values] = height
    end

    return (values, heights)
end

function compute_expansion_mask(
    order::Maybe{AbstractVector{<:Integer}},
    groups::Maybe{AbstractVector},
    groups_gap::Maybe{Integer},
)::Maybe{Union{BitVector, AbstractVector{Bool}}}
    if groups === nothing || groups_gap === nothing
        return nothing
    end

    @assert groups_gap > 0

    if order === nothing
        order = 1:length(groups)
    end

    expanded_mask = Bool[]

    prev_group = groups[order[1]]
    for group in groups[order]
        if group != prev_group
            prev_group = group
            for _ in 1:groups_gap
                push!(expanded_mask, false)
            end
        end
        push!(expanded_mask, true)
    end

    return expanded_mask
end

function expand_z_matrix(
    z::AbstractMatrix{<:Real},
    rows_order::Maybe{AbstractVector{<:Integer}},
    expanded_rows_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
    columns_order::Maybe{AbstractVector{<:Integer}},
    expanded_columns_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
)::AbstractMatrix{<:Real}
    if rows_order === nothing &&
       expanded_rows_mask === nothing &&
       columns_order === nothing &&
       expanded_columns_mask === nothing
        return z
    end

    if expanded_rows_mask === nothing && expanded_columns_mask === nothing
        return z
    end

    n_rows, n_columns = size(z)

    if expanded_rows_mask === nothing
        n_expanded_rows = n_rows  # UNTESTED
        expanded_rows_mask = 1:n_rows  # UNTESTED
    else
        n_expanded_rows = length(expanded_rows_mask)
    end

    if expanded_columns_mask === nothing
        n_expanded_columns = n_columns  # UNTESTED
        expanded_columns_mask = 1:n_columns  # UNTESTED
    else
        n_expanded_columns = length(expanded_columns_mask)
    end

    expanded_z = Matrix{eltype(z)}(undef, n_expanded_rows, n_expanded_columns)
    expanded_z .= NaN
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
                if row_hover !== ""
                    push!(text, row_hover)
                end
            end

            if column_hover !== nothing
                push!(text, column_hover)
            end

            expanded_hovers[row_position, column_position] = join(text, "<br>")
        end
    end

    return expanded_hovers
end

end
