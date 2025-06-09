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
export SameOrder
export SingleLinkage
export SlantedHclust
export SlantedOrder
export SlantedPreSquaredHclust
export SlantedPreSquaredOrder
export WardLinkage
export WardPreSeuaredLinkage

using ..Common
using ..Utilities
using ..Validations

using Clustering
using Distances
using PlotlyJS
using Slanter

import ..Bars.push_annotations_traces
import ..Validations.Maybe

"""
Specify how to reorder the rows and/or columns.

  - `OptimalHclust` orders `hclust` branches using the (better) Bar-Joseph method.
  - `RCompatibleHclust` orders `hclust` branches in the same (bad) way that `R` does.
  - `SlantedHclust` and `SlantedPreSquaredHclust` orders `hclust` branches using `Slanter` (using `slanted_orders`
    and `reorder_hclust`).
  - `SlantedOrder` and `SlantedPreSquaredOrder` uses `slanted_orders` (if a tree is needed, uses `oclust` to create
    a tree preserving this order).
  - `SameOrder` orders the rows/columns in the same way as the other axis. This can only be applied to square matrices
    and can't be specified for both axes.
"""
@enum HeatmapReorder RCompatibleHclust OptimalHclust SlantedHclust SlantedPreSquaredHclust SlantedOrder SlantedPreSquaredOrder SameOrder

"""
Specify the linkage to use when performing hierarchical clustering (`hclust`). The default is `WardLinkage`. If using
`oclust` (from `Slanter`), then only `WardLinkage` and `WardPreSeuaredLinkage` are supported.
"""
@enum HeatmapLinkage SingleLinkage AverageLinkage CompleteLinkage WardLinkage WardPreSeuaredLinkage

"""
Specify where the origin (row 1 column 1) should be displayed. The Plotly default is `HeatmapBottomLeft`.
"""
@enum HeatmapOrigin HeatmapTopLeft HeatmapTopRight HeatmapBottomLeft HeatmapBottomRight

"""
    @kwdef mutable struct HeatmapGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration()
        entries_colors::ColorsConfiguration = ColorsConfiguration()
        rows_annotations::AnnotationSize = AnnotationSize()
        columns_annotations::AnnotationSize = AnnotationSize()
        entries_reorder::Maybe{HeatmapReorder} = nothing
        rows_reorder::Maybe{HeatmapReorder} = nothing
        columns_reorder::Maybe{HeatmapReorder} = nothing
        entries_linkage::Maybe{HeatmapLinkage} = nothing
        rows_linkage::Maybe{HeatmapLinkage} = nothing
        columns_linkage::Maybe{HeatmapLinkage} = nothing
        entries_metric::Maybe{PreMetric} = nothing
        rows_metric::Maybe{PreMetric} = nothing
        columns_metric::Maybe{PreMetric} = nothing
        origin::HeatmapOrigin = HeatmapBottomLeft
    end

Configure a graph showing a heatmap.

This displays a matrix of values using a rectangle at each position. Due to Plotly's limitations, you still to manually
tweak the graph size for best results; there's no way to directly control the width and height of the rectangles. In
addition, the only supported color configurations are using continuous color palettes.

You can use `columns_reorder` and/or `rows_reorder` to reorder the data. When specifying `columns_linkage` and/or
`rows_linkage`, by default, the clustering uses the `Euclidean` distance metric. You can override this by specifying the
`columns_metric` and/or `rows_metric` distance measure.

If you specify `columns_dendogram_size` and/or `rows_dendogram_size`, then you should either specify linkage (for
computing a clustering) or must specify `Hclust` order in the data. The dendogram tree will be shown to the side of the
data. The size is specified in the usual inconvenient units (fractions of the total graph size) because Plotly.

If a dendogram tee is shown, the `dendogram_line` can be used to control it. The `is_filled` field shouldn't be set as
it has no meaning here.

TODO: Implement dendograms.

Specifying `entries_reorder` is equivalent to specifying both `rows_reorder` and `columns_reorder`, and similarly for
`entries_linkage`, `entries_dendogram_size`, and `entries_metric`.

(which you can override in the data as usual).
"""
@kwdef mutable struct HeatmapGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    x_axis_title::Maybe{AbstractString} = nothing
    y_axis_title::Maybe{AbstractString} = nothing
    entries_colors::ColorsConfiguration = ColorsConfiguration()
    rows_annotations::AnnotationSize = AnnotationSize()
    columns_annotations::AnnotationSize = AnnotationSize()
    entries_reorder::Maybe{HeatmapReorder} = nothing
    rows_reorder::Maybe{HeatmapReorder} = nothing
    columns_reorder::Maybe{HeatmapReorder} = nothing
    entries_linkage::Maybe{HeatmapLinkage} = nothing
    rows_linkage::Maybe{HeatmapLinkage} = nothing
    columns_linkage::Maybe{HeatmapLinkage} = nothing
    entries_metric::Maybe{PreMetric} = nothing
    rows_metric::Maybe{PreMetric} = nothing
    columns_metric::Maybe{PreMetric} = nothing
    entries_dendogram_size::Maybe{Real} = nothing
    rows_dendogram_size::Maybe{Real} = nothing
    columns_dendogram_size::Maybe{Real} = nothing
    dendogram_line::LineConfiguration = LineConfiguration()
    origin::HeatmapOrigin = HeatmapBottomLeft
end

function Validations.validate(context::ValidationContext, configuration::HeatmapGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "entries_colors", configuration.entries_colors)
    validate_field(context, "columns_annotations", configuration.columns_annotations)
    validate_field(context, "rows_annotations", configuration.rows_annotations)

    if configuration.entries_colors.fixed !== nothing
        throw(ArgumentError("can't specify heatmap $(location(context)).entries_colors.fixed"))
    end

    if configuration.entries_colors.palette isa CategoricalColors
        throw(ArgumentError("can't specify heatmap categorical $(location(context)).entries_colors.palette"))
    end

    validate_exclusive_entries(
        context,
        "reorder",
        configuration.entries_reorder,
        configuration.rows_reorder,
        configuration.columns_reorder,
    )
    validate_exclusive_entries(
        context,
        "linkage",
        configuration.entries_linkage,
        configuration.rows_linkage,
        configuration.columns_linkage,
    )
    validate_exclusive_entries(
        context,
        "dendogram_size",
        configuration.entries_dendogram_size,
        configuration.rows_dendogram_size,
        configuration.columns_dendogram_size,
    )
    validate_exclusive_entries(
        context,
        "metric",
        configuration.entries_metric,
        configuration.rows_metric,
        configuration.columns_metric,
    )

    validate_in(context, "entries_dendogram_size") do
        return validate_is_above(context, configuration.entries_dendogram_size, 0)
    end
    validate_in(context, "rows_dendogram_size") do
        return validate_is_above(context, configuration.rows_dendogram_size, 0)
    end
    validate_in(context, "columns_dendogram_size") do
        return validate_is_above(context, configuration.columns_dendogram_size, 0)
    end

    if configuration.dendogram_line.is_filled
        throw(ArgumentError("can't specify heatmap $(location(context)).dendogram_line.is_filled"))
    end

    if (
        configuration.entries_dendogram_size === nothing &&
        configuration.rows_dendogram_size === nothing &&
        configuration.columns_dendogram_size === nothing
    ) && (
        configuration.dendogram_line.width !== nothing ||
        configuration.dendogram_line.style != SolidLine ||
        configuration.dendogram_line.color !== nothing
    )
        throw(ArgumentError(chomp("""
                                  can't specify heatmap $(location(context)).dendogram_line.*
                                  without one of $(location(context)).*_dendogram_size
                                  """)))
    end

    if configuration.entries_reorder == SameOrder
        throw(ArgumentError("can't specify heatmap $(location(context)).entries_reorder: SameOrder"))
    end

    if configuration.rows_reorder == SameOrder && configuration.columns_reorder == SameOrder
        throw(ArgumentError(chomp("""
                                  can't specify both heatmap $(location(context)).rows_reorder: SameOrder
                                  and heatmap $(location(context)).columns_reorder: SameOrder
                                  """)))
    end

    return nothing
end

function validate_exclusive_entries(
    context::ValidationContext,
    field::AbstractString,
    entries_value::Any,
    rows_value::Any,
    columns_value::Any,
)::Nothing
    if entries_value !== nothing
        for (name, value) in (("rows", rows_value), ("columns", columns_value))
            if value !== nothing
                throw(ArgumentError(chomp("""
                                          can't specify both heatmap $(location(context)).entries_$(field)
                                          and $(location(context)).$(name)_$(field)
                                          """)))
            end
        end
    end
end

"""
    @kwdef mutable struct HeatmapGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        x_axis_title::Maybe{AbstractString} = nothing
        y_axis_title::Maybe{AbstractString} = nothing
        entries_colors_title::Maybe{AbstractString} = nothing
        rows_names::Maybe{AbstractVector{<:AbstractString}} = nothing
        columns_names::Maybe{AbstractVector{<:AbstractString}} = nothing
        entries_values::Maybe{AbstractMatrix{<:Real}} = Float32[;;]
        entries_hovers::Maybe{AbstractMatrix{<:AbstractString}} = nothing
        rows_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
        columns_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
        rows_annotations::AbstractVector{AnnotationData} = AnnotationData[]
        columns_annotations::AbstractVector{AnnotationData} = AnnotationData[]
        entries_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
        rows_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
        columns_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
        entries_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
        rows_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
        columns_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
    end

The data for a graph showing a heatmap (matrix) of entries.

This is shown as a 2D image where each matrix entry is a small rectangle with some color. Due to Plotly limitation,
colors must be continuous. The hover for each rectangle is a combination of the `entries_hovers`, `rows_hovers`
and `columns_hovers` for the entry.

By default, if reordering the data, this is based on the `entries_values`. You can override this by specifying an
`entries_arrange_by` matrix of the same size, or even separate `rows_arrange_by` and `columns_arrange_by` matrices. For
efficiency the `rows_arrange_by` matrix should be in row-major layout.

Alternatively you can force the order of the data by specifying the `entries_order` permutation (for a square matrix) or
separate `rows_order` and `columns_order` permutations. You can also specify an `Hclust` object as the order, which
would enable showing a dendogram tree (TODO: implement).

Valid combinations of the fields controlling order and clustering are:

| data `order`                | data `arrange_by`                   | config `reorder`                           | config `dendogram_size` | config `linkage`                                  | config `metric`        | result tree                                                                         | result order                         | notes                                                 |
|:--------------------------- |:----------------------------------- |:------------------------------------------ |:----------------------- |:------------------------------------------------- |:---------------------- |:----------------------------------------------------------------------------------- |:------------------------------------ |:----------------------------------------------------- |
| `nothing`                   | `nothing`                           | `nothing`                                  | `nothing`               | `nothing`                                         | `nothing`              | Not computed                                                                        | Original data order                  | Do not cluster, use the original data order (default) |
| `nothing`                   | `nothing`/ `AbstractMatrix{<:Real}` | `nothing`                                  | Any                     | `nothing`/ `WardLinkage`/ `WardPreSeuaredLinkage` | `nothing`/ `PreMetric` | `oclust` of original order with `linkage` or `WardLinkage`                          | Original data order                  | Cluster, preserving the original order                |
| `nothing`                   | `nothing`                           | `SameOrder`                                | `nothing`/ Any          | `nothing`                                         | `nothing`              | Same as other axis                                                                  | Same as other axis                   | Square matrices only                                  |
| `nothing`                   | `nothing`/ `AbstractMatrix{<:Real}` | `OptimalHclust`/ `RCompatibleHclust`       | `nothing`/ Any          | `nothing`/ Any                                    | `nothing`/ `PreMetric` | `hclust` with `linkage` or `WardLinkage`                                            | `hclust` with `reorder`              | Cluster using `linkage` and branch `reorder`          |
| `nothing`                   | `nothing`/ `AbstractMatrix{<:Real}` | `SlantedHclust`/ `SlantedPreSquaredHclust` | `nothing`/ Any          | `nothing`/ Any                                    | `nothing`/ `PreMetric` | `hclust` with `linkage` or `WardLinkage`, then `reorder_hclust` by `slanted_orders` | `reorder_hclust` by `slanted_orders` | Cluster, then slant preserving the tree               |
| `nothing`                   | `nothing`/ `AbstractMatrix{<:Real}` | `SlantedOrder`/ `SlantedPreSquaredOrder`   | `nothing`/ Any          | `nothing`/ `WardLinkage`/ `WardPreSeuaredLinkage` | `nothing`/ `PreMetric` | `oclust` of `slanted_orders` with `linkage` or `WardLinkage`                        | `slanted_orders`                     | Slant, then cluster preserving the slanted order      |
| `Hclust`                    | `nothing`                           | `nothing`                                  | `nothing`/ Any          | `nothing`                                         | `nothing`              | `Hclust` tree                                                                       | `Hclust` order                       | Force a specific tree and order on the data           |
| `Hclust`                    | `nothing`/ `AbstractMatrix{<:Real}` | `SlantedHclust`/ `SlantedPreSquaredHclust` | `nothing`/ Any          | `nothing`                                         | `nothing`              | `reorder_hclust` by `slanted_orders`                                                | `reorder_hclust` by `slanted_orders` | Slant, preserving a given tree                        |
| `AbstractVector{<:Integer}` | `nothing`                           | `nothing`                                  | `nothing`               | `nothing`                                         | `nothing`              | Not computed                                                                        | `order` permutation                  | Do not cluster, use the specified order               |
| `AbstractVector{<:Integer}` | `nothing`                           | `nothing`                                  | Any                     | `nothing`/ `WardLinkage`/ `WardPreSeuaredLinkage` | `nothing`/ `PreMetric` | `oclust` of `order` with `linkage` or `WardLinkage`                                 | `order` permutation                  | Cluster, preserving the specified order               |

All other combinations are invalid. Note:

  - Only `WardLinkage` and `WardPreSeuaredLinkage` are supported by `oclust`.

  - When calling `hclust` and/or `oclust` and/or `slanted_orders`, then specifying `arrange_by` will use it instead of
    the displayed data matrix.
  - When calling `hclust` and/or `oclust`, then specifying a `metric` will be used instead of `Euclidean` to compute
    the distances matrix.
"""
@kwdef mutable struct HeatmapGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    x_axis_title::Maybe{AbstractString} = nothing
    y_axis_title::Maybe{AbstractString} = nothing
    entries_colors_title::Maybe{AbstractString} = nothing
    rows_names::Maybe{AbstractVector{<:AbstractString}} = nothing
    columns_names::Maybe{AbstractVector{<:AbstractString}} = nothing
    entries_values::Maybe{AbstractMatrix{<:Real}} = Float32[;;]
    entries_hovers::Maybe{AbstractMatrix{<:AbstractString}} = nothing
    rows_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
    columns_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
    rows_annotations::AbstractVector{AnnotationData} = AnnotationData[]
    columns_annotations::AbstractVector{AnnotationData} = AnnotationData[]
    entries_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
    rows_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
    columns_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing
    entries_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
    rows_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
    columns_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing
end

function Validations.validate(context::ValidationContext, data::HeatmapGraphData)::Nothing
    n_rows, n_columns = size(data.entries_values)
    validate_matrix_size(context, "entries_hovers", data.entries_hovers, "entries_values", size(data.entries_values))
    validate_matrix_size(
        context,
        "entries_arrange_by",
        data.entries_arrange_by,
        "entries_values",
        size(data.entries_values),
    )
    validate_matrix_size(context, "rows_arrange_by", data.rows_arrange_by, "entries_values", size(data.entries_values))
    validate_matrix_size(
        context,
        "columns_arrange_by",
        data.columns_arrange_by,
        "entries_values",
        size(data.entries_values),
    )

    validate_vector_length(context, "columns_names", data.columns_names, "entries_values.columns", n_columns)
    validate_vector_length(context, "rows_names", data.rows_names, "entries_values.rows", n_rows)

    validate_vector_length(context, "columns_hovers", data.columns_hovers, "entries_values.columns", n_columns)
    validate_vector_length(context, "rows_hovers", data.rows_hovers, "entries_values.rows", n_rows)

    for (field_name, field_value, base_name, base_value) in (
        ("columns_order", data.columns_order, "entries_values.columns", n_columns),
        ("rows_order", data.rows_order, "entries_values.rows", n_rows),
        ("entries_order", data.entries_order, "entries_values.columns", n_columns),
        ("entries_order", data.entries_order, "entries_values.rows", n_rows),
    )
        if field_value isa Hclust
            field_value = field_value.order
        end
        validate_vector_length(context, field_name, field_value, base_name, base_value)
    end

    validate_vector_entries(context, "columns_annotations", data.columns_annotations) do _, columns_annotation
        validate(context, columns_annotation, "entries_values.columns", n_columns)
        return nothing
    end

    validate_vector_entries(context, "rows_annotations", data.rows_annotations) do _, rows_annotation
        validate(context, rows_annotation, "entries_values.rows", n_rows)
        return nothing
    end

    validate_exclusive_entries(
        context,
        "arrange_by",
        data.entries_arrange_by,
        data.rows_arrange_by,
        data.columns_arrange_by,
    )

    validate_exclusive_entries(context, "order", data.entries_order, data.rows_order, data.columns_order)

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
        rows_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
        columns_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
        entries_values::Maybe{AbstractMatrix{<:Real}} = Float32[;;],
        entries_hovers::Maybe{AbstractMatrix{<:AbstractString}} = nothing,
        rows_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
        columns_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
        rows_annotations::AbstractVector{AnnotationData} = AnnotationData[],
        columns_annotations::AbstractVector{AnnotationData} = AnnotationData[],
        entries_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing,
        rows_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing,
        columns_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing,
        entries_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing,
        rows_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing,
        columns_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing,
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
    rows_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    columns_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    entries_values::Maybe{AbstractMatrix{<:Real}} = Float32[;;],
    entries_hovers::Maybe{AbstractMatrix{<:AbstractString}} = nothing,
    rows_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    columns_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    rows_annotations::AbstractVector{AnnotationData} = AnnotationData[],
    columns_annotations::AbstractVector{AnnotationData} = AnnotationData[],
    entries_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing,
    rows_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing,
    columns_arrange_by::Maybe{AbstractMatrix{<:Real}} = nothing,
    entries_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing,
    rows_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing,
    columns_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}} = nothing,
    configuration::HeatmapGraphConfiguration = HeatmapGraphConfiguration(),
)::HeatmapGraph
    return HeatmapGraph(
        HeatmapGraphData(;
            figure_title,
            x_axis_title,
            y_axis_title,
            entries_colors_title,
            rows_names,
            columns_names,
            entries_values,
            entries_hovers,
            rows_hovers,
            columns_hovers,
            rows_annotations,
            columns_annotations,
            entries_arrange_by,
            rows_arrange_by,
            columns_arrange_by,
            entries_order,
            rows_order,
            columns_order,
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
    )

    validate_axis_sizes(;
        axis_name = "rows",
        annotation_size = graph.configuration.rows_annotations,
        n_annotations = length(graph.data.rows_annotations),
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

    data_rows_order, data_columns_order =
        effective_field_values(graph.data.entries_order, graph.data.rows_order, graph.data.columns_order)

    data_rows_arrange_by, data_columns_arrange_by =
        effective_field_values(graph.data.entries_arrange_by, graph.data.rows_arrange_by, graph.data.columns_arrange_by)

    configuration_rows_reorder, configuration_columns_reorder = effective_field_values(
        graph.configuration.entries_reorder,
        graph.configuration.rows_reorder,
        graph.configuration.columns_reorder,
    )

    configuration_rows_dendogram_size, configuration_columns_dendogram_size = effective_field_values(
        graph.configuration.entries_dendogram_size,
        graph.configuration.rows_dendogram_size,
        graph.configuration.columns_dendogram_size,
    )

    configuration_rows_linkage, configuration_columns_linkage = effective_field_values(
        graph.configuration.entries_linkage,
        graph.configuration.rows_linkage,
        graph.configuration.columns_linkage,
    )

    configuration_rows_metric, configuration_columns_metric = effective_field_values(
        graph.configuration.entries_metric,
        graph.configuration.rows_metric,
        graph.configuration.columns_metric,
    )

    for (
        name,
        data_order,
        data_arrange_by,
        configuration_reorder,
        configuration_dendogram_size,
        configuration_linkage,
        configuration_metric,
        other_name,
        other_data_order,
        other_configuration_reorder,
        other_configuration_dendogram_size,
    ) in (
        (
            "columns",
            data_columns_order,
            data_columns_arrange_by,
            configuration_columns_reorder,
            configuration_columns_dendogram_size,
            configuration_columns_linkage,
            configuration_columns_metric,
            "rows",
            data_rows_order,
            configuration_rows_reorder,
            configuration_rows_dendogram_size,
        ),
        (
            "rows",
            data_rows_order,
            data_rows_arrange_by,
            configuration_rows_reorder,
            configuration_rows_dendogram_size,
            configuration_rows_linkage,
            configuration_rows_metric,
            "columns",
            data_columns_order,
            configuration_columns_reorder,
            configuration_columns_dendogram_size,
        ),
    )
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

                else
                    if !(configuration_linkage in (nothing, WardLinkage, WardPreSeuaredLinkage))
                        throw(
                            ArgumentError(
                                "order preserving clustering does not support graph.configuration.$(name)_linkage: $(configuration_linkage)",
                            ),
                        )
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

            elseif configuration_reorder in (SlantedOrder, SlantedPreSquaredOrder)
                if !(configuration_linkage in (nothing, WardLinkage, WardPreSeuaredLinkage))
                    throw(
                        ArgumentError(
                            "slanted order preserving clustering does not support graph.configuration.$(name)_linkage: $(configuration_linkage)",
                        ),
                    )
                end
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

            if configuration_reorder !== nothing
                throw(ArgumentError(chomp("""
                                          can't specify both heatmap vector graph.data.$(name)_order
                                          and graph.configuration.$(name)_reorder
                                          """)))
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

            else
                if !(configuration_linkage in (nothing, WardLinkage, WardPreSeuaredLinkage))
                    throw(
                        ArgumentError(
                            "order preserving clustering does not support graph.configuration.$(name)_linkage: $(configuration_linkage)",
                        ),
                    )
                end
            end

        else
            @assert false
        end
    end

    if configuration_rows_dendogram_size !== nothing || configuration_columns_dendogram_size !== nothing
        @assert false
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

    rows_order, columns_order, z = reorder_data(graph, colors)

    n_rows, n_columns = size(graph.data.entries_values)

    n_rows_annotations = length(graph.data.rows_annotations)
    n_columns_annotations = length(graph.data.columns_annotations)

    rows_axis_index = 1 + (n_rows_annotations > 0)
    columns_axis_index = 1 + (n_columns_annotations > 0)

    push!(
        traces,
        heatmap(;
            name = "",
            x = collect(1:n_columns),
            y = collect(1:n_rows),
            z,
            xaxis = plotly_axis("x", columns_axis_index; short = true),
            yaxis = plotly_axis("y", columns_axis_index; short = true),
            coloraxis = plotly_axis("color", 1),
        ),
    )

    has_legend_only_traces = [false]

    columns_annotations_colors = push_annotations_traces(;
        traces,
        names = nothing,
        value_axis = graph.configuration.entries_colors.axis,
        base_axis_index = 1 + Bool(n_rows_annotations > 0),
        values_orientation = VerticalValues,
        next_colors_scale_index,
        has_legend_only_traces,
        annotations_data = graph.data.columns_annotations,
        annotation_size = graph.configuration.columns_annotations,
        order = columns_order,
    )

    rows_annotations_colors = push_annotations_traces(;
        traces,
        names = nothing,
        value_axis = graph.configuration.entries_colors.axis,
        base_axis_index = 1 + Bool(n_columns_annotations > 0),
        values_orientation = HorizontalValues,
        next_colors_scale_index,
        has_legend_only_traces,
        annotations_data = graph.data.rows_annotations,
        annotation_size = graph.configuration.rows_annotations,
        order = rows_order,
    )

    has_legend =
        (
            n_rows_annotations > 0 &&
            any([annotation_colors.show_in_legend for annotation_colors in rows_annotations_colors])
        ) || (
            n_columns_annotations > 0 &&
            any([annotation_colors.show_in_legend for annotation_colors in columns_annotations_colors])
        )

    layout = plotly_layout(graph.configuration.figure; title = graph.data.figure_title, has_legend)

    set_layout_axis!(
        layout,
        plotly_axis("y", rows_axis_index),
        AxisConfiguration(; show_ticks = graph.data.rows_names !== nothing);
        title = prefer_data(graph.data.y_axis_title, graph.configuration.y_axis_title),
        ticks_values = graph.data.rows_names === nothing ? nothing : collect(1:n_rows),
        ticks_labels = graph.data.rows_names,
        domain = plotly_sub_graph_domain(
            SubGraph(;
                index = 1,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_rows_annotations,
                annotation_size = graph.configuration.rows_annotations,
            ),
        ),
        is_zeroable = false,
    )

    set_layout_axis!(
        layout,
        plotly_axis("x", columns_axis_index),
        AxisConfiguration(; show_ticks = graph.data.columns_names !== nothing);
        title = prefer_data(graph.data.x_axis_title, graph.configuration.x_axis_title),
        ticks_values = graph.data.columns_names === nothing ? nothing : collect(1:n_columns),
        ticks_labels = graph.data.columns_names,
        domain = plotly_sub_graph_domain(
            SubGraph(;
                index = 1,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_columns_annotations,
                annotation_size = graph.configuration.columns_annotations,
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

    for (axis_letter, annotations_data, annotations_colors, annotation_size) in (
        ("y", graph.data.rows_annotations, rows_annotations_colors, graph.configuration.rows_annotations),
        ("x", graph.data.columns_annotations, columns_annotations_colors, graph.configuration.columns_annotations),
    )
        if annotations_colors !== nothing
            n_annotations = length(annotations_colors)
            for (annotation_index, annotation_colors) in enumerate(annotations_colors)
                annotation_data = annotations_data[annotation_index]
                axis_index = (1 + n_annotations + 1 - annotation_index) % (1 + n_annotations) + 1
                set_layout_axis!(  # NOJET
                    layout,
                    plotly_axis(axis_letter, axis_index),
                    AxisConfiguration();
                    title = annotation_data.title,
                    range = Range(; minimum = 0, maximum = 1),
                    domain = plotly_sub_graph_domain(
                        SubGraph(;
                            index = -annotation_index,
                            n_graphs = 1,
                            graphs_gap = nothing,
                            n_annotations,
                            annotation_size,
                        ),
                    ),
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
)::Tuple{Maybe{AbstractVector{<:Integer}}, Maybe{AbstractVector{<:Integer}}, AbstractMatrix{<:Real}}
    data_rows_order, data_columns_order =
        effective_field_values(graph.data.entries_order, graph.data.rows_order, graph.data.columns_order)

    data_rows_arrange_by, data_columns_arrange_by = effective_field_values(
        graph.data.entries_arrange_by,
        graph.data.rows_arrange_by,
        graph.data.columns_arrange_by,
        graph.data.entries_values,
    )

    configuration_rows_reorder, configuration_columns_reorder = effective_field_values(
        graph.configuration.entries_reorder,
        graph.configuration.rows_reorder,
        graph.configuration.columns_reorder,
    )

    configuration_rows_dendogram_size, configuration_columns_dendogram_size = effective_field_values(
        graph.configuration.entries_dendogram_size,
        graph.configuration.rows_dendogram_size,
        graph.configuration.columns_dendogram_size,
    )

    configuration_rows_linkage, configuration_columns_linkage = effective_field_values(
        graph.configuration.entries_linkage,
        graph.configuration.rows_linkage,
        graph.configuration.columns_linkage,
        WardLinkage,
    )

    configuration_rows_metric, configuration_columns_metric = effective_field_values(
        graph.configuration.entries_metric,
        graph.configuration.rows_metric,
        graph.configuration.columns_metric,
        Euclidean(),
    )

    data_rows_arrange_by = prefer_data(data_rows_arrange_by, graph.data.entries_values)
    data_columns_arrange_by = prefer_data(data_columns_arrange_by, graph.data.entries_values)
    @assert data_rows_arrange_by !== nothing
    @assert data_columns_arrange_by !== nothing

    slant_rows =
        configuration_rows_reorder in (SlantedHclust, SlantedPreSquaredHclust, SlantedOrder, SlantedPreSquaredOrder)
    slant_columns =
        configuration_columns_reorder in (SlantedHclust, SlantedPreSquaredHclust, SlantedOrder, SlantedPreSquaredOrder)

    slant_rows_is_pre_squared = configuration_rows_reorder in (SlantedPreSquaredHclust, SlantedPreSquaredOrder)
    slant_columns_is_pre_squared = configuration_columns_reorder in (SlantedPreSquaredHclust, SlantedPreSquaredOrder)

    if slant_rows &&
       slant_columns &&
       slant_rows_is_pre_squared == slant_columns_is_pre_squared &&
       data_rows_arrange_by === data_columns_arrange_by
        slant_rows_order, slant_columns_order =
            slanted_orders(data_rows_arrange_by; squared_order = slant_rows_is_pre_squared)
    else
        slant_rows_order = nothing
        slant_columns_order = nothing

        if slant_rows
            if configuration_columns_reorder == SameOrder
                slant_rows_order, slant_columns_order =
                    slanted_orders(data_rows_arrange_by; same_order = true, squared_order = slant_rows_is_pre_squared)
            else
                slant_rows_order, _ =
                    slanted_orders(data_rows_arrange_by; order_cols = false, squared_order = slant_rows_is_pre_squared)
            end
        end

        if slant_columns
            if configuration_rows_reorder == SameOrder
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

    data_columns_order, data_columns_tree = finalize_order(;
        data_order = data_columns_order,
        data_arrange_by = data_columns_arrange_by,
        slant_order = slant_columns_order,
        configuration_reorder = configuration_columns_reorder,
        configuration_dendogram_size = configuration_columns_dendogram_size,
        configuration_linkage = configuration_columns_linkage,
        configuration_metric = configuration_columns_metric,
    )

    data_rows_order, data_rows_tree = finalize_order(;
        data_order = data_rows_order,
        data_arrange_by = transpose(data_rows_arrange_by),
        slant_order = slant_rows_order,
        configuration_reorder = configuration_rows_reorder,
        configuration_dendogram_size = configuration_rows_dendogram_size,
        configuration_linkage = configuration_rows_linkage,
        configuration_metric = configuration_rows_metric,
    )

    if configuration_rows_reorder == SameOrder
        @assert data_rows_order === nothing
        @assert data_rows_tree === nothing
        data_rows_order = data_columns_order
        data_rows_tree = data_columns_tree
    end

    if configuration_columns_reorder == SameOrder
        @assert data_columns_order === nothing
        @assert data_columns_tree === nothing
        data_columns_order = data_rows_order
        data_columns_tree = data_rows_tree
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

    return (data_rows_order, data_columns_order, z)
end

function effective_field_values(
    entries_value::Any,
    rows_value::Any,
    columns_value::Any,
    default_value::Any = nothing,
)::Any
    rows_value = prefer_data(rows_value, entries_value)
    columns_value = prefer_data(columns_value, entries_value)
    rows_value = prefer_data(rows_value, default_value)
    columns_value = prefer_data(columns_value, default_value)
    return (rows_value, columns_value)
end

function finalize_order(;
    data_order::Maybe{Union{Hclust, AbstractVector{<:Integer}}},
    data_arrange_by::AbstractMatrix{<:Real},
    slant_order::Maybe{AbstractVector{<:Integer}},
    configuration_reorder::Maybe{HeatmapReorder},
    configuration_dendogram_size::Maybe{Real},
    configuration_linkage::HeatmapLinkage,
    configuration_metric::PreMetric,
)::Tuple{Maybe{AbstractVector{<:Integer}}, Maybe{Hclust}}
    if data_order === nothing
        if configuration_reorder === nothing
            if configuration_dendogram_size === nothing
                return (nothing, nothing)
            else
                distances = pairwise(configuration_metric, data_arrange_by; dims = 2)  # UNTESTED dendogram
                clusters = oclust(distances; method = oclust_method(configuration_linkage))  # UNTESTED dendogram
                return (clusters.order, clusters)  # UNTESTED dendogram
            end

        elseif configuration_reorder === SameOrder
            return (nothing, nothing)

        elseif configuration_reorder in (OptimalHclust, RCompatibleHclust)
            distances = pairwise(configuration_metric, data_arrange_by; dims = 2)
            clusters = hclust(  # NOJET
                distances;
                linkage = hclust_linkage(configuration_linkage),
                branchorder = hclust_branchorder(configuration_reorder),
            )
            return (clusters.order, clusters)

        elseif configuration_reorder in (SlantedHclust, SlantedPreSquaredHclust)
            @assert slant_order !== nothing
            distances = pairwise(configuration_metric, data_arrange_by; dims = 2)
            clusters = hclust(distances; linkage = hclust_linkage(configuration_linkage))
            clusters = reorder_hclust(clusters, slant_order)
            return (clusters.order, clusters)

        elseif configuration_reorder in (SlantedOrder, SlantedPreSquaredOrder)
            distances = pairwise(configuration_metric, data_arrange_by; dims = 2)
            clusters = oclust(distances; order = slant_order, method = oclust_method(configuration_linkage))
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
        if configuration_dendogram_size === nothing
            return (data_order, nothing)
        else
            distances = pairwise(configuration_metric, data_arrange_by; dims = 2)  # UNTESTED
            clusters = oclust(distances; order = data_order, method = oclust_method(configuration_linkage))  # UNTESTED
            return (clusters.order, clusters)  # UNTESTED
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
    elseif linkage == WardPreSeuaredLinkage  # UNTESTED
        return :ward_presquared  # UNTESTED
    else
        @assert false
    end
end

function oclust_method(linkage::HeatmapLinkage)::Symbol
    if linkage == WardLinkage
        return :ward
    elseif linkage == WardPreSeuaredLinkage  # UNTESTED
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

end
