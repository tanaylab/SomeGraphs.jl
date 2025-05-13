"""
Graphs for showing a 2D matrix.
"""
module Heatmaps

export heatmap_graph
export HeatmapBottomLeft
export HeatmapBottomRight
export HeatmapGraph
export HeatmapGraphConfiguration
export HeatmapGraphData
export HeatmapOrigin
export HeatmapReorder
export HeatmapTopLeft
export HeatmapTopRight

using ..Common
using ..Utilities
using ..Validations

using Clustering
using Distances
using PlotlyJS

import ..Bars.push_annotations_traces
import ..Validations.Maybe

"""
Specify how to re-order the rows and/or columns of a matrix for display. Options are:

  - The symbol `:same`. This can only be used for square matrices, and indicates the order of this axis should be copied
    from the computed order of the other axis. It is obviously invalid to specify this for both axes, or when the other
    axis has no order specified.

  - A symbol which is a valid value for the `linkage` parameter for `hclust` (that is, one of `:single`, `:average`,
    `:complete`, `:ward`, or `:ward_presquared`). TODO: Also allow `:slanter`.
  - A tuple with a symbol as above and another symbol which is a valid value for the `branchorder` parameter for
    `hclust` (that is, one of `:r`, `:barjoseph`/`:optimal`). TODO: Also allow `:slanter`.
"""
HeatmapReorder = Union{Symbol, Tuple{Symbol, Symbol}}

"""
<<<<<<< HEAD
Specify where the origin (row 1 column 1) should be displayed. The Plotly default is `BottomLeft`.
=======
Specify where the origin (row 1 column 1) should be displayed. The Plotly default is `HeatmapBottomLeft`.
>>>>>>> 0e6a412 (Control heatmap origin point.)
"""
@enum HeatmapOrigin HeatmapTopLeft HeatmapTopRight HeatmapBottomLeft HeatmapBottomRight

"""
    @kwdef mutable struct HeatmapGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration()
        entries_colors::ColorsConfiguration = ColorsConfiguration()
        rows_annotations::AnnotationSize = AnnotationSize()
        columns_annotations::AnnotationSize = AnnotationSize()
        entries_order::Maybe{HeatmapReorder} = nothing
        rows_order::Maybe{HeatmapReorder} = nothing
        columns_order::Maybe{HeatmapReorder} = nothing
        entries_metric::Maybe{PreMetric} = nothing
        rows_metric::Maybe{PreMetric} = nothing
        columns_metric::Maybe{PreMetric} = nothing
        origin::HeatmapOrigin = HeatmapBottomLeft
    end

Configure a graph showing a heatmap.

This displays a matrix of values using a rectangle at each position. Due to Plotly's limitations, you still to manually
tweak the graph size for best results; there's no way to directly control the width and height of the rectangles. In
addition, the only supported color configurations are using continuous color palettes.

You can use `columns_order` and/or `rows_order` to reorder the data. By default, this uses the `Euclidean` distance
metric. You can override this by specifying the `columns_metric` and/or `rows_metric` distance measure.

Specifying `entries_order` is equivalent to specifying both `rows_order` and `columns_order`, and similarly for
`entries_metric`.

There's no `x_axis` or `y_axis` configuration here, as none of the options apply, except for `show_ticks`, which is
implied by setting the `rows_names` and/or `columns_names` of the data. You can however provide default axis titles here
(which you can override in the data as usual).
"""
@kwdef mutable struct HeatmapGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    x_axis_title::Maybe{AbstractString} = nothing
    y_axis_title::Maybe{AbstractString} = nothing
    entries_colors::ColorsConfiguration = ColorsConfiguration()
    rows_annotations::AnnotationSize = AnnotationSize()
    columns_annotations::AnnotationSize = AnnotationSize()
    entries_order::Maybe{HeatmapReorder} = nothing
    rows_order::Maybe{HeatmapReorder} = nothing
    columns_order::Maybe{HeatmapReorder} = nothing
    entries_metric::Maybe{PreMetric} = nothing
    rows_metric::Maybe{PreMetric} = nothing
    columns_metric::Maybe{PreMetric} = nothing
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
        "order",
        configuration.entries_order,
        configuration.rows_order,
        configuration.columns_order,
    )
    validate_exclusive_entries(
        context,
        "metric",
        configuration.entries_metric,
        configuration.rows_metric,
        configuration.columns_metric,
    )

    if configuration.entries_order == :same
        throw(ArgumentError("can't specify heatmap $(location(context)).entries_order: :same"))
    end

    if configuration.rows_order == :same && configuration.columns_order == :same
        throw(ArgumentError(chomp("""
                                  can't specify both heatmap $(location(context)).rows_order: :same
                                  and heatmap $(location(context)).columns_order: :same
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
                                          can't specify both $(location(context)).entries_$(field)
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
        entries_order_by::Maybe{AbstractMatrix{<:Real}} = nothing
        rows_order_by::Maybe{AbstractMatrix{<:Real}} = nothing
        columns_order_by::Maybe{AbstractMatrix{<:Real}} = nothing
        entries_order::Maybe{AbstractVector{<:Integer}} = nothing
        rows_order::Maybe{AbstractVector{<:Integer}} = nothing
        columns_order::Maybe{AbstractVector{<:Integer}} = nothing
    end

The data for a graph showing a heatmap (matrix) of entries.

This is shown as a 2D image where each matrix entry is a small rectangle with some color. Due to Plotly limitation,
colors must be continuous. The hover for each rectangle is a combination of the `entries_hovers`, `rows_hovers`
and `columns_hovers` for the entry.

By default, if reordering the data, this is based on the `entries_values`. You can override this by specifying an
`entries_order_by` matrix of the same size, or even separate `rows_order_by` and `columns_order_by` matrices. For
efficiency the `rows_order_by` matrix should be in row-major layout.

Alternatively you can force the order of the data by specifying the `entries_order` permutation (for a square matrix) or
separate `rows_order` and `columns_order` permutations.
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
    entries_order_by::Maybe{AbstractMatrix{<:Real}} = nothing
    rows_order_by::Maybe{AbstractMatrix{<:Real}} = nothing
    columns_order_by::Maybe{AbstractMatrix{<:Real}} = nothing
    entries_order::Maybe{AbstractVector{<:Integer}} = nothing
    rows_order::Maybe{AbstractVector{<:Integer}} = nothing
    columns_order::Maybe{AbstractVector{<:Integer}} = nothing
end

function Validations.validate(context::ValidationContext, data::HeatmapGraphData)::Nothing
    n_rows, n_columns = size(data.entries_values)
    validate_matrix_size(context, "entries_hovers", data.entries_hovers, "entries_values", size(data.entries_values))
    validate_matrix_size(
        context,
        "entries_order_by",
        data.entries_order_by,
        "entries_values",
        size(data.entries_values),
    )
    validate_matrix_size(context, "rows_order_by", data.rows_order_by, "entries_values", size(data.entries_values))
    validate_matrix_size(
        context,
        "columns_order_by",
        data.columns_order_by,
        "entries_values",
        size(data.entries_values),
    )

    validate_vector_length(context, "columns_names", data.columns_names, "entries_values.columns", n_columns)
    validate_vector_length(context, "rows_names", data.rows_names, "entries_values.rows", n_rows)

    validate_vector_length(context, "columns_hovers", data.columns_hovers, "entries_values.columns", n_columns)
    validate_vector_length(context, "rows_hovers", data.rows_hovers, "entries_values.rows", n_rows)

    validate_vector_length(context, "columns_order", data.columns_order, "entries_values.columns", n_columns)
    validate_vector_length(context, "rows_order", data.rows_order, "entries_values.rows", n_rows)

    validate_vector_entries(context, "columns_annotations", data.columns_annotations) do _, columns_annotation
        validate(context, columns_annotation)
        return nothing
    end

    validate_vector_entries(context, "rows_annotations", data.rows_annotations) do _, rows_annotation
        validate(context, rows_annotation)
        return nothing
    end

    validate_exclusive_entries(context, "order_by", data.entries_order_by, data.rows_order_by, data.columns_order_by)
    return validate_exclusive_entries(context, "order", data.entries_order, data.rows_order, data.columns_order)
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
        entries_order_by::Maybe{AbstractMatrix{<:Real}} = nothing,
        rows_order_by::Maybe{AbstractMatrix{<:Real}} = nothing,
        columns_order_by::Maybe{AbstractMatrix{<:Real}} = nothing,
        entries_order::Maybe{AbstractVector{<:Integer}} = nothing,
        rows_order::Maybe{AbstractVector{<:Integer}} = nothing,
        columns_order::Maybe{AbstractVector{<:Integer}} = nothing,
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
    entries_order_by::Maybe{AbstractMatrix{<:Real}} = nothing,
    rows_order_by::Maybe{AbstractMatrix{<:Real}} = nothing,
    columns_order_by::Maybe{AbstractMatrix{<:Real}} = nothing,
    entries_order::Maybe{AbstractVector{<:Integer}} = nothing,
    rows_order::Maybe{AbstractVector{<:Integer}} = nothing,
    columns_order::Maybe{AbstractVector{<:Integer}} = nothing,
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
            entries_order_by,
            rows_order_by,
            columns_order_by,
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
        for (name, value) in (("rows", graph.configuration.rows_order), ("columns", graph.configuration.columns_order))
            if value == :same
                throw(ArgumentError(chomp("""
                                          can't specify graph.configuration.$(name)_order: :same
                                          for a non-square matrix: $(n_rows) rows x $(n_columns) columns
                                          """)))
            end
        end
    end

    for (name, conf_order, data_order, other_name, other_conf_order, other_data_order) in (
        (
            "rows",
            graph.configuration.rows_order,
            graph.data.rows_order,
            "columns",
            graph.configuration.columns_order,
            graph.data.columns_order,
        ),
        (
            "columns",
            graph.configuration.columns_order,
            graph.data.columns_order,
            "rows",
            graph.configuration.rows_order,
            graph.data.rows_order,
        ),
    )
        if conf_order == :same && data_order === nothing && other_conf_order === nothing && other_data_order === nothing
            throw(ArgumentError(chomp("""
                                      no $(other_name) order to copy into $(name) order
                                      for graph.configuration.$(name)_order: same
                                      """)))
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
    configuration_rows_order, configuration_columns_order = effective_field_values(
        graph.configuration.entries_order,
        graph.configuration.rows_order,
        graph.configuration.columns_order,
    )

    configuration_rows_metric, configuration_columns_metric = effective_field_values(
        graph.configuration.entries_metric,
        graph.configuration.rows_metric,
        graph.configuration.columns_metric,
        Euclidean(),
    )

    data_rows_order_by, data_columns_order_by = effective_field_values(
        graph.data.entries_order_by,
        graph.data.rows_order_by,
        graph.data.columns_order_by,
        graph.data.entries_values,
    )

    data_rows_order, data_columns_order =
        effective_field_values(graph.data.entries_order, graph.data.rows_order, graph.data.columns_order)

    data_columns_order = finalize_order(;
        data_order = data_columns_order,
        metric = configuration_columns_metric,
        configuration_order = configuration_columns_order,
        order_by = data_columns_order_by,
    )

    data_rows_order = finalize_order(;
        data_order = data_rows_order,
        metric = configuration_rows_metric,
        configuration_order = configuration_rows_order,
        order_by = transpose(data_rows_order_by),
    )

    if data_rows_order === nothing && configuration_rows_order == :same
        @assert data_columns_order !== nothing
        data_rows_order = data_columns_order
    end

    if data_columns_order === nothing && configuration_columns_order == :same
        @assert data_rows_order !== nothing
        data_columns_order = data_rows_order
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
    data_order::Maybe{AbstractVector{<:Integer}},
    metric::PreMetric,
    configuration_order::Maybe{HeatmapReorder},
    order_by::Maybe{AbstractMatrix{<:Real}},
)::Maybe{AbstractVector{<:Integer}}
    if data_order !== nothing || configuration_order === nothing || configuration_order == :same
        return data_order
    end

    n_columns = size(order_by, 2)
    distances = pairwise(metric, order_by; dims = 2)
    @assert size(distances) == (n_columns, n_columns)

    if typeof(configuration_order) <: Symbol
        configuration_order = (configuration_order, :barjoseph)
    end

    tree = hclust(distances; linkage = configuration_order[1], branchorder = configuration_order[2])  # NOJET
    @assert length(tree.order) == n_columns

    return tree.order
end

end
