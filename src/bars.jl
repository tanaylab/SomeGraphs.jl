"""
Graphs for showing bars.
"""
module Bars

export BarsGraph
export BarsGraphConfiguration
export BarsGraphData
export SeriesBarsGraph
export SeriesBarsGraphConfiguration
export SeriesBarsGraphData
export bars_graph
export series_bars_graph

using ..Common
using ..Utilities
using ..Validations

using NamedArrays
using PlotlyJS

import ..Validations.Maybe

"""
    @kwdef mutable struct BarsGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration()
        value_bands::BandsConfiguration = BandsConfiguration()
        values_orientation::ValuesOrientation = VerticalValues
        bars_colors::ColorsConfiguration = ColorsConfiguration()
        bars_gap::Real = 0.02,
        bars_annotations::AnnotationSize = AnnotationSize()
    end

Configure a graph for showing a single series of bars.

By default the values are the `y` axis (`VerticalValues`). You can flip the axes using the `values_orientation`. You can
specify bands for this axis using `value_bands`. The `bars_gap` is added between the graps, and is in the usual
inconvenient units of fractions of the total graph size. The `bars_colors` is used to control the color of the bars (if
not specified, chosen automatically by Plotly), in combination with the data bar colors (if any).
"""
@kwdef mutable struct BarsGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    value_axis::AxisConfiguration = AxisConfiguration()
    value_bands::BandsConfiguration = BandsConfiguration()
    values_orientation::ValuesOrientation = VerticalValues
    bars_colors::ColorsConfiguration = ColorsConfiguration()
    bars_gap::Real = 0.02
    bars_annotations::AnnotationSize = AnnotationSize()
end

function Validations.validate(context::ValidationContext, configuration::BarsGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "value_axis", configuration.value_axis)
    validate_field(context, "value_bands", configuration.value_bands)
    validate_field(context, "bars_annotations", configuration.bars_annotations)
    validate_field(context, "bars_colors", configuration.bars_colors)

    validate_in(context, "bars_gap") do
        validate_is_at_least(context, configuration.bars_gap, 0)
        return validate_is_below(context, configuration.bars_gap, 1)
    end

    if configuration.bars_colors.show_legend && configuration.bars_colors.palette isa CategoricalColors
        throw(
            ArgumentError(
                "can't specify $(location(context)).bars_colors.show_legend\n" *
                "for a categorical $(location(context)).bars_colors.palette",
            ),
        )
    end

    return nothing
end

"""
    @kwdef mutable struct BarsGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        bar_axis_title::Maybe{AbstractString} = nothing
        value_axis_title::Maybe{AbstractString} = nothing
        bars_colors_title::Maybe{AbstractString} = nothing
        bars_values::AbstractVector{<:Real} = Float32[]
        bars_names::Maybe{AbstractVector{<:AbstractString}} = nothing
        bars_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}} = nothing
        bars_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
        value_bands::BandsData = BandsData()
        bars_annotations::AbstractVector{AnnotationData} = AnnotationData[]
    end

The data for a graph of a single series of bars.

By default, all the titles are empty. You can specify the overall `figure_title` as well as the `bars_axis_title` and
`value_axis_title` for the axes, , and a name for each bar in `bars_names`. The `bars_colors` are optional; typically
all bars have the same color.

You can even add annotations to the bars.
"""
@kwdef mutable struct BarsGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    bar_axis_title::Maybe{AbstractString} = nothing
    value_axis_title::Maybe{AbstractString} = nothing
    bars_colors_title::Maybe{AbstractString} = nothing
    bars_values::AbstractVector{<:Real} = Float32[]
    bars_names::Maybe{AbstractVector{<:AbstractString}} = nothing
    bars_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing
    bars_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
    value_bands::BandsData = BandsData()
    bars_annotations::AbstractVector{AnnotationData} = AnnotationData[]
end

function Validations.validate(context::ValidationContext, data::BarsGraphData)::Nothing
    validate_vector_is_not_empty(context, "bars_values", data.bars_values)

    n_bars = length(data.bars_values)

    validate_vector_length(context, "bars_names", data.bars_names, "bars_values", n_bars)

    validate_vector_length(context, "bars_colors", data.bars_colors, "bars_values", n_bars)

    validate_vector_length(context, "bars_hovers", data.bars_hovers, "bars_values", n_bars)

    validate_vector_entries(context, "bars_annotations", data.bars_annotations) do _, bars_annotation
        validate(context, bars_annotation, "bars_values", n_bars)
        return nothing
    end

    return nothing
end

"""
A graph showing a single series of bars. See [`BarsGraphData`](@ref) and [`BarsGraphConfiguration`](@ref).
"""
BarsGraph = Graph{BarsGraphData, BarsGraphConfiguration}

"""
    function line_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        bar_axis_title::Maybe{AbstractString} = nothing,
        value_axis_title::Maybe{AbstractString} = nothing,
        bars_values::AbstractVector{<:Real} = Float32[],
        bars_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
        bars_colors::Maybe{AbstractVector{<:AbstractString}} = nothing,
        bars_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
        value_bands::BandsData = BandsData(),
        bars_annotations::AbstractVector{AnnotationData} = AnnotationData[],
        configuration::BarsGraphConfiguration = BarsGraphConfiguration()]
    )::BarsGraph

Create a [`BarsGraph`](@ref) by initializing only the [`BarsGraphData`](@ref) fields (with an optional
[`BarsGraphConfiguration`](@ref)).
"""
function bars_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    bar_axis_title::Maybe{AbstractString} = nothing,
    value_axis_title::Maybe{AbstractString} = nothing,
    bars_values::AbstractVector{<:Real} = Float32[],
    bars_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    bars_colors::Maybe{AbstractVector{<:AbstractString}} = nothing,
    bars_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    value_bands::BandsData = BandsData(),
    bars_annotations::AbstractVector{AnnotationData} = AnnotationData[],
    configuration::BarsGraphConfiguration = BarsGraphConfiguration(),
)::BarsGraph
    return BarsGraph(
        BarsGraphData(;
            figure_title,
            bar_axis_title,
            value_axis_title,
            bars_values,
            bars_names,
            bars_colors,
            bars_hovers,
            value_bands,
            bars_annotations,
        ),
        configuration,
    )
end

function Common.validate_graph(graph::BarsGraph)::Nothing
    validate_values(
        ValidationContext(["graph.data.bars_values"]),
        graph.data.bars_values,
        ValidationContext(["graph.configuration.value_axis"]),
        graph.configuration.value_axis,
    )

    validate_colors(
        ValidationContext(["graph.data.bars_colors"]),
        graph.data.bars_colors,
        ValidationContext(["graph.configuration.bars_colors"]),
        graph.configuration.bars_colors,
    )

    validate_graph_bands(
        "value_bands",
        graph.configuration.value_bands,
        graph.data.value_bands,
        graph.configuration.value_axis,
    )

    validate_axis_sizes(;
        axis_name = "value",
        annotation_size = graph.configuration.bars_annotations,
        n_annotations = length(graph.data.bars_annotations),
    )

    return nothing
end

function Common.graph_to_figure(graph::BarsGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    implicit_values_range = MaybeRange()

    next_colors_scale_index = [1]
    colors = configured_colors(;
        colors_configuration = graph.configuration.bars_colors,
        colors_title = prefer_data(graph.data.bars_colors_title, graph.configuration.bars_colors.title),
        colors_values = graph.data.bars_colors,
        next_colors_scale_index,
    )

    push_bar_trace!(;
        traces,
        sub_graph = SubGraph(;
            index = 1,
            n_graphs = 1,
            graphs_gap = nothing,
            n_annotations = length(graph.data.bars_annotations),
            annotation_size = graph.configuration.bars_annotations,
        ),
        values = graph.data.bars_values,
        value_axis = graph.configuration.value_axis,
        values_orientation = graph.configuration.values_orientation,
        color = prefer_data(colors.final_colors_values, colors.colors_configuration.fixed),
        hovers = graph.data.bars_hovers,
        names = graph.data.bars_names,
        show_in_legend = false,
        implicit_values_range,
        colors_scale_index = colors.colors_scale_index,
    )

    has_legend_only_traces = [false]
    annotations_colors = push_annotations_traces!(;
        traces,
        names = graph.data.bars_names,
        value_axis = graph.configuration.value_axis,
        values_orientation = graph.configuration.values_orientation,
        next_colors_scale_index,
        has_legend_only_traces,
        annotations_data = graph.data.bars_annotations,
        annotation_size = graph.configuration.bars_annotations,
    )

    layout = bars_layout(;
        graph,
        has_tick_names = graph.data.bars_names !== nothing,
        has_legend = false,
        implicit_values_range,
        colors,
        annotations_colors,
        has_legend_only_traces,
    )

    return plotly_figure(traces, layout)
end

"""
    @kwdef mutable struct SeriesBarsGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration()
        values_orientation::ValuesOrientation = VerticalValues
        bars_gap::Maybe{Real} = nothing
        bars_annotations::AnnotationSize = AnnotationSize(),
        series_gap::Maybe{Real} = nothing
        stacking::Maybe{Stacking} = nothing
    end

Configure a graph for showing multiple series of bars.

This expands on [`BarsGraphConfiguration`](@ref) by adding optional `stacking` for stacking the bars of the different
series on top of each other. Alternatively, specifying a `series_gap` will plot each series in its own separate
sub-graph. The `series_gap` is specified as a fraction of the used graph size. If zero the graphs will be adjacent, if 1
then the gaps will be the same size as the graphs. If neither is specified, then the bars will be shown in groups
(adjacent to each other) with the `bars_gap` between the groups.
"""
@kwdef mutable struct SeriesBarsGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    value_axis::AxisConfiguration = AxisConfiguration()
    values_orientation::ValuesOrientation = VerticalValues
    bars_gap::Real = 0.02
    bars_annotations::AnnotationSize = AnnotationSize()
    series_gap::Maybe{Real} = nothing
    stacking::Maybe{Stacking} = nothing
end

function Validations.validate(context::ValidationContext, configuration::SeriesBarsGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "value_axis", configuration.value_axis)
    validate_field(context, "bars_annotations", configuration.bars_annotations)

    validate_in(context, "bars_gap") do
        validate_is_at_least(context, configuration.bars_gap, 0)
        return validate_is_below(context, configuration.bars_gap, 1)
    end

    validate_in(context, "series_gap") do
        validate_is_at_least(context, configuration.bars_gap, 0)
        return validate_is_below(context, configuration.bars_gap, 1)
    end

    if configuration.stacking !== nothing && configuration.series_gap !== nothing
        throw(ArgumentError("""
                            can't specify both $(location(context)).stacking
                            and $(location(context)).series_gap
                            """))
    end

    return nothing
end

"""
    @kwdef mutable struct SeriesBarsGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        bar_axis_title::Maybe{AbstractString} = nothing
        value_axis_title::Maybe{AbstractString} = nothing
        series_bars_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Vector{Float32}}()
        bars_names::Maybe{AbstractVector{<:AbstractString}} = nothing
        bars_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
        bars_annotations::AbstractVector{AnnotationData} = AnnotationData[]
        series_names::Maybe{AbstractVector{<:AbstractString}} = nothing
        series_colors::Maybe{AbstractVector{<:AbstractString}} = nothing
        series_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
    end

The data for a graph of multiple series of bars.

All the vectors in the `series_bars_values` must have the same length (the number of bars). The number of entries in
`bars_names` and/or `bars_hovers` must be the same. The number of entries in `series_names`, `series_colors` and/or
`series_hovers` must be the number of series (the number of vectors in `series_bars_values`).

All the bars of each series have the same color. If the `series_names` are specified, then they are used in a legend
(or, if using `series_gap`, as the separate axes titles).
"""
@kwdef mutable struct SeriesBarsGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    bar_axis_title::Maybe{AbstractString} = nothing
    value_axis_title::Maybe{AbstractString} = nothing
    series_bars_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Vector{Float32}}()
    bars_names::Maybe{AbstractVector{<:AbstractString}} = nothing
    bars_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
    bars_annotations::AbstractVector{AnnotationData} = AnnotationData[]
    series_names::Maybe{AbstractVector{<:AbstractString}} = nothing
    series_colors::Maybe{AbstractVector{<:AbstractString}} = nothing
    series_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
end

function Validations.validate(context::ValidationContext, data::SeriesBarsGraphData)::Nothing
    validate_vector_is_not_empty(context, "series_bars_values", data.series_bars_values)

    n_series = length(data.series_bars_values)
    n_bars = length(data.series_bars_values[1])

    for series_index in 1:n_series
        validate_vector_length(
            context,
            "series_bars_values[$(series_index)]",
            data.series_bars_values[series_index],
            "series_bars_values[1]",
            n_bars,
        )
    end

    validate_vector_length(context, "bars_names", data.bars_names, "series_bars_values[*]", n_bars)
    validate_vector_length(context, "bars_hovers", data.bars_hovers, "series_bars_values[*]", n_bars)

    validate_vector_entries(context, "bars_annotations", data.bars_annotations) do _, bars_annotation
        validate(context, bars_annotation, "series_bars_values[*]", n_bars)
        return nothing
    end

    validate_vector_length(context, "series_names", data.series_names, "series_bars_values", n_series)
    validate_vector_length(context, "series_colors", data.series_colors, "series_bars_values", n_series)
    validate_vector_length(context, "series_hovers", data.series_colors, "series_bars_values", n_series)

    validate_vector_entries(context, "series_colors", data.series_colors) do _, series_color
        validate_is_color(context, series_color)
        return nothing
    end

    return nothing
end

"""
A graph showing multiple series of bars. See [`SeriesBarsGraphData`](@ref) and [`SeriesBarsGraphConfiguration`](@ref).
"""
SeriesBarsGraph = Graph{SeriesBarsGraphData, SeriesBarsGraphConfiguration}

"""
    function series_bars_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        bar_axis_title::Maybe{AbstractString} = nothing,
        value_axis_title::Maybe{AbstractString} = nothing,
        series_bars_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Vector{Float32}}(),
        bars_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
        bars_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
        series_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
        series_colors::Maybe{AbstractVector{<:AbstractString}} = nothing,
        series_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
        configuration::SeriesBarsGraphConfiguration = SeriesBarsGraphConfiguration()]
    )::SeriesBarsGraph

Create a [`SeriesBarsGraph`](@ref) by initializing only the [`SeriesBarsGraphData`](@ref) fields (with an optional
[`SeriesBarsGraphConfiguration`](@ref)).
"""
function series_bars_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    bar_axis_title::Maybe{AbstractString} = nothing,
    value_axis_title::Maybe{AbstractString} = nothing,
    series_bars_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Vector{Float32}}(),
    bars_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    bars_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    series_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    series_colors::Maybe{AbstractVector{<:AbstractString}} = nothing,
    series_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    configuration::SeriesBarsGraphConfiguration = SeriesBarsGraphConfiguration(),
)::SeriesBarsGraph
    return SeriesBarsGraph(
        SeriesBarsGraphData(;
            figure_title,
            bar_axis_title,
            value_axis_title,
            series_bars_values,
            bars_names,
            bars_hovers,
            series_names,
            series_colors,
            series_hovers,
        ),
        configuration,
    )
end

function Common.validate_graph(graph::SeriesBarsGraph)::Nothing
    n_series = length(graph.data.series_bars_values)
    for series_index in 1:n_series
        validate_values(
            ValidationContext(["graph.data.series_bars_values", series_index]),
            graph.data.series_bars_values[series_index],
            ValidationContext(["graph.configuration.value_axis"]),
            graph.configuration.value_axis,
        )

        if graph.configuration.stacking == StackFractions
            for (bar_index, bar_value) in enumerate(graph.data.series_bars_values[series_index])
                scaled_value = scale_axis_value(graph.configuration.value_axis, bar_value)
                if scaled_value === nothing || scaled_value < 0
                    throw(
                        ArgumentError(
                            "too low scaled graph.data.series_bars_values[$(series_index)][$(bar_index)]: $(scaled_value)\n" *
                            "is not at least: 0\n" *
                            "when using graph.configuration.stacking: StackFractions",
                        ),
                    )
                end
            end
        end
    end

    if graph.data.value_axis_title !== nothing &&
       graph.data.series_names !== nothing &&
       graph.configuration.series_gap !== nothing
        throw(
            ArgumentError(
                "can't specify both graph.data.value_axis_title and graph.data.series_names\n" *
                "together with graph.configuration.series_gap",
            ),
        )
    end

    validate_axis_sizes(;
        axis_name = "value",
        graphs_gap = graph.configuration.series_gap,
        n_graphs = n_series,
        annotation_size = graph.configuration.bars_annotations,
        n_annotations = length(graph.data.bars_annotations),
    )

    return nothing
end

function Common.graph_to_figure(graph::SeriesBarsGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    implicit_values_range = MaybeRange()

    n_series = length(graph.data.series_bars_values)
    n_bars = length(graph.data.series_bars_values[1])

    common_implicit_values_range = MaybeRange()
    total_scaled_values = nothing
    specific_scaled_values = Vector{AbstractVector{<:Real}}(undef, n_series)
    if graph.configuration.series_gap === nothing
        specific_scaled_ranges = nothing
    else
        specific_scaled_ranges = Vector{MaybeRange}(undef, n_series)
    end

    show_in_legend = graph.data.series_names !== nothing && graph.configuration.series_gap === nothing

    for series_index in 1:n_series
        if graph.data.series_hovers === nothing
            hovers = graph.data.bars_hovers
        elseif graph.data.bars_hovers === nothing
            hovers = fill(graph.data.series_hovers[series_index], n_bars)
        else
            hovers =
                ["$(graph.data.series_hovers[series_index])<br>$(bar_hover)" for bar_hover in graph.data.bars_hovers]
        end

        if graph.configuration.stacking === nothing
            implicit_values_range = common_implicit_values_range
        else
            implicit_values_range = MaybeRange()
        end

        scaled_values = push_bar_trace!(;
            traces,
            sub_graph = SubGraph(;
                index = series_index,
                n_graphs = n_series,
                graphs_gap = graph.configuration.series_gap,
                n_annotations = length(graph.data.bars_annotations),
                annotation_size = graph.configuration.bars_annotations,
            ),
            name = prefer_data(graph.data.series_names, series_index, nothing),
            values = graph.data.series_bars_values[series_index],
            value_axis = graph.configuration.value_axis,
            values_orientation = graph.configuration.values_orientation,
            color = prefer_data(graph.data.series_colors, series_index, nothing),
            hovers,
            show_in_legend,
            names = graph.data.bars_names,
            implicit_values_range,
        )

        specific_scaled_values[series_index] = scaled_values
        if graph.configuration.series_gap !== nothing
            specific_scaled_ranges[series_index] = MaybeRange()
            collect_range!(specific_scaled_ranges[series_index], scaled_values)
        end

        if graph.configuration.stacking !== nothing
            if total_scaled_values === nothing
                total_scaled_values = copy(scaled_values)
            else
                total_scaled_values .+= scaled_values
            end
        end
    end

    if graph.configuration.stacking == StackValues
        collect_range!(implicit_values_range, total_scaled_values)

    elseif graph.configuration.stacking == StackFractions
        @assert total_scaled_values !== nothing
        total_scaled_values[total_scaled_values .== 0] .= 1

        for series_index in 1:n_series
            specific_scaled_values[series_index] ./= total_scaled_values
            if graph.configuration.value_axis.percent
                specific_scaled_values[series_index] .*= 100
            end
        end
        if graph.configuration.value_axis.percent
            implicit_values_range = MaybeRange(; minimum = 0, maximum = 100)
        else
            implicit_values_range = MaybeRange(; minimum = 0, maximum = 1)
        end

    else
        @assert graph.configuration.stacking === nothing
        implicit_values_range = common_implicit_values_range
    end

    next_colors_scale_index = [1]
    has_legend_only_traces = [false]
    annotations_colors = push_annotations_traces!(;
        traces,
        names = graph.data.bars_names,
        value_axis = graph.configuration.value_axis,
        values_orientation = graph.configuration.values_orientation,
        n_graphs = n_series,
        graphs_gap = graph.configuration.series_gap,
        next_colors_scale_index,
        has_legend_only_traces,
        annotations_data = graph.data.bars_annotations,
        annotation_size = graph.configuration.bars_annotations,
    )

    layout = bars_layout(;
        graph,
        has_tick_names = graph.data.bars_names !== nothing,
        has_legend = show_in_legend,
        implicit_values_range,
        specific_scaled_ranges,
        annotations_colors,
        has_legend_only_traces,
    )

    return plotly_figure(traces, layout)
end

function push_bar_trace!(;
    traces::Vector{GenericTrace},
    values::AbstractVector{<:Real},
    value_axis::AxisConfiguration,
    basis_sub_graph::Maybe{SubGraph} = nothing,
    values_orientation::ValuesOrientation,
    color::Maybe{Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}, AbstractString}} = nothing,
    hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    name::Maybe{AbstractString} = nothing,
    sub_graph::SubGraph,
    show_in_legend::Bool = false,
    implicit_values_range::MaybeRange,
    colors_scale_index::Maybe{Integer} = nothing,
)::AbstractVector{<:AbstractFloat}
    xaxis_index, x0, yaxis_index, y0 =
        plotly_sub_graph_axes(; basis_sub_graph, values_sub_graph = sub_graph, values_orientation)

    scaled_values = scale_axis_values(value_axis, values; clamp = false)
    collect_range!(implicit_values_range, scaled_values)

    if names === nothing
        names = [string(index) for index in 1:length(scaled_values)]
    end

    if values_orientation == VerticalValues
        xs = names
        ys = scaled_values
        orientation = "v"
    elseif values_orientation == HorizontalValues
        xs = scaled_values
        ys = names
        orientation = "h"
    else
        @assert false
    end

    push!(
        traces,
        bar(;
            x = xs,
            y = ys,
            x0,
            y0,
            xaxis = plotly_axis("x", xaxis_index; short = true),
            yaxis = plotly_axis("y", yaxis_index; short = true),
            name,
            orientation = orientation,
            marker_color = color,
            marker_coloraxis = plotly_axis("color", colors_scale_index),
            customdata = hovers,
            hovertemplate = hovers === nothing ? nothing : "%{customdata}<extra></extra>",
            showlegend = show_in_legend,
        ),
    )

    return scaled_values
end

function push_annotations_traces!(;
    traces::Vector{GenericTrace},
    names::Maybe{AbstractVector{<:AbstractString}},
    value_axis::AxisConfiguration,
    basis_sub_graph::Maybe{SubGraph} = nothing,
    values_orientation::ValuesOrientation,
    n_graphs::Integer = 1,
    graphs_gap::Maybe{Real} = nothing,
    next_colors_scale_index::AbstractVector{<:Integer},
    has_legend_only_traces::AbstractVector{Bool},
    annotations_data::AbstractVector{AnnotationData},
    annotation_size::AnnotationSize,
    order::Maybe{AbstractVector{<:Integer}} = nothing,
    expanded_mask::Maybe{Union{BitVector, AbstractVector{Bool}}} = nothing,
)::AbstractVector{ConfiguredColors}
    return ConfiguredColors[
        push_annotation_traces!(;
            traces,
            names,
            value_axis,
            basis_sub_graph,
            values_orientation,
            n_graphs,
            graphs_gap,
            annotation_index,
            n_annotations = length(annotations_data),
            annotation_data = annotation_data,
            annotation_size,
            next_colors_scale_index,
            has_legend_only_traces,
            order,
            expanded_mask,
        ) for (annotation_index, annotation_data) in enumerate(annotations_data)
    ]
end

function push_annotation_traces!(;
    traces::Vector{GenericTrace},
    names::Maybe{AbstractVector{<:AbstractString}},
    value_axis::AxisConfiguration,
    basis_sub_graph::Maybe{SubGraph},
    values_orientation::ValuesOrientation,
    n_graphs::Integer,
    graphs_gap::Maybe{Real},
    annotation_index::Integer,
    n_annotations::Integer,
    annotation_data::AnnotationData,
    annotation_size::AnnotationSize,
    next_colors_scale_index::AbstractVector{<:Integer},
    has_legend_only_traces::AbstractVector{Bool},
    order::Maybe{AbstractVector{<:Integer}},
    expanded_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
)::ConfiguredColors
    colors = configured_colors(;
        colors_configuration = annotation_data.colors,
        colors_title = annotation_data.title,
        colors_values = annotation_data.values,
        next_colors_scale_index,
    )

    sub_graph = SubGraph(; index = -annotation_index, n_graphs, graphs_gap, n_annotations, annotation_size)

    if colors.show_in_legend && colors.colors_configuration.palette isa CategoricalColors
        legend_group = "Annotation$(annotation_index)"
        palette_dict = colors.colors_configuration.palette
        if palette_dict isa NamedArray
            palette_dict = Dict(zip(names(palette_dict, 1), palette_dict.array))  # UNTESTED # NOJET
        end
        for (index, (value, color)) in enumerate(palette_dict)
            has_legend_only_traces[1] = true
            push_annotation_legend_trace!(;
                traces,
                color,
                value,
                legend_group,
                legend_group_title = index == 1 ? annotation_data.title : nothing,
            )
        end
    end

    @assert colors.final_colors_values !== nothing
    if eltype(colors.final_colors_values) <: AbstractString
        gap_color = "white"
    else
        gap_color = NaN
    end

    push_bar_trace!(;  # NOJET
        traces,
        sub_graph,
        values = expanded_mask !== nothing ? expanded_mask : fill(1.0, length(annotation_data.values)),
        value_axis,
        basis_sub_graph,
        values_orientation,
        color = expand_vector(colors.final_colors_values, order, expanded_mask, gap_color),
        hovers = expand_vector(annotation_data.hovers, order, expanded_mask, ""),
        names = expand_vector(names, order, expanded_mask, ""),
        name = annotation_data.title,
        show_in_legend = false,
        implicit_values_range = MaybeRange(),
        colors_scale_index = colors.colors_scale_index,
    )

    return colors
end

function expand_vector(
    values::Maybe{AbstractVector},
    order::Maybe{AbstractVector{<:Integer}},
    expanded_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
    default_value::Any,
)::Maybe{AbstractVector}
    if values === nothing || (order === nothing && expanded_mask === nothing)
        return values
    end

    if order !== nothing
        @views values = values[order]
    end

    if expanded_mask === nothing
        return values
    end

    expanded_values = fill(default_value, length(expanded_mask))
    expanded_values[expanded_mask] .= values

    return expanded_values
end

function push_annotation_legend_trace!(;
    traces::Vector{GenericTrace},
    color::AbstractString,
    value::AbstractString,
    legend_group::Maybe{AbstractString},
    legend_group_title::Maybe{AbstractString},
)::Nothing
    push!(
        traces,
        bar(;
            x = [0],
            y = [0],
            x0 = 0,
            y0 = 0,
            xaxis = "x99",
            yaxis = "y99",
            name = value,
            marker_color = color,
            legendgroup = legend_group,
            legendgrouptitle_text = legend_group_title,
            showlegend = true,
        ),
    )

    return nothing
end

function bars_layout(;
    graph::Union{BarsGraph, SeriesBarsGraph},
    has_tick_names::Bool,
    has_legend::Bool,
    implicit_values_range::MaybeRange,
    specific_scaled_ranges::Maybe{AbstractVector{MaybeRange}} = nothing,
    colors::Maybe{ConfiguredColors} = nothing,
    annotations_colors::AbstractVector{ConfiguredColors},
    has_legend_only_traces::AbstractVector{Bool},
)::Layout
    scaled_values_range = final_scaled_range(implicit_values_range, graph.configuration.value_axis)  # NOJET

    if specific_scaled_ranges !== nothing
        specific_scaled_ranges = [
            final_scaled_range(specific_scaled_range, graph.configuration.value_axis) for
            specific_scaled_range in specific_scaled_ranges
        ]
    end

    shapes = Shape[]

    if graph isa BarsGraph
        if graph.configuration.values_orientation == VerticalValues
            push_horizontal_bands_shapes(
                shapes,
                graph.configuration.value_axis,
                scaled_values_range,
                graph.data.value_bands,
                graph.configuration.value_bands,
            )
        else
            push_vertical_bands_shapes(
                shapes,
                graph.configuration.value_axis,
                scaled_values_range,
                graph.data.value_bands,
                graph.configuration.value_bands,
            )
        end
    end

    n_annotations = length(annotations_colors)
    if n_annotations > 0
        has_legend = has_legend || any([annotation_colors.show_in_legend for annotation_colors in annotations_colors])
    end

    layout = plotly_layout(graph.configuration.figure; title = graph.data.figure_title, has_legend, shapes)

    if graph isa SeriesBarsGraph
        if graph.configuration.stacking == StackValues
            layout["barmode"] = "stack"
        elseif graph.configuration.stacking == StackFractions
            layout["barmode"] = "relative"
        else
            @assert graph.configuration.stacking === nothing
        end
        n_series = length(graph.data.series_bars_values)
        graphs_gap = graph.configuration.series_gap
        if graphs_gap === nothing
            n_graphs = 1
        else
            n_graphs = n_series
        end
    else
        n_series = 1
        graphs_gap = nothing
        n_graphs = 1
    end

    if graph.configuration.values_orientation == VerticalValues
        value_axis_letter = "y"
        bar_axis_letter = "x"
    elseif graph.configuration.values_orientation == HorizontalValues
        value_axis_letter = "x"
        bar_axis_letter = "y"
    else
        @assert false
    end

    layout["bargap"] = graph.configuration.bars_gap

    annotation_size = graph.configuration.bars_annotations

    if graph isa BarsGraph || graph.configuration.series_gap === nothing
        @assert specific_scaled_ranges === nothing
        axis_index = 1 + n_annotations
        set_layout_axis!(
            layout,
            plotly_axis(value_axis_letter, axis_index),
            graph.configuration.value_axis;
            title = prefer_data(graph.data.value_axis_title, graph.configuration.value_axis.title),
            range = scaled_values_range,
            domain = plotly_sub_graph_domain(
                SubGraph(; index = 1, n_graphs, graphs_gap, n_annotations, annotation_size),
            ),
        )
    else
        @assert graph isa SeriesBarsGraph
        @assert specific_scaled_ranges !== nothing
        for series_index in 1:n_series
            axis_index = series_index + n_annotations
            set_layout_axis!(  # NOJET
                layout,
                plotly_axis(value_axis_letter, axis_index),
                graph.configuration.value_axis;
                title = prefer_data(
                    graph.data.series_names,
                    series_index,
                    prefer_data(graph.data.value_axis_title, graph.configuration.value_axis.title),
                ),
                range = specific_scaled_ranges[series_index],
                domain = plotly_sub_graph_domain(
                    SubGraph(; index = series_index, n_graphs, graphs_gap, n_annotations, annotation_size),
                ),
            )
        end
    end

    next_colors_scale_offset_index = [Int(has_legend)]

    layout["$(bar_axis_letter)axis"] =
        Dict(:showgrid => false, :showticklabels => has_tick_names, :title => graph.data.bar_axis_title)

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
    for (annotation_index, annotation_colors) in enumerate(annotations_colors)
        annotation_data = graph.data.bars_annotations[annotation_index]
        sub_graph = SubGraph(; index = -annotation_index, n_graphs, graphs_gap, n_annotations, annotation_size)
        push_plotly_annotation!(;
            plotly_annotations,
            values_sub_graph = sub_graph,
            values_orientation = graph.configuration.values_orientation,
            title = annotation_data.title,
        )
        set_layout_axis!(  # NOJET
            layout,
            plotly_axis(value_axis_letter, annotation_index),
            graph.configuration.value_axis;
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

    if has_legend_only_traces[1]
        layout["xaxis99"] = Dict(:domain => [0, 0.001], :showgrid => false, :showticklabels => false)
        layout["yaxis99"] = Dict(:domain => [0, 0.001], :showgrid => false, :showticklabels => false)
    end

    return layout
end

function push_plotly_annotation!(;
    plotly_annotations::AbstractVector,
    basis_sub_graph::Maybe{SubGraph} = nothing,
    values_sub_graph::Maybe{SubGraph} = nothing,
    values_orientation::ValuesOrientation,
    title::Maybe{AbstractString},
)::Nothing
    if title !== nothing
        xaxis_index, _, yaxis_index, _ = plotly_sub_graph_axes(; values_sub_graph, basis_sub_graph, values_orientation)
        if values_orientation == VerticalValues
            x = 0
            y = 0.5
            xanchor = "right"
            yanchor = "middle"
            textangle = nothing
        else
            x = 0.5
            y = 0
            xanchor = "center"
            yanchor = "top"
            textangle = -90
        end
        push!(
            plotly_annotations,
            Dict(
                :text => title,
                :textangle => textangle,
                :x => x,
                :y => y,
                :xanchor => xanchor,
                :yanchor => yanchor,
                :xref => "$(plotly_axis("x", xaxis_index; short = true, force = true)) domain",
                :yref => "$(plotly_axis("y", yaxis_index; short = true, force = true)) domain",
                :showarrow => false,
            ),
        )
        return nothing
    end
end

end
