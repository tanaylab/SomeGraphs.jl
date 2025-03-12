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

using PlotlyJS

import ..Utilities.Maybe

"""
    @kwdef mutable struct BarsGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration()
        value_bands::BandsConfiguration = BandsConfiguration()
        values_orientation::ValuesOrientation = VerticalValues
        bars_color::Maybe{AbstractString} = nothing
        bars_gap::Real = 0.05,
    end

Configure a graph for showing a single series of bars.

By default the values are the `y` axis (`VerticalValues`). You can flip the axes using the `values_orientation`. You can
specify bands for this axis using `value_bands`. The `bars_gap` is the fraction of white space between bars (0 for no
gap, must be less than 1). The `bars_colors` is used to control the color of the bars (if not specified, chosen
automatically by Plotly), in combination with the data bar colors (if any).
"""
@kwdef mutable struct BarsGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    value_axis::AxisConfiguration = AxisConfiguration()
    value_bands::BandsConfiguration = BandsConfiguration()
    values_orientation::ValuesOrientation = VerticalValues
    bars_color::Maybe{AbstractString} = nothing
    bars_gap::Real = 0.05
end

function Validations.validate(context::ValidationContext, configuration::BarsGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "value_axis", configuration.value_axis)
    validate_field(context, "value_bands", configuration.value_bands)

    validate_in(context, "bars_color") do
        return validate_is_color(context, configuration.bars_color)
    end

    validate_in(context, "bars_gap") do
        validate_is_at_least(context, configuration.bars_gap, 0)
        return validate_is_below(context, configuration.bars_gap, 1)
    end

    return nothing
end

"""
    @kwdef mutable struct BarsGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        bar_axis_title::Maybe{AbstractString} = nothing
        value_axis_title::Maybe{AbstractString} = nothing
        bars_values::AbstractVector{<:Real} = Float32[]
        bars_names::Maybe{AbstractVector{<:AbstractString}} = nothing
        bars_colors::Maybe{AbstractVector{<:AbstractString}} = nothing
        bars_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
        value_bands::BandsData = BandsData()
    end

The data for a graph of a single series of bars.

By default, all the titles are empty. You can specify the overall `figure_title` as well as the `bars_axis_title` and
`value_axis_title` for the axes, , and a name for each bar in `bars_names`. You can specify a color for each bar in
`bars_colors`.
"""
@kwdef mutable struct BarsGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    bar_axis_title::Maybe{AbstractString} = nothing
    value_axis_title::Maybe{AbstractString} = nothing
    bars_values::AbstractVector{<:Real} = Float32[]
    bars_names::Maybe{AbstractVector{<:AbstractString}} = nothing
    bars_colors::Maybe{AbstractVector{<:AbstractString}} = nothing
    bars_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
    value_bands::BandsData = BandsData()
end

function Validations.validate(context::ValidationContext, data::BarsGraphData)::Nothing
    validate_vector_is_not_empty(context, "bars_values", data.bars_values)

    n_bars = length(data.bars_values)

    validate_vector_length(context, "bars_names", data.bars_names, "bars_values", n_bars)

    validate_vector_length(context, "bars_colors", data.bars_colors, "bars_values", n_bars)

    validate_vector_length(context, "bars_hovers", data.bars_hovers, "bars_values", n_bars)

    validate_vector_entries(context, "bars_colors", data.bars_colors) do _, bars_color
        validate_is_color(context, bars_color)
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
        value_bands::BandsData = BandsData()]
    )::BarsGraph

Create a [`BarsGraph`](@ref) by initializing only the [`BarsGraphData`](@ref) fields.
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
        ),
        BarsGraphConfiguration(),
    )
end

function Common.validate_graph(graph::BarsGraph)::Nothing
    validate_values(
        ValidationContext(["graph.data.bars_values"]),
        graph.data.bars_values,
        ValidationContext(["graph.configuration.value_axis"]),
        graph.configuration.value_axis,
    )

    validate_graph_bands(
        "value_bands",
        graph.configuration.value_bands,
        graph.data.value_bands,
        graph.configuration.value_axis,
    )

    return nothing
end

function Common.graph_to_figure(graph::BarsGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    implicit_values_range = MaybeRange()

    push_bar_trace!(;
        traces,
        graph,
        values = graph.data.bars_values,
        color = prefer_data(graph.data.bars_colors, graph.configuration.bars_color),
        hovers = graph.data.bars_hovers,
        names = graph.data.bars_names,
        implicit_values_range,
    )

    layout = bars_layout(;
        graph,
        has_tick_names = graph.data.bars_names !== nothing,
        show_legend = false,
        implicit_values_range,
    )

    return plotly_figure(traces, layout)
end

"""
    @kwdef mutable struct SeriesBarsGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration()
        values_orientation::ValuesOrientation = VerticalValues
        bars_gap::Maybe{Real} = nothing
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
    bars_gap::Real = 0.05
    series_gap::Maybe{Real} = nothing
    stacking::Maybe{Stacking} = nothing
end

function Validations.validate(context::ValidationContext, configuration::SeriesBarsGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "value_axis", configuration.value_axis)

    validate_in(context, "bars_gap") do
        validate_is_at_least(context, configuration.bars_gap, 0)
        return validate_is_below(context, configuration.bars_gap, 1)
    end

    validate_in(context, "series_gap") do
        validate_is_at_least(context, configuration.bars_gap, 0)
        return validate_is_below(context, configuration.bars_gap, 1)
    end

    if configuration.stacking !== nothing && configuration.series_gap !== nothing
        throw(ArgumentError("can't specify both $(location(context)).stacking and $(location(context)).series_gap"))
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
        series_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing]
    )::SeriesBarsGraph

Create a [`SeriesBarsGraph`](@ref) by initializing only the [`SeriesBarsGraphData`](@ref) fields.
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
        SeriesBarsGraphConfiguration(),
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
                if scaled_value < 0
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
            graph,
            sub_graph = SubGraph(; index = series_index, n_graphs = n_series, gap = graph.configuration.series_gap),
            name = prefer_data(graph.data.series_names, series_index, nothing),
            values = graph.data.series_bars_values[series_index],
            color = prefer_data(graph.data.series_colors, series_index, nothing),
            hovers,
            show_legend = graph.data.series_names !== nothing && graph.configuration.series_gap === nothing,
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

    layout = bars_layout(;
        graph,
        has_tick_names = graph.data.bars_names !== nothing,
        show_legend = graph.data.series_names !== nothing && graph.configuration.series_gap === nothing,
        implicit_values_range,
        specific_scaled_ranges,
    )

    return plotly_figure(traces, layout)
end

function push_bar_trace!(;
    traces::Vector{GenericTrace},
    graph::Union{BarsGraph, SeriesBarsGraph},
    values::AbstractVector{<:Real},
    color::Maybe{Union{AbstractVector{<:AbstractString}, AbstractString}} = nothing,
    hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    name::Maybe{AbstractString} = nothing,
    sub_graph::Maybe{SubGraph} = nothing,
    show_legend::Bool = false,
    implicit_values_range::MaybeRange,
)::AbstractVector{<:AbstractFloat}
    scaled_values = scale_axis_values(graph.configuration.value_axis, values; clamp = false)
    if eltype(scaled_values) <: Integer
        scaled_values = Float32.(scaled_values)
    end
    collect_range!(implicit_values_range, scaled_values)

    xaxis, x0, yaxis, y0 = plotly_sub_graph_axes(sub_graph, graph.configuration.values_orientation)

    if names === nothing
        names = ["Bar $(index)" for index in 1:length(scaled_values)]
    end

    if graph.configuration.values_orientation == VerticalValues
        xs = names
        ys = scaled_values
        orientation = "v"
    elseif graph.configuration.values_orientation == HorizontalValues
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
            xaxis,
            yaxis,
            name,
            orientation = orientation,
            marker_color = color,
            customdata = hovers,
            hovertemplate = hovers === nothing ? nothing : "%{customdata}<extra></extra>",
            showlegend = show_legend,
        ),
    )

    return scaled_values
end

function bars_layout(;
    graph::Union{BarsGraph, SeriesBarsGraph},
    has_tick_names::Bool,
    show_legend::Bool,
    implicit_values_range::MaybeRange,
    specific_scaled_ranges::Maybe{AbstractVector{MaybeRange}} = nothing,
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

    layout =
        plotly_layout(graph.configuration.figure; title = graph.data.figure_title, showlegend = show_legend, shapes)

    if graph isa SeriesBarsGraph
        if graph.configuration.stacking == StackValues
            layout["barmode"] = "stack"
        elseif graph.configuration.stacking == StackFractions
            layout["barmode"] = "relative"
        else
            @assert graph.configuration.stacking === nothing
        end
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

    if graph isa BarsGraph || graph.configuration.series_gap === nothing
        @assert specific_scaled_ranges === nothing
        set_layout_axis!(
            layout,
            "$(value_axis_letter)axis",
            graph.configuration.value_axis;
            title = graph.data.value_axis_title,
            range = scaled_values_range,
        )
    else
        @assert graph isa SeriesBarsGraph
        @assert specific_scaled_ranges !== nothing
        n_series = length(graph.data.series_bars_values)
        for series_index in 1:n_series
            set_layout_axis!(  # NOJET
                layout,
                series_index == 1 ? "$(value_axis_letter)axis" : "$(value_axis_letter)axis$(series_index)",
                graph.configuration.value_axis;
                title = prefer_data(graph.data.series_names, series_index, graph.data.value_axis_title),
                range = specific_scaled_ranges[series_index],
                domain = plotly_sub_graph_domain(
                    SubGraph(; index = series_index, n_graphs = n_series, gap = graph.configuration.series_gap),
                ),
            )
        end
    end

    layout["$(bar_axis_letter)axis"] =
        Dict(:showgrid => false, :showticklabels => has_tick_names, :title => graph.data.bar_axis_title)

    return layout
end

end
