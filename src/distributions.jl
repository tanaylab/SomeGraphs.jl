"""
Graphs for showing probability distributions.
"""
module Distributions

export BoxDistribution
export CurveBoxDistribution
export CurveDistribution
export DistributionConfiguration
export DistributionGraphConfiguration
export DistributionGraphData
export DistributionStyle
export DistributionsGraphConfiguration
export DistributionsGraphData
export HistogramDistribution
export ViolinBoxDistribution
export ViolinDistribution
export distribution_graph
export distributions_graph

using PlotlyJS

using ..Common
using ..Utilities
using ..Validations

import ..Validations.Maybe

"""
Possible styles for visualizing a distribution:

`CurveDistribution` - a density curve (the default).

`ViolinDistribution` - same as a density curve but mirrored below the values axis.

`BoxDistribution` - a box with whiskers to show important distribution values.

`CurveBoxDistribution` - combine a curve and a box.

`ViolinBoxDistribution` - combine a violin and a box.

`HistogramDistribution` - a histogram of the distribution.
"""
@enum DistributionStyle CurveDistribution ViolinDistribution BoxDistribution CurveBoxDistribution ViolinBoxDistribution HistogramDistribution

"""
    @kwdef mutable struct DistributionConfiguration <: Validated
        values_orientation::ValuesOrientation = HorizontalValues
        style::DistributionStyle = CurveDistribution
        show_outliers::Bool = false
        line::LineConfiguration = LineConfiguration(; is_filled = true)
    end

Configure the style of the distribution(s) in a graph.

The `values_orientation` will determine the overall orientation of the graph.

If `style` uses a box, then if `show_outliers`, also show the extreme (outlier) points.

The `line.color` is chosen automatically by default. When showing multiple distributions, you can override it per each
one in the [`DistributionsGraphData`](@ref). By default, the distribution is filled. Plotly only allows for solid lines
for distributions, and always fills histogram plots without any line.
"""
@kwdef mutable struct DistributionConfiguration <: Validated
    values_orientation::ValuesOrientation = HorizontalValues
    style::DistributionStyle = CurveDistribution
    show_outliers::Bool = false
    line::LineConfiguration = LineConfiguration(; is_filled = true)
end

function Validations.validate(
    context::ValidationContext,
    distribution_configuration::DistributionConfiguration,
)::Maybe{AbstractString}
    if distribution_configuration.show_outliers &&
       !(distribution_configuration.style in (BoxDistribution, ViolinBoxDistribution, CurveBoxDistribution))
        throw(
            ArgumentError(
                "specified $(location(context)).show_outliers\n" *
                "for non-box $(location(context)).style: $(distribution_configuration.style)",
            ),
        )
    end

    validate_field(context, "line", distribution_configuration.line)

    if distribution_configuration.line.style != SolidLine
        throw(ArgumentError("unsupported $(location(context)).line.style: $(distribution_configuration.line.style)"))
    end

    if distribution_configuration.style == HistogramDistribution
        if distribution_configuration.line.width !== nothing
            throw(
                ArgumentError(
                    "unsupported $(location(context)).line.width: $(distribution_configuration.line.width)\n" *
                    "for $(location(context)).style: $(distribution_configuration.style)",
                ),
            )
        end

        if !distribution_configuration.line.is_filled
            throw(
                ArgumentError(
                    "unsupported $(location(context)).line.is_filled: $(distribution_configuration.line.is_filled)\n" *
                    "for $(location(context)).style: $(distribution_configuration.style)",
                ),
            )
        end
    end

    return nothing
end

"""
    @kwdef mutable struct DistributionGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        distribution::DistributionConfiguration = DistributionConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration()
        value_bands::BandsConfiguration = BandsConfiguration()
    end

Configure a graph for showing a single distribution.
"""
@kwdef mutable struct DistributionGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    distribution::DistributionConfiguration = DistributionConfiguration()
    value_axis::AxisConfiguration = AxisConfiguration()
    value_bands::BandsConfiguration = BandsConfiguration()
end

function Validations.validate(
    context::ValidationContext,
    configuration::DistributionGraphConfiguration,
)::Maybe{AbstractString}
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "distribution", configuration.distribution)
    validate_field(context, "value_axis", configuration.value_axis)
    validate_field(context, "value_bands", configuration.value_bands, configuration.value_axis)

    return nothing
end

"""
    @kwdef mutable struct DistributionsGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        distribution::DistributionConfiguration = DistributionConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration()
        distributions_gap::Maybe{Real} = 0.05
    end

Configure a graph for showing multiple distributions.

This is similar to [`DistributionGraphConfiguration`](@ref), with additions to deal with having multiple distributions.

If `distributions_gap` is set to `nothing`, overlay the distributions on top of each other. Otherwise, the distributions
are plotted next to each other, with the `distributions_gap` specified as a fraction of the used graph size. If zero the
graphs will be adjacent, if 1 then the gaps will be the same size as the graphs.
"""
@kwdef mutable struct DistributionsGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    distribution::DistributionConfiguration = DistributionConfiguration()
    value_axis::AxisConfiguration = AxisConfiguration()
    distributions_gap::Maybe{Real} = 0.05
end

function Validations.validate(
    context::ValidationContext,
    configuration::DistributionsGraphConfiguration,
)::Maybe{AbstractString}
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "distribution", configuration.distribution)
    validate_field(context, "value_axis", configuration.value_axis)

    validate_is_at_least(context, configuration.distributions_gap, 0)

    if configuration.distribution.style == BoxDistribution && configuration.distributions_gap === nothing
        throw(ArgumentError("overlay (no $(location(context)).distributions_gap specified) for box distributions"))
    end

    return nothing
end

"""
    @kwdef mutable struct DistributionGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        value_axis_title::Maybe{AbstractString} = nothing
        distribution_values::AbstractVector{<:Real} = Float32[]
        distribution_name::Maybe{AbstractString} = nothing
        distribution_color::Maybe{AbstractString} = nothing
        value_bands::BandsData = BandsData()
    end

By default, all the titles are empty. You can specify the overall `figure_title` as well as the `value_axis_title`. The
optional `distribution_name` is used as the name of the density axis. You can also specify the `distribution_color`
and/or `value_bands` offsets here, if they are more of a data than a configuration parameter in the specific graph. This
will override whatever is specified in the configuration.
"""
@kwdef mutable struct DistributionGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    value_axis_title::Maybe{AbstractString} = nothing
    distribution_values::AbstractVector{<:Real} = Float32[]
    distribution_name::Maybe{AbstractString} = nothing
    distribution_color::Maybe{AbstractString} = nothing
    value_bands::BandsData = BandsData()
end

function Validations.validate(context::ValidationContext, data::DistributionGraphData)::Maybe{AbstractString}
    validate_vector_is_not_empty(context, "distribution_values", data.distribution_values)
    return nothing
end

"""
    @kwdef mutable struct DistributionsGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        value_axis_title::Maybe{AbstractString} = nothing
        distributions_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[]
        distributions_names::Maybe{AbstractVector{<:AbstractString}} = nothing
        distributions_colors::Maybe{AbstractVector{<:AbstractString}} = nothing
    end

The data for a multiple distributions graph. By default, all the titles are empty. You can specify the overall
`figure_title` as well as the `value_axis_title`. If specified, the `distributions_names` and/or the
`distributions_colors` vectors must contain the same number of elements as the number of vectors in the
`distributions_values`.
"""
@kwdef mutable struct DistributionsGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    value_axis_title::Maybe{AbstractString} = nothing
    distributions_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[]
    distributions_names::Maybe{AbstractVector{<:AbstractString}} = nothing
    distributions_colors::Maybe{AbstractVector{<:AbstractString}} = nothing
end

function Validations.validate(context::ValidationContext, data::DistributionsGraphData)::Maybe{AbstractString}
    validate_vector_is_not_empty(context, "distributions_values", data.distributions_values)

    n_distributions = length(data.distributions_values)

    validate_vector_length(
        context,
        "distributions_names",
        data.distributions_names,
        "distributions_values",
        n_distributions,
    )
    validate_vector_length(
        context,
        "distributions_colors",
        data.distributions_colors,
        "distributions_values",
        n_distributions,
    )

    validate_vector_entries(context, "distributions_colors", data.distributions_colors) do _, distribution_color
        validate_is_color(context, distribution_color)
        return nothing
    end

    validate_vector_entries(context, "distributions_values", data.distributions_values) do _, distribution_values
        validate_vector_is_not_empty(context, distribution_values)
        return nothing
    end

    return nothing
end

"""
A graph for visualizing a single distribution. See [`DistributionGraphData`](@ref) and
[`DistributionGraphConfiguration`](@ref).
"""
DistributionGraph = Graph{DistributionGraphData, DistributionGraphConfiguration}

"""
    distribution_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        value_axis_title::Maybe{AbstractString} = nothing,
        distribution_values::AbstractVector{<:Real} = Float32[],
        distribution_name::Maybe{AbstractString} = nothing],
    )::DistributionGraph

Create a [`DistributionGraph`](@ref) by initializing only the [`DistributionGraphData`](@ref) fields.
"""
function distribution_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    value_axis_title::Maybe{AbstractString} = nothing,
    distribution_values::AbstractVector{<:Real} = Float32[],
    distribution_name::Maybe{AbstractString} = nothing,
)::DistributionGraph
    return DistributionGraph(
        DistributionGraphData(; figure_title, value_axis_title, distribution_values, distribution_name),
        DistributionGraphConfiguration(),
    )
end

"""
A graph for visualizing multiple distributions. See [`DistributionsGraphData`](@ref) and
[`DistributionsGraphConfiguration`](@ref).
"""
DistributionsGraph = Graph{DistributionsGraphData, DistributionsGraphConfiguration}

"""
    distributions_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        value_axis_title::Maybe{AbstractString} = nothing,
        distributions_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[],
        distributions_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
        distributions_colors::Maybe{AbstractVector{<:AbstractString}} = nothing],
    )::DistributionsGraph

Create a [`DistributionsGraph`](@ref) by initializing only the [`DistributionsGraphData`](@ref) fields.
"""
function distributions_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    value_axis_title::Maybe{AbstractString} = nothing,
    distributions_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[],
    distributions_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    distributions_colors::Maybe{AbstractVector{<:AbstractString}} = nothing,
)::DistributionsGraph
    return DistributionsGraph(
        DistributionsGraphData(;
            figure_title,
            value_axis_title,
            distributions_values,
            distributions_names,
            distributions_colors,
        ),
        DistributionsGraphConfiguration(),
    )
end

function Common.validate_graph(graph::DistributionGraph)::Nothing
    validate_graph_bands(
        "value_bands",
        graph.configuration.value_bands,
        graph.data.value_bands,
        graph.configuration.value_axis,
    )
    return nothing
end

function Common.graph_to_figure(graph::DistributionGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    implicit_values_range = Vector{Maybe{Float32}}([nothing, nothing])

    push!(
        traces,
        distribution_trace(;  # NOJET
            values = graph.data.distribution_values,
            name = prefer_data(graph.data.distribution_name, "Trace"),
            color = prefer_data(graph.data.distribution_color, graph.configuration.distribution.line.color),
            width = graph.configuration.distribution.line.width,
            is_filled = graph.configuration.distribution.line.is_filled,
            configuration = graph.configuration,
            implicit_values_range,
        ),
    )

    return plotly_figure(traces, distribution_layout(; graph = graph, implicit_values_range, show_legend = false))
end

function Common.graph_to_figure(graph::DistributionsGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    implicit_values_range = Vector{Maybe{Float32}}([nothing, nothing])

    n_distributions = length(graph.data.distributions_values)

    traces = [
        distribution_trace(;
            values = graph.data.distributions_values[index],
            name = prefer_data(graph.data.distributions_names, index, nothing),
            color = prefer_data(graph.data.distributions_colors, index, graph.configuration.distribution.line.color),
            width = graph.configuration.distribution.line.width,
            is_filled = graph.configuration.distribution.line.is_filled,
            configuration = graph.configuration,
            sub_graph = SubGraph(; index = index, overlay = graph.configuration.distributions_gap === nothing),
            implicit_values_range,
            scale_group = "Distributions",
        ) for index in 1:n_distributions
    ]

    show_legend = graph.configuration.distributions_gap === nothing && graph.data.distributions_names !== nothing

    return plotly_figure(traces, distribution_layout(; graph = graph, implicit_values_range, show_legend))
end

function distribution_trace(;
    values::AbstractVector{<:Real},
    name::Maybe{AbstractString},
    color::Maybe{AbstractString},
    width::Maybe{Real},
    is_filled::Bool,
    configuration::Union{DistributionGraphConfiguration, DistributionsGraphConfiguration},
    sub_graph::Maybe{SubGraph} = nothing,
    implicit_values_range::Vector{Maybe{Float32}},
    scale_group::Maybe{AbstractString} = nothing,
)::GenericTrace
    scaled_values = scale_axis_values(configuration.value_axis, values; clamp = false)
    range_of(scaled_values, implicit_values_range)
    @assert !any(implicit_values_range .=== nothing)

    if configuration.distribution.values_orientation == VerticalValues
        y = scaled_values
        x = nothing

        yaxis = nothing
        y0 = nothing

        if sub_graph === nothing
            xaxis = nothing
            x0 = nothing
        else
            xaxis = sub_graph.overlay || sub_graph.index == 1 ? "x" : "x$(sub_graph.index)"
            x0 = 0
        end

    elseif configuration.distribution.values_orientation == HorizontalValues
        x = scaled_values
        y = nothing

        xaxis = nothing
        x0 = nothing

        if sub_graph === nothing
            yaxis = nothing
            y0 = nothing
        else
            yaxis = sub_graph.overlay || sub_graph.index == 1 ? "y" : "y$(sub_graph.index)"
            y0 = 0
        end

    else
        @assert false
    end

    if configuration.distribution.style == BoxDistribution
        tracer = box
    elseif configuration.distribution.style == HistogramDistribution
        tracer = histogram
    else
        tracer = violin
    end

    return tracer(;
        x,
        y,
        xaxis,
        yaxis,
        x0,
        y0,
        side = configuration.distribution.style in (CurveDistribution, CurveBoxDistribution) ? "positive" : nothing,
        box_visible = configuration.distribution.style in
                      (BoxDistribution, ViolinBoxDistribution, CurveBoxDistribution),
        points = if configuration.distribution.style in (ViolinBoxDistribution, CurveBoxDistribution) &&
                    configuration.distribution.show_outliers
            "outliers"
        else
            false
        end,
        boxpoints = if configuration.distribution.style == BoxDistribution && configuration.distribution.show_outliers
            "outliers"
        else
            false
        end,
        name,
        line_color = color,
        line_width = width,
        fillcolor = is_filled ? fill_color(color) : "transparent",
        scalegroup = scale_group,
        spanmode = "hard",
    )
end

function distribution_layout(;
    graph::Union{DistributionGraph, DistributionsGraph},
    implicit_values_range::Vector{Maybe{Float32}},
    show_legend::Bool,
)::Layout
    expand_range!(implicit_values_range)
    scaled_values_range = final_scaled_range(implicit_values_range, graph.configuration.value_axis)  # NOJET

    shapes = Shape[]

    if graph.configuration.distribution.values_orientation == VerticalValues
        value_axis_letter = "y"
        distributions_axis_letter = "x"
    elseif graph.configuration.distribution.values_orientation == HorizontalValues
        value_axis_letter = "x"
        distributions_axis_letter = "y"
    else
        @assert false
    end

    if graph isa DistributionGraph
        if graph.configuration.distribution.values_orientation == VerticalValues
            push_horizontal_bands_shapes(
                shapes,
                graph.configuration.value_axis,
                scaled_values_range,
                graph.data.value_bands,
                graph.configuration.value_bands,
            )
        elseif graph.configuration.distribution.values_orientation == HorizontalValues
            push_vertical_bands_shapes(
                shapes,
                graph.configuration.value_axis,
                scaled_values_range,
                graph.data.value_bands,
                graph.configuration.value_bands,
            )
        end
    end

    layout = Layout(; title = graph.data.figure_title, showlegend = show_legend, shapes)  # NOJET
    patch_layout_figure!(layout, graph.configuration.figure)

    value_axis_name = "$(value_axis_letter)axis"
    layout[value_axis_name] = Dict(:title => graph.data.value_axis_title, :range => scaled_values_range)
    patch_layout_axis!(layout, value_axis_name, graph.configuration.value_axis)

    if graph isa DistributionGraph
        layout["$(distributions_axis_letter)axis"] =
            Dict(:showticklabels => false, :title => graph.data.distribution_name)

    elseif graph isa DistributionsGraph
        n_distributions = length(graph.data.distributions_values)  # NOJET
        distributions_gap = graph.configuration.distributions_gap  # NOJET

        for index in 1:n_distributions
            if distributions_gap === nothing || n_distributions == 1
                domain = nothing
            else
                graph_size = 1 / (n_distributions + (n_distributions - 1) * distributions_gap)
                gap_size = graph_size * distributions_gap
                domain = [(index - 1) * (graph_size + gap_size), (index - 1) * (graph_size + gap_size) + graph_size]
            end

            distributions_axis_name =
                index == 1 ? "$(distributions_axis_letter)axis" : "$(distributions_axis_letter)axis$(index)"
            layout[distributions_axis_name] = Dict(:title => if distributions_gap === nothing
                nothing
            else
                prefer_data(graph.data.distributions_names, index, nothing)
            end, :domain => domain)
        end
    else
        @assert false
    end

    return layout
end

end  # module
