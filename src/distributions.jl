"""
Graphs for showing probability distributions.
"""
module Distributions

export DistributionConfiguration
export DistributionGraphConfiguration
export DistributionGraphData
export DistributionsGraphConfiguration
export DistributionsGraphData
export distribution_graph
export distributions_graph

using PlotlyJS

using ..Common
using ..Utilities
using ..Validations

import ..Utilities.Maybe  # For static analysis.

"""
    @kwdef mutable struct DistributionConfiguration <: Validated
        values_orientation::ValuesOrientation = HorizontalValues
        show_box::Bool = false
        show_violin::Bool = false
        show_curve::Bool = true
        show_outliers::Bool = false
        color::Maybe{AbstractString} = nothing
    end

Configure the style of a distribution graph.

The `values_orientation` controls which axis is used for the values (the other axis is used for the density). By default
the values are shown on the Y axis (vertical values).

If `show_box`, show a box graph.

If `show_violin`, show a violin graph.

If `show_curve`, show a density curve. This is the default.

You can combine the above; however, a density curve is just the positive side of a violin graph, so you can't combine
the two.

In addition to the (combination) of the above, if `show_outliers`, also show the extreme (outlier) points.

The `color` is chosen automatically by default. When showing multiple distributions, you can override it per each one in
the [`DistributionsGraphData`](@ref).
"""
@kwdef mutable struct DistributionConfiguration <: Validated
    values_orientation::ValuesOrientation = HorizontalValues
    show_box::Bool = false
    show_violin::Bool = false
    show_curve::Bool = true
    show_outliers::Bool = false
    color::Maybe{AbstractString} = nothing
end

function Validations.validate(
    context::ValidationContext,
    distribution_configuration::DistributionConfiguration,
)::Maybe{AbstractString}
    if !distribution_configuration.show_box &&
       !distribution_configuration.show_violin &&
       !distribution_configuration.show_curve
        throw(
            ArgumentError(
                "must specify at least one of: " *
                "$(location(context)).show_box, " *
                "$(location(context)).show_violin, " *
                "$(location(context)).show_curve",
            ),
        )
    end

    if distribution_configuration.show_violin && distribution_configuration.show_curve
        throw(
            ArgumentError(
                "must not specify both of: " * "$(location(context)).show_violin, " * "$(location(context)).show_curve",
            ),
        )
    end

    validate_in(context, "color") do
        return validate_is_color(context, distribution_configuration.color)
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
    validate_in(context, "figure") do
        return validate(context, configuration.figure)
    end
    validate_in(context, "distribution") do
        return validate(context, configuration.distribution)
    end
    validate_in(context, "value_axis") do
        return validate(context, configuration.value_axis)
    end
    validate_in(context, "value_bands") do
        return validate(context, configuration.value_bands, "value_axis", configuration.value_axis)
    end

    return nothing
end

"""
    @kwdef mutable struct DistributionsGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        distribution::DistributionConfiguration = DistributionConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration()
        show_legend::Bool = false
        overlay_distributions::Bool = false
        distributions_gap::Maybe{Real} = nothing
    end

Configure a graph for showing multiple distributions.

This is similar to [`DistributionGraphConfiguration`](@ref), with additions to deal with having multiple distributions.

If `show_legend`, we add a legend listing the distributions. If `overlay_distributions`, we plot the distributions on
top of one another. Otherwise, `distributions_gap` can be used to adjust the amount of space between the distribution
plots.

!!! note

    Adjusting the `distributions_gap` will end badly if using `show_curve`, because Plotly.
"""
@kwdef mutable struct DistributionsGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    distribution::DistributionConfiguration = DistributionConfiguration(; show_curve = false, show_box = true)
    value_axis::AxisConfiguration = AxisConfiguration()
    show_legend::Bool = false
    overlay_distributions::Bool = false
    distributions_gap::Maybe{Real} = nothing
end

function Validations.validate(
    context::ValidationContext,
    configuration::DistributionsGraphConfiguration,
)::Maybe{AbstractString}
    validate_in(context, "figure") do
        validate(context, configuration.figure)
        return nothing
    end
    validate_in(context, "distribution") do
        validate(context, configuration.distribution)
        return nothing
    end
    validate_in(context, "value_axis") do
        validate(context, configuration.value_axis)
        return nothing
    end

    validate_in(context, "distributions_gap") do
        validate_is_at_least(context, configuration.distributions_gap, 0)
        validate_is_below(context, configuration.distributions_gap, 1)
        return nothing
    end

    return nothing
end

"""
    @kwdef mutable struct DistributionGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        value_axis_title::Maybe{AbstractString} = nothing
        trace_axis_title::Maybe{AbstractString} = nothing
        distribution_values::AbstractVector{<:Real} = Float32[]
        distribution_name::Maybe{AbstractString} = nothing
        value_bands::BandsData = BandsData()
    end

By default, all the titles are empty. You can specify the overall `figure_title` as well as the `value_axis_title` and
the `trace_axis_title`. The optional `distribution_name` is used as the tick value for the distribution. You can also
specify the bands offsets here if they are more of a data than a configuration parameter in the specific graph. This
will override whatever is specified in the configuration.
"""
@kwdef mutable struct DistributionGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    value_axis_title::Maybe{AbstractString} = nothing
    trace_axis_title::Maybe{AbstractString} = nothing
    distribution_values::AbstractVector{<:Real} = Float32[]
    distribution_name::Maybe{AbstractString} = nothing
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
        trace_axis_title::Maybe{AbstractString} = nothing
        legend_title::Maybe{AbstractString} = nothing
        distributions_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[]
        distributions_names::Maybe{AbstractVector{<:AbstractString}} = nothing
        distributions_colors::Maybe{AbstractVector{<:AbstractString}} = nothing
    end

The data for a multiple distributions graph. By default, all the titles are empty. You can specify the overall
`figure_title` as well as the `value_axis_title`, the `trace_axis_title` and the `legend_title` (if `show_legend` is
set). If specified, the `distributions_names` and/or the `distributions_colors` vectors must contain the same number of
elements as the number of vectors in the `distributions_values`.
"""
@kwdef mutable struct DistributionsGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    value_axis_title::Maybe{AbstractString} = nothing
    trace_axis_title::Maybe{AbstractString} = nothing
    legend_title::Maybe{AbstractString} = nothing
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
        return validate_is_color(context, distribution_color)
    end

    validate_vector_entries(context, "distributions_values", data.distributions_values) do _, distribution_values
        return validate_vector_is_not_empty(context, distribution_values)
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
        trace_axis_title::Maybe{AbstractString} = nothing,
        distribution_values::AbstractVector{<:Real} = Float32[],
        distribution_name::Maybe{AbstractString} = nothing],
    )::DistributionGraph

Create a [`DistributionGraph`](@ref) by initializing only the [`DistributionGraphData`](@ref) fields.
"""
function distribution_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    value_axis_title::Maybe{AbstractString} = nothing,
    trace_axis_title::Maybe{AbstractString} = nothing,
    distribution_values::AbstractVector{<:Real} = Float32[],
    distribution_name::Maybe{AbstractString} = nothing,
)::DistributionGraph
    return DistributionGraph(
        DistributionGraphData(;
            figure_title,
            value_axis_title,
            trace_axis_title,
            distribution_values,
            distribution_name,
        ),
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
        trace_axis_title::Maybe{AbstractString} = nothing,
        legend_title::Maybe{AbstractString} = nothing,
        distributions_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[],
        distributions_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
        distributions_colors::Maybe{AbstractVector{<:AbstractString}} = nothing],
    )::DistributionsGraph

Create a [`DistributionsGraph`](@ref) by initializing only the [`DistributionsGraphData`](@ref) fields.
"""
function distributions_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    value_axis_title::Maybe{AbstractString} = nothing,
    trace_axis_title::Maybe{AbstractString} = nothing,
    legend_title::Maybe{AbstractString} = nothing,
    distributions_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[],
    distributions_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    distributions_colors::Maybe{AbstractVector{<:AbstractString}} = nothing,
)::DistributionsGraph
    return DistributionsGraph(
        DistributionsGraphData(;
            figure_title,
            value_axis_title,
            trace_axis_title,
            legend_title,
            distributions_values,
            distributions_names,
            distributions_colors,
        ),
        DistributionsGraphConfiguration(),
    )
end

const BOX = 1
const VIOLIN = 2
const CURVE = 4

function Common.graph_to_figure(graph::DistributionGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    implicit_values_range = Vector{Maybe{Float32}}([nothing, nothing])

    push!(
        traces,
        distribution_trace(;  # NOJET
            distribution_values = graph.data.distribution_values,
            distribution_name = graph.data.distribution_name === nothing ? "Trace" : graph.data.distribution_name,
            color = graph.configuration.distribution.color,
            legend_title = nothing,
            configuration = graph.configuration,
            overlay_distributions = false,
            implicit_values_range,
        ),
    )

    layout = distribution_layout(;
        graph = graph,
        has_tick_names = graph.data.distribution_name !== nothing,
        show_legend = false,
        distributions_gap = nothing,
        implicit_values_range,
    )

    return plotly_figure(traces, layout)
end

function Common.graph_to_figure(graph::DistributionsGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    if graph.configuration.distributions_gap !== nothing && graph.configuration.distribution.show_curve
        @warn "setting the distributions_gap for curve is buggy in plotly"
    end

    if graph.configuration.distributions_gap === nothing
        graph.configuration.distributions_gap = 0.05
    end

    implicit_values_range = Vector{Maybe{Float32}}([nothing, nothing])

    n_distributions = length(graph.data.distributions_values)
    traces = [
        distribution_trace(;
            distribution_values = graph.data.distributions_values[index],
            distribution_name = if graph.data.distributions_names === nothing
                "Trace $(index)"
            else
                graph.data.distributions_names[index]
            end,
            color = if graph.data.distributions_colors === nothing
                graph.configuration.distribution.color
            else
                graph.data.distributions_colors[index]
            end,
            legend_title = graph.data.legend_title,
            configuration = graph.configuration,
            overlay_distributions = graph.configuration.overlay_distributions,
            is_first = index == 1,
            implicit_values_range,
        ) for index in 1:n_distributions
    ]

    layout = distribution_layout(;
        graph = graph,
        has_tick_names = graph.data.distributions_names !== nothing,
        show_legend = graph.configuration.show_legend,
        distributions_gap = graph.configuration.distributions_gap,
        implicit_values_range,
    )

    return plotly_figure(traces, layout)
end

function distribution_trace(;
    distribution_values::AbstractVector{<:Real},
    distribution_name::AbstractString,
    color::Maybe{AbstractString},
    legend_title::Maybe{AbstractString},
    configuration::Union{DistributionGraphConfiguration, DistributionsGraphConfiguration},
    overlay_distributions::Bool,
    is_first::Bool = true,
    implicit_values_range::Vector{Maybe{Float32}},
)::GenericTrace
    style = (
        (configuration.distribution.show_box ? BOX : 0) |
        (configuration.distribution.show_violin ? VIOLIN : 0) |
        (configuration.distribution.show_curve ? CURVE : 0)
    )

    scaled_values = scale_axis_values(configuration.value_axis, distribution_values)
    for value in scaled_values
        if value !== nothing && implicit_values_range[1] === nothing || implicit_values_range[1] > value
            implicit_values_range[1] = value
        end
        if value !== nothing && implicit_values_range[2] === nothing || implicit_values_range[2] < value
            implicit_values_range[2] = value
        end
    end

    if configuration.distribution.values_orientation == VerticalValues
        x = nothing
        y = scaled_values
        x0 = overlay_distributions ? " " : nothing
        y0 = nothing
    elseif configuration.distribution.values_orientation == HorizontalValues
        x = scaled_values
        y = nothing
        x0 = nothing
        y0 = overlay_distributions ? " " : nothing
    else
        @assert false
    end

    points = configuration.distribution.show_outliers ? "outliers" : false
    tracer = style == BOX ? box : violin

    return tracer(;
        x = x,
        y = y,
        x0 = x0,
        y0 = y0,
        side = configuration.distribution.show_curve ? "positive" : nothing,
        box_visible = configuration.distribution.show_box,
        boxpoints = points,
        points = points,
        name = distribution_name,
        marker_color = color,
        legendgroup = distribution_name,
        legendgrouptitle_text = is_first ? legend_title : nothing,
    )
end

function distribution_layout(;
    graph::Union{DistributionGraph, DistributionsGraph},
    has_tick_names::Bool,
    show_legend::Bool,
    distributions_gap::Maybe{Real},
    implicit_values_range::Vector{Maybe{Float32}},
)::Layout
    if graph.configuration.distribution.show_outliers
        implicit_values_range .*= [0.99, 1.01]  # NOJET
    end

    explicit_values_range = scale_axis_values(
        graph.configuration.value_axis,
        [graph.configuration.value_axis.minimum, graph.configuration.value_axis.maximum],
    )

    scaled_values_range = [
        explicit_value === nothing ? implicit_value : explicit_value for
        (explicit_value, implicit_value) in zip(explicit_values_range, implicit_values_range)
    ]

    shapes = nothing

    if graph.configuration.distribution.values_orientation == VerticalValues
        xaxis_showticklabels = has_tick_names
        xaxis_title = graph.data.trace_axis_title
        xaxis_range = (nothing, nothing)
        xaxis_tickprefix = nothing
        xaxis_ticksuffix = nothing
        xaxis_zeroline = nothing

        yaxis_showticklabels = graph.configuration.figure.show_ticks
        yaxis_showgrid = graph.configuration.figure.show_grid
        yaxis_title = graph.data.value_axis_title
        yaxis_range = scaled_values_range
        yaxis_tickprefix, yaxis_ticksuffix = axis_ticksformat(graph.configuration.value_axis)
        yaxis_zeroline = graph.configuration.value_axis.log_scale === nothing ? nothing : false

        if graph isa DistributionGraph
            shapes = horizontal_bands_shapes(
                graph.configuration.value_axis,
                scaled_values_range,
                graph.data.value_bands,
                graph.configuration.value_bands,
            )
        end

    elseif graph.configuration.distribution.values_orientation == HorizontalValues
        xaxis_showticklabels = graph.configuration.figure.show_ticks
        xaxis_title = graph.data.value_axis_title
        xaxis_range = scaled_values_range
        xaxis_tickprefix, xaxis_ticksuffix = axis_ticksformat(graph.configuration.value_axis)
        xaxis_zeroline = graph.configuration.value_axis.log_scale === nothing ? nothing : false

        yaxis_showticklabels = has_tick_names
        yaxis_showgrid = false
        yaxis_title = graph.data.trace_axis_title
        yaxis_range = (nothing, nothing)
        yaxis_tickprefix = nothing
        yaxis_ticksuffix = nothing
        yaxis_zeroline = nothing

        if graph isa DistributionGraph
            shapes = vertical_bands_shapes(
                graph.configuration.value_axis,
                scaled_values_range,
                graph.data.value_bands,
                graph.configuration.value_bands,
            )
        end
    else
        @assert false
    end

    layout = Layout(;  # NOJET
        title = graph.data.figure_title,
        xaxis_showgrid = graph.configuration.figure.show_grid,
        xaxis_gridcolor = graph.configuration.figure.grid_color,
        xaxis_showticklabels = xaxis_showticklabels,
        xaxis_title = xaxis_title,
        xaxis_range = xaxis_range,
        xaxis_tickprefix = xaxis_tickprefix,
        xaxis_ticksuffix = xaxis_ticksuffix,
        xaxis_zeroline = xaxis_zeroline,
        yaxis_showgrid = yaxis_showgrid,
        yaxis_gridcolor = graph.configuration.figure.grid_color,
        yaxis_showticklabels = yaxis_showticklabels,
        yaxis_title = yaxis_title,
        yaxis_range = yaxis_range,
        yaxis_tickprefix = yaxis_tickprefix,
        yaxis_ticksuffix = yaxis_ticksuffix,
        yaxis_zeroline = yaxis_zeroline,
        showlegend = show_legend,
        legend_tracegroupgap = 0,
        legend_itemdoubleclick = false,
        violingroupgap = distributions_gap === nothing ? nothing : 0,
        boxgroupgap = distributions_gap === nothing ? nothing : 0,
        boxgap = distributions_gap,
        violingap = distributions_gap,
        shapes,
    )

    return graph_layout(graph.configuration.figure, layout)
end

end
