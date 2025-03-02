"""
Graphs for showing probability distributions.
"""
module Distributions

export BoxDistribution
export CumulativeAxisConfiguration
export CumulativeCounts
export CumulativeDistribution
export CumulativeFractions
export CumulativePercents
export CumulativeUnits
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

'CumulativeDistribution' - a cumulative distribution (aka "CDF"). This one allows for additional configuration options.
"""
@enum DistributionStyle CurveDistribution ViolinDistribution BoxDistribution CurveBoxDistribution ViolinBoxDistribution HistogramDistribution CumulativeDistribution

"""
Possible units for the distribution axis of a cumulative distribution:

`CumulativeFractions` - the axis is the fraction of entries.

`CumulativePercents` - the axis is the percent of entries.

`CumulativeCounts` - the axis is the number of entries.
"""
@enum CumulativeUnits CumulativeFractions CumulativePercents CumulativeCounts

"""
    @kwdef mutable struct CumulativeAxisConfiguration
        units::CumulativeUnits = CumulativeFractions
        descending::Bool = false
        show_ticks::Bool = true
        show_grid::Bool = true
        grid_color::AbstractString = "lightgrey"
    end

Possible configurations for the distribution axis of a cumulative distribution, using the specified `units`. Normally we
count the entries up to some value, so the graph is ascending; if `descending`, we count the entries down to some value,
so the graph is descending.

This intentionally offers only a subset of the fields of [`AxisConfiguration`](@ref).
"""
@kwdef mutable struct CumulativeAxisConfiguration
    units::CumulativeUnits = CumulativeFractions
    descending::Bool = false
    show_ticks::Bool = true
    show_grid::Bool = true
    grid_color::AbstractString = "lightgrey"
end

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

Configure a graph for showing a single distribution. The `cumulative_axis` and `cumulative_bands` are only used if the
`distribution.style` is `CumulativeDistribution`. The offsets of the `cumulative_bands` are always in fractions (between
0 and 1) regardless of the `cumulative_axis.units`.
"""
@kwdef mutable struct DistributionGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    distribution::DistributionConfiguration = DistributionConfiguration()
    value_axis::AxisConfiguration = AxisConfiguration()
    value_bands::BandsConfiguration = BandsConfiguration()
    cumulative_axis::CumulativeAxisConfiguration = CumulativeAxisConfiguration()
    cumulative_bands::BandsConfiguration = BandsConfiguration()
end

function Validations.validate(
    context::ValidationContext,
    configuration::DistributionGraphConfiguration,
)::Maybe{AbstractString}
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "distribution", configuration.distribution)
    validate_field(context, "value_axis", configuration.value_axis)
    validate_field(context, "value_bands", configuration.value_bands, configuration.value_axis)
    validate_field(context, "cumulative_bands", configuration.cumulative_bands)

    if configuration.distribution.style != CumulativeDistribution
        if configuration.cumulative_axis.units !== CumulativeFractions ||
           configuration.cumulative_axis.descending ||
           !configuration.cumulative_axis.show_ticks ||
           configuration.cumulative_axis.grid_color != "lightgrey"
            throw(
                ArgumentError(
                    "specified $(location(context)).cumulative_axis\n" *
                    "for non-cumulative $(location(context)).distribution.style: $(configuration.distribution.style)",
                ),
            )
        end

        if configuration.cumulative_bands.low.offset !== nothing ||
           configuration.cumulative_bands.middle.offset !== nothing ||
           configuration.cumulative_bands.high.offset !== nothing
            throw(
                ArgumentError(
                    "specified $(location(context)).cumulative_bands\n" *
                    "for non-cumulative $(location(context)).distribution.style: $(configuration.distribution.style)",
                ),
            )
        end
    end

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
graphs will be adjacent, if 1 then the gaps will be the same size as the graphs. The `cumulative_axis` is only used if
the `distribution.style` is `CumulativeDistribution`.
"""
@kwdef mutable struct DistributionsGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    distribution::DistributionConfiguration = DistributionConfiguration()
    value_axis::AxisConfiguration = AxisConfiguration()
    cumulative_axis::CumulativeAxisConfiguration = CumulativeAxisConfiguration()
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

    if configuration.distributions_gap === nothing &&
       configuration.distribution.style in (BoxDistribution, ViolinBoxDistribution, CurveBoxDistribution)
        throw(ArgumentError("overlay (no $(location(context)).distributions_gap specified) for box distributions"))
    end

    if configuration.distribution.style != CumulativeDistribution
        if configuration.cumulative_axis.units !== CumulativeFractions ||
           configuration.cumulative_axis.descending ||
           !configuration.cumulative_axis.show_ticks ||
           configuration.cumulative_axis.grid_color != "lightgrey"
            throw(
                ArgumentError(
                    "specified $(location(context)).cumulative_axis\n" *
                    "for non-cumulative $(location(context)).distribution.style: $(configuration.distribution.style)",
                ),
            )
        end
    end

    return nothing
end

"""
    @kwdef mutable struct DistributionGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        value_axis_title::Maybe{AbstractString} = nothing
        cumulative_axis_title::Maybe{AbstractString} = nothing
        distribution_values::AbstractVector{<:Real} = Float32[]
        distribution_name::Maybe{AbstractString} = nothing
        distribution_color::Maybe{AbstractString} = nothing
        value_bands::BandsData = BandsData()
        cumulative_bands::BandsData = BandsData()
    end

By default, all the titles are empty. You can specify the overall `figure_title` as well as the `value_axis_title`. The
optional `distribution_name` is used as the name of the density axis. You can also specify the `distribution_color`
and/or `value_bands` offsets here, if they are more of a data than a configuration parameter in the specific graph. This
will override whatever is specified in the configuration.

The `cumulative_axis_title` and/of `cumulative_bands` should only be specified if the `distribution.style` is
`CumulativeDistribution`.
"""
@kwdef mutable struct DistributionGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    value_axis_title::Maybe{AbstractString} = nothing
    cumulative_axis_title::Maybe{AbstractString} = nothing
    distribution_values::AbstractVector{<:Real} = Float32[]
    distribution_name::Maybe{AbstractString} = nothing
    distribution_color::Maybe{AbstractString} = nothing
    value_bands::BandsData = BandsData()
    cumulative_bands::BandsData = BandsData()
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
        cumulative_axis_title::Maybe{AbstractString} = nothing,
        distribution_values::AbstractVector{<:Real} = Float32[],
        distribution_name::Maybe{AbstractString} = nothing],
    )::DistributionGraph

Create a [`DistributionGraph`](@ref) by initializing only the [`DistributionGraphData`](@ref) fields.
"""
function distribution_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    value_axis_title::Maybe{AbstractString} = nothing,
    cumulative_axis_title::Maybe{AbstractString} = nothing,
    distribution_values::AbstractVector{<:Real} = Float32[],
    distribution_name::Maybe{AbstractString} = nothing,
)::DistributionGraph
    return DistributionGraph(
        DistributionGraphData(;
            figure_title,
            value_axis_title,
            cumulative_axis_title,
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
    validate_values(
        ValidationContext(["graph.data.distribution_values"]),
        graph.data.distribution_values,
        ValidationContext(["graph.configuration.value_axis"]),
        graph.configuration.value_axis,
    )

    validate_graph_bands(
        "value_bands",
        graph.configuration.value_bands,
        graph.data.value_bands,
        graph.configuration.value_axis,
    )

    validate_graph_bands("cumulative_bands", graph.configuration.cumulative_bands, graph.data.cumulative_bands)

    if graph isa DistributionGraph &&
       graph.data.cumulative_axis_title !== nothing &&
       graph.configuration.distribution.style !== CumulativeDistribution
        throw(
            ArgumentError(
                "specified graph.data.cumulative_axis_title: $(graph.data.cumulative_axis_title)\n" *
                "for non-cumulative graph.configuration.distribution.style: $(graph.configuration.distribution.style)",
            ),
        )
    end

    return nothing
end

function Common.validate_graph(graph::DistributionsGraph)::Nothing
    for (index, distribution_values) in enumerate(graph.data.distributions_values)
        validate_values(
            ValidationContext(["graph.data.distributions_values", index]),
            distribution_values,
            ValidationContext(["graph.configuration.value_axis"]),
            graph.configuration.value_axis,
        )
    end

    return nothing
end

function Common.graph_to_figure(graph::DistributionGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    implicit_values_range = MaybeRange()

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

    implicit_values_range = MaybeRange()

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
    implicit_values_range::MaybeRange,
    scale_group::Maybe{AbstractString} = nothing,
)::GenericTrace
    scaled_values = scale_axis_values(configuration.value_axis, values; clamp = false)
    collect_range!(implicit_values_range, scaled_values)

    if configuration.distribution.values_orientation == VerticalValues
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

    if configuration.distribution.style == CumulativeDistribution
        n_values = length(scaled_values)

        sort!(scaled_values)
        cumulative_values = Float32.(collect(1:n_values))

        if configuration.cumulative_axis.descending
            reverse!(cumulative_values)
        end

        if configuration.cumulative_axis.units == CumulativeFractions
            cumulative_values ./= n_values
        elseif configuration.cumulative_axis.units == CumulativePercents
            cumulative_values .*= 100 / n_values
        else
            @assert configuration.cumulative_axis.units == CumulativeCounts
        end

        mask = fill(true, n_values)
        did_mask = false
        for index in 2:(n_values - 1)
            if scaled_values[index - 1] == scaled_values[index] == scaled_values[index + 1]
                mask[index] = false
                did_mask = true
            end
        end

        if did_mask
            scaled_values = scaled_values[mask]
            cumulative_values = cumulative_values[mask]
        end

        if configuration.distribution.values_orientation == VerticalValues
            xs = cumulative_values
            ys = scaled_values
            full = "tozerox"
        elseif configuration.distribution.values_orientation == HorizontalValues
            xs = scaled_values
            ys = cumulative_values
            full = "tozeroy"
        else
            @assert false
        end

        return scatter(;
            x = xs,
            y = ys,
            xaxis,
            yaxis,
            x0,
            y0,
            mode = "lines",
            name,
            line_color = color,
            line_width = width,
            fill = is_filled ? full : "none",
        )

    else # All other styles
        if configuration.distribution.values_orientation == VerticalValues
            ys = scaled_values
            xs = nothing

        elseif configuration.distribution.values_orientation == HorizontalValues
            xs = scaled_values
            ys = nothing

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
            x = xs,
            y = ys,
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
            boxpoints = if configuration.distribution.style == BoxDistribution &&
                           configuration.distribution.show_outliers
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
end

function distribution_layout(;
    graph::Union{DistributionGraph, DistributionsGraph},
    implicit_values_range::MaybeRange,
    show_legend::Bool,
)::Layout
    implicit_values_range = assert_range(implicit_values_range)
    expand_range!(implicit_values_range)
    scaled_values_range = final_scaled_range(implicit_values_range, graph.configuration.value_axis)  # NOJET

    shapes = Shape[]

    if graph.configuration.distribution.values_orientation == VerticalValues
        value_axis_letter = "y"
        density_axis_letter = "x"
    elseif graph.configuration.distribution.values_orientation == HorizontalValues
        value_axis_letter = "x"
        density_axis_letter = "y"
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

    layout =
        plotly_layout(graph.configuration.figure; title = graph.data.figure_title, showlegend = show_legend, shapes)

    set_layout_axis!(
        layout,
        "$(value_axis_letter)axis",
        graph.configuration.value_axis;
        title = graph.data.value_axis_title,
        range = scaled_values_range,
    )

    if graph.configuration.distribution.style == CumulativeDistribution
        cumulative_axis_configuration = AxisConfiguration(;
            log_scale = nothing,
            percent = graph.configuration.cumulative_axis.units == CumulativePercents,
            show_ticks = graph.configuration.cumulative_axis.show_ticks,
            show_grid = graph.configuration.cumulative_axis.show_grid,
            grid_color = graph.configuration.cumulative_axis.grid_color,
        )

        if graph isa DistributionGraph
            if graph.configuration.cumulative_axis.units == CumulativeCounts
                max_count = length(graph.data.distribution_values)
                cumulative_range = Range(; minimum = 0, maximum = max_count + 1)
                cumulative_bands_scale = max_count
            elseif graph.configuration.cumulative_axis.units == CumulativeFractions
                cumulative_range = Range(; minimum = 0, maximum = 1.01)
                cumulative_bands_scale = 1
            elseif graph.configuration.cumulative_axis.units == CumulativePercents
                cumulative_range = Range(; minimum = 0, maximum = 101)
                cumulative_bands_scale = 1
            else
                @assert false
            end

            set_layout_axis!(
                layout,
                "$(density_axis_letter)axis",
                cumulative_axis_configuration;
                title = graph.data.cumulative_axis_title,
                range = cumulative_range,
            )

            if graph.configuration.distribution.values_orientation == VerticalValues
                push_vertical_bands_shapes(
                    shapes,
                    cumulative_axis_configuration,
                    cumulative_range,
                    graph.data.cumulative_bands,
                    graph.configuration.cumulative_bands,
                    cumulative_bands_scale,
                )
            elseif graph.configuration.distribution.values_orientation == HorizontalValues
                push_horizontal_bands_shapes(
                    shapes,
                    cumulative_axis_configuration,
                    cumulative_range,
                    graph.data.cumulative_bands,
                    graph.configuration.cumulative_bands,
                    cumulative_bands_scale,
                )
            end

        elseif graph isa DistributionsGraph
            n_distributions = length(graph.data.distributions_values)  # NOJET
            distributions_gap = graph.configuration.distributions_gap  # NOJET

            max_counts = maximum(length.(graph.data.distributions_values))  # NOJET

            for index in 1:n_distributions
                if graph.configuration.cumulative_axis.units == CumulativeCounts
                    if distributions_gap === nothing
                        max_count = max_counts
                    else
                        max_count = length(graph.data.distributions_values[index])
                    end
                    cumulative_range = Range(; minimum = 0, maximum = max_count + 1)
                    cumulative_bands_scale = max_count
                elseif graph.configuration.cumulative_axis.units == CumulativeFractions
                    cumulative_range = Range(; minimum = 0, maximum = 1.01)
                    cumulative_bands_scale = 1
                elseif graph.configuration.cumulative_axis.units == CumulativePercents
                    cumulative_range = Range(; minimum = 0, maximum = 101)
                    cumulative_bands_scale = 1
                else
                    @assert false
                end

                if distributions_gap === nothing || n_distributions == 1
                    domain = nothing
                else
                    graph_size = 1 / (n_distributions + (n_distributions - 1) * distributions_gap)
                    gap_size = graph_size * distributions_gap
                    domain = [(index - 1) * (graph_size + gap_size), (index - 1) * (graph_size + gap_size) + graph_size]
                end

                density_axis_name = index == 1 ? "$(density_axis_letter)axis" : "$(density_axis_letter)axis$(index)"
                set_layout_axis!(
                    layout,
                    density_axis_name,
                    cumulative_axis_configuration;
                    title = if distributions_gap === nothing
                        nothing
                    else
                        prefer_data(graph.data.distributions_names, index, nothing)
                    end,
                    range = cumulative_range,
                    domain,
                )
            end

        else
            @assert false
        end

    else  # All other styles
        if graph isa DistributionGraph
            layout["$(density_axis_letter)axis"] =
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

                density_axis_name = index == 1 ? "$(density_axis_letter)axis" : "$(density_axis_letter)axis$(index)"
                layout[density_axis_name] = Dict(:title => if distributions_gap === nothing
                    nothing
                else
                    prefer_data(graph.data.distributions_names, index, nothing)  # NOJET
                end, :domain => domain)
            end
        else
            @assert false
        end
    end

    return layout
end

end  # module
