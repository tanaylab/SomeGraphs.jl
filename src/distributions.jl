"""
Graphs for showing probability distributions.
"""
module Distributions

export BoxDistribution
export BoxOutliersDistribution
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

`BoxOutliersDistribution` - a box with outlier points and whiskers to show important distribution values.

`CurveBoxDistribution` - combine a curve and a box.

`ViolinBoxDistribution` - combine a violin and a box.

`HistogramDistribution` - a histogram of the distribution.

'CumulativeDistribution' - a cumulative distribution (aka "CDF"). This one allows for additional configuration options.
"""
@enum DistributionStyle CurveDistribution ViolinDistribution BoxDistribution BoxOutliersDistribution CurveBoxDistribution ViolinBoxDistribution HistogramDistribution CumulativeDistribution

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
        title::Maybe{AbstractString} = nothing
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
    title::Maybe{AbstractString} = nothing
end

"""
    @kwdef mutable struct DistributionConfiguration <: Validated
        values_orientation::ValuesOrientation = HorizontalValues
        style::DistributionStyle = CurveDistribution
        line::LineConfiguration = LineConfiguration(; is_filled = true)
    end

Configure the style of the distribution(s) in a graph.

The `values_orientation` will determine the overall orientation of the graph.

The `line.color` is chosen automatically by default. When showing multiple distributions, you can override it per each
one in the [`DistributionsGraphData`](@ref). By default, the distribution is filled. Plotly only allows for solid lines
for distributions, and always fills histogram plots without any line.
"""
@kwdef mutable struct DistributionConfiguration <: Validated
    values_orientation::ValuesOrientation = HorizontalValues
    style::DistributionStyle = CurveDistribution
    line::LineConfiguration = LineConfiguration(; is_filled = true)
end

function Validations.validate(
    context::ValidationContext,
    distribution_configuration::DistributionConfiguration,
)::Nothing
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

    if configuration.distributions_gap === nothing && configuration.distribution.style in
       (BoxDistribution, BoxOutliersDistribution, ViolinBoxDistribution, CurveBoxDistribution)
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

The data for a single distribution graph. By default, all the titles are empty. You can specify the overall
`figure_title` as well as the `value_axis_title`. The optional `distribution_name` is used as the name of the density
axis. You can also specify the `distribution_color` and/or `value_bands` offsets here, if they are more of a data than a
configuration parameter in the specific graph. This will override whatever is specified in the configuration.

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
        distributions_order::Maybe{AbstractVector{<:Integer}} = nothing
    end

The data for a multiple distributions graph. By default, all the titles are empty. You can specify the overall
`figure_title` as well as the `value_axis_title`. If specified, the `distributions_names` and/or the
`distributions_colors` vectors must contain the same number of elements as the number of vectors in the
`distributions_values`.

If `distributions_order` are specified, we reorder the distributions accordingly. This allows controlling which
distributions will appear on top of the others.
"""
@kwdef mutable struct DistributionsGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    value_axis_title::Maybe{AbstractString} = nothing
    distributions_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[]
    distributions_names::Maybe{AbstractVector{<:AbstractString}} = nothing
    distributions_colors::Maybe{AbstractVector{<:AbstractString}} = nothing
    distributions_order::Maybe{AbstractVector{<:Integer}} = nothing
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
    validate_vector_length(
        context,
        "distributions_order",
        data.distributions_order,
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
        distribution_name::Maybe{AbstractString} = nothing,
        configuration::DistributionGraphConfiguration = DistributionGraphConfiguration()]
    )::DistributionGraph

Create a [`DistributionGraph`](@ref) by initializing only the [`DistributionGraphData`](@ref) fields (with an optional
[`DistributionGraphConfiguration`](@ref)).
"""
function distribution_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    value_axis_title::Maybe{AbstractString} = nothing,
    cumulative_axis_title::Maybe{AbstractString} = nothing,
    distribution_values::AbstractVector{<:Real} = Float32[],
    distribution_name::Maybe{AbstractString} = nothing,
    configuration::DistributionGraphConfiguration = DistributionGraphConfiguration(),
)::DistributionGraph
    return DistributionGraph(
        DistributionGraphData(;
            figure_title,
            value_axis_title,
            cumulative_axis_title,
            distribution_values,
            distribution_name,
        ),
        configuration,
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
        distributions_colors::Maybe{AbstractVector{<:AbstractString}} = nothing,
        configuration::DistributionsGraphConfiguration = DistributionsGraphConfiguration()]
    )::DistributionsGraph

Create a [`DistributionsGraph`](@ref) by initializing only the [`DistributionsGraphData`](@ref) fields (with an optional
[`DistributionsGraphConfiguration`](@ref)).
"""
function distributions_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    value_axis_title::Maybe{AbstractString} = nothing,
    distributions_values::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[],
    distributions_names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    distributions_colors::Maybe{AbstractVector{<:AbstractString}} = nothing,
    distributions_order::Maybe{AbstractVector{<:Integer}} = nothing,
    configuration::DistributionsGraphConfiguration = DistributionsGraphConfiguration(),
)::DistributionsGraph
    return DistributionsGraph(
        DistributionsGraphData(;
            figure_title,
            value_axis_title,
            distributions_values,
            distributions_names,
            distributions_colors,
            distributions_order,
        ),
        configuration,
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

    return plotly_figure(traces, distribution_layout(; graph = graph, implicit_values_range, has_legend = false))
end

function Common.graph_to_figure(graph::DistributionsGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    implicit_values_range = MaybeRange()

    n_distributions = length(graph.data.distributions_values)

    distributions_indices = prefer_data(graph.data.distributions_order, 1:n_distributions)

    traces = [
        distribution_trace(;
            values = graph.data.distributions_values[index],
            name = prefer_data(graph.data.distributions_names, index, nothing),
            color = prefer_data(graph.data.distributions_colors, index, graph.configuration.distribution.line.color),
            width = graph.configuration.distribution.line.width,
            is_filled = graph.configuration.distribution.line.is_filled,
            configuration = graph.configuration,
            sub_graph = SubGraph(;
                index = position,
                n_graphs = n_distributions,
                graphs_gap = graph.configuration.distributions_gap,
            ),
            implicit_values_range,
            scale_group = "Distributions",
        ) for (position, index) in enumerate(distributions_indices)
    ]

    has_legend = graph.configuration.distributions_gap === nothing && graph.data.distributions_names !== nothing

    return plotly_figure(traces, distribution_layout(; graph = graph, implicit_values_range, has_legend))
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
    scaled_values = scale_axis_values(configuration.value_axis, values; clamp = false, copy = true)
    collect_range!(implicit_values_range, scaled_values)

    xaxis_index, x0, yaxis_index, y0 = plotly_sub_graph_axes(;
        basis_sub_graph = sub_graph,
        values_orientation = configuration.distribution.values_orientation,
    )

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
            xaxis = plotly_axis("x", xaxis_index; short = true),
            yaxis = plotly_axis("y", yaxis_index; short = true),
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

        if configuration.distribution.style == BoxOutliersDistribution
            boxpoints = "outliers"
        elseif configuration.distribution.style == BoxDistribution
            boxpoints = false
        else
            boxpoints = nothing
        end

        if configuration.distribution.style in (BoxDistribution, BoxOutliersDistribution)
            tracer = box
        elseif configuration.distribution.style == HistogramDistribution
            tracer = histogram
        else
            tracer = violin
        end

        return tracer(;
            x = xs,
            y = ys,
            xaxis = plotly_axis("x", xaxis_index; short = true),
            yaxis = plotly_axis("y", yaxis_index; short = true),
            x0,
            y0,
            side = if configuration.distribution.style in (CurveDistribution, CurveBoxDistribution)
                "positive"
            else
                nothing
            end,
            box_visible = configuration.distribution.style in
                          (BoxDistribution, ViolinBoxDistribution, CurveBoxDistribution),
            boxpoints,
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
    has_legend::Bool,
)::Layout
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

    layout = plotly_layout(graph.configuration.figure; title = graph.data.figure_title, has_legend, shapes)

    set_layout_axis!(
        layout,
        "$(value_axis_letter)axis",
        graph.configuration.value_axis;
        title = prefer_data(graph.data.value_axis_title, graph.configuration.value_axis.title),
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
                title = prefer_data(graph.data.cumulative_axis_title, graph.configuration.cumulative_axis.title),
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

                set_layout_axis!(
                    layout,
                    plotly_axis(density_axis_letter, index),
                    cumulative_axis_configuration;
                    title = if distributions_gap === nothing
                        nothing
                    else
                        prefer_data(graph.data.distributions_names, index, nothing)  # NOJET
                    end,
                    range = cumulative_range,
                    domain = plotly_sub_graph_domain(
                        SubGraph(; index, n_graphs = n_distributions, graphs_gap = distributions_gap),
                    ),
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
                layout[plotly_axis(density_axis_letter, index)] = Dict(
                    :title => if distributions_gap === nothing
                        nothing  # NOJET
                    else
                        prefer_data(graph.data.distributions_names, index, nothing)  # NOJET
                    end,  # NOJET
                    :domain => plotly_sub_graph_domain(
                        SubGraph(; index, n_graphs = n_distributions, graphs_gap = distributions_gap),
                    ),
                )
            end
        else
            @assert false
        end
    end

    return layout
end

end  # module
