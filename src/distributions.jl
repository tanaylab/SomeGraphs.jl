"""
Graphs for showing probability distributions.
"""
module Distributions

export BoxDistribution
export BoxOutliersDistribution
export CumulativeDistribution
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

using PlotlyBase

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
    @kwdef mutable struct DistributionConfiguration <: Validated
        values_orientation::ValuesOrientation = HorizontalValues
        style::DistributionStyle = CurveDistribution
        line::LineConfiguration = LineConfiguration(; is_filled = true)
        normalize::Bool = false
        cumulative_descending::Bool = false
    end

Configure the style of the distribution(s) in a graph.

The `values_orientation` will determine the overall orientation of the graph.

The `line.color` is chosen automatically by default. When showing multiple distributions, you can override it per each
one in the [`DistributionsGraphData`](@ref). By default, the distribution is filled. Plotly only allows for solid lines
for distributions, and always fills histogram plots without any line.

If `normalize` is set, the density axis shows the fraction of the entries instead of their count; this is only allowed
for `HistogramDistribution` and `CumulativeDistribution`. For a `CumulativeDistribution`, if `cumulative_descending` is
set, we count the entries down to some value (so the graph is descending) instead of up to some value (so the graph is
ascending).
"""
@kwdef mutable struct DistributionConfiguration <: Validated
    values_orientation::ValuesOrientation = HorizontalValues
    style::DistributionStyle = CurveDistribution
    line::LineConfiguration = LineConfiguration(; is_filled = true)
    normalize::Bool = false
    cumulative_descending::Bool = false
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

    if distribution_configuration.normalize &&
       distribution_configuration.style ∉ (HistogramDistribution, CumulativeDistribution)
        throw(
            ArgumentError(
                "specified $(location(context)).normalize\n" *
                "for $(location(context)).style: $(distribution_configuration.style)",
            ),
        )
    end

    if distribution_configuration.cumulative_descending && distribution_configuration.style != CumulativeDistribution
        throw(
            ArgumentError(
                "specified $(location(context)).cumulative_descending\n" *
                "for non-cumulative $(location(context)).style: $(distribution_configuration.style)",
            ),
        )
    end

    return nothing
end

"""
    @kwdef mutable struct DistributionGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        distribution::DistributionConfiguration = DistributionConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
        value_bands::BandsConfiguration = BandsConfiguration()
        density_axis::AxisConfiguration = AxisConfiguration()
        cumulative_bands::BandsConfiguration = BandsConfiguration()
    end

Configure a graph for showing a single distribution. The `density_axis` configures the inner density axis (the bin
counts for a `HistogramDistribution`, the cumulative scale for a `CumulativeDistribution`); it must be left at its
default for the other styles, which have no meaningful density scale. The `cumulative_bands` are only used if the
`distribution.style` is `CumulativeDistribution`; their offsets are always in fractions (between 0 and 1) regardless of
the `distribution.normalize` and `density_axis.percent` settings.
"""
@kwdef mutable struct DistributionGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    distribution::DistributionConfiguration = DistributionConfiguration()
    value_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
    value_bands::BandsConfiguration = BandsConfiguration()
    density_axis::AxisConfiguration = AxisConfiguration()
    cumulative_bands::BandsConfiguration = BandsConfiguration()
end

# Whether an axis configuration is left at its defaults (used to reject a specified density axis for the styles which
# have no meaningful density scale).
function is_default_axis(axis_configuration::AxisConfiguration)::Bool
    return axis_configuration.minimum === nothing &&
           axis_configuration.maximum === nothing &&
           axis_configuration.log_scale === nothing &&
           axis_configuration.log_regularization == 0 &&
           !axis_configuration.percent &&
           axis_configuration.show_ticks &&
           axis_configuration.ticks_angle === nothing &&
           axis_configuration.show_grid &&
           axis_configuration.grid_color == "lightgrey" &&
           axis_configuration.title === nothing
end

# Whether an axis configuration only uses the fields that apply to a categorical (cross-series names) axis, namely
# `show_ticks`, `ticks_angle` and `title`; the numeric and grid fields must be left at their defaults.
function is_categorical_axis(axis_configuration::AxisConfiguration)::Bool
    return axis_configuration.minimum === nothing &&
           axis_configuration.maximum === nothing &&
           axis_configuration.log_scale === nothing &&
           axis_configuration.log_regularization == 0 &&
           !axis_configuration.percent &&
           axis_configuration.show_grid &&
           axis_configuration.grid_color == "lightgrey"
end

function Validations.validate(
    context::ValidationContext,
    configuration::DistributionGraphConfiguration,
)::Maybe{AbstractString}
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "distribution", configuration.distribution)
    validate_field(context, "value_axis", configuration.value_axis)
    validate_field(context, "value_bands", configuration.value_bands, configuration.value_axis)
    validate_field(context, "density_axis", configuration.density_axis)
    validate_field(context, "cumulative_bands", configuration.cumulative_bands)

    if configuration.density_axis.log_scale !== nothing
        throw(
            ArgumentError(
                "unsupported $(location(context)).density_axis.log_scale: $(configuration.density_axis.log_scale)",
            ),
        )
    end

    if configuration.distribution.style ∉ (HistogramDistribution, CumulativeDistribution) &&
       !is_default_axis(configuration.density_axis)
        throw(
            ArgumentError(
                "specified $(location(context)).density_axis\n" *
                "for $(location(context)).distribution.style: $(configuration.distribution.style)",
            ),
        )
    end

    if configuration.density_axis.percent && !configuration.distribution.normalize
        throw(
            ArgumentError(
                "specified $(location(context)).density_axis.percent\n" *
                "without $(location(context)).distribution.normalize",
            ),
        )
    end

    if configuration.distribution.style != CumulativeDistribution
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
        value_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
        density_axis::AxisConfiguration = AxisConfiguration()
        series_axis::AxisConfiguration = AxisConfiguration()
        distributions_gap::Maybe{Real} = 0.05
    end

Configure a graph for showing multiple distributions.

This is similar to [`DistributionGraphConfiguration`](@ref), with additions to deal with having multiple distributions.
The `density_axis` configures the inner density axis exactly as in the single-distribution graph.

If `distributions_gap` is set to `nothing`, overlay the distributions on top of each other. Otherwise, the distributions
are plotted next to each other, with the `distributions_gap` specified as a fraction of the used graph size. If zero the
graphs will be adjacent, if 1 then the gaps will be the same size as the graphs.

When there is a gap, the `series_axis` configures the cross-series names axis (the per-series names shown next to the
sub-graphs). Only its `show_ticks` (whether to show the names), `ticks_angle` (by default the names are shown parallel
to the axis, as if they were its title) and `title` are used; the numeric and grid fields are not applicable.
"""
@kwdef mutable struct DistributionsGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    distribution::DistributionConfiguration = DistributionConfiguration()
    value_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
    density_axis::AxisConfiguration = AxisConfiguration()
    series_axis::AxisConfiguration = AxisConfiguration()
    distributions_gap::Maybe{Real} = 0.05
end

function Validations.validate(
    context::ValidationContext,
    configuration::DistributionsGraphConfiguration,
)::Maybe{AbstractString}
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "distribution", configuration.distribution)
    validate_field(context, "value_axis", configuration.value_axis)
    validate_field(context, "density_axis", configuration.density_axis)

    validate_is_at_least(context, configuration.distributions_gap, 0)

    if configuration.distributions_gap === nothing && configuration.distribution.style in
       (BoxDistribution, BoxOutliersDistribution, ViolinBoxDistribution, CurveBoxDistribution)
        throw(ArgumentError("overlay (no $(location(context)).distributions_gap specified) for box distributions"))
    end

    if configuration.density_axis.log_scale !== nothing
        throw(
            ArgumentError(
                "unsupported $(location(context)).density_axis.log_scale: $(configuration.density_axis.log_scale)",
            ),
        )
    end

    if configuration.distribution.style ∉ (HistogramDistribution, CumulativeDistribution) &&
       !is_default_axis(configuration.density_axis)
        throw(
            ArgumentError(
                "specified $(location(context)).density_axis\n" *
                "for $(location(context)).distribution.style: $(configuration.distribution.style)",
            ),
        )
    end

    if configuration.density_axis.percent && !configuration.distribution.normalize
        throw(
            ArgumentError(
                "specified $(location(context)).density_axis.percent\n" *
                "without $(location(context)).distribution.normalize",
            ),
        )
    end

    validate_field(context, "series_axis", configuration.series_axis)

    if !is_categorical_axis(configuration.series_axis)
        throw(
            ArgumentError(
                "specified numeric or grid fields of $(location(context)).series_axis\n" *
                "(only show_ticks, ticks_angle and title apply to the cross-series names)",
            ),
        )
    end

    if configuration.distributions_gap === nothing && !is_default_axis(configuration.series_axis)
        throw(
            ArgumentError(
                "specified $(location(context)).series_axis\n" *
                "for overlay (no $(location(context)).distributions_gap)",
            ),
        )
    end

    if configuration.density_axis.title !== nothing && configuration.series_axis.title !== nothing
        throw(
            ArgumentError(
                "can't specify both $(location(context)).density_axis.title\n" *
                "and $(location(context)).series_axis.title",
            ),
        )
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
        cumulative_bands::BandsData = BandsData()
    end

The data for a single distribution graph. By default, all the titles are empty. You can specify the overall
`figure_title` as well as the `value_axis_title`. The optional `distribution_name` is used as the name of the density
axis. You can also specify the `distribution_color` and/or `value_bands` offsets here, if they are more of a data than a
configuration parameter in the specific graph. This will override whatever is specified in the configuration.

The `cumulative_bands` should only be specified if the `distribution.style` is `CumulativeDistribution`.
"""
@kwdef mutable struct DistributionGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    value_axis_title::Maybe{AbstractString} = nothing
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
        density_axis_title::Maybe{AbstractString} = nothing
        series_axis_title::Maybe{AbstractString} = nothing
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

The `density_axis_title` titles the inner density axis. The `series_axis_title` titles the cross-series names axis and
may only be specified when there is a gap (`distributions_gap` is not `nothing`). At most one of the two may be
specified; when there is a gap, whichever is given is used to title the series axis.
"""
@kwdef mutable struct DistributionsGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    value_axis_title::Maybe{AbstractString} = nothing
    density_axis_title::Maybe{AbstractString} = nothing
    series_axis_title::Maybe{AbstractString} = nothing
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
    distribution_values::AbstractVector{<:Real} = Float32[],
    distribution_name::Maybe{AbstractString} = nothing,
    configuration::DistributionGraphConfiguration = DistributionGraphConfiguration(),
)::DistributionGraph
    return DistributionGraph(
        DistributionGraphData(; figure_title, value_axis_title, distribution_values, distribution_name),
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
        density_axis_title::Maybe{AbstractString} = nothing,
        series_axis_title::Maybe{AbstractString} = nothing,
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
    density_axis_title::Maybe{AbstractString} = nothing,
    series_axis_title::Maybe{AbstractString} = nothing,
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
            density_axis_title,
            series_axis_title,
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

    if graph.data.density_axis_title !== nothing && graph.data.series_axis_title !== nothing
        throw(ArgumentError(chomp("""
                                  can't specify both graph.data.density_axis_title: $(graph.data.density_axis_title)
                                  and graph.data.series_axis_title: $(graph.data.series_axis_title)
                                  """)))
    end

    if graph.data.series_axis_title !== nothing && graph.configuration.distributions_gap === nothing
        throw(ArgumentError(chomp("""
                                  can't specify graph.data.series_axis_title: $(graph.data.series_axis_title)
                                  for overlay (no graph.configuration.distributions_gap)
                                  """)))
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
            is_one_of_many = false,
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
            is_one_of_many = graph.configuration.distributions_gap === nothing,
        ) for (position, index) in enumerate(distributions_indices)
    ]

    has_legend = graph.configuration.distributions_gap === nothing && graph.data.distributions_names !== nothing

    return plotly_figure(traces, distribution_layout(; graph = graph, implicit_values_range, has_legend))
end

# The (descending, normalized, percent) flags controlling the cumulative density axis, taken from the `distribution` and
# `density_axis` configurations.
function cumulative_density_flags(
    configuration::Union{DistributionGraphConfiguration, DistributionsGraphConfiguration},
)::Tuple{Bool, Bool, Bool}
    return (
        configuration.distribution.cumulative_descending,
        configuration.distribution.normalize,
        configuration.density_axis.percent,
    )
end

# The Plotly `histnorm` for a histogram's density axis: counts (`nothing`), fraction (`"probability"`) or percent.
function histogram_density_histnorm(
    configuration::Union{DistributionGraphConfiguration, DistributionsGraphConfiguration},
)::Maybe{AbstractString}
    if configuration.distribution.style != HistogramDistribution
        return nothing
    elseif configuration.density_axis.percent
        return "percent"
    elseif configuration.distribution.normalize
        return "probability"
    else
        return nothing
    end
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
    is_one_of_many::Bool,
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

        is_descending, is_normalized, is_percent = cumulative_density_flags(configuration)

        if is_descending
            reverse!(cumulative_values)
        end

        if is_percent
            cumulative_values .*= 100 / n_values
        elseif is_normalized
            cumulative_values ./= n_values
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
            xaxis = plotly_axis("x", xaxis_index; short = true, force = is_one_of_many),
            yaxis = plotly_axis("y", yaxis_index; short = true, force = is_one_of_many),
            x0,
            y0,
            mode = "lines",
            name,
            line_color = color,
            line_width = width,
            fill = is_filled ? full : "none",
        )

    else # All other styles
        orientation = nothing
        if configuration.distribution.values_orientation == VerticalValues
            ys = scaled_values
            xs = nothing
            if configuration.distribution.style != HistogramDistribution
                orientation = "v"
                if is_one_of_many
                    xs = fill("X", length(ys))
                end
            end

        elseif configuration.distribution.values_orientation == HorizontalValues
            xs = scaled_values
            ys = nothing
            if configuration.distribution.style != HistogramDistribution
                orientation = "h"
                if is_one_of_many
                    ys = fill("Y", length(xs))
                end
            end

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
            xaxis = plotly_axis("x", xaxis_index; short = true, force = is_one_of_many),
            yaxis = plotly_axis("y", yaxis_index; short = true, force = is_one_of_many),
            x0,
            y0,
            histnorm = histogram_density_histnorm(configuration),
            orientation,
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
            fillcolor = is_filled ? fill_color(color) : nothing,
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
        if graph isa DistributionGraph
            density_axis = graph.configuration.density_axis
            _, is_normalized, is_percent = cumulative_density_flags(graph.configuration)

            if is_percent
                cumulative_maximum = 101
                cumulative_bands_scale = 1
            elseif is_normalized
                cumulative_maximum = 1.01
                cumulative_bands_scale = 1
            else
                max_count = length(graph.data.distribution_values)
                cumulative_maximum = max_count + 1
                cumulative_bands_scale = max_count
            end

            cumulative_range = Range(;
                minimum = density_axis.minimum === nothing ? 0 : density_axis.minimum,
                maximum = density_axis.maximum === nothing ? cumulative_maximum : density_axis.maximum,
            )

            set_layout_axis!(
                layout,
                "$(density_axis_letter)axis",
                density_axis;
                title = prefer_data(graph.data.distribution_name, density_axis.title),
                range = cumulative_range,
            )

            if graph.configuration.distribution.values_orientation == VerticalValues
                push_vertical_bands_shapes(
                    shapes,
                    density_axis,
                    cumulative_range,
                    graph.data.cumulative_bands,
                    graph.configuration.cumulative_bands,
                    cumulative_bands_scale,
                )
            elseif graph.configuration.distribution.values_orientation == HorizontalValues
                push_horizontal_bands_shapes(
                    shapes,
                    density_axis,
                    cumulative_range,
                    graph.data.cumulative_bands,
                    graph.configuration.cumulative_bands,
                    cumulative_bands_scale,
                )
            end

        elseif graph isa DistributionsGraph
            density_axis = graph.configuration.density_axis
            _, is_normalized, is_percent = cumulative_density_flags(graph.configuration)

            n_distributions = length(graph.data.distributions_values)  # NOJET
            distributions_gap = graph.configuration.distributions_gap  # NOJET
            if distributions_gap === nothing
                n_distributions = 1
            end

            max_counts = maximum(length.(graph.data.distributions_values))  # NOJET

            for index in 1:n_distributions
                if is_percent
                    cumulative_maximum = 101
                elseif is_normalized
                    cumulative_maximum = 1.01
                elseif distributions_gap === nothing
                    cumulative_maximum = max_counts + 1
                else
                    cumulative_maximum = length(graph.data.distributions_values[index]) + 1
                end

                cumulative_range = Range(;
                    minimum = density_axis.minimum === nothing ? 0 : density_axis.minimum,
                    maximum = density_axis.maximum === nothing ? cumulative_maximum : density_axis.maximum,
                )

                set_layout_axis!(
                    layout,
                    plotly_axis(density_axis_letter, index; force = true),
                    density_axis;
                    title = distributions_gap === nothing ? graph.data.density_axis_title : nothing,
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
            if graph.configuration.distribution.style == HistogramDistribution
                density_axis = graph.configuration.density_axis
                if density_axis.minimum === nothing || density_axis.maximum === nothing
                    density_range = nothing
                else
                    density_range = Range(; minimum = density_axis.minimum, maximum = density_axis.maximum)
                end
                set_layout_axis!(
                    layout,
                    "$(density_axis_letter)axis",
                    density_axis;
                    title = prefer_data(graph.data.distribution_name, density_axis.title),
                    range = density_range,
                )
            else
                layout["$(density_axis_letter)axis"] =
                    Dict(:showticklabels => false, :title => graph.data.distribution_name)
            end

        elseif graph isa DistributionsGraph
            density_axis = graph.configuration.density_axis
            is_histogram = graph.configuration.distribution.style == HistogramDistribution
            if density_axis.minimum === nothing || density_axis.maximum === nothing
                density_range = nothing
            else
                density_range = Range(; minimum = density_axis.minimum, maximum = density_axis.maximum)
            end

            n_distributions = length(graph.data.distributions_values)  # NOJET
            distributions_gap = graph.configuration.distributions_gap  # NOJET

            if distributions_gap === nothing
                if is_histogram
                    set_layout_axis!(
                        layout,
                        "$(density_axis_letter)axis",
                        density_axis;
                        title = graph.data.density_axis_title,
                        range = density_range,
                    )
                else
                    layout["$(density_axis_letter)axis"] =
                        Dict(:showticklabels => false, :title => graph.data.density_axis_title)
                end
            else
                for index in 1:n_distributions
                    axis = plotly_axis(density_axis_letter, index; force = true)
                    domain = plotly_sub_graph_domain(
                        SubGraph(; index, n_graphs = n_distributions, graphs_gap = distributions_gap),
                    )
                    if is_histogram
                        set_layout_axis!(layout, axis, density_axis; title = nothing, range = density_range, domain)
                    else
                        layout[axis] = Dict(:showticklabels => false, :title => nothing, :domain => domain)
                    end
                end
            end
        else
            @assert false
        end
    end

    maybe_push_series_annotations!(layout, graph, value_axis_letter, density_axis_letter)

    return layout
end

# The cross-series name annotations only apply to a gapped multiple-distributions graph; a single distribution graph has
# none.
function maybe_push_series_annotations!(::Layout, ::DistributionGraph, ::AbstractString, ::AbstractString)::Nothing
    return nothing
end

function maybe_push_series_annotations!(
    layout::Layout,
    graph::DistributionsGraph,
    value_axis_letter::AbstractString,
    density_axis_letter::AbstractString,
)::Nothing
    if graph.configuration.distributions_gap !== nothing
        push_series_annotations!(layout, graph, value_axis_letter, density_axis_letter)
    end
    return nothing
end

# Add the cross-series name annotations (and the series axis title) for a gapped multiple-distributions graph. The names
# replace the per-sub-graph density axis titles: each is placed along the series axis at its sub-graph, rotated by the
# `series_axis` angle (by default parallel to the axis, as an axis title would look). The series axis title is placed
# once, centered along the whole series axis.
function push_series_annotations!(
    layout::Layout,
    graph::DistributionsGraph,
    value_axis_letter::AbstractString,
    density_axis_letter::AbstractString,
)::Nothing
    configuration = graph.configuration
    series_axis = configuration.series_axis
    distributions_names = graph.data.distributions_names
    n_distributions = length(graph.data.distributions_values)
    is_vertical_values = configuration.distribution.values_orientation == VerticalValues

    series_ticks_angle = series_axis.ticks_angle
    series_angle = series_ticks_angle === nothing ? 0 : series_ticks_angle
    screen_angle = axis_screen_ticks_angle(series_angle, !is_vertical_values)
    textangle = screen_angle != 0 ? screen_angle : nothing

    plotly_annotations = []

    if series_axis.show_ticks && distributions_names !== nothing
        for index in 1:n_distributions
            density_ref = "$(plotly_axis(density_axis_letter, index; short = true, force = true)) domain"
            if is_vertical_values
                push!(
                    plotly_annotations,
                    Dict(
                        :text => distributions_names[index],
                        :textangle => textangle,
                        :x => 0.5,
                        :xref => density_ref,
                        :xanchor => "center",
                        :y => 0,
                        :yref => "$(value_axis_letter) domain",
                        :yanchor => "top",
                        :showarrow => false,
                        :automargin => true,
                    ),
                )
            else
                push!(
                    plotly_annotations,
                    Dict(
                        :text => distributions_names[index],
                        :textangle => textangle,
                        :x => 0,
                        :xref => "$(value_axis_letter) domain",
                        :xanchor => "right",
                        :y => 0.5,
                        :yref => density_ref,
                        :yanchor => "middle",
                        :showarrow => false,
                        :automargin => true,
                    ),
                )
            end
        end
    end

    series_title = prefer_data(
        graph.data.series_axis_title,
        prefer_data(graph.data.density_axis_title, prefer_data(series_axis.title, configuration.density_axis.title)),
    )
    if series_title !== nothing
        # Place the title past the names (anchored to the same plot edge, shifted out so its automargin push is the
        # outermost one and Plotly reserves room for the title beyond the names). Plotly measures the rendered names but
        # we can't, so estimate their extent perpendicular to the series axis from the longest name, the (default ~12px)
        # tick font, and the rotation: a name perpendicular to the axis takes its full length, a parallel one one line.
        font_size = 12
        if series_axis.show_ticks && distributions_names !== nothing
            longest_name = maximum(length.(distributions_names))
            names_radians = series_angle * pi / 180
            names_extent =
                longest_name * 0.6 * font_size * abs(sin(names_radians)) + font_size * abs(cos(names_radians))
        else
            names_extent = 0
        end
        names_clearance = names_extent + font_size + 5
        if is_vertical_values
            push!(
                plotly_annotations,
                Dict(
                    :text => series_title,
                    :x => 0.5,
                    :xref => "paper",
                    :xanchor => "center",
                    :y => 0,
                    :yref => "$(value_axis_letter) domain",
                    :yanchor => "bottom",
                    :yshift => -names_clearance,
                    :showarrow => false,
                    :automargin => true,
                ),
            )
        else
            push!(
                plotly_annotations,
                Dict(
                    :text => series_title,
                    :textangle => -90,
                    :x => 0,
                    :xref => "$(value_axis_letter) domain",
                    :xanchor => "left",
                    :xshift => -names_clearance,
                    :y => 0.5,
                    :yref => "paper",
                    :yanchor => "middle",
                    :showarrow => false,
                    :automargin => true,
                ),
            )
        end
    end

    if !isempty(plotly_annotations)
        layout["annotations"] = plotly_annotations
    end

    return nothing
end

end  # module
