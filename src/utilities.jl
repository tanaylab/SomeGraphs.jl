"""
Utility functions for defining graph types. We do not re-export all symbols from this sub-module to the global
`MCGraphs` namespace. You can safely ignore these unless you are implementing a new graph type.
"""
module Utilities

export expand_range!
export fill_color
export final_scaled_range
export plotly_figure
export plotly_layout
export prefer_data
export push_diagonal_bands_shapes
export push_horizontal_bands_shapes
export push_vertical_bands_shapes
export range_of
export scale_axis_value
export scale_axis_values
export scale_size_values
export set_layout_axis!
export validate_colors
export validate_graph_bands

using Colors
using PlotlyJS
using Reexport

using ..Validations
using ..Common

import .Common.Maybe
@reexport import .Common.validate_graph
@reexport import .Common.graph_to_figure

"""
    validate_graph_bands(
        field::AbstractString,
        bands_configuration::BandsConfiguration,
        bands_data::BandsData,
        axis_configuration::AxisConfiguration,
    )::Nothing

Validate that the bands configuration and data is compatible. Assumes these are specified as the same `field` in both
the graph's data and configuration.
"""
function validate_graph_bands(
    field::AbstractString,
    bands_configuration::BandsConfiguration,
    bands_data::BandsData,
    axis_configuration::AxisConfiguration,
)::Nothing
    if bands_configuration.middle.line.is_filled
        if bands_configuration.low.offset === nothing && bands_data.low_offset === nothing
            throw(
                ArgumentError(
                    "graph.configuration.$(field).middle.line.is_filled" *
                    " requires graph.configuration.$(field).low.offset" *
                    " or graph.data.$(field).low_offset",
                ),
            )
        end
        if bands_configuration.high.offset === nothing && bands_data.high_offset === nothing
            throw(
                ArgumentError(
                    "graph.configuration.$(field).middle.line.is_filled" *
                    " requires graph.configuration.$(field).high.offset" *
                    " or graph.data.$(field).high_offset",
                ),
            )
        end
    end

    if axis_configuration.log_scale !== nothing
        validate_is_above(ValidationContext(["graph.configuration", field, "low_offset"]), bands_data.low_offset, 0)
        validate_is_above(
            ValidationContext(["graph.configuration", field, "middle_offset"]),
            bands_data.middle_offset,
            0,
        )
        validate_is_above(ValidationContext(["graph.configuration", field, "high_offset"]), bands_data.high_offset, 0)
    end

    return nothing
end

"""
    validate_colors(
        colors_data_context::ValidationContext,
        colors_data::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}},
        colors_configuration_context::ValidationContext,
        colors_configuration::ColorsConfiguration,
        mask::Maybe{Union{AbstractVector{Bool},BitVector}} = nothing,
    )::Maybe{AbstractString}

Validate that the `colors_data` from the `colors_data_context` is valid and consistent with the `colors_configuration`
from the `colors_configuration_context`. For example, if the color configuration contains a categorical color mapping,
this will validate that all the color names in the data are valid keys of this mapping.

If a `mask` is specified, do not validate colors in the data whose matching value in the mask is false.
"""
function validate_colors(
    colors_data_context::ValidationContext,
    colors_data::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}},
    colors_configuration_context::ValidationContext,
    colors_configuration::ColorsConfiguration,
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
)::Maybe{AbstractString}
    if colors_configuration.fixed isa AbstractString
        if colors_data !== nothing
            throw(
                ArgumentError(
                    "can't specify $(location(colors_data_context))\n" *
                    "for $(location(colors_configuration_context)).fixed: $(colors_configuration.fixed)",
                ),
            )
        end
    end

    if colors_configuration.show_legend && colors_data === nothing
        throw(
            ArgumentError(
                "must specify $(location(colors_data_context))\n" *
                "for $(location(colors_configuration_context)).show_legend",
            ),
        )
    end

    if colors_configuration.axis.minimum !== nothing ||
       colors_configuration.axis.maximum !== nothing ||
       colors_configuration.axis.log_scale !== nothing ||
       colors_configuration.axis.percent
        if !(colors_data isa AbstractVector{<:Real})
            throw(
                ArgumentError(
                    "must specify numeric $(location(colors_data_context))\n" *
                    "when using any of $(location(colors_configuration_context)).axis.(minimum,maximum,log_scale,percent)",
                ),
            )
        end

        if colors_configuration.axis.log_scale !== nothing
            validate_vector_entries(colors_data_context, colors_data, mask) do _, color  # NOJET
                if colors_configuration.axis.minimum === nothing || color >= colors_configuration.axis.minimum
                    validate_in(
                        colors_data_context,
                        "(value + $(location(colors_configuration_context)).axis.log_regularization)",
                    ) do
                        validate_is_above(colors_data_context, color + colors_configuration.axis.log_regularization, 0)
                        return nothing
                    end
                end
            end
        end
    end

    if colors_configuration.palette === nothing
        if colors_data isa AbstractVector{<:AbstractString}
            validate_vector_entries(colors_data_context, colors_data, mask) do _, color  # NOJET
                return validate_is_color(colors_data_context, color)
            end
        end

    elseif colors_configuration.palette isa CategoricalColors
        if colors_data isa AbstractVector{<:AbstractString}
            validate_vector_entries(colors_data_context, colors_data, mask) do _, color  # NOJET
                if !haskey(colors_configuration.palette, color)
                    throw(
                        ArgumentError(
                            "invalid $(location(colors_data_context)): $(color)\n" *
                            "does not exist in $(location(colors_configuration_context)).palette",
                        ),
                    )
                end
            end
        elseif colors_data isa AbstractVector{<:Real}
            throw(
                ArgumentError(
                    "numeric $(location(colors_data_context))\n" *
                    "specified for categorical $(location(colors_configuration_context)).palette",
                ),
            )

        else
            @assert colors_data === nothing
            throw(
                ArgumentError(
                    "must specify (categorical) $(location(colors_data_context))\n" *
                    "for categorical $(location(colors_configuration_context)).palette",
                ),
            )
        end

    elseif colors_configuration.palette isa ContinuousColors
        if colors_data isa AbstractVector{<:AbstractString}
            throw(
                ArgumentError(
                    "categorical $(location(colors_data_context))\n" *
                    "specified for continuous $(location(colors_configuration_context)).palette",
                ),
            )
        elseif colors_data === nothing
            throw(
                ArgumentError(
                    "must specify (numeric) $(location(colors_data_context))\n" *
                    "for continuous $(location(colors_configuration_context)).palette",
                ),
            )

        else
            @assert colors_data isa AbstractVector{<:Real}
        end

    else
        @assert false
    end

    return nothing
end

"""
    plotly_layout(
        figure_configuration::FigureConfiguration;
        title::Maybe{AbstractString},
        showlegend::Bool,
        shapes::AbstractVector{Shape},
    )::Layout

Create a Plotly `Layout` object.
"""
function plotly_layout(
    figure_configuration::FigureConfiguration;
    title::Maybe{AbstractString},
    showlegend::Bool,
    shapes::AbstractVector{Shape},
)::Layout
    return Layout(;  # NOJET
        title,
        showlegend,
        legend_itemdoubleclick = showlegend ? false : nothing,
        shapes,
        margin_l = figure_configuration.margins.left,
        margin_r = figure_configuration.margins.right,
        margin_t = figure_configuration.margins.top,
        margin_b = figure_configuration.margins.bottom,
        template = "simple_white",
        width = prefer_data(figure_configuration.width, nothing),
        height = prefer_data(figure_configuration.height, nothing),
        plot_bgcolor = figure_configuration.background_color,
        paper_bgcolor = figure_configuration.paper_color,
    )
end

"""
    set_layout_axis!(
        layout::Layout,
        axis::AbstractString
        axis_configuration::AxisConfiguration;
        title::Maybe{AbstractString},
        range::AbstractVector{<:Real},
    )::Nothing

Add a Plotly `axis` in a `layout` using the `axis_configuration`.
"""
function set_layout_axis!(
    layout::Layout,
    axis::AbstractString,
    axis_configuration::AxisConfiguration;
    title::Maybe{AbstractString},
    range::AbstractVector{<:Real},
)::Nothing
    layout[axis] = Dict(
        :title => title,
        :range => range,
        :showgrid => axis_configuration.show_grid,
        :gridcolor => axis_configuration.show_grid ? axis_configuration.grid_color : nothing,
        :showticklabels => axis_configuration.show_ticks,
        :tickprefix => axis_ticks_prefix(axis_configuration),
        :ticksuffix => axis_ticks_suffix(axis_configuration),
        :zeroline => axis_configuration.log_scale === nothing,
    )
    return nothing
end

"""
    set_layout_colorscale!(
        layout::Layout,
        colorscale::AbstractString,
        colors_configuration::ColorsConfiguration;
        offset::Maybe{Real},
        title::Maybe{AbstractString},
    )::Nothing

Set a `colorscale` in a Plotly `layout`, as specified by a `colors_configuration`. Since Plotly is dumb when it comes to
placement of color scales, the `offset` must be specified manually to avoid overlaps.
"""
function set_layout_colorscale!(;  # UNTESTED
    layout::Layout,
    color_scale::AbstractString,
    colors_configuration::ColorsConfiguration,
    offset::Maybe{Real},
    range::Maybe{AbstractVector{Real}} = nothing,
    title::Maybe{AbstractString},
)::Nothing
    return layout[color_scale] = Dict(
        :showscale => true,
        :cmin => range === nothing ? nothing : range[1],
        :cmax => range === nothing ? nothing : range[2],
        :colorbar => Dict(
            :title => title,
            :x => offset,
            :ticksprefix => axis_ticks_prefix(colors_configuration.axis),
            :tickssuffix => axis_ticks_suffix(colors_configuration.axis),
        ),
    )
end

"""
    plotly_figure(trace::GenericTrace, layout::Layout)::PlotlyFigure
    plotly_figure(traces::AbstractVector{<:GenericTrace}, layout::Layout)::PlotlyFigure

Wrap a `trace` or a set of `traces` with the accompanying `layout` in a `PlotlyFigure`.
"""
function plotly_figure(trace::GenericTrace, layout::Layout)::PlotlyFigure  # UNTESTED
    purge_nulls!(trace.fields)
    purge_nulls!(layout.fields)
    return plot(trace, layout)  # NOJET
end

function plotly_figure(traces::AbstractVector{<:GenericTrace}, layout::Layout)::PlotlyFigure
    for trace in traces
        purge_nulls!(trace.fields)
    end
    purge_nulls!(layout.fields)
    return plot(traces, layout)  # NOJET
end

function purge_nulls!(dict::AbstractDict)::Nothing
    for (_, value) in dict
        if value isa AbstractDict
            purge_nulls!(value)
        end
    end
    filter!(dict) do pair
        return pair.second !== nothing &&  # NOJET
               !(pair.second isa AbstractDict && isempty(pair.second)) &&
               !(pair.second isa AbstractVector && (isempty(pair.second) || all(pair.second .=== nothing))) &&
               !(pair.second isa Tuple && all(pair.second .=== nothing))
    end
    return nothing
end

function axis_ticks_prefix(axis_configuration::AxisConfiguration)::Maybe{AbstractString}
    if !axis_configuration.show_ticks
        return nothing
    elseif axis_configuration.log_scale == Log10Scale
        return "<sub>10</sub>"
    elseif axis_configuration.log_scale == Log2Scale
        return "<sub>2</sub>"
    else
        @assert axis_configuration.log_scale === nothing
        return nothing
    end
end

function axis_ticks_suffix(axis_configuration::AxisConfiguration)::Maybe{AbstractString}
    if axis_configuration.percent
        return "<sub>%</sub>"
    else
        return nothing
    end
end

"""
    scale_axis_value(axis_configuration::AxisConfiguration, value::Real; clamp::Bool = true)::Real
    scale_axis_value(axis_configuration::AxisConfiguration, value::Nothing; clamp::Bool = true)::Nothing

Scale a single `value` according to the `axis_configuration`. This deals with log scales and percent scaling. By
default, `clamp` the values to a specified explicit range.
"""
function scale_axis_value(axis_configuration::AxisConfiguration, value::Real; clamp::Bool = true)::Real
    if clamp
        if axis_configuration.minimum !== nothing && value < axis_configuration.minimum
            value = axis_configuration.minimum  # UNTESTED
        end
        if axis_configuration.maximum !== nothing && value > axis_configuration.maximum  # NOJET
            value = axis_configuration.maximum  # UNTESTED
        end
    end

    if axis_configuration.percent
        scale = 100.0
    else
        scale = 1.0
    end

    offset = axis_configuration.log_regularization

    if axis_configuration.log_scale === Log10Scale
        return log10(value * scale + offset)
    elseif axis_configuration.log_scale === Log2Scale
        return log2(value * scale + offset)
    else
        @assert axis_configuration.log_scale === nothing
        return value * scale + offset
    end
end

function scale_axis_value(::AxisConfiguration, ::Nothing; clamp::Bool = true)::Nothing  # NOLINT
    return nothing
end

"""
    scale_axis_values(
        axis_configuration::AxisConfiguration,
        values::Maybe{AbstractVector{<:Maybe{Real}}};
        clamp::Bool = true
    )::Maybe{AbstractVector{<:Maybe{Real}}}

Scale a vector of `values` according to the `axis_configuration`. This deals with log scales and percent scaling By
default, `clamp` the values to a specified explicit range.
"""
function scale_axis_values(
    axis_configuration::AxisConfiguration,
    values::Maybe{AbstractVector{<:Maybe{Real}}};
    clamp::Bool = true,
)::Maybe{AbstractVector{<:Maybe{Real}}}
    if values === nothing
        return nothing  # UNTESTED
    elseif !axis_configuration.percent &&
           axis_configuration.log_scale === nothing &&
           axis_configuration.minimum === nothing &&
           axis_configuration.maximum === nothing
        return values
    else
        return [scale_axis_value(axis_configuration, value; clamp) for value in values]
    end
end

"""
    final_scaled_range(
        implicit_scaled_range::AbstractVector{<:Maybe{Real}},
        axis_configuration::AxisConfiguration
    )::AbstractVector{<:Real}

Compute the final range for some axis given the `implicit_scaled_range` computed from the values and the `axis_configuration`.
"""
function final_scaled_range(
    implicit_scaled_range::AbstractVector{<:Maybe{Real}},
    axis_configuration::AxisConfiguration,
)::AbstractVector{<:Real}
    explicit_scaled_range =
        scale_axis_values(axis_configuration, [axis_configuration.minimum, axis_configuration.maximum]; clamp = false)

    scaled_range = Real[  # NOJET
        prefer_data(explicit_scaled, implicit_scaled) for
        (explicit_scaled, implicit_scaled) in zip(explicit_scaled_range, implicit_scaled_range)
    ]

    return scaled_range
end

"""
    scale_size_values(
        axis_configuration::AxisConfiguration,
        size_configuration::SizeConfiguration,
        values::Maybe{AbstractVector{<:Real}},
    )::Maybe{AbstractVector{<:Real}}

Scale a vector of `values` according to the `axis_configuration` and `size_configuration`.
"""
function scale_size_values(
    axis_configuration::AxisConfiguration,
    size_configuration::SizeConfiguration,
    values::Maybe{AbstractVector{<:Real}},
)::Maybe{AbstractVector{<:Real}}
    if values === nothing
        return nothing

    else
        scaled_values = scale_axis_values(axis_configuration, values)
        implicit_values_range = range_of(scaled_values)
        minimum_scaled_value, maximum_scaled_value = final_scaled_range(implicit_values_range, axis_configuration)

        scaled_values_range = maximum_scaled_value - minimum_scaled_value
        if scaled_values_range == 0
            scaled_values_range = 1
        end

        return (scaled_values .- minimum_scaled_value) .* (size_configuration.span / scaled_values_range) .+
               size_configuration.smallest
    end
end

function line_dash(style::LineStyle)::Maybe{AbstractString}
    if style == SolidLine
        return nothing
    elseif style == DashLine
        return "dash"
    elseif style == DotLine
        return "dot"
    elseif style == DashDotLine
        return "dashdot"
    else
        @assert false
    end
end

function line_dash(::Nothing)::Nothing  # UNTESTED
    return nothing
end

"""
    push_vertical_bands_shapes(
        shapes::AbstractVector{Shape},
        axis_configuration::AxisConfiguration,
        scaled_values_range::AbstractVector{<:Real},
        bands_data::BandsData,
        bands_configuration::BandsConfiguration
    )::AbstractVector{<:Shape}

Push shapes for plotting vertical bands. These shapes need to be places in the layout and not the traces because Plotly.
"""
function push_vertical_bands_shapes(
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    scaled_values_range::AbstractVector{<:Real},
    bands_data::BandsData,
    bands_configuration::BandsConfiguration,
)::Nothing
    scaled_low_offset =
        scale_axis_value(axis_configuration, prefer_data(bands_data.low_offset, bands_configuration.low.offset))
    scaled_middle_offset =
        scale_axis_value(axis_configuration, prefer_data(bands_data.middle_offset, bands_configuration.middle.offset))
    scaled_high_offset =
        scale_axis_value(axis_configuration, prefer_data(bands_data.high_offset, bands_configuration.high.offset))

    for (band_configuration, scaled_offset) in (
        (bands_configuration.low, scaled_low_offset),
        (bands_configuration.middle, scaled_middle_offset),
        (bands_configuration.high, scaled_high_offset),
    )
        if scaled_offset !== nothing && band_configuration.line.style !== nothing
            push!(
                shapes,
                Shape(
                    "line";
                    line_color = band_configuration.line.color,
                    line_dash = line_dash(band_configuration.line.style),
                    x0 = scaled_offset,
                    x1 = scaled_offset,
                    xref = "x",
                    y0 = 0,
                    y1 = 1,
                    yref = "y domain",
                ),
            )
        end
    end

    if scaled_low_offset !== nothing && bands_configuration.low.line.is_filled
        push!(
            shapes,
            Shape(
                "rect";
                fillcolor = fill_color(bands_configuration.low.line.color),
                line_width = 0,
                layer = "below",
                x0 = scaled_values_range[1],
                x1 = scaled_low_offset,
                xref = "x",
                y0 = 0,
                y1 = 1,
                yref = "y domain",
            ),
        )
    end

    if scaled_low_offset !== nothing && scaled_high_offset !== nothing && bands_configuration.middle.line.is_filled
        push!(
            shapes,
            Shape(
                "rect";
                layer = "below",
                fillcolor = fill_color(bands_configuration.middle.line.color),
                line_width = 0,
                x0 = scaled_low_offset,
                x1 = scaled_high_offset,
                xref = "x",
                y0 = 0,
                y1 = 1,
                yref = "y domain",
            ),
        )
    end

    if scaled_high_offset !== nothing && bands_configuration.high.line.is_filled
        push!(
            shapes,
            Shape(
                "rect";
                layer = "below",
                fillcolor = fill_color(bands_configuration.high.line.color),
                line_width = 0,
                x0 = scaled_high_offset,
                x1 = scaled_values_range[2],
                xref = "x",
                y0 = 0,
                y1 = 1,
                yref = "y domain",
            ),
        )
    end

    return nothing
end

"""
    push_horizontal_bands_shapes(
        shapes::AbstractVector{Shape},
        axis_configuration::AxisConfiguration,
        scaled_values_range::AbstractVector{<:Real},
        bands_data::BandsData,
        bands_configuration::BandsConfiguration
    )::AbstractVector{<:Shape}

Push shapes for plotting horizontal bands. These shapes need to be placed in the layout and not the traces because
Plotly.
"""
function push_horizontal_bands_shapes(
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    scaled_values_range::AbstractVector{<:Real},
    bands_data::BandsData,
    bands_configuration::BandsConfiguration,
)::Nothing
    scaled_low_offset =
        scale_axis_value(axis_configuration, prefer_data(bands_data.low_offset, bands_configuration.low.offset))
    scaled_middle_offset =
        scale_axis_value(axis_configuration, prefer_data(bands_data.middle_offset, bands_configuration.middle.offset))
    scaled_high_offset =
        scale_axis_value(axis_configuration, prefer_data(bands_data.high_offset, bands_configuration.high.offset))

    for (band_configuration, scaled_offset) in (
        (bands_configuration.low, scaled_low_offset),
        (bands_configuration.middle, scaled_middle_offset),
        (bands_configuration.high, scaled_high_offset),
    )
        if scaled_offset !== nothing && band_configuration.line.style !== nothing
            push!(
                shapes,
                Shape(
                    "line";
                    line_color = band_configuration.line.color,
                    line_dash = line_dash(band_configuration.line.style),
                    y0 = scaled_offset,
                    y1 = scaled_offset,
                    yref = "y",
                    x0 = 0,
                    x1 = 1,
                    xref = "x domain",
                ),
            )
        end
    end

    if scaled_low_offset !== nothing && bands_configuration.low.line.is_filled
        push!(
            shapes,
            Shape(
                "rect";
                fillcolor = fill_color(bands_configuration.low.line.color),
                line_width = 0,
                layer = "below",
                y0 = scaled_values_range[1],
                y1 = scaled_low_offset,
                yref = "y",
                x0 = 0,
                x1 = 1,
                xref = "x domain",
            ),
        )
    end

    if scaled_low_offset !== nothing && scaled_high_offset !== nothing && bands_configuration.middle.line.is_filled
        push!(
            shapes,
            Shape(
                "rect";
                layer = "below",
                fillcolor = fill_color(bands_configuration.middle.line.color),
                line_width = 0,
                y0 = scaled_low_offset,
                y1 = scaled_high_offset,
                yref = "y",
                x0 = 0,
                x1 = 1,
                xref = "x domain",
            ),
        )
    end

    if scaled_high_offset !== nothing && bands_configuration.high.line.is_filled
        push!(
            shapes,
            Shape(
                "rect";
                layer = "below",
                fillcolor = fill_color(bands_configuration.high.line.color),
                line_width = 0,
                y0 = scaled_high_offset,
                y1 = scaled_values_range[2],
                yref = "y",
                x0 = 0,
                x1 = 1,
                xref = "x domain",
            ),
        )
    end

    return nothing
end

@enum Side Left BottomLeft Bottom BottomRight Right TopRight Top TopLeft

struct BandPoint
    x::Real
    y::Real
    side::Side
end

"""
    push_diagonal_bands_shapes(
        shapes::AbstractVector{Shape},
        x_axis_configuration::AxisConfiguration,
        y_axis_configuration::AxisConfiguration,
        x_scaled_values_range::AbstractVector{<:Real},
        y_scaled_values_range::AbstractVector{<:Real},
        bands_data::BandsData,
        bands_configuration::BandsConfiguration
    )::AbstractVector{<:Shape}

Push shapes for plotting diagonal bands. These shapes need to be placed in the layout and not the traces because Plotly.
"""
function push_diagonal_bands_shapes(  # UNTESTED
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
    bands_data::BandsData,
    bands_configuration::BandsConfiguration,
)::Nothing
    low_band_points = push_diagonal_bands_line(
        shapes,
        axis_configuration,
        x_scaled_values_range,
        y_scaled_values_range,
        bands_data.low_offset,
        bands_configuration.low,
    )
    middle_band_points = push_diagonal_bands_line(
        shapes,
        axis_configuration,
        x_scaled_values_range,
        y_scaled_values_range,
        bands_data.middle_offset,
        bands_configuration.middle,
    )
    high_band_points = push_diagonal_bands_line(
        shapes,
        axis_configuration,
        x_scaled_values_range,
        y_scaled_values_range,
        bands_data.high_offset,
        bands_configuration.high,
    )

    if low_band_points !== nothing && bands_configuration.low.line.is_filled
        push_diagonal_bands_low_fill(
            shapes,
            x_scaled_values_range,
            y_scaled_values_range,
            low_band_points,
            bands_configuration.low,
        )
    end

    if low_band_points !== nothing && high_band_points !== nothing && bands_configuration.middle.line.is_filled
        push_diagonal_bands_middle_fill(
            shapes,
            x_scaled_values_range,
            y_scaled_values_range,
            low_band_points,
            middle_band_points,
            bands_configuration.middle,
        )
    end

    if high_band_points !== nothing && bands_configuration.high.line.is_filled
        push_diagonal_bands_high_fill(
            shapes,
            x_scaled_values_range,
            y_scaled_values_range,
            high_band_points,
            bands_configuration.high,
        )
    end
end

function push_diagonal_bands_line(  # UNTESTED
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
    data_offset::Maybe{Real},
    band_configuration::BandConfiguration,
)::Maybe{Tuple{BandPoint, BandPoint}}
    offset = prefer_data(data_offset, band_configuration.offset)
    start_point = start_band_point(axis_configuration, x_scaled_values_range, y_scaled_values_range, offset)
    end_point = end_band_point(axis_configuration, x_scaled_values_range, y_scaled_values_range, offset)
    @assert (start_point === nothing) == (end_point === nothing)
    if start_point === nothing
        return nothing
    end

    push!(
        shapes,
        Shape(
            "line";
            line_color = band_configuration.line.color,
            line_dash = line_dash(band_configuration.line.style),
            y0 = start_point.y,
            y1 = end_point.y,
            yref = "y",
            x0 = start_point.x,
            x1 = end_point.x,
            xref = "x",
        ),
    )

    return (start_point, end_point)
end

function start_band_point(::AxisConfiguration, ::AbstractVector{<:Real}, ::AbstractVector{<:Real}, ::Nothing)::Nothing  # UNTESTED
    return nothing
end

function start_band_point(  # UNTESTED
    axis_configuration::AxisConfiguration,
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
    offset::Real,
)::Maybe{BandPoint}
    scaled_offset = scale_axis_value(axis_configuration, offset)
    bottom = [y_scaled_values_range[1] - scaled_offset, y_scaled_values_range[1]]
    left = [x_scaled_values_range[1], x_scaled_values_range[1] + scaled_offset]

    if is_in_bounds(x_scaled_values_range, y_scaled_values_range, bottom)
        if is_in_bounds(x_scaled_values_range, y_scaled_values_range, left)
            @assert isapprox(bottom, left)  # NOJET
            return (bottom[1], bottom[2], BottomLeft)
        else
            return (bottom[1], bottom[2], Bottom)
        end
    elseif is_in_bounds(x_scaled_values_range, y_scaled_values_range, left)
        return BandPoint(left[1], left[2], Left)
    else
        return nothing
    end
end

function end_band_point(::AxisConfiguration, ::AbstractVector{<:Real}, ::AbstractVector{<:Real}, ::Nothing)::Nothing
    return nothing
end

function end_band_point(
    axis_configuration::AxisConfiguration,
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
    offset::Real,
)::Maybe{BandPoint}
    scaled_offset = scale_axis_value(axis_configuration, offset)
    top = [y_scaled_values_range[2] - scaled_offset, y_scaled_values_range[2]]
    right = [x_scaled_values_range[2], x_scaled_values_range[2] + scaled_offset]

    if is_in_bounds(x_scaled_values_range, y_scaled_values_range, top)
        if is_in_bounds(x_scaled_values_range, y_scaled_values_range, right)
            @assert isapprox(top, right)
            return (top[1], top[2], TopRight)
        else
            return (top[1], top[2], Top)
        end
    elseif is_in_bounds(x_scaled_values_range, y_scaled_values_range, right)
        return BandPoint(right[1], right[2], Right)
    else
        return nothing
    end
end

function is_in_bounds(  # UNTESTED
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
    point::AbstractVector{<:Real},
)::Bool
    return (x_scaled_values_range[1] <= point[1] <= x_scaled_values_range[2]) &&
           (y_scaled_values_range[1] <= point[2] <= y_scaled_values_range[2])
end

function push_diagonal_bands_low_fill(  # UNTESTED
    shapes::AbstractVector{Shape},
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
    band_points::Tuple{BandPoint, BandPoint},
    band_configuration::BandConfiguration,
)::Nothing
    path_parts = AbstractString[]

    to_bottom_right(path_parts, x_scaled_values_range, y_scaled_values_range)
    if band_points[1].side == Left
        to_bottom_left(path_parts, x_scaled_values_range, y_scaled_values_range)
    end
    to_start_point(path_parts, band_points)
    to_end_point(path_parts, band_points)
    if band_points[2].side == Top
        to_top_right(path_parts, x_scaled_values_range, y_scaled_values_range)
    end

    push_fill_path(shapes, path_parts, band_configuration)
    return nothing
end

function push_diagonal_bands_high_fill(  # UNTESTED
    shapes::AbstractVector{Shape},
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
    band_points::Tuple{BandPoint, BandPoint},
    band_configuration::BandConfiguration,
)::Nothing
    path_parts = AbstractString[]

    to_top_left(path_parts, x_scaled_values_range, y_scaled_values_range)
    if band_points[1].side == Bottom
        to_bottom_left(path_parts, x_scaled_values_range, y_scaled_values_range)
    end
    to_start_point(path_parts, band_points)
    to_end_point(path_parts, band_points)
    if band_points[2].side == Right
        to_top_right(path_parts, x_scaled_values_range, y_scaled_values_range)
    end

    push_fill_path(shapes, path_parts, band_configuration)
    return nothing
end

function push_diagonal_bands_middle_fill(  # UNTESTED
    shapes::AbstractVector{Shape},
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
    low_band_points::Tuple{BandPoint, BandPoint},
    high_band_points::Tuple{BandPoint, BandPoint},
    band_configuration::BandConfiguration,
)::Nothing
    path_parts = AbstractString[]

    to_start_point(path_parts, high_band_points)
    to_end_point(path_parts, high_band_points)
    if high_band_points[2].side == Top && low_band_points[2].side == Right
        to_top_right(path_parts, x_scaled_values_range, y_scaled_values_range)
    end
    to_end_point(path_parts, low_band_points)
    to_start_point(path_parts, low_band_points)
    if high_band_points[1].side == Left && low_band_points[1].side == Bottom
        to_bottom_left(path_parts, x_scaled_values_range, y_scaled_values_range)
    end

    push_fill_path(shapes, path_parts, band_configuration)
    return nothing
end

function to_start_point(path_parts::AbstractVector{<:AbstractString}, band_points::Tuple{BandPoint, BandPoint})::Nothing  # UNTESTED
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(band_points[1].x))
    push!(path_parts, string(band_points[1].y))
    return nothing
end

function to_end_point(path_parts::AbstractVector{<:AbstractString}, band_points::Tuple{BandPoint, BandPoint})::Nothing
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(band_points[2].x))
    push!(path_parts, string(band_points[2].y))
    return nothing
end

function to_bottom_left(  # UNTESTED
    path_parts::AbstractVector{<:AbstractString},
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
)::Nothing
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(x_scaled_values_range[1]))
    push!(path_parts, string(y_scaled_values_range[1]))
    return nothing
end

function to_bottom_right(  # UNTESTED
    path_parts::AbstractVector{<:AbstractString},
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
)::Nothing
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(x_scaled_values_range[2]))
    push!(path_parts, string(y_scaled_values_range[1]))
    return nothing
end

function to_top_left(  # UNTESTED
    path_parts::AbstractVector{<:AbstractString},
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
)::Nothing
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(x_scaled_values_range[1]))
    push!(path_parts, string(y_scaled_values_range[2]))
    return nothing
end

function to_top_right(  # UNTESTED
    path_parts::AbstractVector{<:AbstractString},
    x_scaled_values_range::AbstractVector{<:Real},
    y_scaled_values_range::AbstractVector{<:Real},
)::Nothing
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(x_scaled_values_range[2]))
    push!(path_parts, string(y_scaled_values_range[2]))
    return nothing
end

function push_fill_path(  # UNTESTED
    shapes::AbstractVector{Shape},
    path_parts::AbstractVector{<:AbstractString},
    band_configuration::BandConfiguration,
)::Nothing
    push!(path_parts, "Z")

    push!(
        shapes,
        Shape(
            "path";
            layer = "below",
            fillcolor = fill_color(band_configuration.line.color),
            path = join(path_parts, " "),
            line_width = 0,
            yref = "y",
            xref = "x",
        ),
    )

    return nothing
end

"""
    fill_color(line_color::Maybe{AbstractString})::Maybe{AbstractString}

Return a fill color based on a `line_color`. The fill color is twice as transparent as the line color.
"""
function fill_color(::Nothing)::Nothing
    return nothing
end

function fill_color(line_color::AbstractString)::AbstractString
    rgba = parse(RGBA, line_color)
    return hex(RGBA(rgba.r, rgba.g, rgba.b, rgba.alpha * 0.5), :RRGGBBAA)
end

"""
    prefer_data(data_value::Any, configuration_value::Any)::Any
    prefer_data(data_values::AbstractVector, index::Integer, configuration_value::Any)::Any

Return a value to use, prefering the data value (which may be in a vector) to the configuration value.
"""
function prefer_data(data_value::Any, configuration_value::Any)::Any
    if data_value === nothing
        return configuration_value
    else
        return data_value
    end
end

function prefer_data(data_values::Maybe{AbstractVector}, index::Integer, configuration_value::Any)::Any
    if data_values !== nothing
        return data_values[index]  # UNTESTED
    else
        return configuration_value
    end
end

"""
    expand_range!(range::Maybe{AbstractVector{<:Maybe{Real}}})::Nothing

Expand the range of values by 1% to allow for points and lines at the edge to be fully visible.
"""
function expand_range!(range::Maybe{AbstractVector{<:Maybe{Real}}})::Nothing
    low, high = range  # NOJET
    @assert low !== nothing
    @assert high !== nothing
    margins = (high - low) / 100
    range[1] = low - margins
    range[2] = high + margins
    return nothing
end

"""
    range_of(
        values::AbstractVector{<:Maybe{Real}},
        range::Maybe{AbstractVector{<:Maybe{Real}}} = nothing,
    )::AbstractVector{<:Maybe{Real}}

Compute the range of (scaled) values. If a range is give, it is updated in-place as well.
"""
function range_of(
    values::Maybe{AbstractVector{<:Maybe{Real}}},
    range::Maybe{AbstractVector{<:Maybe{Real}}} = nothing,
)::AbstractVector{<:Maybe{Real}}
    if range === nothing
        minimum_value = nothing
        maximum_value = nothing
    else
        minimum_value, maximum_value = range
    end

    if values !== nothing
        for value in values
            if minimum_value === nothing || (value !== nothing && value < minimum_value)
                minimum_value = value
            end
            if maximum_value === nothing || (value !== nothing && value > maximum_value)
                maximum_value = value
            end
        end
    end

    if range === nothing
        range = [minimum_value, maximum_value]
    else
        range .= [minimum_value, maximum_value]
    end

    return range
end

end  # module
