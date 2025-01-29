"""
Utility functions for defining graph types. We do not re-export all symbols from this sub-module to the global
`MCGraphs` namespace.
"""
module Utilities

export axis_ticksformat
export graph_layout
export horizontal_bands_shapes
export plotly_figure
export scale_axis_value
export scale_axis_values
export vertical_bands_shapes

using Colors
using PlotlyJS
using Reexport

using ..Validations
using ..Common

@reexport import .Common.validate_graph
@reexport import .Common.graph_to_figure
@reexport import .Validations.Maybe

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

function purge_nulls!(dict::T)::T where {T <: AbstractDict}
    for (_, value) in dict
        if value isa AbstractDict
            purge_nulls!(value)
        end
    end
    filter!(dict) do pair
        return pair.second !== nothing && !(pair.second isa AbstractDict && isempty(pair.second))
    end
    return dict
end

"""
    graph_layout(figure_configuration::FigureConfiguration, layout::Layout)::Layout

Patch a plotly `layout` with a `figure_configuration`.
"""
function graph_layout(figure_configuration::FigureConfiguration, layout::Layout)::Layout
    layout["margin"] = Dict(
        :l => figure_configuration.margins.left,
        :r => figure_configuration.margins.right,
        :t => figure_configuration.margins.top,
        :b => figure_configuration.margins.bottom,
    )
    if figure_configuration.template !== nothing
        layout["template"] = figure_configuration.template
    end
    if figure_configuration.width !== nothing
        layout["width"] = figure_configuration.width  # UNTESTED
    end
    if figure_configuration.height !== nothing
        layout["height"] = figure_configuration.height  # UNTESTED
    end
    return layout
end

"""
    axis_ticksformat(axis_configuration::AxisConfiguration)::Tuple{Maybe{AbstractString}, Maybe{AbstractString}}

Given the `axis_configuration`, return a prefix and a suffix for the ticks along that axis. This deals with log scales
and percent scaling.
"""
function axis_ticksformat(axis_configuration::AxisConfiguration)::Tuple{Maybe{AbstractString}, Maybe{AbstractString}}
    if axis_configuration.log_scale == Log10Scale
        tickprefix = "<sub>10</sub>"
    elseif axis_configuration.log_scale == Log2Scale
        tickprefix = "<sub>2</sub>"
    else
        @assert axis_configuration.log_scale === nothing
        tickprefix = nothing
    end

    if axis_configuration.percent
        ticksuffix = "<sub>%</sub>"
    else
        ticksuffix = nothing
    end

    return (tickprefix, ticksuffix)
end

"""
    scale_axis_value(axis_configuration::AxisConfiguration, value::Maybe{Real})::Maybe{Real}

Scale a single `value` according to the `axis_configuration`. This deals with log scales and percent scaling.
"""
function scale_axis_value(axis_configuration::AxisConfiguration, value::Maybe{Real})::Maybe{Real}
    if value === nothing
        return nothing
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

"""
    scale_axis_values(
        axis_configuration::AxisConfiguration,
        values::Union{AbstractVector{<:Real},AbstractVector{<:Maybe{Real}}},
    )::Union{AbstractVector{<:Real},AbstractVector{<:Maybe{Real}}}

Scale a vector of `values` according to the `axis_configuration`. This deals with log scales and percent scaling.
"""
function scale_axis_values(
    axis_configuration::AxisConfiguration,
    values::Union{AbstractVector{<:Real}, AbstractVector{<:Maybe{Real}}},
)::Union{AbstractVector{<:Real}, AbstractVector{<:Maybe{Real}}}
    if !axis_configuration.percent && axis_configuration.log_scale === nothing
        return values
    else
        return [scale_axis_value(axis_configuration, value) for value in values]
    end
end

"""
    vertical_bands_shapes(
        axis_configuration::AxisConfiguration,
        scaled_values_range::AbstractVector{<:Real},
        bands_data::BandsData,
        bands_configuration::BandsConfiguration
    )::AbstractVector{<:Shape}

Return a vector of shapes for plotting vertical bands. These shapes need to be places in the layout and not the traces
because Plotly.
"""
function vertical_bands_shapes(
    axis_configuration::AxisConfiguration,
    scaled_values_range::AbstractVector{<:Real},
    bands_data::BandsData,
    bands_configuration::BandsConfiguration,
)::AbstractVector{<:Shape}
    shapes = Vector{Shape}()

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
                    line_dash = band_configuration.line.style == DashedLine ? "dash" : nothing,
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

    return shapes
end

"""
    horizontal_bands_shapes(
        axis_configuration::AxisConfiguration,
        scaled_values_range::AbstractVector{<:Real},
        bands_data::BandsData,
        bands_configuration::BandsConfiguration
    )::AbstractVector{<:Shape}

Return a vector of shapes for plotting horizontal bands. These shapes need to be places in the layout and not the traces
because Plotly.
"""
function horizontal_bands_shapes(
    axis_configuration::AxisConfiguration,
    scaled_values_range::AbstractVector{<:Real},
    bands_data::BandsData,
    bands_configuration::BandsConfiguration,
)::AbstractVector{<:Shape}
    shapes = Vector{Shape}()

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
                    line_dash = band_configuration.line.style == DashedLine ? "dash" : nothing,
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

    return shapes
end

function fill_color(::Nothing)::Nothing  # UNTESTED
    return nothing
end

function fill_color(line_color::AbstractString)::AbstractString
    rgba = parse(RGBA, line_color)
    return hex(RGBA(rgba.r, rgba.g, rgba.b, rgba.alpha * 0.5), :RRGGBBAA)
end

function prefer_data(data_value::Any, configuration_value::Any)::Any
    if data_value !== nothing
        return data_value
    else
        return configuration_value
    end
end

end  # module
