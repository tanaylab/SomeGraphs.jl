"""
Utility functions for defining graph types. We do not re-export all symbols from this sub-module to the global
`MCGraphs` namespace. You can safely ignore these unless you are implementing a new graph type.
"""
module Utilities

export axis_ticks_prefix
export axis_ticks_suffix
export collect_range!
export configured_colors
export ConfiguredColors
export fill_color
export final_scaled_range
export MaybeRange
export plotly_axis
export plotly_figure
export plotly_layout
export plotly_line_dash
export plotly_sub_graph_axes
export plotly_sub_graph_domain
export prefer_data
export push_diagonal_bands_shapes
export push_horizontal_bands_shapes
export push_vertical_bands_shapes
export Range
export scale_axis_value
export scale_axis_values
export scale_size_values
export set_layout_axis!
export set_layout_colorscale!
export SubGraph
export validate_axis_sizes
export validate_colors
export validate_graph_bands
export validate_values

using Colors
using NamedArrays
using PlotlyJS
using Reexport

using ..Validations
using ..Common

import .Common.CACHED_COLOR_SCALES
import .Validations.Maybe

@reexport import .Common.validate_graph
@reexport import .Common.graph_to_figure

@kwdef mutable struct ScaledPoint
    x::Real
    y::Real
end

"""
    @kwdef mutable struct MaybeRange
        minimum::Maybe{Float32} = nothing
        maximum::Maybe{Float32} = nothing
    end

A range of values (possibly partially specified).
"""
@kwdef mutable struct MaybeRange
    minimum::Maybe{Float32} = nothing
    maximum::Maybe{Float32} = nothing
end

"""
    @kwdef mutable struct Range
        minimum::Float32
        maximum::Float32
    end

A range of values (fully specified).
"""
@kwdef mutable struct Range
    minimum::Float32
    maximum::Float32
end

"""
    validate_graph_bands(
        field::AbstractString,
        bands_configuration::BandsConfiguration,
        bands_data::BandsData,
        axis_configuration::Maybe{AxisConfiguration} = nothing,
    )::Nothing

Validate that the bands configuration and data is compatible. Assumes these are specified as the same `field` in both
the graph's data and configuration.
"""
function validate_graph_bands(
    field::AbstractString,
    bands_configuration::BandsConfiguration,
    bands_data::BandsData,
    axis_configuration::Maybe{AxisConfiguration} = nothing,
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

    context = ValidationContext(["graph.configuration", field])

    validate_is_range(context, "low_offset", bands_data.low_offset, "middle_offset", bands_data.middle_offset)

    validate_is_range(context, "middle_offset", bands_data.middle_offset, "high_offset", bands_data.high_offset)

    validate_is_range(context, "low_offset", bands_data.low_offset, "high_offset", bands_data.high_offset)

    if axis_configuration !== nothing && axis_configuration.log_scale !== nothing
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
    )::Nothing
    validate_colors(
        colors_data_context::ValidationContext,
        colors_data::AbstractMatrix{<:Real},
        colors_configuration_context::ValidationContext,
        colors_configuration::ColorsConfiguration,
        mask::Maybe{Union{AbstractVector{Bool},BitVector}} = nothing,
    )::Nothing

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
)::Nothing
    if colors_configuration.fixed isa AbstractString
        if colors_data !== nothing
            throw(
                ArgumentError(
                    "can't specify $(location(colors_data_context))\n" *
                    "for $(location(colors_configuration_context)).fixed: $(colors_configuration.fixed)",
                ),
            )
        end

        return nothing
    end

    if colors_configuration.show_legend
        if colors_data === nothing
            throw(
                ArgumentError(
                    "must specify $(location(colors_data_context))\n" *
                    "for $(location(colors_configuration_context)).show_legend",
                ),
            )
        elseif eltype(colors_data) <: AbstractString && colors_configuration.palette === nothing
            throw(
                ArgumentError(
                    "can't specify $(location(colors_configuration_context)).show_legend\n" *
                    "for named $(location(colors_data_context))",
                ),
            )
        end
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
            palette_dict = colors_configuration.palette
            if palette_dict isa NamedVector
                palette_dict = palette_dict.dicts[1]  # UNTESTED
            end

            validate_vector_entries(colors_data_context, colors_data, mask) do _, color  # NOJET
                if !haskey(palette_dict, color)
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
        @assert colors_configuration.palette isa AbstractString
        lock(COLOR_SCALES_LOCK) do
            @assert haskey(CACHED_COLOR_SCALES, colors_configuration.palette)
        end
    end

    return nothing
end

function validate_colors(
    colors_data_context::ValidationContext,
    colors_data::AbstractMatrix{<:Real},
    colors_configuration_context::ValidationContext,
    colors_configuration::ColorsConfiguration,
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
)::Nothing
    @assert mask === nothing
    if colors_configuration.axis.minimum !== nothing ||
       colors_configuration.axis.maximum !== nothing ||
       colors_configuration.axis.log_scale !== nothing ||
       colors_configuration.axis.percent
        if colors_configuration.axis.log_scale !== nothing
            validate_matrix_entries(colors_data_context, "entries_colors", colors_data) do _, _, color
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

    if colors_configuration.palette isa AbstractString
        lock(COLOR_SCALES_LOCK) do                                                                                                                                                  # UNTESTED
            @assert haskey(CACHED_COLOR_SCALES, colors_configuration.palette)
        end
    end

    return nothing
end

"""
    validate_values(
        values_data_context::ValidationContext,
        values_data::Maybe{AbstractVector{<:Real}}},
        axis_configuration_context::ValidationContext,
        axis_configuration::AxisConfiguration,
    )::Nothing

Validate that the `values_data` from the `values_data_context` is valid and consistent with the `axis_configuration`
from the `axis_configuration_context`. Specifically this ensures that if a `log_scale` is applied, all the values
are positive.
"""
function validate_values(
    values_data_context::ValidationContext,
    values_data::Maybe{AbstractVector{<:Real}},
    axis_configuration_context::ValidationContext,
    axis_configuration::AxisConfiguration,
)::Nothing
    if values_data !== nothing && axis_configuration.log_scale !== nothing
        for (index, value) in enumerate(values_data)
            validate_in(
                values_data_context,
                "([$(index)] + $(location(axis_configuration_context)).axis.log_regularization)",
            ) do
                return validate_is_above(values_data_context, value + axis_configuration.log_regularization, 0)
            end
        end
    end

    return nothing
end

"""
    plotly_layout(
        figure_configuration::FigureConfiguration;
        title::Maybe{AbstractString},
        has_legend::Bool,
        shapes::AbstractVector{Shape},
    )::Layout

Create a Plotly `Layout` object.
"""
function plotly_layout(
    figure_configuration::FigureConfiguration;
    title::Maybe{AbstractString},
    has_legend::Bool,
    has_hovers::Bool = false,
    shapes::Maybe{AbstractVector{Shape}} = nothing,
)::Layout
    return Layout(;  # NOJET
        title,
        showlegend = has_legend,
        legend_itemdoubleclick = has_legend ? false : nothing,
        legend_tracegroupgap = 0,
        hoverlabel_align = has_hovers ? "left" : nothing,
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
        range::Range,
        domain::Maybe{AbstractVector{<:Real}} = nothing,
    )::Nothing

Add a Plotly `axis` in a `layout` using the `axis_configuration`.
"""
function set_layout_axis!(
    layout::Layout,
    axis::AbstractString,
    axis_configuration::AxisConfiguration;
    title::Maybe{AbstractString} = nothing,
    range::Maybe{Range} = nothing,
    ticks_labels::Maybe{AbstractVector{<:AbstractString}} = nothing,
    ticks_values::Maybe{AbstractVector{<:Real}} = nothing,
    domain::Maybe{AbstractVector{<:Real}} = nothing,
    is_tick_axis::Bool = true,
    is_zeroable::Bool = true,
)::Nothing
    show_ticks = is_tick_axis && axis_configuration.show_ticks
    layout[axis] = Dict(
        :title => title,
        :range => range === nothing ? nothing : [range.minimum, range.maximum],
        :showgrid => axis_configuration.show_grid,
        :gridcolor => axis_configuration.show_grid ? axis_configuration.grid_color : nothing,
        :showticklabels => show_ticks,
        :tickprefix => show_ticks ? axis_ticks_prefix(axis_configuration) : nothing,
        :ticksuffix => show_ticks ? axis_ticks_suffix(axis_configuration) : nothing,
        :tickvals => show_ticks ? ticks_values : nothing,
        :ticktext => show_ticks ? ticks_labels : nothing,
        :zeroline => is_zeroable ? axis_configuration.log_scale === nothing : nothing,
        :domain => domain,
    )
    return nothing
end

"""
    plotly_axis(prefix::AbstractString, index::Maybe{Integer}; short::Bool = false, force::Bool = false)::Maybe{AbstractString}

Return the Plotly axis name for a given index. If `short` just use the `prefix`, otherwise add `axis`. If `force` give a
result even for the 1st (typically implicit, unnamed) axis.
"""
function plotly_axis(
    prefix::AbstractString,
    index::Integer;
    short::Bool = false,
    force::Bool = false,
)::Maybe{AbstractString}
    if index == 1
        if short
            if force
                return prefix
            else
                return nothing
            end
        else
            return "$(prefix)axis"
        end
    else
        if short
            return "$(prefix)$(index)"
        else
            return "$(prefix)axis$(index)"
        end
    end
end

function plotly_axis(prefix::AbstractString, ::Nothing; short::Bool = false, force::Bool = false)::Maybe{AbstractString}
    if force
        return plotly_axis(prefix, 1; short, force)
    else
        return nothing
    end
end

"""
    set_layout_colorscale!(;
        layout::Layout,
        colors_scale_index::Integer,
        colors_configuration::ColorsConfiguration,
        scaled_colors_palette::Maybe{AbstractVector{<:Tuple{Real, AbstractString}}},
        offset::Maybe{Real},
        range::Maybe{Range} = nothing,
        title::Maybe{AbstractString},
        show_scale::Bool,
    )::Nothing

Set a `colorscale` in a Plotly `layout`, as specified by a `colors_configuration`. Since Plotly is dumb when it comes to
placement of color scales, the `offset` must be specified manually to avoid overlaps.
"""
function set_layout_colorscale!(;
    layout::Layout,
    colors_scale_index::Integer,
    colors_configuration::ColorsConfiguration,
    scaled_colors_palette::Maybe{AbstractVector{<:Tuple{Real, AbstractString}}},
    range::Maybe{Range} = nothing,
    title::Maybe{AbstractString},
    show_scale::Bool,
    next_colors_scale_offset_index::AbstractVector{<:Integer},
    colors_scale_offsets::AbstractVector{<:Real},
)::Nothing
    if colors_configuration.palette isa CategoricalColors
        @assert false

    elseif colors_configuration.palette isa AbstractString
        @assert scaled_colors_palette === nothing
        colorscale = lock(COLOR_SCALES_LOCK) do
            return CACHED_COLOR_SCALES[colors_configuration.palette]
        end

    elseif colors_configuration.palette isa ContinuousColors
        @assert scaled_colors_palette !== nothing
        colorscale = scaled_colors_palette

    else
        @assert colors_configuration.palette === nothing
        @assert scaled_colors_palette === nothing
        colorscale = nothing
    end

    layout[plotly_axis("color", colors_scale_index)] = Dict(
        :showscale => show_scale,
        :colorscale => colorscale,
        :cmin => range === nothing ? nothing : range.minimum,
        :cmax => range === nothing ? nothing : range.maximum,
        :colorbar => if !show_scale
            nothing
        else
            Dict(
                :title => Dict(:text => title),
                :x => if next_colors_scale_offset_index[1] == 0
                    nothing
                else
                    colors_scale_offsets[next_colors_scale_offset_index[1]]
                end,
                :ticksprefix => axis_ticks_prefix(colors_configuration.axis),
                :tickssuffix => axis_ticks_suffix(colors_configuration.axis),
            )
        end,
    )

    if show_scale
        next_colors_scale_offset_index[1] += 1
    end

    return nothing
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
        if value isa AbstractVector
            for nested in value
                if nested isa AbstractDict
                    purge_nulls!(nested)
                end
            end
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

"""
    axis_ticks_prefix(axis_configuration::AxisConfiguration)::Maybe{AbstractString}

Return the prefix for the ticks of an `axis_configuration`.
"""
function axis_ticks_prefix(axis_configuration::AxisConfiguration)::Maybe{AbstractString}
    @assert axis_configuration.show_ticks
    if axis_configuration.log_scale == Log10Scale
        return "<sub>10</sub>"
    elseif axis_configuration.log_scale == Log2Scale
        return "<sub>2</sub>"
    else
        @assert axis_configuration.log_scale === nothing
        return nothing
    end
end

"""
    axis_ticks_suffix(axis_configuration::AxisConfiguration)::Maybe{AbstractString}

Return the suffix for the ticks of an `axis_configuration`.
"""
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
function scale_axis_value(axis_configuration::AxisConfiguration, value::Real; clamp::Bool = true)::Float64
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
        return log10((value + offset) * scale)
    elseif axis_configuration.log_scale === Log2Scale
        return log2((value + offset) * scale)
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
    )::Maybe{AbstractVector{<:Maybe{AbstractFloat}}}
    scale_axis_values(
        axis_configuration::AxisConfiguration,
        values::Maybe{AbstractMatrix{<:Maybe{Real}}};
        clamp::Bool = true
    )::Maybe{AbstractMatrix{<:Maybe{AbstractFloat}}}

Scale a vector of `values` according to the `axis_configuration`. This deals with log scales and percent scaling. By
default, `clamp` the values to a specified explicit range. If `copy` we always return a copy of the data (so it can be
safely modified further without impacting the original data).
"""
function scale_axis_values(
    axis_configuration::AxisConfiguration,
    values::Maybe{AbstractVector{<:Maybe{Real}}};
    clamp::Bool = true,
    copy::Bool = false,
)::Maybe{AbstractVector{<:Maybe{AbstractFloat}}}
    if values === nothing
        return nothing  # UNTESTED
    elseif !axis_configuration.percent &&
           axis_configuration.log_scale === nothing &&
           axis_configuration.minimum === nothing &&
           axis_configuration.maximum === nothing
        return copy || eltype(values) <: Maybe{Integer} ? floatify.(values) : values
    else
        function scale(value)
            return scale_axis_value(axis_configuration, value; clamp)
        end
        return scale.(values)
    end
end

function scale_axis_values(
    axis_configuration::AxisConfiguration,
    values::AbstractMatrix{<:Maybe{Real}};
    clamp::Bool = true,
    copy::Bool = false,
)::Maybe{AbstractMatrix{<:Maybe{AbstractFloat}}}
    if !axis_configuration.percent &&
       axis_configuration.log_scale === nothing &&
       axis_configuration.minimum === nothing &&
       axis_configuration.maximum === nothing
        return copy || eltype(values) <: Maybe{Integer} ? floatify.(values) : values
    else
        function scale(value)
            return scale_axis_value(axis_configuration, value; clamp)
        end
        return scale.(values)
    end
end

function floatify(::Nothing)::Nothing
    return nothing
end

function floatify(value::Real)::Float64
    return Float64(value)
end

"""
    final_scaled_range(
        implicit_scaled_range::Union{Range, MaybeRange},
        axis_configuration::AxisConfiguration
    )::Range,

Compute the final range for some axis given the `implicit_scaled_range` computed from the values and the `axis_configuration`.
"""
function final_scaled_range(
    implicit_scaled_range::MaybeRange,
    axis_configuration::AxisConfiguration;
    expand::Bool = true,
)::Range
    @assert implicit_scaled_range.minimum !== nothing
    @assert implicit_scaled_range.maximum !== nothing
    return final_scaled_range(  # NOJET
        Range(; minimum = implicit_scaled_range.minimum, maximum = implicit_scaled_range.maximum),
        axis_configuration;
        expand,
    )
end

function final_scaled_range(
    implicit_scaled_range::Range,
    axis_configuration::AxisConfiguration;
    expand::Bool = true,
)::Range
    explicit_scaled_minimum, explicit_scaled_maximum =
        scale_axis_values(axis_configuration, [axis_configuration.minimum, axis_configuration.maximum]; clamp = false)
    explicit_scaled_range = MaybeRange(; minimum = explicit_scaled_minimum, maximum = explicit_scaled_maximum)

    range = Range(;
        minimum = prefer_data(explicit_scaled_range.minimum, implicit_scaled_range.minimum),
        maximum = prefer_data(explicit_scaled_range.maximum, implicit_scaled_range.maximum),
    )

    if expand
        margins = (range.maximum - range.minimum) / 100
        range.minimum -= margins
        range.maximum += margins
    end

    return range
end

"""
    scale_size_values(
        sizes_configuration::SizesConfiguration,
        values::Maybe{AbstractVector{<:Real}},
    )::Maybe{AbstractVector{<:Real}}

Scale a vector of `values` according to `sizes_configuration`.
"""
function scale_size_values(
    sizes_configuration::SizesConfiguration,
    values::Maybe{AbstractVector{<:Real}},
)::Maybe{AbstractVector{<:Real}}
    if values === nothing
        return nothing
    end

    if sizes_configuration.minimum !== nothing
        minimum_value = sizes_configuration.minimum
        values = max.(values, sizes_configuration.minimum)
    else
        minimum_value = minimum(values)
    end

    if sizes_configuration.maximum !== nothing
        maximum_value = sizes_configuration.maximum
        values = min.(values, sizes_configuration.maximum)
    else
        maximum_value = maximum(values)
    end

    if maximum_value == minimum_value
        maximum_value += 1
    end

    if sizes_configuration.log_scale
        minimum_value = log(minimum_value + sizes_configuration.log_regularization)
        maximum_value = log(maximum_value + sizes_configuration.log_regularization)
        values = log.(values .+ sizes_configuration.log_regularization)
    end

    return [
        sizes_configuration.smallest +
        sizes_configuration.span * (value - minimum_value) / (maximum_value - minimum_value) for value in values
    ]
end

"""
    plotly_line_dash(line_style::LineStyle)::Maybe{AbstractString}

Return the Plotly `line_dash` for a `line_style`.
"""
function plotly_line_dash(line_style::LineStyle)::Maybe{AbstractString}
    if line_style == SolidLine
        return nothing
    elseif line_style == DashLine
        return "dash"
    elseif line_style == DotLine
        return "dot"
    elseif line_style == DashDotLine
        return "dashdot"
    else
        @assert false
    end
end

"""
    push_vertical_bands_shapes(
        shapes::AbstractVector{Shape},
        axis_configuration::AxisConfiguration,
        scaled_values_range::Range,
        bands_data::BandsData,
        bands_configuration::BandsConfiguration,
        bands_scale::Real = 1,
    )::AbstractVector{<:Shape}

Push shapes for plotting vertical bands. These shapes need to be places in the layout and not the traces because Plotly.
"""
function push_vertical_bands_shapes(
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    scaled_values_range::Range,
    bands_data::BandsData,
    bands_configuration::BandsConfiguration,
    bands_scale::Real = 1,
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
            push!(  # NOJET
                shapes,
                Shape(
                    "line";
                    line_color = band_configuration.line.color,
                    line_dash = plotly_line_dash(band_configuration.line.style),
                    x0 = scaled_offset * bands_scale,
                    x1 = scaled_offset * bands_scale,
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
                x0 = scaled_values_range.minimum,
                x1 = scaled_low_offset * bands_scale,
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
                x0 = scaled_low_offset * bands_scale,
                x1 = scaled_high_offset * bands_scale,
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
                x0 = scaled_high_offset * bands_scale,
                x1 = scaled_values_range.maximum,
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
        scaled_values_range::Range,
        bands_data::BandsData,
        bands_configuration::BandsConfiguration,
        bands_scale::Real = 1,
    )::AbstractVector{<:Shape}

Push shapes for plotting horizontal bands. These shapes need to be placed in the layout and not the traces because
Plotly.
"""
function push_horizontal_bands_shapes(
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    scaled_values_range::Range,
    bands_data::BandsData,
    bands_configuration::BandsConfiguration,
    bands_scale::Real = 1,
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
            push!(  # NOJET
                shapes,
                Shape(
                    "line";
                    line_color = band_configuration.line.color,
                    line_dash = plotly_line_dash(band_configuration.line.style),
                    y0 = scaled_offset * bands_scale,
                    y1 = scaled_offset * bands_scale,
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
                y0 = scaled_values_range.minimum,
                y1 = scaled_low_offset * bands_scale,
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
                y0 = scaled_low_offset * bands_scale,
                y1 = scaled_high_offset * bands_scale,
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
                y0 = scaled_high_offset * bands_scale,
                y1 = scaled_values_range.maximum,
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
    point::ScaledPoint
    side::Side
end

"""
    push_diagonal_bands_shapes(
        shapes::AbstractVector{Shape},
        x_axis_configuration::AxisConfiguration,
        y_axis_configuration::AxisConfiguration,
        x_scaled_values_range::Range,
        y_scaled_values_range::Range,
        bands_data::BandsData,
        bands_configuration::BandsConfiguration
    )::AbstractVector{<:Shape}

Push shapes for plotting diagonal bands. These shapes need to be placed in the layout and not the traces because Plotly.
"""
function push_diagonal_bands_shapes(
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
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
    push_diagonal_bands_line(
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
            high_band_points,
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

function push_diagonal_bands_line(
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
    data_offset::Maybe{Real},
    band_configuration::BandConfiguration,
)::Maybe{Tuple{BandPoint, BandPoint}}
    offset = prefer_data(data_offset, band_configuration.offset)
    start_point = start_diagonal_band_point(axis_configuration, x_scaled_values_range, y_scaled_values_range, offset)
    end_point = end_diagonal_band_point(axis_configuration, x_scaled_values_range, y_scaled_values_range, offset)
    @assert (start_point === nothing) == (end_point === nothing)
    if start_point === nothing
        return nothing
    end

    if band_configuration.line.style !== nothing
        push!(  # NOJET
            shapes,
            Shape(
                "line";
                line_color = band_configuration.line.color,
                line_dash = plotly_line_dash(band_configuration.line.style),
                y0 = start_point.point.y,
                y1 = end_point.point.y,
                yref = "y",
                x0 = start_point.point.x,
                x1 = end_point.point.x,
                xref = "x",
            ),
        )
    end

    return (start_point, end_point)
end

function start_diagonal_band_point(::AxisConfiguration, ::Range, ::Range, ::Nothing)::Nothing
    return nothing
end

function start_diagonal_band_point(
    axis_configuration::AxisConfiguration,
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
    offset::Real,
)::Maybe{BandPoint}
    scaled_offset = scale_axis_offset(axis_configuration, offset)
    bottom = ScaledPoint(; x = y_scaled_values_range.minimum - scaled_offset, y = y_scaled_values_range.minimum)
    left = ScaledPoint(; x = x_scaled_values_range.minimum, y = x_scaled_values_range.minimum + scaled_offset)

    if is_in_bounds(x_scaled_values_range, y_scaled_values_range, bottom)
        if is_in_bounds(x_scaled_values_range, y_scaled_values_range, left)
            @assert isapprox(bottom.x, left.x)
            @assert isapprox(bottom.y, left.y)
            return BandPoint(bottom, BottomLeft)
        else
            return BandPoint(bottom, Bottom)
        end
    elseif is_in_bounds(x_scaled_values_range, y_scaled_values_range, left)
        return BandPoint(left, Left)
    else
        return nothing  # UNTESTED
    end
end

function end_diagonal_band_point(::AxisConfiguration, ::Range, ::Range, ::Nothing)::Nothing
    return nothing
end

function end_diagonal_band_point(
    axis_configuration::AxisConfiguration,
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
    offset::Real,
)::Maybe{BandPoint}
    scaled_offset = scale_axis_offset(axis_configuration, offset)
    top = ScaledPoint(; x = y_scaled_values_range.maximum - scaled_offset, y = y_scaled_values_range.maximum)
    right = ScaledPoint(; x = x_scaled_values_range.maximum, y = x_scaled_values_range.maximum + scaled_offset)

    if is_in_bounds(x_scaled_values_range, y_scaled_values_range, top)
        if is_in_bounds(x_scaled_values_range, y_scaled_values_range, right)
            @assert isapprox(top.x, right.x)
            @assert isapprox(top.y, right.y)
            return BandPoint(top, TopRight)
        else
            return BandPoint(top, Top)
        end
    elseif is_in_bounds(x_scaled_values_range, y_scaled_values_range, right)
        return BandPoint(right, Right)
    else
        return nothing
    end
end

function scale_axis_offset(axis_configuration::AxisConfiguration, offset::Real)::Real
    if axis_configuration.percent
        scale = 100.0  # UNTESTED
    else
        scale = 1.0
    end

    if axis_configuration.log_scale === Log10Scale
        return log10(offset * scale)
    elseif axis_configuration.log_scale === Log2Scale
        return log2(offset * scale)  # UNTESTED
    else
        @assert axis_configuration.log_scale === nothing
        return offset
    end
end

function is_in_bounds(x_scaled_values_range::Range, y_scaled_values_range::Range, point::ScaledPoint)::Bool
    return (x_scaled_values_range.minimum <= point.x <= x_scaled_values_range.maximum) &&
           (y_scaled_values_range.minimum <= point.y <= y_scaled_values_range.maximum)
end

function push_diagonal_bands_low_fill(
    shapes::AbstractVector{Shape},
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
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

function push_diagonal_bands_high_fill(
    shapes::AbstractVector{Shape},
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
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

function push_diagonal_bands_middle_fill(
    shapes::AbstractVector{Shape},
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
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

function to_start_point(path_parts::AbstractVector{<:AbstractString}, band_points::Tuple{BandPoint, BandPoint})::Nothing
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(band_points[1].point.x))
    push!(path_parts, string(band_points[1].point.y))
    return nothing
end

function to_end_point(path_parts::AbstractVector{<:AbstractString}, band_points::Tuple{BandPoint, BandPoint})::Nothing
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(band_points[2].point.x))
    push!(path_parts, string(band_points[2].point.y))
    return nothing
end

function to_bottom_left(
    path_parts::AbstractVector{<:AbstractString},
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
)::Nothing
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(x_scaled_values_range.minimum))
    push!(path_parts, string(y_scaled_values_range.minimum))
    return nothing
end

function to_bottom_right(
    path_parts::AbstractVector{<:AbstractString},
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
)::Nothing
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(x_scaled_values_range.maximum))
    push!(path_parts, string(y_scaled_values_range.minimum))
    return nothing
end

function to_top_left(
    path_parts::AbstractVector{<:AbstractString},
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
)::Nothing
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(x_scaled_values_range.minimum))
    push!(path_parts, string(y_scaled_values_range.maximum))
    return nothing
end

function to_top_right(
    path_parts::AbstractVector{<:AbstractString},
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
)::Nothing
    push!(path_parts, isempty(path_parts) ? "M" : "L")
    push!(path_parts, string(x_scaled_values_range.maximum))
    push!(path_parts, string(y_scaled_values_range.maximum))
    return nothing
end

function push_fill_path(
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
        return data_values[index]
    else
        return configuration_value
    end
end

"""
    collect_range!(
        range::MaybeRange,
        values::AbstractVector{<:Maybe{Real}},
    )::Nothing

Expand the `range` to cover the `values.
"""
function collect_range!(range::MaybeRange, values::Maybe{AbstractVector{<:Maybe{Real}}})::Nothing
    if values !== nothing
        for value in values
            if range.minimum === nothing || (value !== nothing && value < range.minimum)
                range.minimum = value
            end
            if range.maximum === nothing || (value !== nothing && value > range.maximum)
                range.maximum = value
            end
        end
    end

    return nothing
end

"""
    @kwdef struct SubGraph
        index::Integer
        n_graphs::Integer
        gap::Maybe{AbstractFloat}
        n_annotations::Integer = 0
        annotation_size::Maybe{AnnotationSize} = nothing
        dendogram_size::Maybe{Real} = nothing
    end

Identify one sub-graph out of a set of `n_graphs` adjacent graphs along some axis. If the `index` is 1, this is the 1st
sub-graph (used top initialize some values such as the legend group title). If `gap` is `nothing` then the sub-graphs
are plotted on top of each other, which affects axis parameters; otherwise, the sub-graphs are plotted with this gap,
which affects layout parameters.

This also supports `n_annotations` (of the other axis) with `annotation_size` (along this axis). If the index is
negative, it is the (negated) index of an annotation (of the other axis).

If the `index` is 0, this is the dendogram graph (of the other axis) with `dendogram_size` (along this axis).
"""
@kwdef struct SubGraph
    index::Integer
    n_graphs::Integer
    graphs_gap::Maybe{AbstractFloat}
    n_annotations::Integer = 0
    annotation_size::Maybe{AnnotationSize} = nothing
    dendogram_size::Maybe{Real} = nothing
end

"""
    plotly_sub_graph_domain(sub_graph::SubGraph)::Maybe{AbstractVector{<:AbstractFloat}}

Return the plotly domain (region of the overall figure) for a specific `sub_graph`.
"""
function plotly_sub_graph_domain(sub_graph::SubGraph)::Maybe{AbstractVector{<:AbstractFloat}}
    axis_index = sub_graph.index

    n_graphs = sub_graph.n_graphs
    if sub_graph.graphs_gap === nothing
        n_graphs = 1
        if axis_index > 0
            axis_index = 1
        end
    end

    if n_graphs == 1 && sub_graph.n_annotations == 0 && sub_graph.dendogram_size === nothing
        return nothing
    end

    if axis_index > 0
        @assert 1 <= axis_index <= n_graphs

        if sub_graph.n_annotations == 0
            start_graph_offset = 0
        else
            @assert sub_graph.annotation_size !== nothing
            start_graph_offset =
                (sub_graph.annotation_size.gap + sub_graph.annotation_size.size) * sub_graph.n_annotations
        end

        graphs_total_size = 1 - start_graph_offset - (n_graphs - 1) * prefer_data(sub_graph.graphs_gap, 0)
        if sub_graph.dendogram_size !== nothing
            graphs_total_size -= sub_graph.dendogram_size
        end
        graph_size = graphs_total_size / n_graphs
        start_graph_offset += (axis_index - 1) * (graph_size + prefer_data(sub_graph.graphs_gap, 0))
        end_graph_offset = start_graph_offset + graph_size

    elseif axis_index < 0
        axis_index = -axis_index
        @assert 1 <= axis_index <= sub_graph.n_annotations
        @assert sub_graph.annotation_size !== nothing
        start_graph_offset = (axis_index - 1) * (sub_graph.annotation_size.gap + sub_graph.annotation_size.size)
        end_graph_offset = start_graph_offset + sub_graph.annotation_size.size

    else
        @assert sub_graph.dendogram_size !== nothing
        start_graph_offset = 1 - sub_graph.dendogram_size
        end_graph_offset = 1
    end

    @assert 0 <= start_graph_offset < end_graph_offset <= 1
    return [start_graph_offset, end_graph_offset]
end

"""
    validate_axis_sizes(;
        axis_name::AbstractString,
        graphs_gap::Maybe{Real} = nothing,
        n_graphs::Integer = 1,
        annotation_size::AnnotationSize,
        n_annotations::Integer,
        dendogram_size::Maybe{Real} = nothing,
    )::nothing

Verify there is at least some space left for the actual graph after leaving space for gaps and/or annotations.
"""
function validate_axis_sizes(;
    axis_name::AbstractString,
    graphs_gap::Maybe{Real} = nothing,
    n_graphs::Integer = 1,
    annotation_size::AnnotationSize,
    n_annotations::Integer,
    dendogram_size::Maybe{Real} = nothing,
)::Nothing
    if graphs_gap === nothing
        graph_gaps_overhead = 0
    else
        graph_gaps_overhead = (n_graphs - 1) * graphs_gap
    end

    annotations_gaps_overhead = n_annotations * annotation_size.gap
    annotations_sizes_overhead = n_annotations * annotation_size.size

    if dendogram_size === nothing
        dendogram_size_overhead = 0.0
    else
        dendogram_size_overhead = dendogram_size
    end

    total_overhead_size =
        graph_gaps_overhead + annotations_gaps_overhead + annotations_sizes_overhead + dendogram_size_overhead

    if total_overhead_size >= 1
        text = "no space left in the $(axis_name) axis"
        if graph_gaps_overhead > 0
            text *=
                "\nnumber of graphs: $(n_graphs)" *
                "\nwith gap between graphs: $(graphs_gap) (total: $(graph_gaps_overhead))"
        end
        if n_annotations > 0
            text *=
                "\nnumber of annotations: $(n_annotations)" *
                "\nwith gap between annotations: $(annotation_size.gap) (total: $(annotations_gaps_overhead))" *
                "\nwith size of each annotation: $(annotation_size.size) (total: $(annotations_sizes_overhead))"
        end
        if dendogram_size !== nothing
            text *= "\ndendogram size: $(dendogram_size_overhead)"  # UNTESTED
        end
        text *= "\nthe total overhead: $(total_overhead_size)" * "\nis not less than: 1"
        throw(ArgumentError(text))  # NOJET
    end

    return nothing
end

"""
    plotly_sub_graph_axes(;
        basis_sub_graph::Maybe{SubGraph} = nothing,
        values_sub_graph::Maybe{SubGraph} = nothing,
        values_orientation::ValuesOrientation,
    )::Tuple{
        Maybe{Integer},
        Maybe{AbstractFloat},
        Maybe{Integer},
        Maybe{AbstractFloat},
    }

Return the X axis index and zero value, and the Y axis index and zero value, for a sub-graph.
"""
function plotly_sub_graph_axes(;
    basis_sub_graph::Maybe{SubGraph} = nothing,
    values_sub_graph::Maybe{SubGraph} = nothing,
    values_orientation::ValuesOrientation,
)::Tuple{Maybe{Integer}, Maybe{AbstractFloat}, Maybe{Integer}, Maybe{AbstractFloat}}
    basis_axis_index, basis_zero_value = plotly_sub_graph_axis(basis_sub_graph)
    values_axis_index, values_zero_value = plotly_sub_graph_axis(values_sub_graph)

    if values_orientation == VerticalValues
        return (basis_axis_index, basis_zero_value, values_axis_index, values_zero_value)
    elseif values_orientation == HorizontalValues
        return (values_axis_index, values_zero_value, basis_axis_index, basis_zero_value)
    else
        @assert false
    end
end

function plotly_sub_graph_axis(::Nothing)::Tuple{Nothing, Nothing}
    return (nothing, nothing)
end

function plotly_sub_graph_axis(sub_graph::SubGraph)::Tuple{Maybe{Integer}, Maybe{AbstractFloat}}
    axis_index = sub_graph.index
    n_graphs = sub_graph.n_graphs

    if sub_graph.graphs_gap === nothing
        n_graphs = 1
        if axis_index > 0
            axis_index = 1
        end
    end

    if axis_index < 0
        axis_index = -axis_index
        @assert axis_index <= sub_graph.n_annotations
    elseif axis_index == 0
        axis_index = sub_graph.n_annotations + sub_graph.n_graphs + 1
    else
        @assert axis_index <= sub_graph.n_graphs
        axis_index = sub_graph.n_annotations + axis_index
    end

    zero_value = sub_graph.index > 0 && n_graphs > 1 ? 0 : nothing

    return (axis_index, zero_value)
end

"""
    @kwdef mutable struct ConfiguredColors
        colors_title::Maybe{AbstractString}
        colors_configuration::ColorsConfiguration
        colors_scale_index::Maybe{Integer}
        original_color_values::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}}
        final_colors_values::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}}
        final_colors_range::Maybe{Range}
        scaled_colors_palette::Maybe{AbstractVector{<:Tuple{Real, AbstractString}}}
        show_in_legend::Bool
        show_scale::Bool
    end

Colors data after applying the configuration.
"""
@kwdef mutable struct ConfiguredColors
    colors_title::Maybe{AbstractString}
    colors_configuration::ColorsConfiguration
    colors_scale_index::Maybe{Integer}
    original_color_values::Maybe{
        Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}, AbstractMatrix{<:Real}},
    }
    final_colors_values::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}, AbstractMatrix{<:Real}}}
    final_colors_range::Maybe{Range}
    scaled_colors_palette::Maybe{AbstractVector{<:Tuple{Real, AbstractString}}}
    show_in_legend::Bool
    show_scale::Bool
end

"""
    configured_colors(;
        colors_configuration::ColorsConfiguration,
        colors_title::Maybe{AbstractString},
        colors_values::Maybe{Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}, AbstractMatrix{<:Real}}},
        next_colors_scale_index::AbstractVector{<:Integer},
        mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
    )::ConfiguredColors

Apply the colors configuration to the colors data.
"""
function configured_colors(;
    colors_configuration::ColorsConfiguration,
    colors_title::Maybe{AbstractString},
    colors_values::Maybe{Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}, AbstractMatrix{<:Real}}},
    next_colors_scale_index::AbstractVector{<:Integer},
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
)::ConfiguredColors
    original_color_values = colors_values
    scaled_colors_palette = nothing

    colors_scale_index = nothing
    final_colors_range = nothing
    final_colors_values = nothing
    show_in_legend = false
    show_scale = false

    if colors_values isa AbstractVector{<:AbstractString}
        show_in_legend = colors_configuration.show_legend

        if colors_configuration.palette isa CategoricalColors
            @assert colors_values isa AbstractVector{<:AbstractString}
            final_colors_values = [
                color != "" && prefer_data(mask, index, true) ? colors_configuration.palette[color] : ""  #
                for (index, color) in enumerate(colors_values)
            ]
        else
            @assert colors_configuration.palette === nothing
            final_colors_values = colors_values
        end

    elseif colors_values !== nothing && eltype(colors_values) <: Real
        show_scale = colors_configuration.show_legend
        colors_scale_index = next_colors_scale_index[1]
        next_colors_scale_index[1] += 1

        final_colors_values = scale_axis_values(colors_configuration.axis, colors_values)
        if colors_configuration.palette isa ContinuousColors
            color_palette_values = [entry[1] for entry in colors_configuration.palette]
            scaled_colors_palette_values = scale_axis_values(colors_configuration.axis, color_palette_values)
            implicit_scaled_colors_range =
                Range(; minimum = scaled_colors_palette_values[1], maximum = scaled_colors_palette_values[end])
            final_colors_range =
                final_scaled_range(implicit_scaled_colors_range, colors_configuration.axis; expand = false)

            scale = implicit_scaled_colors_range.maximum - implicit_scaled_colors_range.minimum
            @assert scale > 0
            final_color_palette_values = (scaled_colors_palette_values .- implicit_scaled_colors_range.minimum) ./ scale
            final_color_palette_values[1] = 0
            final_color_palette_values[end] = 1
            scaled_colors_palette = [  # NOJET
                (final_value, entry[2]) for
                (final_value, entry) in zip(final_color_palette_values, colors_configuration.palette)
            ]
        else
            implicit_scaled_colors_range =
                Range(; minimum = minimum(final_colors_values), maximum = maximum(final_colors_values))
            final_colors_range =
                final_scaled_range(implicit_scaled_colors_range, colors_configuration.axis; expand = false)
        end
    end

    return ConfiguredColors(;
        colors_title,
        colors_configuration,
        colors_scale_index,
        original_color_values,
        final_colors_values,
        final_colors_range,
        scaled_colors_palette,
        show_in_legend,
        show_scale,
    )
end

"""
    @kwdef mutable struct ConfiguredAnnotation
        annotation_data::AnnotationData
        final_colors_values::AbstractVector{<:Real}
        final_colors_range::Maybe{Range}
    end
"""
@kwdef mutable struct ConfiguredAnnotation
    annotation_data::AnnotationData
    final_hovers::AbstractVector{<:AbstractString}
    final_colors_values::Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}}
    final_colors_range::Maybe{Range}
end

end  # module
