"""
Utility functions for defining graph types. We do not re-export all symbols from this sub-module to the global
`SomeGraphs` namespace. You can safely ignore these unless you are implementing a new graph type.
"""
module Utilities

export axis_ticks_prefix
export axis_ticks_suffix
export collect_hidden_range!
export collect_range!
export configured_colors
export ConfiguredColors
export displayed_annotations
export entities_hovers
export fill_color
export final_scaled_range
export MaybeRange
export axis_screen_ticks_angle
export masked_values
export numeric_values
export plotly_axis
export plotly_axis_value
export plotly_axis_values
export plotly_figure
export plotly_layout
export plotly_legend
export place_side_panel!
export plotly_line_dash
export plotly_sub_graph_axes
export plotly_sub_graph_domain
export plotly_title
export prefer_data
export push_diagonal_bands_shapes
export push_horizontal_bands_shapes
export push_twin_log_axis_traces!
export push_vertical_bands_shapes
export Range
export range_values
export scale_axis_value
export scale_axis_values
export scale_size_values
export set_layout_axis!
export set_layout_colorscale!
export set_layout_legend!
export set_layout_margins!
export set_layout_twin_log_axis!
export shared_values_title
export SidesContent
export sizes_legend_entries
export SizesLegendEntry
export SubGraph
export text_width
export validate_axis_sizes
export validate_colors
export validate_graph_bands
export validate_numeric_values
export validate_values

using Colors
using PlotlyBase
using Reexport

using ..Validations
using ..Common

import .Common.CACHED_COLOR_SCALES
import .Common.PLOTLY_DEFAULT_HEIGHT
import .Common.PLOTLY_DEFAULT_WIDTH
import .Common.categorical_colors_dict
import .Validations.Maybe

@reexport import .Common.validate_graph
@reexport import .Common.graph_to_figure
@reexport import .Common.graph_to_html
@reexport import .Common.graph_to_image
@reexport import .Common.graph_to_json
@reexport import .Common.flip_axes
@reexport import .Common.flip_axes!

include("lato_widths.jl")

# The advance width of the widest character in `LATO_ADVANCE_PER_CHARACTER`, used for any character it lacks.
const LATO_WIDEST_ADVANCE = maximum(values(LATO_ADVANCE_PER_CHARACTER))

"""
    text_width(text::AbstractString; font_size::Real = 12)::Float64

The width in pixels of a `text` written in Lato (the default font) of `font_size` pixels. This adds the advance width
of each character and the kerning of each pair of adjacent characters, from the Lato font which Kaleido bundles. A
character which is not in the table counts as the widest one. Plotly's own measure of a text (in Kaleido) may be up to
0.6 pixels wider.
"""
function text_width(text::AbstractString; font_size::Real = 12)::Float64
    font_units = 0
    previous_character = nothing
    for character in text
        font_units += get(LATO_ADVANCE_PER_CHARACTER, character, LATO_WIDEST_ADVANCE)
        if previous_character !== nothing
            font_units += get(LATO_KERNING_PER_PAIR, (previous_character, character), 0)
        end
        previous_character = character
    end
    return font_units * font_size / LATO_UNITS_PER_EM
end

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

    for (offset_field, offset) in (
        ("low_offset", bands_data.low_offset),
        ("middle_offset", bands_data.middle_offset),
        ("high_offset", bands_data.high_offset),
    )
        validate_in(context, offset_field) do
            validate_is_finite(context, offset)
            return nothing
        end
    end

    validate_is_range(context, "low_offset", bands_data.low_offset, "middle_offset", bands_data.middle_offset)

    validate_is_range(context, "middle_offset", bands_data.middle_offset, "high_offset", bands_data.high_offset)

    validate_is_range(context, "low_offset", bands_data.low_offset, "high_offset", bands_data.high_offset)

    if axis_configuration !== nothing && axis_configuration.scale.log_base !== nothing
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
        mask::Nothing = nothing,
    )::Nothing

Validate that the `colors_data` from the `colors_data_context` is valid and consistent with the `colors_configuration`
from the `colors_configuration_context`. For example, if the color configuration contains a categorical color mapping,
this will validate that all the color names in the data are valid keys of this mapping.

If a `mask` is specified, do not validate colors in the data whose matching value in the mask is false. A mask is not
supported for a matrix of colors.
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

    if colors_configuration.legend.show
        if colors_data === nothing
            throw(
                ArgumentError(
                    "must specify $(location(colors_data_context))\n" *
                    "for $(location(colors_configuration_context)).legend.show",
                ),
            )
        elseif eltype(colors_data) <: AbstractString && colors_configuration.palette === nothing
            throw(
                ArgumentError(
                    "can't specify $(location(colors_configuration_context)).legend.show\n" *
                    "for named $(location(colors_data_context))",
                ),
            )
        end
    end

    if colors_configuration.scale.minimum !== nothing ||
       colors_configuration.scale.maximum !== nothing ||
       colors_configuration.scale.log_base !== nothing ||
       colors_configuration.scale.percent
        if !(colors_data isa AbstractVector{<:Real})
            throw(
                ArgumentError(
                    "must specify numeric $(location(colors_data_context))\n" *
                    "when using any of " *
                    "$(location(colors_configuration_context)).scale.(minimum,maximum,log_base,percent)",
                ),
            )
        end

        if colors_configuration.scale.log_base !== nothing
            validate_vector_entries(colors_data_context, colors_data, mask) do _, color
                if colors_configuration.scale.minimum === nothing || color >= colors_configuration.scale.minimum
                    validate_in(
                        colors_data_context,
                        "(value + $(location(colors_configuration_context)).scale.log_regularization)",
                    ) do
                        validate_is_above(colors_data_context, color + colors_configuration.scale.log_regularization, 0)
                        return nothing
                    end
                end
            end
        end
    end

    if colors_configuration.palette === nothing
        if colors_data isa AbstractVector{<:AbstractString}
            validate_vector_entries(colors_data_context, colors_data, mask) do _, color
                return validate_is_color(colors_data_context, color)
            end
        end

    elseif colors_configuration.palette isa CategoricalColors
        if colors_data isa AbstractVector{<:AbstractString}
            palette_dict = categorical_colors_dict(colors_configuration.palette)  # NOJET

            validate_vector_entries(colors_data_context, colors_data, mask) do _, color
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

    elseif colors_configuration.palette isa AutomaticColors
        if colors_data isa AbstractVector{<:Real}
            throw(
                ArgumentError(
                    "numeric $(location(colors_data_context))\n" *
                    "specified for automatic $(location(colors_configuration_context)).palette",
                ),
            )
        elseif colors_data === nothing
            throw(
                ArgumentError(
                    "must specify (categorical) $(location(colors_data_context))\n" *
                    "for automatic $(location(colors_configuration_context)).palette",
                ),
            )
        else
            @assert colors_data isa AbstractVector{<:AbstractString}
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
    if colors_configuration.scale.minimum !== nothing ||
       colors_configuration.scale.maximum !== nothing ||
       colors_configuration.scale.log_base !== nothing ||
       colors_configuration.scale.percent
        if colors_configuration.scale.log_base !== nothing
            validate_matrix_entries(colors_data_context, "entries_colors", colors_data) do _, _, color
                if colors_configuration.scale.minimum === nothing || color >= colors_configuration.scale.minimum
                    validate_in(
                        colors_data_context,
                        "(value + $(location(colors_configuration_context)).scale.log_regularization)",
                    ) do
                        validate_is_above(colors_data_context, color + colors_configuration.scale.log_regularization, 0)
                        return nothing
                    end
                end
            end
        end
    end

    if colors_configuration.palette isa AbstractString
        lock(COLOR_SCALES_LOCK) do  # UNTESTED
            @assert haskey(CACHED_COLOR_SCALES, colors_configuration.palette)  # FLAKY TESTED
        end
    end

    return nothing
end

"""
    validate_values(
        values_data_context::ValidationContext,
        values_data::Maybe{AbstractVector{<:Real}},
        axis_configuration_context::ValidationContext,
        axis_configuration::AxisConfiguration,
    )::Nothing

Validate that the `values_data` from the `values_data_context` is valid and consistent with the `axis_configuration`
from the `axis_configuration_context`. Specifically this ensures that if a `log_base` is applied, all the values
are positive.
"""
function validate_values(
    values_data_context::ValidationContext,
    values_data::Maybe{AbstractVector{<:Real}},
    axis_configuration_context::ValidationContext,
    axis_configuration::AxisConfiguration,
)::Nothing
    if values_data !== nothing && axis_configuration.scale.log_base !== nothing
        for (index, value) in enumerate(values_data)
            validate_in(
                values_data_context,
                "([$(index)] + $(location(axis_configuration_context)).scale.log_regularization)",
            ) do
                return validate_is_above(values_data_context, value + axis_configuration.scale.log_regularization, 0)
            end
        end
    end

    return nothing
end

"""
    @kwdef mutable struct SidesContent
        least_plot_width::Float64
        least_plot_height::Float64
        left::Float64 = 0
        right::Float64 = 0
        top::Float64 = 0
        bottom::Float64 = 0
    end

    SidesContent(figure_configuration::FigureConfiguration)

How far the content beyond the plot's area reaches on each side, in pixels. This is the tick labels, the titles of the
axes and of the figure, the titles of the annotations, and the side panel. A graph makes one of these for its layout.
Each part of the layout grows each side to the farthest it reaches. Each margin is then the content of its side plus
the configured margin (see [`set_layout_margins!`](@ref)).

A label near an end of its axis may reach past that end, into the side next to it. How far it is from that end is
bounded from below by the `least_plot_width` and `least_plot_height` of the plot's area. Made from a
`figure_configuration`, these are half the figure's width and height, or half of Plotly's default size (700 by 450
pixels) when the figure doesn't set them.
"""
@kwdef mutable struct SidesContent
    least_plot_width::Float64
    least_plot_height::Float64
    left::Float64 = 0
    right::Float64 = 0
    top::Float64 = 0
    bottom::Float64 = 0
end

function SidesContent(figure_configuration::FigureConfiguration)::SidesContent
    return SidesContent(;
        least_plot_width = prefer_data(figure_configuration.width, PLOTLY_DEFAULT_WIDTH) / 2,
        least_plot_height = prefer_data(figure_configuration.height, PLOTLY_DEFAULT_HEIGHT) / 2,
    )
end

# The font size Plotly uses for the figure's title. Its box is as tall as its font size.
const PLOTLY_FIGURE_TITLE_FONT_SIZE = 17

# The pixels between the figure's title and the plot's area.
const FIGURE_TITLE_GAP = 10

"""
    plotly_layout(
        figure_configuration::FigureConfiguration;
        sides_content::SidesContent,
        title::Maybe{AbstractString},
        has_legend::Bool,
        has_hovers::Bool = false,
        shapes::Maybe{AbstractVector{Shape}} = nothing,
    )::Layout

Create a Plotly `Layout` object. If `has_legend`, the legends are shown; each is set by [`set_layout_legend!`](@ref).

The `title` (if any) is drawn below the top of the figure, separated from it by the top margin (see
[`MarginsConfiguration`](@ref)). It is added to the `sides_content`. The margins themselves are set by
[`set_layout_margins!`](@ref).
"""
function plotly_layout(
    figure_configuration::FigureConfiguration;
    sides_content::SidesContent,
    title::Maybe{AbstractString},
    has_legend::Bool,
    has_hovers::Bool = false,
    shapes::Maybe{AbstractVector{Shape}} = nothing,
)::Layout
    plotly_figure_title = plotly_title(title)
    if plotly_figure_title !== nothing
        plotly_figure_title[:yref] = "container"
        plotly_figure_title[:y] = 1
        plotly_figure_title[:yanchor] = "top"
        plotly_figure_title[:pad] = Dict(:t => figure_configuration.margins.top)
        sides_content.top = max(sides_content.top, PLOTLY_FIGURE_TITLE_FONT_SIZE + FIGURE_TITLE_GAP)
    end

    layout = Layout(;  # NOJET
        title = plotly_figure_title,
        showlegend = has_legend,
        legend_tracegroupgap = 0,
        hoverlabel_align = has_hovers ? "left" : nothing,
        shapes,
        # An empty template, because we style the graph ourselves. Without one, PlotlyBase fills in whatever
        # `PlotlyBase.templates.default` happens to be, which is global state that would change what we emit.
        template = Template(),
        # Lato is bundled with Kaleido, so static images always have it.
        font_family = "Lato, sans-serif",
        width = prefer_data(figure_configuration.width, nothing),
        height = prefer_data(figure_configuration.height, nothing),
        plot_bgcolor = figure_configuration.background_color,
        paper_bgcolor = figure_configuration.paper_color,
    )

    # The margins are exactly as computed (see `set_layout_margins!`), so the plot's area is known. They replace
    # PlotlyBase's default margins, a `Dict` of integers, into which `false` would be converted to 0, which isn't a
    # boolean for Plotly.
    layout.fields[:margin] = Dict{Symbol, Any}(
        :l => figure_configuration.margins.left,
        :r => figure_configuration.margins.right,
        :t => figure_configuration.margins.top,
        :b => figure_configuration.margins.bottom,
        :autoexpand => false,
    )
    return layout
end

"""
    plotly_legend(index::Integer)::String

The name of the Plotly legend of an `index`: `legend` for 1, and `legend2`, `legend3`, etc. for the rest. A trace is
shown in a legend by giving this name as its `legend`.
"""
function plotly_legend(index::Integer)::String
    return index == 1 ? "legend" : "legend$(index)"
end

"""
    set_layout_legend!(layout::Layout, index::Integer; title::Maybe{AbstractString})::Nothing

Set the Plotly legend of an `index` (see [`plotly_legend`](@ref)) in a `layout`, with its `title`. Its place is set by
[`place_side_panel!`](@ref).
"""
function set_layout_legend!(layout::Layout, index::Integer; title::Maybe{AbstractString})::Nothing
    layout[plotly_legend(index)] =
        Dict{Symbol, Any}(:title => plotly_title(title), :itemdoubleclick => false, :tracegroupgap => 0)
    return nothing
end

"""
    axis_screen_ticks_angle(ticks_angle::Nothing, is_vertical_axis::Bool)::Real
    axis_screen_ticks_angle(ticks_angle::Real, is_vertical_axis::Bool)::Real

Convert the axis-relative `ticks_angle` (0 = parallel to the axis, ±90 = perpendicular) to the screen angle used by
Plotly's `tickangle` (0 = horizontal). A vertical axis is parallel when standing the text up (bottom-to-top, -90). A
`nothing` angle keeps the labels horizontal, the readable orientation for numbers regardless of the axis direction.

The screen angle is always between -90 and 90, so the text is never upside down. A label of a vertical axis at a
negative `ticks_angle` lies on the same line it would at `ticks_angle - 90`, but reads the other way.
"""
function axis_screen_ticks_angle(::Nothing, ::Bool)::Real
    return 0
end

function axis_screen_ticks_angle(ticks_angle::Real, is_vertical_axis::Bool)::Real
    if !is_vertical_axis
        return ticks_angle
    elseif ticks_angle < 0
        return ticks_angle + 90
    else
        return ticks_angle - 90
    end
end

# The font sizes Plotly uses for tick labels and for axis titles. A label's box is as tall as its font size.
const PLOTLY_TICK_LABEL_FONT_SIZE = 12  # NOLINT
const PLOTLY_AXIS_TITLE_FONT_SIZE = 14  # NOLINT

# How far Plotly draws tick labels from a horizontal (x) or a vertical (y) axis, in pixels. In Kaleido's renders, their
# ink started 3 to 7 pixels below an x axis, and ended 1 to 5 pixels left of a y axis.
const PLOTLY_X_TICK_LABELS_GAP = 7  # NOLINT
const PLOTLY_Y_TICK_LABELS_GAP = 5  # NOLINT

# How far the text of a Plotly annotation is from its anchor, in pixels: its default `borderpad` plus `borderwidth`
# (not measured).
const PLOTLY_ANNOTATION_PADDING = 2  # NOLINT

# The pixels between an axis' tick labels and its title.
const AXIS_TITLE_STANDOFF = 10  # NOLINT

"""
    set_layout_axis!(
        layout::Layout,
        axis::Maybe{AbstractString},
        axis_configuration::AxisConfiguration;
        sides_content::SidesContent,
        title::Maybe{AbstractString} = nothing,
        range::Maybe{Range} = nothing,
        labels_range::Maybe{Range} = range,
        ticks_labels::Maybe{AbstractVector{<:AbstractString}} = nothing,
        ticks_values::Maybe{AbstractVector{<:Real}} = nothing,
        domain::Maybe{AbstractVector{<:Real}} = nothing,
        is_tick_axis::Bool = true,
        is_zeroable::Bool = true,
        is_reversed::Bool = false,
        is_plotly_log::Bool = false,
    )::Nothing

Add a Plotly `axis` in a `layout` using the `axis_configuration`. The `axis` is the name given by [`plotly_axis`](@ref),
which must not be `nothing`. If `is_reversed`, the axis runs backwards (right to
left, or top to bottom), which is how the left (or lower) side of a mirrored bars graph grows away from the axis it
shares with the other side.

The `ticks_values` and `ticks_labels` (if any) give explicit ticks. Unless `is_tick_axis`, no ticks are shown at all.
Unless `is_zeroable`, the zero line is left to Plotly (otherwise, it is shown for linear scales only).

If `is_plotly_log`, a `Log10Base` axis is a Plotly log axis, which picks its own ticks for the real (not log) values.
The coordinates on such an axis must be given by [`plotly_axis_values`](@ref). The `range` is still the scaled (log)
range.

The axis' tick labels and title are added to the `sides_content`, which the margins are computed from. The labels are
the `ticks_labels`, or else the widest Plotly could pick for the `labels_range`. This is the `range` by default. When
Plotly picks the range itself (no `range`), it must be given as the widest range Plotly may pick. Plotly draws the title
just beyond the labels.
"""
function set_layout_axis!(
    layout::Layout,
    axis::Maybe{AbstractString},
    axis_configuration::AxisConfiguration;
    sides_content::SidesContent,
    title::Maybe{AbstractString} = nothing,
    range::Maybe{Range} = nothing,
    labels_range::Maybe{Range} = range,
    ticks_labels::Maybe{AbstractVector{<:AbstractString}} = nothing,
    ticks_values::Maybe{AbstractVector{<:Real}} = nothing,
    domain::Maybe{AbstractVector{<:Real}} = nothing,
    is_tick_axis::Bool = true,
    is_zeroable::Bool = true,
    is_reversed::Bool = false,
    is_plotly_log::Bool = false,
)::Nothing
    @assert axis !== nothing
    show_ticks = is_tick_axis && axis_configuration.show_ticks
    is_log_axis = is_plotly_log && axis_configuration.scale.log_base === Log10Base
    is_vertical_axis = startswith(axis, "y")
    screen_ticks_angle = axis_screen_ticks_angle(axis_configuration.ticks_angle, is_vertical_axis)

    if show_ticks
        if ticks_labels !== nothing
            # Plotly shows the labels only at their values, and the range places these.
            @assert ticks_values !== nothing && labels_range !== nothing && labels_range.maximum > labels_range.minimum
            labels_widths =
                Float64[text_width(label; font_size = PLOTLY_TICK_LABEL_FONT_SIZE) for label in ticks_labels]
            labels_positions = (ticks_values .- labels_range.minimum) ./ (labels_range.maximum - labels_range.minimum)
        else
            @assert labels_range !== nothing
            labels_widths, labels_positions = axis_ticks_candidates(axis_configuration.scale, labels_range)
        end
        if is_reversed
            labels_positions = 1 .- labels_positions
        end
        domain_start, domain_end = domain === nothing ? (0, 1) : (domain[1], domain[2])
        labels_positions = domain_start .+ labels_positions .* (domain_end - domain_start)
        labels_height = PLOTLY_TICK_LABEL_FONT_SIZE
        labels_gap = is_vertical_axis ? PLOTLY_Y_TICK_LABELS_GAP : PLOTLY_X_TICK_LABELS_GAP
    else
        labels_widths = labels_positions = Float64[]
        labels_height = labels_gap = 0
    end
    add_labels!(
        sides_content;
        widths = labels_widths,
        positions = labels_positions,
        height = labels_height,
        screen_angle = screen_ticks_angle,
        is_vertical_axis,
        gap = labels_gap,
        title_height = title === nothing ? 0 : PLOTLY_AXIS_TITLE_FONT_SIZE,
        standoff = AXIS_TITLE_STANDOFF,
    )

    plotly_axis_title = plotly_title(title)
    if plotly_axis_title !== nothing
        plotly_axis_title[:standoff] = AXIS_TITLE_STANDOFF
    end

    # Plotly runs an axis backwards when its range is given in descending order.
    layout[axis] = Dict(
        :title => plotly_axis_title,
        :automargin => true,
        :type => is_log_axis ? "log" : nothing,
        :range => if range === nothing
            nothing
        elseif is_reversed
            [range.maximum, range.minimum]
        else
            [range.minimum, range.maximum]
        end,
        :showgrid => axis_configuration.show_grid,
        :gridcolor => axis_configuration.show_grid ? axis_configuration.grid_color : nothing,
        :showticklabels => show_ticks,
        :tickprefix => (show_ticks && !is_log_axis) ? axis_ticks_prefix(axis_configuration.scale) : nothing,
        :ticksuffix => show_ticks ? axis_ticks_suffix(axis_configuration.scale) : nothing,
        :exponentformat => (show_ticks && is_log_axis) ? "SI" : nothing,
        :tickvals => show_ticks ? ticks_values : nothing,
        :ticktext => show_ticks ? ticks_labels : nothing,
        :tickangle => (show_ticks && screen_ticks_angle != 0) ? screen_ticks_angle : nothing,
        :zeroline => is_zeroable ? axis_configuration.scale.log_base === nothing : nothing,
        :domain => domain,
    )
    return nothing
end

"""
    set_layout_twin_log_axis!(;
        layout::Layout,
        letter::AbstractString,
        twin_index::Integer,
        axis_configuration::AxisConfiguration,
        title::Maybe{AbstractString},
        range::Range,
        sides_content::SidesContent,
    )::Nothing

Add the 1st Plotly `letter` axis in a `layout` for a `Log10Base` `axis_configuration`, for traces that must be given
the scaled (log) values. This is needed when Plotly computes something from the values (such as a histogram), since it
would compute it from the real values on a Plotly log axis.

The 1st axis is a hidden linear axis, which the traces use. A twin Plotly log axis (at the `twin_index`) is drawn over
it, with the same range, so it shows the ticks of the real values. Plotly only draws it where a trace uses it (see
[`push_twin_log_axis_traces!`](@ref)). The twin axis' labels and title are added to the `sides_content` (see
[`set_layout_axis!`](@ref)).
"""
function set_layout_twin_log_axis!(;
    layout::Layout,
    letter::AbstractString,
    twin_index::Integer,
    axis_configuration::AxisConfiguration,
    title::Maybe{AbstractString},
    range::Range,
    sides_content::SidesContent,
)::Nothing
    @assert axis_configuration.scale.log_base === Log10Base
    @assert letter in ("x", "y")

    axis = plotly_axis(letter, 1)
    set_layout_axis!(layout, axis, axis_configuration; sides_content, range, is_tick_axis = false)  # NOJET
    layout[axis][:showgrid] = false
    layout[axis][:gridcolor] = nothing

    twin_axis = plotly_axis(letter, twin_index)
    set_layout_axis!(layout, twin_axis, axis_configuration; sides_content, title, range, is_plotly_log = true)
    layout[twin_axis][:overlaying] = letter
    # Plotly gives an overlaying axis the ticks of the axis it overlays, unless told to pick its own.
    layout[twin_axis][:tickmode] = "auto"
    layout[twin_axis][:side] = letter == "x" ? "bottom" : "left"
    return nothing
end

"""
    push_twin_log_axis_traces!(
        traces::AbstractVector{GenericTrace};
        letter::AbstractString,
        twin_index::Integer,
        n_cross_axes::Integer = 1,
    )::Nothing

Push to the `traces` an empty trace on the twin Plotly log axis (see [`set_layout_twin_log_axis!`](@ref)) for each of
the `n_cross_axes` of the other letter, since Plotly only draws an axis (and its grid) in the sub-graphs where a trace
uses it.
"""
function push_twin_log_axis_traces!(
    traces::AbstractVector{GenericTrace};
    letter::AbstractString,
    twin_index::Integer,
    n_cross_axes::Integer = 1,
)::Nothing
    @assert letter in ("x", "y")
    other_letter = letter == "x" ? "y" : "x"
    for cross_axis_index in 1:n_cross_axes
        push!(
            traces,
            scatter(;
                x = Float64[],
                y = Float64[],
                Symbol("$(letter)axis") => plotly_axis(letter, twin_index; short = true),
                Symbol("$(other_letter)axis") =>
                    plotly_axis(other_letter, cross_axis_index; short = true, force = true),
                hoverinfo = "skip",
                showlegend = false,
            ),
        )
    end

    return nothing
end

"""
    plotly_axis(
        prefix::AbstractString,
        index::Maybe{Integer};
        short::Bool = false,
        force::Bool = false,
    )::Maybe{AbstractString}

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

# A part of the side panel (a legend or a color bar), before it is placed: its `width` in pixels, its `height` as a
# fraction of the plot's height, the (1-based) `column` it is pinned to (if any), and its `order` in its column (from
# the top; ties keep the order of the parts).
@kwdef struct SidePanelPart
    width::Real
    height::Real
    column::Maybe{Integer} = nothing
    order::Real
end

# Something to place in the side panel: the Plotly `legend` of this name, or the color bar of the Plotly `color_axis`
# of this name, with its `title` (if any), and its `part` for packing it.
@kwdef struct SidePanelItem  # NOLINT
    part::SidePanelPart
    legend::Maybe{AbstractString} = nothing
    color_axis::Maybe{AbstractString} = nothing
    title::Maybe{AbstractString}
end

"""
    set_layout_colorscale!(;
        layout::Layout,
        colors_scale_index::Integer,
        colors_configuration::ColorsConfiguration,
        scaled_colors_palette::Maybe{AbstractVector{<:Tuple{Real, AbstractString}}},
        range::Maybe{Range} = nothing,
        ticks_range::Maybe{Range} = range,
        title::Maybe{AbstractString},
        show_scale::Bool,
        figure_configuration::FigureConfiguration,
        side_panel_items::AbstractVector{SidePanelItem},
        sides_content::SidesContent,
    )::Nothing

Set a `colorscale` in a Plotly `layout`, as specified by a `colors_configuration`. A shown scale is a color bar, which
is pushed to the `side_panel_items` to be placed by [`place_side_panel!`](@ref).

A Plotly color bar shows the ticks of its (scaled) values. For a `Log10Base` scale these are the log values, so a shown
`Log10Base` scale gets its own ticks instead, which show the real values. They are chosen in the `ticks_range`, which
must be given, the way Plotly chooses the ticks of a log axis.

A color bar's length is a fraction of the plot's area, so its ticks depend on the plot's height. This must therefore be
called after everything above and below the plot's area was added to the `sides_content`.
"""
function set_layout_colorscale!(;
    layout::Layout,
    colors_scale_index::Integer,
    colors_configuration::ColorsConfiguration,
    scaled_colors_palette::Maybe{AbstractVector{<:Tuple{Real, AbstractString}}},
    range::Maybe{Range} = nothing,
    ticks_range::Maybe{Range} = range,
    title::Maybe{AbstractString},
    show_scale::Bool,
    figure_configuration::FigureConfiguration,
    side_panel_items::AbstractVector{SidePanelItem},
    sides_content::SidesContent,
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

    color_axis = plotly_axis("color", colors_scale_index)
    colorbar = nothing
    if show_scale
        @assert ticks_range !== nothing
        colorbar = Dict{Symbol, Any}(:title => plotly_title(title))
        if colors_configuration.scale.log_base === Log10Base
            ticks_values, ticks_labels = log10_plotly_ticks(
                ticks_range,
                colors_scale_n_ticks(colors_configuration, figure_configuration, sides_content),
            )
            ticks_suffix = axis_ticks_suffix(colors_configuration.scale)
            colorbar[:tickmode] = "array"
            colorbar[:tickvals] = ticks_values
            colorbar[:ticktext] = ticks_suffix === nothing ? ticks_labels : ticks_labels .* ticks_suffix
        else
            colorbar[:tickprefix] = axis_ticks_prefix(colors_configuration.scale)
            colorbar[:ticksuffix] = axis_ticks_suffix(colors_configuration.scale)
        end
        push!(
            side_panel_items,
            colors_scale_side_panel_item(;
                color_axis,
                colors_configuration,
                ticks_range,
                title,
                figure_configuration,
                sides_content,
            ),
        )
    end

    layout[color_axis] = Dict{Symbol, Any}(
        :showscale => show_scale,
        :colorscale => colorscale,
        :cmin => range === nothing ? nothing : range.minimum,
        :cmax => range === nothing ? nothing : range.maximum,
        :colorbar => colorbar,
    )

    return nothing
end

# Plotly aims at a tick per this many pixels along a vertical axis.
const PLOTLY_VERTICAL_PIXELS_PER_TICK = 40

# The ticks of a `Log10Base` scale whose (log) values are in the `scaled_range`, when Plotly aims at `n_ticks` ticks:
# the log values of the ticks, and their labels, which show the real values (with SI suffixes). They are chosen the way
# plotly.js 4.1.1 chooses the ticks of a log axis (`calcTicks` and `autoTicks`), from the same candidates as the legend
# of sizes. We give these ticks to the color bar of a `Log10Base` color scale.
function log10_plotly_ticks(scaled_range::Range, n_ticks::Real)::Tuple{Vector{Float64}, Vector{String}}
    scaled_minimum = scaled_range.minimum
    scaled_maximum = scaled_range.maximum
    decades = scaled_maximum - scaled_minimum
    rough_step = decades / Float64(n_ticks)  # In `Float64` like Plotly, which matters at the thresholds.
    are_digits = false

    if decades == 0
        chosen = [(Float64(scaled_minimum), single_value_places(exp10(scaled_minimum)))]

    elseif rough_step > 0.7
        # Every k-th power of 10. The candidates are the digits, then 1, 2 and 5, then every power, every 2nd, etc.
        candidates = log10_candidates(scaled_minimum, scaled_maximum)
        chosen = candidates[min(2 + ceil(Int, rough_step), length(candidates))]

    elseif decades < 1
        # Linear steps of the real values, of 2, 5 or 10 times a power of 10 (rounding up, as Plotly's `roundUp`).
        linear_minimum = exp10(scaled_minimum)
        linear_maximum = exp10(scaled_maximum)
        rough_linear_step = (linear_maximum - linear_minimum) / (1.5 * n_ticks)
        power = floor(Int, log10(rough_linear_step))
        multiplier = rough_linear_step / exp10(power)
        step = (multiplier < 2 ? 2 : multiplier < 5 ? 5 : 10) * exp10(power)
        places = max(0, -floor(Int, log10(step)))
        first_index = ceil(Int, linear_minimum / step - 1e-9)
        last_index = floor(Int, linear_maximum / step + 1e-9)
        chosen = Tuple{Float64, Int}[(log10(index * step), places) for index in first_index:last_index]

    else
        # Every digit, or only 1, 2 and 5, in each decade.
        chosen = log10_candidates(scaled_minimum, scaled_maximum)[rough_step > 0.3 ? 2 : 1]
        are_digits = true
    end

    scale_configuration = ScaleConfiguration(; log_base = Log10Base)
    ticks_values = Float64[scaled_value for (scaled_value, _) in chosen]
    ticks_labels = String[]
    for (scaled_value, places) in chosen
        # As Plotly does, digits other than the powers of 10 are labeled by just the digit, in a smaller font.
        if are_digits && mod(scaled_value + 0.01, 1) >= 0.1
            digit = round(Int, exp10(mod(scaled_value, 1)))
            push!(ticks_labels, minor_log_tick_label(digit))
        else
            push!(ticks_labels, scale_value_label(scale_configuration, exp10(scaled_value), places))
        end
    end
    return (ticks_values, ticks_labels)
end

# The font size of the labels of the digits between the powers of 10 on a log axis (75% of the 12 pixels of the rest,
# as Plotly draws them).
const MINOR_LOG_TICK_FONT_SIZE = 9

# The text before and after the label of a digit between the powers of 10 on a log axis.
const MINOR_LOG_TICK_PREFIX = "<span style=\"font-size:$(MINOR_LOG_TICK_FONT_SIZE)px\">"
const MINOR_LOG_TICK_SUFFIX = "</span>"

# The label of a `digit` between the powers of 10 on a log axis.
function minor_log_tick_label(digit::Integer)::String
    return MINOR_LOG_TICK_PREFIX * string(digit) * MINOR_LOG_TICK_SUFFIX
end

# The width in pixels of a tick label of a color bar, at its own font size (a digit between the powers of 10 on a log
# axis is smaller).
function tick_label_pixel_width(label::AbstractString)::Float64
    if startswith(label, MINOR_LOG_TICK_PREFIX)
        text = replace(label, MINOR_LOG_TICK_PREFIX => "", MINOR_LOG_TICK_SUFFIX => "")
        return text_width(text; font_size = MINOR_LOG_TICK_FONT_SIZE)
    else
        return text_width(label; font_size = PLOTLY_LEGEND_FONT_SIZE)
    end
end

# Where a part of the side panel is placed: its (1-based) `column`, the `x_offset` in pixels from the plot's right edge
# to its left edge, and the paper `y_top` fraction of its top.
@kwdef struct SidePanelPlace
    column::Int
    x_offset::Float64
    y_top::Float64
end

# The most parts of the side panel for which every split of them into columns is searched.
const SIDE_PANEL_EXACT_PARTS = 12

# How much floating point noise is ignored when comparing layouts and heights.
const SIDE_PANEL_EPSILON = 1e-9

# Pack the `parts` of the side panel into columns, and return the place of each part (in the same order) and the width
# of the whole panel in pixels. Each non-empty column takes the `horizontal_gap` (pixels) and the width of its widest
# part; an empty column (left by the pins) takes no space. The parts of a column, with the `vertical_gap` (a fraction of
# the plot's height) between them, fit in its height (unless pinned there). They are placed from the top, by their
# `order`. The columns without pinned parts are placed from the left by the smallest `order` of their parts. The split
# into columns has the least total wasted area (the narrowest panel), then the least sum of squared waste per column
# (the most even), then the fewest columns. It is found by searching every split, unless there are too many parts, and
# then by placing the tallest parts first, each in the first column with room.
function pack_side_panel_parts(
    parts::AbstractVector{SidePanelPart};
    horizontal_gap::Real,
    vertical_gap::Real,
)::Tuple{Vector{SidePanelPlace}, Float64}
    if isempty(parts)
        return (SidePanelPlace[], 0.0)
    end

    if length(parts) <= SIDE_PANEL_EXACT_PARTS
        column_per_part = best_side_panel_columns(parts, vertical_gap)
    else
        column_per_part = first_fit_side_panel_columns(parts, vertical_gap)
    end

    order_side_panel_columns!(column_per_part, parts)
    return side_panel_places(parts, column_per_part, horizontal_gap, vertical_gap)
end

# Order the columns which hold no pinned part from left to right by the smallest `order` of their parts (ties by their
# first part). They take the places of these columns between the pinned ones.
function order_side_panel_columns!(
    column_per_part::AbstractVector{C},
    parts::AbstractVector{SidePanelPart},
)::Nothing where {C <: Integer}
    pinned_columns = Set(part.column for part in parts if part.column !== nothing)
    free_columns = sort(C[column for column in unique(column_per_part) if !(column in pinned_columns)])
    keys_of_free_columns = Tuple{Real, Int}[
        (
            minimum(part.order for (part, part_column) in zip(parts, column_per_part) if part_column == column),
            findfirst(==(column), column_per_part),
        ) for column in free_columns
    ]
    ordered_free_columns = free_columns[sortperm(keys_of_free_columns)]
    new_column_per_old_column = Dict(zip(ordered_free_columns, free_columns))
    column_per_part .= C[get(new_column_per_old_column, column, column) for column in column_per_part]
    return nothing
end

# The columns of the pinned parts (and zero for the others), and how many columns they take.
function pinned_side_panel_columns(parts::AbstractVector{SidePanelPart})::Tuple{Vector{Int}, Int}
    column_per_part = Int[part.column === nothing ? 0 : Int(part.column) for part in parts]
    return (column_per_part, maximum(column_per_part))
end

# The height of a column holding parts of these `heights` with the `vertical_gap` between them.
function side_panel_column_height(heights::AbstractVector{<:Real}, vertical_gap::Real)::Float64
    return isempty(heights) ? 0.0 : sum(heights) + vertical_gap * (length(heights) - 1)
end

# The ranking of a split into columns: the total wasted area, the sum of the squared waste per column, and the number of
# (non-empty) columns, to be minimized in this order.
function side_panel_ranking(
    parts::AbstractVector{SidePanelPart},
    column_per_part::AbstractVector{<:Integer},
)::Tuple{Float64, Float64, Int}
    total_waste = 0.0
    total_squared_waste = 0.0
    n_columns = 0
    for column in unique(column_per_part)
        column_parts = parts[column_per_part .== column]
        column_width = maximum(part.width for part in column_parts)
        waste = column_width - sum(part.width * part.height for part in column_parts)
        total_waste += waste
        total_squared_waste += waste^2
        n_columns += 1
    end
    return (total_waste, total_squared_waste, n_columns)
end

# Whether the `ranking` is better than the `best_ranking` (ignoring floating point noise).
function is_better_side_panel_ranking(ranking::Tuple, best_ranking::Maybe{Tuple})::Bool
    if best_ranking === nothing
        return true
    end
    for (value, best_value) in zip(ranking, best_ranking)
        if value < best_value - SIDE_PANEL_EPSILON
            return true
        elseif value > best_value + SIDE_PANEL_EPSILON
            return false
        end
    end
    return false
end

# The state of the search for the best split of the parts of the side panel into columns: the column of each part so
# far (zero for an unplaced one), and the best split found so far with its ranking.
mutable struct SidePanelSearch
    parts::AbstractVector{SidePanelPart}
    vertical_gap::Float64
    unpinned_indices::Vector{Int}
    total_area::Float64
    column_per_part::Vector{Int}
    best_column_per_part::Maybe{Vector{Int}}
    best_ranking::Maybe{Tuple{Float64, Float64, Int}}
end

# The column of each part in the best split, searching every split.
function best_side_panel_columns(parts::AbstractVector{SidePanelPart}, vertical_gap::Real)::Vector{Int}
    column_per_part, n_pinned_columns = pinned_side_panel_columns(parts)
    search = SidePanelSearch(
        parts,
        vertical_gap,
        findall(==(0), column_per_part),
        sum(part.width * part.height for part in parts),
        column_per_part,
        nothing,
        nothing,
    )
    search_side_panel_columns!(search, 1, n_pinned_columns)
    best_column_per_part = search.best_column_per_part
    @assert best_column_per_part !== nothing
    return best_column_per_part
end

# Place the unpinned parts from the `next_unpinned` one on, given the `n_columns` used so far. Each goes in turn into
# any column with room for it, or into a new column after the last.
function search_side_panel_columns!(search::SidePanelSearch, next_unpinned::Integer, n_columns::Integer)::Nothing
    parts = search.parts
    column_per_part = search.column_per_part

    # Adding parts never narrows a column, so the waste of the columns so far is a lower bound of the final waste.
    best_ranking = search.best_ranking
    if best_ranking !== nothing
        partial_width = sum(
            maximum(part.width for (part, part_column) in zip(parts, column_per_part) if part_column == column) for
            column in unique(column_per_part) if column != 0;
            init = 0.0,
        )
        if partial_width - search.total_area > best_ranking[1] + SIDE_PANEL_EPSILON
            return nothing
        end
    end

    if next_unpinned > length(search.unpinned_indices)
        ranking = side_panel_ranking(parts, column_per_part)
        if is_better_side_panel_ranking(ranking, search.best_ranking)
            search.best_ranking = ranking
            search.best_column_per_part = copy(column_per_part)
        end
        return nothing
    end

    part_index = search.unpinned_indices[next_unpinned]
    for column in 1:(n_columns + 1)
        heights = Real[parts[index].height for index in eachindex(parts) if column_per_part[index] == column]
        push!(heights, parts[part_index].height)
        if side_panel_column_height(heights, search.vertical_gap) <= 1 + SIDE_PANEL_EPSILON
            column_per_part[part_index] = column
            search_side_panel_columns!(search, next_unpinned + 1, max(n_columns, column))
            column_per_part[part_index] = 0
        end
    end
    return nothing
end

# The column of each part, placing the tallest unpinned parts first, each in the first column with room for it.
function first_fit_side_panel_columns(parts::AbstractVector{SidePanelPart}, vertical_gap::Real)::Vector{Int}
    column_per_part, n_columns = pinned_side_panel_columns(parts)
    unpinned_indices = findall(==(0), column_per_part)
    sort!(unpinned_indices; by = index -> -parts[index].height, alg = MergeSort)
    for part_index in unpinned_indices
        for column in 1:(n_columns + 1)
            heights = Real[parts[index].height for index in eachindex(parts) if column_per_part[index] == column]
            push!(heights, parts[part_index].height)
            if side_panel_column_height(heights, vertical_gap) <= 1 + SIDE_PANEL_EPSILON
                column_per_part[part_index] = column
                n_columns = max(n_columns, column)
                break
            end
        end
    end
    return column_per_part
end

# The place of each part given its column, and the width of the whole panel.
function side_panel_places(
    parts::AbstractVector{SidePanelPart},
    column_per_part::AbstractVector{<:Integer},
    horizontal_gap::Real,
    vertical_gap::Real,
)::Tuple{Vector{SidePanelPlace}, Float64}
    x_offset_per_column = Dict{Int, Float64}()
    panel_width = 0.0
    for column in sort(unique(column_per_part))
        x_offset_per_column[column] = panel_width + horizontal_gap
        panel_width +=
            horizontal_gap +
            maximum(part.width for (part, part_column) in zip(parts, column_per_part) if part_column == column)
    end

    # The parts of each column are stacked from the top by their `order` (the sort is stable, so ties keep their order).
    y_top_per_column = Dict(column => 1.0 for column in keys(x_offset_per_column))
    places = Vector{SidePanelPlace}(undef, length(parts))
    for part_index in sort(collect(eachindex(parts)); by = index -> parts[index].order, alg = MergeSort)
        column = column_per_part[part_index]
        places[part_index] =
            SidePanelPlace(; column, x_offset = x_offset_per_column[column], y_top = y_top_per_column[column])
        y_top_per_column[column] -= parts[part_index].height + vertical_gap
    end

    return (places, panel_width)
end

# The sizes of a Plotly legend, in pixels, as measured in Kaleido's renders: its text and title font sizes, the width
# before a label (the symbol) and after it (the padding, rounded up), the width beyond a wider title, and the height of
# a row and of the padding.
const PLOTLY_LEGEND_FONT_SIZE = 12
const PLOTLY_LEGEND_TITLE_FONT_SIZE = 14
const PLOTLY_LEGEND_LABEL_EXTRA_WIDTH = 46
const PLOTLY_LEGEND_TITLE_EXTRA_WIDTH = 6
const PLOTLY_LEGEND_ROW_HEIGHT = 19
const PLOTLY_LEGEND_EXTRA_HEIGHT = 10

# The sizes of a Plotly color bar, in pixels: its thickness, the width beyond its widest tick label, and the width
# beyond a wider title.
const PLOTLY_COLORS_SCALE_THICKNESS = 30
const PLOTLY_COLORS_SCALE_TICKS_EXTRA_WIDTH = 2
const PLOTLY_COLORS_SCALE_TITLE_EXTRA_WIDTH = 2

# The width in pixels of a Plotly legend of entries with these `labels` and the `title` (if any).
function legend_pixel_width(labels::AbstractVector{<:AbstractString}; title::Maybe{AbstractString})::Float64
    width =
        maximum(text_width(label; font_size = PLOTLY_LEGEND_FONT_SIZE) for label in labels; init = 0.0) +
        PLOTLY_LEGEND_LABEL_EXTRA_WIDTH
    if title !== nothing
        width =
            max(width, text_width(title; font_size = PLOTLY_LEGEND_TITLE_FONT_SIZE) + PLOTLY_LEGEND_TITLE_EXTRA_WIDTH)
    end
    return width
end

# The natural height in pixels of a Plotly legend of `n_entries` (with a title row if it `has_title`).
function legend_pixel_height(n_entries::Integer; has_title::Bool)::Float64
    return PLOTLY_LEGEND_ROW_HEIGHT * (n_entries + Int(has_title)) + PLOTLY_LEGEND_EXTRA_HEIGHT
end

# The width in pixels of a Plotly color bar with these `tick_labels` and the `title` (if any).
function colors_scale_pixel_width(tick_labels::AbstractVector{<:AbstractString}; title::Maybe{AbstractString})::Float64
    width =
        PLOTLY_COLORS_SCALE_THICKNESS +
        maximum(tick_label_pixel_width(label) for label in tick_labels; init = 0.0) +
        PLOTLY_COLORS_SCALE_TICKS_EXTRA_WIDTH
    if title !== nothing
        width = max(
            width,
            text_width(title; font_size = PLOTLY_LEGEND_TITLE_FONT_SIZE) + PLOTLY_COLORS_SCALE_TITLE_EXTRA_WIDTH,
        )
    end
    return width
end

# The text of a prefix or a suffix of ticks, without its HTML tags (if any).
function plain_ticks_affix(affix::Maybe{AbstractString})::String
    return affix === nothing ? "" : replace(affix, r"<[^>]*>" => "")
end

# The ticks Plotly draws for a `scale` whose (scaled) values are in the `scaled_range`, when it aims at `n_ticks` ticks:
# their scaled values, and their labels as far as their widths are concerned. This holds for a color bar and for an
# axis. For a `Log10Base` scale they follow Plotly's rules for a log axis (see `log10_plotly_ticks`). For the rest,
# Plotly picks the ticks of a linear axis (a step of 2, 5 or 10 times a power of 10), and each label is the wider of its
# plain and its SI form, since the form Plotly picks isn't known. Negative values have Plotly's minus sign.
function plotly_ticks(
    scale::ScaleConfiguration,
    scaled_range::Range,
    n_ticks::Real,
)::Tuple{Vector{Float64}, Vector{String}}
    suffix = plain_ticks_affix(axis_ticks_suffix(scale))

    if scale.log_base === Log10Base
        log10_values, log10_labels = log10_plotly_ticks(scaled_range, n_ticks)
        return (log10_values, String[label * suffix for label in log10_labels])
    end

    prefix = plain_ticks_affix(axis_ticks_prefix(scale))
    span = scaled_range.maximum - scaled_range.minimum
    if span == 0
        values = [Float64(scaled_range.minimum)]
        places = single_value_places(scaled_range.minimum)
    else
        rough_step = span / Float64(n_ticks)  # In `Float64` like Plotly, which matters at the thresholds.
        power = floor(Int, log10(rough_step))
        multiplier = rough_step / exp10(power)
        step = (multiplier < 2 ? 2 : multiplier < 5 ? 5 : 10) * exp10(power)
        places = max(0, -floor(Int, log10(step)))
        first_index = ceil(Int, scaled_range.minimum / step - 1e-9)
        last_index = floor(Int, scaled_range.maximum / step + 1e-9)
        values = Float64[index * step for index in first_index:last_index]
    end

    si_scale = ScaleConfiguration(; log_base = Log10Base)
    labels = String[]
    for value in values
        plain = decimal_text(value, places)
        si = abs(value) >= 1000 ? scale_value_label(si_scale, value, places) : plain
        label = text_width(si) > text_width(plain) ? si : plain
        push!(labels, prefix * replace(label, "-" => "−") * suffix)
    end
    return (Float64.(values), labels)
end

# The labels of the ticks Plotly draws (see `plotly_ticks`).
function plotly_tick_labels(scale::ScaleConfiguration, scaled_range::Range, n_ticks::Real)::Vector{String}
    return plotly_ticks(scale, scaled_range, n_ticks)[2]
end

# Every tick label Plotly could draw on an axis of the `scale` whose (scaled) values are in the `scaled_range`, whatever
# the axis' length: the width of each in pixels, and its position as a fraction of the range (0 at its minimum). Plotly
# aims at between 5 and 10 ticks (one per 40 pixels of a vertical axis, or 80 of a horizontal one, clamped to 4..9,
# plus 1). Every step it can pick for a count in that range, it also picks for a whole count, so trying those covers
# them all.
function axis_ticks_candidates(scale::ScaleConfiguration, scaled_range::Range)::Tuple{Vector{Float64}, Vector{Float64}}
    span = scaled_range.maximum - scaled_range.minimum
    widths = Float64[]
    positions = Float64[]
    for n_ticks in 5:10
        values, labels = plotly_ticks(scale, scaled_range, n_ticks)
        append!(widths, tick_label_pixel_width.(labels))
        append!(positions, span == 0 ? fill(0.5, length(values)) : (values .- scaled_range.minimum) ./ span)
    end
    return (widths, positions)
end

# How far a label reaches from its tick, in pixels: `depth` away from the axis, and `before` and `after` the tick
# along the axis (left and right of an x tick, above and below a y tick).
@kwdef struct LabelExtent
    depth::Float64
    before::Float64
    after::Float64
end

# The extent of a label `width` pixels wide and `height` tall, drawn at the `screen_angle` (between -90 and 90) beside
# an axis (vertical if `is_vertical_axis`), anchored at its tick the way Plotly anchors tick labels. A horizontal or
# vertical label is centered on its tick. A label which rises to the right (a negative angle) ends at its tick, so it
# hangs to the left of an x tick and below a y tick. One which falls to the right starts at its tick, so it hangs to the
# right of an x tick and above a y tick.
function rotated_label_extent(; width::Real, height::Real, screen_angle::Real, is_vertical_axis::Bool)::LabelExtent
    @assert -90 <= screen_angle <= 90
    cosine = abs(cosd(screen_angle))
    sine = abs(sind(screen_angle))
    box_width = width * cosine + height * sine
    box_height = width * sine + height * cosine

    if is_vertical_axis
        depth = box_width
        along = box_height
    else
        depth = box_height
        along = box_width
    end

    if -90 < screen_angle < 0
        before, after = is_vertical_axis ? (0.0, along) : (along, 0.0)
    elseif 0 < screen_angle < 90
        before, after = is_vertical_axis ? (along, 0.0) : (0.0, along)
    else
        before = after = along / 2
    end

    return LabelExtent(; depth, before, after)
end

# Grow the `sides_content` to hold a group of labels beside a vertical (left) or horizontal (bottom) axis: each of the
# `widths` wide, at its position (a fraction of the plot's area, 0 at its bottom or left end), all `height` tall at the
# `screen_angle`, `gap` pixels from the axis, followed by a title `title_height` tall (0 for none) `standoff` pixels
# beyond them. A label near an end of the axis may reach past it, into the side next to it, by its reach along the axis
# less its distance from that end (see `SidesContent`).
function add_labels!(
    sides_content::SidesContent;
    widths::AbstractVector{<:Real},
    positions::AbstractVector{<:Real},
    height::Real,
    screen_angle::Real,
    is_vertical_axis::Bool,
    gap::Real,
    title_height::Real = 0,
    standoff::Real = 0,
)::Nothing
    @assert length(widths) == length(positions)
    least_length = is_vertical_axis ? sides_content.least_plot_height : sides_content.least_plot_width
    depth = 0.0
    before = 0.0
    after = 0.0
    for (width, position) in zip(widths, positions)
        extent = rotated_label_extent(; width, height, screen_angle, is_vertical_axis)
        depth = max(depth, extent.depth)
        # Before the tick is to its left on an x axis, and above it on a y axis, where the axis runs up from 0.
        before_distance = (is_vertical_axis ? 1 - position : position) * least_length
        after_distance = (is_vertical_axis ? position : 1 - position) * least_length
        before = max(before, extent.before - before_distance)
        after = max(after, extent.after - after_distance)
    end

    depth += gap
    if title_height > 0
        depth += standoff + title_height
    end
    if is_vertical_axis
        sides_content.left = max(sides_content.left, depth)
        sides_content.top = max(sides_content.top, before)
        sides_content.bottom = max(sides_content.bottom, after)
    else
        sides_content.bottom = max(sides_content.bottom, depth)
        sides_content.left = max(sides_content.left, before)
        sides_content.right = max(sides_content.right, after)
    end
    return nothing
end

# The default order in a column of the side panel of each kind of part (see `LegendConfiguration`).
const CATEGORICAL_LEGEND_ORDER = 1  # NOLINT
const COLORS_SCALE_ORDER = 2
const SIZES_LEGEND_ORDER = 3  # NOLINT

# The default height of a color bar, as a fraction of the plot's height.
const COLORS_SCALE_DEFAULT_HEIGHT = 0.5

# How far down our title label of a legend is from the legend's top, in pixels, to be exactly where Plotly draws its
# own title (measured in Kaleido's renders).
const LEGEND_TITLE_Y_SHIFT = -5

# The height in pixels of the plot's area: the figure's height (or Plotly's default), less the top and bottom margins.
# These are the configured margins plus the `sides_content` above and below the plot's area, which must be complete.
function plot_pixel_height(figure_configuration::FigureConfiguration, sides_content::SidesContent)::Float64
    return prefer_data(figure_configuration.height, PLOTLY_DEFAULT_HEIGHT) - figure_configuration.margins.top -
           sides_content.top - figure_configuration.margins.bottom - sides_content.bottom
end

# The number of ticks Plotly aims at on the color bar of the `colors_configuration` (from its length in pixels).
function colors_scale_n_ticks(
    colors_configuration::ColorsConfiguration,
    figure_configuration::FigureConfiguration,
    sides_content::SidesContent,
)::Float64
    pixel_length =
        prefer_data(colors_configuration.legend.height, COLORS_SCALE_DEFAULT_HEIGHT) *
        plot_pixel_height(figure_configuration, sides_content)
    return clamp(pixel_length / PLOTLY_VERTICAL_PIXELS_PER_TICK, 4, 9) + 1
end

# The side panel item of a Plotly `legend` of entries with these `labels` and the `title` (if any), configured by the
# `legend_configuration` (if any), whose order in its column is the `default_order` unless configured otherwise.
function legend_side_panel_item(;
    legend::AbstractString,
    labels::AbstractVector{<:AbstractString},
    title::Maybe{AbstractString},
    legend_configuration::Maybe{LegendConfiguration} = nothing,
    default_order::Real,
    figure_configuration::FigureConfiguration,
    sides_content::SidesContent,
)::SidePanelItem
    natural_height =
        legend_pixel_height(length(labels); has_title = title !== nothing) /
        plot_pixel_height(figure_configuration, sides_content)
    part = SidePanelPart(;
        width = something(
            legend_configuration === nothing ? nothing : legend_configuration.width,
            legend_pixel_width(labels; title),
        ),
        height = something(
            legend_configuration === nothing ? nothing : legend_configuration.height,
            min(natural_height, 1.0),
        ),
        column = legend_configuration === nothing ? nothing : legend_configuration.column,
        order = something(legend_configuration === nothing ? nothing : legend_configuration.order, default_order),
    )
    return SidePanelItem(; part, legend, title)
end

# The side panel item of the color bar of the Plotly `color_axis` of a `colors_configuration`, whose (scaled) values are
# in the `ticks_range`, with the `title` (if any).
function colors_scale_side_panel_item(;
    color_axis::AbstractString,
    colors_configuration::ColorsConfiguration,
    ticks_range::Range,
    title::Maybe{AbstractString},
    figure_configuration::FigureConfiguration,
    sides_content::SidesContent,
)::SidePanelItem
    legend_configuration = colors_configuration.legend
    tick_labels = plotly_tick_labels(
        colors_configuration.scale,
        ticks_range,
        colors_scale_n_ticks(colors_configuration, figure_configuration, sides_content),
    )
    part = SidePanelPart(;
        width = something(legend_configuration.width, colors_scale_pixel_width(tick_labels; title)),
        height = something(legend_configuration.height, COLORS_SCALE_DEFAULT_HEIGHT),
        column = legend_configuration.column,
        order = something(legend_configuration.order, COLORS_SCALE_ORDER),
    )
    return SidePanelItem(; part, color_axis, title)
end

"""
    place_side_panel!(
        layout::Layout,
        figure_configuration::FigureConfiguration,
        items::AbstractVector{SidePanelItem};
        sides_content::SidesContent,
    )::Nothing

Place the legends and color bars of the `items` in the side panel to the right of the plot, as described in
[`FigureConfiguration`](@ref), and add its width to the right of the `sides_content`. This must be called after
everything else was added to the `layout`, and before [`set_layout_margins!`](@ref).

Plotly can't place a legend at a pixel offset, so a legend is anchored at the plot's right edge, with its entries
indented to its column, and a transparent background. Plotly doesn't indent the legend's title, so this is drawn as
a text label above the legend's (transparent) title. A color bar is placed at its column by its padding.
"""
function place_side_panel!(
    layout::Layout,
    figure_configuration::FigureConfiguration,
    items::AbstractVector{SidePanelItem};
    sides_content::SidesContent,
)::Nothing
    if isempty(items)
        return nothing
    end

    places, panel_width = pack_side_panel_parts(
        SidePanelPart[item.part for item in items];
        horizontal_gap = figure_configuration.legends_gap,
        vertical_gap = figure_configuration.legends_gap / plot_pixel_height(figure_configuration, sides_content),
    )
    sides_content.right = max(sides_content.right, panel_width)

    if !haskey(layout.fields, :annotations)
        layout[:annotations] = []
    end
    annotations = layout[:annotations]

    for (item, place) in zip(items, places)
        if item.legend !== nothing
            legend = layout[item.legend]
            legend[:x] = 1
            legend[:xanchor] = "left"
            legend[:y] = place.y_top
            legend[:yanchor] = "top"
            legend[:indentation] = place.x_offset
            legend[:bgcolor] = NO_COLOR
            legend[:maxheight] = item.part.height
            if item.title !== nothing
                legend[:title] = Dict(:text => item.title, :font => Dict(:color => NO_COLOR))
                push!(
                    annotations,
                    Dict(
                        :text => item.title,
                        :showarrow => false,
                        :x => 1,
                        :xref => "paper",
                        :xanchor => "left",
                        :xshift => place.x_offset,
                        :y => place.y_top,
                        :yref => "paper",
                        :yanchor => "top",
                        :yshift => LEGEND_TITLE_Y_SHIFT,
                        :font => Dict(:size => PLOTLY_LEGEND_TITLE_FONT_SIZE),
                    ),
                )
            end
        else
            @assert item.color_axis !== nothing
            colorbar = layout[item.color_axis][:colorbar]
            colorbar[:x] = 1
            colorbar[:xanchor] = "left"
            colorbar[:xpad] = place.x_offset
            colorbar[:y] = place.y_top
            colorbar[:yanchor] = "top"
            colorbar[:ypad] = 0
            colorbar[:len] = item.part.height
            colorbar[:lenmode] = "fraction"
            colorbar[:thickness] = PLOTLY_COLORS_SCALE_THICKNESS
        end
    end

    return nothing
end

"""
    set_layout_margins!(
        layout::Layout,
        figure_configuration::FigureConfiguration,
        sides_content::SidesContent,
    )::Nothing

Set each margin of the `layout` to how far the `sides_content` reaches on that side, plus that side's margin in the
`figure_configuration`. This must be called last, when the `sides_content` is complete.
"""
function set_layout_margins!(
    layout::Layout,
    figure_configuration::FigureConfiguration,
    sides_content::SidesContent,
)::Nothing
    margins = figure_configuration.margins
    layout[:margin][:l] = margins.left + ceil(Int, sides_content.left)
    layout[:margin][:r] = margins.right + ceil(Int, sides_content.right)
    layout[:margin][:t] = margins.top + ceil(Int, sides_content.top)
    layout[:margin][:b] = margins.bottom + ceil(Int, sides_content.bottom)
    return nothing
end

# The size of the plot's area in pixels which the configured margins leave in the figure (or in Plotly's default size),
# before the margins grow to hold the content of their sides: its width and its height.
function configured_plot_pixel_size(figure_configuration::FigureConfiguration)::Tuple{Float64, Float64}
    margins = figure_configuration.margins
    return (
        prefer_data(figure_configuration.width, PLOTLY_DEFAULT_WIDTH) - margins.left - margins.right,
        prefer_data(figure_configuration.height, PLOTLY_DEFAULT_HEIGHT) - margins.top - margins.bottom,
    )
end

# Build a layout twice: first for the plot's size which the configured margins leave in the figure (or in Plotly's
# default size), only to find the final margins; then for the plot's size which these margins leave, which is the
# layout to use. The `build_layout` takes the plot's width and height in pixels.
function two_passes_layout(build_layout::Function, figure_configuration::FigureConfiguration)::Layout
    first_layout = build_layout(configured_plot_pixel_size(figure_configuration)...)
    margin = first_layout[:margin]
    return build_layout(
        prefer_data(figure_configuration.width, PLOTLY_DEFAULT_WIDTH) - margin[:l] - margin[:r],
        prefer_data(figure_configuration.height, PLOTLY_DEFAULT_HEIGHT) - margin[:t] - margin[:b],
    )
end

"""
    plotly_figure(trace::GenericTrace, layout::Layout)::PlotlyFigure
    plotly_figure(traces::AbstractVector{<:GenericTrace}, layout::Layout)::PlotlyFigure

Wrap a `trace` or a set of `traces` with the accompanying `layout` in a `PlotlyFigure`.
"""
function plotly_figure(trace::GenericTrace, layout::Layout)::PlotlyFigure
    purge_nulls!(trace.fields)
    purge_nulls!(layout.fields)
    return Plot(trace, layout)
end

function plotly_figure(traces::AbstractVector{<:GenericTrace}, layout::Layout)::PlotlyFigure
    for trace in traces
        purge_nulls!(trace.fields)
    end
    purge_nulls!(layout.fields)
    return Plot(traces, layout)
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
        return pair.second !== nothing &&
               !(pair.second isa AbstractDict && isempty(pair.second)) &&
               !(pair.second isa AbstractVector && (isempty(pair.second) || all(pair.second .=== nothing))) &&
               !(pair.second isa Tuple && all(pair.second .=== nothing))
    end
    return nothing
end

"""
    axis_ticks_prefix(scale_configuration::ScaleConfiguration)::Maybe{AbstractString}

Return the prefix for the ticks of a `scale_configuration`.
"""
function axis_ticks_prefix(scale_configuration::ScaleConfiguration)::Maybe{AbstractString}
    if scale_configuration.log_base == Log10Base
        # A `Log10Base` scale is shown on a Plotly log axis, whose ticks need no prefix.
        @assert false
    elseif scale_configuration.log_base == Log2Base
        return "<sub>2</sub>"
    else
        @assert scale_configuration.log_base === nothing
        return nothing
    end
end

"""
    axis_ticks_suffix(scale_configuration::ScaleConfiguration)::Maybe{AbstractString}

Return the suffix for the ticks of a `scale_configuration`.
"""
function axis_ticks_suffix(scale_configuration::ScaleConfiguration)::Maybe{AbstractString}
    if scale_configuration.percent
        return "<sub>%</sub>"
    else
        return nothing
    end
end

"""
    scale_axis_value(scale_configuration::ScaleConfiguration, value::Real; clamp::Bool = true)::Float64
    scale_axis_value(scale_configuration::ScaleConfiguration, value::Nothing; clamp::Bool = true)::Nothing

Scale a single `value` according to the `scale_configuration`. This deals with log scales and percent scaling. By
default, `clamp` the values to a specified explicit range.
"""
function scale_axis_value(scale_configuration::ScaleConfiguration, value::Real; clamp::Bool = true)::Float64
    if clamp
        if scale_configuration.minimum !== nothing && value < scale_configuration.minimum
            value = scale_configuration.minimum
        end
        if scale_configuration.maximum !== nothing && value > scale_configuration.maximum  # NOJET
            value = scale_configuration.maximum
        end
    end

    if scale_configuration.percent
        scale = 100.0
    else
        scale = 1.0
    end

    offset = scale_configuration.log_regularization

    if scale_configuration.log_base === Log10Base
        return log10((value + offset) * scale)
    elseif scale_configuration.log_base === Log2Base
        return log2((value + offset) * scale)
    else
        @assert scale_configuration.log_base === nothing
        return value * scale + offset
    end
end

function scale_axis_value(::ScaleConfiguration, ::Nothing; clamp::Bool = true)::Nothing  # NOLINT
    return nothing
end

"""
    scale_axis_values(
        scale_configuration::ScaleConfiguration,
        values::AbstractVector{<:Maybe{Real}};
        clamp::Bool = true,
        copy::Bool = false,
    )::AbstractVector{<:Maybe{AbstractFloat}}
    scale_axis_values(
        scale_configuration::ScaleConfiguration,
        values::AbstractMatrix{<:Maybe{Real}};
        clamp::Bool = true,
        copy::Bool = false,
    )::AbstractMatrix{<:Maybe{AbstractFloat}}
    scale_axis_values(
        scale_configuration::ScaleConfiguration,
        values::Nothing;
        clamp::Bool = true,
        copy::Bool = false,
    )::Nothing

Scale a vector (or a matrix) of `values` according to the `scale_configuration`. This deals with log scales and percent
scaling. By default, `clamp` the values to a specified explicit range. If `copy` we always return a copy of the data (so
it can be safely modified further without impacting the original data). If the `values` are `nothing`, so is the
result.
"""
function scale_axis_values(
    ::ScaleConfiguration,
    ::Nothing;
    clamp::Bool = true,  # NOLINT
    copy::Bool = false,  # NOLINT
)::Nothing
    return nothing  # UNTESTED
end

function scale_axis_values(
    scale_configuration::ScaleConfiguration,
    values::AbstractVector{<:Maybe{Real}};
    clamp::Bool = true,
    copy::Bool = false,
)::AbstractVector{<:Maybe{AbstractFloat}}
    if !scale_configuration.percent &&
       scale_configuration.log_base === nothing &&
       scale_configuration.minimum === nothing &&
       scale_configuration.maximum === nothing
        return copy || eltype(values) <: Maybe{Integer} ? floatify.(values) : values
    else
        function scale(value)
            return scale_axis_value(scale_configuration, value; clamp)
        end
        return scale.(values)
    end
end

function scale_axis_values(
    scale_configuration::ScaleConfiguration,
    values::AbstractMatrix{<:Maybe{Real}};
    clamp::Bool = true,
    copy::Bool = false,
)::AbstractMatrix{<:Maybe{AbstractFloat}}
    if !scale_configuration.percent &&
       scale_configuration.log_base === nothing &&
       scale_configuration.minimum === nothing &&
       scale_configuration.maximum === nothing
        return copy || eltype(values) <: Maybe{Integer} ? floatify.(values) : values
    else
        function scale(value)
            return scale_axis_value(scale_configuration, value; clamp)
        end
        return scale.(values)
    end
end

"""
    plotly_axis_value(scale_configuration::ScaleConfiguration, scaled_value::Real; is_plotly_log::Bool)::Real
    plotly_axis_value(scale_configuration::ScaleConfiguration, scaled_value::Nothing; is_plotly_log::Bool)::Nothing

Convert a single `scaled_value` (as returned by [`scale_axis_value`](@ref)) to the coordinate Plotly expects. If
`is_plotly_log`, a `Log10Base` axis is a Plotly log axis (see [`set_layout_axis!`](@ref)), which expects the real (not
log) value. Otherwise, this returns the scaled value.
"""
function plotly_axis_value(scale_configuration::ScaleConfiguration, scaled_value::Real; is_plotly_log::Bool)::Real
    if is_plotly_log && scale_configuration.log_base === Log10Base
        return exp10(scaled_value)
    else
        return scaled_value
    end
end

function plotly_axis_value(::ScaleConfiguration, ::Nothing; is_plotly_log::Bool)::Nothing  # NOLINT
    return nothing
end

"""
    plotly_axis_values(
        scale_configuration::ScaleConfiguration,
        scaled_values::AbstractVector{<:Maybe{Real}};
        is_plotly_log::Bool,
    )::AbstractVector{<:Maybe{Real}}

Convert a vector of `scaled_values` (as returned by [`scale_axis_values`](@ref)) to the coordinates Plotly expects. See
[`plotly_axis_value`](@ref).
"""
function plotly_axis_values(
    scale_configuration::ScaleConfiguration,
    scaled_values::AbstractVector{<:Maybe{Real}};
    is_plotly_log::Bool,
)::AbstractVector{<:Maybe{Real}}
    if is_plotly_log && scale_configuration.log_base === Log10Base
        return plotly_axis_value.(Ref(scale_configuration), scaled_values; is_plotly_log)
    else
        return scaled_values
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
        scale_configuration::ScaleConfiguration
    )::Range
    final_scaled_range(
        implicit_scaled_range::Union{Range, MaybeRange},
        axis_configuration::AxisConfiguration
    )::Range

Compute the final range given the `implicit_scaled_range` computed from the values and the explicit `minimum` and
`maximum` of the `scale_configuration`. Given an `axis_configuration`, the range is also grown by its
`expand_fraction`. Only a drawn axis expands, since the expansion exists to keep markers clear of the plot border.
"""
function final_scaled_range(implicit_scaled_range::MaybeRange, scale_configuration::ScaleConfiguration)::Range
    @assert implicit_scaled_range.minimum !== nothing
    @assert implicit_scaled_range.maximum !== nothing
    return final_scaled_range(  # NOJET
        Range(; minimum = implicit_scaled_range.minimum, maximum = implicit_scaled_range.maximum),
        scale_configuration,
    )
end

function final_scaled_range(implicit_scaled_range::Range, scale_configuration::ScaleConfiguration)::Range
    explicit_scaled_minimum, explicit_scaled_maximum = scale_axis_values(
        scale_configuration,
        [scale_configuration.minimum, scale_configuration.maximum];
        clamp = false,
    )
    explicit_scaled_range = MaybeRange(; minimum = explicit_scaled_minimum, maximum = explicit_scaled_maximum)

    return Range(;
        minimum = prefer_data(explicit_scaled_range.minimum, implicit_scaled_range.minimum),
        maximum = prefer_data(explicit_scaled_range.maximum, implicit_scaled_range.maximum),
    )
end

function final_scaled_range(
    implicit_scaled_range::Union{Range, MaybeRange},
    axis_configuration::AxisConfiguration,
)::Range
    range = final_scaled_range(implicit_scaled_range, axis_configuration.scale)

    if axis_configuration.expand_fraction > 0
        margins = (range.maximum - range.minimum) * axis_configuration.expand_fraction
        range.minimum -= margins
        range.maximum += margins
    end

    return range
end

"""
    validate_numeric_values(
        context::ValidationContext,
        field::AbstractString,
        values::Maybe{AbstractVector};
        is_required::Bool = false,
    )::Nothing

Validate that the `values` of a [`VectorValuesData`](@ref) `field` are numeric and finite (if specified, or at all if
`is_required`).
"""
function validate_numeric_values(
    context::ValidationContext,
    field::AbstractString,
    values::Maybe{AbstractVector};
    is_required::Bool = false,
)::Nothing
    if values === nothing
        if is_required
            throw(ArgumentError("must specify $(location(context)).$(field)"))
        end
    elseif !(eltype(values) <: Real)
        throw(ArgumentError("non-numeric $(location(context)).$(field)"))
    else
        validate_vector_is_finite(context, field, values)
    end
    return nothing
end

"""
    numeric_values(values_data::VectorValuesData)::Maybe{AbstractVector{<:Real}}

The values of a [`VectorValuesData`](@ref) which was validated to be numeric.
"""
function numeric_values(values_data::VectorValuesData)::Maybe{AbstractVector{<:Real}}
    values = values_data.vector
    @assert values === nothing || values isa AbstractVector{<:Real}
    return values
end

"""
    entities_hovers(entities::VectorEntitiesData)::Maybe{AbstractVector{<:AbstractString}}

The hover to show for each of the `entities`: its name, followed by whatever its `hovers` say in addition to it. This
is `nothing` if the entities have neither.
"""
function entities_hovers(entities::VectorEntitiesData)::Maybe{AbstractVector{<:AbstractString}}
    names = entities.names
    hovers = entities.hovers
    if names === nothing
        return hovers
    elseif hovers === nothing
        return names
    else
        return names .* "<br>" .* hovers
    end
end

"""
    shared_values_title(
        context::ValidationContext,
        field::AbstractString,
        role::AbstractString,
        values_datas::AbstractVector{VectorValuesData},
    )::Maybe{AbstractString}

The title shared by the `values_datas` of the `role` of the entries of a `field` (e.g., the `x` of each of the `lines`):
the one non-`nothing` title among them. Two different titles are an error.
"""
function shared_values_title(
    context::ValidationContext,
    field::AbstractString,
    role::AbstractString,
    values_datas::AbstractVector{VectorValuesData},
)::Maybe{AbstractString}
    title = nothing
    title_index = 0
    for (index, values_data) in enumerate(values_datas)
        entry_title = values_data.title
        if entry_title !== nothing
            if title === nothing
                title = entry_title
                title_index = index
            elseif entry_title != title
                throw(
                    ArgumentError(
                        "conflicting $(location(context)).$(field)[$(index)].$(role).title: $(entry_title)\n" *
                        "is different from $(location(context)).$(field)[$(title_index)].$(role).title: $(title)",
                    ),
                )
            end
        end
    end
    return title
end

"""
    masked_values(
        values::Maybe{AbstractVector},
        mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
        order::Maybe{AbstractVector{<:Integer}},
    )::Maybe{AbstractVector}

Reorder the `values` by the `order` (if any) and keep only the entries whose `mask` is true (if any). The result is
shorter than the input when a mask is given.
"""
function masked_values(::Nothing, ::Any, ::Any)::Any
    return nothing
end

function masked_values(::Nothing, ::Nothing, ::Any)::Nothing
    return nothing
end

function masked_values(values::AbstractVector{T}, ::Nothing, ::Nothing)::AbstractVector{T} where {T}
    return values
end

function masked_values(
    values::AbstractVector{T},
    ::Nothing,
    order::AbstractVector{<:Integer},
)::AbstractVector{T} where {T}
    return values[order]
end

function masked_values(
    values::AbstractVector{T},
    mask::Union{AbstractVector{Bool}, BitVector},
    ::Nothing,
)::AbstractVector{T} where {T}
    return values[mask]
end

function masked_values(
    values::AbstractVector{T},
    mask::Union{AbstractVector{Bool}, BitVector},
    order::AbstractVector{<:Integer},
)::AbstractVector{T} where {T}
    return masked_values(values[order], mask[order], nothing)
end

"""
    scale_size_values(
        sizes_configuration::SizesConfiguration,
        values::Maybe{AbstractVector{<:Real}},
        mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
    )::Maybe{AbstractVector{<:Real}}

Scale a vector of `values` according to `sizes_configuration`. The `mask` (if any) marks the shown entities; the hidden
ones are left out of the implicit range of the sizes unless the scale `include_hidden` is set.
"""
function scale_size_values(
    sizes_configuration::SizesConfiguration,
    values::Maybe{AbstractVector{<:Real}},
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
)::Maybe{AbstractVector{<:Real}}
    if values === nothing
        return nothing
    end

    sizes_mapping = SizesMapping(sizes_configuration, values, mask)

    scale_configuration = sizes_configuration.scale
    if scale_configuration.minimum !== nothing
        values = max.(values, scale_configuration.minimum)
    end
    if scale_configuration.maximum !== nothing
        values = min.(values, scale_configuration.maximum)
    end

    scaled_values = scale_axis_values(scale_configuration, values; clamp = false)
    @assert scaled_values !== nothing

    return Float64[pixel_size(sizes_mapping, scaled_value) for scaled_value in scaled_values]
end

# How the scaled values of sizes map to pixel sizes, for a specific sizes data.
struct SizesMapping
    sizes_configuration::SizesConfiguration
    minimum_value::Real
    scaled_minimum_value::Real
    scaled_maximum_value::Real
    is_single_value::Bool
    is_minimum_clamped::Bool
    is_maximum_clamped::Bool
end

function SizesMapping(
    sizes_configuration::SizesConfiguration,
    values::AbstractVector{<:Real},
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
)::SizesMapping
    scale_configuration = sizes_configuration.scale
    ranged_values = range_values(scale_configuration, values, mask)

    # Validation rejects non-finite data, so reaching here means some validation was skipped or is incomplete.
    @assert all(isfinite, ranged_values) "non-finite value in the sizes range"  # NOJET

    if scale_configuration.minimum !== nothing
        minimum_value = scale_configuration.minimum
        is_minimum_clamped = any(values .< minimum_value)
    else
        minimum_value = minimum(ranged_values)
        is_minimum_clamped = false
    end

    if scale_configuration.maximum !== nothing
        maximum_value = scale_configuration.maximum
        is_maximum_clamped = any(values .> maximum_value)
    else
        maximum_value = maximum(ranged_values)
        is_maximum_clamped = false
    end

    is_single_value = maximum_value == minimum_value
    if is_single_value
        maximum_value += 1
    end

    return SizesMapping(
        sizes_configuration,
        minimum_value,
        scale_axis_value(scale_configuration, minimum_value; clamp = false),
        scale_axis_value(scale_configuration, maximum_value; clamp = false),
        is_single_value,
        is_minimum_clamped,
        is_maximum_clamped,
    )
end

function pixel_size(sizes_mapping::SizesMapping, scaled_value::Real)::Real
    sizes_configuration = sizes_mapping.sizes_configuration
    return sizes_configuration.smallest +
           sizes_configuration.span * (scaled_value - sizes_mapping.scaled_minimum_value) /
           (sizes_mapping.scaled_maximum_value - sizes_mapping.scaled_minimum_value)
end

"""
    @kwdef struct SizesLegendEntry
        value::Real
        pixel_size::Real
        label::AbstractString
    end

An entry of the legend of sizes. The `value` includes the `log_regularization` (if any). It is drawn with a symbol of
the `pixel_size`, labeled by the `label`. The `label` shows the value the way the ticks of an axis with the same scale
do: the value for a linear or a `Log10Base` scale, and the log value (the power) for a `Log2Base` scale.
"""
@kwdef struct SizesLegendEntry
    value::Real
    pixel_size::Real
    label::AbstractString
end

# The minimal difference in pixels between the sizes of consecutive entries of the legend of sizes.
const SIZES_LEGEND_MINIMAL_PIXELS_GAP = 1

# The maximal number of entries in the legend of sizes (not counting the ends added for clamped values).
const SIZES_LEGEND_MAXIMAL_ENTRIES = 6

# The minimal number of entries in the legend of sizes, unless even the next finer candidate has sizes too close.
const SIZES_LEGEND_MINIMAL_ENTRIES = 3

"""
    sizes_legend_entries(
        sizes_configuration::SizesConfiguration,
        values::AbstractVector{<:Real},
        mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
    )::AbstractVector{SizesLegendEntry}

The entries of the legend of the sizes `values`, as mapped by the `sizes_configuration` (see
[`scale_size_values`](@ref)).

The entries are "nice" values, chosen the way ticks are. For a linear scale these are multiples of a step of 1, 2 or 5
times a power of 10. For a `Log10Base` scale these are the digits, or 1, 2 and 5, or the powers of 10 (or of 100 etc.).
If the range is too small for these, it uses linear steps instead. For a `Log2Base` scale, these are linear steps of the
log values. This uses the finest step whose entries are at least 1 pixel apart, with no more than 6 entries. If that
gives fewer than 3 entries, it uses the next finer step instead, if its entries are at least 1 pixel apart (it has at
most 8 entries). If fewer than 2 such values fit in the range, the entries are the ends of the range. If all the values
are the same, there is a single entry.

If a value was clamped to the `minimum` (or `maximum`) of the scale, the end of the legend is at that value, with a "≤ "
(or "≥ ") prefix. It replaces the nearest entry if that is less than 1 pixel away, and is added to the entries
otherwise.
"""
function sizes_legend_entries(
    sizes_configuration::SizesConfiguration,
    values::AbstractVector{<:Real},
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
)::AbstractVector{SizesLegendEntry}
    sizes_mapping = SizesMapping(sizes_configuration, values, mask)
    scale_configuration = sizes_configuration.scale

    if sizes_mapping.is_single_value
        scaled_value = sizes_mapping.scaled_minimum_value
        places = single_value_places(legend_value(scale_configuration, scaled_value))
        return [sizes_legend_entry(sizes_mapping, scaled_value, places)]
    end

    candidates = sizes_legend_candidates(sizes_mapping)
    chosen_index = nothing
    for (index, candidate) in enumerate(candidates)
        if length(candidate) < 2
            break
        end
        if length(candidate) <= SIZES_LEGEND_MAXIMAL_ENTRIES && is_sizes_legend_distinct(sizes_mapping, candidate)
            chosen_index = index
            break
        end
    end

    # Too few entries say little about the range, so the next finer candidate is used instead, if its sizes are
    # distinct. It has at most 8 entries, so it isn't held to the maximal number of entries.
    if chosen_index !== nothing &&
       chosen_index > 1 &&
       length(candidates[chosen_index]) < SIZES_LEGEND_MINIMAL_ENTRIES &&
       is_sizes_legend_distinct(sizes_mapping, candidates[chosen_index - 1])
        chosen_index -= 1
    end

    chosen = chosen_index === nothing ? nothing : candidates[chosen_index]

    # Too few nice values fit in the range, so show its ends, rounded to the decimal place of their difference.
    if chosen === nothing
        difference =
            legend_value(scale_configuration, sizes_mapping.scaled_maximum_value) -
            legend_value(scale_configuration, sizes_mapping.scaled_minimum_value)
        places = max(0, -floor(Int, log10(difference)))
        chosen = [(sizes_mapping.scaled_minimum_value, places), (sizes_mapping.scaled_maximum_value, places)]
    end

    entries =
        SizesLegendEntry[sizes_legend_entry(sizes_mapping, scaled_value, places) for (scaled_value, places) in chosen]

    if sizes_mapping.is_minimum_clamped
        clamp_sizes_legend_end!(;
            entries,
            sizes_mapping,
            scaled_value = sizes_mapping.scaled_minimum_value,
            places = chosen[1][2],
            prefix = "≤ ",
            is_first = true,
        )
    end
    if sizes_mapping.is_maximum_clamped
        clamp_sizes_legend_end!(;
            entries,
            sizes_mapping,
            scaled_value = sizes_mapping.scaled_maximum_value,
            places = chosen[end][2],
            prefix = "≥ ",
            is_first = false,
        )
    end

    return entries
end

# Whether the sizes of the entries of a `candidate` of the legend of sizes are distinct: each is at least 1 pixel more
# than the previous one.
function is_sizes_legend_distinct(sizes_mapping::SizesMapping, candidate::AbstractVector{Tuple{Float64, Int}})::Bool
    pixel_sizes = Float64[pixel_size(sizes_mapping, scaled_value) for (scaled_value, _) in candidate]
    return minimum(diff(pixel_sizes)) >= SIZES_LEGEND_MINIMAL_PIXELS_GAP
end

# The candidate entries of the legend of sizes, from the finest to the coarsest. Each is a vector of the scaled values
# of the entries, and the number of decimal places to show for each.
function sizes_legend_candidates(sizes_mapping::SizesMapping)::AbstractVector{<:AbstractVector{Tuple{Float64, Int}}}
    scale_configuration = sizes_mapping.sizes_configuration.scale
    scaled_minimum = sizes_mapping.scaled_minimum_value
    scaled_maximum = sizes_mapping.scaled_maximum_value

    if scale_configuration.log_base === Log10Base
        # Within less than a decade, linear steps of the real values are nicer (as Plotly does for its log axes).
        if scaled_maximum - scaled_minimum >= 1
            return log10_candidates(scaled_minimum, scaled_maximum)
        end
        linear_candidates = linear_candidates_of(exp10(scaled_minimum), exp10(scaled_maximum))
        return Vector{Tuple{Float64, Int}}[
            Tuple{Float64, Int}[(log10(value), places) for (value, places) in candidate] for
            candidate in linear_candidates
        ]
    else
        # Linear steps of the scaled values: the values themselves, or the log values of a `Log2Base` scale.
        return linear_candidates_of(scaled_minimum, scaled_maximum)
    end
end

# Candidates of multiples of steps of 1, 2 or 5 times a power of 10, in a range, from the finest to the coarsest (until
# there are less than 2 multiples in the range).
function linear_candidates_of(
    minimum_value::Real,
    maximum_value::Real,
)::AbstractVector{<:AbstractVector{Tuple{Float64, Int}}}
    candidates = Vector{Tuple{Float64, Int}}[]
    power = floor(Int, log10(maximum_value - minimum_value)) - 3
    while true
        for multiplier in (1, 2, 5)
            step = multiplier * exp10(power)
            first_index = ceil(Int, minimum_value / step - 1e-9)
            last_index = floor(Int, maximum_value / step + 1e-9)
            if last_index - first_index + 1 < 2
                return candidates
            end
            places = max(0, -power)
            push!(candidates, Tuple{Float64, Int}[(index * step, places) for index in first_index:last_index])
        end
        power += 1
    end
end

# Candidates of "nice" values of a `Log10Base` scale (the digits, then 1, 2 and 5, then the powers of 10, then of 100,
# etc.) in a range of log values, from the finest to the coarsest (until there are less than 2 values in the range).
function log10_candidates(
    scaled_minimum::Real,
    scaled_maximum::Real,
)::AbstractVector{<:AbstractVector{Tuple{Float64, Int}}}
    candidates = Vector{Tuple{Float64, Int}}[]
    first_power = floor(Int, scaled_minimum)
    last_power = ceil(Int, scaled_maximum)

    for multipliers in ((1, 2, 3, 4, 5, 6, 7, 8, 9), (1, 2, 5))
        candidate = Tuple{Float64, Int}[
            (log10(multiplier) + power, max(0, -power)) for power in first_power:last_power for
            multiplier in multipliers if scaled_minimum - 1e-9 <= log10(multiplier) + power <= scaled_maximum + 1e-9
        ]
        push!(candidates, candidate)
    end

    for powers_step in Iterators.countfrom(1)
        candidate = Tuple{Float64, Int}[
            (Float64(power), max(0, -power)) for power in first_power:last_power if
            power % powers_step == 0 && scaled_minimum - 1e-9 <= power <= scaled_maximum + 1e-9
        ]
        if length(candidate) < 2
            return candidates
        end
        push!(candidates, candidate)
    end
    @assert false
end

# The value shown for a scaled value of sizes: the real value for a linear or a `Log10Base` scale, and the log value
# for a `Log2Base` scale, the same as the ticks of an axis with the same scale.
function legend_value(scale_configuration::ScaleConfiguration, scaled_value::Real)::Real
    if scale_configuration.log_base === Log10Base
        return exp10(scaled_value)
    else
        return scaled_value
    end
end

function sizes_legend_entry(sizes_mapping::SizesMapping, scaled_value::Real, places::Integer)::SizesLegendEntry
    scale_configuration = sizes_mapping.sizes_configuration.scale
    if scale_configuration.log_base === Log2Base
        value = exp2(scaled_value)
    else
        value = legend_value(scale_configuration, scaled_value)
    end
    return SizesLegendEntry(;
        value,
        pixel_size = pixel_size(sizes_mapping, scaled_value),
        label = scale_value_label(scale_configuration, legend_value(scale_configuration, scaled_value), places),
    )
end

# The SI suffixes for the powers of 1000, as used by Plotly log axes (with `exponentformat = "SI"`).
const SI_SUFFIXES = ("k", "M", "G", "T", "P", "E")

# The label of a `value` shown by an entry of a legend of sizes, or by a tick of a color bar: with `places` decimal
# places, and with SI suffixes for a `Log10Base` scale.
function scale_value_label(scale_configuration::ScaleConfiguration, value::Real, places::Integer)::AbstractString
    if scale_configuration.log_base === Log10Base
        thousands = min(floor(Int, log10(abs(value)) / 3), length(SI_SUFFIXES))
        if thousands > 0
            return decimal_text(value / exp10(3 * thousands), max(0, places - 3 * thousands)) * SI_SUFFIXES[thousands]
        else
            return decimal_text(value, places)
        end
    elseif scale_configuration.log_base === Log2Base
        ticks_prefix = axis_ticks_prefix(scale_configuration)
        @assert ticks_prefix !== nothing
        return ticks_prefix * decimal_text(value, places)
    else
        @assert scale_configuration.log_base === nothing
        return decimal_text(value, places)
    end
end

# The number of decimal places to show a single `value` with, which has no step to take them from: (up to) 3
# significant digits, without trailing zeros.
function single_value_places(value::Real)::Int
    places = value == 0 ? 0 : max(0, 2 - floor(Int, log10(abs(value))))
    while places > 0 && round(value; digits = places - 1) == round(value; digits = places)
        places -= 1
    end
    return places
end

# The text of a `value` with exactly this number of decimal `places` (never in scientific notation).
function decimal_text(value::Real, places::Integer)::AbstractString
    digits = string(round(Int, abs(value) * exp10(places)))
    sign = value < 0 && digits != "0" ? "-" : ""
    if places == 0
        return sign * digits
    else
        digits = lpad(digits, places + 1, '0')
        return sign * digits[1:(end - places)] * "." * digits[(end - places + 1):end]
    end
end

# Make an end of the legend of sizes (the 1st entry if `is_first`, the last otherwise) be at a clamped value, with a
# `prefix`. If the nearest entry is too close to it, it is replaced; otherwise, the end is added next to it.
function clamp_sizes_legend_end!(;
    entries::AbstractVector{SizesLegendEntry},
    sizes_mapping::SizesMapping,
    scaled_value::Real,
    places::Integer,
    prefix::AbstractString,
    is_first::Bool,
)::Nothing
    nearest_index = is_first ? 1 : length(entries)
    end_entry = sizes_legend_entry(sizes_mapping, scaled_value, places)
    end_entry = SizesLegendEntry(; end_entry.value, end_entry.pixel_size, label = prefix * end_entry.label)

    if abs(entries[nearest_index].pixel_size - end_entry.pixel_size) < SIZES_LEGEND_MINIMAL_PIXELS_GAP
        entries[nearest_index] = end_entry
    elseif is_first
        pushfirst!(entries, end_entry)
    else
        push!(entries, end_entry)
    end

    return nothing
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

# How many spans of a range the far range extends past each of its ends.
const FAR_RANGE_SPANS = 1000

# How many spans of a range the far range of a trace extends past each of its ends. Plotly moves the points of a trace
# line which are more than 20 plot sizes off the plot, which skews a fill down to them, and the plot shows at least
# the range.
const FAR_TRACE_RANGE_SPANS = 10  # NOLINT

# The largest exponent of a far range on a Plotly log axis, whose values are given as 10 to the exponent (computed as a
# `Float32`, like the range).
const FAR_RANGE_LOG10_EXPONENT = 37

# The `range` extended far past both its ends by `spans` spans, so shapes (or traces) reaching them still reach the
# plot's edges when Plotly widens the axis or the user zooms out. On a Plotly log axis it is clamped to the exponents a
# `Float32` can hold, even if the `range` itself reaches past them.
function far_range(
    range::Range,
    scale_configuration::ScaleConfiguration;
    is_plotly_log::Bool,
    spans::Integer = FAR_RANGE_SPANS,
)::Range
    span = range.maximum - range.minimum
    minimum = range.minimum - spans * span
    maximum = range.maximum + spans * span
    if is_plotly_log && scale_configuration.log_base === Log10Base
        minimum = clamp(minimum, -FAR_RANGE_LOG10_EXPONENT, FAR_RANGE_LOG10_EXPONENT)
        maximum = clamp(maximum, -FAR_RANGE_LOG10_EXPONENT, FAR_RANGE_LOG10_EXPONENT)
    end
    return Range(; minimum, maximum)
end

"""
    push_vertical_bands_shapes(
        shapes::AbstractVector{Shape},
        axis_configuration::AxisConfiguration,
        scaled_values_range::Range,
        bands_data::BandsData,
        bands_configuration::BandsConfiguration,
        bands_scale::Real = 1;
        cross_ref::AbstractString = "y domain",
        is_plotly_log::Bool = false,
    )::Nothing

Push shapes for plotting vertical bands. These shapes need to be places in the layout and not the traces because Plotly.

The bands stretch across the whole of the `cross_ref` axis. When the graph is split into sub-graphs, push the bands
once per sub-graph, using each one's own axis, so they do not stretch across the gaps between them. The low and high
fills reach far past the `scaled_values_range`, so they reach the plot's edges even if the axis is widened.

If `is_plotly_log`, the coordinates are for a Plotly log axis (see [`plotly_axis_value`](@ref)).
"""
function push_vertical_bands_shapes(
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    scaled_values_range::Range,
    bands_data::BandsData,
    bands_configuration::BandsConfiguration,
    bands_scale::Real = 1;
    cross_ref::AbstractString = "y domain",
    is_plotly_log::Bool = false,
)::Nothing
    function plotly_value(scaled_value::Real)::Real
        return plotly_axis_value(axis_configuration.scale, scaled_value; is_plotly_log)
    end

    far_scaled_values_range = far_range(scaled_values_range, axis_configuration.scale; is_plotly_log)

    scaled_low_offset =
        scale_axis_value(axis_configuration.scale, prefer_data(bands_data.low_offset, bands_configuration.low.offset))
    scaled_middle_offset = scale_axis_value(
        axis_configuration.scale,
        prefer_data(bands_data.middle_offset, bands_configuration.middle.offset),
    )
    scaled_high_offset =
        scale_axis_value(axis_configuration.scale, prefer_data(bands_data.high_offset, bands_configuration.high.offset))

    for (band_configuration, scaled_offset) in (
        (bands_configuration.low, scaled_low_offset),
        (bands_configuration.middle, scaled_middle_offset),
        (bands_configuration.high, scaled_high_offset),
    )
        line_style = band_configuration.line.style
        if scaled_offset !== nothing && line_style !== nothing
            push!(
                shapes,
                Shape(
                    "line";
                    line_color = band_configuration.line.color,
                    line_dash = plotly_line_dash(line_style),
                    x0 = plotly_value(scaled_offset * bands_scale),
                    x1 = plotly_value(scaled_offset * bands_scale),
                    xref = "x",
                    y0 = 0,
                    y1 = 1,
                    yref = cross_ref,
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
                x0 = plotly_value(far_scaled_values_range.minimum),
                x1 = plotly_value(scaled_low_offset * bands_scale),
                xref = "x",
                y0 = 0,
                y1 = 1,
                yref = cross_ref,
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
                x0 = plotly_value(scaled_low_offset * bands_scale),
                x1 = plotly_value(scaled_high_offset * bands_scale),
                xref = "x",
                y0 = 0,
                y1 = 1,
                yref = cross_ref,
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
                x0 = plotly_value(scaled_high_offset * bands_scale),
                x1 = plotly_value(far_scaled_values_range.maximum),
                xref = "x",
                y0 = 0,
                y1 = 1,
                yref = cross_ref,
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
        bands_scale::Real = 1;
        cross_ref::AbstractString = "x domain",
        is_plotly_log::Bool = false,
    )::Nothing

Push shapes for plotting horizontal bands. These shapes need to be placed in the layout and not the traces because
Plotly.

The bands stretch across the whole of the `cross_ref` axis. When the graph is split into sub-graphs, push the bands
once per sub-graph, using each one's own axis, so they do not stretch across the gaps between them. The low and high
fills reach far past the `scaled_values_range`, so they reach the plot's edges even if the axis is widened.

If `is_plotly_log`, the coordinates are for a Plotly log axis (see [`plotly_axis_value`](@ref)).
"""
function push_horizontal_bands_shapes(
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    scaled_values_range::Range,
    bands_data::BandsData,
    bands_configuration::BandsConfiguration,
    bands_scale::Real = 1;
    cross_ref::AbstractString = "x domain",
    is_plotly_log::Bool = false,
)::Nothing
    function plotly_value(scaled_value::Real)::Real
        return plotly_axis_value(axis_configuration.scale, scaled_value; is_plotly_log)
    end

    far_scaled_values_range = far_range(scaled_values_range, axis_configuration.scale; is_plotly_log)

    scaled_low_offset =
        scale_axis_value(axis_configuration.scale, prefer_data(bands_data.low_offset, bands_configuration.low.offset))
    scaled_middle_offset = scale_axis_value(
        axis_configuration.scale,
        prefer_data(bands_data.middle_offset, bands_configuration.middle.offset),
    )
    scaled_high_offset =
        scale_axis_value(axis_configuration.scale, prefer_data(bands_data.high_offset, bands_configuration.high.offset))

    for (band_configuration, scaled_offset) in (
        (bands_configuration.low, scaled_low_offset),
        (bands_configuration.middle, scaled_middle_offset),
        (bands_configuration.high, scaled_high_offset),
    )
        line_style = band_configuration.line.style
        if scaled_offset !== nothing && line_style !== nothing
            push!(
                shapes,
                Shape(
                    "line";
                    line_color = band_configuration.line.color,
                    line_dash = plotly_line_dash(line_style),
                    y0 = plotly_value(scaled_offset * bands_scale),
                    y1 = plotly_value(scaled_offset * bands_scale),
                    yref = "y",
                    x0 = 0,
                    x1 = 1,
                    xref = cross_ref,
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
                y0 = plotly_value(far_scaled_values_range.minimum),
                y1 = plotly_value(scaled_low_offset * bands_scale),
                yref = "y",
                x0 = 0,
                x1 = 1,
                xref = cross_ref,
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
                y0 = plotly_value(scaled_low_offset * bands_scale),
                y1 = plotly_value(scaled_high_offset * bands_scale),
                yref = "y",
                x0 = 0,
                x1 = 1,
                xref = cross_ref,
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
                y0 = plotly_value(scaled_high_offset * bands_scale),
                y1 = plotly_value(far_scaled_values_range.maximum),
                yref = "y",
                x0 = 0,
                x1 = 1,
                xref = cross_ref,
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
        axis_configuration::AxisConfiguration,
        x_scaled_values_range::Range,
        y_scaled_values_range::Range,
        bands_data::BandsData,
        bands_configuration::BandsConfiguration;
        is_plotly_log::Bool = false,
    )::Nothing

Push shapes for plotting diagonal bands. These shapes need to be placed in the layout and not the traces because Plotly.
The lines and fills reach far past the `x_scaled_values_range` and `y_scaled_values_range`, so they reach the plot's
edges even if the axes are widened.

If `is_plotly_log`, the coordinates are for Plotly log axes (see [`plotly_axis_value`](@ref)).
"""
function push_diagonal_bands_shapes(
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
    bands_data::BandsData,
    bands_configuration::BandsConfiguration;
    is_plotly_log::Bool = false,
)::Nothing
    far_x_scaled_values_range = far_range(x_scaled_values_range, axis_configuration.scale; is_plotly_log)
    far_y_scaled_values_range = far_range(y_scaled_values_range, axis_configuration.scale; is_plotly_log)

    low_band_points = push_diagonal_bands_line(
        shapes,
        axis_configuration,
        is_plotly_log,
        far_x_scaled_values_range,
        far_y_scaled_values_range,
        bands_data.low_offset,
        bands_configuration.low,
    )
    push_diagonal_bands_line(
        shapes,
        axis_configuration,
        is_plotly_log,
        far_x_scaled_values_range,
        far_y_scaled_values_range,
        bands_data.middle_offset,
        bands_configuration.middle,
    )
    high_band_points = push_diagonal_bands_line(
        shapes,
        axis_configuration,
        is_plotly_log,
        far_x_scaled_values_range,
        far_y_scaled_values_range,
        bands_data.high_offset,
        bands_configuration.high,
    )

    if low_band_points !== nothing && bands_configuration.low.line.is_filled
        push_diagonal_bands_low_fill(
            shapes,
            axis_configuration,
            is_plotly_log,
            far_x_scaled_values_range,
            far_y_scaled_values_range,
            low_band_points,
            bands_configuration.low,
        )
    end

    if low_band_points !== nothing && high_band_points !== nothing && bands_configuration.middle.line.is_filled
        push_diagonal_bands_middle_fill(
            shapes,
            axis_configuration,
            is_plotly_log,
            far_x_scaled_values_range,
            far_y_scaled_values_range,
            low_band_points,
            high_band_points,
            bands_configuration.middle,
        )
    end

    if high_band_points !== nothing && bands_configuration.high.line.is_filled
        push_diagonal_bands_high_fill(
            shapes,
            axis_configuration,
            is_plotly_log,
            far_x_scaled_values_range,
            far_y_scaled_values_range,
            high_band_points,
            bands_configuration.high,
        )
    end

    return nothing
end

function push_diagonal_bands_line(
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    is_plotly_log::Bool,
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
    data_offset::Maybe{Real},
    band_configuration::BandConfiguration,
)::Maybe{Tuple{BandPoint, BandPoint}}
    offset = prefer_data(data_offset, band_configuration.offset)
    start_point = start_diagonal_band_point(axis_configuration, x_scaled_values_range, y_scaled_values_range, offset)
    end_point = end_diagonal_band_point(axis_configuration, x_scaled_values_range, y_scaled_values_range, offset)
    @assert (start_point === nothing) == (end_point === nothing)
    if start_point === nothing || end_point === nothing
        return nothing
    end

    line_style = band_configuration.line.style
    if line_style !== nothing
        push!(
            shapes,
            Shape(
                "line";
                line_color = band_configuration.line.color,
                line_dash = plotly_line_dash(line_style),
                y0 = plotly_axis_value(axis_configuration.scale, start_point.point.y; is_plotly_log),
                y1 = plotly_axis_value(axis_configuration.scale, end_point.point.y; is_plotly_log),
                yref = "y",
                x0 = plotly_axis_value(axis_configuration.scale, start_point.point.x; is_plotly_log),
                x1 = plotly_axis_value(axis_configuration.scale, end_point.point.x; is_plotly_log),
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
    scaled_offset = scale_axis_offset(axis_configuration.scale, offset)
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
    scaled_offset = scale_axis_offset(axis_configuration.scale, offset)
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

function scale_axis_offset(scale_configuration::ScaleConfiguration, offset::Real)::Real
    if scale_configuration.percent
        scale = 100.0
    else
        scale = 1.0
    end

    if scale_configuration.log_base === Log10Base
        return log10(offset * scale)
    elseif scale_configuration.log_base === Log2Base
        return log2(offset * scale)
    else
        @assert scale_configuration.log_base === nothing
        return offset
    end
end

function is_in_bounds(x_scaled_values_range::Range, y_scaled_values_range::Range, point::ScaledPoint)::Bool
    return (x_scaled_values_range.minimum <= point.x <= x_scaled_values_range.maximum) &&
           (y_scaled_values_range.minimum <= point.y <= y_scaled_values_range.maximum)
end

function push_diagonal_bands_low_fill(
    shapes::AbstractVector{Shape},
    axis_configuration::AxisConfiguration,
    is_plotly_log::Bool,
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
    band_points::Tuple{BandPoint, BandPoint},
    band_configuration::BandConfiguration,
)::Nothing
    path_parts = PathParts(; scale_configuration = axis_configuration.scale, is_plotly_log)

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
    axis_configuration::AxisConfiguration,
    is_plotly_log::Bool,
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
    band_points::Tuple{BandPoint, BandPoint},
    band_configuration::BandConfiguration,
)::Nothing
    path_parts = PathParts(; scale_configuration = axis_configuration.scale, is_plotly_log)

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
    axis_configuration::AxisConfiguration,
    is_plotly_log::Bool,
    x_scaled_values_range::Range,
    y_scaled_values_range::Range,
    low_band_points::Tuple{BandPoint, BandPoint},
    high_band_points::Tuple{BandPoint, BandPoint},
    band_configuration::BandConfiguration,
)::Nothing
    path_parts = PathParts(; scale_configuration = axis_configuration.scale, is_plotly_log)

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

# The parts of a Plotly SVG path, given scaled points and emitting them as Plotly coordinates.
@kwdef struct PathParts
    scale_configuration::ScaleConfiguration
    is_plotly_log::Bool
    parts::Vector{AbstractString} = AbstractString[]
end

function to_point(path_parts::PathParts, x::Real, y::Real)::Nothing
    push!(path_parts.parts, isempty(path_parts.parts) ? "M" : "L")
    for value in (x, y)
        plotly_value = plotly_axis_value(path_parts.scale_configuration, value; path_parts.is_plotly_log)
        push!(path_parts.parts, string(plotly_value))
    end
    return nothing
end

function to_start_point(path_parts::PathParts, band_points::Tuple{BandPoint, BandPoint})::Nothing
    to_point(path_parts, band_points[1].point.x, band_points[1].point.y)
    return nothing
end

function to_end_point(path_parts::PathParts, band_points::Tuple{BandPoint, BandPoint})::Nothing
    to_point(path_parts, band_points[2].point.x, band_points[2].point.y)
    return nothing
end

function to_bottom_left(path_parts::PathParts, x_scaled_values_range::Range, y_scaled_values_range::Range)::Nothing
    to_point(path_parts, x_scaled_values_range.minimum, y_scaled_values_range.minimum)
    return nothing
end

function to_bottom_right(path_parts::PathParts, x_scaled_values_range::Range, y_scaled_values_range::Range)::Nothing
    to_point(path_parts, x_scaled_values_range.maximum, y_scaled_values_range.minimum)
    return nothing
end

function to_top_left(path_parts::PathParts, x_scaled_values_range::Range, y_scaled_values_range::Range)::Nothing
    to_point(path_parts, x_scaled_values_range.minimum, y_scaled_values_range.maximum)
    return nothing
end

function to_top_right(path_parts::PathParts, x_scaled_values_range::Range, y_scaled_values_range::Range)::Nothing
    to_point(path_parts, x_scaled_values_range.maximum, y_scaled_values_range.maximum)
    return nothing
end

function push_fill_path(
    shapes::AbstractVector{Shape},
    path_parts::PathParts,
    band_configuration::BandConfiguration,
)::Nothing
    push!(path_parts.parts, "Z")

    push!(
        shapes,
        Shape(
            "path";
            layer = "below",
            fillcolor = fill_color(band_configuration.line.color),
            path = join(path_parts.parts, " "),
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
    return "#$(hex(RGBA(rgba.r, rgba.g, rgba.b, rgba.alpha * 0.5), :RRGGBBAA))"
end

"""
    plotly_title(title::Maybe{AbstractString})::Maybe{AbstractDict}

Return the Plotly form of a `title` (of the figure, an axis, a color bar, ...), or `nothing` if there is none. Plotly
ignores a title given as a plain string, so it is given as the `text` of the title.
"""
function plotly_title(::Nothing)::Nothing
    return nothing
end

function plotly_title(title::AbstractString)::AbstractDict
    return Dict{Symbol, Any}(:text => title)
end

"""
    prefer_data(data_value::Any, configuration_value::Any)::Any
    prefer_data(data_values::Maybe{AbstractVector}, index::Integer, configuration_value::Any)::Any

Return a value to use, prefering the data value (which may be in a vector, where a `nothing` vector or entry also falls
back to the configuration value) to the configuration value.
"""
function prefer_data(data_value::Any, configuration_value::Any)::Any
    if data_value === nothing
        return configuration_value
    else
        return data_value
    end
end

function prefer_data(data_values::Maybe{AbstractVector}, index::Integer, configuration_value::Any)::Any
    if data_values === nothing || data_values[index] === nothing
        return configuration_value
    else
        return data_values[index]
    end
end

"""
    displayed_annotations(
        annotations::AbstractVector{AnnotationData},
        order::Maybe{AbstractVector{<:Integer}},
    )::AbstractVector{AnnotationData}

The annotations which are actually drawn, in the order they are drawn in: reordered by the `order`, leaving out the
ones that are not `is_shown`. A hidden annotation gives up its place, so the rest of the row closes up.
"""
function displayed_annotations(
    annotations::AbstractVector{AnnotationData},
    order::Maybe{AbstractVector{<:Integer}},
)::AbstractVector{AnnotationData}
    return AnnotationData[
        annotations[index] for index in prefer_data(order, 1:length(annotations)) if annotations[index].is_shown
    ]
end

# The indices of the `parts` which are `is_shown`, in the order of the `ordered_indices`.
function shown_indices(ordered_indices::AbstractVector{T}, parts::AbstractVector)::Vector{T} where {T <: Integer}
    return T[index for index in ordered_indices if parts[index].is_shown]
end

"""
    collect_range!(
        range::MaybeRange,
        values::Maybe{Union{Tuple{Vararg{Maybe{Real}}}, AbstractVector{<:Maybe{Real}}}},
    )::Nothing

Expand the `range` to cover the `values` (ignoring `nothing` values).
"""
function collect_range!(
    range::MaybeRange,
    values::Maybe{Union{Tuple{Vararg{Maybe{Real}}}, AbstractVector{<:Maybe{Real}}}},
)::Nothing
    if values !== nothing
        for value in values
            # Validation rejects non-finite data, so reaching here means some validation was skipped or is incomplete.
            @assert value === nothing || isfinite(value) "non-finite value in a computed range: $(value)"
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
    range_values(
        scale_configuration::ScaleConfiguration,
        values::AbstractArray,
        mask::Maybe{AbstractArray{Bool}},
    )::AbstractArray

The `values` which take part in the implicit range: all of them, unless the scale does not `include_hidden`, in which
case only the ones shown by the `mask`.
"""
function range_values(
    scale_configuration::ScaleConfiguration,
    values::AbstractArray,
    mask::Maybe{AbstractArray{Bool}},
)::AbstractArray
    if mask === nothing || scale_configuration.include_hidden
        return values
    else
        return values[mask]
    end
end

"""
    collect_hidden_range!(
        range::MaybeRange,
        scale_configuration::ScaleConfiguration,
        scaled_values::AbstractVector{<:Real},
        mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
    )::Nothing

Expand the implicit `range` to cover the hidden `scaled_values` (the ones whose `mask` is false), if the scale does
`include_hidden`. The shown values are collected by whoever draws them.
"""
function collect_hidden_range!(
    range::MaybeRange,
    scale_configuration::ScaleConfiguration,
    scaled_values::AbstractVector{<:Real},
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
)::Nothing
    if mask !== nothing && scale_configuration.include_hidden
        collect_range!(range, scaled_values[.!mask])
    end
    return nothing
end

"""
    @kwdef struct SubGraph
        index::Integer
        n_graphs::Integer
        graphs_gap::Maybe{Real}
        mirrored::Bool = false
        n_annotations::Integer = 0
        annotation_size::Maybe{AnnotationSize} = nothing
        dendogram_size::Maybe{Real} = nothing
        pixel_length::Maybe{Real} = nothing
    end

Identify one sub-graph out of a set of `n_graphs` adjacent graphs along some axis. If the `index` is 1, this is the 1st
sub-graph (used top initialize some values such as the legend group title). If `graphs_gap` is `nothing` then the
sub-graphs are plotted on top of each other, which affects axis parameters; otherwise, the sub-graphs are plotted with
this gap, which affects layout parameters.

If `mirrored`, the sub-graphs are paired - an odd one and the even one following it - and the `graphs_gap` is only
placed between the pairs; the two sub-graphs of a pair are adjacent, meeting at the axis they share. The `n_graphs`
must then be even.

This also supports `n_annotations` (of the other axis) with `annotation_size` (along this axis). If the index is
negative, it is the (negated) index of an annotation (of the other axis).

If the `index` is 0, this is the dendogram graph (of the other axis) with `dendogram_size` (along this axis).

The `graphs_gap`, `annotation_size` and `dendogram_size` are in pixels. The `pixel_length` is the length of the plot's
area along this axis, in pixels, which turns them into fractions of the axis. It must be given to compute the
sub-graph's domain.
"""
@kwdef struct SubGraph
    index::Integer
    n_graphs::Integer
    graphs_gap::Maybe{Real}
    mirrored::Bool = false
    n_annotations::Integer = 0
    annotation_size::Maybe{AnnotationSize} = nothing
    dendogram_size::Maybe{Real} = nothing
    pixel_length::Maybe{Real} = nothing
end

# The number of gaps between the sub-graphs. Mirrored, the gap is only between the pairs, so there is one less gap than
# there are pairs; otherwise there is one between each two sub-graphs.
function n_sub_graph_gaps(n_graphs::Integer, mirrored::Bool)::Int
    if mirrored
        @assert n_graphs % 2 == 0
        return div(n_graphs, 2) - 1
    else
        return n_graphs - 1
    end
end

# At most this fraction of an axis is spent on the annotations and on the gaps between the sub-graphs; most of the graph
# should be the graph itself.
const MAX_OVERHEAD_FRACTION = 1 / 3

# The size of each annotation, the gap following it, the gap between the sub-graphs, and the size of the dendogram (0
# if none), as fractions of the axis; if together they would take more than `MAX_OVERHEAD_FRACTION` of the axis, they
# are all scaled down to fit.
function sub_graph_overhead_sizes(sub_graph::SubGraph)::NTuple{4, Float64}
    pixel_length = sub_graph.pixel_length
    @assert pixel_length !== nothing
    if sub_graph.annotation_size === nothing
        annotation_size = annotation_gap = 0.0
    else
        annotation_size = sub_graph.annotation_size.size / pixel_length
        annotation_gap = sub_graph.annotation_size.gap / pixel_length
    end
    graphs_gap = prefer_data(sub_graph.graphs_gap, 0) / pixel_length
    dendogram_size = prefer_data(sub_graph.dendogram_size, 0) / pixel_length

    overhead =
        sub_graph.n_annotations * (annotation_size + annotation_gap) +
        n_sub_graph_gaps(sub_graph.n_graphs, sub_graph.mirrored) * graphs_gap +
        dendogram_size
    if overhead > MAX_OVERHEAD_FRACTION
        scale = MAX_OVERHEAD_FRACTION / overhead  # UNTESTED
        annotation_size *= scale  # UNTESTED
        annotation_gap *= scale  # UNTESTED
        graphs_gap *= scale  # UNTESTED
        dendogram_size *= scale  # UNTESTED
    end

    return (annotation_size, annotation_gap, graphs_gap, dendogram_size)
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

    annotation_size, annotation_gap, graphs_gap, dendogram_size = sub_graph_overhead_sizes(sub_graph)

    annotations_size = (annotation_gap + annotation_size) * sub_graph.n_annotations

    # A mirrored pair of sub-graphs meets in the middle, so when they are the only two, the annotations belong in that
    # middle - the spine of the butterfly - rather than outside the whole of it. More than one pair has more than one
    # such middle and no single spine, so there the annotations stay where they are for any other graph.
    is_spine = sub_graph.mirrored && n_graphs == 2 && sub_graph.n_annotations > 0

    if axis_index > 0
        @assert 1 <= axis_index <= n_graphs

        if sub_graph.n_annotations == 0 || is_spine
            start_graph_offset = 0.0
        else
            @assert sub_graph.annotation_size !== nothing
            start_graph_offset = annotations_size
        end

        graphs_total_size = 1 - start_graph_offset - n_sub_graph_gaps(n_graphs, sub_graph.mirrored) * graphs_gap
        if is_spine
            graphs_total_size -= annotations_size
        end
        graphs_total_size -= dendogram_size
        graph_size = graphs_total_size / n_graphs

        # Mirrored, only the pairs before this sub-graph contribute a gap; otherwise every sub-graph before it does.
        if sub_graph.mirrored
            n_gaps_before = div(axis_index - 1, 2)
        else
            n_gaps_before = axis_index - 1
        end
        start_graph_offset += (axis_index - 1) * graph_size + n_gaps_before * graphs_gap
        if is_spine && axis_index == 2
            start_graph_offset += annotations_size
        end
        end_graph_offset = start_graph_offset + graph_size

    elseif axis_index < 0
        axis_index = -axis_index
        @assert 1 <= axis_index <= sub_graph.n_annotations
        @assert sub_graph.annotation_size !== nothing
        if is_spine
            # Half a gap on each side of the band of annotations, so that it sits evenly between the two sides.
            start_graph_offset = (1 - annotations_size) / 2 + annotation_gap / 2
        else
            start_graph_offset = 0.0
        end
        start_graph_offset += (axis_index - 1) * (annotation_gap + annotation_size)
        end_graph_offset = start_graph_offset + annotation_size

    else
        @assert sub_graph.dendogram_size !== nothing
        start_graph_offset = 1 - dendogram_size
        end_graph_offset = 1
    end

    # Accumulating the offsets of many sub-graphs can drift past the edges of the figure by an epsilon.
    start_graph_offset = max(start_graph_offset, 0)
    end_graph_offset = min(end_graph_offset, 1)

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
        pixel_length::Real,
    )::Nothing

Verify there is at least some space left for the actual graph after leaving space for gaps and/or annotations. These
are in pixels, and the `pixel_length` is the most the axis may have: the length which the configured margins leave in
the figure (see [`MarginsConfiguration`](@ref)). The margins grow to hold the content of their sides, so the axis may
end up shorter; the layout then scales the gaps, annotations and dendogram down to at most a third of the axis.
"""
function validate_axis_sizes(;
    axis_name::AbstractString,
    graphs_gap::Maybe{Real} = nothing,
    n_graphs::Integer = 1,
    annotation_size::AnnotationSize,
    n_annotations::Integer,
    dendogram_size::Maybe{Real} = nothing,
    pixel_length::Real,
)::Nothing
    if graphs_gap === nothing
        graph_gaps_overhead = 0
    else
        graph_gaps_overhead = (n_graphs - 1) * graphs_gap
    end

    annotations_gaps_overhead = n_annotations * annotation_size.gap
    annotations_sizes_overhead = n_annotations * annotation_size.size

    if dendogram_size === nothing
        dendogram_size_overhead = 0
    else
        dendogram_size_overhead = dendogram_size
    end

    total_overhead_size =
        graph_gaps_overhead + annotations_gaps_overhead + annotations_sizes_overhead + dendogram_size_overhead

    if total_overhead_size >= pixel_length
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
        text *= "\nthe total overhead: $(total_overhead_size)" * "\nis not less than the axis length: $(pixel_length)"
        throw(ArgumentError(text))
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
        original_color_values::Maybe{
            Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}, AbstractMatrix{<:Real}},
        }
        final_colors_values::Maybe{
            Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}, AbstractMatrix{<:Real}},
        }
        final_colors_range::Maybe{Range}
        final_categorical_palette::Maybe{CategoricalColors} = nothing
        scaled_colors_palette::Maybe{AbstractVector{<:Tuple{Real, AbstractString}}}
        show_in_legend::Bool
        show_scale::Bool
    end

Colors data after applying the configuration. The `final_categorical_palette` is the categorical palette of
categorical colors, either as configured or (for [`AutomaticColors`](@ref)) as picked automatically.
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
    final_categorical_palette::Maybe{CategoricalColors} = nothing
    scaled_colors_palette::Maybe{AbstractVector{<:Tuple{Real, AbstractString}}}
    show_in_legend::Bool
    show_scale::Bool
end

# The color of an entity with no categorical color. Plotly rejects an empty color, so this is a fully transparent one.
const NO_COLOR = "rgba(0,0,0,0)"

"""
    configured_colors(;
        colors_configuration::ColorsConfiguration,
        colors_title::Maybe{AbstractString},
        colors_values::Maybe{Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}, AbstractMatrix{<:Real}}},
        next_colors_scale_index::AbstractVector{<:Integer},
        mask::Maybe{Union{AbstractVector{Bool}, BitVector, AbstractMatrix{Bool}, BitMatrix}} = nothing,
    )::ConfiguredColors

Apply the colors configuration to the colors data. The `mask` (if any) marks the shown entities; the hidden ones get no
categorical color, and are left out of the implicit range of the colors scale unless the scale `include_hidden` is set.
An empty categorical value gets the color of the empty value in the palette. If the palette has none, it gets no color.
"""
function configured_colors(;
    colors_configuration::ColorsConfiguration,
    colors_title::Maybe{AbstractString},
    colors_values::Maybe{Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}, AbstractMatrix{<:Real}}},
    next_colors_scale_index::AbstractVector{<:Integer},
    mask::Maybe{Union{AbstractVector{Bool}, BitVector, AbstractMatrix{Bool}, BitMatrix}} = nothing,
)::ConfiguredColors
    original_color_values = colors_values
    scaled_colors_palette = nothing

    colors_scale_index = nothing
    final_colors_range = nothing
    final_colors_values = nothing
    final_categorical_palette = nothing
    show_in_legend = false
    show_scale = false

    if colors_values isa AbstractVector{<:AbstractString}
        show_in_legend = colors_configuration.legend.show

        # Automatic colors are resolved here to an explicit categorical palette, so consumers that render the final
        # color values directly (unlike scatters, which draw one trace per category and let Plotly pick) get valid color
        # names.
        palette = if colors_configuration.palette isa AutomaticColors
            categorical_palette(non_empty_strings(colors_values))  # NOJET
        else
            colors_configuration.palette
        end

        if palette isa CategoricalColors
            final_categorical_palette = palette
            palette_dict = categorical_colors_dict(palette)
            empty_value_color = get(palette_dict, "", NO_COLOR)
            @assert !(mask isa AbstractMatrix)
            final_colors_values = categorical_final_colors(
                Union{String, valtype(palette_dict)},
                palette,
                colors_values,
                empty_value_color,
                mask,
            )
        else
            @assert palette === nothing
            final_colors_values = colors_values
        end

    elseif colors_values !== nothing && eltype(colors_values) <: Real
        show_scale = colors_configuration.legend.show
        colors_scale_index = next_colors_scale_index[1]
        next_colors_scale_index[1] += 1

        final_colors_values = scale_axis_values(colors_configuration.scale, colors_values)
        continuous_palette = colors_configuration.palette
        if continuous_palette isa ContinuousColors
            color_palette_values = Float64[entry[1] for entry in continuous_palette]
            scaled_colors_palette_values = scale_axis_values(colors_configuration.scale, color_palette_values)
            @assert scaled_colors_palette_values !== nothing
            implicit_scaled_colors_range =  # NOJET
                Range(; minimum = scaled_colors_palette_values[1], maximum = scaled_colors_palette_values[end])
            final_colors_range = final_scaled_range(implicit_scaled_colors_range, colors_configuration.scale)

            scale = implicit_scaled_colors_range.maximum - implicit_scaled_colors_range.minimum
            @assert scale > 0
            final_color_palette_values = (scaled_colors_palette_values .- implicit_scaled_colors_range.minimum) ./ scale
            final_color_palette_values[1] = 0
            final_color_palette_values[end] = 1
            scaled_colors_palette = palette_with_values(
                Tuple{Float64, fieldtype(eltype(continuous_palette), 2)},
                final_color_palette_values,
                continuous_palette,
            )
        else
            ranged_values = range_values(colors_configuration.scale, final_colors_values, mask)
            # Validation rejects non-finite data, so reaching here means some validation was skipped or is incomplete.
            is_finite_range = all(value -> value !== nothing && isfinite(value), ranged_values)
            @assert is_finite_range "non-finite value in the colors range"
            implicit_scaled_colors_range = Range(; minimum = minimum(ranged_values), maximum = maximum(ranged_values))
            final_colors_range = final_scaled_range(implicit_scaled_colors_range, colors_configuration.scale)
        end
    end

    return ConfiguredColors(;
        colors_title,
        colors_configuration,
        colors_scale_index,
        original_color_values,
        final_colors_values,
        final_colors_range,
        final_categorical_palette,
        scaled_colors_palette,
        show_in_legend,
        show_scale,
    )
end

# The `strings` which are not empty.
function non_empty_strings(strings::AbstractVector{S})::Vector{S} where {S <: AbstractString}
    return S[value for value in strings if value != ""]
end

# The final color of each of the `colors_values`, as a vector of `T`: no color if the `mask` hides it, the
# `empty_value_color` if it is empty, and its color in the `palette` otherwise.
function categorical_final_colors(
    ::Type{T},
    palette::CategoricalColors,
    colors_values::AbstractVector{<:AbstractString},
    empty_value_color::AbstractString,
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
)::Vector{T} where {T <: AbstractString}
    return T[if !prefer_data(mask, index, true)
        NO_COLOR
    elseif color == ""
        empty_value_color
    else
        palette[color]
    end for (index, color) in enumerate(colors_values)]
end

# The `values` paired with the colors of the entries of the `palette`, as a vector of `T`.
function palette_with_values(
    ::Type{T},
    values::AbstractVector{<:Real},
    palette::ContinuousColors,
)::Vector{T} where {T <: Tuple}
    return T[(value, entry[2]) for (value, entry) in zip(values, palette)]
end

# The labels of the entries of the categorical legend of some `colors`: their categories, except the empty one.
function categorical_legend_labels(colors::ConfiguredColors)::Vector{String}
    palette = colors.final_categorical_palette
    @assert palette !== nothing
    return String[String(name) for name in keys(categorical_colors_dict(palette)) if name != ""]
end

end  # module
