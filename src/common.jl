"""
Common data types and utilities for defining specific graph types.
"""
module Common

export AbstractGraphConfiguration
export AbstractGraphData
export AxisConfiguration
export BandConfiguration
export BandsConfiguration
export BandsData
export COLOR_PALETTES_LOCK
export CategoricalColors
export ColorsConfiguration
export ContinuousColors
export DashDotLine
export DashLine
export DotLine
export FigureConfiguration
export Graph
export HorizontalValues
export LineConfiguration
export LineStyle
export Log10Scale
export Log2Scale
export LogScale
export MarginsConfiguration
export NAMED_COLOR_PALETTES
export PlotlyFigure
export SizeConfiguration
export SolidLine
export SubGraph
export ValuesOrientation
export VerticalValues
export save_graph

using Base.Multimedia
using PlotlyJS

using ..Validations
using Colors
using ColorVectorSpace

import PlotlyJS.SyncPlot

import ..Validations.Maybe

"""
The type of a rendered graph which Julia knows how to display.

A plotly figure contains everything needed to display an interactive graph (or generate a static one on disk). It can also be
converted to a JSON string for handing it over to a different programming language (e.g., to be used to display the
interactive graph in a Python Jupyter notebook, given an appropriate wrapper code).
"""
PlotlyFigure = Union{Plot, SyncPlot}

"""
A configuration of a [`Graph`] specifies how to display the data while being (as much as possible) independent of the
data itself. That is, it should be possible to apply the same configuration to multiple sets of data to generate
multiple similar graphs. In some cases (e.g., colors) you can specify a default in the configuration and override it for
specific entities in the data.
"""
abstract type AbstractGraphConfiguration <: Validated end

"""
Some data of a [`Graph`] specifies what to display the data while being (as much as possible) independent of how to
display it. That is, it should be possible to apply multiple sets of data to the same configuration to to generate
multiple similar graphs. In some cases (e.g., colors) you can specify a default in the configuration and override it for
specific entities in the data.
"""
abstract type AbstractGraphData <: Validated end

"""
The type of a figure we can display. This is a combination of some [`AbstractGraphData`](@ref) and
[`AbstractGraphConfiguration`](@ref). Accessing the `.figure` property of the graph will return it as a `PlotlyFigure`,
which can be displayed in interactive environments (Julia REPL and/or Jupyter notebooks).

!!! note

    You should call [`save_graph`](@ref) to save the graph to a file, instead of calling `savefig` on the `.figure`,
    because the latter does't obey the graph size, because Plotly.
"""
@kwdef mutable struct Graph{D <: AbstractGraphData, C <: AbstractGraphConfiguration} <: Validated  # NOJET
    data::D = D()
    configuration::C = C()
end

function Base.show(io::IO, graph::Graph)::Nothing
    print(io, "$(typeof(graph)) (use .figure to show the graph)")
    return nothing
end

function Base.getproperty(graph::Graph, property::Symbol)::Any
    if property == :figure
        return graph_to_figure(graph)
    else
        return getfield(graph, property)
    end
end

function Validations.validate(context::ValidationContext, graph::Graph)::Nothing
    validate_field(context, "data", graph.data)
    validate_field(context, "configuration", graph.configuration)
    validate_graph(graph)  # NOJET
    return nothing
end

"""
    validate_graph(::Graph)::Maybe{AbstractString}

Validate that the combination of data and configuration in a graph is valid, after validating each one separately. This
isn't invoked manually, instead it is called by the overall `validate` of the graph. It is provided (with a default
empty implementation) to allow for type-specific validations.
"""
function validate_graph(::Graph)::Maybe{AbstractString}  # UNTESTED
    return nothing
end

"""
    save_graph(graph::Graph, output_file::AbstractString)::Nothing

Save the graph to a file. Unlike the Plotly `savefig` function, this function will actually obey the `width` and
`height` parameters specified in the graph's configuration. The format is deduced from the suffix of the file name.
"""
function save_graph(graph::Graph, output_file::AbstractString)::Nothing
    savefig(  # NOJET
        graph_to_figure(graph),
        output_file;
        width = graph.configuration.figure.width,
        height = graph.configuration.figure.height,
    )
    return nothing
end

"""
    graph_to_figure(graph::Graph)::PlotlyFigure

Render a graph given its data and configuration. Technically this just converts the graph to a [`PlotlyFigure`](@ref)
which Julia knows how display for us, rather than actually display the graph. The implementation depends on the specific
graph type.

You can just write `graph.figure` instead of `graph_to_figure(graph)`.

!!! note

    When saving a figure to a file, Plotly in its infinite wisdom ignores the graph `width` and `height` specified
    inside the figure, (except for saving HTML file). You should therefore use [`save_graph`](@ref) rather than call
    `savefig` on the result of `graph_to_figure`.
"""
function graph_to_figure(graph::Graph)::PlotlyFigure end  # NOLINT

"""
    @kwdef mutable struct MarginsConfiguration <: Validated
        left::Int = 50
        bottom::Int = 50
        right::Int = 50
        top::Int = 50
    end

Configure the margins of the graph. Sizes are in pixels (1/96th of an inch). Plotly is uncapable of automatically
adjusting the margins to adapt to tick labels, so you may have to tweak this manually to avoid clipping or reduce wasted
white space. In the 21st century. Sigh.
"""
@kwdef mutable struct MarginsConfiguration <: Validated
    left::Int = 50
    bottom::Int = 50
    right::Int = 50
    top::Int = 50
end

function Validations.validate(context::ValidationContext, margins_configuration::MarginsConfiguration)::Nothing
    for (field, value) in (
        ("left", margins_configuration.left),
        ("right", margins_configuration.right),
        ("bottom", margins_configuration.bottom),
        ("top", margins_configuration.top),
    )
        validate_in(context, field) do
            validate_is_at_least(context, value, 0)
            return nothing
        end
    end
    return nothing
end

"""
    @kwdef mutable struct FigureConfiguration <: Validated
        margins::MarginsConfiguration = MarginsConfiguration()
        width::Maybe{Int} = nothing
        height::Maybe{Int} = nothing
        template::Maybe{AbstractString} = nothing
        show_grid::Bool = true
        grid_color::Maybe{AbstractString} = nothing
    end

Generic configuration that applies to the whole figure. Each complete [`AbstractGraphConfiguration`](@ref) contains a
`figure` field of this type.

The optional `width` and `height` are in pixels, that is, 1/96 of an inch. The `margins` are specified in the same
units.

By default, `show_grid` is set.

The default `template` is "simple_white" which is the cleanest (we also force the background color to white if this is
the cae). The `show_grid` flag can be used to disable the grid for an even cleaner (but less informative) look, and the
`grid_color` can also be changed.
"""
@kwdef mutable struct FigureConfiguration <: Validated
    margins::MarginsConfiguration = MarginsConfiguration()
    width::Maybe{Int} = nothing
    height::Maybe{Int} = nothing
    template::Maybe{AbstractString} = nothing
    show_grid::Bool = true
    grid_color::Maybe{AbstractString} = nothing
end

function Validations.validate(context::ValidationContext, figure_configuration::FigureConfiguration)::Nothing
    validate_field(context, "margins", figure_configuration.margins)
    validate_in(context, "width") do
        validate_is_above(context, figure_configuration.width, 0)
        return nothing
    end
    validate_in(context, "height") do
        validate_is_above(context, figure_configuration.height, 0)
        return nothing
    end
    validate_in(context, "grid_color") do
        validate_is_color(context, figure_configuration.grid_color)
        return nothing
    end
    return nothing
end

"""
The orientation of the values axis in a distribution(s) or bars graph:

`HorizontalValues` - The values are the X axis

`VerticalValues` - The values are the Y axis (the default).
"""
@enum ValuesOrientation HorizontalValues VerticalValues

"""
Supported log scales (when log scaling is enabled):

  - `Log10Scale` converts values to their log (base 10).

  - `Log2Scale` converts values to their log (base 2).
"""
@enum LogScale Log10Scale Log2Scale

"""
    @kwdef mutable struct SizeConfiguration <: Validated
        smallest::Maybe{Real} = nothing
        largest::Maybe{Real} = nothing
    end

Configure the range of sizes in pixels (1/96th of an inch) to map the sizes data into. If no bounds are given, and also
the `size_axis` scale is linear, then we assume the sizes data is just the size in pixels. Otherwise, by default we use
2 pixels for the `smallest` size and make the `largest` size be 8 pixels larger than the `smallest` size. Sizes must be
positive.

!!! note

    A zero size disables drawing the entity.
"""
@kwdef mutable struct SizeConfiguration <: Validated
    smallest::Maybe{Real} = nothing
    largest::Maybe{Real} = nothing
end

function Validations.validate(context::ValidationContext, size_configuration::SizeConfiguration)::Nothing
    validate_in(context, "smallest") do
        validate_is_at_least(context, size_configuration.smallest, 0)
        return nothing
    end
    validate_in(context, "largest") do
        validate_is_above(context, size_configuration.largest, 0)
        return nothing
    end
    validate_is_range(context, "smallest", size_configuration.smallest, "largest", size_configuration.largest)
    return nothing
end

"""
    @kwdef mutable struct AxisConfiguration <: Validated
        minimum::Maybe{Real} = nothing
        maximum::Maybe{Real} = nothing
        log_scale::Maybe{LogScale} = nothing
        log_regularization::Real = 0
        percent::Bool = false
    end

Generic configuration for a graph axis. Everything is optional; by default, the `minimum` and `maximum` are computed
automatically from the data.

If `log_scale` is specified, then the `log_regularization` is added to the coordinate to avoid zero values, and the axis
is shown according to the [`LogScale`](@ref). Otherwise, `log_regularization` must be 0.

If `percent` is set, then the values are multiplied by 100 and a `%` suffix is added to the tick labels.

The minimum/maximum, data values, color palette values etc. are all in the original scale. That is, you should be able
to control log scale and/or percent scaling without changing anything else.
"""
@kwdef mutable struct AxisConfiguration <: Validated
    minimum::Maybe{Real} = nothing
    maximum::Maybe{Real} = nothing
    log_scale::Maybe{LogScale} = nothing
    log_regularization::Real = 0
    percent::Bool = false
end

function Validations.validate(context::ValidationContext, axis_configuration::AxisConfiguration)::Nothing
    validate_is_range(context, "minimum", axis_configuration.minimum, "maximum", axis_configuration.maximum)

    if axis_configuration.log_scale === nothing
        if axis_configuration.log_regularization != 0
            throw(
                ArgumentError(
                    "non-zero non-log $(location(context)).log_regularization: $(axis_configuration.log_regularization)",
                ),
            )
        end
    else
        validate_in(context, "log_regularization") do
            validate_is_at_least(context, axis_configuration.log_regularization, 0)
            return nothing
        end
        if axis_configuration.minimum !== nothing
            validate_in(context, "(minimum + log_regularization)") do
                validate_is_above(context, axis_configuration.minimum + axis_configuration.log_regularization, 0)
                return nothing
            end
        end
        if axis_configuration.maximum !== nothing
            validate_in(context, "(maximum + log_regularization)") do
                validate_is_above(context, axis_configuration.maximum + axis_configuration.log_regularization, 0)
                return nothing
            end
        end
    end

    return nothing
end

"""
Styles of drawing a line

  - `SolidLine` draws a solid line (the default).

  - `DashLine` draws a dashed line.
  - `DotLine` draws a dotted line.
  - `DashDotLine` draws a dash-dotted line.
"""
@enum LineStyle SolidLine DashLine DotLine DashDotLine

"""
    @kwdef mutable struct LineConfiguration <: Validated
        width::Real = 1
        style::Maybe{LineStyle} = SolidLine
        is_filled::Bool = false
        color::Maybe{AbstractString} = nothing
    end

Configure a line in a graph.

If `style` is `nothing` no line is drawn. If `is_filled` then the region defined by the line (below it in a lines graph)
is filled. A line with no style and no filled is not drawn but can still be used to define a region (e.g., for a
[`BandsConfiguration`](@ref)).

By default, the `color` is chosen automatically.
"""
@kwdef mutable struct LineConfiguration <: Validated
    width::Real = 1
    style::Maybe{LineStyle} = SolidLine
    is_filled::Bool = false
    color::Maybe{AbstractString} = nothing
end

function Validations.validate(context::ValidationContext, line_configuration::LineConfiguration)::Nothing
    validate_in(context, "width") do
        validate_is_above(context, line_configuration.width, 0)
        return nothing
    end
    validate_in(context, "color") do
        validate_is_color(context, line_configuration.color)
        return nothing
    end
    return nothing
end

"""
    @kwdef mutable struct BandConfiguration <: Validated
        offset::Maybe{Real} = nothing
        line::LineConfiguration = LineConfiguration()
    end

Configure a region of the graph defined by some band of values.

The `offset` specifies the band's defining line position. We allow up to three bands in a complete
[`BandsConfiguration`](@ref). A band exists only if its `offset` is specified, in which case the `line` specifies how to
render its defining line. The low and high bands are defined the region below and above their defining line's `offset`.
If both are defined, the middle band `offset` defines the center line of the band; the band can still be filled even if
this offset is not specified.
"""
@kwdef mutable struct BandConfiguration <: Validated
    offset::Maybe{Real} = nothing
    line::LineConfiguration = LineConfiguration()
end

function Validations.validate(
    context::ValidationContext,
    band_configuration::BandConfiguration,
    axis_configuration::AxisConfiguration,
)::Nothing
    if axis_configuration.log_scale !== nothing
        validate_in(context, "offset") do
            validate_is_above(context, band_configuration.offset, 0)
            return nothing
        end
    end

    validate_field(context, "line", band_configuration.line)

    return nothing
end

"""
    @kwdef mutable struct BandsConfiguration <: Validated
        low::BandConfiguration = BandConfiguration(is_dashed = true)
        middle::BandConfiguration = BandConfiguration()
        high::BandConfiguration = BandConfiguration(is_dashed = true)
    end

Configure the partition of the graph up to three band regions. The `low` and `high` bands are for the "outer" regions
(so their lines are at their border, dashed by default) and the `middle` band is for the "inner" region between them (so
its line is inside it, solid by default).

If `show_legend`, then a legend showing the bands will be shown.
"""
@kwdef mutable struct BandsConfiguration <: Validated
    low::BandConfiguration = BandConfiguration(; line = LineConfiguration(; style = DotLine))
    middle::BandConfiguration = BandConfiguration()
    high::BandConfiguration = BandConfiguration(; line = LineConfiguration(; style = DashLine))
end

function Validations.validate(
    context::ValidationContext,
    bands_configuration::BandsConfiguration,
    axis_configuration::AxisConfiguration,
)::Nothing
    for (field, band_configuration) in
        (("low", bands_configuration.low), ("middle", bands_configuration.middle), ("high", bands_configuration.high))
        validate_field(context, field, band_configuration, axis_configuration)
    end

    validate_is_range(
        context,
        "low.offset",
        bands_configuration.low.offset,
        "middle.offset",
        bands_configuration.middle.offset,
    )

    validate_is_range(
        context,
        "middle.offset",
        bands_configuration.middle.offset,
        "high.offset",
        bands_configuration.high.offset,
    )

    validate_is_range(
        context,
        "low.offset",
        bands_configuration.low.offset,
        "high.offset",
        bands_configuration.high.offset,
    )

    return nothing
end

"""
    @kwdef mutable struct BandsData
        low_offset::Maybe{Real} = nothing
        middle_offset::Maybe{Real} = nothing
        high_offset::Maybe{Real} = nothing
    end

Specify data for bands.
"""
@kwdef mutable struct BandsData
    low_offset::Maybe{Real} = nothing
    middle_offset::Maybe{Real} = nothing
    high_offset::Maybe{Real} = nothing
end

"""
A continuous colors palette, mapping numeric values to colors. We also allow specifying tuples instead of pairs to make
it easy to invoke the API from other languages such as Python which do not have the concept of a `Pair`. The
`legend_title` is only used if `show_legend` is set in the configuration.
"""
ContinuousColors =
    Union{AbstractVector{<:Pair{<:Real, <:AbstractString}}, AbstractVector{<:Tuple{<:Real, <:AbstractString}}}

"""
A categorical colors palette, mapping string values to colors. An empty string color means the entity will not be shown.
"""
CategoricalColors = Dict{<:AbstractString, <:AbstractString}

"""
    @kwdef mutable struct ColorsConfiguration <: Validated
        show_legend::Bool = false
        color_axis::AxisConfiguration = AxisConfiguration()
        colors_palette::Maybe{Union{AbstractString, ContinuousColors, CategoricalColors}} = nothing
    end

Configure how to color some values. The `colors_palette` is applied; this can be:

  - The name of a standard [Plotly palette](https://plotly.com/python/builtin-colorscales/) (see [`NAMED_COLOR_PALETTES`](@ref));
  - A vector of (value, color) tuples for a continuous (numeric value) scale;
  - Or a vector of (value, color) for a categorical (string value) scale.

If `show_legend` is set, then the colors will be shown (in the legend or as a color bar, as appropriate).

The `color_axis` can be used to tweak (continuous) colors. Specifically, the values to be converted to color are
modified according to the `log_scale` and/or `percent` flags. This is also done to the values specified in an explicit
continuous colors pallete.

In Plotly's API, a color pallete is always specified to cover the values between 0 and 1, and it linearly maps some
range of colors to this unit range. You can control the mapping of the values to be colored by using `color_axis` to
modify the values (`log_scale`, `percent`) and also specify the specific range of color values to map to the unit range
(`minimum`, `maximum`).

When specifying an explict continuous colors pallete, we must map its lowest value to 0 and the highest value to 1 (and
the values must be in non-decreasing order). In this case, there's no point in specifying `minimum` and `maximum` in the
`color_axis`, as these are already encoded in the pallete itself.
"""
@kwdef mutable struct ColorsConfiguration <: Validated
    show_legend::Bool = false
    color_axis::AxisConfiguration = AxisConfiguration()
    colors_palette::Maybe{Union{AbstractString, ContinuousColors, CategoricalColors}} = nothing
end

function continuous_colors(colors::AbstractVector{<:AbstractString})::ContinuousColors  # ONLY SEEMS UNTESTED
    size = length(colors)
    return [((index - 1) / (size - 1)) => color for (index, color) in enumerate(colors)]
end

function reverse_pallete(pallete::ContinuousColors)::ContinuousColors
    return reverse!([(1 - value, color) for (value, color) in pallete])
end

function zero_pallete(
    pallete::ContinuousColors,
    value_fraction::AbstractFloat,
    color_fraction::AbstractFloat,
)::ContinuousColors
    result = Vector{Tuple{AbstractFloat, AbstractString}}()

    push!(result, (0, "white"))
    push!(result, (value_fraction - 1e-6, "white"))

    is_first = true
    previous_color = nothing
    previous_value = nothing

    for (value, color) in pallete
        if value <= color_fraction
            previous_color = color
            previous_value = value
            continue
        end

        if is_first
            is_first = false
            @assert previous_color !== nothing
            @assert previous_value !== nothing
            alpha = (color_fraction - previous_value) / (value - previous_value)
            if alpha > 1e-6
                previous_xyz = parse(XYZ, previous_color)
                first_middle_xyz = parse(XYZ, color)
                correct_xyz = previous_xyz * (1 - alpha) + first_middle_xyz * alpha
                correct_color = "#$(hex(correct_xyz))"
                push!(result, (value_fraction, correct_color))
            end
        end

        value = (value - color_fraction) * (1 - value_fraction) / (1 - color_fraction) + value_fraction
        push!(result, (value, color))
    end

    @assert !is_first
    result[1] = (0, result[1][2])
    result[end] = (1, result[end][2])
    return result
end

function center_pallete(
    pallete::ContinuousColors,
    value_fraction::AbstractFloat,
    color_fraction::AbstractFloat,
)::ContinuousColors
    result = Vector{Tuple{AbstractFloat, AbstractString}}()

    is_first_in_high = true
    is_first_in_middle = true
    previous_color = nothing
    previous_value = nothing

    for (value, color) in pallete
        if value <= 0.5 - color_fraction / 2
            previous_color = color
            previous_value = value
            push!(result, (value * (0.5 - value_fraction / 2) / (0.5 - color_fraction / 2), color))
            continue
        end

        if is_first_in_middle
            is_first_in_middle = false
            @assert previous_color !== nothing
            @assert previous_value !== nothing
            alpha = (0.5 - color_fraction / 2 - previous_value) / (value - previous_value)
            if alpha > 1e-6
                previous_xyz = parse(XYZ, previous_color)
                first_middle_xyz = parse(XYZ, color)
                correct_xyz = previous_xyz * (1 - alpha) + first_middle_xyz * alpha
                correct_color = "#$(hex(correct_xyz))"
                push!(result, (0.5 - value_fraction / 2, correct_color))
                push!(result, (0.5 - value_fraction / 2 + 1e-6, "white"))
                push!(result, (0.5 + value_fraction / 2 - 1e-6, "white"))
            end
        end

        if value < 0.5 + color_fraction / 2
            previous_color = color
            previous_value = value
            continue
        end

        if is_first_in_high
            is_first_in_high = false
            @assert previous_color !== nothing
            @assert previous_value !== nothing
            alpha = (0.5 + color_fraction / 2 - previous_value) / (value - previous_value)
            if alpha > 1e-6
                previous_xyz = parse(XYZ, previous_color)
                first_high_xyz = parse(XYZ, color)
                correct_xyz = previous_xyz * (1 - alpha) + first_high_xyz * alpha
                correct_color = "#$(hex(correct_xyz))"
                push!(result, (0.5 + value_fraction / 2, correct_color))
            end
        end

        value = (
            (value - (0.5 + color_fraction / 2)) * (1 - (0.5 + value_fraction / 2)) / (1 - (0.5 + color_fraction / 2)) +
            0.5 +
            value_fraction / 2
        )
        push!(result, (value, color))
    end

    @assert !is_first_in_high
    result[1] = (0, result[1][2])
    result[end] = (1, result[end][2])
    return result
end

function overflow_pallete(
    pallete::ContinuousColors,
    value_fraction::AbstractFloat,
    overflow_color::AbstractString,
)::ContinuousColors
    result = Vector{Tuple{AbstractFloat, AbstractString}}()

    for (value, color) in pallete
        push!(result, (value * (1 - value_fraction), color))
    end

    push!(result, (1 - value_fraction + 1e-6, overflow_color))
    push!(result, (1, overflow_color))

    result[1] = (0, result[1][2])
    result[end] = (1, result[end][2])
    return result
end

function underflow_pallete(
    pallete::ContinuousColors,
    value_fraction::AbstractFloat,
    underflow_color::AbstractString,
)::ContinuousColors
    result = Vector{Tuple{AbstractFloat, AbstractString}}()

    push!(result, (0, underflow_color))
    push!(result, (value_fraction - 1e-6, underflow_color))

    for (value, color) in pallete
        push!(result, (value * (1 - value_fraction) + value_fraction, color))
    end

    result[1] = (0, result[1][2])
    result[end] = (1, result[end][2])
    return result
end

"""
Builtin color palattes from [Plotly](https://plotly.com/python/builtin-colorscales/), both linear: `Blackbody`,
`Bluered`, `Blues`, `Cividis`, `Earth`, `Electric`, `Greens`, `Greys`, `Hot`, `Jet`, `Picnic`, `Portland`, `Rainbow`,
`RdBu`, `Reds`, `Viridis`, `YlGnBu`, `YlOrRd` and cyclical: `Twilight`, `IceFire`, `Edge`, `Phase`, `HSV`, `mrybm`,
`mygbm`.

!!! note

    You would think we could just give the builtin color palette names to plotly, but it turns out that "builtin" in
    Python plotly doesn't mean "builtin" in JavaScript plotly, because "reasons". We therefore have to copy their
    definition here. An upside of having this dictionary is that you are free to insert additional named palettes into
    and gain the convenience of refering to them by name (e.g., for coloring heatmap annotations).

A `_r` suffix specifies reversing the order of the pallete.

You can also append a final `_z:<value_fraction>:<color_fraction>` suffix to the name. This will map values in the bottom
0..`value_fraction` of the range to white, and map the rest of the values to the top `color_fraction`..1 range of the
palette. For example, `Blues_z:0.3:0.2` will color the bottom 30% of the values in white, and color the top 70% of the
values to the top 80% of the `Blues` pallete.

A `_c:<value_fraction>:<color_fraction>` works similarly to the `_z` suffix, except that the fractions are centered on
0.5 (the middle) of the values and color ranges. This is meant to be applied when the values range is +/-Diff, and the
pallete has white in the middle. For example `RdBu_c:0.3:0.2` will color the 30% of the values near the 0 middle in
white, and color the rest of the values using the top and bottom 40% of the `RdBu` pallete.

An `_o:<value_fraction>:<color>` suffix maps the pallete to the range 0..`1-value_fraction`, and all the values above this
to the `color`, to denote overflow (too high) values. For example, `Blues_o:0.01:magenta` will color all the values in
the bottom 99% of the values range to `Blues`, and the top 1% of the range to magenta. A `_u` suffix works similarly but
applies to the bottom range of the values.

You can combine multiple suffixes together, for example `Reds_z:0.2:0.2_o:0.99:magenta` or
`RdBu_r_c:0.2:0.2_o:0.01:magenta_u:0.01:darkgreen`.

Palletes with suffixes (including `_r`) are computed on the fly and cached for future use.

!!! note

    The implementation of the suffixes uses `1e-6` as a color difference "too small to matter". Don't use fractions this
    small in the prefixes or you will have a bad day.

!!! note

    Always hold the [`COLOR_PALETTES_LOCK`](@ref) when manually accessing the `NAMED_COLOR_PALETTES`, otherwise you
    *will* regret it at some point.
"""
NAMED_COLOR_PALETTES = Dict{String, ContinuousColors}([
    "Twilight" => continuous_colors([
        "#e2d9e2",
        "#9ebbc9",
        "#6785be",
        "#5e43a5",
        "#421257",
        "#471340",
        "#8e2c50",
        "#ba6657",
        "#ceac94",
        "#e2d9e2",
    ]),
    "IceFire" => continuous_colors([
        "#000000",
        "#001f4d",
        "#003786",
        "#0e58a8",
        "#217eb8",
        "#30a4ca",
        "#54c8df",
        "#9be4ef",
        "#e1e9d1",
        "#f3d573",
        "#e7b000",
        "#da8200",
        "#c65400",
        "#ac2301",
        "#820000",
        "#4c0000",
        "#000000",
    ]),
    "Edge" => continuous_colors([
        "#313131",
        "#3d019d",
        "#3810dc",
        "#2d47f9",
        "#2593ff",
        "#2adef6",
        "#60fdfa",
        "#aefdff",
        "#f3f3f1",
        "#fffda9",
        "#fafd5b",
        "#f7da29",
        "#ff8e25",
        "#f8432d",
        "#d90d39",
        "#97023d",
        "#313131",
    ]),
    "Phase" => continuous_colors([
        "rgb(167, 119, 12)",
        "rgb(197, 96, 51)",
        "rgb(217, 67, 96)",
        "rgb(221, 38, 163)",
        "rgb(196, 59, 224)",
        "rgb(153, 97, 244)",
        "rgb(95, 127, 228)",
        "rgb(40, 144, 183)",
        "rgb(15, 151, 136)",
        "rgb(39, 153, 79)",
        "rgb(119, 141, 17)",
        "rgb(167, 119, 12)",
    ]),
    "HSV" => continuous_colors([
        "#ff0000",
        "#ffa700",
        "#afff00",
        "#08ff00",
        "#00ff9f",
        "#00b7ff",
        "#0010ff",
        "#9700ff",
        "#ff00bf",
        "#ff0000",
    ]),
    "mrybm" => continuous_colors([
        "#f884f7",
        "#f968c4",
        "#ea4388",
        "#cf244b",
        "#b51a15",
        "#bd4304",
        "#cc6904",
        "#d58f04",
        "#cfaa27",
        "#a19f62",
        "#588a93",
        "#2269c4",
        "#3e3ef0",
        "#6b4ef9",
        "#956bfa",
        "#cd7dfe",
        "#f884f7",
    ]),
    "mygbm" => continuous_colors([
        "#ef55f1",
        "#fb84ce",
        "#fbafa1",
        "#fcd471",
        "#f0ed35",
        "#c6e516",
        "#96d310",
        "#61c10b",
        "#31ac28",
        "#439064",
        "#3d719a",
        "#284ec8",
        "#2e21ea",
        "#6324f5",
        "#9139fa",
        "#c543fa",
        "#ef55f1",
    ]),
    "Blackbody" =>
        continuous_colors(["rgb(0,0,0)", "rgb(230,0,0)", "rgb(230,210,0)", "rgb(255,255,255)", "rgb(160,200,255)"]),
    "Bluered" => continuous_colors(["rgb(0,0,255)", "rgb(255,0,0)"]),
    "Blues" => continuous_colors([
        "rgb(5,10,172)",
        "rgb(40,60,190)",
        "rgb(70,100,245)",
        "rgb(90,120,245)",
        "rgb(106,137,247)",
        "rgb(220,220,220)",
    ]),
    "Cividis" => continuous_colors([
        "rgb(0,32,76)",
        "rgb(0,42,102)",
        "rgb(0,52,110)",
        "rgb(39,63,108)",
        "rgb(60,74,107)",
        "rgb(76,85,107)",
        "rgb(91,95,109)",
        "rgb(104,106,112)",
        "rgb(117,117,117)",
        "rgb(131,129,120)",
        "rgb(146,140,120)",
        "rgb(161,152,118)",
        "rgb(176,165,114)",
        "rgb(192,177,109)",
        "rgb(209,191,102)",
        "rgb(225,204,92)",
        "rgb(243,219,79)",
        "rgb(255,233,69)",
    ]),
    "Earth" => continuous_colors([
        "rgb(0,0,130)",
        "rgb(0,180,180)",
        "rgb(40,210,40)",
        "rgb(230,230,50)",
        "rgb(120,70,20)",
        "rgb(255,255,255)",
    ]),
    "Electric" => continuous_colors([
        "rgb(0,0,0)",
        "rgb(30,0,100)",
        "rgb(120,0,100)",
        "rgb(160,90,0)",
        "rgb(230,200,0)",
        "rgb(255,250,220)",
    ]),
    "Greens" => continuous_colors([
        "rgb(0,68,27)",
        "rgb(0,109,44)",
        "rgb(35,139,69)",
        "rgb(65,171,93)",
        "rgb(116,196,118)",
        "rgb(161,217,155)",
        "rgb(199,233,192)",
        "rgb(229,245,224)",
        "rgb(247,252,245)",
    ]),
    "Greys" => continuous_colors(["rgb(0,0,0)", "rgb(255,255,255)"]),
    "Hot" => continuous_colors(["rgb(0,0,0)", "rgb(230,0,0)", "rgb(255,210,0)", "rgb(255,255,255)"]),
    "Jet" => continuous_colors([
        "rgb(0,0,131)",
        "rgb(0,60,170)",
        "rgb(5,255,255)",
        "rgb(255,255,0)",
        "rgb(250,0,0)",
        "rgb(128,0,0)",
    ]),
    "Picnic" => continuous_colors([
        "rgb(0,0,255)",
        "rgb(51,153,255)",
        "rgb(102,204,255)",
        "rgb(153,204,255)",
        "rgb(204,204,255)",
        "rgb(255,255,255)",
        "rgb(255,204,255)",
        "rgb(255,153,255)",
        "rgb(255,102,204)",
        "rgb(255,102,102)",
        "rgb(255,0,0)",
    ]),
    "Portland" => continuous_colors([
        "rgb(12,51,131)",
        "rgb(10,136,186)",
        "rgb(242,211,56)",
        "rgb(242,143,56)",
        "rgb(217,30,30)",
    ]),
    "Rainbow" => continuous_colors([
        "rgb(150,0,90)",
        "rgb(0,0,200)",
        "rgb(0,25,255)",
        "rgb(0,152,255)",
        "rgb(44,255,150)",
        "rgb(151,255,0)",
        "rgb(255,234,0)",
        "rgb(255,111,0)",
        "rgb(255,0,0)",
    ]),
    "RdBu" => continuous_colors([
        "rgb(5,10,172)",
        "rgb(106,137,247)",
        "rgb(190,190,190)",
        "rgb(220,170,132)",
        "rgb(230,145,90)",
        "rgb(178,10,28)",
    ]),
    "Reds" => continuous_colors(["rgb(220,220,220)", "rgb(245,195,157)", "rgb(245,160,105)", "rgb(178,10,28)"]),
    "Viridis" => continuous_colors([
        "#440154",
        "#48186a",
        "#472d7b",
        "#424086",
        "#3b528b",
        "#33638d",
        "#2c728e",
        "#26828e",
        "#21918c",
        "#1fa088",
        "#28ae80",
        "#3fbc73",
        "#5ec962",
        "#84d44b",
        "#addc30",
        "#d8e219",
        "#fde725",
    ]),
    "YlGnBu" => continuous_colors([
        "rgb(8,29,88)",
        "rgb(37,52,148)",
        "rgb(34,94,168)",
        "rgb(29,145,192)",
        "rgb(65,182,196)",
        "rgb(127,205,187)",
        "rgb(199,233,180)",
        "rgb(237,248,217)",
        "rgb(255,255,217)",
    ]),
    "YlOrRd" => continuous_colors([
        "rgb(128,0,38)",
        "rgb(189,0,38)",
        "rgb(227,26,28)",
        "rgb(252,78,42)",
        "rgb(253,141,60)",
        "rgb(254,178,76)",
        "rgb(254,217,118)",
        "rgb(255,237,160)",
        "rgb(255,255,204)",
    ]),
])

"""
A global lock to use when accessing the `NAMED_COLOR_PALETTES`. Always hold the [`COLOR_PALETTES_LOCK`](@ref) when
manually accessing the `NAMED_COLOR_PALETTES`, otherwise you *will* regret it at some point.
"""
COLOR_PALETTES_LOCK = ReentrantLock()

CACHED_COLOR_PALETTES = Dict{String, ContinuousColors}()

function Validations.validate(
    context::ValidationContext,
    colors_configuration::ColorsConfiguration,
)::Maybe{AbstractString}
    validate_field(context, "color_axis", colors_configuration.color_axis)

    colors_palette = colors_configuration.colors_palette
    if colors_palette isa AbstractString
        lock(COLOR_PALETTES_LOCK) do
            actual_palette = get(CACHED_COLOR_PALETTES, colors_palette, nothing)
            if actual_palette === nothing
                parts = split(colors_palette, "_")

                actual_palette = get(NAMED_COLOR_PALETTES, parts[1], nothing)
                if actual_palette === nothing
                    throw(ArgumentError("invalid $(location(context)).colors_palette: $(parts[1])"))
                end

                for part in parts[2:end]
                    pieces = split(part, ":")
                    try
                        if pieces[1] == "r" && length(pieces) == 1
                            actual_palette = reverse_pallete(actual_palette)
                            continue
                        elseif pieces[1] == "z" && length(pieces) == 3
                            value_fraction = parse(Float32, pieces[2])
                            color_fraction = parse(Float32, pieces[3])
                            if 0 <= value_fraction < 1 && 0 <= color_fraction < 1
                                actual_palette = zero_pallete(actual_palette, value_fraction, color_fraction)
                                continue
                            end
                        elseif pieces[1] == "c" && length(pieces) == 3
                            value_fraction = parse(Float32, pieces[2])
                            color_fraction = parse(Float32, pieces[3])
                            if 0 <= value_fraction < 1 && 0 <= color_fraction < 1
                                actual_palette = center_pallete(actual_palette, value_fraction, color_fraction)
                                continue
                            end
                        elseif pieces[1] == "o" && length(pieces) == 3
                            value_fraction = parse(Float32, pieces[2])
                            color = pieces[3]
                            parse(Colorant, color)  # NOJET
                            if 0 <= value_fraction < 1
                                actual_palette = overflow_pallete(actual_palette, value_fraction, color)
                                continue
                            end
                        elseif pieces[1] == "u" && length(pieces) == 3
                            value_fraction = parse(Float32, pieces[2])
                            color = pieces[3]
                            parse(Colorant, color)  # NOJET
                            if 0 <= value_fraction < 1
                                actual_palette = underflow_pallete(actual_palette, value_fraction, color)
                                continue
                            end
                        end
                    catch
                    end
                    throw(ArgumentError("invalid $(location(context)).colors_palette: $(colors_palette)"))
                end

                CACHED_COLOR_PALETTES[colors_palette] = actual_palette
            end
        end

    elseif colors_palette isa ContinuousColors
        validate_vector_is_not_empty(context, "colors_palette", colors_palette)  # NOJET

        values = [entry[1] for entry in colors_palette]
        for (index, (low_value, high_value)) in enumerate(zip(values[1:(end - 1)], values[2:end]))
            if low_value > high_value
                throw(
                    ArgumentError(
                        "pallete value $(location(context)).colors_palette[$(index)].value: $(low_value)\n" *
                        "is above value $(location(context)).colors_palette[$(index + 1)].value: $(high_value)",
                    ),
                )
            end
        end

        validate_vector_entries(context, "colors_palette", colors_palette) do _, (_, color)  # NOJET
            validate_in(context, "color") do
                validate_is_color(context, color)
                return nothing
            end
        end

        if colors_configuration.color_axis.minimum !== nothing
            throw(
                ArgumentError(
                    "must not specify both: explicit continuous colors $(location(context)).colors_palette\n" *
                    "and explicit $(location(context)).color_axis.minimum: $(colors_configuration.color_axis.minimum)",
                ),
            )
        end

        if colors_configuration.color_axis.maximum !== nothing
            throw(
                ArgumentError(
                    "must not specify both: explicit continuous colors $(location(context)).colors_palette\n" *
                    "and explicit $(location(context)).color_axis.maximum: $(colors_configuration.color_axis.maximum)",
                ),
            )
        end

        cmin = minimum([value for (value, _) in colors_palette])
        cmax = maximum([value for (value, _) in colors_palette])
        validate_is_range(context, "minimum(colors_palette[*].value)", cmin, "maximum(colors_palette[*].value)", cmax)

        if colors_configuration.color_axis.log_scale !== nothing
            index = argmin(colors_palette)  # NOJET
            validate_in(context, "(colors_palette[$(index)].value + color_axis.log_regularization)") do
                validate_is_above(
                    context,
                    colors_palette[index][1] + colors_configuration.color_axis.log_regularization,
                    0,
                )
                return nothing
            end
        end

    elseif colors_palette isa CategoricalColors
        validate_dict_is_not_empty(context, "colors_palette", colors_palette)  # NOJET

        validate_dict_entries(context, "colors_palette", colors_palette) do _, color  # NOJET
            validate_in(context, "color") do
                validate_is_color(context, color)
                return nothing
            end
        end
    end

    return nothing
end

"""
    @kwdef struct SubGraph
        index::Integer
        overlay::Bool
    end

Identify one sub-graph out of a set of adjacent graphs. If the `index` is 1, this is the 1st sub-graph (used top
initialize some values such as the legend group title). If `overlay` then the sub-graphs are plotted on top of each
other, which affects axis parameters.
"""
@kwdef struct SubGraph
    index::Integer
    overlay::Bool
end

end  # module
