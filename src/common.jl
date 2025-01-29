"""
Common data types and utilities for defining specific graph types.
"""
module Common

export AbstractGraphConfiguration
export AbstractGraphData
export AxisConfiguration
export BandConfiguration
export BandsConfiguration
export DashedLine
export FigureConfiguration
export Graph
export HorizontalValues
export LineConfiguration
export LineStyle
export Log10Scale
export Log2Scale
export LogScale
export MarginsConfiguration
export PlotlyFigure
export SolidLine
export ValuesOrientation
export VerticalValues
export save_graph

using Base.Multimedia
using PlotlyJS

using ..Validations

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
    validate_in(context, "data") do
        return validate(context, graph.data)
    end
    validate_in(context, "configuration") do
        return validate(context, graph.configuration)
    end
    validate_graph(graph)
    return nothing
end

"""
    validate_graph(::Graph)::Maybe{AbstractString}

Validate that the combination of data and configuration in a graph is valid, after validating each one separately. This
isn't invoked manually, instead it is called by the overall `validate` of the graph. It is provided (with a default
empty implementation) to allow for type-specific validations.
"""
function validate_graph(::Graph)::Maybe{AbstractString}
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
            return validate_is_above(context, value, 0)
        end
    end
    return nothing
end

"""
    @kwdef mutable struct FigureConfiguration <: Validated
        margins::MarginsConfiguration = MarginsConfiguration()
        width::Maybe{Int} = nothing
        height::Maybe{Int} = nothing
        template::AbstractString = "simple_white"
        show_grid::Bool = true
        grid_color::AbstractString = "lightgrey"
        show_ticks::Bool = true
    end

Generic configuration that applies to the whole figure. Each complete [`AbstractGraphConfiguration`](@ref) contains a
`figure` field of this type.

The optional `width` and `height` are in pixels, that is, 1/96 of an inch. The `margins` are specified in the same
units.

By default, `show_grid` and `show_ticks` are set.

The default `template` is "simple_white" which is the cleanest. The `show_grid` and `show_ticks` can be used to disable
the grid and/or ticks for an even cleaner (but less informative) look, and the `grid_color` can also be changed.
"""
@kwdef mutable struct FigureConfiguration <: Validated
    margins::MarginsConfiguration = MarginsConfiguration()
    width::Maybe{Int} = nothing
    height::Maybe{Int} = nothing
    template::AbstractString = "simple_white"
    show_grid::Bool = true
    grid_color::AbstractString = "lightgrey"
    show_ticks::Bool = true
end

function Validations.validate(context::ValidationContext, graph_configuration::FigureConfiguration)::Nothing
    validate_in(context, "margins") do
        return validate(context, graph_configuration.margins)
    end
    validate_in(context, "width") do
        return validate_is_above(context, graph_configuration.width, 0)
    end
    validate_in(context, "height") do
        return validate_is_above(context, graph_configuration.height, 0)
    end
    validate_in(context, "grid_color") do
        return validate_is_color(context, graph_configuration.grid_color)
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
            throw(  # UNTESTED
                ArgumentError(
                    "non-zero non-log $(location(context)).log_regularization: $(axis_configuration.log_regularization)",
                ),
            )
        end
    else
        validate_in(context, "log_regularization") do
            return validate_is_at_least(context, axis_configuration.log_regularization, 0)
        end
        if axis_configuration.minimum !== nothing
            validate_in(context, "(minimum + log_regularization)") do  # UNTESTED
                return validate_is_above(context, axis_configuration.minimum + axis_configuration.log_regularization, 0)  # UNTESTED
            end
        end
        if axis_configuration.maximum !== nothing
            validate_in(context, "(maximum + log_regularization)") do  # UNTESTED
                return validate_is_above(context, axis_configuration.maximum + axis_configuration.log_regularization, 0)  # UNTESTED
            end
        end
    end

    return nothing
end

"""
Styles of drawing a line

  - `SolidLine` draws a solid line (the default).

  - `DashedLine` draws a dashed line.
"""
@enum LineStyle SolidLine DashedLine

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
        return validate_is_above(context, line_configuration.width, 0)
    end
    validate_in(context, "color") do
        return validate_is_color(context, line_configuration.color)
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

!!! note

    Whether the offset is a part of the data or the configuration depends on the kind of graph. In some graphs the bands
    are defined by constant offsets (e.g., some maximal fold factor to mark in/significant change), in which case, the
    offset is more of a configuration parameter. In other cases the bands are defined by some quantile of the data, in
    which case the offset is more of a data parameter. Allowing for both would cause ambiguities and confusion. We
    decided to make the offset a configuration parameter for simplicity and since the first scenario is more common in
    our graphs.
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
    if axis_configuration.log_scale !== nothing && band_configuration.offset !== nothing
        validate_in(context, "(offset + log_regularization)") do  # UNTESTED
            return validate_is_above(context, band_configuration.offset + axis_configuration.log_regularization, 0)  # UNTESTED
        end
    end

    validate_in(context, "line") do
        return validate(context, band_configuration.line)
    end

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
"""
@kwdef mutable struct BandsConfiguration <: Validated
    low::BandConfiguration = BandConfiguration(; line = LineConfiguration(; style = DashedLine))
    middle::BandConfiguration = BandConfiguration()
    high::BandConfiguration = BandConfiguration(; line = LineConfiguration(; style = DashedLine))
end

function Validations.validate(
    context::ValidationContext,
    bands_configuration::BandsConfiguration,
    axis_configuration::AxisConfiguration,
)::Nothing
    for (field, band_configuration) in
        (("low", bands_configuration.low), ("middle", bands_configuration.middle), ("high", bands_configuration.high))
        validate_in(context, field) do
            return validate(context, band_configuration, axis_configuration)
        end
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

end  # module
