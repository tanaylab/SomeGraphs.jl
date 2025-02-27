"""
Graphs for showing scatter points and/or lines.
"""
module Scatters

export LineGraph
export LineGraphConfiguration
export LineGraphData
export PointsGraph
export PointsGraphConfiguration
export PointsGraphData
export ScattersConfiguration
export line_graph
export points_graph

using ..Common
using ..Utilities
using ..Validations

using PlotlyJS

import ..Utilities.Maybe

"""
    @kwdef mutable struct ScattersConfiguration <: Validated
        colors::ColorsConfiguration = ColorsConfiguration()
        sizes::SizesConfiguration() = SizesConfiguration()
    end

Configure points (or borders, which are just larger points drawn under the actual points) or edges in a scatter graph.
Point sizes are the diameter of the points. Border sizes are added to the point sizes. Edge sizes are the width of the
lines.
"""
@kwdef mutable struct ScattersConfiguration <: Validated
    colors::ColorsConfiguration = ColorsConfiguration()
    sizes::SizesConfiguration = SizesConfiguration()
end

function Validations.validate(context::ValidationContext, scatters_configuration::ScattersConfiguration)::Nothing
    validate_field(context, "colors", scatters_configuration.colors)
    validate_field(context, "sizes", scatters_configuration.sizes)
    return nothing
end

"""
    @kwdef mutable struct PointsGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        x_axis::AxisConfiguration = AxisConfiguration()
        y_axis::AxisConfiguration = AxisConfiguration()
        points::ScattersConfiguration = ScattersConfiguration()
        borders::ScattersConfiguration = ScattersConfiguration()
        edges::ScattersConfiguration = ScattersConfiguration(sizes = SizesConfiguration(smallest = 2))
        edges_over_points::Bool = true
        vertical_bands::BandsConfiguration = BandsConfiguration()
        horizontal_bands::BandsConfiguration = BandsConfiguration()
        diagonal_bands::BandsConfiguration = BandsConfiguration()
    end

Configure a graph for showing a scatter of points and/or edges.

If `edges_over_points` is set, the edges will be plotted above the points; otherwise, the points will be plotted above
the edges. Edges are plotted using the `edges_style` unless the styles are specified in the data.

The `borders` is used if the [`PointsGraphData`](@ref) contains either the `borders_colors` and/or `borders_sizes`.
This allows displaying some additional data per point.

Using the `vertical_bands`, `horizontal_bands` and/or `diagonal_bands` you can partition the graph into regions. The
`diagonal_bands` can only be used if both axes are linear or both axes are in (the same) log scale. They are parallel to
the X = Y line. For linear axes, the offset is additive, (Y = X + offset). For log scale axes, the offset is
multiplicative (Y = X * offset), and the offset must be positive. This is a rare case where we must break orthogonality
between flags, as switching between linear and log scales must be accompanied by patching the diagonal band offsets to
match.

!!! note

    There is no `show_legend` here. Instead you probably want to set the `show_legend` of the `points`, `borders` and/or
    `edges`. There's no way to create a legend for sizes or edge styles.
"""
@kwdef mutable struct PointsGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    x_axis::AxisConfiguration = AxisConfiguration()
    y_axis::AxisConfiguration = AxisConfiguration()
    points::ScattersConfiguration = ScattersConfiguration()
    borders::ScattersConfiguration = ScattersConfiguration()
    edges::ScattersConfiguration = ScattersConfiguration(; sizes = SizesConfiguration(; smallest = 2))
    edges_style::LineStyle = SolidLine
    edges_over_points::Bool = true
    vertical_bands::BandsConfiguration = BandsConfiguration()
    horizontal_bands::BandsConfiguration = BandsConfiguration()
    diagonal_bands::BandsConfiguration = BandsConfiguration()
end

function Validations.validate(context::ValidationContext, configuration::PointsGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "x_axis", configuration.x_axis)
    validate_field(context, "y_axis", configuration.y_axis)
    validate_field(context, "points", configuration.points)
    validate_field(context, "borders", configuration.borders)
    validate_field(context, "edges", configuration.edges)
    validate_field(context, "vertical_bands", configuration.vertical_bands, configuration.x_axis)
    validate_field(context, "horizontal_bands", configuration.horizontal_bands, configuration.y_axis)
    validate_field(context, "diagonal_bands", configuration.diagonal_bands, configuration.x_axis)

    if configuration.diagonal_bands.low.offset !== nothing ||
       configuration.diagonal_bands.middle.offset !== nothing ||
       configuration.diagonal_bands.high.offset !== nothing
        if configuration.x_axis.log_scale != configuration.y_axis.log_scale
            throw(ArgumentError("diagonal bands require graph.configuration.(x_axis.log_scale == y_axis.log_scale)"))
        end
        if configuration.x_axis.percent != configuration.y_axis.percent
            throw(ArgumentError("diagonal bands require graph.configuration.(x_axis.percent == y_axis.percent)"))
        end
    end

    return nothing
end

"""
    @kwdef mutable struct PointsGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        x_axis_title::Maybe{AbstractString} = nothing
        y_axis_title::Maybe{AbstractString} = nothing
        points_colors_title::Maybe{AbstractString} = nothing
        borders_colors_title::Maybe{AbstractString} = nothing
        edges_colors_title::Maybe{AbstractString} = nothing
        points_xs::AbstractVector{<:Real} = Float32[]
        points_ys::AbstractVector{<:Real} = Float32[]
        points_sizes::Maybe{AbstractVector{<:Real}} = nothing
        points_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing
        points_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
        points_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
        borders_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing
        borders_sizes::Maybe{AbstractVector{<:Real}} = nothing
        borders_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
        edges_points::Maybe{AbstractVector{<:Tuple{Integer, Integer}}} = nothing
        edges_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing
        edges_sizes::Maybe{AbstractVector{<:Real}} = nothing
        edges_styles::Maybe{AbstractVector{LineStyle}} = nothing
        edges_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing
        vertical_bands::BandsData = BandsData()
        horizontal_bands::BandsData = BandsData()
        diagonal_bands::BandsData = BandsData()
    end

The data for a scatter graph of points.

By default, all the titles are empty. You can specify the overall `figure_title` as well as the `x_axis_title` and
`y_axis_title` for the axes, and the `points_colors_title` and `borders_colors_title` for the legends.

The `points_xs` and `points_ys` vectors must be of the same size. If specified, the `points_colors`, `points_sizes`
`points_hovers` and/or `points_mask` vectors must also be of the same size. The `points_colors` can be explicit color
names if no `palette` is specified in the configuration; otherwise, they are either numeric values or category
names depending on the type of palette specified. Sizes are the diameter in pixels (1/96th of an inch). Hovers are only
shown in interactive graphs (or when saving an HTML file).

The `borders_colors`, `borders_sizes` and/or `borders_mask` vectors can be used to provide additional data per point.
The border size is in addition to the point size.

It is possible to draw straight `edges_points` between specific point pairs. In this case the `edges` of the
[`PointsGraphConfiguration`](@ref) will be used, and the `edges_colors`, `edges_sizes` (widths) and `edges_styles` will
override it per edge.

The `points_mask`, `borders_mask` and `edges_mask` allow disabling an arbitrary subset of the points, borders and/or
edges. This is often more convenient than excluding the data from the arrays. This is also useful for defining points
which are only used to draw edges between them and aren't drawn as actual points. The properties of excluded entities,
other than their coordinates, are ignored (e.g., the `points_colors` of points with a zero `points_mask` value need not
be valid color names).

The `points_colors_title`, `borders_colors_title` and `edges_colors_title` are only used if `show_legend` is set for the
relevant color configurations. You can't specify `show_legend` if the colors data contains explicit color names.
palette.
"""
@kwdef mutable struct PointsGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    x_axis_title::Maybe{AbstractString} = nothing
    y_axis_title::Maybe{AbstractString} = nothing
    points_colors_title::Maybe{AbstractString} = nothing
    borders_colors_title::Maybe{AbstractString} = nothing
    edges_colors_title::Maybe{AbstractString} = nothing
    points_xs::AbstractVector{<:Real} = Float32[]
    points_ys::AbstractVector{<:Real} = Float32[]
    points_sizes::Maybe{AbstractVector{<:Real}} = nothing
    points_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing
    points_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
    points_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing
    borders_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing
    borders_sizes::Maybe{AbstractVector{<:Real}} = nothing
    borders_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing
    edges_points::Maybe{AbstractVector{<:Tuple{Integer, Integer}}} = nothing
    edges_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing
    edges_sizes::Maybe{AbstractVector{<:Real}} = nothing
    edges_styles::Maybe{AbstractVector{LineStyle}} = nothing
    edges_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing
    vertical_bands::BandsData = BandsData()
    horizontal_bands::BandsData = BandsData()
    diagonal_bands::BandsData = BandsData()
end

function Validations.validate(context::ValidationContext, data::PointsGraphData)::Nothing
    validate_vector_is_not_empty(context, "points_xs", data.points_xs)
    n_points = length(data.points_xs)

    validate_vector_length(context, "points_ys", data.points_ys, "points_ys", n_points)
    validate_vector_length(context, "points_sizes", data.points_sizes, "points_sizes", n_points)
    validate_vector_length(context, "points_colors", data.points_colors, "points_colors", n_points)
    validate_vector_length(context, "points_hovers", data.points_sizes, "points_hovers", n_points)
    validate_vector_length(context, "points_mask", data.points_sizes, "points_mask", n_points)

    validate_vector_length(context, "borders_colors", data.borders_colors, "borders_colors", n_points)
    validate_vector_length(context, "borders_sizes", data.borders_sizes, "borders_sizes", n_points)
    validate_vector_length(context, "borders_mask", data.borders_sizes, "borders_mask", n_points)

    if data.edges_points === nothing
        n_edges = 0
    else
        n_edges = length(data.edges_points)
    end
    validate_vector_length(context, "edges_colors", data.edges_colors, "edges_points", n_edges)
    validate_vector_length(context, "edges_sizes", data.edges_colors, "edges_sizes", n_edges)
    validate_vector_length(context, "edges_styles", data.edges_colors, "edges_styles", n_edges)
    validate_vector_length(context, "edges_mask", data.edges_colors, "edges_mask", n_edges)

    validate_vector_entries(context, "edges_points", data.edges_points, data.edges_mask) do _, (from_point, to_point)
        for (field, value) in (("from_point", from_point), ("to_point", to_point))
            validate_in(context, field) do
                validate_is_at_least(context, value, 1)
                validate_is_at_most(context, value, n_points)
                return nothing
            end
        end
    end

    return nothing
end

"""
A graph visualizing scattered points (possibly with edges between them). See [`PointsGraphData`](@ref) and
[`PointsGraphConfiguration`](@ref).
"""
PointsGraph = Graph{PointsGraphData, PointsGraphConfiguration}

"""
    function points_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        x_axis_title::Maybe{AbstractString} = nothing,
        y_axis_title::Maybe{AbstractString} = nothing,
        points_colors_title::Maybe{AbstractString} = nothing,
        borders_colors_title::Maybe{AbstractString} = nothing,
        edges_colors_title::Maybe{AbstractString} = nothing,
        points_xs::AbstractVector{<:Real} = Float32[],
        points_ys::AbstractVector{<:Real} = Float32[],
        points_sizes::Maybe{AbstractVector{<:Real}} = nothing,
        points_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing,
        points_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
        points_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
        borders_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing,
        borders_sizes::Maybe{AbstractVector{<:Real}} = nothing,
        borders_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
        edges_points::Maybe{AbstractVector{<:Tuple{Integer, Integer}}} = nothing,
        edges_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing,
        edges_sizes::Maybe{AbstractVector{<:Real}} = nothing,
        edges_styles::Maybe{AbstractVector{LineStyle}} = nothing,
        edges_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
        vertical_bands::BandsData = BandsData(),
        horizontal_bands::BandsData = BandsData(),
        diagonal_bands::BandsData = BandsData()]
    )::PointsGraph

Create a [`PointsGraph`](@ref) by initializing only the [`PointsGraphData`](@ref) fields.
"""
function points_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    x_axis_title::Maybe{AbstractString} = nothing,
    y_axis_title::Maybe{AbstractString} = nothing,
    points_colors_title::Maybe{AbstractString} = nothing,
    borders_colors_title::Maybe{AbstractString} = nothing,
    edges_colors_title::Maybe{AbstractString} = nothing,
    points_xs::AbstractVector{<:Real} = Float32[],
    points_ys::AbstractVector{<:Real} = Float32[],
    points_sizes::Maybe{AbstractVector{<:Real}} = nothing,
    points_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing,
    points_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    points_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
    borders_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing,
    borders_sizes::Maybe{AbstractVector{<:Real}} = nothing,
    borders_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
    edges_points::Maybe{AbstractVector{<:Tuple{Integer, Integer}}} = nothing,
    edges_colors::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}} = nothing,
    edges_sizes::Maybe{AbstractVector{<:Real}} = nothing,
    edges_styles::Maybe{AbstractVector{LineStyle}} = nothing,
    edges_mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing,
    vertical_bands::BandsData = BandsData(),
    horizontal_bands::BandsData = BandsData(),
    diagonal_bands::BandsData = BandsData(),
)::PointsGraph
    return PointsGraph(
        PointsGraphData(;
            figure_title,
            x_axis_title,
            y_axis_title,
            points_colors_title,
            borders_colors_title,
            edges_colors_title,
            points_xs,
            points_ys,
            points_sizes,
            points_colors,
            points_hovers,
            points_mask,
            borders_colors,
            borders_sizes,
            borders_mask,
            edges_points,
            edges_colors,
            edges_sizes,
            edges_styles,
            edges_mask,
            vertical_bands,
            horizontal_bands,
            diagonal_bands,
        ),
        PointsGraphConfiguration(),
    )
end

function Common.validate_graph(graph::PointsGraph)::Nothing
    validate_colors(
        ValidationContext(["graph.data.points_colors"]),
        graph.data.points_colors,
        ValidationContext(["graph.configuration.points.colors"]),
        graph.configuration.points.colors,
        graph.data.points_mask,
    )

    validate_colors(
        ValidationContext(["graph.data.borders_colors"]),
        graph.data.borders_colors,
        ValidationContext(["graph.configuration.borders.colors"]),
        graph.configuration.borders.colors,
        graph.data.borders_mask,
    )

    validate_colors(
        ValidationContext(["graph.data.edges_colors"]),
        graph.data.edges_colors,
        ValidationContext(["graph.configuration.edges.colors"]),
        graph.configuration.edges.colors,
        graph.data.edges_mask,
    )

    n_color_scales = 0
    for colors_configuration in
        (graph.configuration.points.colors, graph.configuration.borders.colors, graph.configuration.edges.colors)
        if colors_configuration.show_legend && !(colors_configuration.palette isa CategoricalColors)
            n_color_scales += 1
        end
    end

    if n_color_scales > 2
        throw(ArgumentError("can't specify show_legend in more than two continuous color configurations"))
    end

    if graph.configuration.diagonal_bands.low.offset !== nothing ||
       graph.configuration.diagonal_bands.middle.offset !== nothing ||
       graph.configuration.diagonal_bands.high.offset !== nothing ||
       graph.data.diagonal_bands.low_offset !== nothing ||
       graph.data.diagonal_bands.middle_offset !== nothing ||
       graph.data.diagonal_bands.high_offset !== nothing
        if graph.configuration.x_axis.log_scale !== graph.configuration.y_axis.log_scale
            throw(ArgumentError("diagonal bands require graph.configuration.(x_axis.log_scale == y_axis.log_scale)"))
        end
        if graph.configuration.x_axis.percent !== graph.configuration.y_axis.percent
            throw(ArgumentError("diagonal bands require graph.configuration.(x_axis.percent == y_axis.percent)"))
        end
    end

    validate_graph_bands(
        "vertical_bands",
        graph.configuration.vertical_bands,
        graph.data.vertical_bands,
        graph.configuration.x_axis,
    )
    validate_graph_bands(
        "horizontal_bands",
        graph.configuration.horizontal_bands,
        graph.data.horizontal_bands,
        graph.configuration.y_axis,
    )
    validate_graph_bands(
        "diagonal_bands",
        graph.configuration.diagonal_bands,
        graph.data.diagonal_bands,
        graph.configuration.x_axis,
    )

    return nothing
end

@kwdef struct ScaledData
    values::AbstractVector{<:Real}
    range::AbstractVector{<:Real}
end

function scaled_data(axis_configuration::AxisConfiguration, values::Maybe{AbstractVector{<:Real}})::ScaledData
    scaled_values = scale_axis_values(axis_configuration, values)
    implicit_scaled_range = [minimum(scaled_values), maximum(scaled_values)]
    expand_range!(implicit_scaled_range)
    scaled_range = final_scaled_range(implicit_scaled_range, axis_configuration)
    return ScaledData(; values = scaled_values, range = scaled_range)
end

@kwdef mutable struct ConfiguredScatters
    legend_group::AbstractString
    has_legend::Bool
    colors_title::Maybe{AbstractString}
    colors_configuration::ColorsConfiguration
    colors_scale::Maybe{AbstractString}
    original_color_values::Maybe{AbstractVector{<:AbstractString}}
    final_colors_values::Maybe{Union{AbstractVector{<:AbstractString}, AbstractVector{<:Real}}}
    final_colors_range::Maybe{AbstractVector{<:Real}}
    scaled_colors_palette::Maybe{AbstractVector{<:Tuple{Real, AbstractString}}}
    pixel_size::Maybe{Real}
    pixel_sizes::Maybe{AbstractVector{<:Real}}
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}}
end

function configured_scatters(;
    legend_group::AbstractString,
    scatters_configuration::ScattersConfiguration,
    colors_title::Maybe{AbstractString},
    colors_values::Maybe{Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}}},
    colors_scales::AbstractVector{<:AbstractString},
    size_values::Maybe{AbstractVector{<:Real}},
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
)::ConfiguredScatters
    scaled_colors_palette = nothing
    if colors_values isa AbstractVector{<:Real}
        colors_scale = pop!(colors_scales)
        original_color_values = nothing
        final_colors_values = scale_axis_values(scatters_configuration.colors.axis, colors_values)
        if scatters_configuration.colors.palette isa ContinuousColors
            color_palette_values = [entry[1] for entry in scatters_configuration.colors.palette]
            scaled_colors_palette_values = scale_axis_values(scatters_configuration.colors.axis, color_palette_values)
            implicit_scaled_colors_range = [scaled_colors_palette_values[1], scaled_colors_palette_values[end]]
            final_colors_range = final_scaled_range(implicit_scaled_colors_range, scatters_configuration.colors.axis)

            scale = implicit_scaled_colors_range[2] - implicit_scaled_colors_range[1]
            @assert scale > 0
            final_color_palette_values = (scaled_colors_palette_values .- implicit_scaled_colors_range[1]) ./ scale
            final_color_palette_values[1] = 0
            final_color_palette_values[end] = 1
            scaled_colors_palette = [  # NOJET
                (final_value, entry[2]) for
                (final_value, entry) in zip(final_color_palette_values, scatters_configuration.colors.palette)
            ]
        else
            implicit_scaled_colors_range = [minimum(final_colors_values), maximum(final_colors_values)]
            final_colors_range = final_scaled_range(implicit_scaled_colors_range, scatters_configuration.colors.axis)
        end

    elseif colors_values isa AbstractVector{<:AbstractString}
        final_colors_range = nothing
        colors_scale = nothing

        if scatters_configuration.colors.palette isa CategoricalColors
            @assert colors_values isa AbstractVector{<:AbstractString}
            original_color_values = colors_values
            final_colors_values = [
                prefer_data(mask, index, true) ? scatters_configuration.colors.palette[color] : "masked" for
                (index, color) in enumerate(colors_values)
            ]
        else
            original_color_values = nothing
            @assert scatters_configuration.colors.palette === nothing
            final_colors_values = colors_values
        end

    else
        colors_scale = nothing
        @assert colors_values === nothing
        final_colors_values = nothing
        final_colors_range = nothing
        original_color_values = nothing
    end

    pixel_size = scatters_configuration.sizes.fixed
    if pixel_size === nothing
        pixel_sizes = scale_size_values(scatters_configuration.sizes, size_values)
        if pixel_sizes === nothing
            pixel_size = scatters_configuration.sizes.smallest
        end
    else
        pixel_sizes = nothing
    end

    has_legend =
        scatters_configuration.colors.show_legend && scatters_configuration.colors.palette isa CategoricalColors

    return ConfiguredScatters(;
        legend_group,
        has_legend,
        colors_title,
        colors_configuration = scatters_configuration.colors,
        colors_scale,
        original_color_values,
        final_colors_values,
        final_colors_range,
        scaled_colors_palette,
        pixel_size,
        pixel_sizes,
        mask,
    )
end

function Common.graph_to_figure(graph::PointsGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    scaled_points_xs = scaled_data(graph.configuration.x_axis, graph.data.points_xs)
    scaled_points_ys = scaled_data(graph.configuration.y_axis, graph.data.points_ys)

    colors_scales = ["coloraxis", "coloraxis2"]

    configured_points = configured_scatters(;
        legend_group = "Points",
        scatters_configuration = graph.configuration.points,
        colors_title = graph.data.points_colors_title,
        colors_values = graph.data.points_colors,
        colors_scales,
        size_values = graph.data.points_sizes,
        mask = graph.data.points_mask,
    )

    configured_borders = configured_scatters(;
        legend_group = "Borders",
        scatters_configuration = graph.configuration.borders,
        colors_title = graph.data.borders_colors_title,
        colors_values = graph.data.borders_colors,
        colors_scales,
        size_values = graph.data.borders_sizes,
        mask = graph.data.borders_mask,
    )

    add_pixel_sizes(configured_points, configured_borders)

    configured_edges = configured_scatters(;
        legend_group = "Edges",
        scatters_configuration = graph.configuration.edges,
        colors_title = graph.data.edges_colors_title,
        colors_values = graph.data.edges_colors,
        colors_scales,
        size_values = graph.data.edges_sizes,
        mask = graph.data.edges_mask,
    )

    edges_points = graph.data.edges_points
    if edges_points !== nothing && !graph.configuration.edges_over_points
        push_edge_traces!(; traces, graph, scaled_points_xs, scaled_points_ys, configured_edges)
    end

    if graph.data.borders_colors !== nothing ||
       graph.data.borders_sizes !== nothing ||
       graph.data.borders_mask !== nothing ||
       graph.configuration.borders.colors.fixed !== nothing ||
       graph.configuration.borders.sizes.fixed !== nothing
        push_points_traces!(; traces, graph, scaled_points_xs, scaled_points_ys, configured_points = configured_borders)
    end

    push_points_traces!(; traces, graph, scaled_points_xs, scaled_points_ys, configured_points)

    if edges_points !== nothing && graph.configuration.edges_over_points
        push_edge_traces!(; traces, graph, scaled_points_xs, scaled_points_ys, configured_edges)
    end

    shapes = Shape[]

    push_vertical_bands_shapes(
        shapes,
        graph.configuration.x_axis,
        scaled_points_xs.range,
        graph.data.vertical_bands,
        graph.configuration.vertical_bands,
    )

    push_horizontal_bands_shapes(
        shapes,
        graph.configuration.y_axis,
        scaled_points_ys.range,
        graph.data.horizontal_bands,
        graph.configuration.horizontal_bands,
    )

    push_diagonal_bands_shapes(
        shapes,
        graph.configuration.x_axis,
        scaled_points_xs.range,
        scaled_points_ys.range,
        graph.data.diagonal_bands,
        graph.configuration.diagonal_bands,
    )

    has_legend = configured_points.has_legend || configured_borders.has_legend || configured_edges.has_legend
    layout = points_layout(; graph, scaled_points_xs, scaled_points_ys, shapes, show_legend = has_legend)
    color_offset_index = Int(has_legend)

    for configured in (configured_points, configured_borders, configured_edges)
        if configured.colors_scale !== nothing
            set_layout_colorscale!(;
                layout,
                colors_scale = configured.colors_scale,
                colors_configuration = configured.colors_configuration,
                scaled_colors_palette = configured.scaled_colors_palette,
                offset = if !configured.colors_configuration.show_legend || color_offset_index == 0
                    nothing
                else
                    graph.configuration.figure.color_scale_offsets[color_offset_index]
                end,
                range = configured.final_colors_range,
                title = configured.colors_title,
                show_legend = configured.colors_configuration.show_legend,
            )
            if configured.colors_configuration.show_legend
                color_offset_index += 1
            end
        end
    end

    return plotly_figure(traces, layout)
end

function add_pixel_sizes(configured_points::ConfiguredScatters, configured_borders::ConfiguredScatters)::Nothing
    if configured_points.pixel_size !== nothing
        @assert configured_points.pixel_sizes === nothing

        if configured_borders.pixel_size !== nothing
            @assert configured_borders.pixel_sizes === nothing
            configured_borders.pixel_size += configured_points.pixel_size
        else
            @assert configured_borders.pixel_sizes !== nothing
            configured_borders.pixel_sizes .+= configured_points.pixel_size  # NOJET
        end

    else
        @assert configured_points.pixel_sizes !== nothing

        if configured_borders.pixel_size !== nothing
            @assert configured_borders.pixel_sizes === nothing
            configured_borders.pixel_sizes = configured_borders.pixel_size .+ configured_points.pixel_sizes
            configured_borders.pixel_size = nothing
        else
            @assert configured_borders.pixel_sizes !== nothing
            configured_borders.pixel_sizes .+= configured_points.pixel_sizes
        end
    end

    return nothing
end

function push_edge_traces!(;
    traces::AbstractVector{GenericTrace},
    graph::PointsGraph,
    scaled_points_xs::ScaledData,
    scaled_points_ys::ScaledData,
    configured_edges::ConfiguredScatters,
)::Nothing
    if configured_edges.colors_configuration.show_legend &&
       graph.data.edges_colors isa AbstractVector{<:AbstractString} &&
       configured_edges.colors_configuration.palette isa CategoricalColors
        edges_names = graph.data.edges_colors
    else
        edges_names = nothing
    end

    edges_points = graph.data.edges_points
    @assert edges_points !== nothing
    if edges_names !== nothing
        seen_names = Set{AbstractString}()
    end
    legend_group_title = configured_edges.colors_title
    for (index, (from_point, to_point)) in enumerate(edges_points)
        if prefer_data(configured_edges.mask, index, true)
            name = prefer_data(edges_names, index, nothing)
            show_legend = configured_edges.colors_configuration.show_legend

            if name === nothing
                legend_group = "Edges"
            else
                legend_group = "Edges $(name)"
                if name in seen_names
                    show_legend = false
                else
                    push!(seen_names, name)
                end
            end

            push!(  # NOJET
                traces,
                scatter(;
                    x = [scaled_points_xs.values[from_point], scaled_points_xs.values[to_point]],
                    y = [scaled_points_ys.values[from_point], scaled_points_ys.values[to_point]],
                    line_width = prefer_data(configured_edges.pixel_sizes, index, configured_edges.pixel_size),
                    line_color = prefer_data(
                        prefer_data(
                            configured_edges.final_colors_values,
                            index,
                            configured_edges.colors_configuration.fixed,
                        ),
                        "darkgrey",
                    ),
                    line_dash = plotly_line_dash(
                        prefer_data(graph.data.edges_styles, index, graph.configuration.edges_style),
                    ),
                    name = prefer_data(edges_names, index, nothing),
                    mode = "lines",
                    legendgroup = legend_group,
                    legendgrouptitle_text = legend_group_title,
                    showlegend = show_legend,
                    coloraxis = "coloraxis3",
                ),
            )
            legend_group_title = nothing
        end
    end

    return nothing
end

function push_points_traces!(;
    traces::AbstractVector{GenericTrace},
    graph::PointsGraph,
    scaled_points_xs::ScaledData,
    scaled_points_ys::ScaledData,
    configured_points::ConfiguredScatters,
)::Nothing
    if configured_points.final_colors_values !== nothing &&
       configured_points.colors_configuration.palette isa CategoricalColors
        is_first = true
        for (name, color) in configured_points.colors_configuration.palette
            mask = configured_points.original_color_values .== name
            if configured_points.mask !== nothing
                mask .&= configured_points.mask
            end
            if any(mask)
                push_points_trace!(;
                    traces,
                    scaled_points_xs,
                    scaled_points_ys,
                    points_hovers = graph.data.points_hovers,
                    configured_points,
                    mask,
                    name,
                    color,
                    legend_group_suffix = name,
                    is_first,
                )
                is_first = false
            end
        end
    else
        push_points_trace!(;
            traces,
            scaled_points_xs,
            scaled_points_ys,
            points_hovers = graph.data.points_hovers,
            configured_points,
            mask = configured_points.mask,
            name = nothing,
            color = prefer_data(configured_points.final_colors_values, configured_points.colors_configuration.fixed),
        )
    end

    return nothing
end

"""
    @kwdef mutable struct LineGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        x_axis::AxisConfiguration = AxisConfiguration()
        y_axis::AxisConfiguration = AxisConfiguration()
        line_width::Maybe{Real} = nothing
        line_color::Maybe{AbstractString} = nothing
        line_style::LineStyle = SolidLine
        show_points::Bool = false
        points_size::Maybe{Real} = nothing
        points_color::Maybe{AbstractString} = nothing
        vertical_bands::BandsConfiguration = BandsConfiguration()
        horizontal_bands::BandsConfiguration = BandsConfiguration()
        diagonal_bands::BandsConfiguration = BandsConfiguration()
    end

Configure a graph for showing a single line.

The `line_width` and `line_color` are chosen automatically by default. If `show_points` is set, so are `points_color`
and `points_size`. The bands are similar to [`PointsGraphConfiguration`](@ref).
"""
@kwdef mutable struct LineGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    x_axis::AxisConfiguration = AxisConfiguration()
    y_axis::AxisConfiguration = AxisConfiguration()
    line_width::Maybe{Real} = nothing
    line_color::Maybe{AbstractString} = nothing
    line_style::LineStyle = SolidLine
    show_points::Bool = false
    points_size::Maybe{Real} = nothing
    points_color::Maybe{AbstractString} = nothing
    vertical_bands::BandsConfiguration = BandsConfiguration()
    horizontal_bands::BandsConfiguration = BandsConfiguration()
    diagonal_bands::BandsConfiguration = BandsConfiguration()
end

function Validations.validate(context::ValidationContext, configuration::LineGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "x_axis", configuration.x_axis)
    validate_field(context, "y_axis", configuration.y_axis)

    validate_in(context, "line_color") do
        return validate_is_color(context, configuration.line_color)
    end
    validate_in(context, "points_size") do
        return validate_is_above(context, configuration.points_size, 0)
    end
    validate_in(context, "points_color") do
        return validate_is_color(context, configuration.points_color)
    end

    validate_field(context, "vertical_bands", configuration.vertical_bands, configuration.x_axis)
    validate_field(context, "horizontal_bands", configuration.horizontal_bands, configuration.y_axis)
    validate_field(context, "diagonal_bands", configuration.diagonal_bands, configuration.x_axis)

    if !configuration.show_points
        if configuration.points_size !== nothing
            throw(ArgumentError("can't specify $(location(context)).points_size w/o $(location(context)).show_points"))
        end

        if configuration.points_color !== nothing
            throw(ArgumentError("can't specify $(location(context)).points_color w/o $(location(context)).show_points"))
        end
    end

    if configuration.diagonal_bands.low.offset !== nothing ||
       configuration.diagonal_bands.middle.offset !== nothing ||
       configuration.diagonal_bands.high.offset !== nothing
        if configuration.x_axis.log_scale != configuration.y_axis.log_scale
            throw(ArgumentError("diagonal bands require graph.configuration.(x_axis.log_scale == y_axis.log_scale)"))
        end
        if configuration.x_axis.percent != configuration.y_axis.percent
            throw(ArgumentError("diagonal bands require graph.configuration.(x_axis.percent == y_axis.percent)"))
        end
    end

    return nothing
end

"""
    @kwdef mutable struct LineGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        x_axis_title::Maybe{AbstractString} = nothing
        y_axis_title::Maybe{AbstractString} = nothing
        points_xs::AbstractVector{<:Real} = Float32[]
        points_ys::AbstractVector{<:Real} = Float32[]
        points_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
        vertical_bands::BandsData = BandsData()
        horizontal_bands::BandsData = BandsData()
        diagonal_bands::BandsData = BandsData()
    end

The data for a single line graph.

By default, all the titles are empty. You can specify the overall `figure_title` as well as the `x_axis_title` and
`y_axis_title` for the axes.

The `points_xs` and `points_ys` vectors must be of the same size. If specified, the `points_hovers` vector must also be
of the same size.
"""
@kwdef mutable struct LineGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    x_axis_title::Maybe{AbstractString} = nothing
    y_axis_title::Maybe{AbstractString} = nothing
    points_xs::AbstractVector{<:Real} = Float32[]
    points_ys::AbstractVector{<:Real} = Float32[]
    points_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing
    vertical_bands::BandsData = BandsData()
    horizontal_bands::BandsData = BandsData()
    diagonal_bands::BandsData = BandsData()
end

function Validations.validate(context::ValidationContext, data::LineGraphData)::Nothing
    validate_vector_is_not_empty(context, "points_xs", data.points_xs)
    n_points = length(data.points_xs)

    validate_vector_length(context, "points_ys", data.points_ys, "points_ys", n_points)
    validate_vector_length(context, "points_hovers", data.points_hovers, "points_hovers", n_points)

    return nothing
end

"""
A graph showing a single line. See [`LineGraphData`](@ref) and [`LineGraphConfiguration`](@ref).
"""
LineGraph = Graph{LineGraphData, LineGraphConfiguration}

"""
    function line_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        x_axis_title::Maybe{AbstractString} = nothing,
        y_axis_title::Maybe{AbstractString} = nothing,
        points_xs::AbstractVector{<:Real} = Float32[],
        points_ys::AbstractVector{<:Real} = Float32[],
        points_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
        vertical_bands::BandsData = BandsData(),
        horizontal_bands::BandsData = BandsData(),
        diagonal_bands::BandsData = BandsData()]
    )::LineGraph

Create a [`LineGraph`](@ref) by initializing only the [`LineGraphData`](@ref) fields.
"""
function line_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    x_axis_title::Maybe{AbstractString} = nothing,
    y_axis_title::Maybe{AbstractString} = nothing,
    points_xs::AbstractVector{<:Real} = Float32[],
    points_ys::AbstractVector{<:Real} = Float32[],
    points_hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    vertical_bands::BandsData = BandsData(),
    horizontal_bands::BandsData = BandsData(),
    diagonal_bands::BandsData = BandsData(),
)::LineGraph
    return LineGraph(
        LineGraphData(;
            figure_title,
            x_axis_title,
            y_axis_title,
            points_xs,
            points_ys,
            points_hovers,
            vertical_bands,
            horizontal_bands,
            diagonal_bands,
        ),
        LineGraphConfiguration(),
    )
end

function Common.validate_graph(graph::LineGraph)::Nothing
    if graph.configuration.diagonal_bands.low.offset !== nothing ||
       graph.configuration.diagonal_bands.middle.offset !== nothing ||
       graph.configuration.diagonal_bands.high.offset !== nothing ||
       graph.data.diagonal_bands.low_offset !== nothing ||
       graph.data.diagonal_bands.middle_offset !== nothing ||
       graph.data.diagonal_bands.high_offset !== nothing
        if graph.configuration.x_axis.log_scale !== graph.configuration.y_axis.log_scale
            throw(ArgumentError("diagonal bands require graph.configuration.(x_axis.log_scale == y_axis.log_scale)"))
        end
        if graph.configuration.x_axis.percent !== graph.configuration.y_axis.percent
            throw(ArgumentError("diagonal bands require graph.configuration.(x_axis.percent == y_axis.percent)"))
        end
    end

    validate_graph_bands(
        "vertical_bands",
        graph.configuration.vertical_bands,
        graph.data.vertical_bands,
        graph.configuration.x_axis,
    )
    validate_graph_bands(
        "horizontal_bands",
        graph.configuration.horizontal_bands,
        graph.data.horizontal_bands,
        graph.configuration.y_axis,
    )
    validate_graph_bands(
        "diagonal_bands",
        graph.configuration.diagonal_bands,
        graph.data.diagonal_bands,
        graph.configuration.x_axis,
    )

    return nothing
end

function Common.graph_to_figure(graph::LineGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    scaled_points_xs = scaled_data(graph.configuration.x_axis, graph.data.points_xs)
    scaled_points_ys = scaled_data(graph.configuration.y_axis, graph.data.points_ys)

    traces = Vector{GenericTrace}()

    configured_points = ConfiguredScatters(;
        legend_group = "Line",
        has_legend = false,
        colors_title = nothing,
        colors_configuration = ColorsConfiguration(),
        colors_scale = nothing,
        original_color_values = nothing,
        final_colors_values = nothing,
        final_colors_range = nothing,
        scaled_colors_palette = nothing,
        pixel_size = graph.configuration.points_size,
        pixel_sizes = nothing,
        mask = nothing,
    )

    push_points_trace!(;
        traces,
        points_hovers = graph.data.points_hovers,
        scaled_points_xs,
        scaled_points_ys,
        configured_points,
        mask = nothing,
        name = nothing,
        color = graph.configuration.points_color,
        mode = graph.configuration.show_points ? "lines+markers" : "lines",
        line_width = graph.configuration.line_width,
        line_color = graph.configuration.line_color,
        line_style = graph.configuration.line_style,
    )

    shapes = Shape[]

    push_vertical_bands_shapes(
        shapes,
        graph.configuration.x_axis,
        scaled_points_xs.range,
        graph.data.vertical_bands,
        graph.configuration.vertical_bands,
    )

    push_horizontal_bands_shapes(
        shapes,
        graph.configuration.y_axis,
        scaled_points_ys.range,
        graph.data.horizontal_bands,
        graph.configuration.horizontal_bands,
    )

    push_diagonal_bands_shapes(
        shapes,
        graph.configuration.x_axis,
        scaled_points_xs.range,
        scaled_points_ys.range,
        graph.data.diagonal_bands,
        graph.configuration.diagonal_bands,
    )

    layout = points_layout(; graph, scaled_points_xs, scaled_points_ys, shapes, show_legend = false)

    return plotly_figure(traces, layout)
end

function push_points_trace!(;
    traces::AbstractVector{GenericTrace},
    scaled_points_xs::ScaledData,
    scaled_points_ys::ScaledData,
    points_hovers::Maybe{AbstractVector{<:AbstractString}},
    configured_points::ConfiguredScatters,
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
    name::Maybe{AbstractString},
    color::Maybe{Union{AbstractString, AbstractVector{<:Real}, AbstractVector{<:AbstractString}}},
    legend_group_suffix::Maybe{AbstractString} = nothing,
    is_first = true,
    mode::AbstractString = "markers",
    line_width::Maybe{Real} = nothing,
    line_color::Maybe{AbstractString} = nothing,
    line_style::Maybe{LineStyle} = nothing,
)::Nothing
    if color isa AbstractVector
        color = masked_values(color, mask)
        is_colors_scale = color isa AbstractVector{<:Real}
    else
        is_colors_scale = false
    end

    if legend_group_suffix === nothing
        legend_group = configured_points.legend_group
    else
        legend_group = "$(configured_points.legend_group) $(legend_group_suffix)"
    end

    hovers = masked_values(points_hovers, mask)

    push!(  # NOJET
        traces,
        scatter(;
            x = masked_values(scaled_points_xs.values, mask),
            y = masked_values(scaled_points_ys.values, mask),
            text = hovers,
            marker_size = prefer_data(masked_values(configured_points.pixel_sizes, mask), configured_points.pixel_size),
            marker_color = prefer_data(color, configured_points.colors_configuration.fixed),
            marker_coloraxis = configured_points.colors_scale,
            marker_showscale = configured_points.colors_configuration.show_legend && is_colors_scale,
            legendgroup = legend_group,
            legendgrouptitle_text = is_first ? configured_points.colors_title : nothing,
            showlegend = configured_points.colors_configuration.show_legend && !is_colors_scale,
            name = name,
            hovertemplate = hovers === nothing ? nothing : "%{text}<extra></extra>",
            mode,
            line_color,
            line_width,
            line_dash = plotly_line_dash(line_style),
        ),
    )

    return nothing
end

function masked_values(::Nothing, ::Any)::Any
    return nothing
end

function masked_values(::Nothing, ::Nothing)::Nothing
    return nothing
end

function masked_values(values::AbstractVector{T}, ::Nothing)::AbstractVector{T} where {T}
    return values
end

function masked_values(
    values::AbstractVector{T},
    mask::Union{AbstractVector{Bool}, BitVector},
)::AbstractVector{T} where {T}
    return values[mask]
end

function points_layout(;
    graph::Union{PointsGraph, LineGraph},
    scaled_points_xs::ScaledData,
    scaled_points_ys::ScaledData,
    shapes::AbstractVector{Shape},
    show_legend::Bool,
)::Layout
    layout =
        plotly_layout(graph.configuration.figure; title = graph.data.figure_title, showlegend = show_legend, shapes)

    set_layout_axis!(
        layout,
        "xaxis",
        graph.configuration.x_axis;
        title = graph.data.x_axis_title,
        range = scaled_points_xs.range,
    )

    set_layout_axis!(
        layout,
        "yaxis",
        graph.configuration.y_axis;
        title = graph.data.y_axis_title,
        range = scaled_points_ys.range,
    )

    return layout
end

end  # module
