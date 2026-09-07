"""
Graphs for showing scatter points and/or lines.
"""
module Scatters

export BordersData
export EdgesData
export LineGraph
export LineGraphConfiguration
export LineGraphData
export LinesGraph
export LinesGraphConfiguration
export LinesGraphData
export PointsData
export PointsGraph
export PointsGraphConfiguration
export PointsGraphData
export ScattersConfiguration
export line_graph
export lines_graph
export points_density
export points_graph

using ..Common
using ..Utilities
using ..Validations

using KernelDensity
using NamedArrays
using PlotlyBase

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
        x_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
        y_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
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

!!! note

    Continuous colors for edges are not implemented due to the difficulty of getting Plotly to render them, and given
    we didn't find (m)any use cases for them.
"""
@kwdef mutable struct PointsGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    x_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
    y_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
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

    if configuration.edges.colors.palette isa ContinuousColors || configuration.edges.colors.palette isa AbstractString
        throw(ArgumentError("continuous colors for edges are not implemented"))
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
    @kwdef mutable struct PointsData
        colors::ValuesData = ValuesData()
        sizes::ValuesData = ValuesData()
        entities::EntitiesData = EntitiesData()
        order::Maybe{AbstractVector{<:Integer}} = nothing
    end

The per-point data of a [`PointsGraphData`](@ref), other than the coordinates: the `colors` and `sizes` values, the
hovers and mask of the points (`entities`), and the `order` the points are drawn in. It mirrors the `points` of the
[`PointsGraphConfiguration`](@ref).
"""
@kwdef mutable struct PointsData
    colors::ValuesData = ValuesData()
    sizes::ValuesData = ValuesData()
    entities::EntitiesData = EntitiesData()
    order::Maybe{AbstractVector{<:Integer}} = nothing
end

"""
    @kwdef mutable struct BordersData
        colors::ValuesData = ValuesData()
        sizes::ValuesData = ValuesData()
        mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing
    end

The per-point data of the borders of a [`PointsGraphData`](@ref): the `colors` and `sizes` values, and the `mask` of
the borders. Borders share the hovers and order of the points. It mirrors the `borders` of the
[`PointsGraphConfiguration`](@ref).
"""
@kwdef mutable struct BordersData
    colors::ValuesData = ValuesData()
    sizes::ValuesData = ValuesData()
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}} = nothing
end

"""
    @kwdef mutable struct EdgesData
        points::Maybe{AbstractVector{<:Tuple{Integer, Integer}}} = nothing
        colors::ValuesData = ValuesData()
        sizes::ValuesData = ValuesData()
        styles::Maybe{AbstractVector{LineStyle}} = nothing
        entities::EntitiesData = EntitiesData()
        order::Maybe{AbstractVector{<:Integer}} = nothing
    end

The edges of a [`PointsGraphData`](@ref): straight lines between pairs of `points` (given by their indices). The
`colors`, `sizes` (widths) and `styles` override the `edges` of the [`PointsGraphConfiguration`](@ref) per edge. The
`entities` hold the hovers and mask of the edges. If `order` is specified, the edges are drawn in that order.

!!! note

    Continuous colors for edges are not implemented due to the difficulty of getting Plotly to render them, and given
    we didn't find (m)any use cases for them.
"""
@kwdef mutable struct EdgesData
    points::Maybe{AbstractVector{<:Tuple{Integer, Integer}}} = nothing
    colors::ValuesData = ValuesData()
    sizes::ValuesData = ValuesData()
    styles::Maybe{AbstractVector{LineStyle}} = nothing
    entities::EntitiesData = EntitiesData()
    order::Maybe{AbstractVector{<:Integer}} = nothing
end

"""
    @kwdef mutable struct PointsGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        x::ValuesData = ValuesData()
        y::ValuesData = ValuesData()
        points::PointsData = PointsData()
        borders::BordersData = BordersData()
        edges::EdgesData = EdgesData()
        vertical_bands::BandsData = BandsData()
        horizontal_bands::BandsData = BandsData()
        diagonal_bands::BandsData = BandsData()
    end

The data for a scatter graph of points.

The `x` and `y` values are required, numeric, and of the same size (the number of points); their titles are the axis
titles. If specified, the `points` and `borders` colors and sizes values must also be of the same size, as must the
hovers and masks. Colors can be explicit color names if no `palette` is specified in the configuration; otherwise, they
are either numeric values or category names depending on the type of palette specified. Sizes are the diameter in
pixels (1/96th of an inch); border sizes are added to the point sizes. The colors titles are used for the legends, if
`show_legend` is set for the relevant colors configuration (you can't specify `show_legend` if the colors data
contains explicit color names).

The `edges` draw straight lines between pairs of points; see [`EdgesData`](@ref).

The masks of the points, borders and edges allow disabling an arbitrary subset of them. This is often more convenient
than excluding the data from the arrays. This is also useful for defining points which are only used to draw edges
between them and aren't drawn as actual points. The properties of excluded entities, other than their coordinates, are
ignored (e.g., the colors of masked points need not be valid color names).

If the points and/or edges `order` is specified, we reorder the points and/or edges accordingly. This allows
controlling which points and/or edges will appear on top of the others. Due to Plotly limitations, when using
categorical colors, all the points (or edges) of one category must all be either above or below all the points of each
other category. We therefore compute an overall priority for each category as the mean reordered index of all the points
(or edges) of that category, and reorder the categories based on that.
"""
@kwdef mutable struct PointsGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    x::ValuesData = ValuesData()
    y::ValuesData = ValuesData()
    points::PointsData = PointsData()
    borders::BordersData = BordersData()
    edges::EdgesData = EdgesData()
    vertical_bands::BandsData = BandsData()
    horizontal_bands::BandsData = BandsData()
    diagonal_bands::BandsData = BandsData()
end

function Validations.validate(context::ValidationContext, data::PointsGraphData)::Nothing
    validate_numeric_values(context, "x.values", data.x.values; is_required = true)
    validate_numeric_values(context, "y.values", data.y.values; is_required = true)
    validate_numeric_values(context, "points.sizes.values", data.points.sizes.values)
    validate_numeric_values(context, "borders.sizes.values", data.borders.sizes.values)
    validate_numeric_values(context, "edges.sizes.values", data.edges.sizes.values)

    xs = data.x.values
    @assert xs !== nothing
    validate_vector_is_not_empty(context, "x.values", xs)
    n_points = length(xs)

    validate_vector_length(context, "y.values", data.y.values, "x.values", n_points)

    points = data.points
    validate_vector_length(context, "points.colors.values", points.colors.values, "x.values", n_points)
    validate_vector_length(context, "points.sizes.values", points.sizes.values, "x.values", n_points)
    validate_vector_length(context, "points.entities.hovers", points.entities.hovers, "x.values", n_points)
    validate_vector_length(context, "points.entities.mask", points.entities.mask, "x.values", n_points)
    validate_vector_length(context, "points.order", points.order, "x.values", n_points)

    borders = data.borders
    validate_vector_length(context, "borders.colors.values", borders.colors.values, "x.values", n_points)
    validate_vector_length(context, "borders.sizes.values", borders.sizes.values, "x.values", n_points)
    validate_vector_length(context, "borders.mask", borders.mask, "x.values", n_points)

    edges = data.edges
    if edges.points === nothing
        n_edges = 0
    else
        n_edges = length(edges.points)
    end
    validate_vector_length(context, "edges.colors.values", edges.colors.values, "edges.points", n_edges)
    validate_vector_length(context, "edges.sizes.values", edges.sizes.values, "edges.points", n_edges)
    validate_vector_length(context, "edges.styles", edges.styles, "edges.points", n_edges)
    validate_vector_length(context, "edges.entities.hovers", edges.entities.hovers, "edges.points", n_edges)
    validate_vector_length(context, "edges.entities.mask", edges.entities.mask, "edges.points", n_edges)
    validate_vector_length(context, "edges.order", edges.order, "edges.points", n_edges)

    if edges.colors.values !== nothing && eltype(edges.colors.values) <: Real
        throw(ArgumentError("continuous colors for edges are not implemented"))
    end

    validate_vector_entries(context, "edges.points", edges.points, edges.entities.mask) do _, (from_point, to_point)
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
        x::ValuesData = ValuesData(),
        y::ValuesData = ValuesData(),
        points::PointsData = PointsData(),
        borders::BordersData = BordersData(),
        edges::EdgesData = EdgesData(),
        vertical_bands::BandsData = BandsData(),
        horizontal_bands::BandsData = BandsData(),
        diagonal_bands::BandsData = BandsData(),
        configuration::PointsGraphConfiguration = PointsGraphConfiguration()]
    )::PointsGraph

Create a [`PointsGraph`](@ref) by initializing only the [`PointsGraphData`](@ref) fields (with an optional
[`PointsGraphConfiguration`](@ref)).
"""
function points_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    x::ValuesData = ValuesData(),
    y::ValuesData = ValuesData(),
    points::PointsData = PointsData(),
    borders::BordersData = BordersData(),
    edges::EdgesData = EdgesData(),
    vertical_bands::BandsData = BandsData(),
    horizontal_bands::BandsData = BandsData(),
    diagonal_bands::BandsData = BandsData(),
    configuration::PointsGraphConfiguration = PointsGraphConfiguration(),
)::PointsGraph
    return PointsGraph(
        PointsGraphData(; figure_title, x, y, points, borders, edges, vertical_bands, horizontal_bands, diagonal_bands),
        configuration,
    )
end

function Common.validate_graph(graph::PointsGraph)::Nothing
    validate_values(
        ValidationContext(["graph.data.x.values"]),
        numeric_values(graph.data.x),
        ValidationContext(["graph.configuration.x_axis"]),
        graph.configuration.x_axis,
    )

    validate_values(
        ValidationContext(["graph.data.y.values"]),
        numeric_values(graph.data.y),
        ValidationContext(["graph.configuration.y_axis"]),
        graph.configuration.y_axis,
    )

    validate_colors(
        ValidationContext(["graph.data.points.colors.values"]),
        graph.data.points.colors.values,
        ValidationContext(["graph.configuration.points.colors"]),
        graph.configuration.points.colors,
        graph.data.points.entities.mask,
    )

    validate_colors(
        ValidationContext(["graph.data.borders.colors.values"]),
        graph.data.borders.colors.values,
        ValidationContext(["graph.configuration.borders.colors"]),
        graph.configuration.borders.colors,
        graph.data.borders.mask,
    )

    validate_colors(
        ValidationContext(["graph.data.edges.colors.values"]),
        graph.data.edges.colors.values,
        ValidationContext(["graph.configuration.edges.colors"]),
        graph.configuration.edges.colors,
        graph.data.edges.entities.mask,
    )

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

    has_legend = false
    n_colors_scales = 0
    for (colors_configuration, colors_values) in (
        (graph.configuration.points.colors, graph.data.points.colors.values),
        (graph.configuration.borders.colors, graph.data.borders.colors.values),
        (graph.configuration.edges.colors, graph.data.edges.colors.values),
    )
        if colors_configuration.show_legend
            if colors_configuration.palette isa CategoricalColors || colors_values isa AbstractVector{<:AbstractString}
                has_legend = true
            else
                n_colors_scales += 1
            end
        end
    end

    if n_colors_scales > length(graph.configuration.figure.colors_scale_offsets)
        text =  # UNTESTED
            "insufficient number of graph.figure.colors_scale_offsets: $(length(graph.figure.colors_scale_offsets))\n" *
            "is not enough for the shown color scales: $(n_colors_scales)"
        if has_legend  # UNTESTED
            text *= "\nfollowing a legend"  # UNTESTED
        end
        throw(ArgumentError(text))  # UNTESTED
    end

    return nothing
end

@kwdef struct ScaledData
    values::AbstractVector{<:Real}
    range::Range
end

function scaled_data(axis_configuration::AxisConfiguration, values::AbstractVector{<:Real})::ScaledData
    scaled_values = scale_axis_values(axis_configuration, values)
    implicit_scaled_range = Range(; minimum = minimum(scaled_values), maximum = maximum(scaled_values))
    scaled_range = final_scaled_range(implicit_scaled_range, axis_configuration)
    return ScaledData(; values = scaled_values, range = scaled_range)
end

@kwdef mutable struct ConfiguredScatters
    colors::ConfiguredColors
    show_in_legend::Bool
    legend_group::AbstractString
    pixel_size::Maybe{Real}
    pixel_sizes::Maybe{AbstractVector{<:Real}}
    original_sizes::Maybe{AbstractVector{<:Real}}
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}}
    order::Maybe{AbstractVector{<:Integer}}
end

function configured_scatters(;
    legend_group::AbstractString,
    scatters_configuration::ScattersConfiguration,
    colors_title::Maybe{AbstractString},
    colors_values::Maybe{Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}}},
    next_colors_scale_index::AbstractVector{<:Integer},
    size_values::Maybe{AbstractVector{<:Real}},
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
    order::Maybe{AbstractVector{<:Integer}},
)::ConfiguredScatters
    colors = configured_colors(;
        colors_configuration = scatters_configuration.colors,
        colors_title,
        colors_values,
        next_colors_scale_index,
        mask,
    )

    pixel_size = scatters_configuration.sizes.fixed
    if pixel_size === nothing
        pixel_sizes = scale_size_values(scatters_configuration.sizes, size_values)
        if pixel_sizes === nothing
            pixel_size = scatters_configuration.sizes.smallest
        end
    else
        pixel_sizes = nothing
    end

    show_in_legend =
        scatters_configuration.colors.show_legend && (
            scatters_configuration.colors.palette isa CategoricalColors ||
            scatters_configuration.colors.palette isa AutomaticColors
        )

    return ConfiguredScatters(;
        colors,
        show_in_legend,
        legend_group,
        original_sizes = size_values,
        pixel_size,
        pixel_sizes,
        mask,
        order,
    )
end

function Common.graph_to_figure(graph::PointsGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    points_xs = numeric_values(graph.data.x)
    points_ys = numeric_values(graph.data.y)
    @assert points_xs !== nothing
    @assert points_ys !== nothing
    scaled_points_xs = scaled_data(graph.configuration.x_axis, points_xs)
    scaled_points_ys = scaled_data(graph.configuration.y_axis, points_ys)

    next_colors_scale_index = [1]

    points = graph.data.points
    configured_points = configured_scatters(;
        legend_group = "Points",
        scatters_configuration = graph.configuration.points,
        colors_title = prefer_data(points.colors.title, graph.configuration.points.colors.title),
        colors_values = points.colors.values,
        next_colors_scale_index,
        size_values = numeric_values(points.sizes),
        mask = points.entities.mask,
        order = points.order,
    )

    borders = graph.data.borders
    configured_borders = configured_scatters(;
        legend_group = "Borders",
        scatters_configuration = graph.configuration.borders,
        colors_title = prefer_data(borders.colors.title, graph.configuration.borders.colors.title),
        colors_values = borders.colors.values,
        next_colors_scale_index,
        size_values = numeric_values(borders.sizes),
        mask = borders.mask,
        order = points.order,
    )

    points_hovers = points.entities.hovers

    add_pixel_sizes(configured_points, configured_borders)

    configured_edges = configured_scatters(;
        legend_group = "Edges",
        scatters_configuration = graph.configuration.edges,
        colors_title = prefer_data(graph.data.edges.colors.title, graph.configuration.edges.colors.title),
        colors_values = graph.data.edges.colors.values,
        next_colors_scale_index,
        size_values = numeric_values(graph.data.edges.sizes),
        mask = graph.data.edges.entities.mask,
        order = graph.data.edges.order,
    )

    edges_points = graph.data.edges.points
    if edges_points !== nothing && !graph.configuration.edges_over_points
        push_edge_traces!(; traces, graph, scaled_points_xs, scaled_points_ys, configured_edges)
    end

    if borders.colors.values !== nothing ||
       borders.sizes.values !== nothing ||
       borders.mask !== nothing ||
       graph.configuration.borders.colors.fixed !== nothing ||
       graph.configuration.borders.sizes.fixed !== nothing
        push_points_traces!(;
            traces,
            scaled_points_xs,
            scaled_points_ys,
            configured_points = configured_borders,
            points_hovers,
        )
    end

    push_points_traces!(; traces, scaled_points_xs, scaled_points_ys, configured_points, points_hovers)

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

    has_legend =
        configured_points.show_in_legend || configured_borders.show_in_legend || configured_edges.show_in_legend
    has_hovers = points_hovers !== nothing || graph.data.edges.entities.hovers !== nothing
    layout = scatters_layout(;
        graph,
        scaled_xs_range = scaled_points_xs.range,
        scaled_ys_range = scaled_points_ys.range,
        shapes,
        has_legend,
        has_hovers,
    )

    next_colors_scale_offset_index = [Int(has_legend)]

    for configured in (configured_points, configured_borders, configured_edges)
        if configured.colors.colors_scale_index !== nothing
            set_layout_colorscale!(;
                layout,
                colors_scale_index = configured.colors.colors_scale_index,
                colors_configuration = configured.colors.colors_configuration,
                scaled_colors_palette = configured.colors.scaled_colors_palette,
                range = configured.colors.final_colors_range,
                title = configured.colors.colors_title,
                show_scale = configured.colors.show_scale,
                next_colors_scale_offset_index,
                colors_scale_offsets = graph.configuration.figure.colors_scale_offsets,
            )
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
    edges = graph.data.edges

    if configured_edges.colors.show_in_legend
        edges_names = edges.colors.values
    else
        edges_names = nothing
    end

    edges_points = edges.points
    @assert edges_points !== nothing
    seen_names = Set{AbstractString}()
    legend_group_title = configured_edges.colors.colors_title

    edges_indices = prefer_data(configured_edges.order, 1:length(edges_points))

    for edge_index in edges_indices
        from_point, to_point = edges_points[edge_index]
        if prefer_data(configured_edges.mask, edge_index, true)
            name = prefer_data(edges_names, edge_index, nothing)
            show_in_legend = configured_edges.show_in_legend

            if name === nothing
                legend_group = "Edges"
            else
                legend_group = "Edges $(name)"
                if name in seen_names
                    show_in_legend = false
                else
                    push!(seen_names, name)
                end
            end

            edge_trace = scatter(;  # NOJET
                x = [scaled_points_xs.values[from_point], scaled_points_xs.values[to_point]],
                y = [scaled_points_ys.values[from_point], scaled_points_ys.values[to_point]],
                line_width = prefer_data(configured_edges.pixel_sizes, edge_index, configured_edges.pixel_size),
                line_color = prefer_data(
                    prefer_data(
                        configured_edges.colors.final_colors_values,
                        edge_index,
                        configured_edges.colors.colors_configuration.fixed,
                    ),
                    "darkgrey",
                ),
                line_dash = plotly_line_dash(prefer_data(edges.styles, edge_index, graph.configuration.edges_style)),
                name = prefer_data(edges_names, edge_index, nothing),
                mode = "lines",
                legendgroup = show_in_legend ? legend_group : nothing,
                legendgrouptitle_text = show_in_legend ? legend_group_title : nothing,
                showlegend = show_in_legend,
                coloraxis = plotly_axis("color", configured_edges.colors.colors_scale_index),
            )
            edge_hover = prefer_data(edges.entities.hovers, edge_index, nothing)
            if edge_hover !== nothing
                edge_trace[:text] = edge_hover
                edge_trace[:hovertemplate] = "%{text}<extra></extra>"
            end
            push!(traces, edge_trace)  # NOJET
            legend_group_title = nothing
        end
    end

    return nothing
end

function push_points_traces!(;
    traces::AbstractVector{GenericTrace},
    scaled_points_xs::ScaledData,
    scaled_points_ys::ScaledData,
    configured_points::ConfiguredScatters,
    points_hovers::Maybe{AbstractVector{<:AbstractString}},
)::Nothing
    if configured_points.colors.final_colors_values !== nothing && (
        configured_points.colors.colors_configuration.palette isa CategoricalColors ||
        configured_points.colors.colors_configuration.palette isa AutomaticColors
    )
        is_first = true

        colors_masks = Union{AbstractVector{Bool}, BitVector}[]
        colors_names = AbstractString[]
        colors = Maybe{AbstractString}[]
        if configured_points.order === nothing
            priorities = nothing
            positions = nothing
        else
            positions = invperm(configured_points.order)  # NOJET
            priorities = Float32[]
        end

        palette = configured_points.colors.colors_configuration.palette
        if palette isa AutomaticColors
            # The categories are the (unique) keys, in order of appearance, and the colors are picked automatically.
            name_color_pairs = [(name, nothing) for name in unique(configured_points.colors.original_color_values)]
        else
            palette_dict = palette
            if palette_dict isa NamedArray
                palette_dict = Dict(zip(names(palette_dict, 1), palette_dict.array))  # UNTESTED # NOJET
            end
            name_color_pairs = collect(palette_dict)
        end
        for (name, color) in name_color_pairs
            push!(colors_names, name)
            push!(colors, color)
            mask = configured_points.colors.original_color_values .== name
            if configured_points.mask !== nothing
                mask .&= configured_points.mask
            end
            push!(colors_masks, mask)
            if configured_points.order !== nothing
                @assert priorities !== nothing
                @assert positions !== nothing
                n_points = sum(mask)
                if n_points > 0
                    push!(priorities, sum(positions[mask]) / n_points)
                else
                    push!(priorities, Inf32)  # UNTESTED
                end
            end
        end

        if priorities === nothing
            color_indices = 1:length(colors_names)
        else
            color_indices = sortperm(priorities)
        end

        for color_index in color_indices
            mask = colors_masks[color_index]
            if any(mask)
                push_points_trace!(;
                    traces,
                    scaled_points_xs,
                    scaled_points_ys,
                    points_hovers,
                    configured_points,
                    mask,
                    name = colors_names[color_index],
                    color = colors[color_index],
                    legend_group_suffix = colors_names[color_index],
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
            points_hovers,
            configured_points,
            mask = configured_points.mask,
            name = nothing,
            color = prefer_data(
                configured_points.colors.final_colors_values,
                configured_points.colors.colors_configuration.fixed,
            ),
        )
    end

    return nothing
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
)::Nothing
    order = configured_points.order

    if color isa AbstractVector
        color = masked_values(color, mask, order)
    end

    if legend_group_suffix === nothing
        legend_group = configured_points.legend_group
    else
        legend_group = "$(configured_points.legend_group) $(legend_group_suffix)"
    end

    hovers = masked_values(points_hovers, mask, order)

    show_in_legend = configured_points.colors.show_in_legend
    push!(  # NOJET
        traces,
        scatter(;
            x = masked_values(scaled_points_xs.values, mask, order),
            y = masked_values(scaled_points_ys.values, mask, order),
            text = hovers,
            marker_size = prefer_data(
                masked_values(configured_points.pixel_sizes, mask, order),
                configured_points.pixel_size,
            ),
            marker_color = prefer_data(color, configured_points.colors.colors_configuration.fixed),
            marker_coloraxis = plotly_axis("color", configured_points.colors.colors_scale_index),
            marker_showscale = configured_points.colors.show_scale,
            legendgroup = show_in_legend ? legend_group : nothing,
            legendgrouptitle_text = show_in_legend && is_first ? configured_points.colors.colors_title : nothing,
            showlegend = show_in_legend,
            name = name,
            hovertemplate = hovers === nothing ? nothing : "%{text}<extra></extra>",
            mode = "markers",
        ),
    )

    return nothing
end

"""
    @kwdef mutable struct LineGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        x_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
        y_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
        line::LineConfiguration = LineConfiguration()
        show_points::Bool = false
        points_size::Maybe{Real} = nothing
        points_color::Maybe{AbstractString} = nothing
        vertical_bands::BandsConfiguration = BandsConfiguration()
        horizontal_bands::BandsConfiguration = BandsConfiguration()
        diagonal_bands::BandsConfiguration = BandsConfiguration()
    end

Configure a graph for showing a single line.

If `show_points` is set, each point is drawn, using the `points_size` and/or `points_color` if specified. The bands are
similar to [`PointsGraphConfiguration`](@ref).
"""
@kwdef mutable struct LineGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    x_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
    y_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
    line::LineConfiguration = LineConfiguration()
    show_points::Bool = false
    points_size::Maybe{Real} = nothing
    points_color::Maybe{AbstractString} = nothing
    vertical_bands::BandsConfiguration = BandsConfiguration()
    horizontal_bands::BandsConfiguration = BandsConfiguration()
    diagonal_bands::BandsConfiguration = BandsConfiguration()
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

    validate_vector_length(context, "points_ys", data.points_ys, "points_xs", n_points)
    validate_vector_length(context, "points_hovers", data.points_hovers, "points_xs", n_points)

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
        vertical_bands::BandsData = BandsData(),
        horizontal_bands::BandsData = BandsData(),
        diagonal_bands::BandsData = BandsData(),
        configuration::LineGraphConfiguration = LineGraphConfiguration()]
    )::LineGraph

Create a [`LineGraph`](@ref) by initializing only the [`LineGraphData`](@ref) fields (with an optional
[`LineGraphConfiguration`](@ref)).
"""
function line_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    x_axis_title::Maybe{AbstractString} = nothing,
    y_axis_title::Maybe{AbstractString} = nothing,
    points_xs::AbstractVector{<:Real} = Float32[],
    points_ys::AbstractVector{<:Real} = Float32[],
    vertical_bands::BandsData = BandsData(),
    horizontal_bands::BandsData = BandsData(),
    diagonal_bands::BandsData = BandsData(),
    configuration::LineGraphConfiguration = LineGraphConfiguration(),
)::LineGraph
    return LineGraph(
        LineGraphData(;
            figure_title,
            x_axis_title,
            y_axis_title,
            points_xs,
            points_ys,
            vertical_bands,
            horizontal_bands,
            diagonal_bands,
        ),
        configuration,
    )
end

function Common.graph_to_figure(graph::LineGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    scaled_points_xs = scaled_data(graph.configuration.x_axis, graph.data.points_xs)
    scaled_points_ys = scaled_data(graph.configuration.y_axis, graph.data.points_ys)

    traces = Vector{GenericTrace}()

    push_line_trace!(;
        traces,
        scaled_points_xs,
        scaled_points_ys,
        line_color = graph.configuration.line.color,
        line_width = graph.configuration.line.width,
        line_style = graph.configuration.line.style,
        mode = graph.configuration.show_points ? "lines+markers" : "lines",
        points_size = graph.configuration.points_size,
        points_color = graph.configuration.points_color,
        fill = graph.configuration.line.is_filled ? "tozeroy" : nothing,
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

    layout = scatters_layout(;
        graph,
        scaled_xs_range = scaled_points_xs.range,
        scaled_ys_range = scaled_points_ys.range,
        shapes,
        has_legend = false,
    )

    return plotly_figure(traces, layout)
end

"""
    @kwdef mutable struct LinesGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        x_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
        y_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
        line::LineConfiguration = LineConfiguration()
        show_points::Bool = false
        points_size::Maybe{Real} = nothing
        points_color::Maybe{AbstractString} = nothing
        vertical_bands::BandsConfiguration = BandsConfiguration()
        horizontal_bands::BandsConfiguration = BandsConfiguration()
        diagonal_bands::BandsConfiguration = BandsConfiguration()
        show_legend::Bool = false
    end

Configure a graph for showing multiple lines.

This is similar to [`LineGraphConfiguration`](@ref), with the addition of `show_legend`. If this is set, then the data
must specify the title to use for each line.

If `stacking` is specified, we stack the values on top of each other.
"""
@kwdef mutable struct LinesGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    x_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
    y_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
    line::LineConfiguration = LineConfiguration()
    show_points::Bool = false
    points_size::Maybe{Real} = nothing
    points_color::Maybe{AbstractString} = nothing
    vertical_bands::BandsConfiguration = BandsConfiguration()
    horizontal_bands::BandsConfiguration = BandsConfiguration()
    diagonal_bands::BandsConfiguration = BandsConfiguration()
    show_legend::Bool = false
    stacking::Maybe{Stacking} = nothing
end

function Validations.validate(
    context::ValidationContext,
    configuration::Union{LineGraphConfiguration, LinesGraphConfiguration},
)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "x_axis", configuration.x_axis)
    validate_field(context, "y_axis", configuration.y_axis)
    validate_field(context, "line", configuration.line)

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

    if configuration isa LinesGraphConfiguration &&
       configuration.y_axis.log_scale !== nothing &&
       configuration.stacking !== nothing
        throw(
            ArgumentError("can't specify both $(location(context)).stacking and $(location(context)).y_axis.log_scale"),
        )
    end

    return nothing
end

"""
    @kwdef mutable struct LinesGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        x_axis_title::Maybe{AbstractString} = nothing
        y_axis_title::Maybe{AbstractString} = nothing
        lines_titles::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing
        lines_points_xs::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[]
        lines_points_ys::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[]
        lines_points_sizes::Maybe{AbstractVector{<:Maybe{Real}}} = nothing
        lines_points_colors::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing
        lines_widths::Maybe{AbstractVector{<:Maybe{Real}}} = nothing
        lines_colors::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing
        lines_styles::Maybe{AbstractVector{<:Maybe{LineStyle}}} = nothing
        lines_order::Maybe{AbstractVector{<:Integer}} = nothing
        vertical_bands::BandsData = BandsData()
        horizontal_bands::BandsData = BandsData()
        diagonal_bands::BandsData = BandsData()
    end

The data for a multi-line graph.

By default, all the titles are empty. You can specify the overall `figure_title` as well as the `x_axis_title` and
`y_axis_title` for the axes.

All the `lines_*` vectors must be of the same size (the number of lines), and contain a vector per line. The
`lines_points_xs` and `lines_points_ys` contain a vector per line; these vectors must all be of the same size for each
line (the number of points in that specific line). A `nothing` entry in the other `lines_*` vectors means the
configuration default is used for that line.

The `lines_titles` is required if `show_legend` is specified in the [`LinesGraphConfiguration`](@ref).

If `lines_order` is specified, we reorder the lines accordingly.
"""
@kwdef mutable struct LinesGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    x_axis_title::Maybe{AbstractString} = nothing
    y_axis_title::Maybe{AbstractString} = nothing
    lines_titles::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing
    lines_points_xs::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[]
    lines_points_ys::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[]
    lines_points_sizes::Maybe{AbstractVector{<:Maybe{Real}}} = nothing
    lines_points_colors::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing
    lines_widths::Maybe{AbstractVector{<:Maybe{Real}}} = nothing
    lines_colors::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing
    lines_styles::Maybe{AbstractVector{<:Maybe{LineStyle}}} = nothing
    lines_order::Maybe{AbstractVector{<:Integer}} = nothing
    vertical_bands::BandsData = BandsData()
    horizontal_bands::BandsData = BandsData()
    diagonal_bands::BandsData = BandsData()
end

function Validations.validate(context::ValidationContext, data::LinesGraphData)::Nothing
    validate_vector_is_not_empty(context, "lines_points_xs", data.lines_points_xs)
    n_lines = length(data.lines_points_xs)

    validate_vector_length(context, "lines_titles", data.lines_titles, "lines_points_xs", n_lines)
    validate_vector_length(context, "lines_points_ys", data.lines_points_ys, "lines_points_xs", n_lines)
    validate_vector_length(context, "lines_points_sizes", data.lines_points_sizes, "lines_points_xs", n_lines)
    validate_vector_length(context, "lines_points_colors", data.lines_points_colors, "lines_points_xs", n_lines)
    validate_vector_length(context, "lines_widths", data.lines_widths, "lines_points_xs", n_lines)
    validate_vector_length(context, "lines_colors", data.lines_colors, "lines_points_xs", n_lines)
    validate_vector_length(context, "lines_styles", data.lines_styles, "lines_points_xs", n_lines)

    # These color-name vectors are used directly as Plotly colors; validate them (an invalid color name would otherwise
    # be silently rendered black by Plotly).
    validate_vector_entries(context, "lines_colors", data.lines_colors) do _, color
        validate_is_color(context, color)
        return nothing
    end
    validate_vector_entries(context, "lines_points_colors", data.lines_points_colors) do _, color
        validate_is_color(context, color)
        return nothing
    end
    validate_vector_length(context, "lines_order", data.lines_order, "lines_points_xs", n_lines)

    for line_index in 1:n_lines
        validate_vector_is_not_empty(context, "lines_points_xs[$(line_index)]", data.lines_points_xs[line_index])
        n_points = length(data.lines_points_xs[line_index])

        validate_vector_length(
            context,
            "lines_points_ys[$(line_index)]",
            data.lines_points_ys[line_index],
            "lines_points_xs[$(line_index)]",
            n_points,
        )
    end

    return nothing
end

"""
A graph showing multiple lines. See [`LinesGraphData`](@ref) and [`LinesGraphConfiguration`](@ref).
"""
LinesGraph = Graph{LinesGraphData, LinesGraphConfiguration}

"""
    function lines_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        x_axis_title::Maybe{AbstractString} = nothing,
        y_axis_title::Maybe{AbstractString} = nothing,
        lines_titles::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing
        lines_points_xs::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[]
        lines_points_ys::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[]
        lines_points_sizes::Maybe{AbstractVector{<:Maybe{Real}}} = nothing
        lines_points_colors::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing
        lines_widths::Maybe{AbstractVector{<:Maybe{Real}}} = nothing
        lines_colors::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing
        lines_styles::Maybe{AbstractVector{<:Maybe{LineStyle}}} = nothing
        lines_order::Maybe{<:AbstractVector{<:Integer}} = nothing
        vertical_bands::BandsData = BandsData(),
        horizontal_bands::BandsData = BandsData(),
        diagonal_bands::BandsData = BandsData(),
        configuration::LinesGraphConfiguration = LinesGraphConfiguration()]
    )::LinesGraph

Create a [`LinesGraph`](@ref) by initializing only the [`LinesGraphData`](@ref) fields (with an optional
[`LinesGraphConfiguration`](@ref)).
"""
function lines_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    x_axis_title::Maybe{AbstractString} = nothing,
    y_axis_title::Maybe{AbstractString} = nothing,
    lines_titles::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing,
    lines_points_xs::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[],
    lines_points_ys::AbstractVector{<:AbstractVector{<:Real}} = Vector{Float32}[],
    lines_points_sizes::Maybe{AbstractVector{<:Maybe{Real}}} = nothing,
    lines_points_colors::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing,
    lines_widths::Maybe{AbstractVector{<:Maybe{Real}}} = nothing,
    lines_colors::Maybe{AbstractVector{<:Maybe{AbstractString}}} = nothing,
    lines_styles::Maybe{AbstractVector{<:Maybe{LineStyle}}} = nothing,
    lines_order::Maybe{<:AbstractVector{<:Integer}} = nothing,
    vertical_bands::BandsData = BandsData(),
    horizontal_bands::BandsData = BandsData(),
    diagonal_bands::BandsData = BandsData(),
    configuration::LinesGraphConfiguration = LinesGraphConfiguration(),
)::LinesGraph
    return LinesGraph(
        LinesGraphData(;
            figure_title,
            x_axis_title,
            y_axis_title,
            lines_titles,
            lines_points_xs,
            lines_points_ys,
            lines_points_sizes,
            lines_points_colors,
            lines_widths,
            lines_colors,
            lines_styles,
            lines_order,
            vertical_bands,
            horizontal_bands,
            diagonal_bands,
        ),
        configuration,
    )
end

function Common.validate_graph(graph::Union{LineGraph, LinesGraph})::Nothing
    if graph isa LineGraph
        validate_values(
            ValidationContext(["graph.data.points_xs"]),
            graph.data.points_xs,
            ValidationContext(["graph.configuration.x_axis"]),
            graph.configuration.x_axis,
        )

        validate_values(
            ValidationContext(["graph.data.points_ys"]),
            graph.data.points_ys,
            ValidationContext(["graph.configuration.y_axis"]),
            graph.configuration.y_axis,
        )

    elseif graph isa LinesGraph
        n_lines = length(graph.data.lines_points_xs)
        for line_index in 1:n_lines
            validate_values(
                ValidationContext(["graph.data.lines_points_xs", line_index]),
                graph.data.lines_points_xs[line_index],
                ValidationContext(["graph.configuration.x_axis"]),
                graph.configuration.x_axis,
            )

            ys_context = ValidationContext(["graph.data.lines_points_ys", line_index])
            validate_values(
                ys_context,
                graph.data.lines_points_ys[line_index],
                ValidationContext(["graph.configuration.y_axis"]),
                graph.configuration.y_axis,
            )

            if graph.configuration.stacking == StackFractions
                for (y_index, y_value) in enumerate(graph.data.lines_points_ys[line_index])
                    scaled_value = scale_axis_value(graph.configuration.y_axis, y_value)
                    if scaled_value !== nothing && scaled_value < 0
                        throw(
                            ArgumentError(
                                "too low scaled $(location(ys_context))[$(y_index)]: $(scaled_value)\n" *
                                "is not at least: 0\n" *
                                "when using graph.configuration.stacking: StackFractions",
                            ),
                        )
                    end
                end
            end
        end

    else
        @assert false
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

    if graph isa LinesGraph && graph.configuration.show_legend  # NOJET
        lines_titles = graph.data.lines_titles  # NOJET
        if lines_titles === nothing
            throw(ArgumentError("must specify graph.data.lines_titles for graph.configuration.show_legend"))
        end
        for (line_index, line_title) in enumerate(lines_titles)
            if line_title === nothing
                throw(
                    ArgumentError(
                        "must specify graph.data.lines_titles[$(line_index)] for graph.configuration.show_legend",
                    ),
                )
            end
        end
    end

    return nothing
end

function Common.graph_to_figure(graph::LinesGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    n_lines = length(graph.data.lines_points_xs)

    scaled_lines_points_xs =
        [scale_axis_values(graph.configuration.x_axis, line_points_xs) for line_points_xs in graph.data.lines_points_xs]
    scaled_lines_points_ys =
        [scale_axis_values(graph.configuration.y_axis, line_points_ys) for line_points_ys in graph.data.lines_points_ys]

    stacked_scaled_ys_range = MaybeRange()
    if graph.configuration.stacking !== nothing
        scaled_lines_points_xs, scaled_lines_points_ys, stacked_scaled_ys_range =
            unify_lines_points(scaled_lines_points_xs, scaled_lines_points_ys)
    end

    implicit_scaled_xs_range = MaybeRange()
    for scaled_points_xs in scaled_lines_points_xs
        collect_range!(implicit_scaled_xs_range, scaled_points_xs)
    end
    scaled_xs_range = final_scaled_range(implicit_scaled_xs_range, graph.configuration.x_axis)

    implicit_scaled_ys_range = MaybeRange()
    if graph.configuration.stacking == StackValues
        implicit_scaled_ys_range = stacked_scaled_ys_range
    else
        for scaled_points_ys in scaled_lines_points_ys
            collect_range!(implicit_scaled_ys_range, scaled_points_ys)
        end
    end
    scaled_ys_range = final_scaled_range(implicit_scaled_ys_range, graph.configuration.y_axis)

    traces = Vector{GenericTrace}()

    if graph.configuration.stacking === nothing
        stack_group = nothing
        group_norm = nothing

    else
        stack_group = "stacked"

        if graph.configuration.stacking == StackValues
            group_norm = nothing
        elseif graph.configuration.stacking == StackFractions
            if graph.configuration.y_axis.percent
                scaled_ys_range = Range(; minimum = -1, maximum = 101)
                group_norm = "percent"
            else
                scaled_ys_range = Range(; minimum = -0.01, maximum = 1.01)
                group_norm = "fraction"
            end
        else
            @assert false
        end
    end

    lines_indices = prefer_data(graph.data.lines_order, 1:n_lines)

    for line_index in lines_indices
        if graph.configuration.stacking === nothing
            fill = !graph.configuration.line.is_filled ? "none" : "tozeroy"
        else
            fill = !graph.configuration.line.is_filled ? "none" : line_index == 1 ? "tozeroy" : "tonexty"
        end

        push_line_trace!(;
            traces,
            scaled_points_xs = ScaledData(; values = scaled_lines_points_xs[line_index], range = scaled_xs_range),
            scaled_points_ys = ScaledData(; values = scaled_lines_points_ys[line_index], range = scaled_ys_range),
            name = prefer_data(graph.data.lines_titles, line_index, nothing),
            line_color = prefer_data(graph.data.lines_colors, line_index, graph.configuration.line.color),
            line_width = prefer_data(graph.data.lines_widths, line_index, graph.configuration.line.width),
            line_style = prefer_data(graph.data.lines_styles, line_index, graph.configuration.line.style),
            mode = graph.configuration.show_points ? "lines+markers" : "lines",
            points_size = prefer_data(graph.data.lines_points_sizes, line_index, graph.configuration.points_size),
            points_color = prefer_data(graph.data.lines_points_colors, line_index, graph.configuration.points_color),
            show_in_legend = graph.configuration.show_legend,
            fill,
            stack_group,
            group_norm,
        )
    end

    shapes = Shape[]

    push_vertical_bands_shapes(
        shapes,
        graph.configuration.x_axis,
        scaled_xs_range,
        graph.data.vertical_bands,
        graph.configuration.vertical_bands,
    )

    push_horizontal_bands_shapes(
        shapes,
        graph.configuration.y_axis,
        scaled_ys_range,
        graph.data.horizontal_bands,
        graph.configuration.horizontal_bands,
    )

    push_diagonal_bands_shapes(
        shapes,
        graph.configuration.x_axis,
        scaled_xs_range,
        scaled_ys_range,
        graph.data.diagonal_bands,
        graph.configuration.diagonal_bands,
    )

    layout =
        scatters_layout(; graph, scaled_xs_range, scaled_ys_range, shapes, has_legend = graph.configuration.show_legend)

    return plotly_figure(traces, layout)
end

function unify_lines_points(
    lines_points_xs::AbstractVector{<:AbstractVector{<:Real}},
    lines_points_ys::AbstractVector{<:AbstractVector{<:Real}},
)::Tuple{Vector{Vector{Float32}}, Vector{Vector{Float32}}, MaybeRange}
    n_lines = length(lines_points_xs)

    # The lines are stacked on top of each other, so this covers their accumulated values, not the values of any one of
    # them. The stack is built up from zero, so zero is always in the range.
    stacked_range = MaybeRange(; minimum = 0, maximum = 0)

    unified_xs = Vector{Vector{Float32}}()
    unified_ys = Vector{Vector{Float32}}()

    zero_before = zeros(Bool, n_lines)
    zero_after = zeros(Bool, n_lines)

    for _ in 1:n_lines
        push!(unified_xs, Vector{Float32}())
        push!(unified_ys, Vector{Float32}())
    end

    last_x = nothing
    last_y = nothing

    next_point_indices = fill(1, n_lines)

    while true
        unified_x = nothing

        for line_index in 1:n_lines
            point_index = next_point_indices[line_index]
            if point_index <= length(lines_points_xs[line_index])
                if unified_x === nothing
                    unified_x = lines_points_xs[line_index][point_index]
                else
                    unified_x = min(unified_x, lines_points_xs[line_index][point_index])
                end
            end
        end

        if unified_x === nothing
            return (unified_xs, unified_ys, stacked_range)
        end

        if unified_x != last_x
            last_x = unified_x
            last_y = 0
        end

        for line_index in 1:n_lines
            point_index = next_point_indices[line_index]
            next_x = lines_points_xs[line_index][min(point_index, length(lines_points_xs[line_index]))]

            if unified_x > next_x
                if !zero_after[line_index]
                    push!(unified_xs[line_index], next_x)
                    push!(unified_xs[line_index], next_x)
                    push!(unified_ys[line_index], 0)
                    zero_after[line_index] = true
                end
                push!(unified_xs[line_index], unified_x)
                push!(unified_ys[line_index], 0)

            else
                next_y = lines_points_ys[line_index][point_index]

                if unified_x == next_x
                    if zero_before[line_index]
                        push!(unified_xs[line_index], unified_x)
                        push!(unified_ys[line_index], 0)
                        push!(unified_xs[line_index], unified_x)
                        zero_before[line_index] = false
                    end
                    last_y += next_y
                    push!(unified_xs[line_index], next_x)
                    push!(unified_ys[line_index], next_y)
                    next_point_indices[line_index] += 1

                elseif point_index == 1
                    push!(unified_xs[line_index], unified_x)
                    push!(unified_ys[line_index], 0)
                    push!(unified_xs[line_index], unified_x)
                    zero_before[line_index] = true

                else
                    @assert !zero_before[line_index]
                    prev_x = lines_points_xs[line_index][point_index - 1]
                    prev_y = lines_points_ys[line_index][point_index - 1]
                    push!(unified_xs[line_index], unified_x)
                    mid_y = prev_y + (next_y - prev_y) * (unified_x - prev_x) / (next_x - prev_x)
                    push!(unified_ys[line_index], mid_y)
                    last_y += mid_y
                end
            end
        end

        collect_range!(stacked_range, (last_y,))
    end
end

function push_line_trace!(;
    traces::AbstractVector{GenericTrace},
    scaled_points_xs::ScaledData,
    scaled_points_ys::ScaledData,
    line_color::Maybe{AbstractString},
    line_width::Maybe{Real},
    line_style::Maybe{LineStyle},
    name::Maybe{AbstractString} = nothing,
    legend_group::Maybe{AbstractString} = nothing,
    legend_group_title::Maybe{AbstractString} = nothing,
    show_in_legend::Bool = false,
    is_first = true,
    mode::AbstractString,
    points_size::Maybe{Real},
    points_color::Maybe{AbstractString},
    fill::Maybe{AbstractString},
    stack_group::Maybe{AbstractString} = nothing,
    group_norm::Maybe{AbstractString} = nothing,
)::Nothing
    push!(  # NOJET
        traces,
        scatter(;
            x = scaled_points_xs.values,
            y = scaled_points_ys.values,
            marker_size = points_size,
            marker_color = points_color,
            legendgroup = show_in_legend ? legend_group : nothing,
            legendgrouptitle_text = show_in_legend && is_first ? legend_group_title : nothing,
            showlegend = show_in_legend,
            name = name,
            mode,
            line_color,
            line_width,
            line_dash = plotly_line_dash(line_style),
            fill,
            stackgroup = stack_group,
            groupnorm = group_norm,
        ),
    )

    return nothing
end

# The titles of the X and Y axes given in the graph data.
function data_axes_titles(graph::PointsGraph)::Tuple{Maybe{AbstractString}, Maybe{AbstractString}}
    return (graph.data.x.title, graph.data.y.title)
end

function data_axes_titles(graph::Union{LineGraph, LinesGraph})::Tuple{Maybe{AbstractString}, Maybe{AbstractString}}
    return (graph.data.x_axis_title, graph.data.y_axis_title)
end

function scatters_layout(;
    graph::Union{PointsGraph, LineGraph, LinesGraph},
    scaled_xs_range::Range,
    scaled_ys_range::Range,
    shapes::AbstractVector{Shape},
    has_legend::Bool,
    has_hovers::Bool = false,
)::Layout
    layout = plotly_layout(graph.configuration.figure; title = graph.data.figure_title, has_legend, has_hovers, shapes)

    x_axis_title, y_axis_title = data_axes_titles(graph)

    set_layout_axis!(
        layout,
        "xaxis",
        graph.configuration.x_axis;
        title = prefer_data(x_axis_title, graph.configuration.x_axis.title),
        range = scaled_xs_range,
    )

    set_layout_axis!(
        layout,
        "yaxis",
        graph.configuration.y_axis;
        title = prefer_data(y_axis_title, graph.configuration.y_axis.title),
        range = scaled_ys_range,
    )

    return layout
end

"""
    points_density(
        points_xs::AbstractVector{<:Real},
        points_ys::AbstractVector{<:Real},
    )::AbstractVector{<:AbstractFloat}

Given a set of point coordinates, compute for each one the density of its environment. This can be used to color the
points by density.
"""
function points_density(
    points_xs::AbstractVector{<:Real},
    points_ys::AbstractVector{<:Real},
)::AbstractVector{<:AbstractFloat}
    itk = InterpKDE(kde((points_xs, points_ys)))  # NOJET
    return [itk.itp(point_x, point_y) for (point_x, point_y) in zip(points_xs, points_ys)]
end

function Common.flip_axes(graph::PointsGraph)::PointsGraph
    return PointsGraph(
        PointsGraphData(;
            figure_title = graph.data.figure_title,
            x = graph.data.y,
            y = graph.data.x,
            points = graph.data.points,
            borders = graph.data.borders,
            edges = graph.data.edges,
            vertical_bands = graph.data.horizontal_bands,
            horizontal_bands = graph.data.vertical_bands,
            diagonal_bands = graph.data.diagonal_bands,
        ),
        PointsGraphConfiguration(;
            figure = graph.configuration.figure,
            x_axis = graph.configuration.y_axis,
            y_axis = graph.configuration.x_axis,
            points = graph.configuration.points,
            borders = graph.configuration.borders,
            edges = graph.configuration.edges,
            edges_style = graph.configuration.edges_style,
            edges_over_points = graph.configuration.edges_over_points,
            vertical_bands = graph.configuration.horizontal_bands,
            horizontal_bands = graph.configuration.vertical_bands,
            diagonal_bands = graph.configuration.diagonal_bands,
        ),
    )
end

function Common.flip_axes(graph::LineGraph)::LineGraph
    return LineGraph(
        LineGraphData(;
            figure_title = graph.data.figure_title,
            x_axis_title = graph.data.y_axis_title,
            y_axis_title = graph.data.x_axis_title,
            points_xs = graph.data.points_ys,
            points_ys = graph.data.points_xs,
            points_hovers = graph.data.points_hovers,
            vertical_bands = graph.data.horizontal_bands,
            horizontal_bands = graph.data.vertical_bands,
            diagonal_bands = graph.data.diagonal_bands,
        ),
        LineGraphConfiguration(;
            figure = graph.configuration.figure,
            x_axis = graph.configuration.y_axis,
            y_axis = graph.configuration.x_axis,
            line = graph.configuration.line,
            show_points = graph.configuration.show_points,
            points_size = graph.configuration.points_size,
            points_color = graph.configuration.points_color,
            vertical_bands = graph.configuration.horizontal_bands,
            horizontal_bands = graph.configuration.vertical_bands,
            diagonal_bands = graph.configuration.diagonal_bands,
        ),
    )
end

function Common.flip_axes(graph::LinesGraph)::LinesGraph
    return LinesGraph(
        LinesGraphData(;
            figure_title = graph.data.figure_title,
            x_axis_title = graph.data.y_axis_title,
            y_axis_title = graph.data.x_axis_title,
            lines_titles = graph.data.lines_titles,
            lines_points_xs = graph.data.lines_points_ys,
            lines_points_ys = graph.data.lines_points_xs,
            lines_points_sizes = graph.data.lines_points_sizes,
            lines_points_colors = graph.data.lines_points_colors,
            lines_widths = graph.data.lines_widths,
            lines_colors = graph.data.lines_colors,
            lines_styles = graph.data.lines_styles,
            lines_order = graph.data.lines_order,
            vertical_bands = graph.data.horizontal_bands,
            horizontal_bands = graph.data.vertical_bands,
            diagonal_bands = graph.data.diagonal_bands,
        ),
        LinesGraphConfiguration(;
            figure = graph.configuration.figure,
            x_axis = graph.configuration.y_axis,
            y_axis = graph.configuration.x_axis,
            line = graph.configuration.line,
            show_points = graph.configuration.show_points,
            points_size = graph.configuration.points_size,
            points_color = graph.configuration.points_color,
            vertical_bands = graph.configuration.horizontal_bands,
            horizontal_bands = graph.configuration.vertical_bands,
            diagonal_bands = graph.configuration.diagonal_bands,
            show_legend = graph.configuration.show_legend,
            stacking = graph.configuration.stacking,
        ),
    )
end

function Common.flip_axes!(graph::PointsGraph)::PointsGraph
    data = graph.data
    data.x, data.y = data.y, data.x
    data.vertical_bands, data.horizontal_bands = data.horizontal_bands, data.vertical_bands

    configuration = graph.configuration
    configuration.x_axis, configuration.y_axis = configuration.y_axis, configuration.x_axis
    configuration.vertical_bands, configuration.horizontal_bands =
        configuration.horizontal_bands, configuration.vertical_bands

    return graph
end

function Common.flip_axes!(graph::LineGraph)::LineGraph
    data = graph.data
    data.x_axis_title, data.y_axis_title = data.y_axis_title, data.x_axis_title
    data.points_xs, data.points_ys = data.points_ys, data.points_xs
    data.vertical_bands, data.horizontal_bands = data.horizontal_bands, data.vertical_bands

    configuration = graph.configuration
    configuration.x_axis, configuration.y_axis = configuration.y_axis, configuration.x_axis
    configuration.vertical_bands, configuration.horizontal_bands =
        configuration.horizontal_bands, configuration.vertical_bands

    return graph
end

function Common.flip_axes!(graph::LinesGraph)::LinesGraph
    data = graph.data
    data.x_axis_title, data.y_axis_title = data.y_axis_title, data.x_axis_title
    data.lines_points_xs, data.lines_points_ys = data.lines_points_ys, data.lines_points_xs
    data.vertical_bands, data.horizontal_bands = data.horizontal_bands, data.vertical_bands

    configuration = graph.configuration
    configuration.x_axis, configuration.y_axis = configuration.y_axis, configuration.x_axis
    configuration.vertical_bands, configuration.horizontal_bands =
        configuration.horizontal_bands, configuration.vertical_bands

    return graph
end

end  # module
