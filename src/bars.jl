"""
Graphs for showing bars.
"""
module Bars

export BarsGraph
export BarsGraphConfiguration
export BarsGraphData
export SeriesBarsGraph
export SeriesBarsGraphConfiguration
export SeriesBarsGraphData
export SeriesData
export bars_graph
export series_bars_graph

using ..Common
using ..Utilities
using ..Validations

using NamedArrays
using PlotlyBase

import ..Validations.Maybe

"""
    @kwdef mutable struct BarsGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
        value_bands::BandsConfiguration = BandsConfiguration()
        values_orientation::ValuesOrientation = VerticalValues
        bars_colors::ColorsConfiguration = ColorsConfiguration()
        bars_gap::Real = 0.02,
        bars_annotations::AnnotationSize = AnnotationSize()
    end

Configure a graph for showing a single series of bars.

By default the values are the `y` axis (`VerticalValues`). You can flip the axes using the `values_orientation`. You can
specify bands for this axis using `value_bands`. The `bars_gap` is added between the graps, and is in the usual
inconvenient units of fractions of the total graph size. The `bars_colors` is used to control the color of the bars (if
not specified, chosen automatically by Plotly), in combination with the data bar colors (if any).

The `value_axis` always shows zero, which is where a bar is measured from, however far from it the values are - so the
bars show their sizes rather than their differences. Setting an explicit `value_axis.minimum` (or `maximum`) overrides
this. A log scale never reaches zero, so there a bar is measured from the smallest value shown.
"""
@kwdef mutable struct BarsGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    value_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
    value_bands::BandsConfiguration = BandsConfiguration()
    values_orientation::ValuesOrientation = VerticalValues
    bars_colors::ColorsConfiguration = ColorsConfiguration()
    bars_gap::Real = 0.02
    bars_annotations::AnnotationSize = AnnotationSize()
end

function Validations.validate(context::ValidationContext, configuration::BarsGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "value_axis", configuration.value_axis)
    validate_field(context, "value_bands", configuration.value_bands)
    validate_field(context, "bars_annotations", configuration.bars_annotations)
    validate_field(context, "bars_colors", configuration.bars_colors)

    validate_in(context, "bars_gap") do
        validate_is_at_least(context, configuration.bars_gap, 0)
        return validate_is_below(context, configuration.bars_gap, 1)
    end

    if configuration.bars_colors.show_legend && configuration.bars_colors.palette isa CategoricalColors
        throw(
            ArgumentError(
                "can't specify $(location(context)).bars_colors.show_legend\n" *
                "for a categorical $(location(context)).bars_colors.palette",
            ),
        )
    end

    return nothing
end

"""
    @kwdef mutable struct BarsGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        values::ValuesData = ValuesData()
        names::ValuesData = ValuesData()
        bars::EntitiesData = EntitiesData()
        colors::ValuesData = ValuesData()
        annotations::AbstractVector{AnnotationData} = AnnotationData[]
        value_bands::BandsData = BandsData()
    end

The data for a graph of a single series of bars.

The `values` are required and numeric, one per bar; their title is the value axis title. The `names` values (if any)
are strings, one per bar, shown as the bar axis ticks; their title is the bar axis title. The `bars` hold the hovers and
mask of the bars; masked bars are left out of the graph. The `colors` are optional (typically all bars have the same
color); their title is the legend title. You can even add annotations to the bars.
"""
@kwdef mutable struct BarsGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    values::ValuesData = ValuesData()
    names::ValuesData = ValuesData()
    bars::EntitiesData = EntitiesData()
    colors::ValuesData = ValuesData()
    annotations::AbstractVector{AnnotationData} = AnnotationData[]
    value_bands::BandsData = BandsData()
end

function Validations.validate(context::ValidationContext, data::BarsGraphData)::Nothing
    validate_numeric_values(context, "values.values", data.values.values; is_required = true)
    validate_string_values(context, "names.values", data.names.values)

    values = data.values.values
    @assert values !== nothing
    validate_vector_is_not_empty(context, "values.values", values)
    n_bars = length(values)

    validate_vector_length(context, "names.values", data.names.values, "values.values", n_bars)
    validate_vector_length(context, "bars.hovers", data.bars.hovers, "values.values", n_bars)
    validate_vector_length(context, "bars.mask", data.bars.mask, "values.values", n_bars)
    validate_vector_length(context, "colors.values", data.colors.values, "values.values", n_bars)

    validate_vector_entries(context, "annotations", data.annotations) do _, annotation
        validate(context, annotation, "values.values", n_bars)
        return nothing
    end

    return nothing
end

"""
A graph showing a single series of bars. See [`BarsGraphData`](@ref) and [`BarsGraphConfiguration`](@ref).
"""
BarsGraph = Graph{BarsGraphData, BarsGraphConfiguration}

"""
    function bars_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        values::ValuesData = ValuesData(),
        names::ValuesData = ValuesData(),
        bars::EntitiesData = EntitiesData(),
        colors::ValuesData = ValuesData(),
        annotations::AbstractVector{AnnotationData} = AnnotationData[],
        value_bands::BandsData = BandsData(),
        configuration::BarsGraphConfiguration = BarsGraphConfiguration()]
    )::BarsGraph

Create a [`BarsGraph`](@ref) by initializing only the [`BarsGraphData`](@ref) fields (with an optional
[`BarsGraphConfiguration`](@ref)).
"""
function bars_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    values::ValuesData = ValuesData(),
    names::ValuesData = ValuesData(),
    bars::EntitiesData = EntitiesData(),
    colors::ValuesData = ValuesData(),
    annotations::AbstractVector{AnnotationData} = AnnotationData[],
    value_bands::BandsData = BandsData(),
    configuration::BarsGraphConfiguration = BarsGraphConfiguration(),
)::BarsGraph
    return BarsGraph(
        BarsGraphData(; figure_title, values, names, bars, colors, annotations, value_bands),
        configuration,
    )
end

function Common.validate_graph(graph::BarsGraph)::Nothing
    validate_values(
        ValidationContext(["graph.data.values.values"]),
        numeric_values(graph.data.values),
        ValidationContext(["graph.configuration.value_axis"]),
        graph.configuration.value_axis,
    )

    validate_colors(
        ValidationContext(["graph.data.colors.values"]),
        graph.data.colors.values,
        ValidationContext(["graph.configuration.bars_colors"]),
        graph.configuration.bars_colors,
        graph.data.bars.mask,
    )

    validate_graph_bands(
        "value_bands",
        graph.configuration.value_bands,
        graph.data.value_bands,
        graph.configuration.value_axis,
    )

    validate_axis_sizes(;
        axis_name = "value",
        annotation_size = graph.configuration.bars_annotations,
        n_annotations = length(graph.data.annotations),
    )

    return nothing
end

# An annotation restricted to the unmasked entries.
function masked_annotation(
    annotation::AnnotationData,
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
)::AnnotationData
    return AnnotationData(;
        title = annotation.title,
        values = masked_values(annotation.values, mask, nothing),
        hovers = masked_values(annotation.hovers, mask, nothing),
        colors = annotation.colors,
    )
end

function Common.graph_to_figure(graph::BarsGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    implicit_values_range = MaybeRange()

    values = numeric_values(graph.data.values)
    @assert values !== nothing
    n_bars = length(values)
    mask = graph.data.bars.mask

    # Default names are given before masking so masked out bars do not shift the names of the rest.
    names = masked_values(prefer_data(string_values(graph.data.names), string.(1:n_bars)), mask, nothing)
    values = masked_values(values, mask, nothing)
    hovers = masked_values(graph.data.bars.hovers, mask, nothing)
    annotations = [masked_annotation(annotation, mask) for annotation in graph.data.annotations]

    next_colors_scale_index = [1]
    colors = configured_colors(;
        colors_configuration = graph.configuration.bars_colors,
        colors_title = prefer_data(graph.data.colors.title, graph.configuration.bars_colors.title),
        colors_values = masked_values(graph.data.colors.values, mask, nothing),
        next_colors_scale_index,
    )

    push_bar_trace!(;
        traces,
        sub_graph = SubGraph(;
            index = 1,
            n_graphs = 1,
            graphs_gap = nothing,
            n_annotations = length(annotations),
            annotation_size = graph.configuration.bars_annotations,
        ),
        values,
        value_axis = graph.configuration.value_axis,
        values_orientation = graph.configuration.values_orientation,
        color = prefer_data(colors.final_colors_values, colors.colors_configuration.fixed),
        hovers,
        names,
        show_in_legend = false,
        implicit_values_range,
        colors_scale_index = colors.colors_scale_index,
    )

    collect_zero_range!(implicit_values_range, graph.configuration.value_axis)

    has_legend_only_traces = [false]
    annotations_colors = push_annotations_traces!(;
        traces,
        names,
        value_axis = graph.configuration.value_axis,
        values_orientation = graph.configuration.values_orientation,
        next_colors_scale_index,
        has_legend_only_traces,
        annotations_data = annotations,
        annotation_size = graph.configuration.bars_annotations,
        entries_hovers = hovers,
    )

    layout = bars_layout(;
        graph,
        has_tick_names = graph.data.names.values !== nothing,
        has_legend = false,
        has_hovers = hovers !== nothing,
        implicit_values_range,
        colors,
        annotations_colors,
        has_legend_only_traces,
    )

    return plotly_figure(traces, layout)
end

"""
    @kwdef mutable struct SeriesBarsGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        value_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
        values_orientation::ValuesOrientation = VerticalValues
        bars_gap::Maybe{Real} = nothing
        bars_annotations::AnnotationSize = AnnotationSize(),
        series_gap::Maybe{Real} = nothing
        stacking::Maybe{Stacking} = nothing
        mirrored::Bool = false
    end

Configure a graph for showing multiple series of bars.

This expands on [`BarsGraphConfiguration`](@ref) by adding optional `stacking` for stacking the bars of the different
series on top of each other. Alternatively, specifying a `series_gap` will plot each series in its own separate
sub-graph. The `series_gap` is specified as a fraction of the used graph size. If zero the graphs will be adjacent, if 1
then the gaps will be the same size as the graphs. If neither is specified, then the bars will be shown in groups
(adjacent to each other) with the `bars_gap` between the groups.

The `value_axis` always shows zero, which is where a bar is measured from, however far from it the values are - so the
bars show their sizes rather than their differences. Setting an explicit `value_axis.minimum` (or `maximum`) overrides
this. A log scale never reaches zero, so there a bar is measured from the smallest value shown.

If `mirrored`, the series are read in pairs, so their number must be even. The 1st series of each pair grows to the left
(or down) and the 2nd to the right (or up), away from the bar axis they share - a butterfly graph. Each pair therefore
needs a value axis per side, and both of them show the same range, so the two sides are drawn to the same scale.
Without a `series_gap` all the pairs share the same two value axes and are shown as groups (or stacked, given
`stacking`); with one, each pair is given two value axes of its own and the `series_gap` separates the pairs.
"""
@kwdef mutable struct SeriesBarsGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    value_axis::AxisConfiguration = AxisConfiguration(; expand_fraction = 0.01)
    values_orientation::ValuesOrientation = VerticalValues
    bars_gap::Real = 0.02
    bars_annotations::AnnotationSize = AnnotationSize()
    series_gap::Maybe{Real} = nothing
    stacking::Maybe{Stacking} = nothing
    mirrored::Bool = false
end

function Validations.validate(context::ValidationContext, configuration::SeriesBarsGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "value_axis", configuration.value_axis)
    validate_field(context, "bars_annotations", configuration.bars_annotations)

    validate_in(context, "bars_gap") do
        validate_is_at_least(context, configuration.bars_gap, 0)
        return validate_is_below(context, configuration.bars_gap, 1)
    end

    validate_in(context, "series_gap") do
        validate_is_at_least(context, configuration.series_gap, 0)
        return validate_is_below(context, configuration.series_gap, 1)
    end

    if configuration.stacking !== nothing && configuration.series_gap !== nothing
        throw(ArgumentError("""
                            can't specify both $(location(context)).stacking
                            and $(location(context)).series_gap
                            """))
    end

    return nothing
end

"""
    @kwdef mutable struct SeriesData
        values::ValuesData = ValuesData()
        bars::EntitiesData = EntitiesData()
        name::Maybe{AbstractString} = nothing
        hover::Maybe{AbstractString} = nothing
        color::Maybe{AbstractString} = nothing
    end

One series of a [`SeriesBarsGraphData`](@ref). The `values` are required and numeric, one per bar; their title is the
value axis title shared by all the series. The `bars` hold the hovers and mask of the bars of this series alone. The
`name` is shown in the legend (or, if using `series_gap`, as the title of the series' own axis). The `hover` (if any) is
prefixed to the hover of each bar of the series. All the bars of a series have the same `color`; a `nothing` means the
color is chosen automatically by Plotly.
"""
@kwdef mutable struct SeriesData
    values::ValuesData = ValuesData()
    bars::EntitiesData = EntitiesData()
    name::Maybe{AbstractString} = nothing
    hover::Maybe{AbstractString} = nothing
    color::Maybe{AbstractString} = nothing
end

function Validations.validate(context::ValidationContext, series::SeriesData)::Nothing
    validate_numeric_values(context, "values.values", series.values.values; is_required = true)

    values = series.values.values
    @assert values !== nothing
    validate_vector_is_not_empty(context, "values.values", values)
    n_bars = length(values)

    validate_vector_length(context, "bars.hovers", series.bars.hovers, "values.values", n_bars)
    validate_vector_length(context, "bars.mask", series.bars.mask, "values.values", n_bars)

    validate_in(context, "color") do
        validate_is_color(context, series.color)
        return nothing
    end

    return nothing
end

"""
    @kwdef mutable struct SeriesBarsGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        series::AbstractVector{SeriesData} = SeriesData[]
        names::ValuesData = ValuesData()
        bars::EntitiesData = EntitiesData()
        annotations::AbstractVector{AnnotationData} = AnnotationData[]
    end

The data for a graph of multiple series of bars, a [`SeriesData`](@ref) per series.

All the series must have the same number of bars. The value axis title is the title of the series' values: all the
series that give one must give the same. The `names` values (if any) are strings, one per bar, shown as the bar axis
ticks; their title is the bar axis title. The `bars` hold the hovers and mask shared by the bars of all the series;
masked bars are left out of every series. You can even add annotations to the bars.

The hover of a bar in a series is the `hover` of the series, then the shared hover of the bar, then the hover of the bar
in the series, skipping whichever is not specified.
"""
@kwdef mutable struct SeriesBarsGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    series::AbstractVector{SeriesData} = SeriesData[]
    names::ValuesData = ValuesData()
    bars::EntitiesData = EntitiesData()
    annotations::AbstractVector{AnnotationData} = AnnotationData[]
end

function Validations.validate(context::ValidationContext, data::SeriesBarsGraphData)::Nothing
    validate_vector_is_not_empty(context, "series", data.series)
    validate_string_values(context, "names.values", data.names.values)

    validate_vector_entries(context, "series", data.series) do _, series
        validate(context, series)
        return nothing
    end

    first_values = data.series[1].values.values
    @assert first_values !== nothing
    n_bars = length(first_values)
    for (series_index, series) in enumerate(data.series)
        validate_vector_length(
            context,
            "series[$(series_index)].values.values",
            series.values.values,
            "series[1].values.values",
            n_bars,
        )
    end

    validate_vector_length(context, "names.values", data.names.values, "series[1].values.values", n_bars)
    validate_vector_length(context, "bars.hovers", data.bars.hovers, "series[1].values.values", n_bars)
    validate_vector_length(context, "bars.mask", data.bars.mask, "series[1].values.values", n_bars)

    validate_vector_entries(context, "annotations", data.annotations) do _, annotation
        validate(context, annotation, "series[1].values.values", n_bars)
        return nothing
    end

    shared_values_title(context, "series", "values", [series.values for series in data.series])

    return nothing
end

"""
A graph showing multiple series of bars. See [`SeriesBarsGraphData`](@ref) and [`SeriesBarsGraphConfiguration`](@ref).
"""
SeriesBarsGraph = Graph{SeriesBarsGraphData, SeriesBarsGraphConfiguration}

"""
    function series_bars_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        series::AbstractVector{SeriesData} = SeriesData[],
        names::ValuesData = ValuesData(),
        bars::EntitiesData = EntitiesData(),
        annotations::AbstractVector{AnnotationData} = AnnotationData[],
        configuration::SeriesBarsGraphConfiguration = SeriesBarsGraphConfiguration()]
    )::SeriesBarsGraph

Create a [`SeriesBarsGraph`](@ref) by initializing only the [`SeriesBarsGraphData`](@ref) fields (with an optional
[`SeriesBarsGraphConfiguration`](@ref)).
"""
function series_bars_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    series::AbstractVector{SeriesData} = SeriesData[],
    names::ValuesData = ValuesData(),
    bars::EntitiesData = EntitiesData(),
    annotations::AbstractVector{AnnotationData} = AnnotationData[],
    configuration::SeriesBarsGraphConfiguration = SeriesBarsGraphConfiguration(),
)::SeriesBarsGraph
    return SeriesBarsGraph(SeriesBarsGraphData(; figure_title, series, names, bars, annotations), configuration)
end

# The value axis title shared by the series.
function series_value_axis_title(graph::SeriesBarsGraph)::Maybe{AbstractString}
    return shared_values_title(
        ValidationContext(["graph.data"]),
        "series",
        "values",
        [series.values for series in graph.data.series],
    )
end

function Common.validate_graph(graph::SeriesBarsGraph)::Nothing
    n_series = length(graph.data.series)
    for (series_index, series) in enumerate(graph.data.series)
        values = numeric_values(series.values)
        values_context = ValidationContext(["graph.data.series", series_index, "values.values"])
        validate_values(
            values_context,
            values,
            ValidationContext(["graph.configuration.value_axis"]),
            graph.configuration.value_axis,
        )

        if graph.configuration.stacking == StackFractions
            @assert values !== nothing
            for (bar_index, bar_value) in enumerate(values)
                scaled_value = scale_axis_value(graph.configuration.value_axis, bar_value)
                if scaled_value === nothing || scaled_value < 0
                    throw(
                        ArgumentError(
                            "too low scaled $(location(values_context))[$(bar_index)]: $(scaled_value)\n" *
                            "is not at least: 0\n" *
                            "when using graph.configuration.stacking: StackFractions",
                        ),
                    )
                end
            end
        end

        # Stacking adds up the series bar by bar, so the bars of one series can't be masked on their own.
        if graph.configuration.stacking !== nothing && series.bars.mask !== nothing
            throw(
                ArgumentError(
                    "can't specify both graph.data.series[$(series_index)].bars.mask\n" *
                    "and graph.configuration.stacking",
                ),
            )
        end
    end

    if series_value_axis_title(graph) !== nothing &&
       any(series.name !== nothing for series in graph.data.series) &&
       graph.configuration.series_gap !== nothing
        throw(
            ArgumentError(
                "can't specify both graph.data.series[*].values.title and graph.data.series[*].name\n" *
                "together with graph.configuration.series_gap",
            ),
        )
    end

    if graph.configuration.mirrored && n_series % 2 != 0
        throw(
            ArgumentError("odd number of graph.data.series: $(n_series)\n" * "when using graph.configuration.mirrored"),
        )
    end

    validate_axis_sizes(;
        axis_name = "value",
        graphs_gap = graph.configuration.series_gap,
        n_graphs = n_series,
        annotation_size = graph.configuration.bars_annotations,
        n_annotations = length(graph.data.annotations),
    )

    return nothing
end

# The combination of the mask shared by all the series and the mask of one series (if any).
function combined_mask(
    shared_mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
    series_mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
)::Maybe{Union{AbstractVector{Bool}, BitVector}}
    if shared_mask === nothing
        return series_mask
    elseif series_mask === nothing
        return shared_mask
    else
        return shared_mask .& series_mask
    end
end

# Join two sets of hovers (if any) line by line.
function joined_hovers(
    first_hovers::Maybe{AbstractVector{<:AbstractString}},
    second_hovers::Maybe{AbstractVector{<:AbstractString}},
)::Maybe{AbstractVector{<:AbstractString}}
    if first_hovers === nothing
        return second_hovers
    elseif second_hovers === nothing
        return first_hovers
    else
        return first_hovers .* "<br>" .* second_hovers
    end
end

# Widen a range to include zero, which is where a bar is measured from - so a bar graph whose values are all far from
# zero still shows their sizes rather than their differences. A log scale never reaches zero, so there a bar is measured
# from the smallest value shown.
function collect_zero_range!(scaled_range::MaybeRange, value_axis::AxisConfiguration)::Nothing
    if value_axis.log_scale === nothing
        collect_range!(scaled_range, (0.0,))  # NOJET
    end
    return nothing
end

# The number of value axes. Mirrored, each pair of series needs one axis per side; without a `series_gap` all the pairs
# share the same two, and with one each pair is given two of its own. Otherwise there is an axis per series, or one for
# all of them when there is no `series_gap` to separate them.
function n_value_axes(configuration::SeriesBarsGraphConfiguration, n_series::Integer)::Int
    if configuration.mirrored
        return configuration.series_gap === nothing ? 2 : n_series
    else
        return configuration.series_gap === nothing ? 1 : n_series
    end
end

# The value axis a series is drawn on. Mirrored, the 1st series of each pair is drawn on the axis growing one way and
# the 2nd on the axis growing the other way.
function value_axis_index(configuration::SeriesBarsGraphConfiguration, series_index::Integer)::Int
    if !configuration.mirrored
        return configuration.series_gap === nothing ? 1 : series_index
    elseif configuration.series_gap === nothing
        return 2 - series_index % 2
    else
        return series_index
    end
end

# The value axis of the other side of the same pair, which shows the same range so that both sides are drawn to the
# same scale.
function mirror_value_axis_index(value_axis_index::Integer)::Int
    return value_axis_index + (value_axis_index % 2 == 1 ? 1 : -1)
end

# The gap between the value axes. Mirrored, the two axes of a pair are adjacent whether or not there is a `series_gap`
# between the pairs, so they are laid out with an explicit zero gap rather than with no gap at all - which would place
# them on top of each other.
function value_axes_gap(configuration::SeriesBarsGraphConfiguration)::Maybe{Real}
    if configuration.mirrored
        return configuration.series_gap === nothing ? 0.0 : configuration.series_gap
    else
        return configuration.series_gap
    end
end

function Common.graph_to_figure(graph::SeriesBarsGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    implicit_values_range = MaybeRange()

    all_series = graph.data.series
    n_series = length(all_series)
    first_values = numeric_values(all_series[1].values)
    @assert first_values !== nothing
    n_bars = length(first_values)

    shared_mask = graph.data.bars.mask
    # Default names are given before masking so masked out bars do not shift the names of the rest.
    default_names = prefer_data(string_values(graph.data.names), string.(1:n_bars))
    names = masked_values(default_names, shared_mask, nothing)
    bars_hovers = masked_values(graph.data.bars.hovers, shared_mask, nothing)
    annotations = [masked_annotation(annotation, shared_mask) for annotation in graph.data.annotations]

    n_axes = n_value_axes(graph.configuration, n_series)

    common_implicit_values_range = MaybeRange()
    total_scaled_values_per_axis = Maybe{AbstractVector{<:AbstractFloat}}[nothing for _ in 1:n_axes]
    specific_scaled_values = Vector{AbstractVector{<:Real}}(undef, n_series)
    if n_axes == 1
        specific_scaled_ranges = nothing
    else
        specific_scaled_ranges = MaybeRange[MaybeRange() for _ in 1:n_axes]
    end

    show_in_legend = any(series.name !== nothing for series in all_series) && graph.configuration.series_gap === nothing
    has_hovers = false

    for (series_index, series) in enumerate(all_series)
        series_mask = combined_mask(shared_mask, series.bars.mask)
        values = numeric_values(series.values)
        @assert values !== nothing
        values = masked_values(values, series_mask, nothing)

        hovers = joined_hovers(
            masked_values(graph.data.bars.hovers, series_mask, nothing),
            masked_values(series.bars.hovers, series_mask, nothing),
        )
        if series.hover !== nothing
            if hovers === nothing
                hovers = fill(series.hover, length(values))
            else
                hovers = "$(series.hover)<br>" .* hovers
            end
        end
        has_hovers |= hovers !== nothing

        if graph.configuration.stacking === nothing
            implicit_values_range = common_implicit_values_range
        else
            implicit_values_range = MaybeRange()
        end

        scaled_values = push_bar_trace!(;
            traces,
            sub_graph = SubGraph(;
                index = value_axis_index(graph.configuration, series_index),
                n_graphs = n_axes,
                graphs_gap = value_axes_gap(graph.configuration),
                mirrored = graph.configuration.mirrored,
                n_annotations = length(annotations),
                annotation_size = graph.configuration.bars_annotations,
            ),
            name = series.name,
            values,
            value_axis = graph.configuration.value_axis,
            values_orientation = graph.configuration.values_orientation,
            color = series.color,
            hovers,
            show_in_legend,
            names = masked_values(default_names, series_mask, nothing),
            implicit_values_range,
        )

        specific_scaled_values[series_index] = scaled_values
        axis_index = value_axis_index(graph.configuration, series_index)

        # Stacked, it is the totals below which say how far each axis has to reach, so they are collected instead.
        if specific_scaled_ranges !== nothing && graph.configuration.stacking === nothing
            collect_range!(specific_scaled_ranges[axis_index], scaled_values)
            if graph.configuration.mirrored
                collect_range!(specific_scaled_ranges[mirror_value_axis_index(axis_index)], scaled_values)
            end
        end

        if graph.configuration.stacking !== nothing
            total_scaled_values = total_scaled_values_per_axis[axis_index]
            if total_scaled_values === nothing
                total_scaled_values_per_axis[axis_index] = copy(scaled_values)
            else
                total_scaled_values .+= scaled_values
            end
        end
    end

    if graph.configuration.stacking == StackValues
        for (axis_index, total_scaled_values) in enumerate(total_scaled_values_per_axis)
            @assert total_scaled_values !== nothing
            if specific_scaled_ranges === nothing
                collect_range!(implicit_values_range, total_scaled_values)
            else
                collect_range!(specific_scaled_ranges[axis_index], total_scaled_values)
                collect_range!(specific_scaled_ranges[mirror_value_axis_index(axis_index)], total_scaled_values)
            end
        end

    elseif graph.configuration.stacking == StackFractions
        for total_scaled_values in total_scaled_values_per_axis
            @assert total_scaled_values !== nothing
            total_scaled_values[total_scaled_values .== 0] .= 1
        end

        for series_index in 1:n_series
            specific_scaled_values[series_index] ./=
                total_scaled_values_per_axis[value_axis_index(graph.configuration, series_index)]
            if graph.configuration.value_axis.percent
                specific_scaled_values[series_index] .*= 100
            end
        end
        if graph.configuration.value_axis.percent
            implicit_values_range = MaybeRange(; minimum = 0, maximum = 100)
        else
            implicit_values_range = MaybeRange(; minimum = 0, maximum = 1)
        end
        # A range of its own for each axis, rather than one shared between them, since they are mutated below.
        if specific_scaled_ranges !== nothing
            for axis_index in 1:n_axes
                specific_scaled_ranges[axis_index] =
                    MaybeRange(; minimum = implicit_values_range.minimum, maximum = implicit_values_range.maximum)
            end
        end

    else
        @assert graph.configuration.stacking === nothing
        implicit_values_range = common_implicit_values_range
    end

    collect_zero_range!(implicit_values_range, graph.configuration.value_axis)
    if specific_scaled_ranges !== nothing
        for specific_scaled_range in specific_scaled_ranges
            collect_zero_range!(specific_scaled_range, graph.configuration.value_axis)
        end
    end

    next_colors_scale_index = [1]
    has_legend_only_traces = [false]
    annotations_colors = push_annotations_traces!(;
        traces,
        names,
        value_axis = graph.configuration.value_axis,
        values_orientation = graph.configuration.values_orientation,
        n_graphs = n_axes,
        graphs_gap = value_axes_gap(graph.configuration),
        mirrored = graph.configuration.mirrored,
        next_colors_scale_index,
        has_legend_only_traces,
        annotations_data = annotations,
        annotation_size = graph.configuration.bars_annotations,
        entries_hovers = bars_hovers,
    )

    layout = bars_layout(;
        graph,
        has_tick_names = graph.data.names.values !== nothing,
        has_legend = show_in_legend,
        has_hovers,
        implicit_values_range,
        specific_scaled_ranges,
        annotations_colors,
        has_legend_only_traces,
    )

    return plotly_figure(traces, layout)
end

function push_bar_trace!(;
    traces::Vector{GenericTrace},
    values::AbstractVector{<:Real},
    value_axis::AxisConfiguration,
    basis_sub_graph::Maybe{SubGraph} = nothing,
    values_orientation::ValuesOrientation,
    color::Maybe{Union{AbstractVector{<:Union{Real, Missing}}, AbstractVector{<:AbstractString}, AbstractString}} = nothing,
    hovers::Maybe{AbstractVector{<:AbstractString}} = nothing,
    names::Maybe{AbstractVector{<:AbstractString}} = nothing,
    name::Maybe{AbstractString} = nothing,
    sub_graph::SubGraph,
    show_in_legend::Bool = false,
    implicit_values_range::MaybeRange,
    colors_scale_index::Maybe{Integer} = nothing,
)::AbstractVector{<:AbstractFloat}
    xaxis_index, x0, yaxis_index, y0 =
        plotly_sub_graph_axes(; basis_sub_graph, values_sub_graph = sub_graph, values_orientation)

    scaled_values = scale_axis_values(value_axis, values; clamp = false)
    collect_range!(implicit_values_range, scaled_values)

    if names === nothing
        names = [string(index) for index in 1:length(scaled_values)]
    end

    if values_orientation == VerticalValues
        xs = names
        ys = scaled_values
        orientation = "v"
    elseif values_orientation == HorizontalValues
        xs = scaled_values
        ys = names
        orientation = "h"
    else
        @assert false
    end

    push!(
        traces,
        bar(;
            x = xs,
            y = ys,
            x0,
            y0,
            xaxis = plotly_axis("x", xaxis_index; short = true),
            yaxis = plotly_axis("y", yaxis_index; short = true),
            name,
            orientation = orientation,
            marker_color = color,
            marker_coloraxis = plotly_axis("color", colors_scale_index),
            customdata = hovers,
            hovertemplate = hovers === nothing ? nothing : "%{customdata}<extra></extra>",
            showlegend = show_in_legend,
        ),
    )

    return scaled_values
end

function push_annotations_traces!(;
    traces::Vector{GenericTrace},
    names::Maybe{AbstractVector{<:AbstractString}},
    value_axis::AxisConfiguration,
    basis_sub_graph::Maybe{SubGraph} = nothing,
    values_orientation::ValuesOrientation,
    n_graphs::Integer = 1,
    graphs_gap::Maybe{Real} = nothing,
    mirrored::Bool = false,
    next_colors_scale_index::AbstractVector{<:Integer},
    has_legend_only_traces::AbstractVector{Bool},
    annotations_data::AbstractVector{AnnotationData},
    annotation_size::AnnotationSize,
    entries_hovers::Maybe{AbstractVector{<:AbstractString}},
    order::Maybe{AbstractVector{<:Integer}} = nothing,
    expanded_mask::Maybe{Union{BitVector, AbstractVector{Bool}}} = nothing,
)::AbstractVector{ConfiguredColors}
    return ConfiguredColors[
        push_annotation_traces!(;
            traces,
            names,
            value_axis,
            basis_sub_graph,
            values_orientation,
            n_graphs,
            graphs_gap,
            mirrored,
            annotation_index,
            n_annotations = length(annotations_data),
            annotation_data,
            annotation_size,
            entries_hovers,
            next_colors_scale_index,
            has_legend_only_traces,
            order,
            expanded_mask,
        ) for (annotation_index, annotation_data) in enumerate(annotations_data)
    ]
end

function push_annotation_traces!(;
    traces::Vector{GenericTrace},
    names::Maybe{AbstractVector{<:AbstractString}},
    value_axis::AxisConfiguration,
    basis_sub_graph::Maybe{SubGraph},
    values_orientation::ValuesOrientation,
    n_graphs::Integer,
    graphs_gap::Maybe{Real},
    mirrored::Bool,
    annotation_index::Integer,
    n_annotations::Integer,
    annotation_data::AnnotationData,
    annotation_size::AnnotationSize,
    entries_hovers::Maybe{AbstractVector{<:AbstractString}},
    next_colors_scale_index::AbstractVector{<:Integer},
    has_legend_only_traces::AbstractVector{Bool},
    order::Maybe{AbstractVector{<:Integer}},
    expanded_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
)::ConfiguredColors
    colors = configured_colors(;
        colors_configuration = annotation_data.colors,
        colors_title = annotation_data.title,
        colors_values = annotation_data.values,
        next_colors_scale_index,
    )

    sub_graph = SubGraph(; index = -annotation_index, n_graphs, graphs_gap, mirrored, n_annotations, annotation_size)

    if colors.show_in_legend && colors.colors_configuration.palette isa CategoricalColors
        legend_group = "Annotation$(annotation_index)"
        palette_dict = colors.colors_configuration.palette
        if palette_dict isa NamedArray
            palette_dict = Dict(zip(names(palette_dict, 1), palette_dict.array))  # UNTESTED # NOJET
        end
        for (index, (value, color)) in enumerate(palette_dict)
            has_legend_only_traces[1] = true
            push_annotation_legend_trace!(;
                traces,
                color,
                value,
                legend_group,
                legend_group_title = index == 1 ? annotation_data.title : nothing,
            )
        end
    end

    @assert colors.final_colors_values !== nothing
    if eltype(colors.final_colors_values) <: AbstractString
        gap_color = "white"
    else
        # `missing` (serialized as JSON `null`) rather than `NaN`, which the JSON writer used by `to_html` rejects.
        gap_color = missing
    end

    hovers = annotation_data.hovers
    if hovers === nothing
        hovers = entries_hovers
    elseif entries_hovers !== nothing  # UNTESTED
        hovers = hovers .* "<br>" .* entries_hovers  # UNTESTED
    end

    push_bar_trace!(;  # NOJET
        traces,
        sub_graph,
        values = expanded_mask !== nothing ? expanded_mask : fill(1.0, length(annotation_data.values)),
        value_axis = AxisConfiguration(;
            minimum = 0,
            maximum = 1,
            show_ticks = false,
            show_grid = value_axis.show_grid,
        ),
        basis_sub_graph,
        values_orientation,
        color = expand_vector(colors.final_colors_values, order, expanded_mask, gap_color),
        hovers = expand_vector(hovers, order, expanded_mask, ""),
        names = expand_vector(names, order, expanded_mask, ""),
        name = annotation_data.title,
        show_in_legend = false,
        implicit_values_range = MaybeRange(),
        colors_scale_index = colors.colors_scale_index,
    )

    return colors
end

function expand_vector(
    values::Maybe{AbstractVector},
    order::Maybe{AbstractVector{<:Integer}},
    expanded_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
    default_value::Any,
)::Maybe{AbstractVector}
    if values === nothing || (order === nothing && expanded_mask === nothing)
        return values
    end

    if order !== nothing
        @views values = values[order]
    end

    if expanded_mask === nothing
        return values
    end

    # The element type must hold both the values and the (possibly `missing`) gap default.
    expanded_values = Vector{Union{eltype(values), typeof(default_value)}}(undef, length(expanded_mask))
    fill!(expanded_values, default_value)
    expanded_values[expanded_mask] .= values

    return expanded_values
end

function push_annotation_legend_trace!(;
    traces::Vector{GenericTrace},
    color::AbstractString,
    value::AbstractString,
    legend_group::Maybe{AbstractString},
    legend_group_title::Maybe{AbstractString},
)::Nothing
    push!(
        traces,
        bar(;
            x = [0],
            y = [0],
            x0 = 0,
            y0 = 0,
            xaxis = "x99",
            yaxis = "y99",
            name = value,
            marker_color = color,
            legendgroup = legend_group,
            legendgrouptitle_text = legend_group_title,
            showlegend = true,
        ),
    )

    return nothing
end

# The titles of the value and bar axes given in the graph data.
function data_axes_titles(graph::BarsGraph)::Tuple{Maybe{AbstractString}, Maybe{AbstractString}}
    return (graph.data.values.title, graph.data.names.title)
end

function data_axes_titles(graph::SeriesBarsGraph)::Tuple{Maybe{AbstractString}, Maybe{AbstractString}}
    return (series_value_axis_title(graph), graph.data.names.title)
end

function bars_layout(;
    graph::Union{BarsGraph, SeriesBarsGraph},
    has_tick_names::Bool,
    has_legend::Bool,
    has_hovers::Bool = false,
    implicit_values_range::MaybeRange,
    specific_scaled_ranges::Maybe{AbstractVector{MaybeRange}} = nothing,
    colors::Maybe{ConfiguredColors} = nothing,
    annotations_colors::AbstractVector{ConfiguredColors},
    has_legend_only_traces::AbstractVector{Bool},
)::Layout
    scaled_values_range = final_scaled_range(implicit_values_range, graph.configuration.value_axis)  # NOJET

    if specific_scaled_ranges !== nothing
        specific_scaled_ranges = [
            final_scaled_range(specific_scaled_range, graph.configuration.value_axis) for
            specific_scaled_range in specific_scaled_ranges
        ]
    end

    shapes = Shape[]

    if graph isa BarsGraph
        if graph.configuration.values_orientation == VerticalValues
            push_horizontal_bands_shapes(
                shapes,
                graph.configuration.value_axis,
                scaled_values_range,
                graph.data.value_bands,
                graph.configuration.value_bands,
            )
        else
            push_vertical_bands_shapes(
                shapes,
                graph.configuration.value_axis,
                scaled_values_range,
                graph.data.value_bands,
                graph.configuration.value_bands,
            )
        end
    end

    n_annotations = length(annotations_colors)
    if n_annotations > 0
        has_legend = has_legend || any([annotation_colors.show_in_legend for annotation_colors in annotations_colors])
    end

    layout = plotly_layout(graph.configuration.figure; title = graph.data.figure_title, has_legend, has_hovers, shapes)

    value_axis_title, bar_axis_title = data_axes_titles(graph)

    if graph isa SeriesBarsGraph
        if graph.configuration.stacking == StackValues
            layout["barmode"] = "stack"
        elseif graph.configuration.stacking == StackFractions
            layout["barmode"] = "relative"
        else
            @assert graph.configuration.stacking === nothing
        end
        n_series = length(graph.data.series)  # NOJET
        graphs_gap = value_axes_gap(graph.configuration)  # NOJET
        n_graphs = n_value_axes(graph.configuration, n_series)  # NOJET
        mirrored = graph.configuration.mirrored
    else
        n_series = 1
        graphs_gap = nothing
        n_graphs = 1
        mirrored = false
    end

    if graph.configuration.values_orientation == VerticalValues
        value_axis_letter = "y"
        bar_axis_letter = "x"
    elseif graph.configuration.values_orientation == HorizontalValues
        value_axis_letter = "x"
        bar_axis_letter = "y"
    else
        @assert false
    end

    layout["bargap"] = graph.configuration.bars_gap

    annotation_size = graph.configuration.bars_annotations

    if specific_scaled_ranges === nothing
        axis_index = 1 + n_annotations
        set_layout_axis!(
            layout,
            plotly_axis(value_axis_letter, axis_index),
            graph.configuration.value_axis;
            title = prefer_data(value_axis_title, graph.configuration.value_axis.title),
            range = scaled_values_range,
            domain = plotly_sub_graph_domain(
                SubGraph(; index = 1, n_graphs, graphs_gap, mirrored, n_annotations, annotation_size),
            ),
        )
    else
        @assert graph isa SeriesBarsGraph
        for value_axis_index in 1:n_graphs
            axis_index = value_axis_index + n_annotations
            # Only an axis of its own can be titled by a series; when the pairs share their axes, the series are named
            # in the legend instead.
            if graph.configuration.series_gap === nothing
                title = prefer_data(value_axis_title, graph.configuration.value_axis.title)
            else
                title = prefer_data(
                    graph.data.series[value_axis_index].name,  # NOJET
                    prefer_data(value_axis_title, graph.configuration.value_axis.title),
                )
            end
            set_layout_axis!(  # NOJET
                layout,
                plotly_axis(value_axis_letter, axis_index),
                graph.configuration.value_axis;
                title,
                range = specific_scaled_ranges[value_axis_index],
                domain = plotly_sub_graph_domain(
                    SubGraph(;
                        index = value_axis_index,
                        n_graphs,
                        graphs_gap,
                        mirrored,
                        n_annotations,
                        annotation_size,
                    ),
                ),
                # Mirrored, the 1st axis of each pair grows away from the shared bar axis, which is to its right.
                is_reversed = mirrored && value_axis_index % 2 == 1,
            )
        end
    end

    next_colors_scale_offset_index = [Int(has_legend)]

    # The bar axis is drawn against the 1st axis of the other direction, which is the 1st annotation when there is one.
    # That is where the names belong for any other graph - outside everything - but a mirrored pair puts its
    # annotations in the spine, so there the names are anchored to the 1st of the two sides instead.
    if mirrored && n_graphs == 2 && n_annotations > 0
        bar_axis_anchor = plotly_axis(value_axis_letter, 1 + n_annotations; short = true, force = true)
    else
        bar_axis_anchor = nothing
    end

    layout["$(bar_axis_letter)axis"] = Dict(
        :showgrid => false,
        :showticklabels => has_tick_names,
        :title => bar_axis_title,
        :anchor => bar_axis_anchor,
    )

    if colors !== nothing && colors.colors_scale_index !== nothing
        set_layout_colorscale!(;
            layout,
            colors_scale_index = colors.colors_scale_index,
            colors_configuration = colors.colors_configuration,
            scaled_colors_palette = colors.scaled_colors_palette,
            range = colors.final_colors_range,
            title = colors.colors_title,
            show_scale = colors.show_scale,
            next_colors_scale_offset_index,
            colors_scale_offsets = graph.configuration.figure.colors_scale_offsets,
        )
    end

    layout["annotations"] = plotly_annotations = []
    for (annotation_index, annotation_colors) in enumerate(annotations_colors)
        annotation_data = graph.data.annotations[annotation_index]
        sub_graph =
            SubGraph(; index = -annotation_index, n_graphs, graphs_gap, mirrored, n_annotations, annotation_size)
        push_plotly_annotation!(;
            plotly_annotations,
            values_sub_graph = sub_graph,
            values_orientation = graph.configuration.values_orientation,
            title = annotation_data.title,
        )
        set_layout_axis!(  # NOJET
            layout,
            plotly_axis(value_axis_letter, annotation_index),
            graph.configuration.value_axis;
            range = Range(; minimum = 0, maximum = 1),
            domain = plotly_sub_graph_domain(sub_graph),
            is_tick_axis = false,
            is_zeroable = false,
        )
        if annotation_colors.colors_scale_index !== nothing
            set_layout_colorscale!(;
                layout,
                colors_scale_index = annotation_colors.colors_scale_index,
                colors_configuration = annotation_data.colors,
                scaled_colors_palette = annotation_colors.scaled_colors_palette,
                range = nothing,
                annotation_data.title,
                show_scale = annotation_colors.show_scale,
                next_colors_scale_offset_index,
                colors_scale_offsets = graph.configuration.figure.colors_scale_offsets,
            )
        end
    end

    if has_legend_only_traces[1]
        layout["xaxis99"] = Dict(:domain => [0, 0.001], :showgrid => false, :showticklabels => false)
        layout["yaxis99"] = Dict(:domain => [0, 0.001], :showgrid => false, :showticklabels => false)
    end

    return layout
end

function push_plotly_annotation!(;
    plotly_annotations::AbstractVector,
    basis_sub_graph::Maybe{SubGraph} = nothing,
    values_sub_graph::Maybe{SubGraph} = nothing,
    values_orientation::ValuesOrientation,
    title::Maybe{AbstractString},
)::Nothing
    if title !== nothing
        xaxis_index, _, yaxis_index, _ = plotly_sub_graph_axes(; values_sub_graph, basis_sub_graph, values_orientation)
        if values_orientation == VerticalValues
            x = 0
            y = 0.5
            xanchor = "right"
            yanchor = "middle"
            textangle = nothing
        else
            x = 0.5
            y = 0
            xanchor = "center"
            yanchor = "top"
            textangle = -90
        end
        push!(
            plotly_annotations,
            Dict(
                :text => title,
                :textangle => textangle,
                :x => x,
                :y => y,
                :xanchor => xanchor,
                :yanchor => yanchor,
                :xref => "$(plotly_axis("x", xaxis_index; short = true, force = true)) domain",
                :yref => "$(plotly_axis("y", yaxis_index; short = true, force = true)) domain",
                :showarrow => false,
            ),
        )
        return nothing
    end
end

end
