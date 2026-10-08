# The data source views of each graph type: the implementations of the view accessors (`x_axis_vector_fields`, ...)
# and of the hooks of `PartFields`. This is included at the end of the `Sources` module, which comes after all the graph
# types, so the views can name them.

"""
    distribution_axis_vector_fields(graph::DistributionGraph)::AxisVectorFields

The values of the distribution, along the `value_axis`.
"""
function distribution_axis_vector_fields(graph::DistributionGraph)::AxisVectorFields
    distribution = graph.data.distribution
    return VectorFields(
        distribution.values,
        distribution.points,
        AxisConfigurationFields(graph.configuration.value_axis),
    )
end

function distribution_entities(graph::DistributionGraph)::VectorEntitiesData
    return graph.data.distribution.points
end

entities_field(::DistributionData)::Symbol = :points

function part_role_fields(part::PartFields{DistributionsGraph, DistributionData}, ::Val{:values})::AxisVectorFields
    distribution = part.data
    return VectorFields(
        distribution.values,
        distribution.points,
        AxisConfigurationFields(part.graph.configuration.value_axis),
    )
end

"""
    distribution_part_fields(graph::DistributionsGraph, index::Integer)::PartFields

The view of the `index` distribution (see [`PartFields`](@ref)).
"""
function distribution_part_fields(graph::DistributionsGraph, index::Integer)::PartFields
    distribution::DistributionData = graph.data.distributions[index]
    return PartFields(graph, Int(index), distribution)
end

"""
    add_distribution!(graph::DistributionsGraph, [distribution::DistributionData = DistributionData()])::PartFields

Append a `distribution` and return its view (see [`PartFields`](@ref)).
"""
function add_distribution!(graph::DistributionsGraph, distribution::DistributionData = DistributionData())::PartFields
    push!(graph.data.distributions, distribution)
    n_distributions::Int = length(graph.data.distributions)
    return PartFields(graph, n_distributions, distribution)
end

"""
    x_axis_vector_fields(graph::PointsGraph)::AxisVectorFields

The X coordinates of the points, along the `x_axis`.
"""
function x_axis_vector_fields(graph::PointsGraph)::AxisVectorFields
    return VectorFields(graph.data.x, graph.data.points.entities, AxisConfigurationFields(graph.configuration.x_axis))
end

"""
    y_axis_vector_fields(graph::PointsGraph)::AxisVectorFields

The Y coordinates of the points, along the `y_axis`.
"""
function y_axis_vector_fields(graph::PointsGraph)::AxisVectorFields
    return VectorFields(graph.data.y, graph.data.points.entities, AxisConfigurationFields(graph.configuration.y_axis))
end

"""
    points_colors_vector_fields(graph::PointsGraph)::ColorsVectorFields

The colors of the points.
"""
function points_colors_vector_fields(graph::PointsGraph)::ColorsVectorFields
    return VectorFields(
        graph.data.points.colors,
        graph.data.points.entities,
        ColorsConfigurationFields(graph.configuration.points.colors),
    )
end

"""
    points_sizes_vector_fields(graph::PointsGraph)::SizesVectorFields

The sizes of the points.
"""
function points_sizes_vector_fields(graph::PointsGraph)::SizesVectorFields
    return VectorFields(
        graph.data.points.sizes,
        graph.data.points.entities,
        SizesConfigurationFields(graph.configuration.points.sizes),
    )
end

"""
    borders_colors_vector_fields(graph::PointsGraph)::ColorsVectorFields

The colors of the borders of the points, which share the entities of the points.
"""
function borders_colors_vector_fields(graph::PointsGraph)::ColorsVectorFields
    return VectorFields(
        graph.data.borders.colors,
        graph.data.points.entities,
        ColorsConfigurationFields(graph.configuration.borders.colors),
    )
end

"""
    borders_sizes_vector_fields(graph::PointsGraph)::SizesVectorFields

The sizes of the borders of the points, which share the entities of the points.
"""
function borders_sizes_vector_fields(graph::PointsGraph)::SizesVectorFields
    return VectorFields(
        graph.data.borders.sizes,
        graph.data.points.entities,
        SizesConfigurationFields(graph.configuration.borders.sizes),
    )
end

"""
    edges_colors_vector_fields(graph::PointsGraph)::ColorsVectorFields

The colors of the edges.
"""
function edges_colors_vector_fields(graph::PointsGraph)::ColorsVectorFields
    return VectorFields(
        graph.data.edges.colors,
        graph.data.edges.entities,
        ColorsConfigurationFields(graph.configuration.edges.colors),
    )
end

function points_entities(graph::PointsGraph)::VectorEntitiesData
    return graph.data.points.entities
end

function edges_entities(graph::PointsGraph)::VectorEntitiesData
    return graph.data.edges.entities
end

"""
    edges_sizes_vector_fields(graph::PointsGraph)::SizesVectorFields

The sizes (widths) of the edges.
"""
function edges_sizes_vector_fields(graph::PointsGraph)::SizesVectorFields
    return VectorFields(
        graph.data.edges.sizes,
        graph.data.edges.entities,
        SizesConfigurationFields(graph.configuration.edges.sizes),
    )
end

"""
    x_axis_vector_fields(graph::LineGraph)::AxisVectorFields

The X coordinates of the points of the line, along the `x_axis`.
"""
function x_axis_vector_fields(graph::LineGraph)::AxisVectorFields
    return VectorFields(graph.data.x, graph.data.points, AxisConfigurationFields(graph.configuration.x_axis))
end

"""
    y_axis_vector_fields(graph::LineGraph)::AxisVectorFields

The Y coordinates of the points of the line, along the `y_axis`.
"""
function y_axis_vector_fields(graph::LineGraph)::AxisVectorFields
    return VectorFields(graph.data.y, graph.data.points, AxisConfigurationFields(graph.configuration.y_axis))
end

function points_entities(graph::LineGraph)::VectorEntitiesData
    return graph.data.points
end

entities_field(::LineData)::Symbol = :points

function part_role_fields(part::PartFields{LinesGraph, LineData}, ::Val{:x})::AxisVectorFields
    line = part.data
    return VectorFields(line.x, line.points, AxisConfigurationFields(part.graph.configuration.x_axis))
end

function part_role_fields(part::PartFields{LinesGraph, LineData}, ::Val{:y})::AxisVectorFields
    line = part.data
    return VectorFields(line.y, line.points, AxisConfigurationFields(part.graph.configuration.y_axis))
end

"""
    line_part_fields(graph::LinesGraph, index::Integer)::PartFields

The view of the `index` line (see [`PartFields`](@ref)).
"""
function line_part_fields(graph::LinesGraph, index::Integer)::PartFields
    line::LineData = graph.data.lines[index]
    return PartFields(graph, Int(index), line)
end

"""
    add_line!(graph::LinesGraph, [line::LineData = LineData()])::PartFields

Append a `line` and return its view (see [`PartFields`](@ref)).
"""
function add_line!(graph::LinesGraph, line::LineData = LineData())::PartFields
    push!(graph.data.lines, line)
    n_lines::Int = length(graph.data.lines)
    return PartFields(graph, n_lines, line)
end

"""
    values_axis_vector_fields(graph::BarsGraph)::AxisVectorFields

The values of the bars, along the `value_axis`.
"""
function values_axis_vector_fields(graph::BarsGraph)::AxisVectorFields
    return VectorFields(graph.data.values, graph.data.bars, AxisConfigurationFields(graph.configuration.value_axis))
end

function bars_entities(graph::BarsGraph)::VectorEntitiesData
    return graph.data.bars
end

"""
    colors_vector_fields(graph::BarsGraph)::ColorsVectorFields

The colors of the bars.
"""
function colors_vector_fields(graph::BarsGraph)::ColorsVectorFields
    return VectorFields(graph.data.colors, graph.data.bars, ColorsConfigurationFields(graph.configuration.colors))
end

"""
    annotations_colors_vector_fields(graph::BarsGraph, index::Integer)::ColorsVectorFields

The `index` annotation of the bars, which shares the entities of the bars.
"""
function annotations_colors_vector_fields(graph::BarsGraph, index::Integer)::ColorsVectorFields
    annotation = graph.data.annotations[index]
    return VectorFields(annotation.values, graph.data.bars, ColorsConfigurationFields(annotation.colors))
end

"""
    add_annotation!(graph::BarsGraph, [annotation::AnnotationData = AnnotationData()])::Int

Append an `annotation` of the bars and return its index.
"""
function add_annotation!(graph::BarsGraph, annotation::AnnotationData = AnnotationData())::Int
    push!(graph.data.annotations, annotation)
    return length(graph.data.annotations)
end

entities_field(::SeriesData)::Symbol = :bars

# The entities of a series are the bars of that series alone, rather than the bars shared by all of them.
function part_role_fields(part::PartFields{SeriesBarsGraph, SeriesData}, ::Val{:values})::AxisVectorFields
    series = part.data
    return VectorFields(series.values, series.bars, AxisConfigurationFields(part.graph.configuration.value_axis))
end

"""
    series_part_fields(graph::SeriesBarsGraph, index::Integer)::PartFields

The view of the `index` series of bars (see [`PartFields`](@ref)).
"""
function series_part_fields(graph::SeriesBarsGraph, index::Integer)::PartFields
    series::SeriesData = graph.data.series[index]
    return PartFields(graph, Int(index), series)
end

"""
    add_series!(graph::SeriesBarsGraph, [series::SeriesData = SeriesData()])::PartFields

Append a `series` of bars and return its view (see [`PartFields`](@ref)).
"""
function add_series!(graph::SeriesBarsGraph, series::SeriesData = SeriesData())::PartFields
    push!(graph.data.series, series)
    n_series::Int = length(graph.data.series)
    return PartFields(graph, n_series, series)
end

"""
    annotations_colors_vector_fields(graph::SeriesBarsGraph, index::Integer)::ColorsVectorFields

The `index` annotation of the bars, which shares the entities of the bars (the ones shared by all the series).
"""
function annotations_colors_vector_fields(graph::SeriesBarsGraph, index::Integer)::ColorsVectorFields
    annotation = graph.data.annotations[index]
    return VectorFields(annotation.values, graph.data.bars, ColorsConfigurationFields(annotation.colors))
end

function bars_entities(graph::SeriesBarsGraph)::VectorEntitiesData
    return graph.data.bars
end

"""
    add_annotation!(graph::SeriesBarsGraph, [annotation::AnnotationData = AnnotationData()])::Int

Append an `annotation` of the bars (the ones shared by all the series) and return its index.
"""
function add_annotation!(graph::SeriesBarsGraph, annotation::AnnotationData = AnnotationData())::Int
    push!(graph.data.annotations, annotation)
    return length(graph.data.annotations)
end

"""
    entries_matrix_fields(graph::HeatmapGraph)::MatrixFields

The entries of the heatmap, colored by the `entries.colors`. This also gives access to the entities of the rows and of
the columns, so a source can add hovers per cell, per row and per column.
"""
function entries_matrix_fields(graph::HeatmapGraph)::MatrixFields
    return MatrixFields(
        graph.data.entries,
        graph.data.cells,
        graph.data.rows.entities,
        graph.data.columns.entities,
        graph.configuration.entries.colors,
    )
end

function rows_entities(graph::HeatmapGraph)::VectorEntitiesData
    return graph.data.rows.entities
end

function columns_entities(graph::HeatmapGraph)::VectorEntitiesData
    return graph.data.columns.entities
end

function rows_arrangement(graph::HeatmapGraph)::ArrangementData
    return graph.data.rows.arrangement
end

function columns_arrangement(graph::HeatmapGraph)::ArrangementData
    return graph.data.columns.arrangement
end

function rows_side(graph::HeatmapGraph)::HeatmapSide
    return HeatmapSide(graph, true)
end

function columns_side(graph::HeatmapGraph)::HeatmapSide
    return HeatmapSide(graph, false)
end

function side_data(side::HeatmapSide)::HeatmapSideData
    return side.is_rows ? side.graph.data.rows : side.graph.data.columns
end

function side_configuration(side::HeatmapSide)::HeatmapSideConfiguration
    return side.is_rows ? side.graph.configuration.rows : side.graph.configuration.columns
end

function side_placement(side::HeatmapSide)::SidePlacement
    placement = heatmap_placement(side.graph)
    return side.is_rows ? placement.rows : placement.columns
end

function reset_side_placement!(side::HeatmapSide)::Nothing
    return reset_placement!(side.graph)
end

"""
    rows_annotations_colors_vector_fields(graph::HeatmapGraph, index::Integer)::ColorsVectorFields

The `index` annotation of the rows, which shares the entities of the rows.
"""
function rows_annotations_colors_vector_fields(graph::HeatmapGraph, index::Integer)::ColorsVectorFields
    annotation = graph.data.rows.annotations[index]
    return VectorFields(annotation.values, graph.data.rows.entities, ColorsConfigurationFields(annotation.colors))
end

"""
    add_rows_annotation!(graph::HeatmapGraph, [annotation::AnnotationData = AnnotationData()])::Int

Append an `annotation` of the rows and return its index.
"""
function add_rows_annotation!(graph::HeatmapGraph, annotation::AnnotationData = AnnotationData())::Int
    push!(graph.data.rows.annotations, annotation)
    return length(graph.data.rows.annotations)
end

"""
    columns_annotations_colors_vector_fields(graph::HeatmapGraph, index::Integer)::ColorsVectorFields

The `index` annotation of the columns, which shares the entities of the columns.
"""
function columns_annotations_colors_vector_fields(graph::HeatmapGraph, index::Integer)::ColorsVectorFields
    annotation = graph.data.columns.annotations[index]
    return VectorFields(annotation.values, graph.data.columns.entities, ColorsConfigurationFields(annotation.colors))
end

"""
    add_columns_annotation!(graph::HeatmapGraph, [annotation::AnnotationData = AnnotationData()])::Int

Append an `annotation` of the columns and return its index.
"""
function add_columns_annotation!(graph::HeatmapGraph, annotation::AnnotationData = AnnotationData())::Int
    push!(graph.data.columns.annotations, annotation)
    return length(graph.data.columns.annotations)
end

"""
    rows_groups_vector_data_fields(graph::HeatmapGraph)::VectorDataFields

The groups of the rows.
"""
function rows_groups_vector_data_fields(graph::HeatmapGraph)::VectorDataFields
    return VectorDataFields(graph.data.rows.arrangement.groups, graph.data.rows.entities)
end

"""
    rows_subgroups_vector_data_fields(graph::HeatmapGraph)::VectorDataFields

The subgroups of the rows.
"""
function rows_subgroups_vector_data_fields(graph::HeatmapGraph)::VectorDataFields
    return VectorDataFields(graph.data.rows.arrangement.subgroups, graph.data.rows.entities)
end

"""
    columns_groups_vector_data_fields(graph::HeatmapGraph)::VectorDataFields

The groups of the columns.
"""
function columns_groups_vector_data_fields(graph::HeatmapGraph)::VectorDataFields
    return VectorDataFields(graph.data.columns.arrangement.groups, graph.data.columns.entities)
end

"""
    columns_subgroups_vector_data_fields(graph::HeatmapGraph)::VectorDataFields

The subgroups of the columns.
"""
function columns_subgroups_vector_data_fields(graph::HeatmapGraph)::VectorDataFields
    return VectorDataFields(graph.data.columns.arrangement.subgroups, graph.data.columns.entities)
end
