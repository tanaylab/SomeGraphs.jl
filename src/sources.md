# Sources

```@meta
DocTestSetup = quote
  using SomeGraphs
end
```

```@docs
SomeGraphs.Sources
SomeGraphs.Sources.AbstractFields
SomeGraphs.Sources.AbstractConfigurationFields
```

## Vectors

## Types

```@docs
SomeGraphs.Sources.VectorFields
SomeGraphs.Sources.VectorDataFields
SomeGraphs.Sources.AxisConfigurationFields
SomeGraphs.Sources.ColorsConfigurationFields
SomeGraphs.Sources.SizesConfigurationFields
```

### Accessors

#### Vector Data Fields

```@docs
SomeGraphs.Sources.rows_groups_vector_data_fields
SomeGraphs.Sources.rows_subgroups_vector_data_fields
SomeGraphs.Sources.columns_groups_vector_data_fields
SomeGraphs.Sources.columns_subgroups_vector_data_fields
```

#### Axis Vector Fields

```@docs
SomeGraphs.Sources.x_axis_vector_fields
SomeGraphs.Sources.y_axis_vector_fields
SomeGraphs.Sources.values_axis_vector_fields
SomeGraphs.Sources.distribution_axis_vector_fields
```

#### Colors Vector Fields

```@docs
SomeGraphs.Sources.points_colors_vector_fields
SomeGraphs.Sources.borders_colors_vector_fields
SomeGraphs.Sources.edges_colors_vector_fields
SomeGraphs.Sources.colors_vector_fields
SomeGraphs.Sources.annotations_colors_vector_fields
SomeGraphs.Sources.rows_annotations_colors_vector_fields
SomeGraphs.Sources.columns_annotations_colors_vector_fields
```

### Sizes Vector Fields

```@docs
SomeGraphs.Sources.points_sizes_vector_fields
SomeGraphs.Sources.borders_sizes_vector_fields
SomeGraphs.Sources.edges_sizes_vector_fields
```

#### Entities

The entities are shared by all the roles of the same thing, so writing them through any role is seen by all. Reach them
directly to add a hover line, or to hide entities with a mask, without filling any role.

```@docs
SomeGraphs.Sources.points_entities
SomeGraphs.Sources.edges_entities
SomeGraphs.Sources.bars_entities
SomeGraphs.Sources.distribution_entities
SomeGraphs.Sources.rows_entities
SomeGraphs.Sources.columns_entities
```

## Matrices

### Types

```@docs
SomeGraphs.Sources.MatrixFields
SomeGraphs.Sources.MatrixDataFields
SomeGraphs.Sources.MatrixConfigurationFields
```

### Accessors

```@docs
SomeGraphs.Sources.entries_matrix_fields
```

## Parts

### Types

```@docs
SomeGraphs.Sources.PartFields
```

### Creators

```@docs
SomeGraphs.Sources.add_series!
SomeGraphs.Sources.add_line!
SomeGraphs.Sources.add_distribution!
SomeGraphs.Sources.add_annotation!
SomeGraphs.Sources.add_rows_annotation!
SomeGraphs.Sources.add_columns_annotation!
```

### Accessors

```@docs
SomeGraphs.Sources.series_part_fields
SomeGraphs.Sources.distribution_part_fields
SomeGraphs.Sources.line_part_fields
```

## Sinks

A data source doesn't need to know which view it is filling, or how many. It takes `Sinks` and walks them with
`visit_data_sinks` or `visit_configuration_sinks`, writing a method per leaf it fills and an explicit no-op for the
leaves it ignores.

```@docs
SomeGraphs.Sources.VectorDataLeaf
SomeGraphs.Sources.MatrixDataLeaf
SomeGraphs.Sources.DataLeaf
SomeGraphs.Sources.ConfigurationLeaf
SomeGraphs.Sources.AnyLeaf
SomeGraphs.Sources.DataContainer
SomeGraphs.Sources.ConfigurationContainer
SomeGraphs.Sources.AnyContainer
SomeGraphs.Sources.DataSink
SomeGraphs.Sources.ConfigurationSink
SomeGraphs.Sources.AnySink
SomeGraphs.Sources.Sinks
SomeGraphs.Sources.VectorDataSinks
SomeGraphs.Sources.MatrixDataSinks
SomeGraphs.Sources.visit_data_sinks
SomeGraphs.Sources.visit_configuration_sinks
```

## Hovers

```@docs
SomeGraphs.Sources.add_hovers!
```

**Example:**

One source function, writing a vector of values as the values of a role and as a hover line. It says what to do with a
`VectorValuesData` and with a `VectorEntitiesData`, and `visit_data_sinks` finds them in whatever it is given. Given a
matrix struct, it fails rather than silently doing nothing, since it says nothing about those:

```@example
using SomeGraphs

function source!(
    sinks::Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector},
    values::AbstractVector{<:Real},
    title::AbstractString,
)::Nothing
    visit_data_sinks(sinks) do sink
        return source!(sink, values, title)
    end
    return nothing
end

function source!(values_data::VectorValuesData, values::AbstractVector{<:Real}, title::AbstractString)::Nothing
    values_data.vector = values
    values_data.title = title
    return nothing
end

function source!(entities::VectorEntitiesData, values::AbstractVector{<:Real}, title::AbstractString)::Nothing
    add_hovers!(entities, string.(values); title)
    return nothing
end

graph = points_graph()
source!(x_axis_vector_fields(graph), collect(0:10) .* 10, "X")
source!(y_axis_vector_fields(graph), collect(0:10) .^ 2, "Y")
source!(points_colors_vector_fields(graph), collect(0:10), "Color")
using PlotlyDocumenter
to_documenter(graph.figure)
```

All three roles share the points' entities, so each call adds one hover line to the same entities, and the points end
up with three. Had they been passed together in one call, `visit_data_sinks` would have visited those entities once,
and the hover would have been added once.

## Index

```@index
Pages = ["sources.md"]
```
