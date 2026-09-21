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
SomeGraphs.Sources.series_axis_vector_fields
SomeGraphs.Sources.distribution_axis_vector_fields
SomeGraphs.Sources.distributions_axis_vector_fields
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

## Hovers

```@docs
SomeGraphs.Sources.add_hovers!
```

**Example:**

One source function, filling a role from a vector of values with a title and a hover line, applied to the X and Y
coordinates and to the colors of the points of a graph:

```@example
using SomeGraphs
function source!(fields::VectorFields, values::AbstractVector{<:Real}, title::AbstractString)::Nothing
    fields.data.values.vector = values
    fields.data.values.title = title
    add_hovers!(fields.data.entities, string.(values); title)
    return nothing
end
graph = points_graph()
source!(x_axis_vector_fields(graph), collect(0:10) .* 10, "X")
source!(y_axis_vector_fields(graph), collect(0:10) .^ 2, "Y")
source!(points_colors_vector_fields(graph), collect(0:10), "Color")
using PlotlyDocumenter
to_documenter(graph.figure)
```

## Index

```@index
Pages = ["sources.md"]
```
