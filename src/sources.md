# Sources

```@meta
DocTestSetup = quote
  using SomeGraphs
end
```

```@docs
SomeGraphs.Sources
```

A data source view bundles the parts of a graph that one source of data fills: the values of one role, the entities they
belong to, and the configuration they are shown by. A function written against a view works on any graph and any role
that offers the same kind of view.

```@docs
SomeGraphs.Sources.VectorFields
SomeGraphs.Sources.VectorDataFields
SomeGraphs.Sources.AxisConfigurationFields
SomeGraphs.Sources.ColorsConfigurationFields
SomeGraphs.Sources.SizesConfigurationFields
SomeGraphs.Sources.MatrixFields
SomeGraphs.Sources.MatrixDataFields
SomeGraphs.Sources.MatrixConfigurationFields
SomeGraphs.Sources.add_hovers!
```

The views are obtained from a graph by accessor functions, named by the path of the values in the data of the graph:

```@docs
SomeGraphs.Sources.x_fields
SomeGraphs.Sources.y_fields
SomeGraphs.Sources.points_colors_fields
SomeGraphs.Sources.points_sizes_fields
SomeGraphs.Sources.borders_colors_fields
SomeGraphs.Sources.borders_sizes_fields
SomeGraphs.Sources.edges_colors_fields
SomeGraphs.Sources.edges_sizes_fields
SomeGraphs.Sources.values_fields
SomeGraphs.Sources.colors_fields
SomeGraphs.Sources.series_values_fields
SomeGraphs.Sources.annotations_fields
SomeGraphs.Sources.distribution_values_fields
SomeGraphs.Sources.distributions_values_fields
SomeGraphs.Sources.entries_fields
SomeGraphs.Sources.rows_annotations_fields
SomeGraphs.Sources.columns_annotations_fields
SomeGraphs.Sources.names_fields
SomeGraphs.Sources.rows_names_fields
SomeGraphs.Sources.columns_names_fields
SomeGraphs.Sources.rows_groups_fields
SomeGraphs.Sources.rows_subgroups_fields
SomeGraphs.Sources.columns_groups_fields
SomeGraphs.Sources.columns_subgroups_fields
```

A view of one entry of a vector of structures (a series, a line, a distribution, an annotation) needs the entry to
exist. These append one and return the index the accessors take:

```@docs
SomeGraphs.Sources.add_series!
SomeGraphs.Sources.add_line!
SomeGraphs.Sources.add_distribution!
SomeGraphs.Sources.add_annotation!
SomeGraphs.Sources.add_rows_annotation!
SomeGraphs.Sources.add_columns_annotation!
```

**Example:**

One source function, filling a role from a vector of values with a title and a hover line, applied to the X and Y
coordinates and to the colors of the points of a graph:

```@example
using SomeGraphs
function source!(fields::VectorFields, values::AbstractVector{<:Real}, title::AbstractString)::Nothing
    fields.data.values.values = values
    fields.data.values.title = title
    add_hovers!(fields.data.entities, string.(values); title)
    return nothing
end
graph = points_graph()
source!(x_fields(graph), collect(0:10) .* 10, "X")
source!(y_fields(graph), collect(0:10) .^ 2, "Y")
source!(points_colors_fields(graph), collect(0:10), "Color")
using PlotlyDocumenter
to_documenter(graph.figure)
```

## Index

```@index
Pages = ["sources.md"]
```
