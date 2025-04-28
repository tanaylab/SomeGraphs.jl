# Heatmaps

```@meta
DocTestSetup = quote
  using SomeGraphs
  using PlotlyDocumenter
end
```

```@docs
SomeGraphs.Heatmaps
SomeGraphs.Heatmaps.HeatmapGraph
SomeGraphs.Heatmaps.heatmap_graph
SomeGraphs.Heatmaps.HeatmapGraphData
SomeGraphs.Heatmaps.HeatmapGraphConfiguration
SomeGraphs.Heatmaps.Reorder
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = heatmap_graph(; entries_values = [
    0 1 2 3;
    7 6 5 4;
    8 9 10 11;
])
using PlotlyDocumenter
to_documenter(graph.figure)
```

Annotations:

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries_values = [
        0 1 2 3;
        7 6 5 4;
        8 9 10 11;
    ],
    rows_annotations = [
        AnnotationData(;
            title = "is",
            values = ["yes", "maybe", "no"],
            colors = ColorsConfiguration(;
                palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                show_legend = true,
            ),
        ),
    ],
    columns_annotations = [AnnotationData(; title = "score", values = [1, 0.5, 0, 1])],
    entries_colors_title = "values",
    rows_names = ["X", "Y", "Z"],
    columns_names = ["A", "B", "C", "D"],
)
graph.configuration.entries_colors.show_legend = true
using PlotlyDocumenter
to_documenter(graph.figure)
```

## Index

```@index
Pages = ["heatmaps.md"]
```
