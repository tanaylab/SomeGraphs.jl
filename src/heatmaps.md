# Heatmaps

```@meta
DocTestSetup = quote
  using SomeGraphs
  using PlotlyJS
end
```

```@docs
SomeGraphs.Heatmaps
SomeGraphs.Heatmaps.HeatmapGraph
SomeGraphs.Heatmaps.heatmap_graph
SomeGraphs.Heatmaps.HeatmapGraphData
SomeGraphs.Heatmaps.HeatmapGraphConfiguration
SomeGraphs.Heatmaps.HeatmapReorder
SomeGraphs.Heatmaps.HeatmapLinkage
SomeGraphs.Heatmaps.HeatmapOrigin
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries_values = [
        4 1 5;
        3 2 4;
        2 3 3;
        1 4 2;
    ],
    rows_names = ["A", "B", "C", "D"],
    columns_names = ["X", "Y", "Z"],
)
using PlotlyDocumenter
to_documenter(graph.figure)
```

Annotations:

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries_values = [
        4 1 5;
        3 2 4;
        2 3 3;
        1 4 2;
    ],
    rows_names = ["A", "B", "C", "D"],
    columns_names = ["X", "Y", "Z"],
    rows_annotations = [AnnotationData(; title = "score", values = [1, 0.5, 0, 1])],
    columns_annotations = [
        AnnotationData(;
            title = "is_special",
            values = ["yes", "maybe", "no"],
            colors = ColorsConfiguration(;
                palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
            ),
        ),
    ],
)
using PlotlyDocumenter
to_documenter(graph.figure)
```

Dendograms:

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries_values = [
        4 1 5;
        3 2 4;
        2 3 3;
        1 4 2;
    ],
    rows_names = ["A", "B", "C", "D"],
    columns_names = ["X", "Y", "Z"],
    rows_annotations = [AnnotationData(; title = "score", values = [1, 0.5, 0, 1])],
    columns_annotations = [
        AnnotationData(;
            title = "is_special",
            values = ["yes", "maybe", "no"],
            colors = ColorsConfiguration(;
                palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
            ),
        ),
    ],
)
graph.configuration.rows_reorder = OptimalHclust
graph.configuration.columns_reorder = OptimalHclust
graph.configuration.rows_dendogram_size = 0.2
graph.configuration.columns_dendogram_size = 0.2
using PlotlyDocumenter
to_documenter(graph.figure)
```

Gaps:

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries_values = [
        4 1 5;
        3 2 4;
        2 3 3;
        1 4 2;
    ],
    rows_names = ["A", "B", "C", "D"],
    columns_names = ["X", "Y", "Z"],
    rows_annotations = [AnnotationData(; title = "score", values = [1, 0.5, 0, 1])],
    columns_annotations = [
        AnnotationData(;
            title = "is_special",
            values = ["yes", "maybe", "no"],
            colors = ColorsConfiguration(;
                palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
            ),
        ),
    ],
    rows_gaps = [2],
    columns_gaps = [1, 1],
)
graph.configuration.rows_reorder = OptimalHclust
graph.configuration.columns_reorder = OptimalHclust
graph.configuration.rows_dendogram_size = 0.2
graph.configuration.columns_dendogram_size = 0.2
using PlotlyDocumenter
to_documenter(graph.figure)
```

## Index

```@index
Pages = ["heatmaps.md"]
```
