# Heatmaps

```@meta
DocTestSetup = quote
  using SomeGraphs
  using PlotlyBase
end
```

```@docs
SomeGraphs.Heatmaps
SomeGraphs.Heatmaps.HeatmapGraph
SomeGraphs.Heatmaps.heatmap_graph
SomeGraphs.Heatmaps.HeatmapGraphData
SomeGraphs.Heatmaps.HeatmapGraphConfiguration
SomeGraphs.Heatmaps.HeatmapReorder
SomeGraphs.Heatmaps.HeatmapGraphOrder
SomeGraphs.Heatmaps.heatmap_order
SomeGraphs.Heatmaps.reset_order!
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

Flip axes (non-mutating):

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
flipped = flip_axes(graph)
using PlotlyDocumenter
to_documenter(flipped.figure)
```

Flip axes (in-place):

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
flip_axes!(graph)
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
    rows_groups = [1, 1, 2, 2],
    columns_groups = ["L", "M", "M"],
)
graph.configuration.rows_reorder = OptimalHclust
graph.configuration.columns_reorder = OptimalHclust
graph.configuration.rows_dendogram_size = 0.2
graph.configuration.columns_dendogram_size = 0.2
using PlotlyDocumenter
to_documenter(graph.figure)
```

Subgroups (a 2nd level of grouping nested in the groups). The groups are numbered, so they are shown in the order of
their numbers; the subgroups are named, so they are placed by the clustering, but each of them is still contiguous
inside its group:

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries_values = [
        4 1 5 2 4 1;
        3 2 4 3 3 2;
        2 3 3 4 2 3;
        1 4 2 5 1 4;
    ],
    rows_names = ["A", "B", "C", "D"],
    columns_names = ["U", "V", "W", "X", "Y", "Z"],
    columns_groups = [1, 1, 1, 2, 2, 2],
    columns_subgroups = ["P", "Q", "P", "R", "R", "S"],
)
graph.configuration.columns_reorder = OptimalHclust
graph.configuration.columns_subgroups_gap = 1
using PlotlyDocumenter
to_documenter(graph.figure)
```

## Index

```@index
Pages = ["heatmaps.md"]
```
