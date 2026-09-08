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
SomeGraphs.Heatmaps.HeatmapAxisData
SomeGraphs.Heatmaps.HeatmapGraphConfiguration
SomeGraphs.Heatmaps.HeatmapAxisConfiguration
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
    entries = MatrixData([
        4 1 5;
        3 2 4;
        2 3 3;
        1 4 2;
    ]),
    rows = HeatmapAxisData(; names = ValuesData(["A", "B", "C", "D"])),
    columns = HeatmapAxisData(; names = ValuesData(["X", "Y", "Z"])),
)
using PlotlyDocumenter
to_documenter(graph.figure)
```

Flip axes (non-mutating):

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries = MatrixData([
        4 1 5;
        3 2 4;
        2 3 3;
        1 4 2;
    ]),
    rows = HeatmapAxisData(; names = ValuesData(["A", "B", "C", "D"])),
    columns = HeatmapAxisData(; names = ValuesData(["X", "Y", "Z"])),
)
flipped = flip_axes(graph)
using PlotlyDocumenter
to_documenter(flipped.figure)
```

Flip axes (in-place):

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries = MatrixData([
        4 1 5;
        3 2 4;
        2 3 3;
        1 4 2;
    ]),
    rows = HeatmapAxisData(; names = ValuesData(["A", "B", "C", "D"])),
    columns = HeatmapAxisData(; names = ValuesData(["X", "Y", "Z"])),
)
flip_axes!(graph)
using PlotlyDocumenter
to_documenter(graph.figure)
```

Annotations:

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries = MatrixData([
        4 1 5;
        3 2 4;
        2 3 3;
        1 4 2;
    ]),
    rows = HeatmapAxisData(;
        names = ValuesData(["A", "B", "C", "D"]),
        annotations = [AnnotationData(; values = ValuesData([1, 0.5, 0, 1], "score"))],
    ),
    columns = HeatmapAxisData(;
        names = ValuesData(["X", "Y", "Z"]),
        annotations = [
            AnnotationData(;
                values = ValuesData(["yes", "maybe", "no"], "is_special"),
                colors = ColorsConfiguration(;
                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                ),
            ),
        ],
    ),
)
using PlotlyDocumenter
to_documenter(graph.figure)
```

Dendograms:

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries = MatrixData([
        4 1 5;
        3 2 4;
        2 3 3;
        1 4 2;
    ]),
    rows = HeatmapAxisData(;
        names = ValuesData(["A", "B", "C", "D"]),
        annotations = [AnnotationData(; values = ValuesData([1, 0.5, 0, 1], "score"))],
    ),
    columns = HeatmapAxisData(;
        names = ValuesData(["X", "Y", "Z"]),
        annotations = [
            AnnotationData(;
                values = ValuesData(["yes", "maybe", "no"], "is_special"),
                colors = ColorsConfiguration(;
                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                ),
            ),
        ],
    ),
)
graph.configuration.rows.reorder = OptimalHclust
graph.configuration.columns.reorder = OptimalHclust
graph.configuration.rows.dendogram_size = 0.2
graph.configuration.columns.dendogram_size = 0.2
using PlotlyDocumenter
to_documenter(graph.figure)
```

Gaps:

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries = MatrixData([
        4 1 5;
        3 2 4;
        2 3 3;
        1 4 2;
    ]),
    rows = HeatmapAxisData(;
        names = ValuesData(["A", "B", "C", "D"]),
        annotations = [AnnotationData(; values = ValuesData([1, 0.5, 0, 1], "score"))],
        groups = [1, 1, 2, 2],
    ),
    columns = HeatmapAxisData(;
        names = ValuesData(["X", "Y", "Z"]),
        annotations = [
            AnnotationData(;
                values = ValuesData(["yes", "maybe", "no"], "is_special"),
                colors = ColorsConfiguration(;
                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                ),
            ),
        ],
        groups = ["L", "M", "M"],
    ),
)
graph.configuration.rows.reorder = OptimalHclust
graph.configuration.columns.reorder = OptimalHclust
graph.configuration.rows.dendogram_size = 0.2
graph.configuration.columns.dendogram_size = 0.2
using PlotlyDocumenter
to_documenter(graph.figure)
```

Subgroups (a 2nd level of grouping nested in the groups). The groups are numbered, so they are shown in the order of
their numbers; the subgroups are named, so they are placed by the clustering, but each of them is still contiguous
inside its group:

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries = MatrixData([
        4 1 5 2 4 1;
        3 2 4 3 3 2;
        2 3 3 4 2 3;
        1 4 2 5 1 4;
    ]),
    rows = HeatmapAxisData(; names = ValuesData(["A", "B", "C", "D"])),
    columns = HeatmapAxisData(;
        names = ValuesData(["U", "V", "W", "X", "Y", "Z"]),
        groups = [1, 1, 1, 2, 2, 2],
        subgroups = ["P", "Q", "P", "R", "R", "S"],
    ),
)
graph.configuration.columns.reorder = OptimalHclust
graph.configuration.columns.subgroups_gap = 1
using PlotlyDocumenter
to_documenter(graph.figure)
```

Hide some rows and cells. The hidden ones are still part of the data, so they still count in the colors scale (unless
`include_hidden` is disabled in its axis):

```@example
using SomeGraphs
graph = heatmap_graph(;
    entries = MatrixData([
        4 1 5;
        3 2 4;
        2 3 3;
        1 4 2;
    ]),
    cells = MatrixEntitiesData(; mask = [
        true true false;
        true true true;
        true true true;
        false true true;
    ]),
    rows = HeatmapAxisData(;
        names = ValuesData(["A", "B", "C", "D"]),
        entities = EntitiesData(; mask = [true, false, true, true]),
    ),
    columns = HeatmapAxisData(; names = ValuesData(["X", "Y", "Z"])),
)
using PlotlyDocumenter
to_documenter(graph.figure)
```

## Index

```@index
Pages = ["heatmaps.md"]
```
