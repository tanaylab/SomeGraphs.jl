# Bar Plots

```@meta
DocTestSetup = quote
  using SomeGraphs
  using PlotlyDocumenter
end
```

```@docs
SomeGraphs.Bars
SomeGraphs.Bars.BarsGraph
SomeGraphs.Bars.bars_graph
SomeGraphs.Bars.BarsGraphData
SomeGraphs.Bars.BarsGraphConfiguration
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = bars_graph(; bars_values = collect(0:10) .* 10)
using PlotlyDocumenter
to_documenter(graph.figure)
```

Annotations:

```@example
using SomeGraphs
graph = bars_graph(; bars_values = collect(0:10) .* 10)
graph.data.bars_annotations = [AnnotationData(; title = "score", values = collect(0:10) .% 3)]
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Bars.SeriesBarsGraph
SomeGraphs.Bars.series_bars_graph
SomeGraphs.Bars.SeriesBarsGraphData
SomeGraphs.Bars.SeriesBarsGraphConfiguration
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = series_bars_graph(; series_bars_values = [collect(0:10) .* 5, collect(0:10) .^ 2])
using PlotlyDocumenter
to_documenter(graph.figure)
```

Annotations:

```@example
using SomeGraphs
graph = series_bars_graph(; series_bars_values = [collect(0:10) .* 5, collect(0:10) .^ 2])
graph.data.bars_annotations = [AnnotationData(; title = "score", values = collect(0:10) .% 3)]
using PlotlyDocumenter
to_documenter(graph.figure)
```

## Index

```@index
Pages = ["bars.md"]
```
