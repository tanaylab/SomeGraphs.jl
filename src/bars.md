# Bar Plots

```@meta
DocTestSetup = quote
  using SomeGraphs
  using PlotlyBase
end
```

```@docs
SomeGraphs.Bars
SomeGraphs.Bars.BarsGraph
SomeGraphs.Bars.bars_graph
SomeGraphs.Bars.BarsGraphData
SomeGraphs.Bars.BarsGraphConfiguration
SomeGraphs.Bars.BarsConfiguration
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = bars_graph(; values = ValuesData(collect(0:10) .* 10))
using PlotlyDocumenter
to_documenter(graph.figure)
```

Annotations:

```@example
using SomeGraphs
graph = bars_graph(; values = ValuesData(collect(0:10) .* 10))
graph.data.annotations = [AnnotationData(; values = ValuesData(collect(0:10) .% 3, "score"))]
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Bars.SeriesBarsGraph
SomeGraphs.Bars.series_bars_graph
SomeGraphs.Bars.SeriesBarsGraphData
SomeGraphs.Bars.SeriesData
SomeGraphs.Bars.SeriesBarsGraphConfiguration
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = series_bars_graph(;
    series = [
        SeriesData(; values = ValuesData(collect(0:10) .* 5)),
        SeriesData(; values = ValuesData(collect(0:10) .^ 2)),
    ],
)
using PlotlyDocumenter
to_documenter(graph.figure)
```

Annotations:

```@example
using SomeGraphs
graph = series_bars_graph(;
    series = [
        SeriesData(; values = ValuesData(collect(0:10) .* 5)),
        SeriesData(; values = ValuesData(collect(0:10) .^ 2)),
    ],
)
graph.data.annotations = [AnnotationData(; values = ValuesData(collect(0:10) .% 3, "score"))]
using PlotlyDocumenter
to_documenter(graph.figure)
```

## Index

```@index
Pages = ["bars.md"]
```
