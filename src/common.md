# Common

```@meta
DocTestSetup = quote
  using SomeGraphs
  using PlotlyJS
end
```

```@docs
SomeGraphs.Common
SomeGraphs.Common.Graph
SomeGraphs.Common.AbstractGraphConfiguration
SomeGraphs.Common.AbstractGraphData
SomeGraphs.Common.PlotlyFigure
SomeGraphs.Common.save_graph
SomeGraphs.Common.FigureConfiguration
SomeGraphs.Common.Stacking
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
using PlotlyDocumenter
to_documenter(graph.figure)
```

Reduced size:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.figure.width = 300
graph.configuration.figure.height = 300
using PlotlyDocumenter
to_documenter(graph.figure)
```

Change colors:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.figure.background_color = "lightyellow"
graph.configuration.figure.paper_color = "lightgrey"
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Common.MarginsConfiguration
```

**Examples:**

Increased margins:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.figure.margins.left = 100
graph.configuration.figure.margins.right = 200
graph.configuration.figure.margins.top = 150
graph.configuration.figure.margins.bottom = 250
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Common.AxisConfiguration
```

**Examples:**

Disable ticks:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.value_axis.show_ticks = false
using PlotlyDocumenter
to_documenter(graph.figure)
```

Disable grid:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.value_axis.show_grid = false
using PlotlyDocumenter
to_documenter(graph.figure)
```

Override grid color:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.value_axis.grid_color = "red"
using PlotlyDocumenter
to_documenter(graph.figure)
```

Override range:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.value_axis.minimum = 1
graph.configuration.value_axis.maximum = 4
using PlotlyDocumenter
to_documenter(graph.figure)
```

Percent:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.value_axis.percent = 1
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Common.LogScale
```

**Examples:**

Log 2:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.value_axis.log_scale = Log2Scale
graph.configuration.value_axis.log_regularization = 1
using PlotlyDocumenter
to_documenter(graph.figure)
```

Log 10:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.value_axis.log_scale = Log10Scale
graph.configuration.value_axis.log_regularization = 1e-5
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Common.SizesConfiguration
SomeGraphs.Common.LineConfiguration
SomeGraphs.Common.LineStyle
SomeGraphs.Common.ValuesOrientation
SomeGraphs.Common.BandsConfiguration
SomeGraphs.Common.BandConfiguration
SomeGraphs.Common.BandsData
SomeGraphs.Common.ColorsConfiguration
SomeGraphs.Common.ContinuousColors
SomeGraphs.Common.CategoricalColors
SomeGraphs.Common.categorical_palette
SomeGraphs.Common.NAMED_COLOR_SCALES
SomeGraphs.Common.COLOR_SCALES_LOCK
```

```@docs
SomeGraphs.Common.AnnotationData
SomeGraphs.Common.AnnotationSize
```

## Index

```@index
Pages = ["common.md"]
```
