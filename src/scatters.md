# Scatter Plots

```@meta
DocTestSetup = quote
  using SomeGraphs
  using PlotlyDocumenter
end
```

```@docs
SomeGraphs.Scatters
SomeGraphs.Scatters.PointsGraph
SomeGraphs.Scatters.points_graph
SomeGraphs.Scatters.PointsGraphData
SomeGraphs.Scatters.PointsGraphConfiguration
SomeGraphs.Scatters.ScattersConfiguration
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = points_graph(; points_xs = collect(0:10) .* 10, points_ys = collect(0:10) .^ 2)
using PlotlyDocumenter
to_documenter(graph.figure)
```

Borders:

```@example
using SomeGraphs
graph = points_graph(; points_xs = collect(0:10) .* 10, points_ys = collect(0:10) .^ 2)
graph.configuration.borders.colors.fixed = "black"
graph.configuration.borders.sizes.fixed = 1
using PlotlyDocumenter
to_documenter(graph.figure)
```

Edges:

```@example
using SomeGraphs
graph = points_graph(;
    points_xs = collect(0:10) .* 10,
    points_ys = collect(0:10) .^ 2,
    edges_points = [(1, 8), (2, 9), (3, 10), (4, 11)],
)
using PlotlyDocumenter
to_documenter(graph.figure)
```

Diagonal bands (linear scales):

```@example
using SomeGraphs
graph = points_graph(; points_xs = collect(0:10) .* 10, points_ys = collect(0:10) .^ 2)
graph.configuration.diagonal_bands.low.offset = -25
graph.configuration.diagonal_bands.middle.offset = 0
graph.configuration.diagonal_bands.high.offset = +25
using PlotlyDocumenter
to_documenter(graph.figure)
```

Diagonal bands (log scales):

```@example
using SomeGraphs
graph = points_graph(; points_xs = collect(0:10) .* 10, points_ys = collect(0:10) .^ 2)
graph.configuration.x_axis.log_scale = Log10Scale
graph.configuration.y_axis.log_scale = Log10Scale
graph.configuration.x_axis.log_regularization = 1
graph.configuration.y_axis.log_regularization = 1
graph.configuration.diagonal_bands.low.offset = 1 / 4
graph.configuration.diagonal_bands.middle.offset = 1
graph.configuration.diagonal_bands.high.offset = 4
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Scatters.LineGraph
SomeGraphs.Scatters.line_graph
SomeGraphs.Scatters.LineGraphData
SomeGraphs.Scatters.LineGraphConfiguration
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = line_graph(; points_xs = collect(0:10) .* 10, points_ys = collect(0:10) .^ 2)
using PlotlyDocumenter
to_documenter(graph.figure)
```

With points:

```@example
using SomeGraphs
graph = line_graph(; points_xs = collect(0:10) .* 10, points_ys = collect(0:10) .^ 2)
graph.configuration.show_points = true
using PlotlyDocumenter
to_documenter(graph.figure)
```

Filled:

```@example
using SomeGraphs
graph = line_graph(; points_xs = collect(0:10) .* 10, points_ys = collect(0:10) .^ 2)
graph.configuration.line.is_filled = true
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Scatters.LinesGraph
SomeGraphs.Scatters.lines_graph
SomeGraphs.Scatters.LinesGraphData
SomeGraphs.Scatters.LinesGraphConfiguration
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = lines_graph(; lines_points_xs = [collect(0:10) .* 10, [0, 90]], lines_points_ys = [collect(0:10) .^ 2, [50, 0]])
using PlotlyDocumenter
to_documenter(graph.figure)
```

Filled:

```@example
using SomeGraphs
graph = lines_graph(; lines_points_xs = [collect(0:10) .* 10, [0, 90]], lines_points_ys = [collect(0:10) .^ 2, [50, 0]])
graph.configuration.line.is_filled = true
using PlotlyDocumenter
to_documenter(graph.figure)
```

Stacked:

```@example
using SomeGraphs
graph = lines_graph(; lines_points_xs = [collect(0:10) .* 10, [0, 90]], lines_points_ys = [collect(0:10) .^ 2, [50, 0]])
graph.configuration.line.is_filled = true
graph.configuration.stacking = StackValues
using PlotlyDocumenter
to_documenter(graph.figure)
```

Fractions:

```@example
using SomeGraphs
graph = lines_graph(; lines_points_xs = [collect(0:10) .* 10, [0, 90]], lines_points_ys = [collect(0:10) .^ 2, [50, 0]])
graph.configuration.line.is_filled = true
graph.configuration.stacking = StackFractions
using PlotlyDocumenter
to_documenter(graph.figure)
```

Percents:

```@example
using SomeGraphs
graph = lines_graph(; lines_points_xs = [collect(0:10) .* 10, [0, 90]], lines_points_ys = [collect(0:10) .^ 2, [50, 0]])
graph.configuration.line.is_filled = true
graph.configuration.stacking = StackFractions
graph.configuration.y_axis.percent = true
using PlotlyDocumenter
to_documenter(graph.figure)
```

## Index

```@index
Pages = ["scatters.md"]
```
