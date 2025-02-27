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

## Index

```@index
Pages = ["scatters.md"]
```
