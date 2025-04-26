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
SomeGraphs.Scatters.points_density
```

**Examples:**

```@example
using SomeGraphs
graph = points_graph()
graph.data.points_xs = [
    0.2698393176826803,
    0.21199888259395777,
    -1.1403772919081927,
    0.015375662421357001,
    -1.067372097104871,
    -0.05131680407322392,
    1.1476690271171557,
    0.2619998741581797,
    -0.3294624837610639,
    0.3990906575326256,
    0.016185972979333094,
    -0.6295842065710322,
    1.74273570356108,
    -1.612316716623975,
    -1.2696818434826393,
    -2.3942962323946806,
    -0.0683194741744384,
    -0.6991502371264332,
    1.3005476302710504,
    -0.3156364801379863,
]
graph.data.points_ys = [
    -0.1764741545510277,
    0.5007984744043152,
    -1.0092288051861404,
    0.28862095432807144,
    0.3216029374844889,
    1.1177946117474804,
    0.11865114901055787,
    -2.173777902643006,
    -0.5131646448399668,
    -0.4180196978042471,
    -1.7758801658517032,
    0.5019767811414706,
    0.6519383169746722,
    1.306115558967419,
    -0.6077449865370641,
    0.6968047575410379,
    1.7053341710917538,
    -0.6584463274588279,
    0.9034430051864035,
    -0.631083973233279,
]
graph.data.points_colors = points_density(graph.data.points_xs, graph.data.points_ys)
graph.data.points_order = sortperm(graph.data.points_colors)
graph.configuration.points.colors.palette = "Viridis"
graph.configuration.points.sizes.fixed = 16
graph.configuration.figure.width = 200
graph.configuration.figure.height = 200
graph.configuration.x_axis.minimum = -3
graph.configuration.y_axis.minimum = -3
graph.configuration.x_axis.maximum = 3
graph.configuration.y_axis.maximum = 3
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
