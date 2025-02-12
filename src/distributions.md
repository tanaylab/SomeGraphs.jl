# Distributions

```@meta
DocTestSetup = quote
  using SomeGraphs
  using PlotlyDocumenter
end
```

```@docs
SomeGraphs.Distributions
SomeGraphs.Distributions.DistributionGraph
SomeGraphs.Distributions.distribution_graph
SomeGraphs.Distributions.DistributionGraphData
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
using PlotlyDocumenter
to_documenter(graph.figure)
```

Titles:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.data.figure_title = "Figure title"
graph.data.value_axis_title = "Values axis"
graph.data.distribution_name = "Distribution name"
using PlotlyDocumenter
to_documenter(graph.figure)
```

Color (if it is more of a data than a configuration parameter):

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.data.distribution_color = "red"
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Distributions.DistributionGraphConfiguration
SomeGraphs.Distributions.DistributionConfiguration
```

**Examples:**

Change orientation:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.distribution.values_orientation = VerticalValues
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Distributions.DistributionStyle
```

**Examples:**

Violin:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.distribution.style = ViolinDistribution
using PlotlyDocumenter
to_documenter(graph.figure)
```

Box:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.distribution.style = BoxDistribution
using PlotlyDocumenter
to_documenter(graph.figure)
```

Box with outliers:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.distribution.style = BoxDistribution
graph.configuration.distribution.show_outliers = true
using PlotlyDocumenter
to_documenter(graph.figure)
```

Curve and Box:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.distribution.style = CurveBoxDistribution
using PlotlyDocumenter
to_documenter(graph.figure)
```

Violin and Box:

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.distribution.style = ViolinBoxDistribution
using PlotlyDocumenter
to_documenter(graph.figure)
```

Color (if it is more of a configuration parameter than data):

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.distribution.color = "red"
using PlotlyDocumenter
to_documenter(graph.figure)
```

Bands (if the offset is more of a configuration parameter than data):

```@example
using SomeGraphs
graph = distribution_graph(; distribution_values = [0, 0, 1, 1, 1, 3])
graph.configuration.value_bands.middle.offset = 2
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Distributions.DistributionsGraph
SomeGraphs.Distributions.distributions_graph
SomeGraphs.Distributions.DistributionsGraphData
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = distributions_graph(; distributions_values = [[0, 0, 1, 1, 1, 3], [4, 4, 3, 3, 3, 1]])
using PlotlyDocumenter
to_documenter(graph.figure)
```

```@docs
SomeGraphs.Distributions.DistributionsGraphConfiguration
```

Titles:

```@example
using SomeGraphs
graph = distributions_graph(;
    distributions_values = [[0, 0, 1, 1, 1, 3], [4, 4, 3, 3, 3, 1]],
    distributions_names = ["Foo", "Bar"],
    figure_title = "Figure title",
    value_axis_title = "Values title",
)
using PlotlyDocumenter
to_documenter(graph.figure)
```

Size of gap between distributions:

```@example
using SomeGraphs
graph = distributions_graph(; distributions_values = [[0, 0, 1, 1, 1, 3], [4, 4, 3, 3, 3, 1]])
graph.configuration.distributions_gap = 1.0
using PlotlyDocumenter
to_documenter(graph.figure)
```

Overlay the distributions:

```@example
using SomeGraphs
graph = distributions_graph(; distributions_values = [[0, 0, 1, 1, 1, 3], [4, 4, 3, 3, 3, 1]])
graph.configuration.distributions_gap = nothing
using PlotlyDocumenter
to_documenter(graph.figure)
```

Overlay the distributions with a legend:

```@example
using SomeGraphs
graph = distributions_graph(;
    distributions_values = [[0, 0, 1, 1, 1, 3], [4, 4, 3, 3, 3, 1]],
    distributions_names = ["Foo", "Bar"],
)
graph.configuration.distributions_gap = nothing
using PlotlyDocumenter
to_documenter(graph.figure)
```

Colors (if they are part of the data):

```@example
using SomeGraphs
graph = distributions_graph(;
    distributions_values = [[0, 0, 1, 1, 1, 3], [4, 4, 3, 3, 3, 1]],
    distributions_colors = ["red", "green"],
)
using PlotlyDocumenter
to_documenter(graph.figure)
```

You can also apply any of the distribution and/or value axis configuration options; these will apply to all the distributions:

```@example
using SomeGraphs
graph = distributions_graph(; distributions_values = [[0, 0, 1, 1, 1, 3], [4, 4, 3, 3, 3, 1]])
graph.configuration.distribution.values_orientation = VerticalValues
graph.configuration.distribution.color = "red"
graph.configuration.distribution.style = BoxDistribution
graph.configuration.distribution.show_outliers = true
using PlotlyDocumenter
to_documenter(graph.figure)
```

## Index

```@index
Pages = ["distributions.md"]
```
