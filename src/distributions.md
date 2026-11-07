# Distributions

```@meta
DocTestSetup = quote
  using SomeGraphs
  using PlotlyBase
end
```

```@docs
SomeGraphs.Distributions
SomeGraphs.Distributions.DistributionGraph
SomeGraphs.Distributions.distribution_graph
SomeGraphs.Distributions.DistributionGraphData
SomeGraphs.Distributions.DistributionData
```

**Examples:**

Default (serves as a baseline to compare with when modifying options):

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
using PlotlyDocumenter
to_documenter(graph.fig)
```

Titles:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.data.figure_title = "Figure title"
graph.data.distribution.values.title = "Values axis"
graph.data.distribution.name = "Distribution name"
using PlotlyDocumenter
to_documenter(graph.fig)
```

Color (if it is more of a data than a configuration parameter):

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.data.distribution.color = "red"
using PlotlyDocumenter
to_documenter(graph.fig)
```

```@docs
SomeGraphs.Distributions.DistributionGraphConfiguration
SomeGraphs.Distributions.DistributionConfiguration
```

**Examples:**

Change orientation:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.values_orientation = VerticalValues
using PlotlyDocumenter
to_documenter(graph.fig)
```

```@docs
SomeGraphs.Distributions.DistributionStyle
```

**Examples:**

Violin:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.style = ViolinDistribution
using PlotlyDocumenter
to_documenter(graph.fig)
```

Box:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.style = BoxDistribution
using PlotlyDocumenter
to_documenter(graph.fig)
```

Box with outliers:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.style = BoxOutliersDistribution
using PlotlyDocumenter
to_documenter(graph.fig)
```

Curve and Box:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.style = CurveBoxDistribution
using PlotlyDocumenter
to_documenter(graph.fig)
```

Violin and Box:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.style = ViolinBoxDistribution
using PlotlyDocumenter
to_documenter(graph.fig)
```

Histogram:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.style = HistogramDistribution
using PlotlyDocumenter
to_documenter(graph.fig)
```

Color (if it is more of a configuration parameter than data):

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.line.color = "red"
using PlotlyDocumenter
to_documenter(graph.fig)
```

Line width and disable fill:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.line.width = 4
graph.configuration.distribution.line.is_filled = false
using PlotlyDocumenter
to_documenter(graph.fig)
```

Bands (if the offset is more of a configuration parameter than data):

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.value_bands.middle.offset = 2
using PlotlyDocumenter
to_documenter(graph.fig)
```

**Examples:**

Cumulative distribution functions are an undervalued tool for showing distributions. They have the advantage that the
second axis is in actual units (by default, counts). This opens up additional configuration options.

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.style = CumulativeDistribution
graph.configuration.distribution.line.is_filled = true
using PlotlyDocumenter
to_documenter(graph.fig)
```

Fractions:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.style = CumulativeDistribution
graph.configuration.distribution.line.is_filled = true
graph.configuration.distribution.normalize = true
using PlotlyDocumenter
to_documenter(graph.fig)
```

Percents:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.style = CumulativeDistribution
graph.configuration.distribution.line.is_filled = true
graph.configuration.distribution.normalize = true
graph.configuration.density_axis.scale.percent = true
using PlotlyDocumenter
to_documenter(graph.fig)
```

Descending:

```@example
using SomeGraphs
graph = distribution_graph(; distribution = DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])))
graph.configuration.distribution.style = CumulativeDistribution
graph.configuration.distribution.line.is_filled = true
graph.configuration.distribution.cumulative_descending = true
using PlotlyDocumenter
to_documenter(graph.fig)
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
graph = distributions_graph(;
    distributions = [
        DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])),
        DistributionData(; values = VectorValuesData([4, 4, 3, 3, 3, 1])),
    ],
)
using PlotlyDocumenter
to_documenter(graph.fig)
```

```@docs
SomeGraphs.Distributions.DistributionsGraphConfiguration
```

Titles:

```@example
using SomeGraphs
graph = distributions_graph(;
    distributions = [
        DistributionData(;
            values = VectorValuesData(; vector = [0, 0, 1, 1, 1, 3], title = "Values title"),
            name = "Foo",
        ),
        DistributionData(; values = VectorValuesData([4, 4, 3, 3, 3, 1]), name = "Bar"),
    ],
    figure_title = "Figure title",
)
using PlotlyDocumenter
to_documenter(graph.fig)
```

Size of gap between distributions:

```@example
using SomeGraphs
graph = distributions_graph(;
    distributions = [
        DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])),
        DistributionData(; values = VectorValuesData([4, 4, 3, 3, 3, 1])),
    ],
)
graph.configuration.distributions_gap = 80
using PlotlyDocumenter
to_documenter(graph.fig)
```

Overlay the distributions:

```@example
using SomeGraphs
graph = distributions_graph(;
    distributions = [
        DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])),
        DistributionData(; values = VectorValuesData([4, 4, 3, 3, 3, 1])),
    ],
)
graph.configuration.distributions_gap = nothing
using PlotlyDocumenter
to_documenter(graph.fig)
```

Overlay the distributions with a legend:

```@example
using SomeGraphs
graph = distributions_graph(;
    distributions = [
        DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3]), name = "Foo"),
        DistributionData(; values = VectorValuesData([4, 4, 3, 3, 3, 1]), name = "Bar"),
    ],
)
graph.configuration.distributions_gap = nothing
using PlotlyDocumenter
to_documenter(graph.fig)
```

Colors (if they are part of the data):

```@example
using SomeGraphs
graph = distributions_graph(;
    distributions = [
        DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3]), color = "red"),
        DistributionData(; values = VectorValuesData([4, 4, 3, 3, 3, 1]), color = "green"),
    ],
)
using PlotlyDocumenter
to_documenter(graph.fig)
```

You can also apply any of the distribution and/or value axis configuration options; these will apply to all the distributions:

```@example
using SomeGraphs
graph = distributions_graph(;
    distributions = [
        DistributionData(; values = VectorValuesData([0, 0, 1, 1, 1, 3])),
        DistributionData(; values = VectorValuesData([4, 4, 3, 3, 3, 1])),
    ],
)
graph.configuration.distribution.values_orientation = VerticalValues
graph.configuration.distribution.line.color = "red"
graph.configuration.distribution.style = BoxOutliersDistribution
using PlotlyDocumenter
to_documenter(graph.fig)
```

## Index

```@index
Pages = ["distributions.md"]
```
