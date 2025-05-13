# SomeGraphs - Generate graphs (using Plotly)

## Motivation

The goals of this are:

  - Provide a specific set of graphs (chosen for supporting a specific application, but covers the common types).

  - Generate static PNG and/or SVG files (for publishing).
  - Generate interactive graphs in Jupyter notebook, also from Python and from R (for exploration).
  - Be reasonably efficient when the data is large.
  - Minimize dependencies (except for Plotly).
  - Provide strongly types set of structures for specifying the graphs, where fields are as high-level ("what") as
    possible, and are orthogonal to each other (as much as humanly possible). This is very different from Plotly
    APIs where fields are low level ("how") and achieving "simple" effects requires changing many of them at
    the same time.

Non-goals are:

  - Provide "every" graph type and/or feature.

See the [documentation](https://tanaylab.github.io/SomeGraphs.jl/v0.1.0) for details.

## Architecture

The subset of graph types and features was picked to support a specific set of applications, so there's explicitly no
attempt to be an end-all-be-all do-everything graph library. Still, basic graph types are reasonably well covered so
this may be useful in general.

The APIs were designed to be strongly typed, with a focus on orthogonality of features. This makes it natural to create
UI widgets to control the graphs. To further support wrapping this in a UI, there's also a focus on checking the data
and providing reasonably informative error messages when something is invalid.

A graph is specified as a combination of the data (what to display) and a configuration (how to display it). For most
things, this distinction is clear (e.g., coordinates of a point are data, the size of the graph is configuration).
Sometimes it is less so (colors). We therefore allow the data to override the configuration in cases where this depends
on the context of the graph. The idea is that you can apply the same configuration to multiple sets of data.

Each graph type has its own strongly-types data and configuration classes, with reasonable defaults. Therefore any IDE
with auto-completion will assist you in setting a specific option. It also makes it easier to create UIs widgets to
generate a valid configuration. We try to reuse structures as much as possible to support UI widgets reuse.

It turns out that converting such an API to Plotly calls is way more complex that you'd expect. Consider for example a
flag that changes the layout of a graph showing a probability distribution function from horizontal to vertical; this
affects almost every flag you pass to the Plotly objects as you need to rename all the X and Y related flags.

Using Plotly allows us to create pretty much every type of graph we want (though sometimes we needed to get "creative"),
and has the advantage we can create a JSON representation of the graph which we can pass on to allow using this package
from Python and R (e.g. in a Jupyter notebook).

Choosing Plotly as the underlying graph rendering technology exposes us to some of its limitations, though. The most
annoying ones are related to Plotly's lack of consideration to sizes of some graph elements - basically everything
ourtside the graph area proper. This means one has to manually specify margin sizes, legend positions, gaps between
sub-graphs etc., which is tedious. Even worse, in most cases one needs to specify these in fractions of the overall
graph size, which is an unnatural unit for most of these.

While the overall architecture of the code here generalizes well, the functionality here is intentionally restricted to
what we found useful for our applications. Likewise, the amount of customization of the graphs is intentionally limited.
The intent here is to make it as easy as possible for the analyst working in Jupyter notebook to explore some data, and
generate graphs for academic papers, **not** to create yet another end-all-be-all graphs framework (of which there are
too many already).

## Installation

Just `Pkg.add("SomeGraphs")`, like installing any other Julia package. However, if being used in Jupyter notebook,
you will probably want to install the language specific package instead:

To install the Python `Daf` [package](https://github.com/tanaylab/some_graphs.py), just `pip install some_graphs`, like
installing any other Python package.

TODO: To install the R wrappers...

## License (MIT)

Copyright © 2025 Weizmann Institute of Science

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
