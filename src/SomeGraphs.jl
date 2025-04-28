"""
Wrappers around Plotly to generate some types of graphs.
"""
module SomeGraphs

using Reexport

include("validations.jl")
@reexport using .Validations

include("common.jl")
@reexport using .Common

include("utilities.jl")
@reexport using .Utilities

include("distributions.jl")
@reexport using .Distributions

include("scatters.jl")
@reexport using .Scatters

include("bars.jl")
@reexport using .Bars

include("heatmaps.jl")
@reexport using .Heatmaps

end  # module
