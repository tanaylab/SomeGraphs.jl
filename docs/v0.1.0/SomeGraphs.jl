"""
Wrappers around Plotly to generate some types of graphs.
"""
module SomeGraphs

using Reexport

include("validations.jl")
@reexport using .Validations

end  # module
