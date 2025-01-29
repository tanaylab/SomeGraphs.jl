using Test

using SomeGraphs
using NestedTests

test_prefixes(ARGS)
abort_on_first_failure(true)

# NOTE: This is replicated from DataAxesFormats to avoid making it a dependency.
function dedent(string::AbstractString; indent::AbstractString = "")::String
    lines = split(string, "\n")
    while !isempty(lines) && isempty(lines[1])
        @views lines = lines[2:end]  # untested
    end
    while !isempty(lines) && isempty(lines[end])
        @views lines = lines[1:(end - 1)]
    end

    first_non_space = nothing
    for line in lines
        line_non_space = findfirst(character -> character != ' ', line)
        if first_non_space === nothing || (line_non_space !== nothing && line_non_space < first_non_space)
            first_non_space = line_non_space
        end
    end  # NOJET

    if first_non_space === nothing
        return indent * string  # untested NOJET
    else
        return join([indent * line[first_non_space:end] for line in lines], "\n")  # NOJET
    end
end

include("validations.jl")
