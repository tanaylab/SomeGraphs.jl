"""
Validate graph data.

Rendering graphs requires two objects: data and configuration. Both objects need to be internally consistent, as does
their combination. This is especially relevant for the graph configuration. When creating UI for filling in these
objects, we can do limited validation of each field on its own based on its type (e.g. (e.g., ensure that a "color"
field contains a valid color name). Some restrictions, however, are not easily deduced from the field type, or relate
different fields to each other. Replicating all the restrictions in the UI is tedious and error prone.

We therefore provide a framework here for validating the objects and generate a hopefully informative error message if
anything is wrong. This can be used by the UI to validate the data without worrying about the details.
"""
module Validations

export ValidationContext
export location

"""
A context (path of field names and/or indices) leading to a validated value. A string indicates access of a data member,
an integer indicates accessing a vector or matrix element.
"""
ValidationContext = Vector{Union{AbstractString, Integer}}

"""
    stringify_context(context::ValidationContext)::AbstractString

Convert a [`ValidationContext`](@ref) to a string for error messages.
"""
function location(context::ValidationContext)::AbstractString
    text = AbstractString[]

    last_is_index = false
    for entry in context
        if entry isa AbstractString
            if length(text) != 0
                if last_is_index
                    push!(text, "].")
                else
                    push!(text, ".")
                end
            end
            push!(text, entry)
            last_is_index = false
        else
            if last_is_index
                push!(text, ", ")
            else
                push!(text, "[")
            end
            push!(text, string(entry))
            last_is_index = true
        end
    end

    if last_is_index
        push!(text, "]")
    end

    return join(text)
end

end
