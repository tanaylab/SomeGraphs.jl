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

export Validated
export ValidationContext
export location
export validate
export validate_in
export validate_is_above
export validate_is_at_least
export validate_is_at_most
export validate_is_below
export validate_is_color
export validate_is_range
export validate_matrix_entries
export validate_matrix_is_not_empty
export validate_matrix_size
export validate_vector_entries
export validate_vector_is_not_empty
export validate_vector_length

using Colors

"""
    Maybe{T} = Union{T, Nothing}

The type to use when maybe there is a value, maybe there isn't. This is exactly as if writing the explicit `Union`
with `Nothing` but is shorter and more readable. This is extremely common.

!!! note

    This is replicated from `DataAxesFormats` to avoid making it a dependency. We do not export it but use it
    extensively in the type signatures.
"""
Maybe = Union{T, Nothing} where {T}

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

    return join(text)  # NOJET
end

"""
A common type for objects that support validation, that is, that one can invoke [`validate`](@ref) on.
"""
abstract type Validated end

"""
    validate(context::ValidationContext, value::Validated)::Nothing end
    validate(context::ValidationContext, value::Validated, extra::Any)::Nothing end
    validate(context::ValidationContext, value::Validated, extra::Any, another::Any)::Nothing end

Validate the `value` which was accessed via the `context`, possibly using some `extra` informative. Will throw
`ArgumentError` if the value isn't valid.
"""
function validate(context::ValidationContext, value::Validated)::Nothing end  # NOLINT
function validate(context::ValidationContext, value::Validated, extra::Any)::Nothing end  # NOLINT
function validate(context::ValidationContext, value::Validated, extra::Any, another::Any)::Nothing end  # NOLINT

"""
    validate_in(validation::Function, context::ValidationContext, where::Union{AbstractString, Integer})::Nothing

Invoke the `validation` function with the `context` updated to include some `where`.
"""
function validate_in(validation::Function, context::ValidationContext, where::Union{AbstractString, Integer})::Nothing
    push!(context, where)
    try
        validation()
    finally
        pop!(context)  # NOJET
    end
end

"""
    validate_is_at_least(context::ValidationContext, value::Maybe{Real}, minimum::Real)::Nothing

Validate that a `value` is at least some `minimum` (if it is specified).
"""
function validate_is_at_least(context::ValidationContext, value::Maybe{Real}, minimum::Real)::Nothing
    if value !== nothing && value < minimum
        throw(ArgumentError("too low $(location(context)): $(value)\nis not at least: $(minimum)"))
    end
    return nothing
end

"""
    validate_is_above(context::ValidationContext, value::Maybe{Real}, minimum::Real)::Nothing

Validate that a `value` is above some `minimum` (if it is specified).
"""
function validate_is_above(context::ValidationContext, value::Maybe{Real}, minimum::Real)::Nothing
    if value !== nothing && value <= minimum
        throw(ArgumentError("too low $(location(context)): $(value)\nis not above: $(minimum)"))
    end
    return nothing
end

"""
    validate_is_at_most(context::ValidationContext, value::Maybe{Real}, maximum::Real)::Nothing

Validate that a `value` is at most some `maximum` (if it is specified).
"""
function validate_is_at_most(context::ValidationContext, value::Maybe{Real}, maximum::Real)::Nothing
    if value !== nothing && value > maximum
        throw(ArgumentError("too high $(location(context)): $(value)\nis not at most: $(maximum)"))
    end
    return nothing
end

"""
    validate_is_below(context::ValidationContext, value::Maybe{Real}, maximum::Real)::Nothing

Validate that a `value` is below some `maximum` (if it is specified).
"""
function validate_is_below(context::ValidationContext, value::Maybe{Real}, maximum::Real)::Nothing
    if value !== nothing && value >= maximum
        throw(ArgumentError("too high $(location(context)): $(value)\nis not below: $(maximum)"))
    end
    return nothing
end

"""
    validate_is_color(context::ValidationContext, color::Maybe{AbstractString})::Nothing

Validate that a `color` is a valid color name (if it is specified).
"""
function validate_is_color(context::ValidationContext, color::Maybe{AbstractString})::Nothing
    if color !== nothing
        try
            parse(Colorant, color)  # NOJET
        catch
            throw(ArgumentError("invalid $(location(context)): $(color)"))
        end
    end
    return nothing
end

"""
    validate_is_range(
        context::ValidationContext,
        low_where::AbstractString,
        low_value::Maybe{Real},
        high_where::AbstractString,
        high_value::Maybe{Real},
    )::Nothing

Validate that if both `low_value` and `high_value` are specified, they define a non-empty range.
"""
function validate_is_range(
    context::ValidationContext,
    low_where::AbstractString,
    low_value::Maybe{Real},
    high_where::AbstractString,
    high_value::Maybe{Real},
)::Nothing
    if low_value !== nothing && high_value !== nothing && low_value >= high_value
        throw(
            ArgumentError(
                "range low limit $(location(context)).$(low_where): $(low_value)\n" *
                "is not below high limit $(location(context)).$(high_where): $(high_value)",
            ),
        )
    end
    return nothing
end

"""
    validate_vector_length(
        context::ValidationContext,
        field::AbstractString,
        vector::Maybe{AbstractVector},
        expected_base::AbstractString,
        expected_length::Integer
    )::Nothing

Validate that a `field` containing a `vector` has (if it is specified) the `expected_length` of an `expected_base` field.
"""
function validate_vector_length(
    context::ValidationContext,
    field::AbstractString,
    vector::Maybe{AbstractVector},
    expected_base::AbstractString,
    expected_length::Integer,
)::Nothing
    if vector !== nothing && length(vector) != expected_length
        throw(
            ArgumentError(
                "invalid length of $(location(context)).$(field): $(length(vector))\n" *
                "is different from length of $(location(context)).$(expected_base): $(expected_length)",
            ),
        )
    end
    return nothing
end

"""
    validate_vector_is_not_empty(
        context::ValidationContext,
        [field::AbstractString,]
        vector::AbstractVector
    )::Nothing

Validate that a `field` containing a `vector` has at least one entry.
"""
function validate_vector_is_not_empty(
    context::ValidationContext,
    field::AbstractString,
    vector::AbstractVector,
)::Nothing
    validate_in(context, field) do
        return validate_vector_is_not_empty(context, vector)
    end
    return nothing
end

function validate_vector_is_not_empty(context::ValidationContext, vector::AbstractVector)::Nothing
    if length(vector) == 0
        throw(ArgumentError("empty vector $(location(context))"))
    end
    return nothing
end

"""
    validate_vector_entries(
        validation::Function,
        context::ValidationContext,
        field::AbstractString,
        vector::Maybe{AbstractVector}
    )::Nothing

Validate all the entries of a `field` containing a `vector` using the `validation` function. It is given the entry's
index, and its value. The context is updated to include the index for the duration of the function.
"""
function validate_vector_entries(
    validation::Function,
    context::ValidationContext,
    field::AbstractString,
    vector::Maybe{AbstractVector},
)::Nothing
    validate_in(context, field) do
        if vector !== nothing
            for (index, entry) in enumerate(vector)
                validate_in(context, index) do
                    return validation(index, entry)
                end
            end
        end
    end
    return nothing
end

"""
    validate_matrix_is_not_empty(
        context::ValidationContext,
        field::AbstractString,
        matrix::AbstractMatrix
    )::Nothing

Validate that a `field` containing a `matrix` has at least one entry.
"""
function validate_matrix_is_not_empty(
    context::ValidationContext,
    field::AbstractString,
    matrix::AbstractMatrix,
)::Nothing
    if size(matrix, 1) == 0 || size(matrix, 2) == 0
        throw(ArgumentError("empty matrix $(location(context)).$(field) size: $(size(matrix))"))
    end
    return nothing
end

"""
    validate_matrix_size(
        context::ValidationContext,
        matrix::Maybe{AbstractMatrix},
        field::AbstractString,
        expected_size::Tuple{Integer, Integer}
    )::Nothing

Validate that a `field` containing `matrix` has (if it is specified) the `expected_size` of a `base_field`.
"""
function validate_matrix_size(
    context::ValidationContext,
    field::AbstractString,
    matrix::Maybe{AbstractMatrix},
    base_field::AbstractString,
    expected_size::Tuple{Integer, Integer},
)::Nothing
    if matrix !== nothing && size(matrix) != expected_size
        throw(
            ArgumentError(
                "invalid size of $(location(context)).$(field): $(size(matrix))\n" *
                "is different from size of $(location(context)).$(base_field): $(expected_size)",
            ),
        )
    end
    return nothing
end

"""
    validate_matrix_entries(
        validation::Function,
        context::ValidationContext,
        field::AbstractString,
        matrix::Maybe{AbstractMatrix}
    )::Nothing

Validate all the entries of a `field` containing a `matrix` using the `validation` function. It is given the entry's row
and column indices, and its value. The context is updated to include the indices for the duration of the function.
"""
function validate_matrix_entries(
    validation::Function,
    context::ValidationContext,
    field::AbstractString,
    matrix::Maybe{AbstractMatrix},
)::Nothing
    if matrix !== nothing
        validate_in(context, field) do
            n_rows, n_columns = size(matrix)
            for row_index in 1:n_rows
                validate_in(context, row_index) do
                    for column_index in n_columns
                        validate_in(context, column_index) do
                            return validation(row_index, column_index, matrix[row_index, column_index])
                        end
                    end
                end
            end
        end
    end
    return nothing
end

end
