"""
Provide convenient API for setting up graph's data. The core concept is that we can break the graph definition to parts
w/ uniform structure, so that writing a data source to fill this structure allows using it to fill whatever data we want

  - X coordinates, Y coordinates, colors, bar sizes, etc.

We have a few kinds of these uniform structures. Each is built from two parts - one for all the data fields and one for
all the configuration fields. All these fields are direct references to the graph fields, so they can be set via this
view into the graph.

Some fields are shared between several views. For example, hovers for points in scatter graphs can be given by the X
data, the Y data, or the color data. To support this we provide functions for adding hovers into the common (shared)
field instead of simply overwriting it.

Some views are of one entry of a vector of structures (a series of bars, a line, a distribution, an annotation). Such an
entry is appended by `add_series!` and its siblings, which return the index the view accessors take.
"""
module Sources

export AbstractFields
export AbstractConfigurationFields
export AxisConfigurationFields
export AxisVectorFields
export ColorsConfigurationFields
export ColorsVectorFields
export MatrixConfigurationFields
export MatrixDataFields
export MatrixFields
export PartFields
export SizesConfigurationFields
export SizesVectorFields
export VectorDataFields
export VectorFields
export add_annotation!
export add_columns_annotation!
export add_distribution!
export add_hovers!
export add_line!
export add_rows_annotation!
export add_series!
export annotations_colors_vector_fields
export borders_colors_vector_fields
export borders_sizes_vector_fields
export colors_vector_fields
export columns_annotations_colors_vector_fields
export columns_groups_vector_data_fields
export columns_subgroups_vector_data_fields
export distribution_axis_vector_fields
export distribution_part_fields
export distributions_axis_vector_fields
export edges_colors_vector_fields
export edges_sizes_vector_fields
export entries_matrix_fields
export line_part_fields
export points_colors_vector_fields
export points_sizes_vector_fields
export rows_annotations_colors_vector_fields
export rows_groups_vector_data_fields
export rows_subgroups_vector_data_fields
export series_axis_vector_fields
export series_part_fields
export values_axis_vector_fields
export x_axis_vector_fields
export y_axis_vector_fields

using ..Common
using ..Validations

import ..Validations.Maybe

"""
    add_hovers!(
        entities::Union{VectorEntitiesData, MatrixEntitiesData},
        hovers::AbstractArray{<:AbstractString};
        [title::Maybe{AbstractString} = nothing]
    )::Nothing

Add a line to the hover of each of the `entities`: the `hovers` entry of the entity (a vector for [`VectorEntitiesData`](@ref),
a matrix for [`MatrixEntitiesData`](@ref)), prefixed by the `title` (if any) as `title: hover`. The lines of several
calls are joined by `<br>`, in the order of the calls. All the `hovers` given to the same entities must be of the same
size.
"""
function add_hovers!(
    entities::Union{VectorEntitiesData, MatrixEntitiesData},
    hovers::AbstractArray{<:AbstractString};
    title::Maybe{AbstractString} = nothing,
)::Nothing
    if title !== nothing
        hovers = "$(title): " .* hovers
    end

    existing_hovers = entities.hovers
    if existing_hovers === nothing
        entities.hovers = hovers
    else
        if size(existing_hovers) != size(hovers)
            throw(
                ArgumentError(
                    "invalid size of added hovers: $(size(hovers))\n" *
                    "is different from size of existing hovers: $(size(existing_hovers))",
                ),
            )
        end
        entities.hovers = existing_hovers .* "<br>" .* hovers
    end

    return nothing
end

"""
    struct VectorDataFields
        values::VectorValuesData
        entities::VectorEntitiesData
    end

The data half of a data source view (see [`VectorFields`](@ref)): the [`VectorValuesData`](@ref) of one role of a graph (the
X coordinates of its points, their colors, ...) and the [`VectorEntitiesData`](@ref) of the entities these values belong to.
Both are the graph's own objects, so writing into them changes the graph. Several roles of the same entities (say, the
X, Y, colors and sizes of points) share one `entities`, so hovers added through any of them are seen by all.

A source which only writes values, a title and hovers takes a `VectorDataFields`. The data half of every
[`VectorFields`](@ref) is one, and so is a view of a role that has no configuration to speak of (the names of the bars,
the groups of the rows of a heatmap), so such a source applies to all of them alike.
"""
struct VectorDataFields
    values::VectorValuesData
    entities::VectorEntitiesData
end

"""
    struct MatrixDataFields
        values::MatrixValuesData
        entities::MatrixEntitiesData
        rows_entities::VectorEntitiesData
        columns_entities::VectorEntitiesData
    end

The data half of a [`MatrixFields`](@ref) data source view: the [`MatrixValuesData`](@ref) of the entries of a graph (a
heatmap), the [`MatrixEntitiesData`](@ref) of its cells, and the [`VectorEntitiesData`](@ref) of each of its two axes.
All are the graph's own objects, so writing into them changes the graph.

A matrix source knows the two axes its data is indexed by, so it can add hovers to all three: one per cell, one per row
and one per column. The axis entities are the same ones the row and column views hand out
(`rows_annotations_colors_vector_fields`, ...), so whatever is written through either path is seen by both.
"""
struct MatrixDataFields
    values::MatrixValuesData
    entities::MatrixEntitiesData
    rows_entities::VectorEntitiesData
    columns_entities::VectorEntitiesData
end

"""
Abstract interface for the configuration half of `AbstractFields`. All concrete types have an `axis::AxisConfiguration`
field, most have additional fields as appropriate for the specific configuration.
"""
abstract type AbstractConfigurationFields end

"""
    struct AxisConfigurationFields <: AbstractConfigurationFields
        axis::AxisConfiguration
    end

The configuration half of an `AxisVectorFields` data source view (see [`VectorFields`](@ref)): the
[`AxisConfiguration`](@ref) the values are shown along.
"""
struct AxisConfigurationFields <: AbstractConfigurationFields
    axis::AxisConfiguration
end

"""
    struct ColorsConfigurationFields <: AbstractConfigurationFields
        axis::AxisConfiguration
        colors::ColorsConfiguration
    end

The configuration half of a `ColorsVectorFields` data source view (see [`VectorFields`](@ref)): the
[`ColorsConfiguration`](@ref) the values are colored by, and its `axis`.
"""
struct ColorsConfigurationFields <: AbstractConfigurationFields
    axis::AxisConfiguration
    colors::ColorsConfiguration
end

function ColorsConfigurationFields(colors::ColorsConfiguration)::ColorsConfigurationFields
    return ColorsConfigurationFields(colors.axis, colors)
end

"""
    struct SizesConfigurationFields <: AbstractConfigurationFields
        axis::AxisConfiguration
        sizes::SizesConfiguration
    end

The configuration half of a `SizesVectorFields` data source view (see [`VectorFields`](@ref)): the
[`SizesConfiguration`](@ref) the values are sized by, and its `axis`.
"""
struct SizesConfigurationFields <: AbstractConfigurationFields
    axis::AxisConfiguration
    sizes::SizesConfiguration
end

function SizesConfigurationFields(sizes::SizesConfiguration)::SizesConfigurationFields
    return SizesConfigurationFields(sizes.axis, sizes)
end

"""
    struct MatrixConfigurationFields <: AbstractConfigurationFields
        axis::AxisConfiguration
        colors::ColorsConfiguration
    end

The configuration half of a [`MatrixFields`](@ref) data source view: the [`ColorsConfiguration`](@ref) the entries are
colored by, and its `axis`.
"""
struct MatrixConfigurationFields <: AbstractConfigurationFields
    axis::AxisConfiguration
    colors::ColorsConfiguration
end

function MatrixConfigurationFields(colors::ColorsConfiguration)::MatrixConfigurationFields
    return MatrixConfigurationFields(colors.axis, colors)
end

"""
Abstract interface for a full data source consisting of `data` and a `configuration` fields. The concrete types of each
depend on the specific graph role. These are views into the graph fields, bundled together to allow a function to
easily set them in a uniform way regardless of the specific rule they play in the graph.
"""
abstract type AbstractFields end

"""
    struct VectorFields{Configuration} <: AbstractFields
        data::VectorDataFields
        configuration::Configuration
    end

    AxisVectorFields = VectorFields{AxisConfigurationFields}
    ColorsVectorFields = VectorFields{ColorsConfigurationFields}
    SizesVectorFields = VectorFields{SizesConfigurationFields}

A data source view of one role of a graph whose entities are a vector: the `data` (a [`VectorDataFields`](@ref)) and
the `configuration` (whose `axis` is an [`AxisConfiguration`](@ref), whatever else it holds). A function writing into
such a view fills the role from some source of data, and works the same on the X coordinates of points, the values of
bars, the colors of either, and so on. The views are `AxisVectorFields` for values shown along an axis, `ColorsVectorFields` for
values shown as colors (see [`ColorsConfigurationFields`](@ref)) and `SizesVectorFields` for values shown as sizes (see
[`SizesConfigurationFields`](@ref)). They are obtained from a graph by the accessor functions (`x_axis_vector_fields`,
`colors_vector_fields`, ...), whose names follow the path of the values in the data of the graph.
"""
struct VectorFields{Configuration} <: AbstractFields
    data::VectorDataFields
    configuration::Configuration
end

AxisVectorFields = VectorFields{AxisConfigurationFields}
ColorsVectorFields = VectorFields{ColorsConfigurationFields}
SizesVectorFields = VectorFields{SizesConfigurationFields}

function VectorFields(values::VectorValuesData, entities::VectorEntitiesData, configuration::Any)::VectorFields
    return VectorFields(VectorDataFields(values, entities), configuration)
end

# The name of the field of a part which holds its `VectorEntitiesData` (`:bars` for a series of bars, `:points` for a
# line or a distribution). This is what lets a `PartFields` offer them all as `entities`. Each kind of part implements
# it in its own module.
function entities_field end

"""
    struct PartFields{GraphType, PartType <: AbstractPartData}
        graph::GraphType
        index::Int
        data::PartType
    end

The data source view of one part of a graph built from several such parts (a series of bars, a line, a distribution).
This allows a data source to set the part regardless of the role it plays in the graph. Since parts may have multiple
instances, they are identified by their `index` in the `graph`. Parts are different from simple vector data in that they
have scalar properties (e.g., name, hover) and may contain multiple vector data (e.g., both x and y coordinates for a
line part). The part `data` acts as a view that allows accessing the relevant fields depending on the `PartType`.
"""
struct PartFields{GraphType, PartType <: AbstractPartData}
    graph::GraphType
    index::Int
    data::PartType
end

# The roles a part plays in a graph. A part has either one `values` role (a series of bars, a distribution) or an `x`
# and a `y` role (a line), never both.
const PART_ROLES = (:x, :y, :values)

# Build the data source view of one role of a part. Each kind of part implements this for the roles it has, since only
# it knows which axis of the graph its values are shown along. Asking a part for a role it does not have lands here.
function part_role_fields(part::PartFields, ::Val{role})::VectorFields where {role}
    return throw(ArgumentError("no $(role) role for $(typeof(getfield(part, :data)))"))
end

function Base.getproperty(part::PartFields, name::Symbol)
    if name in (:graph, :data, :index)
        return getfield(part, name)
    elseif name in PART_ROLES
        return part_role_fields(part, Val(name))
    end
    data = getfield(part, :data)
    return getproperty(data, name === :entities ? entities_field(data) : name)
end

function Base.setproperty!(part::PartFields, name::Symbol, value::Any)::Any
    if name in (:graph, :data, :index)
        throw(ArgumentError("can't set $(name) of a PartFields"))
    elseif name in PART_ROLES
        throw(ArgumentError("can't set the $(name) role of a PartFields\nset its .data.$(name) instead"))
    end
    data = getfield(part, :data)
    return setproperty!(data, name === :entities ? entities_field(data) : name, value)
end

function Base.propertynames(part::PartFields, private::Bool = false)::Tuple
    data = getfield(part, :data)
    names = Tuple(name === entities_field(data) ? :entities : name for name in propertynames(data, private))
    return (:graph, :data, :index, names...)
end

"""
    struct MatrixFields <: AbstractFields
        data::MatrixDataFields
        configuration::MatrixConfigurationFields
    end

The data source view of the entries of a graph whose entities are arranged in rows and columns (the entries of a
heatmap), shown as colors.
"""
struct MatrixFields <: AbstractFields
    data::MatrixDataFields
    configuration::MatrixConfigurationFields
end

function MatrixFields(
    values::MatrixValuesData,
    entities::MatrixEntitiesData,
    rows_entities::VectorEntitiesData,
    columns_entities::VectorEntitiesData,
    colors::ColorsConfiguration,
)::MatrixFields
    return MatrixFields(
        MatrixDataFields(values, entities, rows_entities, columns_entities),
        MatrixConfigurationFields(colors),
    )
end

"""
    x_axis_vector_fields(graph)::AxisVectorFields
    x_axis_vector_fields(graph, index::Integer)::AxisVectorFields

The data source view of the X coordinates of a graph (of its points; of the points of one of its lines, given the
`index` of the line).
"""
function x_axis_vector_fields end

"""
    y_axis_vector_fields(graph)::AxisVectorFields
    y_axis_vector_fields(graph, index::Integer)::AxisVectorFields

The data source view of the Y coordinates of a graph (of its points; of the points of one of its lines, given the
`index` of the line).
"""
function y_axis_vector_fields end

"""
    points_colors_vector_fields(graph)::ColorsVectorFields

The data source view of the colors of the points of a graph.
"""
function points_colors_vector_fields end

"""
    points_sizes_vector_fields(graph)::SizesVectorFields

The data source view of the sizes of the points of a graph.
"""
function points_sizes_vector_fields end

"""
    borders_colors_vector_fields(graph)::ColorsVectorFields

The data source view of the colors of the borders of the points of a graph. The borders share the entities of the
points.
"""
function borders_colors_vector_fields end

"""
    borders_sizes_vector_fields(graph)::SizesVectorFields

The data source view of the sizes of the borders of the points of a graph. The borders share the entities of the points.
"""
function borders_sizes_vector_fields end

"""
    edges_colors_vector_fields(graph)::ColorsVectorFields

The data source view of the colors of the edges of a graph.
"""
function edges_colors_vector_fields end

"""
    edges_sizes_vector_fields(graph)::SizesVectorFields

The data source view of the sizes (widths) of the edges of a graph.
"""
function edges_sizes_vector_fields end

"""
    values_axis_vector_fields(graph)::AxisVectorFields

The data source view of the values of a graph (of its bars).
"""
function values_axis_vector_fields end

"""
    colors_vector_fields(graph)::ColorsVectorFields

The data source view of the colors of a graph (of its bars).
"""
function colors_vector_fields end

"""
    series_axis_vector_fields(graph, index::Integer)::AxisVectorFields

The data source view of the values of one series of a graph, given the `index` of the series.
"""
function series_axis_vector_fields end

"""
    annotations_colors_vector_fields(graph, index::Integer)::ColorsVectorFields

The data source view of one annotation of a graph, given the `index` of the annotation. The annotation shares the
entities of the axis it annotates (the bars).
"""
function annotations_colors_vector_fields end

"""
    distribution_axis_vector_fields(graph)::AxisVectorFields

The data source view of the values of the distribution of a graph.
"""
function distribution_axis_vector_fields end

"""
    distributions_axis_vector_fields(graph, index::Integer)::AxisVectorFields

The data source view of the values of one distribution of a graph, given the `index` of the distribution.
"""
function distributions_axis_vector_fields end

"""
    distribution_part_fields(graph, index::Integer)::PartFields

The data source view of one distribution of a graph, given the `index` of the distribution (see [`PartFields`](@ref)).
"""
function distribution_part_fields end

"""
    line_part_fields(graph, index::Integer)::PartFields

The data source view of one line of a graph, given the `index` of the line (see [`PartFields`](@ref)).
"""
function line_part_fields end

"""
    series_part_fields(graph, index::Integer)::PartFields

The data source view of one series of bars of a graph, given the `index` of the series (see [`PartFields`](@ref)).
"""
function series_part_fields end

"""
    entries_matrix_fields(graph)::MatrixFields

The data source view of the entries of a graph (of a heatmap).
"""
function entries_matrix_fields end

"""
    rows_annotations_colors_vector_fields(graph, index::Integer)::ColorsVectorFields

The data source view of one annotation of the rows of a graph, given the `index` of the annotation. The annotation
shares the entities of the rows.
"""
function rows_annotations_colors_vector_fields end

"""
    columns_annotations_colors_vector_fields(graph, index::Integer)::ColorsVectorFields

The data source view of one annotation of the columns of a graph, given the `index` of the annotation. The annotation
shares the entities of the columns.
"""
function columns_annotations_colors_vector_fields end

"""
    rows_groups_vector_data_fields(graph)::VectorDataFields

The data source view of the groups of the rows of a graph. The groups have no title.
"""
function rows_groups_vector_data_fields end

"""
    rows_subgroups_vector_data_fields(graph)::VectorDataFields

The data source view of the subgroups of the rows of a graph. The subgroups have no title.
"""
function rows_subgroups_vector_data_fields end

"""
    columns_groups_vector_data_fields(graph)::VectorDataFields

The data source view of the groups of the columns of a graph. The groups have no title.
"""
function columns_groups_vector_data_fields end

"""
    columns_subgroups_vector_data_fields(graph)::VectorDataFields

The data source view of the subgroups of the columns of a graph. The subgroups have no title.
"""
function columns_subgroups_vector_data_fields end

"""
    add_series!(graph, [series::SeriesData = SeriesData()])::Int

Append a series to a graph (of series of bars) and return its index (for `series_axis_vector_fields`). Whatever the `series`
leaves at its defaults can be set later, through the view or directly.
"""
function add_series! end

"""
    add_line!(graph, [line::LineData = LineData()])::Int

Append a line to a graph (of lines) and return its index (for `x_axis_vector_fields` and `y_axis_vector_fields`). Whatever the `line` leaves
at its defaults can be set later, through the views or directly.
"""
function add_line! end

"""
    add_distribution!(graph, [distribution::DistributionData = DistributionData()])::Int

Append a distribution to a graph (of distributions) and return its index (for `distributions_axis_vector_fields`). Whatever
the `distribution` leaves at its defaults can be set later, through the view or directly.
"""
function add_distribution! end

"""
    add_annotation!(graph, [annotation::AnnotationData = AnnotationData()])::Int

Append an annotation to the entities of a graph (the bars) and return its index (for `annotations_colors_vector_fields`). Whatever
the `annotation` leaves at its defaults can be set later, through the view.
"""
function add_annotation! end

"""
    add_rows_annotation!(graph, [annotation::AnnotationData = AnnotationData()])::Int

Append an annotation to the rows of a graph and return its index (for `rows_annotations_colors_vector_fields`). Whatever the
`annotation` leaves at its defaults can be set later, through the view.
"""
function add_rows_annotation! end

"""
    add_columns_annotation!(graph, [annotation::AnnotationData = AnnotationData()])::Int

Append an annotation to the columns of a graph and return its index (for `columns_annotations_colors_vector_fields`). Whatever the
`annotation` leaves at its defaults can be set later, through the view.
"""
function add_columns_annotation! end

end  # module
