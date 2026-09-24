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
export AnyContainer
export AnyLeaf
export AnySink
export ConfigurationContainer
export ConfigurationLeaf
export ConfigurationSink
export DataContainer
export DataLeaf
export DataSink
export Sinks
export visit_configuration_sinks
export visit_data_sinks
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
export bars_entities
export borders_colors_vector_fields
export borders_sizes_vector_fields
export columns_entities
export distribution_entities
export edges_entities
export points_entities
export rows_entities
export colors_vector_fields
export columns_annotations_colors_vector_fields
export columns_groups_vector_data_fields
export columns_subgroups_vector_data_fields
export distribution_axis_vector_fields
export distribution_part_fields
export edges_colors_vector_fields
export edges_sizes_vector_fields
export entries_matrix_fields
export line_part_fields
export points_colors_vector_fields
export points_sizes_vector_fields
export rows_annotations_colors_vector_fields
export rows_groups_vector_data_fields
export rows_subgroups_vector_data_fields
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

A matrix source knows the two axes its data is indexed by, so it can fill what belongs to them: the names of the rows
and of the columns, shown as their tick labels, and a hover per row and per column on top of the hover of each cell.
The axis entities are the same ones the row and column views hand out (`rows_annotations_colors_vector_fields`, ...),
so whatever is written through either path is seen by both.
"""
struct MatrixDataFields
    values::MatrixValuesData
    entities::MatrixEntitiesData
    rows_entities::VectorEntitiesData
    columns_entities::VectorEntitiesData
end

"""
Abstract interface for the configuration half of `AbstractFields`. Every concrete type says how the values are mapped
to the range they are shown in, and most have additional fields as appropriate for the specific configuration. A role
drawn along an axis has an `axis::AxisConfiguration`; a role shown as colors or sizes is not drawn along an axis, so it
has only a `scale::ScaleConfiguration`.
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
        scale::ScaleConfiguration
        colors::ColorsConfiguration
    end

The configuration half of a `ColorsVectorFields` data source view (see [`VectorFields`](@ref)): the
[`ColorsConfiguration`](@ref) the values are colored by, and its `scale`. Colors are not drawn along an axis, so there
is no title or ticks here; the colors title is the title of the values.
"""
struct ColorsConfigurationFields <: AbstractConfigurationFields
    scale::ScaleConfiguration
    colors::ColorsConfiguration
end

function ColorsConfigurationFields(colors::ColorsConfiguration)::ColorsConfigurationFields
    return ColorsConfigurationFields(colors.scale, colors)
end

"""
    struct SizesConfigurationFields <: AbstractConfigurationFields
        scale::ScaleConfiguration
        sizes::SizesConfiguration
    end

The configuration half of a `SizesVectorFields` data source view (see [`VectorFields`](@ref)): the
[`SizesConfiguration`](@ref) the values are sized by, and its `scale`.
"""
struct SizesConfigurationFields <: AbstractConfigurationFields
    scale::ScaleConfiguration
    sizes::SizesConfiguration
end

function SizesConfigurationFields(sizes::SizesConfiguration)::SizesConfigurationFields
    return SizesConfigurationFields(sizes.scale, sizes)
end

"""
    struct MatrixConfigurationFields <: AbstractConfigurationFields
        scale::ScaleConfiguration
        colors::ColorsConfiguration
    end

The configuration half of a [`MatrixFields`](@ref) data source view: the [`ColorsConfiguration`](@ref) the entries are
colored by, and its `scale`.
"""
struct MatrixConfigurationFields <: AbstractConfigurationFields
    scale::ScaleConfiguration
    colors::ColorsConfiguration
end

function MatrixConfigurationFields(colors::ColorsConfiguration)::MatrixConfigurationFields
    return MatrixConfigurationFields(colors.scale, colors)
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

The data source view of the X coordinates of a graph (of its points). For one line of a multi-line graph, use the `x`
of its [`PartFields`](@ref).
"""
function x_axis_vector_fields end

"""
    y_axis_vector_fields(graph)::AxisVectorFields

The data source view of the Y coordinates of a graph (of its points). For one line of a multi-line graph, use the `y`
of its [`PartFields`](@ref).
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
    points_entities(graph)::VectorEntitiesData

The entities of the points of a graph, shared by all their roles. Use this to add a hover line, or to hide points with
a mask, without filling any role. For one line of a multi-line graph, use the `entities` of its [`PartFields`](@ref).
"""
function points_entities end

"""
    edges_entities(graph)::VectorEntitiesData

The entities of the edges of a graph, shared by all their roles.
"""
function edges_entities end

"""
    bars_entities(graph)::VectorEntitiesData

The entities of the bars of a graph, shared by all their roles. In a multiple series graph these are the bars shared by
all the series; for the bars of one series, use the `entities` of its [`PartFields`](@ref).
"""
function bars_entities end

"""
    distribution_entities(graph)::VectorEntitiesData

The entities of the points of the distribution of a graph. For one distribution of a multiple distributions graph, use
the `entities` of its [`PartFields`](@ref).
"""
function distribution_entities end

"""
    rows_entities(graph)::VectorEntitiesData

The entities of the rows of a graph (of a heatmap), shared by all their roles.
"""
function rows_entities end

"""
    columns_entities(graph)::VectorEntitiesData

The entities of the columns of a graph (of a heatmap), shared by all their roles.
"""
function columns_entities end

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
    add_series!(graph, [series::SeriesData = SeriesData()])::PartFields

Append a series to a graph (of series of bars) and return its view (see [`PartFields`](@ref)). Whatever the `series`
leaves at its defaults can be set later, through the view or directly.
"""
function add_series! end

"""
    add_line!(graph, [line::LineData = LineData()])::PartFields

Append a line to a graph (of lines) and return its view (see [`PartFields`](@ref)). Whatever the `line` leaves at its
defaults can be set later, through the view or directly.
"""
function add_line! end

"""
    add_distribution!(graph, [distribution::DistributionData = DistributionData()])::PartFields

Append a distribution to a graph (of distributions) and return its view (see [`PartFields`](@ref)). Whatever the
`distribution` leaves at its defaults can be set later, through the view or directly.
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

"""
A struct holding graph data: the values of a role, or the entities they belong to.
"""
DataLeaf = Union{VectorValuesData, VectorEntitiesData, MatrixValuesData, MatrixEntitiesData}

"""
A struct holding graph configuration: how a role is shown.
"""
ConfigurationLeaf = Union{AxisConfiguration, ScaleConfiguration, ColorsConfiguration, SizesConfiguration}

"""
A [`DataLeaf`](@ref) or a [`ConfigurationLeaf`](@ref).
"""
AnyLeaf = Union{DataLeaf, ConfigurationLeaf}

"""
Anything that may contain graph data: a view, or the data half of one. [`visit_data_sinks`](@ref) reaches the
[`DataLeaf`](@ref) structs inside.
"""
DataContainer = Union{VectorFields, MatrixFields, VectorDataFields, MatrixDataFields}

"""
Anything that may contain graph configuration: a view, or the configuration half of one.
[`visit_configuration_sinks`](@ref) reaches the [`ConfigurationLeaf`](@ref) structs inside.
"""
ConfigurationContainer = Union{VectorFields, MatrixFields, AbstractConfigurationFields}

"""
A [`DataContainer`](@ref) or a [`ConfigurationContainer`](@ref).
"""
AnyContainer = Union{DataContainer, ConfigurationContainer}

"""
One struct a data source writes data into: a [`DataContainer`](@ref) or a [`DataLeaf`](@ref).
"""
DataSink = Union{DataContainer, DataLeaf}

"""
One struct a data source writes configuration into: a [`ConfigurationContainer`](@ref) or a [`ConfigurationLeaf`](@ref).
"""
ConfigurationSink = Union{ConfigurationContainer, ConfigurationLeaf}

"""
Any one struct a data source writes into: an [`AnyContainer`](@ref) or an [`AnyLeaf`](@ref).
"""
AnySink = Union{AnyContainer, AnyLeaf}

"""
What a data source accepts: one [`AnySink`](@ref), or a tuple or vector of them. Passing several lets one set of data
feed several places in the graph. A data source writes only the sinks which hold the half it fills and ignores the
rest, so a mixed collection is fine and either half may match nothing at all.

The method of a data source which walks the sinks takes every `Sinks` but the leaves it writes:
`Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector}` when writing data, and the mirror when writing
configuration. It has a method per leaf it writes, and one explicit no-op method for the leaves it ignores, so a leaf it
says nothing about is a `MethodError`. Taking `Sinks` in the walking method would match such a leaf too, and walking it
calls the same method again, forever.
"""
Sinks = Union{AnySink, Tuple, AbstractVector}

"""
    visit_data_sinks(visitor::Function, sinks::Sinks)::Nothing

Walk the `sinks` down to the data structs they are built from, and call the `visitor` on each of them. This lets a data
source write a method per struct it fills, without a traversal of its own.

A struct is visited once however many sinks reach it. All the roles of an axis share its entities, and
[`add_hovers!`](@ref) appends, so without this a hover would be added once per sink which reaches the same entities.

A matrix is walked into its entries only. Its row and column entities belong to their axes and are sized by one axis
each, so a value per matrix entry can't be written to them; they are reached by naming them directly.
"""
function visit_data_sinks(visitor::Function, sinks::Sinks)::Nothing
    visited = Base.IdSet{Any}()
    for sink in data_sinks(sinks)
        visit_data_sink(visitor, sink, visited)
    end
    return nothing
end

function visit_data_sink(visitor::Function, fields::Union{VectorFields, MatrixFields}, visited::Base.IdSet)::Nothing
    if !is_visited(fields, visited)
        visit_data_sink(visitor, fields.data, visited)
    end
    return nothing
end

function visit_data_sink(
    visitor::Function,
    data_fields::Union{VectorDataFields, MatrixDataFields},
    visited::Base.IdSet,
)::Nothing
    if !is_visited(data_fields, visited)
        visit_data_sink(visitor, data_fields.values, visited)
        visit_data_sink(visitor, data_fields.entities, visited)
    end
    return nothing
end

function visit_data_sink(
    visitor::Function,
    values_or_entities::Union{VectorValuesData, VectorEntitiesData, MatrixValuesData, MatrixEntitiesData},
    visited::Base.IdSet,
)::Nothing
    if !is_visited(values_or_entities, visited)
        visitor(values_or_entities)
    end
    return nothing
end

"""
    visit_configuration_sinks(visitor::Function, sinks::Sinks)::Nothing

Walk the `sinks` down to the configuration structs they are built from, and call the `visitor` on each of them. The
mirror of [`visit_data_sinks`](@ref).
"""
function visit_configuration_sinks(visitor::Function, sinks::Sinks)::Nothing
    visited = Base.IdSet{Any}()
    for sink in configuration_sinks(sinks)
        visit_configuration_sink(visitor, sink, visited)
    end
    return nothing
end

function visit_configuration_sink(
    visitor::Function,
    fields::Union{VectorFields, MatrixFields},
    visited::Base.IdSet,
)::Nothing
    if !is_visited(fields, visited)
        visit_configuration_sink(visitor, fields.configuration, visited)
    end
    return nothing
end

function visit_configuration_sink(
    visitor::Function,
    configuration_fields::AxisConfigurationFields,
    visited::Base.IdSet,
)::Nothing
    if !is_visited(configuration_fields, visited)
        visit_configuration_sink(visitor, configuration_fields.axis, visited)
    end
    return nothing
end

function visit_configuration_sink(
    visitor::Function,
    configuration_fields::Union{ColorsConfigurationFields, MatrixConfigurationFields},
    visited::Base.IdSet,
)::Nothing
    if !is_visited(configuration_fields, visited)
        visit_configuration_sink(visitor, configuration_fields.colors, visited)
    end
    return nothing
end

function visit_configuration_sink(
    visitor::Function,
    configuration_fields::SizesConfigurationFields,
    visited::Base.IdSet,
)::Nothing
    if !is_visited(configuration_fields, visited)
        visit_configuration_sink(visitor, configuration_fields.sizes, visited)
    end
    return nothing
end

function visit_configuration_sink(
    visitor::Function,
    configuration::Union{AxisConfiguration, ColorsConfiguration, SizesConfiguration, ScaleConfiguration},
    visited::Base.IdSet,
)::Nothing
    if !is_visited(configuration, visited)
        visitor(configuration)
    end
    return nothing
end

# Which of the `sinks` each half is written into. Either may be empty. A collection is asserted rather than typed,
# because a literal vector of several kinds of sink is a `Vector{Any}`. The result is typed, so that a visitor is seen
# to be called only with the half it handles.
function data_sinks(sinks::Union{Tuple, AbstractVector})::Vector{DataSink}
    assert_sinks(sinks)
    return collect(DataSink, filter(sink -> sink isa DataSink, sinks))
end

function data_sinks(sink::AnySink)::Vector{DataSink}
    return data_sinks((sink,))
end

function configuration_sinks(sinks::Union{Tuple, AbstractVector})::Vector{ConfigurationSink}
    assert_sinks(sinks)
    return collect(ConfigurationSink, filter(sink -> sink isa ConfigurationSink, sinks))
end

function configuration_sinks(sink::AnySink)::Vector{ConfigurationSink}
    return configuration_sinks((sink,))
end

function assert_sinks(sinks::Union{Tuple, AbstractVector})::Nothing
    for sink in sinks
        @assert sink isa AnySink "not a graph data or configuration struct: $(typeof(sink))"
    end
    return nothing
end

# Whether the `sink` was already visited, adding it to the set if it wasn't. Identity is what matters here, since two
# distinct empty entities compare equal.
function is_visited(sink::AnySink, visited::Base.IdSet)::Bool
    if sink in visited
        return true
    end
    push!(visited, sink)
    return false
end

end  # module
