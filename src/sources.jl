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
export HeatmapSide
export MatrixDataLeaf
export MatrixDataSinks
export Sinks
export VectorDataLeaf
export VectorDataSinks
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
export columns_arrangement
export columns_entities
export columns_side
export distribution_entities
export edges_entities
export points_entities
export rows_arrangement
export rows_entities
export rows_side
export side_configuration
export side_data
export side_placement
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
export fill_annotations!
export fill_arrangement!
export fill_configuration!
export fill_entities!
export fill_placement!
export fill_side!
export put_matrix_data!
export put_matrix_names_data!
export put_vector_data!
export put_vector_mask_data!
export put_vector_names_data!
export put_vector_order_data!
export put_vector_tree_data!

using ..Common
using ..Validations

import Clustering.Hclust  # NOLINT
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
has a `scale::ScaleConfiguration` (and the colors or sizes configuration it is the scale of).
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
are no ticks here; the title of the values takes precedence over the `title` of the colors configuration.
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
the `configuration` (an [`AxisConfigurationFields`](@ref), [`ColorsConfigurationFields`](@ref) or
[`SizesConfigurationFields`](@ref)). A function writing into
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

"""
A [`VectorFields`](@ref) of values shown along an axis.
"""
AxisVectorFields = VectorFields{AxisConfigurationFields}

"""
A [`VectorFields`](@ref) of values shown as colors.
"""
ColorsVectorFields = VectorFields{ColorsConfigurationFields}

"""
A [`VectorFields`](@ref) of values shown as sizes.
"""
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
    rows_arrangement(graph)::ArrangementData

The arrangement of the rows of a graph (of a heatmap): the tree, groups and matrix they are arranged by.
"""
function rows_arrangement end

"""
    columns_arrangement(graph)::ArrangementData

The arrangement of the columns of a graph (of a heatmap): the tree, groups and matrix they are arranged by.
"""
function columns_arrangement end

"""
    rows_side(graph)::HeatmapSide

The rows side of a graph (of a heatmap): its data, configuration and placement (see [`HeatmapSide`](@ref)).
"""
function rows_side end

"""
    columns_side(graph)::HeatmapSide

The columns side of a graph (of a heatmap): its data, configuration and placement (see [`HeatmapSide`](@ref)).
"""
function columns_side end

"""
    side_data(side::HeatmapSide)::HeatmapSideData

The data of a `side` of a heatmap: the entities, the arrangement and the annotations of its entries.
"""
function side_data end

"""
    side_configuration(side::HeatmapSide)::HeatmapSideConfiguration

The configuration of a `side` of a heatmap.
"""
function side_configuration end

"""
    side_placement(side::HeatmapSide)::SidePlacement

The computed placement of a `side` of a heatmap: the final order of its entries, and the tree they were placed by, if
one was needed. Computing it once (see `heatmap_placement`) serves both sides.
"""
function side_placement end

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
    struct HeatmapSide
        graph::Graph
        is_rows::Bool
    end

One side (the rows or the columns) of a heatmap graph, as returned by [`rows_side`](@ref) and [`columns_side`](@ref).
It stands for the three things a side has: its data ([`side_data`](@ref)), its configuration
([`side_configuration`](@ref)) and its computed placement ([`side_placement`](@ref)).

As a sink, it reaches the entities and the arrangement of the side. As a source, it is what the fills copying one side
onto another take.
"""
struct HeatmapSide
    graph::Graph
    is_rows::Bool
end

"""
A struct holding graph data with a value per entity: the values of a role, the entities they belong to, or the
arrangement of the entities of a heatmap axis.
"""
VectorDataLeaf = Union{VectorValuesData, VectorEntitiesData, ArrangementData}

"""
A struct holding graph data with a value per row per column: the entries of a heatmap, or the cells they belong to.
"""
MatrixDataLeaf = Union{MatrixValuesData, MatrixEntitiesData}

"""
A struct holding graph data: a [`VectorDataLeaf`](@ref) or a [`MatrixDataLeaf`](@ref).
"""
DataLeaf = Union{VectorDataLeaf, MatrixDataLeaf}

"""
A struct holding graph configuration: how a role is shown.
"""
ConfigurationLeaf = Union{AxisConfiguration, ScaleConfiguration, ColorsConfiguration, SizesConfiguration}

"""
A [`DataLeaf`](@ref) or a [`ConfigurationLeaf`](@ref).
"""
AnyLeaf = Union{DataLeaf, ConfigurationLeaf}

"""
Anything that may contain graph data: a view, the data half of one, or a side of a heatmap. [`visit_data_sinks`](@ref)
reaches the [`DataLeaf`](@ref) structs inside.
"""
DataContainer = Union{VectorFields, MatrixFields, VectorDataFields, MatrixDataFields, HeatmapSide}

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
What a data source writing a value per entity accepts: every [`Sinks`](@ref) but a [`MatrixDataLeaf`](@ref), which has
no place for such a value.
"""
VectorDataSinks = Union{AnyContainer, ConfigurationLeaf, VectorDataLeaf, Tuple, AbstractVector}

"""
What a data source writing a value per row per column accepts: every [`Sinks`](@ref) but a [`VectorDataLeaf`](@ref),
which has no place for such a value.
"""
MatrixDataSinks = Union{AnyContainer, ConfigurationLeaf, MatrixDataLeaf, Tuple, AbstractVector}

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

function visit_data_sink(visitor::Function, side::HeatmapSide, visited::Base.IdSet)::Nothing
    if !is_visited(side, visited)
        data = side_data(side)
        visit_data_sink(visitor, data.entities, visited)
        visit_data_sink(visitor, data.arrangement, visited)
    end
    return nothing
end

function visit_data_sink(visitor::Function, leaf::DataLeaf, visited::Base.IdSet)::Nothing
    if !is_visited(leaf, visited)
        visitor(leaf)
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

"""
    put_vector_data!(
        sinks::VectorDataSinks,
        value_per_entry::Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}};
        title::Maybe{AbstractString} = nothing,
    )::Nothing

Put a `value_per_entry` into the `sinks`: as the values of a role, and as a hover line on the entities.

The `title` prefixes the hover line, as `title: value`. Nothing is configured here, because how to show a value depends
on what it is; a specific data source says that in its own configuration put.
"""
function put_vector_data!(
    sinks::Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector},
    value_per_entry::Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}};
    title::Maybe{AbstractString} = nothing,
)::Nothing
    visit_data_sinks(sinks) do sink
        return put_vector_data!(sink, value_per_entry; title)
    end
    return nothing
end

function put_vector_data!(
    values::VectorValuesData,
    value_per_entry::Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}};
    title::Maybe{AbstractString} = nothing,  # NOLINT
)::Nothing
    values.vector = value_per_entry
    return nothing
end

function put_vector_data!(
    entities::VectorEntitiesData,
    value_per_entry::Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}};
    title::Maybe{AbstractString} = nothing,
)::Nothing
    add_hovers!(entities, hover_strings(value_per_entry); title)
    return nothing
end

# The arrangement of a heatmap side is reached through its own views (the groups, the subgroups), not by a value of a
# role.
function put_vector_data!(
    ::ArrangementData,
    ::Union{AbstractVector{<:Real}, AbstractVector{<:AbstractString}};
    title::Maybe{AbstractString} = nothing,  # NOLINT
)::Nothing
    return nothing
end

"""
    put_vector_names_data!(sinks::VectorDataSinks, name_per_entry::AbstractVector{<:AbstractString})::Nothing

Name the entities of the `sinks` after the `name_per_entry`. Where the graph has room to label the entities, these
become their tick labels. They are also the first line of the hover of each entity.
"""
function put_vector_names_data!(
    sinks::Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector},
    name_per_entry::AbstractVector{<:AbstractString},
)::Nothing
    visit_data_sinks(sinks) do sink
        return put_vector_names_data!(sink, name_per_entry)
    end
    return nothing
end

function put_vector_names_data!(entities::VectorEntitiesData, name_per_entry::AbstractVector{<:AbstractString})::Nothing
    entities.names = name_per_entry
    return nothing
end

# A name identifies an entity, so it belongs to the entities rather than to any one role's values or to the arrangement.
function put_vector_names_data!(::Union{VectorValuesData, ArrangementData}, ::AbstractVector{<:AbstractString})::Nothing
    return nothing
end

"""
    put_vector_mask_data!(
        sinks::VectorDataSinks,
        is_shown_per_entry::Union{AbstractVector{Bool}, BitVector},
    )::Nothing

Hide the entities of the `sinks` which are not shown by the `is_shown_per_entry` mask. Hidden entities are still part
of the data, so they take part in whatever is computed from it, unless the relevant configuration says otherwise.
"""
function put_vector_mask_data!(
    sinks::Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector},
    is_shown_per_entry::Union{AbstractVector{Bool}, BitVector},
)::Nothing
    visit_data_sinks(sinks) do sink
        return put_vector_mask_data!(sink, is_shown_per_entry)
    end
    return nothing
end

function put_vector_mask_data!(
    entities::VectorEntitiesData,
    is_shown_per_entry::Union{AbstractVector{Bool}, BitVector},
)::Nothing
    entities.mask = is_shown_per_entry
    return nothing
end

# A mask hides an entity, so it belongs to the entities rather than to any one role's values or to the arrangement.
function put_vector_mask_data!(
    ::Union{VectorValuesData, ArrangementData},
    ::Union{AbstractVector{Bool}, BitVector},
)::Nothing
    return nothing
end

"""
    put_vector_order_data!(
        sinks::VectorDataSinks,
        order::AbstractVector{<:Integer},
    )::Nothing

Give the entities of the `sinks` the `order` (a permutation of their indices). What the order means depends on the graph;
for a heatmap side, see `HeatmapSideConfiguration`.
"""
function put_vector_order_data!(
    sinks::Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector},
    order::AbstractVector{<:Integer},
)::Nothing
    visit_data_sinks(sinks) do sink
        return put_vector_order_data!(sink, order)
    end
    return nothing
end

function put_vector_order_data!(entities::VectorEntitiesData, order::AbstractVector{<:Integer})::Nothing
    entities.order = order
    return nothing
end

# An order belongs to the entities rather than to any one role's values. It is not one of the other inputs to arranging
# a heatmap side, which are what the arrangement holds.
function put_vector_order_data!(::Union{VectorValuesData, ArrangementData}, ::AbstractVector{<:Integer})::Nothing
    return nothing
end

"""
    put_matrix_data!(
        sinks::MatrixDataSinks,
        value_per_row_per_column::AbstractMatrix{<:Real};
        title::Maybe{AbstractString} = nothing,
    )::Nothing

Put a `value_per_row_per_column` into the `sinks`: as the values of the entries, and as a hover line on each entry. The
matrix twin of [`put_vector_data!`](@ref).

The row and the column entities are not written here. They belong to their sides and are sized by one side each, so
they are named separately; see [`put_matrix_names_data!`](@ref).
"""
function put_matrix_data!(
    sinks::Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector},
    value_per_row_per_column::AbstractMatrix{<:Real};
    title::Maybe{AbstractString} = nothing,
)::Nothing
    visit_data_sinks(sinks) do sink
        return put_matrix_data!(sink, value_per_row_per_column; title)
    end
    return nothing
end

function put_matrix_data!(
    values::MatrixValuesData,
    value_per_row_per_column::AbstractMatrix{<:Real};
    title::Maybe{AbstractString} = nothing,  # NOLINT
)::Nothing
    values.matrix = value_per_row_per_column
    return nothing
end

function put_matrix_data!(
    entities::MatrixEntitiesData,
    value_per_row_per_column::AbstractMatrix{<:Real};
    title::Maybe{AbstractString} = nothing,
)::Nothing
    add_hovers!(entities, hover_strings(value_per_row_per_column); title)
    return nothing
end

"""
    put_matrix_names_data!(
        sinks::MatrixDataSinks,
        name_per_row::AbstractVector{<:AbstractString},
        name_per_column::AbstractVector{<:AbstractString},
    )::Nothing

Name the rows and the columns of the `sinks` after the `name_per_row` and the `name_per_column`. The matrix twin of
[`put_vector_names_data!`](@ref).

The two sides are named together here rather than through [`visit_data_sinks`](@ref), which doesn't walk into them
because they are sized by one side each while the entries are sized by both. A vector sink has no rows or columns to
name, so it is an error here.
"""
function put_matrix_names_data!(
    sinks::Union{Tuple, AbstractVector},
    name_per_row::AbstractVector{<:AbstractString},
    name_per_column::AbstractVector{<:AbstractString},
)::Nothing
    for sink in sinks
        put_matrix_names_data!(sink, name_per_row, name_per_column)
    end
    return nothing
end

function put_matrix_names_data!(
    fields::MatrixFields,
    name_per_row::AbstractVector{<:AbstractString},
    name_per_column::AbstractVector{<:AbstractString},
)::Nothing
    put_matrix_names_data!(fields.data, name_per_row, name_per_column)
    return nothing
end

function put_matrix_names_data!(
    data_fields::MatrixDataFields,
    name_per_row::AbstractVector{<:AbstractString},
    name_per_column::AbstractVector{<:AbstractString},
)::Nothing
    put_vector_names_data!(data_fields.rows_entities, name_per_row)
    put_vector_names_data!(data_fields.columns_entities, name_per_column)
    return nothing
end

# A configuration has no rows or columns to name, and the entries or cells of a matrix hold no entities of either.
function put_matrix_names_data!(
    ::Union{AbstractConfigurationFields, ConfigurationLeaf, MatrixDataLeaf},
    ::AbstractVector{<:AbstractString},
    ::AbstractVector{<:AbstractString},
)::Nothing
    return nothing
end

# A vector sink is admitted by `MatrixDataSinks` since it is a container, but it has no rows or columns to name.
function put_matrix_names_data!(
    sinks::Union{VectorFields, VectorDataFields, HeatmapSide},
    ::AbstractVector{<:AbstractString},
    ::AbstractVector{<:AbstractString},
)::Nothing
    return throw(ArgumentError("can't name the rows and columns of a vector sink: $(typeof(sinks))"))
end

"""
    put_vector_tree_data!(sinks::VectorDataSinks, hclust::Maybe{Hclust})::Nothing

Give the arrangement of the `sinks` (that is, of the sides of a heatmap among them) the `hclust` tree of its entries.
The twin of [`put_vector_order_data!`](@ref) for the tree.
"""
function put_vector_tree_data!(
    sinks::Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector},
    hclust::Maybe{Hclust},
)::Nothing
    visit_data_sinks(sinks) do sink
        return put_vector_tree_data!(sink, hclust)
    end
    reset_sides_placement!(sinks)
    return nothing
end

function put_vector_tree_data!(arrangement::ArrangementData, hclust::Maybe{Hclust})::Nothing
    arrangement.hclust = hclust
    return nothing
end

# A tree arranges the entries of a heatmap side, so it belongs to the arrangement rather than to the values of a role or
# to the entities.
function put_vector_tree_data!(::Union{VectorValuesData, VectorEntitiesData}, ::Maybe{Hclust})::Nothing
    return nothing
end

"""
    fill_entities!(sinks::VectorDataSinks, source::Union{HeatmapSide, VectorEntitiesData})::Nothing

Fill the entities of the `sinks` with a copy of the names, hovers, mask and order of the entities of the `source`.
"""
function fill_entities!(sinks::VectorDataSinks, side::HeatmapSide)::Nothing
    return fill_entities!(sinks, side_data(side).entities)
end

function fill_entities!(
    sinks::Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector},
    source::VectorEntitiesData,
)::Nothing
    visit_data_sinks(sinks) do sink
        return fill_entities!(sink, source)
    end
    reset_sides_placement!(sinks)
    return nothing
end

function fill_entities!(entities::VectorEntitiesData, source::VectorEntitiesData)::Nothing
    entities.names = copy_or_nothing(source.names)
    entities.hovers = copy_or_nothing(source.hovers)
    entities.mask = copy_or_nothing(source.mask)
    entities.order = copy_or_nothing(source.order)
    return nothing
end

# The entities are all that is filled.
function fill_entities!(::Union{VectorValuesData, ArrangementData}, ::VectorEntitiesData)::Nothing
    return nothing
end

"""
    fill_arrangement!(sinks::VectorDataSinks, source::Union{HeatmapSide, ArrangementData})::Nothing

Fill the arrangement of the `sinks` (that is, of the sides of a heatmap among them) with the tree, and a copy of the
groups, subgroups and `arrange_by` matrix, of the arrangement of the `source`.
"""
function fill_arrangement!(sinks::VectorDataSinks, side::HeatmapSide)::Nothing
    return fill_arrangement!(sinks, side_data(side).arrangement)
end

function fill_arrangement!(
    sinks::Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector},
    source::ArrangementData,
)::Nothing
    visit_data_sinks(sinks) do sink
        return fill_arrangement!(sink, source)
    end
    reset_sides_placement!(sinks)
    return nothing
end

function fill_arrangement!(arrangement::ArrangementData, source::ArrangementData)::Nothing
    arrangement.hclust = source.hclust
    arrangement.groups = deepcopy(source.groups)
    arrangement.subgroups = deepcopy(source.subgroups)
    arrangement.arrange_by = copy_or_nothing(source.arrange_by)
    return nothing
end

# The arrangement is all that is filled.
function fill_arrangement!(::Union{VectorValuesData, VectorEntitiesData}, ::ArrangementData)::Nothing
    return nothing
end

"""
    fill_annotations!(target::HeatmapSide, source::HeatmapSide)::Nothing

Fill the annotations of the `target` side with a copy of the annotations of the `source` side, and of the order they
are shown in.
"""
function fill_annotations!(target::HeatmapSide, source::HeatmapSide)::Nothing
    target_data = side_data(target)
    source_data = side_data(source)
    target_data.annotations = [deepcopy(annotation) for annotation in source_data.annotations]
    target_data.annotations_order = copy_or_nothing(source_data.annotations_order)
    return nothing
end

"""
    fill_configuration!(target::HeatmapSide, source::HeatmapSide)::Nothing

Fill the configuration of the `target` side with a copy of the configuration of the `source` side.
"""
function fill_configuration!(target::HeatmapSide, source::HeatmapSide)::Nothing
    target_configuration = side_configuration(target)
    source_configuration = side_configuration(source)
    for field in fieldnames(typeof(source_configuration))
        setfield!(target_configuration, field, deepcopy(getfield(source_configuration, field)))
    end
    reset_side_placement!(target)
    return nothing
end

"""
    fill_placement!(target::HeatmapSide, source::Union{HeatmapSide, SidePlacement})::Nothing

Fill the `target` side with the computed placement of the `source`: its final order into the `order` of the entities,
and its tree (if any) into the `hclust` of the arrangement. The `target` is then placed exactly as the `source` was,
without clustering or slanting anything.

Unlike the other fills, this does not copy an input of the graph. It copies what the inputs of the `source` computed,
and turns it into inputs of the `target`. The inputs of the `target` which computed its own placement then have no
effect, and giving an input with no effect is an error (see `HeatmapSideConfiguration`). So this also clears them:

  - The `tree_source`, `order_source`, `linkage` and `metric` of the configuration of the `target`. The sources are
    inferred from the given order and tree instead.
  - The `arrange_by` matrix of the arrangement of the `target`, which nothing is clustered by.
  - The `groups` of the arrangement of the `target` if its configuration has no `groups_gap`, and likewise the
    `subgroups` if it has no `subgroups_gap`. Groups which are drawn as gaps are kept.

Since what is cleared depends on the configuration of the `target`, fill it first when filling both (as
[`fill_side!`](@ref) does).
"""
function fill_placement!(target::HeatmapSide, side::HeatmapSide)::Nothing
    return fill_placement!(target, side_placement(side))
end

function fill_placement!(target::HeatmapSide, placement::SidePlacement)::Nothing
    data = side_data(target)
    data.entities.order = copy(placement.order)

    arrangement = data.arrangement
    arrangement.hclust = placement.hclust
    arrangement.arrange_by = nothing

    configuration = side_configuration(target)
    configuration.tree_source = nothing
    configuration.order_source = nothing
    configuration.linkage = nothing
    configuration.metric = nothing
    if configuration.groups_gap === nothing
        arrangement.groups.vector = nothing
    end
    if configuration.subgroups_gap === nothing
        arrangement.subgroups.vector = nothing
    end

    reset_side_placement!(target)
    return nothing
end

"""
    fill_side!(target::HeatmapSide, source::HeatmapSide)::Nothing

Fill the `target` side with a copy of everything about the `source` side: its entities, arrangement, annotations and
configuration, then its computed placement. This lays out the `target` exactly as the `source`, even when the data of
the two graphs differ.

The placement comes last because [`fill_placement!`](@ref) is not a plain copy. It turns the computed placement of the
`source` into inputs of the `target`, and clears the copied inputs which then have no effect.
"""
function fill_side!(target::HeatmapSide, source::HeatmapSide)::Nothing
    # The placement is taken before the copies, which reset the cached placement of the graph of the `target`. That may
    # be the graph of the `source`, when copying one side of a graph onto the other.
    placement = side_placement(source)
    fill_entities!(target, source)
    fill_arrangement!(target, source)
    fill_annotations!(target, source)
    fill_configuration!(target, source)
    fill_placement!(target, placement)
    return nothing
end

# A copy of the `values`, if any, so the filled graph does not share them with the source.
function copy_or_nothing(values::Maybe{AbstractArray})::Maybe{AbstractArray}
    return values === nothing ? nothing : copy(values)
end

# The sides of a heatmap among the `sinks`, whose placement is recomputed from their data on next use.
function reset_sides_placement!(sinks::Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector})::Nothing
    for sink in (sinks isa Union{Tuple, AbstractVector} ? sinks : (sinks,))
        if sink isa HeatmapSide
            reset_side_placement!(sink)
        end
    end
    return nothing
end

# Forget the cached placement of the graph of a heatmap `side`.
function reset_side_placement! end

# The values as the strings a hover shows. Only strings can be a hover, so anything else is converted.
function hover_strings(value_per_entry::AbstractArray{<:AbstractString})::AbstractArray{<:AbstractString}
    return value_per_entry
end

function hover_strings(value_per_entry::AbstractArray{<:Real})::AbstractArray{<:AbstractString}
    return string.(value_per_entry)
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
