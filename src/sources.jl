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
"""
module Sources

export AxisConfigurationFields
export AxisFields
export ColorsConfigurationFields
export ColorsFields
export MatrixConfigurationFields
export MatrixDataFields
export MatrixFields
export SizesConfigurationFields
export SizesFields
export VectorDataFields
export VectorFields
export add_hovers!
export annotations_fields
export borders_colors_fields
export borders_sizes_fields
export colors_fields
export columns_annotations_fields
export distribution_values_fields
export distributions_values_fields
export edges_colors_fields
export edges_sizes_fields
export entries_fields
export points_colors_fields
export points_sizes_fields
export rows_annotations_fields
export series_values_fields
export values_fields
export x_fields
export y_fields

using ..Common
using ..Validations

import ..Validations.Maybe

"""
    add_hovers!(
        entities::Union{EntitiesData, MatrixEntitiesData},
        hovers::AbstractArray{<:AbstractString};
        [title::Maybe{AbstractString} = nothing]
    )::Nothing

Add a line to the hover of each of the `entities`: the `hovers` entry of the entity (a vector for [`EntitiesData`](@ref),
a matrix for [`MatrixEntitiesData`](@ref)), prefixed by the `title` (if any) as `title: hover`. The lines of several
calls are joined by `<br>`, in the order of the calls. All the `hovers` given to the same entities must be of the same
size.
"""
function add_hovers!(
    entities::Union{EntitiesData, MatrixEntitiesData},
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
        values::ValuesData
        entities::EntitiesData
    end

The data half of a data source view (see [`VectorFields`](@ref)): the [`ValuesData`](@ref) of one role of a graph (the
X coordinates of its points, their colors, ...) and the [`EntitiesData`](@ref) of the entities these values belong to.
Both are the graph's own objects, so writing into them changes the graph. Several roles of the same entities (say, the
X, Y, colors and sizes of points) share one `entities`, so hovers added through any of them are seen by all.
"""
struct VectorDataFields
    values::ValuesData
    entities::EntitiesData
end

"""
    struct MatrixDataFields
        values::MatrixData
        entities::MatrixEntitiesData
    end

The data half of a [`MatrixFields`](@ref) data source view: the [`MatrixData`](@ref) of the entries of a graph (a
heatmap) and the [`MatrixEntitiesData`](@ref) of its cells.
"""
struct MatrixDataFields
    values::MatrixData
    entities::MatrixEntitiesData
end

"""
    struct AxisConfigurationFields
        axis::AxisConfiguration
    end

The configuration half of an `AxisFields` data source view (see [`VectorFields`](@ref)): the
[`AxisConfiguration`](@ref) the values are shown along.
"""
struct AxisConfigurationFields
    axis::AxisConfiguration
end

"""
    struct ColorsConfigurationFields
        axis::AxisConfiguration
        colors::ColorsConfiguration
    end

The configuration half of a `ColorsFields` data source view (see [`VectorFields`](@ref)): the
[`ColorsConfiguration`](@ref) the values are colored by, and its `axis`.
"""
struct ColorsConfigurationFields
    axis::AxisConfiguration
    colors::ColorsConfiguration
end

function ColorsConfigurationFields(colors::ColorsConfiguration)::ColorsConfigurationFields
    return ColorsConfigurationFields(colors.axis, colors)
end

"""
    struct SizesConfigurationFields
        axis::AxisConfiguration
        sizes::SizesConfiguration
    end

The configuration half of a `SizesFields` data source view (see [`VectorFields`](@ref)): the
[`SizesConfiguration`](@ref) the values are sized by, and its `axis`.
"""
struct SizesConfigurationFields
    axis::AxisConfiguration
    sizes::SizesConfiguration
end

function SizesConfigurationFields(sizes::SizesConfiguration)::SizesConfigurationFields
    return SizesConfigurationFields(sizes.axis, sizes)
end

"""
    struct MatrixConfigurationFields
        axis::AxisConfiguration
        colors::ColorsConfiguration
    end

The configuration half of a [`MatrixFields`](@ref) data source view: the [`ColorsConfiguration`](@ref) the entries are
colored by, and its `axis`.
"""
struct MatrixConfigurationFields
    axis::AxisConfiguration
    colors::ColorsConfiguration
end

function MatrixConfigurationFields(colors::ColorsConfiguration)::MatrixConfigurationFields
    return MatrixConfigurationFields(colors.axis, colors)
end

"""
    struct VectorFields{Configuration}
        data::VectorDataFields
        configuration::Configuration
    end

    AxisFields = VectorFields{AxisConfigurationFields}
    ColorsFields = VectorFields{ColorsConfigurationFields}
    SizesFields = VectorFields{SizesConfigurationFields}

A data source view of one role of a graph whose entities are a vector: the `data` (a [`VectorDataFields`](@ref)) and
the `configuration` (whose `axis` is an [`AxisConfiguration`](@ref), whatever else it holds). A function writing into
such a view fills the role from some source of data, and works the same on the X coordinates of points, the values of
bars, the colors of either, and so on. The views are `AxisFields` for values shown along an axis, `ColorsFields` for
values shown as colors (see [`ColorsConfigurationFields`](@ref)) and `SizesFields` for values shown as sizes (see
[`SizesConfigurationFields`](@ref)). They are obtained from a graph by the accessor functions (`x_fields`,
`colors_fields`, ...), whose names follow the path of the values in the data of the graph.
"""
struct VectorFields{Configuration}
    data::VectorDataFields
    configuration::Configuration
end

AxisFields = VectorFields{AxisConfigurationFields}
ColorsFields = VectorFields{ColorsConfigurationFields}
SizesFields = VectorFields{SizesConfigurationFields}

function VectorFields(values::ValuesData, entities::EntitiesData, configuration::Any)::VectorFields
    return VectorFields(VectorDataFields(values, entities), configuration)
end

"""
    struct MatrixFields
        data::MatrixDataFields
        configuration::MatrixConfigurationFields
    end

The data source view of the entries of a graph whose entities are arranged in rows and columns (the entries of a
heatmap), shown as colors.
"""
struct MatrixFields
    data::MatrixDataFields
    configuration::MatrixConfigurationFields
end

function MatrixFields(values::MatrixData, entities::MatrixEntitiesData, colors::ColorsConfiguration)::MatrixFields
    return MatrixFields(MatrixDataFields(values, entities), MatrixConfigurationFields(colors))
end

"""
    x_fields(graph)::AxisFields
    x_fields(graph, index::Integer)::AxisFields

The data source view of the X coordinates of a graph (of its points; of the points of one of its lines, given the
`index` of the line).
"""
function x_fields end

"""
    y_fields(graph)::AxisFields
    y_fields(graph, index::Integer)::AxisFields

The data source view of the Y coordinates of a graph (of its points; of the points of one of its lines, given the
`index` of the line).
"""
function y_fields end

"""
    points_colors_fields(graph)::ColorsFields

The data source view of the colors of the points of a graph.
"""
function points_colors_fields end

"""
    points_sizes_fields(graph)::SizesFields

The data source view of the sizes of the points of a graph.
"""
function points_sizes_fields end

"""
    borders_colors_fields(graph)::ColorsFields

The data source view of the colors of the borders of the points of a graph. The borders share the entities of the
points.
"""
function borders_colors_fields end

"""
    borders_sizes_fields(graph)::SizesFields

The data source view of the sizes of the borders of the points of a graph. The borders share the entities of the points.
"""
function borders_sizes_fields end

"""
    edges_colors_fields(graph)::ColorsFields

The data source view of the colors of the edges of a graph.
"""
function edges_colors_fields end

"""
    edges_sizes_fields(graph)::SizesFields

The data source view of the sizes (widths) of the edges of a graph.
"""
function edges_sizes_fields end

"""
    values_fields(graph)::AxisFields

The data source view of the values of a graph (of its bars).
"""
function values_fields end

"""
    colors_fields(graph)::ColorsFields

The data source view of the colors of a graph (of its bars).
"""
function colors_fields end

"""
    series_values_fields(graph, index::Integer)::AxisFields

The data source view of the values of one series of a graph, given the `index` of the series.
"""
function series_values_fields end

"""
    annotations_fields(graph, index::Integer)::ColorsFields

The data source view of one annotation of a graph, given the `index` of the annotation. The annotation shares the
entities of the axis it annotates (the bars).
"""
function annotations_fields end

"""
    distribution_values_fields(graph)::AxisFields

The data source view of the values of the distribution of a graph.
"""
function distribution_values_fields end

"""
    distributions_values_fields(graph, index::Integer)::AxisFields

The data source view of the values of one distribution of a graph, given the `index` of the distribution.
"""
function distributions_values_fields end

"""
    entries_fields(graph)::MatrixFields

The data source view of the entries of a graph (of a heatmap).
"""
function entries_fields end

"""
    rows_annotations_fields(graph, index::Integer)::ColorsFields

The data source view of one annotation of the rows of a graph, given the `index` of the annotation. The annotation
shares the entities of the rows.
"""
function rows_annotations_fields end

"""
    columns_annotations_fields(graph, index::Integer)::ColorsFields

The data source view of one annotation of the columns of a graph, given the `index` of the annotation. The annotation
shares the entities of the columns.
"""
function columns_annotations_fields end

end  # module
