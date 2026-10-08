"""
Graphs for showing a 2D matrix.
"""
module Heatmaps

export AverageLinkage
export ClusteredTree
export CompleteLinkage
export EntriesConfiguration
export EntryOrder
export GivenOrder
export GivenTree
export GivenTreeOrder
export heatmap_graph
export heatmap_placement
export HeatmapBottomLeft
export HeatmapBottomRight
export HeatmapGraph
export HeatmapGraphConfiguration
export HeatmapGraphData
export HeatmapGraphPlacement
export HeatmapLinkage
export HeatmapOrigin
export HeatmapSideConfiguration
export HeatmapSideData
export HeatmapTopLeft
export HeatmapTopRight
export OptimalTreeReorder
export OrderSource
export OrderTree
export RCompatibleTreeReorder
export reset_placement!
export SameOrder
export SameTree
export SingleLinkage
export SlantedOrder
export SlantedPreSquaredOrder
export TreeSource
export WardLinkage
export WardPreSquaredLinkage

using ..Common
using ..Utilities
using ..Validations

using Clustering
using Distances
using PlotlyBase
using Slanter

import ..Bars.push_annotations_traces!
import ..Bars.push_plotly_annotation!
import ..Bars.expand_vector
import ..Common.validate_entities_order
import ..Common.visit_graph_fields
import ..Validations.Maybe

"""
Specify where the tree of a heatmap side comes from.

  - `GivenTree` uses the `hclust` of the arrangement.
  - `ClusteredTree` clusters the data (using the `linkage` and `metric`, and the `groups` and `arrange_by` of the
    arrangement).
  - `OrderTree` builds a tree around the target order (using `ehclust`), so the leaves are exactly the target order.
  - `SameTree` uses the tree of the other side. This can only be applied to square matrices and can't be specified for
    both sides.

A tree is needed when any field depends on one: the dendogram is shown, a `tree_source` is specified, the
`order_source` is `GivenTreeOrder` or a tree-reorder, or an `hclust` is given. Otherwise no tree is built.
"""
@enum TreeSource GivenTree ClusteredTree OrderTree SameTree

"""
Specify where the order of a heatmap side comes from.

  - `GivenTreeOrder` uses the leaves of the given tree, as they are.
  - `OptimalTreeReorder` and `RCompatibleTreeReorder` are tree-reorders. They reorder the branches of the tree (using
    the (better) Bar-Joseph method, or in the same (bad) way that `R` does) and use its leaves.
  - `GivenOrder` uses the `order` of the entities.
  - `EntryOrder` uses the entries as they are.
  - `SlantedOrder` and `SlantedPreSquaredOrder` slant the data (using `slanted_orders`).
  - `SameOrder` uses the order of the other side. This can only be applied to square matrices and can't be specified for
    both sides.

The last five name a target order. When a tree is needed, the branches of a given, same or clustered tree are reordered
toward the target (using `reorder_hclust`), so the final order only approximates the target. An `OrderTree` is built
around the target, so the final order is the target.
"""
@enum OrderSource begin
    GivenTreeOrder
    OptimalTreeReorder
    RCompatibleTreeReorder
    GivenOrder
    EntryOrder
    SlantedOrder
    SlantedPreSquaredOrder
    SameOrder
end

"""
Specify the linkage to use when performing hierarchical clustering (`hclust` / `ehclust`). The default is `WardLinkage`.
"""
@enum HeatmapLinkage SingleLinkage AverageLinkage CompleteLinkage WardLinkage WardPreSquaredLinkage

"""
Specify where the origin (row 1 column 1) should be displayed. The Plotly default is `HeatmapBottomLeft`.
"""
@enum HeatmapOrigin HeatmapTopLeft HeatmapTopRight HeatmapBottomLeft HeatmapBottomRight

"""
    struct HeatmapGraphPlacement
        rows::SidePlacement
        columns::SidePlacement
    end

The computed [`SidePlacement`](@ref) of the rows and of the columns of a heatmap graph, as returned by
[`heatmap_placement`](@ref).
"""
struct HeatmapGraphPlacement
    rows::SidePlacement
    columns::SidePlacement
end

function Common.visit_graph_parts(visitor::Function, part::HeatmapGraphPlacement, visited::Base.IdSet)::Nothing
    visit_graph_fields(visitor, part, visited)
    return nothing
end

"""
    @kwdef mutable struct HeatmapSideConfiguration <: Validated
        title::Maybe{AbstractString} = nothing
        show_ticks::Bool = true
        ticks_angle::Maybe{Real} = nothing
        annotations::AnnotationSize = AnnotationSize()
        tree_source::Maybe{TreeSource} = nothing
        order_source::Maybe{OrderSource} = nothing
        linkage::Maybe{HeatmapLinkage} = nothing
        metric::Maybe{PreMetric} = nothing
        include_hidden::Bool = true
        groups_gap::Maybe{Integer} = 1
        subgroups_gap::Maybe{Integer} = nothing
        total_gaps_fraction::Maybe{Real} = 1 / 20
        dendogram_size::Maybe{Real} = nothing
        dendogram_line::LineConfiguration = LineConfiguration()
    end

Configure one side (the rows or the columns) of a heatmap. The `title` is shown on the axis of the side. The
`annotations` are the sizes of the annotations shown next to the entries.

The entries are labelled by their names from the [`HeatmapSideData`](@ref), if they have any. Set `show_ticks` to
`false` to leave them unlabelled, which is what a side holding thousands of entries wants; the names are still the
first line of each hover. By default the labels are shown horizontally, and `ticks_angle` rotates them (relative to the
axis, as in [`AxisConfiguration`](@ref)).

The layout of the side is an order, and optionally a tree for the dendogram. The `tree_source` says where the tree comes
from (see [`TreeSource`](@ref)) and the `order_source` says where the order comes from (see [`OrderSource`](@ref)). A
tree is needed when any field depends on one: the dendogram is shown, a `tree_source` is specified, the `order_source`
is `GivenTreeOrder` or a tree-reorder, or an `hclust` is given in the arrangement. Otherwise no tree is built, and
specifying `linkage` or `metric` is an error. Nothing that is specified is ever ignored.

"Toward the target" means the branches of the tree are reordered to be as close as possible to the target order, so the
final order is the leaves and only approximates the target. "Around the target" means the tree is built so that its
leaves are exactly the target.

| tree source   | `hclust`  | order source     | `order`      | final tree, when needed                                                              | final order               |
|:------------- |:--------- |:---------------- |:------------ |:------------------------------------------------------------------------------------ |:------------------------- |
| Given         | required  | `GivenTreeOrder` | forbidden    | given                                                                                | leaves, as is             |
| Given         | required  | tree-reorder     | forbidden    | INVALID: Clustering.jl has no public way to reorder the branches of an existing tree |                           |
| Given         | required  | target-order     | `GivenOrder` | given                                                                                | leaves, toward the target |
| Same          | forbidden | `GivenTreeOrder` | forbidden    | the other side's                                                                     | leaves, as is             |
| Same          | forbidden | tree-reorder     | forbidden    | INVALID, as for Given                                                                |                           |
| Same          | forbidden | target-order     | `GivenOrder` | the other side's                                                                     | leaves, toward the target |
| Clustered     | forbidden | tree-reorder     | forbidden    | clustered                                                                            | leaves, reordered         |
| Clustered     | forbidden | target-order     | `GivenOrder` | clustered                                                                            | leaves, toward the target |
| Order         | forbidden | target-order     | `GivenOrder` | around the target                                                                    | the target                |
| (none)        | forbidden | target-order     | `GivenOrder` |                                                                                      | the target                |
| anything else |           |                  |              | INVALID                                                                              |                           |

The `order` column says which order source requires the `order` data; the other sources forbid it. The invalid
remainder is `GivenTreeOrder` without a given or same tree, `OrderTree` with `GivenTreeOrder` or a tree-reorder, which
is circular, and a tree-reorder of a given or same tree, which is not implemented.

`SameOrder` is a plain target-order. With `SameTree` it copies the layout of the other side; with `OrderTree` it builds
this side's tree around the other's order; with `ClusteredTree` it clusters toward it.

Both sources default to `nothing`, which infers them from what is given:

| `order` given | `hclust` given | tree needed | tree source     | order source         |
|:------------- |:-------------- |:----------- |:--------------- |:-------------------- |
| no            | no             | no          | (none)          | `EntryOrder`         |
| no            | no             | yes         | `ClusteredTree` | `OptimalTreeReorder` |
| yes           | no             | no          | (none)          | `GivenOrder`         |
| yes           | no             | yes         | `OrderTree`     | `GivenOrder`         |
| no            | yes            | always      | `GivenTree`     | `GivenTreeOrder`     |
| yes           | yes            | always      | `GivenTree`     | `GivenOrder`         |

One source may be inferred while the other is explicit. An `order` with an explicit `ClusteredTree` infers `GivenOrder`
(cluster, then toward the given order). An explicit `SlantedOrder` with a dendogram infers `OrderTree`. An explicit
`SameOrder` with a tree needed and nothing given infers `SameTree`. An explicit `SameTree` infers `SameOrder`.

A `ClusteredTree` or an `OrderTree` is built using the `linkage` (by default, `WardLinkage`) and the `metric` (by
default, `Euclidean`). Neither applies to a `GivenTree` or a `SameTree`, so specifying them for one is an error.

By default, a computed clustering sees all the entries of the side, hidden ones included, so hiding some entries does
not move the rest. Set `include_hidden` to `false` to cluster the shown entries only. Either way the resulting order and
tree (see [`heatmap_placement`](@ref)) describe all the entries; when the hidden ones were left out of the clustering,
they come last, joined to the root of the tree. This has no effect on an `Hclust` given in the data, which is used as
is.

If groups are specified for the entries in the [`HeatmapSideData`](@ref), they can be used to constrain the clustering,
and/or to create visible gaps in the heatmap (between entries of different groups). The `groups_gap` is the number of
fake entries to add between the separated entries. That is, the default gap of 1 will add a blank gap of one entry
between adjacent entries of different groups. A gap of `nothing` will not be shown.

If subgroups are also specified, they are a second, finer level of grouping nested in the groups; each group is
contiguous, and within it each subgroup is contiguous. Their `subgroups_gap` works the same way, and defaults to
`nothing` because the usual reason to specify subgroups is to constrain the clustering rather than to show gaps. A
`subgroups_gap` requires subgroups.

Groups (or subgroups) which have no effect are rejected: if the side is not clustered, they must be shown with a gap.
Likewise, an `arrange_by` matrix (in the [`HeatmapSideData`](@ref)) must have an effect.

A gap of a fixed number of entries is invisible in a side holding thousands of them, and is most of a side holding a
few dozen. The `total_gaps_fraction` fixes this. The gaps are widened together, keeping their ratio, until they take
this fraction of the axis. They are never narrowed, so `groups_gap` and `subgroups_gap` are minimums. Set it to
`nothing` to use the gaps as given.

Each level is placed independently: a level specified by numbers is laid out in the order of these numbers, and a level
specified by names is laid out by the clustering. Numbering both levels therefore lays the entries out in the order of
their (group, subgroup) pair, and numbering just the groups keeps the groups in a fixed order while clustering the
subgroups inside each of them.

If you specify `dendogram_size`, the tree (see above) is shown to the side of the data. The size is specified in the
usual inconvenient units (fractions of the total graph size) because Plotly.

If a dendogram tree is shown, the `dendogram_line` can be used to control it. The default color is black. The
`is_filled` field shouldn't be set as it has no meaning here.
"""
@kwdef mutable struct HeatmapSideConfiguration <: Validated
    title::Maybe{AbstractString} = nothing
    show_ticks::Bool = true
    ticks_angle::Maybe{Real} = nothing
    annotations::AnnotationSize = AnnotationSize()
    tree_source::Maybe{TreeSource} = nothing
    order_source::Maybe{OrderSource} = nothing
    linkage::Maybe{HeatmapLinkage} = nothing
    metric::Maybe{PreMetric} = nothing
    include_hidden::Bool = true
    groups_gap::Maybe{Integer} = 1
    subgroups_gap::Maybe{Integer} = nothing
    total_gaps_fraction::Maybe{Real} = 1 / 20
    dendogram_size::Maybe{Real} = nothing
    dendogram_line::LineConfiguration = LineConfiguration()
end

function Common.visit_graph_parts(visitor::Function, part::HeatmapSideConfiguration, visited::Base.IdSet)::Nothing
    visit_graph_fields(visitor, part, visited)
    return nothing
end

function Validations.validate(context::ValidationContext, configuration::HeatmapSideConfiguration)::Nothing
    validate_field(context, "annotations", configuration.annotations)
    validate_field(context, "dendogram_line", configuration.dendogram_line)

    if configuration.ticks_angle !== nothing
        validate_in(context, "ticks_angle") do
            validate_is_finite(context, configuration.ticks_angle)
            validate_is_at_least(context, configuration.ticks_angle, -90)
            validate_is_at_most(context, configuration.ticks_angle, 90)
            return nothing
        end
    end

    validate_in(context, "groups_gap") do
        return validate_is_above(context, configuration.groups_gap, 0)
    end
    validate_in(context, "subgroups_gap") do
        return validate_is_above(context, configuration.subgroups_gap, 0)
    end
    validate_in(context, "total_gaps_fraction") do
        validate_is_above(context, configuration.total_gaps_fraction, 0)
        return validate_is_below(context, configuration.total_gaps_fraction, 1)
    end
    validate_in(context, "dendogram_size") do
        return validate_is_above(context, configuration.dendogram_size, 0)
    end

    dendogram_line = configuration.dendogram_line
    if dendogram_line.is_filled
        throw(ArgumentError("can't specify heatmap $(location(context)).dendogram_line.is_filled"))
    end

    if configuration.dendogram_size === nothing &&
       (dendogram_line.width !== nothing || dendogram_line.style != SolidLine || dendogram_line.color !== nothing)
        throw(ArgumentError(chomp("""
                                  can't specify heatmap $(location(context)).dendogram_line.*
                                  without $(location(context)).dendogram_size
                                  """)))
    end

    return nothing
end

"""
    @kwdef mutable struct EntriesConfiguration <: Validated
        colors::ColorsConfiguration = ColorsConfiguration()
    end

Configure the entries of a heatmap. The `colors` map the values of the entries to colors. Due to Plotly's limitations,
only continuous color palettes are supported, and a `fixed` color can't be specified.
"""
@kwdef mutable struct EntriesConfiguration <: Validated
    colors::ColorsConfiguration = ColorsConfiguration()
end

function Common.visit_graph_parts(visitor::Function, part::EntriesConfiguration, visited::Base.IdSet)::Nothing
    visit_graph_fields(visitor, part, visited)
    return nothing
end

function Validations.validate(context::ValidationContext, configuration::EntriesConfiguration)::Nothing
    validate_field(context, "colors", configuration.colors)

    if configuration.colors.fixed !== nothing
        throw(ArgumentError("can't specify heatmap $(location(context)).colors.fixed"))
    end

    if configuration.colors.palette isa CategoricalColors
        throw(ArgumentError("can't specify heatmap categorical $(location(context)).colors.palette"))
    end

    return nothing
end

"""
    @kwdef mutable struct HeatmapGraphConfiguration <: AbstractGraphConfiguration
        figure::FigureConfiguration = FigureConfiguration()
        entries::EntriesConfiguration = EntriesConfiguration()
        rows::HeatmapSideConfiguration = HeatmapSideConfiguration()
        columns::HeatmapSideConfiguration = HeatmapSideConfiguration()
        origin::HeatmapOrigin = HeatmapBottomLeft
        final_placement::Maybe{HeatmapGraphPlacement} = nothing
    end

Configure a graph showing a heatmap.

This displays a matrix of values using a rectangle at each position. Due to Plotly's limitations, you still to manually
tweak the graph size for best results; there's no way to directly control the width and height of the rectangles.

The `entries` configure the entries (see [`EntriesConfiguration`](@ref)); the `rows` and `columns` configure each side
(see [`HeatmapSideConfiguration`](@ref)).

The `final_placement` caches the computed placement of the rows and the columns; access it through the graph's
`placement` (e.g., for generating other graphs in an identical placement). It is computed once, whether the graph's
figure is generated or its placement is asked for first.

!!! note

    Nothing detects that the cache went stale. Call [`reset_placement!`](@ref) if anything it was computed from is
    changed after it was computed - that is, the `tree_source`, `order_source`, `linkage`, `metric`, `dendogram_size`
    and `include_hidden` of the sides configuration, and the `entries.matrix`, the `order` (and, when not
    `include_hidden`, the `mask`) of the sides entities and their `arrangement`. The groups are easy to forget: they
    constrain the clustering, so saving the same graph twice, grouped differently each time, silently reuses the
    placement of the first grouping unless the cache is reset in between.
"""
@kwdef mutable struct HeatmapGraphConfiguration <: AbstractGraphConfiguration
    figure::FigureConfiguration = FigureConfiguration()
    entries::EntriesConfiguration = EntriesConfiguration()
    rows::HeatmapSideConfiguration = HeatmapSideConfiguration()
    columns::HeatmapSideConfiguration = HeatmapSideConfiguration()
    origin::HeatmapOrigin = HeatmapBottomLeft
    final_placement::Maybe{HeatmapGraphPlacement} = nothing
end

function Common.visit_graph_parts(visitor::Function, part::HeatmapGraphConfiguration, visited::Base.IdSet)::Nothing
    visit_graph_fields(visitor, part, visited)
    return nothing
end

function Validations.validate(context::ValidationContext, configuration::HeatmapGraphConfiguration)::Nothing
    validate_field(context, "figure", configuration.figure)
    validate_field(context, "entries", configuration.entries)
    validate_field(context, "rows", configuration.rows)
    validate_field(context, "columns", configuration.columns)

    return nothing
end

"""
    @kwdef mutable struct HeatmapSideData
        entities::VectorEntitiesData = VectorEntitiesData()
        arrangement::ArrangementData = ArrangementData()
        annotations::AbstractVector{AnnotationData} = AnnotationData[]
        annotations_order::Maybe{AbstractVector{<:Integer}} = nothing
    end

The data of one side (the rows or the columns) of a [`HeatmapGraphData`](@ref). The `entities` hold the names, hovers,
mask and order of the entries; their names are shown as the tick labels, titled by the `title` of the side
configuration. The `arrangement` holds what else the entries are arranged by. The `annotations` are shown next to the
entries. If `annotations_order` is specified, they are shown in that order; it describes all the annotations,
including the ones that are not `is_shown`.

Hidden entries (see the mask of [`VectorEntitiesData`](@ref)) are not drawn, but they are still part of the data: the
clustering sees them (unless the `include_hidden` of the side configuration is `false`), and the order (a permutation or
a tree) always describes all the entries, hidden ones included. This way the order computed for one graph (see
[`heatmap_placement`](@ref)) can be given to another graph of the same data, whether or not the two hide the same
entries. At least one entry must be shown.
"""
@kwdef mutable struct HeatmapSideData
    entities::VectorEntitiesData = VectorEntitiesData()
    arrangement::ArrangementData = ArrangementData()
    annotations::AbstractVector{AnnotationData} = AnnotationData[]
    annotations_order::Maybe{AbstractVector{<:Integer}} = nothing
end

function Common.visit_graph_parts(visitor::Function, part::HeatmapSideData, visited::Base.IdSet)::Nothing
    visit_graph_fields(visitor, part, visited)
    return nothing
end

# Validate the data of the `name` (rows or columns) side of a heatmap with `n_entries`. The context is that of the whole
# graph data, so the messages can refer to the entries.
function Validations.validate(
    context::ValidationContext,
    side::HeatmapSideData,
    name::AbstractString,
    n_entries::Integer,
)::Nothing
    base = "entries.matrix.$(name)"

    validate_vector_length(context, "$(name).entities.names", side.entities.names, base, n_entries)
    validate_vector_length(context, "$(name).entities.hovers", side.entities.hovers, base, n_entries)
    validate_vector_length(context, "$(name).entities.mask", side.entities.mask, base, n_entries)
    if side.entities.mask !== nothing && !any(side.entities.mask)
        throw(ArgumentError("all entries hidden by $(location(context)).$(name).entities.mask"))
    end

    validate_entities_order(context, "$(name).entities", side.entities, base, n_entries; is_ordered = true)

    arrangement = side.arrangement
    hclust = arrangement.hclust
    if hclust !== nothing
        validate_vector_length(context, "$(name).arrangement.hclust.order", hclust.order, base, n_entries)
    end

    for (field, values_data) in (("groups", arrangement.groups), ("subgroups", arrangement.subgroups))
        validate_vector_length(context, "$(name).arrangement.$(field).vector", values_data.vector, base, n_entries)
        validate_vector_is_finite(context, "$(name).arrangement.$(field).vector", values_data.vector)
        if values_data.title !== nothing
            throw(ArgumentError("can't specify heatmap $(location(context)).$(name).arrangement.$(field).title"))
        end
    end

    # The subgroups of a side are a second, finer level of grouping, so they only make sense together with the groups.
    # A subgroup is nested in its group, so the same subgroup in two different groups is two different subgroups;
    # there's no need for the subgroups to be unique.
    if arrangement.subgroups.vector !== nothing && arrangement.groups.vector === nothing
        throw(
            ArgumentError(
                "can't specify heatmap $(location(context)).$(name).arrangement.subgroups.vector" *
                " without $(name).arrangement.groups.vector",
            ),
        )
    end

    validate_matrix_dimension(
        context,
        "$(name).arrangement.arrange_by",
        arrangement.arrange_by,
        name == "rows" ? 1 : 2,
        base,
        n_entries,
    )
    validate_matrix_is_finite(context, "$(name).arrangement.arrange_by", arrangement.arrange_by)

    validate_vector_length(
        context,
        "$(name).annotations_order",
        side.annotations_order,
        "$(name).annotations",
        length(side.annotations),
    )

    validate_vector_entries(context, "$(name).annotations", side.annotations) do _, annotation
        validate(context, annotation, base, n_entries)
        return nothing
    end

    return nothing
end

"""
    @kwdef mutable struct HeatmapGraphData <: AbstractGraphData
        figure_title::Maybe{AbstractString} = nothing
        entries::MatrixValuesData = MatrixValuesData()
        cells::MatrixEntitiesData = MatrixEntitiesData()
        rows::HeatmapSideData = HeatmapSideData()
        columns::HeatmapSideData = HeatmapSideData()
    end

The data for a graph showing a heatmap (matrix) of entries.

This is shown as a 2D image where each matrix entry is a small rectangle with some color. Due to Plotly limitation,
colors must be continuous. The `entries` values are required; their title is the title of the colors scale. The `cells`
hold the hovers of the entries. The hover for each rectangle is a combination of the hovers of the cell, of its row and
of its column.

The `rows` and `columns` hold the data of each side (see [`HeatmapSideData`](@ref)). A cell is shown if both its row
and its column are shown; there is no mask of its own. The values of the hidden cells still take part in the range of
the colors scale, unless `include_hidden` is disabled in the `entries.colors.scale` of the configuration.

The `order` of the entities of each side and the `hclust` of its arrangement (see [`ArrangementData`](@ref)) take part
in the layout of the side as described in [`HeatmapSideConfiguration`](@ref). The `groups` and `subgroups` of the
arrangement constrain a `ClusteredTree`. They can still be specified to denote gaps in the heatmap, even when they do
not impact the tree and/or order.
"""
@kwdef mutable struct HeatmapGraphData <: AbstractGraphData
    figure_title::Maybe{AbstractString} = nothing
    entries::MatrixValuesData = MatrixValuesData()
    cells::MatrixEntitiesData = MatrixEntitiesData()
    rows::HeatmapSideData = HeatmapSideData()
    columns::HeatmapSideData = HeatmapSideData()
end

function Common.visit_graph_parts(visitor::Function, part::HeatmapGraphData, visited::Base.IdSet)::Nothing
    visit_graph_fields(visitor, part, visited)
    return nothing
end

function Validations.validate(context::ValidationContext, data::HeatmapGraphData)::Nothing
    values = data.entries.matrix
    if values === nothing
        throw(ArgumentError("must specify $(location(context)).entries.matrix"))
    end
    n_rows, n_columns = size(values)

    validate_matrix_is_finite(context, "entries.matrix", values)
    validate_matrix_size(context, "cells.hovers", data.cells.hovers, "entries.matrix", size(values))

    validate(context, data.rows, "rows", n_rows)
    validate(context, data.columns, "columns", n_columns)

    return nothing
end

"""
A graph showing a heatmap. See [`HeatmapGraphData`](@ref) and [`HeatmapGraphConfiguration`](@ref).
"""
HeatmapGraph = Graph{HeatmapGraphData, HeatmapGraphConfiguration}

"""
    function heatmap_graph(;
        [figure_title::Maybe{AbstractString} = nothing,
        entries::MatrixValuesData = MatrixValuesData(),
        cells::MatrixEntitiesData = MatrixEntitiesData(),
        rows::HeatmapSideData = HeatmapSideData(),
        columns::HeatmapSideData = HeatmapSideData(),
        configuration::HeatmapGraphConfiguration = HeatmapGraphConfiguration()]
    )::HeatmapGraph

Create a [`HeatmapGraph`](@ref) by initializing only the [`HeatmapGraphData`](@ref) fields (with an optional
[`HeatmapGraphConfiguration`](@ref)).
"""
function heatmap_graph(;
    figure_title::Maybe{AbstractString} = nothing,
    entries::MatrixValuesData = MatrixValuesData(),
    cells::MatrixEntitiesData = MatrixEntitiesData(),
    rows::HeatmapSideData = HeatmapSideData(),
    columns::HeatmapSideData = HeatmapSideData(),
    configuration::HeatmapGraphConfiguration = HeatmapGraphConfiguration(),
)::HeatmapGraph
    return HeatmapGraph(HeatmapGraphData(; figure_title, entries, cells, rows, columns), configuration)
end

# The cells whose values take part in the colors range. A cell is shown if both its row and its column are shown; there
# is no mask of its own. This is `nothing` unless the range is restricted to the shown cells, so the matrix is only
# built when it can make a difference.
function shown_cells_mask(graph::HeatmapGraph)::Maybe{Union{AbstractMatrix{Bool}, BitMatrix}}
    if graph.configuration.entries.colors.scale.include_hidden
        return nothing
    end

    rows_mask = graph.data.rows.entities.mask
    columns_mask = graph.data.columns.entities.mask
    if rows_mask === nothing && columns_mask === nothing
        return nothing
    end

    n_rows, n_columns = size(entries_values(graph))
    return (rows_mask === nothing ? trues(n_rows) : rows_mask) .&
           transpose(columns_mask === nothing ? trues(n_columns) : columns_mask)
end

# The annotations of a side which are actually drawn, in the order they are drawn in.
function side_annotations(side::HeatmapSideData)::AbstractVector{AnnotationData}
    return displayed_annotations(side.annotations, side.annotations_order)
end

# The entries values of a validated heatmap graph.
function entries_values(graph::HeatmapGraph)::AbstractMatrix{<:Real}
    values = graph.data.entries.matrix
    @assert values !== nothing
    return values
end

# The resolved sources of the layout of a side. The tree source is `nothing` when no tree is needed.
struct SideSources
    tree_source::Maybe{TreeSource}
    order_source::OrderSource
end

# Whether an order source is a tree-reorder: the leaves of the tree after its branches are reordered.
function is_tree_reorder(order_source::OrderSource)::Bool
    return order_source in (OptimalTreeReorder, RCompatibleTreeReorder)
end

# Whether an order source is the leaves of the tree, so it needs a tree.
function is_tree_order(order_source::OrderSource)::Bool
    return order_source == GivenTreeOrder || is_tree_reorder(order_source)
end

# Whether an order source names a target order that is slanted.
function is_slanted_order(order_source::OrderSource)::Bool
    return order_source in (SlantedOrder, SlantedPreSquaredOrder)
end

# Resolve the sources of the layout of a side, inferring the unspecified ones from what is given as described in
# `HeatmapSideConfiguration`. This does not validate the result against the data; `validate_graph` does.
function side_sources(side_data::HeatmapSideData, side_configuration::HeatmapSideConfiguration)::SideSources
    is_order_given = side_data.entities.order !== nothing
    is_hclust_given = side_data.arrangement.hclust !== nothing
    tree_source = side_configuration.tree_source
    order_source = side_configuration.order_source

    if order_source === nothing
        if is_order_given
            order_source = GivenOrder
        elseif tree_source == SameTree
            order_source = SameOrder
        elseif tree_source == GivenTree || is_hclust_given
            order_source = GivenTreeOrder
        elseif tree_source == ClusteredTree ||
               (tree_source != OrderTree && side_configuration.dendogram_size !== nothing)
            order_source = OptimalTreeReorder
        else
            order_source = EntryOrder
        end
    end

    if tree_source === nothing &&
       (side_configuration.dendogram_size !== nothing || is_hclust_given || is_tree_order(order_source))
        if is_hclust_given
            tree_source = GivenTree
        elseif order_source == SameOrder
            tree_source = SameTree
        elseif is_tree_order(order_source)
            tree_source = ClusteredTree
        else
            tree_source = OrderTree
        end
    end

    return SideSources(tree_source, order_source)
end

function Common.validate_graph(graph::HeatmapGraph)::Nothing
    values = entries_values(graph)

    validate_colors(
        ValidationContext(["graph.data.entries.matrix"]),
        values,
        ValidationContext(["graph.configuration.entries.colors"]),
        graph.configuration.entries.colors,
    )

    validate_axis_sizes(;
        axis_name = "columns",
        annotation_size = graph.configuration.columns.annotations,
        n_annotations = length(side_annotations(graph.data.columns)),
        dendogram_size = graph.configuration.rows.dendogram_size,
    )

    validate_axis_sizes(;
        axis_name = "rows",
        annotation_size = graph.configuration.rows.annotations,
        n_annotations = length(side_annotations(graph.data.rows)),
        dendogram_size = graph.configuration.columns.dendogram_size,
    )

    rows_sources = side_sources(graph.data.rows, graph.configuration.rows)
    columns_sources = side_sources(graph.data.columns, graph.configuration.columns)

    rows_same_source = same_source(graph.configuration.rows)
    columns_same_source = same_source(graph.configuration.columns)
    if rows_same_source !== nothing && columns_same_source !== nothing
        throw(ArgumentError(chomp("""
                                  can't specify both heatmap graph.configuration.rows.$(rows_same_source)
                                  and heatmap graph.configuration.columns.$(columns_same_source)
                                  """)))
    end

    n_rows, n_columns = size(values)
    if n_rows != n_columns
        for (name, side_same_source) in (("rows", rows_same_source), ("columns", columns_same_source))
            if side_same_source !== nothing
                throw(ArgumentError(chomp("""
                                          can't specify heatmap graph.configuration.$(name).$(side_same_source)
                                          for a non-square matrix: $(n_rows) rows x $(n_columns) columns
                                          """)))
            end
        end
    end

    validate_side_sources("rows", graph.data.rows, graph.configuration.rows, rows_sources, "columns", columns_sources)
    validate_side_sources(
        "columns",
        graph.data.columns,
        graph.configuration.columns,
        columns_sources,
        "rows",
        rows_sources,
    )

    return nothing
end

# The specified source of a side which copies from the other side, as `field: value` for error messages, if any. An
# inferred `SameOrder` or `SameTree` always comes with a specified one.
function same_source(side_configuration::HeatmapSideConfiguration)::Maybe{String}
    if side_configuration.order_source == SameOrder
        return "order_source: SameOrder"
    elseif side_configuration.tree_source == SameTree
        return "tree_source: SameTree"
    else
        return nothing
    end
end

# Validate the layout data and configuration of a side against its resolved sources, per the table in
# `HeatmapSideConfiguration`.
function validate_side_sources(
    name::AbstractString,
    side_data::HeatmapSideData,
    side_configuration::HeatmapSideConfiguration,
    sources::SideSources,
    other_name::AbstractString,
    other_sources::SideSources,
)::Nothing
    tree_source = sources.tree_source
    order_source = sources.order_source

    is_tree_built = tree_source in (ClusteredTree, OrderTree)
    for (field, value) in (("linkage", side_configuration.linkage), ("metric", side_configuration.metric))
        if value !== nothing && !is_tree_built
            throw(ArgumentError(chomp("""
                                      can't specify heatmap graph.configuration.$(name).$(field)
                                      without graph.configuration.$(name).tree_source: ClusteredTree or OrderTree
                                      """)))
        end
    end

    if tree_source == GivenTree
        if side_data.arrangement.hclust === nothing
            throw(ArgumentError(chomp("""
                                      must specify heatmap graph.data.$(name).arrangement.hclust
                                      for graph.configuration.$(name).tree_source: GivenTree
                                      """)))
        end
    elseif side_data.arrangement.hclust !== nothing
        throw(ArgumentError(chomp("""
                                  can't specify heatmap graph.data.$(name).arrangement.hclust
                                  for graph.configuration.$(name).tree_source: $(tree_source)
                                  """)))
    end

    if order_source == GivenOrder
        if side_data.entities.order === nothing
            throw(ArgumentError(chomp("""
                                      must specify heatmap graph.data.$(name).entities.order
                                      for graph.configuration.$(name).order_source: GivenOrder
                                      """)))
        end
    elseif side_data.entities.order !== nothing
        throw(ArgumentError(chomp("""
                                  can't specify heatmap graph.data.$(name).entities.order
                                  for graph.configuration.$(name).order_source: $(order_source)
                                  """)))
    end

    if order_source == GivenTreeOrder && !(tree_source in (GivenTree, SameTree))
        throw(ArgumentError(chomp("""
                                  can't specify heatmap graph.configuration.$(name).order_source: GivenTreeOrder
                                  for graph.configuration.$(name).tree_source: $(tree_source)
                                  """)))
    end

    if is_tree_reorder(order_source) && tree_source != ClusteredTree
        throw(ArgumentError(chomp("""
                                  can't specify heatmap graph.configuration.$(name).order_source: $(order_source)
                                  for graph.configuration.$(name).tree_source: $(tree_source)
                                  """)))
    end

    if tree_source == SameTree && other_sources.tree_source === nothing
        throw(ArgumentError(chomp("""
                                  can't specify heatmap graph.configuration.$(name).tree_source: SameTree
                                  without a tree for the $(other_name)
                                  """)))
    end

    if side_data.arrangement.arrange_by !== nothing && !is_tree_built && !is_slanted_order(order_source)
        throw(ArgumentError("no effect for specified graph.data.$(name).arrangement.arrange_by"))
    end

    is_clustered = tree_source == ClusteredTree
    if !is_clustered && side_configuration.groups_gap === nothing && side_data.arrangement.groups.vector !== nothing
        throw(ArgumentError("no effect for specified graph.data.$(name).arrangement.groups.vector"))
    end

    ## Unlike the groups, the subgroups have their own gap, so they are of use if either the side is clustered (they
    ## constrain the clustering) or they are gapped.
    if !is_clustered &&
       side_configuration.subgroups_gap === nothing &&
       side_data.arrangement.subgroups.vector !== nothing
        throw(ArgumentError("no effect for specified graph.data.$(name).arrangement.subgroups.vector"))
    end

    if side_configuration.subgroups_gap !== nothing && side_data.arrangement.subgroups.vector === nothing
        throw(ArgumentError(chomp("""
                                  can't specify heatmap graph.configuration.$(name).subgroups_gap
                                  without graph.data.$(name).arrangement.subgroups.vector
                                  """)))
    end

    return nothing
end

function Common.graph_to_figure(graph::HeatmapGraph)::PlotlyFigure
    validate(ValidationContext(["graph"]), graph)

    traces = Vector{GenericTrace}()

    next_colors_scale_index = [1]
    colors = configured_colors(;
        colors_configuration = graph.configuration.entries.colors,
        colors_title = prefer_data(graph.data.entries.title, graph.configuration.entries.colors.title),
        colors_values = entries_values(graph),
        next_colors_scale_index,
        mask = shown_cells_mask(graph),
    )

    placement = heatmap_placement(graph)

    # The order is that of the data; the `origin` decides which end of each axis the first entry is shown at.
    rows_mask = graph.data.rows.entities.mask
    columns_mask = graph.data.columns.entities.mask
    rows_order = displayed_order(
        placement.rows.order,
        rows_mask,
        graph.configuration.origin in (HeatmapTopLeft, HeatmapTopRight),
    )
    columns_order = displayed_order(
        placement.columns.order,
        columns_mask,
        graph.configuration.origin in (HeatmapBottomRight, HeatmapTopRight),
    )

    reordered_values = colors.final_colors_values[rows_order, columns_order]

    rows_annotations_data = side_annotations(graph.data.rows)
    columns_annotations_data = side_annotations(graph.data.columns)
    n_rows_annotations = length(rows_annotations_data)
    n_columns_annotations = length(columns_annotations_data)

    columns_sub_graph = SubGraph(;
        index = 1,
        n_graphs = 1,
        graphs_gap = nothing,
        n_annotations = n_rows_annotations,
        annotation_size = graph.configuration.rows.annotations,
        dendogram_size = graph.configuration.rows.dendogram_size,
    )

    rows_sub_graph = SubGraph(;
        index = 1,
        n_graphs = 1,
        graphs_gap = nothing,
        n_annotations = n_columns_annotations,
        annotation_size = graph.configuration.columns.annotations,
        dendogram_size = graph.configuration.columns.dendogram_size,
    )

    xaxis_index, _, yaxis_index, _ = plotly_sub_graph_axes(;
        basis_sub_graph = columns_sub_graph,
        values_sub_graph = rows_sub_graph,
        values_orientation = VerticalValues,
    )

    expanded_rows_mask = compute_expansion_mask(
        rows_order,
        graph.data.rows.arrangement.groups.vector,
        graph.data.rows.arrangement.subgroups.vector,
        graph.configuration.rows,
    )
    expanded_columns_mask = compute_expansion_mask(
        columns_order,
        graph.data.columns.arrangement.groups.vector,
        graph.data.columns.arrangement.subgroups.vector,
        graph.configuration.columns,
    )

    expanded_z = expand_z_matrix(reordered_values, rows_order, expanded_rows_mask, columns_order, expanded_columns_mask)

    n_expanded_rows, n_expanded_columns = size(expanded_z)

    rows_hovers = entities_hovers(graph.data.rows.entities)
    if rows_hovers !== nothing
        rows_hovers = rows_hovers[rows_order]
    end

    columns_hovers = entities_hovers(graph.data.columns.entities)
    if columns_hovers !== nothing
        columns_hovers = columns_hovers[columns_order]
    end

    entries_hovers = graph.data.cells.hovers
    if entries_hovers !== nothing
        entries_hovers = entries_hovers[rows_order, columns_order]
    end

    hovers = expand_hovers(;
        n_expanded_rows,
        n_expanded_columns,
        expanded_rows_mask,
        expanded_columns_mask,
        rows_hovers,
        columns_hovers,
        entries_hovers,
    )
    if hovers !== nothing
        hovers = permutedims(hovers)
    end

    push!(
        traces,
        heatmap(;
            name = "",
            x = collect(1:n_expanded_columns),
            y = collect(1:n_expanded_rows),
            z = expanded_z,
            xaxis = plotly_axis("x", xaxis_index; short = true),
            yaxis = plotly_axis("y", yaxis_index; short = true),
            text = hovers,
            coloraxis = plotly_axis("color", 1),
        ),
    )

    has_legend_only_traces = [false]

    columns_annotations_colors = push_annotations_traces!(;
        traces,
        names = nothing,
        basis_sub_graph = columns_sub_graph,
        show_grid = true,
        values_orientation = VerticalValues,
        next_colors_scale_index,
        has_legend_only_traces,
        annotations_data = columns_annotations_data,
        annotation_size = graph.configuration.columns.annotations,
        entries_hovers = entities_hovers(graph.data.columns.entities),
        mask = columns_mask,
        order = columns_order,
        expanded_mask = expanded_columns_mask,
    )

    rows_annotations_colors = push_annotations_traces!(;
        traces,
        names = nothing,
        basis_sub_graph = rows_sub_graph,
        show_grid = true,
        values_orientation = HorizontalValues,
        next_colors_scale_index,
        has_legend_only_traces,
        annotations_data = rows_annotations_data,
        annotation_size = graph.configuration.rows.annotations,
        entries_hovers = entities_hovers(graph.data.rows.entities),
        mask = rows_mask,
        order = rows_order,
        expanded_mask = expanded_rows_mask,
    )

    if graph.configuration.rows.dendogram_size !== nothing
        rows_max_height = push_dendogram_trace!(;
            traces,
            clusters = displayed_hclust(placement.rows.hclust, rows_mask),
            values_orientation = HorizontalValues,
            dendogram_line = graph.configuration.rows.dendogram_line,
            expanded_mask = expanded_rows_mask,
            basis_sub_graph = rows_sub_graph,
            values_sub_graph = SubGraph(;
                index = 0,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_rows_annotations,
                annotation_size = graph.configuration.rows.annotations,
                dendogram_size = graph.configuration.rows.dendogram_size,
            ),
        )
    else
        rows_max_height = 0
    end

    if graph.configuration.columns.dendogram_size !== nothing
        columns_max_height = push_dendogram_trace!(;
            traces,
            clusters = displayed_hclust(placement.columns.hclust, columns_mask),
            values_orientation = VerticalValues,
            dendogram_line = graph.configuration.columns.dendogram_line,
            expanded_mask = expanded_columns_mask,
            basis_sub_graph = columns_sub_graph,
            values_sub_graph = SubGraph(;
                index = 0,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_columns_annotations,
                annotation_size = graph.configuration.columns.annotations,
                dendogram_size = graph.configuration.columns.dendogram_size,
            ),
        )
    else
        columns_max_height = 0
    end

    has_legend =
        (
            n_rows_annotations > 0 &&
            any([annotation_colors.show_in_legend for annotation_colors in rows_annotations_colors])
        ) || (
            n_columns_annotations > 0 &&
            any([annotation_colors.show_in_legend for annotation_colors in columns_annotations_colors])
        )
    has_hovers =
        graph.data.cells.hovers !== nothing ||
        entities_hovers(graph.data.rows.entities) !== nothing ||
        entities_hovers(graph.data.columns.entities) !== nothing

    layout = plotly_layout(graph.configuration.figure; title = graph.data.figure_title, has_legend, has_hovers)

    rows_names = graph.data.rows.entities.names
    expanded_rows_names = expand_vector(rows_names, rows_order, expanded_rows_mask, "")
    set_layout_axis!(
        layout,
        plotly_axis("y", yaxis_index),
        AxisConfiguration(;
            show_grid = false,
            show_ticks = rows_names !== nothing && graph.configuration.rows.show_ticks,
            ticks_angle = graph.configuration.rows.ticks_angle,
        );
        title = graph.configuration.rows.title,
        ticks_values = expanded_rows_names === nothing ? nothing : collect(1:n_expanded_rows),
        ticks_labels = expanded_rows_names,
        range = Range(; minimum = 0.5, maximum = n_expanded_rows + 0.5),
        domain = plotly_sub_graph_domain(
            SubGraph(;
                index = 1,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_columns_annotations,
                annotation_size = graph.configuration.columns.annotations,
                dendogram_size = graph.configuration.columns.dendogram_size,
            ),
        ),
        is_zeroable = false,
    )

    columns_names = graph.data.columns.entities.names
    expanded_columns_names = expand_vector(columns_names, columns_order, expanded_columns_mask, "")
    set_layout_axis!(
        layout,
        plotly_axis("x", xaxis_index),
        AxisConfiguration(;
            show_grid = false,
            show_ticks = columns_names !== nothing && graph.configuration.columns.show_ticks,
            ticks_angle = graph.configuration.columns.ticks_angle,
        );
        title = graph.configuration.columns.title,
        ticks_values = expanded_columns_names === nothing ? nothing : collect(1:n_expanded_columns),
        ticks_labels = expanded_columns_names,
        range = Range(; minimum = 0.5, maximum = n_expanded_columns + 0.5),
        domain = plotly_sub_graph_domain(
            SubGraph(;
                index = 1,
                n_graphs = 1,
                graphs_gap = nothing,
                n_annotations = n_rows_annotations,
                annotation_size = graph.configuration.rows.annotations,
                dendogram_size = graph.configuration.rows.dendogram_size,
            ),
        ),
        is_zeroable = false,
    )

    next_colors_scale_offset_index = [Int(has_legend)]
    side_panels = SidePanel[]

    if colors !== nothing && colors.colors_scale_index !== nothing
        set_layout_colorscale!(;
            layout,
            traces,
            colors_scale_index = colors.colors_scale_index,
            colors_configuration = colors.colors_configuration,
            scaled_colors_palette = colors.scaled_colors_palette,
            range = colors.final_colors_range,
            title = colors.colors_title,
            show_scale = colors.show_scale,
            next_colors_scale_offset_index,
            colors_scale_offsets = graph.configuration.figure.colors_scale_offsets,
            side_panels,
        )
    end

    layout["annotations"] = plotly_annotations = []
    for (
        axis_letter,
        values_orientation,
        annotations_data,
        annotations_colors,
        annotation_size,
        dendogram_size,
        max_height,
    ) in (
        (
            "y",
            VerticalValues,
            columns_annotations_data,
            columns_annotations_colors,
            graph.configuration.columns.annotations,
            graph.configuration.columns.dendogram_size,
            columns_max_height,
        ),
        (
            "x",
            HorizontalValues,
            rows_annotations_data,
            rows_annotations_colors,
            graph.configuration.rows.annotations,
            graph.configuration.rows.dendogram_size,
            rows_max_height,
        ),
    )
        n_annotations = 0
        if annotations_colors !== nothing
            n_annotations = length(annotations_colors)
            for (annotation_index, annotation_colors) in enumerate(annotations_colors)
                annotation_data = annotations_data[annotation_index]
                sub_graph = SubGraph(;
                    index = -annotation_index,
                    n_graphs = 1,
                    graphs_gap = nothing,
                    n_annotations,
                    annotation_size,
                    dendogram_size,
                )
                push_plotly_annotation!(;
                    plotly_annotations,
                    values_sub_graph = sub_graph,
                    values_orientation,
                    title = prefer_data(annotation_data.values.title, annotation_data.colors.title),
                )
                set_layout_axis!(  # NOJET
                    layout,
                    plotly_axis(axis_letter, annotation_index),
                    AxisConfiguration(; show_grid = false, show_ticks = false);
                    range = Range(; minimum = 0, maximum = 1),
                    domain = plotly_sub_graph_domain(sub_graph),
                    is_tick_axis = false,
                    is_zeroable = false,
                )
                if annotation_colors.colors_scale_index !== nothing
                    set_layout_colorscale!(;
                        layout,
                        traces,
                        colors_scale_index = annotation_colors.colors_scale_index,
                        colors_configuration = annotation_data.colors,
                        scaled_colors_palette = annotation_colors.scaled_colors_palette,
                        range = nothing,
                        strip_range = annotation_colors.final_colors_range,
                        title = prefer_data(annotation_data.values.title, annotation_data.colors.title),
                        show_scale = annotation_colors.show_scale,
                        next_colors_scale_offset_index,
                        colors_scale_offsets = graph.configuration.figure.colors_scale_offsets,
                        side_panels,
                    )
                end
            end
        end

        if dendogram_size !== nothing
            set_layout_axis!(  # NOJET
                layout,
                plotly_axis(axis_letter, n_annotations + 1 + 1),
                AxisConfiguration(; show_grid = false, show_ticks = false);
                title = nothing,
                range = Range(0, max_height),
                domain = plotly_sub_graph_domain(
                    SubGraph(;
                        index = 0,
                        n_graphs = 1,
                        graphs_gap = nothing,
                        n_annotations,
                        annotation_size,
                        dendogram_size,
                    ),
                ),
                is_tick_axis = false,
                is_zeroable = false,
            )
        end
    end

    if has_legend_only_traces[1]
        layout["xaxis99"] = Dict(:domain => [0, 0.001], :showgrid => false, :showticklabels => false)
        layout["yaxis99"] = Dict(:domain => [0, 0.001], :showgrid => false, :showticklabels => false)
    end

    if n_rows_annotations > 0 || n_columns_annotations > 0
        layout["bargap"] = 0
    end

    place_side_panels!(; layout, figure_configuration = graph.configuration.figure, side_panels)

    return plotly_figure(traces, layout)
end

# Whether the clustering of a side leaves out its hidden entries. A given tree covers all of them, so it is used as is.
function is_clustering_shown(side_data::HeatmapSideData, side_configuration::HeatmapSideConfiguration)::Bool
    return !side_configuration.include_hidden &&
           side_data.entities.mask !== nothing &&
           side_data.arrangement.hclust === nothing
end

# The data of a side restricted to its shown entries, for clustering them alone: the shown entries of the (explicit)
# order, the groups and subgroups, and the matching dimension of the `arrange_by` matrix. Nothing else takes part in the
# clustering.
function shown_side_data(
    side::HeatmapSideData,
    mask::Union{AbstractVector{Bool}, BitVector},
    dimension::Integer,
)::HeatmapSideData
    @assert side.arrangement.hclust === nothing
    order = side.entities.order
    if order !== nothing
        shown_positions = cumsum(mask)
        order = [shown_positions[index] for index in order if mask[index]]
    end

    arrange_by = side.arrangement.arrange_by
    if arrange_by !== nothing
        arrange_by = dimension == 1 ? arrange_by[mask, :] : arrange_by[:, mask]
    end

    return HeatmapSideData(;
        entities = VectorEntitiesData(; order),
        arrangement = ArrangementData(;
            groups = VectorValuesData(; vector = masked_values(side.arrangement.groups.vector, mask, nothing)),
            subgroups = VectorValuesData(; vector = masked_values(side.arrangement.subgroups.vector, mask, nothing)),
            arrange_by,
        ),
    )
end

# The order of a side clustered without its hidden entries, extended to all the entries: the shown ones in their order,
# then the hidden ones.
function all_entries_order(
    order::AbstractVector{<:Integer},
    mask::Union{AbstractVector{Bool}, BitVector},
)::AbstractVector{<:Integer}
    return vcat(findall(mask)[order], findall(.!mask))
end

# The tree of a side clustered without its hidden entries, extended to all the entries: the leaves renumbered back to
# the entries they stand for, and each hidden entry joined to the root at the height of the tree, so the order of the
# tree is the order above.
function all_entries_hclust(clusters::Hclust, mask::Union{AbstractVector{Bool}, BitVector})::Hclust
    shown_indices = findall(mask)

    merges = copy(clusters.merges)
    is_leaf = merges .< 0
    merges[is_leaf] .= .-shown_indices[.-merges[is_leaf]]

    heights = copy(clusters.heights)
    top_height = isempty(heights) ? zero(eltype(heights)) : maximum(heights)
    root = isempty(heights) ? -shown_indices[1] : length(heights)
    for hidden_index in findall(.!mask)
        merges = vcat(merges, [root -hidden_index])
        push!(heights, top_height)
        root = length(heights)
    end

    return Hclust(merges, heights, all_entries_order(clusters.order, mask), clusters.linkage)
end

function compute_heatmap_placement(graph::HeatmapGraph)::HeatmapGraphPlacement
    rows_mask = is_clustering_shown(graph.data.rows, graph.configuration.rows) ? graph.data.rows.entities.mask : nothing
    columns_mask = if is_clustering_shown(graph.data.columns, graph.configuration.columns)
        graph.data.columns.entities.mask
    else
        nothing
    end

    # A side copying the order or tree of the other one is clustered (and completed) exactly as the side it copies from;
    # its own mask only matters when it is displayed.
    if same_source(graph.configuration.rows) !== nothing
        rows_mask = columns_mask
    elseif same_source(graph.configuration.columns) !== nothing
        columns_mask = rows_mask
    end

    if rows_mask === nothing && columns_mask === nothing
        return compute_clustered_placement(graph)
    end

    values = entries_values(graph)
    clustered_graph = HeatmapGraph(
        HeatmapGraphData(;
            entries = MatrixValuesData(
                values[rows_mask === nothing ? (:) : rows_mask, columns_mask === nothing ? (:) : columns_mask],
            ),
            rows = rows_mask === nothing ? graph.data.rows : shown_side_data(graph.data.rows, rows_mask, 1),
            columns = if columns_mask === nothing
                graph.data.columns
            else
                shown_side_data(graph.data.columns, columns_mask, 2)
            end,
        ),
        graph.configuration,
    )
    clustered_placement = compute_clustered_placement(clustered_graph)

    rows_order = clustered_placement.rows.order
    rows_hclust = clustered_placement.rows.hclust
    if rows_mask !== nothing
        rows_order = all_entries_order(rows_order, rows_mask)
        rows_hclust = rows_hclust === nothing ? nothing : all_entries_hclust(rows_hclust, rows_mask)
    end

    columns_order = clustered_placement.columns.order
    columns_hclust = clustered_placement.columns.hclust
    if columns_mask !== nothing
        columns_order = all_entries_order(columns_order, columns_mask)
        columns_hclust = columns_hclust === nothing ? nothing : all_entries_hclust(columns_hclust, columns_mask)
    end

    return HeatmapGraphPlacement(SidePlacement(rows_order, rows_hclust), SidePlacement(columns_order, columns_hclust))
end

# The placement of the entries of a graph, clustering all of them.
function compute_clustered_placement(graph::HeatmapGraph)::HeatmapGraphPlacement
    data_rows_arrange_by = prefer_data(graph.data.rows.arrangement.arrange_by, entries_values(graph))
    data_columns_arrange_by = prefer_data(graph.data.columns.arrangement.arrange_by, entries_values(graph))
    @assert data_rows_arrange_by !== nothing
    @assert data_columns_arrange_by !== nothing

    rows_sources = side_sources(graph.data.rows, graph.configuration.rows)
    columns_sources = side_sources(graph.data.columns, graph.configuration.columns)

    slant_rows = is_slanted_order(rows_sources.order_source)
    slant_columns = is_slanted_order(columns_sources.order_source)

    slant_rows_is_pre_squared = rows_sources.order_source == SlantedPreSquaredOrder
    slant_columns_is_pre_squared = columns_sources.order_source == SlantedPreSquaredOrder

    if slant_rows &&
       slant_columns &&
       slant_rows_is_pre_squared == slant_columns_is_pre_squared &&
       data_rows_arrange_by === data_columns_arrange_by
        slant_rows_order, slant_columns_order =
            slanted_orders(data_rows_arrange_by; squared_order = !slant_rows_is_pre_squared)
    else
        slant_rows_order = nothing
        slant_columns_order = nothing

        if slant_rows
            if columns_sources.order_source == SameOrder
                slant_rows_order, slant_columns_order =
                    slanted_orders(data_rows_arrange_by; same_order = true, squared_order = !slant_rows_is_pre_squared)
            else
                slant_rows_order, _ =
                    slanted_orders(data_rows_arrange_by; order_cols = false, squared_order = !slant_rows_is_pre_squared)
            end
        end

        if slant_columns
            if rows_sources.order_source == SameOrder
                slant_rows_order, slant_columns_order = slanted_orders(
                    data_columns_arrange_by;
                    same_order = true,
                    squared_order = !slant_columns_is_pre_squared,
                )
            else
                _, slant_columns_order = slanted_orders(
                    data_columns_arrange_by;
                    order_rows = false,
                    squared_order = !slant_columns_is_pre_squared,
                )
            end
        end
    end

    # The rows `arrange_by` is transposed so that, as for the columns, the distances are between its columns.
    data_rows_arrange_by = PermutedDimsArray(data_rows_arrange_by, (2, 1))

    # A side copying from the other one is finalized after it.
    if same_source(graph.configuration.rows) !== nothing
        columns_order, columns_hclust = finalize_order(
            graph.data.columns,
            graph.configuration.columns,
            columns_sources,
            data_columns_arrange_by,
            slant_columns_order,
            nothing,
            nothing,
        )
        rows_order, rows_hclust = finalize_order(
            graph.data.rows,
            graph.configuration.rows,
            rows_sources,
            data_rows_arrange_by,
            slant_rows_order,
            columns_order,
            columns_hclust,
        )
    else
        rows_order, rows_hclust = finalize_order(
            graph.data.rows,
            graph.configuration.rows,
            rows_sources,
            data_rows_arrange_by,
            slant_rows_order,
            nothing,
            nothing,
        )
        columns_order, columns_hclust = finalize_order(
            graph.data.columns,
            graph.configuration.columns,
            columns_sources,
            data_columns_arrange_by,
            slant_columns_order,
            rows_order,
            rows_hclust,
        )
    end

    return HeatmapGraphPlacement(SidePlacement(rows_order, rows_hclust), SidePlacement(columns_order, columns_hclust))
end

"""
    heatmap_placement(graph::HeatmapGraph)::HeatmapGraphPlacement

Return the [`HeatmapGraphPlacement`](@ref) of a heatmap `graph`, that is, the final order of its rows and columns and
the trees they were placed by, without rendering it.

You can just write `graph.placement` instead of `heatmap_placement(graph)`. Either way the placement is only computed
once; showing the graph will reuse it, and vice versa.

Use this to list the entries in the order they are shown:

```julia
ordered_rows_names = graph.data.rows.entities.names[graph.placement.rows.order]
```

Use it to show several graphs in the same order, so they can be compared. Cluster one of them, then give the rest its
order (and, if they use the same groups, they will also have the same gaps):

```julia
graph.configuration.columns.order_source = OptimalTreeReorder
other_graph.data.columns.entities.order = graph.placement.columns.order
```

If the graphs also show a dendogram, give them the tree instead of the order. This arranges them in the same order
*and* draws the same tree above each of them (this only makes sense if the graphs share the same columns, as the tree
refers to the original column indices):

```julia
graph.configuration.columns.dendogram_size = 0.1
other_graph.data.columns.arrangement.hclust = graph.placement.columns.hclust
other_graph.configuration.columns.dendogram_size = 0.1
```
"""
function heatmap_placement(graph::HeatmapGraph)::HeatmapGraphPlacement
    final_placement = graph.configuration.final_placement  # NOJET
    if final_placement === nothing
        graph.configuration.final_placement = final_placement = compute_heatmap_placement(graph)  # NOJET
    end
    return final_placement
end

"""
    reset_placement!(graph::HeatmapGraph)::Nothing

Forget the [`HeatmapGraphPlacement`](@ref) cached in the graph's `final_placement`, so that asking for the graph's
`placement` (or showing it) will compute it again. Call this after changing anything the placement was computed from.
"""
function reset_placement!(graph::HeatmapGraph)::Nothing
    graph.configuration.final_placement = nothing
    return nothing
end

# Only a heatmap has a computed placement, so only a heatmap has this property; any other graph will complain there's
# no such field. The property is propagated like that of any graph.
Base.@constprop :aggressive function Base.getproperty(graph::HeatmapGraph, property::Symbol)
    if property == :placement
        return heatmap_placement(graph)
    elseif property == :figure || property == :json
        return invoke(Base.getproperty, Tuple{Graph, Symbol}, graph, property)
    else
        return getfield(graph, property)
    end
end

# The entries of a side which are shown, in the order they are shown in: the order of the data without the hidden
# entries, reversed if the `origin` places the first entry at the far end of the axis.
function displayed_order(
    order::AbstractVector{<:Integer},
    mask::Maybe{Union{AbstractVector{Bool}, BitVector}},
    is_reversed::Bool,
)::AbstractVector{<:Integer}
    if mask !== nothing
        order = [index for index in order if mask[index]]
    end
    return is_reversed ? reverse(order) : order
end

# The tree of the shown entries of a side: the tree of the data with the hidden entries pruned out of it, and the
# remaining leaves renumbered to the positions of the shown entries. A merge left with a single subtree is dropped, and
# that subtree takes its place.
function displayed_hclust(clusters::Hclust, ::Nothing)::Hclust
    return clusters
end

function displayed_hclust(clusters::Hclust, mask::Union{AbstractVector{Bool}, BitVector})::Hclust
    shown_positions = cumsum(mask)
    n_merges = size(clusters.merges, 1)

    # The pruned node each merge maps to: a leaf (negative) or a merge (positive) of the pruned tree, or `nothing` if it
    # held only hidden leaves.
    pruned_node_per_merge = Vector{Maybe{Int}}(undef, n_merges)

    function pruned_node(node::Integer)::Maybe{Int}
        if node < 0
            return mask[-node] ? -shown_positions[-node] : nothing
        else
            return pruned_node_per_merge[node]
        end
    end

    pruned_merges = Int[]
    pruned_heights = eltype(clusters.heights)[]
    for merge_index in 1:n_merges
        left_node = pruned_node(clusters.merges[merge_index, 1])
        right_node = pruned_node(clusters.merges[merge_index, 2])
        if left_node === nothing
            pruned_node_per_merge[merge_index] = right_node
        elseif right_node === nothing
            pruned_node_per_merge[merge_index] = left_node
        else
            push!(pruned_merges, left_node, right_node)
            push!(pruned_heights, clusters.heights[merge_index])
            pruned_node_per_merge[merge_index] = length(pruned_heights)
        end
    end

    pruned_order = [shown_positions[index] for index in clusters.order if mask[index]]

    return Hclust(permutedims(reshape(pruned_merges, 2, :)), pruned_heights, pruned_order, clusters.linkage)
end

# The final order and tree of a side from its resolved sources: the target order, if the order source names one, then
# the tree, if one is needed, reordered toward the target unless built around it. The `same_order` and `same_hclust`
# are the final order and tree of the other side, for the sources copying from it. The `data_arrange_by` holds the
# entries of the side in its columns.
function finalize_order(
    side_data::HeatmapSideData,
    side_configuration::HeatmapSideConfiguration,
    sources::SideSources,
    data_arrange_by::AbstractMatrix{<:Real},
    slant_order::Maybe{AbstractVector{<:Integer}},
    same_order::Maybe{AbstractVector{<:Integer}},
    same_hclust::Maybe{Hclust},
)::Tuple{AbstractVector{<:Integer}, Maybe{Hclust}}
    tree_source = sources.tree_source
    order_source = sources.order_source

    if order_source == GivenOrder
        target_order = side_data.entities.order
    elseif order_source == EntryOrder
        target_order = collect(1:size(data_arrange_by, 2))
    elseif is_slanted_order(order_source)
        target_order = slant_order
    elseif order_source == SameOrder
        target_order = same_order
    else
        @assert is_tree_order(order_source)
        target_order = nothing
    end

    if tree_source === nothing
        @assert target_order !== nothing
        return (target_order, nothing)
    end

    if tree_source == GivenTree
        clusters = side_data.arrangement.hclust
    elseif tree_source == SameTree
        clusters = same_hclust
    else
        configuration_linkage = side_configuration.linkage
        if configuration_linkage === nothing
            configuration_linkage = WardLinkage
        end
        linkage = hclust_linkage(configuration_linkage)

        configuration_metric = side_configuration.metric
        if configuration_metric === nothing
            configuration_metric = Euclidean()
        end
        distances = pairwise(configuration_metric, data_arrange_by; dims = 2)
        if tree_source == OrderTree
            @assert target_order !== nothing
            clusters = ehclust(distances; order = target_order, linkage)
            return (target_order, clusters)
        end
        @assert tree_source == ClusteredTree
        clusters = ehclust(  # NOJET
            distances;
            linkage,
            groups = side_data.arrangement.groups.vector,
            subgroups = side_data.arrangement.subgroups.vector,
            branchorder = is_tree_reorder(order_source) ? hclust_branchorder(order_source) : nothing,
        )
    end
    @assert clusters !== nothing

    if target_order !== nothing
        clusters = reorder_hclust(clusters, target_order)
    end
    return (clusters.order, clusters)
end

function hclust_linkage(linkage::HeatmapLinkage)::Symbol
    if linkage == SingleLinkage
        return :single
    elseif linkage == AverageLinkage
        return :average
    elseif linkage == CompleteLinkage
        return :complete
    elseif linkage == WardLinkage
        return :ward
    elseif linkage == WardPreSquaredLinkage
        return :ward_presquared
    else
        @assert false
    end
end

function hclust_branchorder(order_source::OrderSource)::Symbol
    if order_source == RCompatibleTreeReorder
        return :r
    elseif order_source == OptimalTreeReorder
        return :optimal
    else
        @assert false
    end
end

function push_dendogram_trace!(;
    traces::Vector{GenericTrace},
    clusters::Hclust,
    values_orientation::ValuesOrientation,
    dendogram_line::LineConfiguration,
    expanded_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
    basis_sub_graph::SubGraph,
    values_sub_graph::SubGraph,
)::Real
    values, heights = dendogram_coordinates(clusters, expanded_mask)

    if values_orientation == VerticalValues
        xs = values
        ys = heights
    elseif values_orientation == HorizontalValues
        ys = values
        xs = heights
    else
        @assert false
    end

    xaxis_index, _, yaxis_index, _ = plotly_sub_graph_axes(; basis_sub_graph, values_sub_graph, values_orientation)

    push!(
        traces,
        scatter(;
            x = xs,
            y = ys,
            x0 = nothing,
            y0 = nothing,
            xaxis = plotly_axis("x", xaxis_index; short = true),
            yaxis = plotly_axis("y", yaxis_index; short = true),
            mode = "lines",
            name = "",
            line_width = dendogram_line.width,
            line_color = prefer_data(dendogram_line.color, "black"),
            line_dash = plotly_line_dash(prefer_data(dendogram_line.style, SolidLine)),
            showlegend = false,
        ),
    )

    return maximum(skipmissing(heights))
end

function dendogram_coordinates(
    clusters::Hclust,
    expanded_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
)::Tuple{AbstractVector{<:Union{AbstractFloat, Missing}}, AbstractVector{<:Union{AbstractFloat, Missing}}}
    # The separators between the line segments are `missing` (serialized as JSON `null`, which breaks the line) rather
    # than `NaN`, which the JSON writer used by `to_html` rejects.
    values = Union{Float32, Missing}[]
    heights = Union{Float32, Missing}[]

    n_values = length(clusters.order)
    @assert size(clusters.merges, 1) == n_values - 1
    value_per_node = Vector{Float32}(undef, n_values * 2 - 1)
    height_per_node = Vector{Float32}(undef, n_values * 2 - 1)
    height_per_node[1:n_values] .= 0

    if expanded_mask === nothing
        expanded_positions = nothing
    else
        expanded_positions = findall(expanded_mask)
        @assert length(expanded_positions) == n_values
    end

    for (position, index) in enumerate(clusters.order)
        if expanded_positions !== nothing
            position = expanded_positions[position]
        end
        value_per_node[index] = position
    end

    for merge_index in 1:(n_values - 1)
        left_merge_index, right_merge_index = clusters.merges[merge_index, :]
        height = clusters.heights[merge_index]

        @assert left_merge_index != 0
        @assert right_merge_index != 0
        @assert height >= 0

        left_node_index = left_merge_index < 0 ? -left_merge_index : left_merge_index + n_values
        right_node_index = right_merge_index < 0 ? -right_merge_index : right_merge_index + n_values

        left_value = value_per_node[left_node_index]
        right_value = value_per_node[right_node_index]

        left_height = height_per_node[left_node_index]
        right_height = height_per_node[right_node_index]

        middle_value = (left_value + right_value) / 2

        push!(values, left_value, left_value, right_value, right_value, missing)
        push!(heights, left_height, height, height, right_height, missing)

        value_per_node[merge_index + n_values] = middle_value
        height_per_node[merge_index + n_values] = height
    end

    return (values, heights)
end

function compute_expansion_mask(
    order::Maybe{AbstractVector{<:Integer}},
    groups::Maybe{AbstractVector},
    subgroups::Maybe{AbstractVector},
    side_configuration::HeatmapSideConfiguration,
)::Maybe{Union{BitVector, AbstractVector{Bool}}}
    groups_gap = side_configuration.groups_gap
    subgroups_gap = side_configuration.subgroups_gap

    has_groups_gap = groups !== nothing && groups_gap !== nothing
    has_subgroups_gap = subgroups !== nothing && subgroups_gap !== nothing
    if !has_groups_gap && !has_subgroups_gap
        return nothing
    end

    @assert groups_gap === nothing || groups_gap > 0
    @assert subgroups_gap === nothing || subgroups_gap > 0

    if order === nothing
        order = 1:length(groups === nothing ? subgroups : groups)  # UNTESTED
    end

    ## The gap before each entry, in entries, at the minimal widths. A boundary between the groups is also a boundary
    ## between the subgroups, and is gapped as the wider of the two.
    gap_per_entry = zeros(Int, length(order))
    prev_group = has_groups_gap ? groups[order[1]] : nothing
    prev_subgroup = has_subgroups_gap ? subgroups[order[1]] : nothing
    for (entry_position, entry_index) in enumerate(order)
        if has_groups_gap && groups[entry_index] != prev_group
            gap_per_entry[entry_position] = groups_gap
        elseif has_subgroups_gap && subgroups[entry_index] != prev_subgroup
            gap_per_entry[entry_position] = subgroups_gap
        end
        if has_groups_gap
            prev_group = groups[entry_index]
        end
        if has_subgroups_gap
            prev_subgroup = subgroups[entry_index]
        end
    end

    scale = gaps_scale(length(order), sum(gap_per_entry), side_configuration.total_gaps_fraction)

    expanded_mask = Bool[]
    for gap in gap_per_entry
        for _ in 1:round(Int, gap * scale)
            push!(expanded_mask, false)
        end
        push!(expanded_mask, true)
    end

    return expanded_mask
end

# How much to widen every gap so that together they take the `total_gaps_fraction` of the axis. Gaps are never
# narrowed, so this is at least 1.
function gaps_scale(n_entries::Integer, total_gap_entries::Integer, total_gaps_fraction::Maybe{Real})::Float64
    if total_gaps_fraction === nothing || total_gap_entries == 0
        return 1.0
    end
    return max(1.0, n_entries * total_gaps_fraction / total_gap_entries)
end

function expand_z_matrix(
    z::AbstractMatrix{<:Union{Real, Missing}},
    rows_order::Maybe{AbstractVector{<:Integer}},
    expanded_rows_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
    columns_order::Maybe{AbstractVector{<:Integer}},
    expanded_columns_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
)::AbstractMatrix{<:Union{Real, Missing}}
    if rows_order === nothing &&
       expanded_rows_mask === nothing &&
       columns_order === nothing &&
       expanded_columns_mask === nothing
        return z  # UNTESTED
    end

    if expanded_rows_mask === nothing && expanded_columns_mask === nothing
        return z
    end

    n_rows, n_columns = size(z)

    if expanded_rows_mask === nothing
        n_expanded_rows = n_rows
        expanded_rows_mask = 1:n_rows
    else
        n_expanded_rows = length(expanded_rows_mask)
    end

    if expanded_columns_mask === nothing
        n_expanded_columns = n_columns
        expanded_columns_mask = 1:n_columns
    else
        n_expanded_columns = length(expanded_columns_mask)
    end

    # The gap entries are `missing` (serialized as JSON `null`) rather than `NaN`: Plotly renders both as blank gaps,
    # but the JSON writer used by `to_html` rejects `NaN`.
    expanded_z = Matrix{Union{eltype(z), Missing}}(undef, n_expanded_rows, n_expanded_columns)
    expanded_z .= missing
    expanded_z[expanded_rows_mask, expanded_columns_mask] .= z

    return expanded_z
end

function expand_hovers(;
    n_expanded_rows::Integer,
    n_expanded_columns::Integer,
    expanded_rows_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
    expanded_columns_mask::Maybe{Union{BitVector, AbstractVector{Bool}}},
    rows_hovers::Maybe{AbstractVector{<:AbstractString}},
    columns_hovers::Maybe{AbstractVector{<:AbstractString}},
    entries_hovers::Maybe{AbstractMatrix{<:AbstractString}},
)::Maybe{AbstractMatrix{<:AbstractString}}
    if columns_hovers === nothing &&
       rows_hovers === nothing &&
       (entries_hovers === nothing || (expanded_rows_mask === nothing && expanded_columns_mask === nothing))
        return entries_hovers
    end

    expanded_hovers = Matrix{AbstractString}(undef, n_expanded_rows, n_expanded_columns)
    expanded_hovers .= ""

    if expanded_rows_mask === nothing
        expanded_rows_indices = 1:n_expanded_rows
    else
        expanded_rows_indices = findall(expanded_rows_mask)
    end

    if expanded_columns_mask === nothing
        expanded_columns_indices = 1:n_expanded_columns
    else
        expanded_columns_indices = findall(expanded_columns_mask)
    end

    for (column_index, column_position) in enumerate(expanded_columns_indices)
        if columns_hovers !== nothing
            column_hover = columns_hovers[column_index]
        else
            column_hover = ""
        end

        for (row_index, row_position) in enumerate(expanded_rows_indices)
            text = String[]
            if entries_hovers !== nothing
                entry_hover = entries_hovers[row_index, column_index]
                if entry_hover != ""
                    push!(text, entry_hover)
                end
            end

            if rows_hovers !== nothing
                row_hover = rows_hovers[row_index]
                if row_hover != ""
                    push!(text, row_hover)
                end
            end

            if column_hover != ""
                push!(text, column_hover)
            end

            expanded_hovers[row_position, column_position] = join(text, "<br>")
        end
    end

    return expanded_hovers
end

# The data of a side, moved to the other side of the graph (that is, with its `arrange_by` matrix transposed).
function flipped_side_data(side::HeatmapSideData)::HeatmapSideData
    arrangement = side.arrangement
    return HeatmapSideData(;
        entities = side.entities,
        arrangement = ArrangementData(;
            hclust = arrangement.hclust,
            groups = arrangement.groups,
            subgroups = arrangement.subgroups,
            arrange_by = arrangement.arrange_by === nothing ? nothing : transpose(arrangement.arrange_by),
        ),
        annotations = side.annotations,
        annotations_order = side.annotations_order,
    )
end

function Common.flip_axes(graph::HeatmapGraph)::HeatmapGraph
    entries = graph.data.entries
    cells = graph.data.cells
    return HeatmapGraph(  # NOJET
        HeatmapGraphData(;
            figure_title = graph.data.figure_title,
            entries = MatrixValuesData(;
                matrix = entries.matrix === nothing ? nothing : transpose(entries.matrix),
                title = entries.title,
            ),
            cells = MatrixEntitiesData(;
                hovers = cells.hovers === nothing ? nothing : PermutedDimsArray(cells.hovers, (2, 1)),
            ),
            rows = flipped_side_data(graph.data.columns),
            columns = flipped_side_data(graph.data.rows),
        ),
        HeatmapGraphConfiguration(;
            figure = graph.configuration.figure,
            entries = graph.configuration.entries,
            rows = graph.configuration.columns,
            columns = graph.configuration.rows,
            origin = graph.configuration.origin,
        ),
    )
end

function Common.flip_axes!(graph::HeatmapGraph)::HeatmapGraph
    data = graph.data
    data.entries.matrix = data.entries.matrix === nothing ? nothing : transpose(data.entries.matrix)
    data.cells.hovers = data.cells.hovers === nothing ? nothing : PermutedDimsArray(data.cells.hovers, (2, 1))  # NOJET
    data.rows, data.columns = data.columns, data.rows
    for side in (data.rows, data.columns)
        arrangement = side.arrangement
        if arrangement.arrange_by !== nothing
            arrangement.arrange_by = transpose(arrangement.arrange_by)
        end
    end

    configuration = graph.configuration
    configuration.rows, configuration.columns = configuration.columns, configuration.rows

    return graph
end

end
