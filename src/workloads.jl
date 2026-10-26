# Compile the common uses of the graphs when the package is precompiled, so that their first use in a session is fast.
module Workloads

using PrecompileTools

using ..Bars
using ..Common
using ..Distributions
using ..Heatmaps
using ..Scatters
using ..Sources

import ..Common.graph_to_json

# Build a small graph of each kind, with the data and configuration graphs typically have, and convert it to JSON
# (which is what showing a graph does). This runs when the package is precompiled.
function run_workload()::Nothing
    names = ["a", "b", "c"]
    values = Float32[1, 2, 3]

    points = points_graph(; x = VectorValuesData(values), y = VectorValuesData(values .* 2))
    put_vector_data!(points_colors_vector_fields(points), Float32[0.1, 0.5, 0.9]; title = "colors")
    put_vector_data!(points_sizes_vector_fields(points), values; title = "sizes")
    put_vector_names_data!(points_entities(points), names)
    graph_to_json(points)

    heatmap = heatmap_graph(; entries = MatrixValuesData(Float32[1 2 3; 4 5 6; 7 8 10]))
    heatmap.configuration.rows.order_source = OptimalTreeReorder
    heatmap.configuration.columns.order_source = OptimalTreeReorder
    put_matrix_names_data!(entries_matrix_fields(heatmap), names, names)
    put_vector_data!(
        rows_annotations_colors_vector_fields(heatmap, add_rows_annotation!(heatmap)),
        values;
        title = "rows",
    )
    put_vector_data!(
        columns_annotations_colors_vector_fields(heatmap, add_columns_annotation!(heatmap)),
        values;
        title = "columns",
    )
    graph_to_json(heatmap)

    graph_to_json(bars_graph(; values = VectorValuesData(values)))
    graph_to_json(
        series_bars_graph(;
            series = [SeriesData(; values = VectorValuesData(values)), SeriesData(; values = VectorValuesData(values))],
        ),
    )

    graph_to_json(line_graph(; x = VectorValuesData(values), y = VectorValuesData(values)))
    graph_to_json(lines_graph(; lines = [LineData(; x = VectorValuesData(values), y = VectorValuesData(values))]))

    samples = Float32.(collect(1:20))
    graph_to_json(distribution_graph(; distribution = DistributionData(; values = VectorValuesData(samples))))
    graph_to_json(
        distributions_graph(;
            distributions = [
                DistributionData(; values = VectorValuesData(samples)),
                DistributionData(; values = VectorValuesData(samples .* 2)),
            ],
        ),
    )
    return nothing
end

@compile_workload begin
    run_workload()
end

end  # module
