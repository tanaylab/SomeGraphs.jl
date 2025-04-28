nested_test("heatmaps") do
    graph = heatmap_graph(; entries_values = [
        0 1 2 3;
        7 6 5 4;
        8 9 10 11;
    ])

    nested_test("invalid") do
        nested_test("fixed") do
            graph.configuration.entries_colors.fixed = "black"
            @test_throws "ArgumentError: can't specify heatmap graph.configuration.entries_colors.fixed" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("categorical") do
            graph.configuration.entries_colors.palette = Dict("Foo" => "red", "Bar" => "green")
            @test_throws "ArgumentError: can't specify heatmap categorical graph.configuration.entries_colors.palette" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("conflict") do
            graph.configuration.entries_order = :complete
            graph.configuration.rows_order = :ward
            @test_throws chomp("""
                               can't specify both graph.configuration.entries_order
                               and graph.configuration.rows_order
                               """) validate(ValidationContext(["graph"]), graph)
        end
    end

    nested_test("()") do
        test_html(graph, "heatmap.html")
        return nothing
    end

    nested_test("names") do
        graph.data.rows_names = ["X", "Y", "Z"]
        graph.data.columns_names = ["A", "B", "C", "D"]
        test_html(graph, "heatmap.names.html")
        return nothing
    end

    nested_test("log") do
        graph.configuration.entries_colors.axis.log_scale = Log2Scale
        graph.configuration.entries_colors.axis.log_regularization = 1
        test_html(graph, "heatmap.log2.html")
        return nothing
    end

    nested_test("legend") do
        graph.configuration.entries_colors.show_legend = true
        test_html(graph, "heatmap.legend.html")
        return nothing
    end

    nested_test("annotations") do
        graph.data.rows_annotations = [
            AnnotationData(;
                title = "is",
                values = ["yes", "maybe", "no"],
                colors = ColorsConfiguration(;
                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                ),
            ),
        ]
        graph.data.columns_annotations = [AnnotationData(; title = "score", values = [1, 0.5, 0, 1])]

        nested_test("()") do
            test_html(graph, "heatmap.annotations.html")
            return nothing
        end

        nested_test("legend") do
            graph.data.entries_colors_title = "values"
            graph.configuration.entries_colors.show_legend = true
            graph.data.rows_annotations[1].colors.show_legend = true
            graph.data.columns_annotations[1].colors.show_legend = true
            graph.data.rows_names = ["X", "Y", "Z"]
            graph.data.columns_names = ["A", "B", "C", "D"]
            test_html(graph, "heatmap.annotations.legend.html")
            return nothing
        end

        nested_test("reorder") do
            nested_test("rows") do
                graph.data.rows_order = [1, 3, 2]
                return test_html(graph, "heatmap.reorder.rows.html")
            end

            nested_test("columns") do
                graph.data.columns_order = [1, 3, 2, 4]
                return test_html(graph, "heatmap.reorder.columns.html")
            end

            nested_test("both") do
                graph.data.rows_order = [1, 3, 2]
                graph.data.columns_order = [1, 3, 2, 4]
                return test_html(graph, "heatmap.reorder.both.html")
            end

            nested_test("ward") do
                graph.data.entries_values = graph.data.entries_values[[1, 3, 2], [1, 3, 2, 4]]
                graph.configuration.rows_order = :ward
                graph.configuration.columns_order = :ward
                return test_html(graph, "heatmap.reorder.ward.html")
            end
        end
    end
end
