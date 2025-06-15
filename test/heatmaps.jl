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

        nested_test("same") do
            nested_test("entries") do
                graph.configuration.columns_reorder = SameOrder
                @test_throws "ArgumentError: can't specify heatmap graph.configuration.columns_reorder: SameOrder" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end

            nested_test("both") do
                graph.configuration.rows_reorder = SameOrder
                graph.configuration.columns_reorder = SameOrder
                @test_throws chomp("""
                                   can't specify both heatmap graph.configuration.rows_reorder: SameOrder
                                   and heatmap graph.configuration.columns_reorder: SameOrder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("rectangle") do
                graph.configuration.rows_reorder = SameOrder
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.rows_reorder: SameOrder
                                   for a non-square matrix: 3 rows x 4 columns
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("missing") do
                graph = heatmap_graph(; entries_values = [
                    0 1 2;
                    7 6 5;
                    8 9 10;
                ])
                graph.configuration.rows_reorder = SameOrder
                @test_throws chomp("""
                                   specify heatmap graph.configuration.rows_reorder: SameOrder
                                   without an order to copy from the columns
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("groups") do
            graph.data.rows_groups = [1, 2, 2]
            graph.configuration.rows_groups_gap = nothing
            @test_throws chomp("no effect for specified graph.data.rows_groups") validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("reorder") do
            graph.configuration.rows_reorder = ReorderHclust
            @test_throws chomp("""
                               can't specify heatmap graph.configuration.rows_reorder: ReorderHclust
                               without explicit vector graph.data.rows_order
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("categorical") do
            graph.configuration.entries_colors.palette = Dict("Foo" => "red", "Bar" => "green")
            @test_throws "ArgumentError: can't specify heatmap categorical graph.configuration.entries_colors.palette" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("filled") do
            graph.configuration.rows_dendogram_line.is_filled = true
            @test_throws chomp("""
                               can't specify heatmap graph.configuration.rows_dendogram_line.is_filled
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("width") do
            graph.configuration.rows_dendogram_line.width = 1
            @test_throws chomp("""
                               can't specify heatmap graph.configuration.rows_dendogram_line.*
                               without graph.configuration.rows_dendogram_size
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("order") do
            graph.data.columns_order = collect(1:4)
            graph.data.columns_arrange_by = graph.data.entries_values
            @test_throws chomp("""
                               can't specify heatmap graph.data.columns_arrange_by
                               for explicit vector graph.data.columns_order
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("arrange_by") do
            graph.data.columns_arrange_by = graph.data.entries_values

            nested_test("()") do
                @test_throws chomp("""
                                   can't specify heatmap graph.data.columns_arrange_by
                                   without graph.configuration.columns_dendogram_size
                                   or graph.configuration.columns_reorder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("hclust") do
                distances = pairwise(Euclidean(), graph.data.entries_values; dims = 2)
                graph.data.columns_order = hclust(distances)
                @test_throws chomp("""
                                   can't specify heatmap graph.data.columns_arrange_by
                                   without graph.configuration.columns_reorder
                                   for explicit hclust graph.data.columns_order
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("linkage") do
            graph.configuration.columns_linkage = CompleteLinkage
            nested_test("()") do
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns_linkage
                                   without graph.configuration.columns_dendogram_size
                                   or graph.configuration.columns_reorder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("hclust") do
                distances = pairwise(Euclidean(), graph.data.entries_values; dims = 2)
                graph.data.columns_order = hclust(distances)
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns_linkage
                                   for explicit hclust graph.data.columns_order
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("vector") do
                graph.data.columns_order = collect(1:4)
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns_linkage
                                   for explicit vector graph.data.columns_order
                                   without graph.configuration.columns_dendogram_size
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("metric") do
            graph.configuration.columns_metric = Euclidean()

            nested_test("()") do
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns_metric
                                   without graph.configuration.columns_dendogram_size
                                   or graph.configuration.columns_reorder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("hclust") do
                distances = pairwise(Euclidean(), graph.data.entries_values; dims = 2)
                graph.data.columns_order = hclust(distances)
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns_metric
                                   for explicit hclust graph.data.columns_order
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("vector") do
                graph.data.columns_order = collect(1:4)
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns_metric
                                   for explicit vector graph.data.columns_order
                                   without graph.configuration.columns_dendogram_size
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("reorder") do
            nested_test("hclust") do
                distances = pairwise(Euclidean(), graph.data.entries_values; dims = 2)
                graph.data.columns_order = hclust(distances)
                graph.configuration.columns_reorder = OptimalHclust
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns_reorder: OptimalHclust
                                   for explicit hclust graph.data.columns_order
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("vector") do
                graph.data.columns_order = collect(1:4)
                graph.configuration.columns_reorder = OptimalHclust
                @test_throws chomp("""
                                   specify heatmap graph.configuration.columns_reorder: OptimalHclust
                                   for explicit vector graph.data.columns_order
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("same") do
            graph.configuration.columns_reorder = SameOrder
            graph.data.entries_values = [
                0 1 2;
                7 6 5;
                8 9 10;
            ]

            nested_test("()") do
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns_reorder: SameOrder
                                   without an order to copy from the rows
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            graph.data.rows_order = collect(1:3)

            nested_test("arrange_by") do
                graph.data.columns_arrange_by = graph.data.entries_values
                @test_throws chomp("""
                                   can't specify heatmap graph.data.columns_arrange_by
                                   for graph.configuration.columns_reorder: SameOrder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("linkage") do
                graph.configuration.columns_linkage = WardLinkage
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns_linkage
                                   for graph.configuration.columns_reorder: SameOrder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("metric") do
                graph.configuration.columns_metric = Euclidean()
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns_metric
                                   for graph.configuration.columns_reorder: SameOrder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("dendogram_size") do
                graph.configuration.columns_dendogram_size = 0.1
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns_dendogram_size
                                   with graph.configuration.columns_reorder: SameOrder
                                   without a tree to copy from the rows
                                   """) validate(ValidationContext(["graph"]), graph)
            end
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

    nested_test("origin") do
        nested_test("bottom_left") do
            graph.configuration.origin = HeatmapBottomLeft
            test_html(graph, "heatmap.bottom_left.html")
            return nothing
        end

        nested_test("bottom_right") do
            graph.configuration.origin = HeatmapBottomRight
            test_html(graph, "heatmap.bottom_right.html")
            return nothing
        end

        nested_test("top_left") do
            graph.configuration.origin = HeatmapTopLeft
            test_html(graph, "heatmap.top_left.html")
            return nothing
        end

        nested_test("top_right") do
            graph.configuration.origin = HeatmapTopRight
            test_html(graph, "heatmap.top_right.html")
            return nothing
        end
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

        nested_test("dendogram") do
            graph.configuration.rows_reorder = OptimalHclust
            graph.configuration.rows_dendogram_size = 0.2
            graph.configuration.columns_dendogram_size = 0.2

            nested_test("()") do
                test_html(graph, "heatmap.annotations.dendogram.html")
                return nothing
            end

            nested_test("gaps") do
                graph.data.rows_groups = [1, 2, 2]
                graph.data.columns_groups = [1, 1, 2, 3]
                graph.data.rows_names = ["X", "Y", "Z"]
                graph.data.columns_names = ["A", "B", "C", "D"]
                test_html(graph, "heatmap.annotations.dendogram.gaps.html")
                return nothing
            end
        end

        nested_test("gaps") do
            graph.data.rows_groups = [1, 2, 2]
            graph.data.columns_groups = [1, 1, 2, 3]
            graph.data.rows_names = ["X", "Y", "Z"]
            graph.data.columns_names = ["A", "B", "C", "D"]
            test_html(graph, "heatmap.annotations.gaps.html")
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
                test_html(graph, "heatmap.reorder.rows.html")
                return nothing
            end

            nested_test("columns") do
                graph.data.columns_order = [1, 3, 2, 4]
                test_html(graph, "heatmap.reorder.columns.html")
                return nothing
            end

            nested_test("both") do
                graph.data.rows_order = [1, 3, 2]
                graph.data.columns_order = [1, 3, 2, 4]
                test_html(graph, "heatmap.reorder.both.html")
                return nothing
            end

            nested_test("dendogram") do
                graph.data.rows_order = [1, 3, 2]
                graph.data.columns_order = [1, 3, 2, 4]
                graph.configuration.rows_dendogram_size = 0.2
                graph.configuration.columns_dendogram_size = 0.2
                graph.configuration.columns_reorder = ReorderHclust
                return test_html(graph, "heatmap.reorder.dendogram.html")
            end

            nested_test("ward") do
                graph.data.entries_values = graph.data.entries_values[[1, 3, 2], [1, 3, 2, 4]]
                graph.configuration.rows_reorder = OptimalHclust
                graph.configuration.columns_reorder = OptimalHclust
                test_html(graph, "heatmap.reorder.ward.html")
                return nothing
            end

            nested_test("slant") do
                nested_test("rows") do
                    graph.configuration.rows_reorder = SlantedOrder

                    nested_test("()") do
                        test_html(graph, "heatmap.reorder.slanted.rows.html")
                        return nothing
                    end

                    nested_test("same") do
                        graph.data.entries_values = [
                            0 1 2;
                            7 6 5;
                            8 9 10;
                        ]
                        pop!(graph.data.columns_annotations[1].values)
                        graph.configuration.columns_reorder = SameOrder
                        return test_html(graph, "heatmap.reorder.slanted.rows.same.html")
                    end
                end

                nested_test("columns") do
                    graph.configuration.columns_reorder = SlantedOrder

                    nested_test("()") do
                        test_html(graph, "heatmap.reorder.slanted.columns.html")
                        return nothing
                    end

                    nested_test("same") do
                        graph.data.entries_values = [
                            0 1 2;
                            7 6 5;
                            8 9 10;
                        ]
                        pop!(graph.data.columns_annotations[1].values)
                        graph.configuration.rows_reorder = SameOrder
                        return test_html(graph, "heatmap.reorder.slanted.columns.same.html")
                    end
                end

                nested_test("both") do
                    graph.configuration.rows_reorder = SlantedOrder
                    graph.configuration.columns_reorder = SlantedOrder
                    test_html(graph, "heatmap.reorder.slanted.both.html")
                    return nothing
                end

                nested_test("hclust") do
                    graph.configuration.rows_reorder = SlantedHclust
                    graph.configuration.columns_reorder = SlantedHclust
                    test_html(graph, "heatmap.reorder.slanted.hclust.html")
                    return nothing
                end
            end
        end
    end

    nested_test("hclust") do
        distances = pairwise(Euclidean(), graph.data.entries_values; dims = 2)
        graph.data.columns_order = hclust(distances)

        nested_test("()") do
            test_html(graph, "heatmap.hclust.html")
            return nothing
        end

        nested_test("dendogram") do
            graph.configuration.rows_reorder = OptimalHclust
            graph.configuration.rows_dendogram_size = 0.2
            graph.configuration.columns_dendogram_size = 0.2

            nested_test("()") do
                test_html(graph, "heatmap.hclust.dendogram.html")
                return nothing
            end

            nested_test("gaps") do
                graph.data.rows_groups = [1, 2, 2]
                graph.data.columns_groups = [1, 1, 2, 3]
                graph.data.rows_names = ["X", "Y", "Z"]
                graph.data.columns_names = ["A", "B", "C", "D"]
                test_html(graph, "heatmap.dendogram.gaps.html")
                return nothing
            end
        end

        nested_test("slanted") do
            graph.configuration.columns_reorder = SlantedHclust
            test_html(graph, "heatmap.hclust.slanted.html")
            return nothing
        end
    end

    nested_test("same") do
        graph = heatmap_graph(; entries_values = [
            0 1 2;
            7 6 5;
            8 9 10;
        ])
        nested_test("rows") do
            graph.configuration.rows_reorder = SameOrder
            graph.data.columns_order = [1, 3, 2]
            test_html(graph, "heatmap.reorder.rows=columns.html")
            return nothing
        end

        nested_test("columns") do
            graph.data.rows_order = [1, 3, 2]
            graph.configuration.columns_reorder = SameOrder
            test_html(graph, "heatmap.reorder.columns=rows.html")
            return nothing
        end
    end

    nested_test("hovers") do
        graph.data.rows_names = ["X", "Y", "Z"]
        graph.data.columns_names = ["A", "B", "C", "D"]

        nested_test("entries") do
            graph.data.entries_hovers = [
                "XA" "XB" "XC" "XD";
                "YA" "YB" "YC" "YD";
                "ZA" "ZB" "ZC" "ZD";
            ]

            nested_test("()") do
                test_html(graph, "heatmap.hovers.entries.html")
                return nothing
            end

            nested_test("gaps") do
                graph.data.rows_groups = [1, 2, 2]
                graph.data.columns_groups = [1, 1, 2, 3]
                graph.data.rows_names = ["X", "Y", "Z"]
                graph.data.columns_names = ["A", "B", "C", "D"]
                test_html(graph, "heatmap.hovers.entries.gaps.html")
                return nothing
            end
        end

        nested_test("axes") do
            graph.data.rows_hovers = ["R:X", "R:Y", "R:Z"]
            graph.data.columns_hovers = ["C:A", "C:B", "C:C", "C:D"]

            nested_test("()") do
                test_html(graph, "heatmap.hovers.axes.html")
                return nothing
            end

            nested_test("gaps") do
                graph.data.rows_groups = [1, 2, 2]
                graph.data.columns_groups = [1, 1, 2, 3]
                graph.data.rows_names = ["X", "Y", "Z"]
                graph.data.columns_names = ["A", "B", "C", "D"]
                test_html(graph, "heatmap.hovers.axes.gaps.html")
                return nothing
            end
        end

        nested_test("both") do
            graph.data.entries_hovers = [
                "XA" "XB" "XC" "XD";
                "YA" "YB" "YC" "YD";
                "ZA" "ZB" "ZC" "ZD";
            ]
            graph.data.rows_hovers = ["R:X", "R:Y", "R:Z"]
            graph.data.columns_hovers = ["C:A", "C:B", "C:C", "C:D"]

            nested_test("()") do
                test_html(graph, "heatmap.hovers.both.html")
                return nothing
            end

            nested_test("gaps") do
                graph.data.rows_groups = [1, 2, 2]
                graph.data.columns_groups = [1, 1, 2, 3]
                graph.data.rows_names = ["X", "Y", "Z"]
                graph.data.columns_names = ["A", "B", "C", "D"]
                test_html(graph, "heatmap.hovers.both.gaps.html")
                return nothing
            end
        end
    end
end
