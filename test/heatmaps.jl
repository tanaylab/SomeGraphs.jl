nested_test("heatmaps") do
    graph = heatmap_graph(; entries = MatrixValuesData([
        0 1 2 3;
        7 6 5 4;
        8 9 10 11;
    ]))

    nested_test("fields") do
        fields = entries_matrix_fields(graph)
        @test fields.data.values === graph.data.entries
        @test fields.data.entities === graph.data.cells
        @test fields.data.rows_entities === graph.data.rows.entities
        @test fields.data.columns_entities === graph.data.columns.entities
        @test fields.configuration.scale === graph.configuration.entries.colors.scale
        @test fields.configuration.colors === graph.configuration.entries.colors

        annotation = AnnotationData(; values = VectorValuesData([1, 0.5, 0], "score"))
        @test add_rows_annotation!(graph, annotation) == 1
        @test graph.data.rows.annotations[1] === annotation
        fields = rows_annotations_colors_vector_fields(graph, 1)
        @test fields.data.values === annotation.values
        @test fields.data.entities === graph.data.rows.entities
        @test fields.configuration.colors === annotation.colors

        @test add_rows_annotation!(graph) == 2
        rows_annotations_colors_vector_fields(graph, 2).data.values.vector = [0, 1, 0]
        @test graph.data.rows.annotations[2].values.vector == [0, 1, 0]

        annotation = AnnotationData(; values = VectorValuesData([1, 0.5, 0, 1], "score"))
        @test add_columns_annotation!(graph, annotation) == 1
        @test graph.data.columns.annotations[1] === annotation
        fields = columns_annotations_colors_vector_fields(graph, 1)
        @test fields.data.values === annotation.values
        @test fields.data.entities === graph.data.columns.entities
        @test fields.configuration.colors === annotation.colors

        @test add_columns_annotation!(graph) == 2
        columns_annotations_colors_vector_fields(graph, 2).data.values.vector = [0, 1, 0, 1]
        @test graph.data.columns.annotations[2].values.vector == [0, 1, 0, 1]

        for (fields, values, entities) in (
            (rows_groups_vector_data_fields(graph), graph.data.rows.groups, graph.data.rows.entities),
            (rows_subgroups_vector_data_fields(graph), graph.data.rows.subgroups, graph.data.rows.entities),
            (columns_groups_vector_data_fields(graph), graph.data.columns.groups, graph.data.columns.entities),
            (columns_subgroups_vector_data_fields(graph), graph.data.columns.subgroups, graph.data.columns.entities),
        )
            @test fields.values === values
            @test fields.entities === entities
        end
        return nothing
    end

    nested_test("invalid") do
        nested_test("fixed") do
            graph.configuration.entries.colors.fixed = "black"
            @test_throws "ArgumentError: can't specify heatmap graph.configuration.entries.colors.fixed" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("same") do
            nested_test("entries") do
                graph.configuration.columns.reorder = SameOrder
                @test_throws "ArgumentError: can't specify heatmap graph.configuration.columns.reorder: SameOrder" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end

            nested_test("both") do
                graph.configuration.rows.reorder = SameOrder
                graph.configuration.columns.reorder = SameOrder
                @test_throws chomp("""
                                   can't specify both heatmap graph.configuration.rows.reorder: SameOrder
                                   and heatmap graph.configuration.columns.reorder: SameOrder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("rectangle") do
                graph.configuration.rows.reorder = SameOrder
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.rows.reorder: SameOrder
                                   for a non-square matrix: 3 rows x 4 columns
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("missing") do
                graph = heatmap_graph(; entries = MatrixValuesData([
                    0 1 2;
                    7 6 5;
                    8 9 10;
                ]))
                graph.configuration.rows.reorder = SameOrder
                @test_throws chomp("""
                                   specify heatmap graph.configuration.rows.reorder: SameOrder
                                   without an order to copy from the columns
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("groups") do
            graph.data.rows.groups.vector = [1, 2, 2]
            graph.configuration.rows.groups_gap = nothing
            @test_throws chomp("no effect for specified graph.data.rows.groups.vector") validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("reorder") do
            graph.configuration.rows.reorder = ReorderHclust
            @test_throws chomp("""
                               can't specify heatmap graph.configuration.rows.reorder: ReorderHclust
                               without explicit vector graph.data.rows.order
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("categorical") do
            graph.configuration.entries.colors.palette = Dict("Foo" => "red", "Bar" => "green")
            @test_throws "ArgumentError: can't specify heatmap categorical graph.configuration.entries.colors.palette" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("annotation") do
            push!(graph.data.rows.annotations, AnnotationData(; values = VectorValuesData(["red", "green", "Oobleck"])))
            @test_throws "ArgumentError: invalid graph.data.rows.annotations[1].values.vector[3]: Oobleck" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("~annotations_order") do
            push!(graph.data.rows.annotations, AnnotationData(; values = VectorValuesData([1, 0.5, 0])))
            graph.data.rows.annotations_order = [1, 2]
            @test_throws chomp("""
                               ArgumentError: invalid length of graph.data.rows.annotations_order: 2
                               is different from length of graph.data.rows.annotations: 1
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("!annotation") do
            push!(graph.data.rows.annotations, AnnotationData())
            @test_throws "ArgumentError: must specify graph.data.rows.annotations[1].values.vector" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("!entries") do
            graph.data.entries.matrix = nothing
            @test_throws "ArgumentError: must specify graph.data.entries.matrix" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("~names") do
            graph.data.rows.entities.names = ["X", "Y"]
            @test_throws chomp("""
                               ArgumentError: invalid length of graph.data.rows.entities.names: 2
                               is different from length of graph.data.entries.matrix.rows: 3
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("mask") do
            nested_test("!rows") do
                graph.data.rows.entities.mask = [false, false, false]
                @test_throws "ArgumentError: all entries hidden by graph.data.rows.entities.mask" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end

            nested_test("~rows") do
                graph.data.rows.entities.mask = [true, false]
                @test_throws chomp("""
                                   ArgumentError: invalid length of graph.data.rows.entities.mask: 2
                                   is different from length of graph.data.entries.matrix.rows: 3
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("~cells") do
            graph.data.cells.hovers = fill("H", 2, 2)
            @test_throws chomp("""
                               ArgumentError: invalid size of graph.data.cells.hovers: (2, 2)
                               is different from size of graph.data.entries.matrix: (3, 4)
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("finite") do
            nested_test("entries") do
                graph.data.entries.matrix = Float32[0 1 2 3; 7 6 NaN 4; 8 9 10 11]
                @test_throws "ArgumentError: non-finite graph.data.entries.matrix[2, 3]: NaN" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end

            nested_test("arrange_by") do
                graph.data.rows.arrange_by = Float32[0 1; 2 Inf; 4 5]
                @test_throws "ArgumentError: non-finite graph.data.rows.arrange_by[2, 2]: Inf" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end

            nested_test("groups") do
                graph.data.rows.groups.vector = Float32[1, 1, NaN]
                @test_throws "ArgumentError: non-finite graph.data.rows.groups.vector[3]: NaN" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end

            nested_test("annotation") do
                graph.data.rows.annotations = [AnnotationData(; values = VectorValuesData(Float32[1, -Inf, 0]))]
                @test_throws "ArgumentError: non-finite graph.data.rows.annotations[1].values.vector[2]: -Inf" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end
        end

        nested_test("filled") do
            graph.configuration.rows.dendogram_line.is_filled = true
            @test_throws chomp("""
                               can't specify heatmap graph.configuration.rows.dendogram_line.is_filled
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("width") do
            graph.configuration.rows.dendogram_line.width = 1
            @test_throws chomp("""
                               can't specify heatmap graph.configuration.rows.dendogram_line.*
                               without graph.configuration.rows.dendogram_size
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("order") do
            graph.data.columns.order = collect(1:4)
            graph.data.columns.arrange_by = graph.data.entries.matrix
            @test_throws chomp("""
                               can't specify heatmap graph.data.columns.arrange_by
                               for explicit vector graph.data.columns.order
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("arrange_by") do
            graph.data.columns.arrange_by = graph.data.entries.matrix

            nested_test("()") do
                @test_throws chomp("""
                                   can't specify heatmap graph.data.columns.arrange_by
                                   without graph.configuration.columns.dendogram_size
                                   or graph.configuration.columns.reorder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("hclust") do
                distances = pairwise(Euclidean(), graph.data.entries.matrix; dims = 2)
                graph.data.columns.order = hclust(distances)
                @test_throws chomp("""
                                   can't specify heatmap graph.data.columns.arrange_by
                                   without graph.configuration.columns.reorder
                                   for explicit hclust graph.data.columns.order
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("linkage") do
            graph.configuration.columns.linkage = CompleteLinkage
            nested_test("()") do
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.linkage
                                   without graph.configuration.columns.dendogram_size
                                   or graph.configuration.columns.reorder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("hclust") do
                distances = pairwise(Euclidean(), graph.data.entries.matrix; dims = 2)
                graph.data.columns.order = hclust(distances)
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.linkage
                                   for explicit hclust graph.data.columns.order
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("vector") do
                graph.data.columns.order = collect(1:4)
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.linkage
                                   for explicit vector graph.data.columns.order
                                   without graph.configuration.columns.dendogram_size
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("metric") do
            graph.configuration.columns.metric = Euclidean()

            nested_test("()") do
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.metric
                                   without graph.configuration.columns.dendogram_size
                                   or graph.configuration.columns.reorder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("hclust") do
                distances = pairwise(Euclidean(), graph.data.entries.matrix; dims = 2)
                graph.data.columns.order = hclust(distances)
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.metric
                                   for explicit hclust graph.data.columns.order
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("vector") do
                graph.data.columns.order = collect(1:4)
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.metric
                                   for explicit vector graph.data.columns.order
                                   without graph.configuration.columns.dendogram_size
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("reorder") do
            nested_test("hclust") do
                distances = pairwise(Euclidean(), graph.data.entries.matrix; dims = 2)
                graph.data.columns.order = hclust(distances)
                graph.configuration.columns.reorder = OptimalHclust
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.reorder: OptimalHclust
                                   for explicit hclust graph.data.columns.order
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("vector") do
                graph.data.columns.order = collect(1:4)
                graph.configuration.columns.reorder = OptimalHclust
                @test_throws chomp("""
                                   specify heatmap graph.configuration.columns.reorder: OptimalHclust
                                   for explicit vector graph.data.columns.order
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("same") do
            graph.configuration.columns.reorder = SameOrder
            graph.data.entries.matrix = [
                0 1 2;
                7 6 5;
                8 9 10;
            ]

            nested_test("()") do
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.reorder: SameOrder
                                   without an order to copy from the rows
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            graph.data.rows.order = collect(1:3)

            nested_test("arrange_by") do
                graph.data.columns.arrange_by = graph.data.entries.matrix
                @test_throws chomp("""
                                   can't specify heatmap graph.data.columns.arrange_by
                                   for graph.configuration.columns.reorder: SameOrder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("linkage") do
                graph.configuration.columns.linkage = WardLinkage
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.linkage
                                   for graph.configuration.columns.reorder: SameOrder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("metric") do
                graph.configuration.columns.metric = Euclidean()
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.metric
                                   for graph.configuration.columns.reorder: SameOrder
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("dendogram_size") do
                graph.configuration.columns.dendogram_size = 0.1
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.dendogram_size
                                   with graph.configuration.columns.reorder: SameOrder
                                   without a tree to copy from the rows
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end
    end

    nested_test("arrange") do
        nested_test("columns") do
            graph.configuration.columns.reorder = OptimalHclust

            nested_test("features") do
                # A different number of feature rows is allowed; only the reordered (columns) count must match.
                graph.data.columns.arrange_by = Float32[1 2 3 4; 5 6 7 8]
                validate(ValidationContext(["graph"]), graph)
                return nothing
            end

            nested_test("mismatch") do
                graph.data.columns.arrange_by = Float32[1 2 3; 4 5 6]
                @test_throws chomp("""
                                   ArgumentError: invalid columns count of graph.data.columns.arrange_by: 3
                                   is different from length of graph.data.entries.matrix.columns: 4
                                   """) validate(ValidationContext(["graph"]), graph)
                return nothing
            end
        end

        nested_test("rows") do
            graph.configuration.rows.reorder = OptimalHclust

            nested_test("features") do
                # A different number of feature columns is allowed; only the reordered (rows) count must match.
                graph.data.rows.arrange_by = Float32[1 2; 3 4; 5 6]
                validate(ValidationContext(["graph"]), graph)
                return nothing
            end

            nested_test("mismatch") do
                graph.data.rows.arrange_by = Float32[1 2 3 4; 5 6 7 8]
                @test_throws chomp("""
                                   ArgumentError: invalid rows count of graph.data.rows.arrange_by: 2
                                   is different from length of graph.data.entries.matrix.rows: 3
                                   """) validate(ValidationContext(["graph"]), graph)
                return nothing
            end
        end
    end

    nested_test("()") do
        test_html(graph, "heatmap.html")
        return nothing
    end

    nested_test("names") do
        graph.data.rows.entities.names = ["X", "Y", "Z"]

        nested_test("()") do
            graph.data.columns.entities.names = ["A", "B", "C", "D"]
            test_html(graph, "heatmap.names.html")
            return nothing
        end

        # Naming one axis alone is normal when the other has too many entries to label.
        nested_test("rows") do
            test_html(graph, "heatmap.names.rows.html")
            return nothing
        end
    end

    nested_test("gaps") do
        graph.data.rows.entities.names = ["X", "Y", "Z"]
        graph.data.columns.entities.names = ["A", "B", "C", "D"]
        graph.data.columns.groups.vector = [1, 1, 2, 2]

        # The default fraction of four columns is less than one entry, so the gap stays at its minimum.
        nested_test("minimal") do
            test_html(graph, "heatmap.gaps.html")
            return nothing
        end

        # Asking for three quarters of the axis widens the single gap from one entry to three.
        nested_test("fraction") do
            graph.configuration.columns.total_gaps_fraction = 3 / 4
            test_html(graph, "heatmap.gaps.fraction.html")
            return nothing
        end

        # Without a fraction the gap is used as given, however many entries the axis holds.
        nested_test("none") do
            graph.configuration.columns.total_gaps_fraction = nothing
            test_html(graph, "heatmap.gaps.html")
            return nothing
        end

        nested_test("invalid") do
            nested_test("low") do
                graph.configuration.columns.total_gaps_fraction = 0
                @test_throws chomp("""
                                   too low graph.configuration.columns.total_gaps_fraction: 0
                                   is not above: 0
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("high") do
                graph.configuration.columns.total_gaps_fraction = 1
                @test_throws chomp("""
                                   too high graph.configuration.columns.total_gaps_fraction: 1
                                   is not below: 1
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end
    end

    nested_test("flip") do
        graph.data.rows.entities.names = ["X", "Y", "Z"]
        graph.data.columns.entities.names = ["A", "B", "C", "D"]
        graph.data.columns.groups.vector = [1, 1, 2, 2]
        graph.data.columns.subgroups.vector = ["P", "Q", "Q", "R"]
        graph.configuration.columns.subgroups_gap = 1
        graph.data.columns.arrange_by = Float32[1 2 3 4; 5 6 7 8]
        graph.configuration.columns.reorder = OptimalHclust
        graph.data.cells.hovers = [
            "XA" "XB" "XC" "XD";
            "YA" "YB" "YC" "YD";
            "ZA" "ZB" "ZC" "ZD";
        ]

        nested_test("()") do
            test_html(flip_axes(graph), "heatmap.flip.html")
            return nothing
        end

        nested_test("!") do
            flip_axes!(graph)
            test_html(graph, "heatmap.flip.html")
            return nothing
        end
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
        graph.configuration.entries.colors.scale.log_base = Log2Base
        graph.configuration.entries.colors.scale.log_regularization = 1
        test_html(graph, "heatmap.log2.html")
        return nothing
    end

    nested_test("legend") do
        graph.configuration.entries.colors.show_legend = true
        test_html(graph, "heatmap.legend.html")
        return nothing
    end

    nested_test("annotations") do
        graph.data.rows.annotations = [
            AnnotationData(;
                values = VectorValuesData(["yes", "maybe", "no"], "is"),
                colors = ColorsConfiguration(;
                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                ),
            ),
        ]
        graph.data.columns.annotations = [AnnotationData(; values = VectorValuesData([1, 0.5, 0, 1], "score"))]

        nested_test("()") do
            test_html(graph, "heatmap.annotations.html")
            return nothing
        end

        nested_test("automatic") do
            graph.data.rows.annotations[1].colors.palette = AutomaticColors()
            graph.data.rows.annotations[1].colors.show_legend = true
            test_html(graph, "heatmap.annotations.automatic.html")
            return nothing
        end

        nested_test("arrangement") do
            push!(graph.data.columns.annotations, AnnotationData(; values = VectorValuesData([0, 1, 1, 0], "mark")))

            nested_test("()") do
                test_html(graph, "heatmap.annotations.arrangement.html")
                return nothing
            end

            nested_test("order") do
                graph.data.columns.annotations_order = [2, 1]
                test_html(graph, "heatmap.annotations.arrangement.order.html")
                return nothing
            end

            nested_test("is_shown") do
                graph.data.columns.annotations[1].is_shown = false
                test_html(graph, "heatmap.annotations.arrangement.is_shown.html")
                return nothing
            end
        end

        nested_test("dendogram") do
            graph.configuration.rows.reorder = OptimalHclust
            graph.configuration.rows.dendogram_size = 0.2
            graph.configuration.columns.dendogram_size = 0.2

            nested_test("()") do
                test_html(graph, "heatmap.annotations.dendogram.html")
                return nothing
            end

            nested_test("gaps") do
                graph.data.rows.groups.vector = [1, 2, 2]
                graph.data.columns.groups.vector = [1, 1, 2, 3]
                graph.data.rows.entities.names = ["X", "Y", "Z"]
                graph.data.columns.entities.names = ["A", "B", "C", "D"]
                test_html(graph, "heatmap.annotations.dendogram.gaps.html")
                return nothing
            end
        end

        nested_test("gaps") do
            graph.data.rows.groups.vector = [1, 2, 2]
            graph.data.columns.groups.vector = [1, 1, 2, 3]
            graph.data.rows.entities.names = ["X", "Y", "Z"]
            graph.data.columns.entities.names = ["A", "B", "C", "D"]
            test_html(graph, "heatmap.annotations.gaps.html")
            return nothing
        end

        nested_test("legend") do
            graph.data.entries.title = "values"
            graph.configuration.entries.colors.show_legend = true
            graph.data.rows.annotations[1].colors.show_legend = true
            graph.data.columns.annotations[1].colors.show_legend = true
            graph.data.rows.entities.names = ["X", "Y", "Z"]
            graph.data.columns.entities.names = ["A", "B", "C", "D"]
            test_html(graph, "heatmap.annotations.legend.html")
            return nothing
        end

        nested_test("reorder") do
            nested_test("rows") do
                graph.data.rows.order = [1, 3, 2]
                test_html(graph, "heatmap.reorder.rows.html")
                return nothing
            end

            nested_test("columns") do
                graph.data.columns.order = [1, 3, 2, 4]
                test_html(graph, "heatmap.reorder.columns.html")
                return nothing
            end

            nested_test("both") do
                graph.data.rows.order = [1, 3, 2]
                graph.data.columns.order = [1, 3, 2, 4]
                test_html(graph, "heatmap.reorder.both.html")
                return nothing
            end

            nested_test("dendogram") do
                graph.data.rows.order = [1, 3, 2]
                graph.data.columns.order = [1, 3, 2, 4]
                graph.configuration.rows.dendogram_size = 0.2
                graph.configuration.columns.dendogram_size = 0.2
                graph.configuration.columns.reorder = ReorderHclust
                return test_html(graph, "heatmap.reorder.dendogram.html")
            end

            nested_test("ward") do
                graph.data.entries.matrix = graph.data.entries.matrix[[1, 3, 2], [1, 3, 2, 4]]
                graph.configuration.rows.reorder = OptimalHclust
                graph.configuration.columns.reorder = OptimalHclust
                test_html(graph, "heatmap.reorder.ward.html")
                return nothing
            end

            nested_test("slant") do
                nested_test("rows") do
                    graph.configuration.rows.reorder = SlantedOrder

                    nested_test("()") do
                        test_html(graph, "heatmap.reorder.slanted.rows.html")
                        return nothing
                    end

                    nested_test("same") do
                        graph.data.entries.matrix = [
                            0 1 2;
                            7 6 5;
                            8 9 10;
                        ]
                        pop!(graph.data.columns.annotations[1].values.vector)
                        graph.configuration.columns.reorder = SameOrder
                        return test_html(graph, "heatmap.reorder.slanted.rows.same.html")
                    end
                end

                nested_test("columns") do
                    graph.configuration.columns.reorder = SlantedOrder

                    nested_test("()") do
                        test_html(graph, "heatmap.reorder.slanted.columns.html")
                        return nothing
                    end

                    nested_test("same") do
                        graph.data.entries.matrix = [
                            0 1 2;
                            7 6 5;
                            8 9 10;
                        ]
                        pop!(graph.data.columns.annotations[1].values.vector)
                        graph.configuration.rows.reorder = SameOrder
                        return test_html(graph, "heatmap.reorder.slanted.columns.same.html")
                    end
                end

                nested_test("both") do
                    graph.configuration.rows.reorder = SlantedOrder
                    graph.configuration.columns.reorder = SlantedOrder
                    test_html(graph, "heatmap.reorder.slanted.both.html")
                    return nothing
                end

                nested_test("hclust") do
                    graph.configuration.rows.reorder = SlantedHclust
                    graph.configuration.columns.reorder = SlantedHclust
                    test_html(graph, "heatmap.reorder.slanted.hclust.html")
                    return nothing
                end
            end
        end
    end

    nested_test("hclust") do
        distances = pairwise(Euclidean(), graph.data.entries.matrix; dims = 2)
        graph.data.columns.order = hclust(distances)

        nested_test("()") do
            test_html(graph, "heatmap.hclust.html")
            return nothing
        end

        nested_test("dendogram") do
            graph.configuration.rows.reorder = OptimalHclust
            graph.configuration.rows.dendogram_size = 0.2
            graph.configuration.columns.dendogram_size = 0.2

            nested_test("()") do
                test_html(graph, "heatmap.hclust.dendogram.html")
                return nothing
            end

            nested_test("gaps") do
                graph.data.rows.groups.vector = [1, 2, 2]
                graph.data.columns.groups.vector = [1, 1, 2, 3]
                graph.data.rows.entities.names = ["X", "Y", "Z"]
                graph.data.columns.entities.names = ["A", "B", "C", "D"]
                test_html(graph, "heatmap.dendogram.gaps.html")
                return nothing
            end
        end

        nested_test("slanted") do
            graph.configuration.columns.reorder = SlantedHclust
            test_html(graph, "heatmap.hclust.slanted.html")
            return nothing
        end
    end

    nested_test("same") do
        graph = heatmap_graph(; entries = MatrixValuesData([
            0 1 2;
            7 6 5;
            8 9 10;
        ]))
        nested_test("rows") do
            graph.configuration.rows.reorder = SameOrder
            graph.data.columns.order = [1, 3, 2]
            test_html(graph, "heatmap.reorder.rows=columns.html")
            return nothing
        end

        nested_test("columns") do
            graph.data.rows.order = [1, 3, 2]
            graph.configuration.columns.reorder = SameOrder
            test_html(graph, "heatmap.reorder.columns=rows.html")
            return nothing
        end

        # The rows copy the columns order, which was clustered without the hidden column; both put it last.
        nested_test("!hidden") do
            graph.data.columns.entities.mask = [true, false, true]
            graph.configuration.columns.reorder = OptimalHclust
            graph.configuration.columns.include_hidden = false
            graph.configuration.rows.reorder = SameOrder
            test_html(graph, "heatmap.reorder.rows=columns.!hidden.html")
            @test graph.order.rows_order == graph.order.columns_order
            @test graph.order.columns_order[end] == 2
            return nothing
        end
    end

    nested_test("mask") do
        graph.data.rows.entities.names = ["X", "Y", "Z"]
        graph.data.columns.entities.names = ["A", "B", "C", "D"]
        graph.data.rows.entities.hovers = ["R:X", "R:Y", "R:Z"]
        graph.data.columns.entities.hovers = ["C:A", "C:B", "C:C", "C:D"]
        graph.data.cells.hovers = [
            "XA" "XB" "XC" "XD";
            "YA" "YB" "YC" "YD";
            "ZA" "ZB" "ZC" "ZD";
        ]
        graph.data.rows.annotations = [AnnotationData(; values = VectorValuesData([1, 0.5, 0], "score"))]
        graph.data.columns.annotations = [
            AnnotationData(;
                values = VectorValuesData(["yes", "maybe", "no", "yes"], "is"),
                colors = ColorsConfiguration(;
                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                ),
            ),
        ]

        nested_test("rows") do
            nested_test("()") do
                graph.data.rows.entities.mask = [true, false, true]
                test_html(graph, "heatmap.mask.rows.html")
                return nothing
            end

            nested_test("!hidden") do
                graph.data.rows.entities.mask = [true, true, false]
                graph.data.rows.annotations[1].colors.scale.include_hidden = false
                test_html(graph, "heatmap.mask.rows.!hidden.html")
                return nothing
            end

            # A cell is hidden when its row is, so the hidden row leaves the range of the colors scale.
            nested_test("!colors") do
                graph.data.rows.entities.mask = [true, true, false]
                @test graph.figure.layout[:coloraxis][:cmax] == 11
                graph.configuration.entries.colors.scale.include_hidden = false
                @test graph.figure.layout[:coloraxis][:cmax] == 7

                # With no mask at all there is nothing to leave out, so the range covers everything again.
                graph.data.rows.entities.mask = nothing
                @test graph.figure.layout[:coloraxis][:cmax] == 11
                return nothing
            end
        end

        nested_test("columns") do
            graph.data.columns.entities.mask = [true, false, true, false]
            test_html(graph, "heatmap.mask.columns.html")
            return nothing
        end

        nested_test("both") do
            graph.data.rows.entities.mask = [true, false, true]
            graph.data.columns.entities.mask = [true, false, true, true]
            graph.data.rows.groups.vector = [1, 2, 2]
            graph.data.columns.groups.vector = [1, 1, 2, 3]
            test_html(graph, "heatmap.mask.both.html")
            return nothing
        end

        # The order describes all the rows, hidden ones included.
        nested_test("order") do
            graph.data.rows.entities.mask = [true, false, true]
            graph.data.rows.order = [3, 2, 1]
            test_html(graph, "heatmap.mask.order.html")
            @test graph.order.rows_order == [3, 2, 1]
            return nothing
        end

        # Rows clustered without the hidden row, which comes last; the columns are left alone.
        nested_test("!hidden") do
            graph.data.rows.entities.mask = [true, false, true]
            graph.configuration.rows.include_hidden = false

            nested_test("order") do
                graph.data.rows.order = [3, 2, 1]
                graph.configuration.rows.reorder = ReorderHclust
                @test graph.order.rows_order == [3, 1, 2]
                return nothing
            end

            nested_test("arrange_by") do
                graph.data.rows.arrange_by = Float32[1 2; 3 4; 5 6]
                graph.configuration.rows.reorder = OptimalHclust
                @test sort(graph.order.rows_order) == 1:3
                @test graph.order.rows_order[end] == 2
                @test graph.order.columns_order == 1:4
                return nothing
            end
        end

        # The clustering sees all the columns, hidden ones included; only the drawn tree is pruned.
        nested_test("dendogram") do
            graph.configuration.columns.reorder = OptimalHclust
            graph.configuration.columns.dendogram_size = 0.2

            nested_test("()") do
                graph.data.columns.entities.mask = [true, false, true, true]
                test_html(graph, "heatmap.mask.dendogram.html")
                @test sort(graph.order.columns_order) == 1:4
                @test graph.order.columns_hclust.order == graph.order.columns_order

                other_graph = heatmap_graph(; entries = MatrixValuesData(graph.data.entries.matrix))
                other_graph.data.columns.order = graph.order.columns_hclust
                @test other_graph.order.columns_order == graph.order.columns_order
                return nothing
            end

            nested_test("first") do
                graph.data.columns.entities.mask = [false, true, true, true]
                test_html(graph, "heatmap.mask.dendogram.first.html")
                return nothing
            end

            # The clustering sees only the shown columns; the order and the tree still cover all of them, hidden last.
            nested_test("!hidden") do
                graph.data.columns.entities.mask = [true, false, true, true]
                graph.configuration.columns.include_hidden = false
                test_html(graph, "heatmap.mask.dendogram.!hidden.html")
                @test sort(graph.order.columns_order) == 1:4
                @test graph.order.columns_order[end] == 2
                @test graph.order.columns_hclust.order == graph.order.columns_order

                other_graph = heatmap_graph(; entries = MatrixValuesData(graph.data.entries.matrix))
                other_graph.data.columns.entities.mask = graph.data.columns.entities.mask
                other_graph.data.columns.order = graph.order.columns_hclust
                @test other_graph.order.columns_order == graph.order.columns_order
                return nothing
            end
        end
    end

    nested_test("hovers") do
        graph.data.rows.entities.names = ["X", "Y", "Z"]
        graph.data.columns.entities.names = ["A", "B", "C", "D"]

        nested_test("entries") do
            graph.data.cells.hovers = [
                "XA" "XB" "XC" "XD";
                "YA" "YB" "YC" "YD";
                "ZA" "ZB" "ZC" "ZD";
            ]

            nested_test("()") do
                test_html(graph, "heatmap.hovers.entries.html")
                return nothing
            end

            nested_test("gaps") do
                graph.data.rows.groups.vector = [1, 2, 2]
                graph.data.columns.groups.vector = [1, 1, 2, 3]
                graph.data.rows.entities.names = ["X", "Y", "Z"]
                graph.data.columns.entities.names = ["A", "B", "C", "D"]
                test_html(graph, "heatmap.hovers.entries.gaps.html")
                return nothing
            end
        end

        nested_test("axes") do
            graph.data.rows.entities.hovers = ["R:X", "R:Y", "R:Z"]
            graph.data.columns.entities.hovers = ["C:A", "C:B", "C:C", "C:D"]

            nested_test("()") do
                test_html(graph, "heatmap.hovers.axes.html")
                return nothing
            end

            nested_test("gaps") do
                graph.data.rows.groups.vector = [1, 2, 2]
                graph.data.columns.groups.vector = [1, 1, 2, 3]
                graph.data.rows.entities.names = ["X", "Y", "Z"]
                graph.data.columns.entities.names = ["A", "B", "C", "D"]
                test_html(graph, "heatmap.hovers.axes.gaps.html")
                return nothing
            end
        end

        nested_test("both") do
            graph.data.cells.hovers = [
                "XA" "XB" "XC" "XD";
                "YA" "YB" "YC" "YD";
                "ZA" "ZB" "ZC" "ZD";
            ]
            graph.data.rows.entities.hovers = ["R:X", "R:Y", "R:Z"]
            graph.data.columns.entities.hovers = ["C:A", "C:B", "C:C", "C:D"]

            nested_test("()") do
                test_html(graph, "heatmap.hovers.both.html")
                return nothing
            end

            nested_test("gaps") do
                graph.data.rows.groups.vector = [1, 2, 2]
                graph.data.columns.groups.vector = [1, 1, 2, 3]
                graph.data.rows.entities.names = ["X", "Y", "Z"]
                graph.data.columns.entities.names = ["A", "B", "C", "D"]
                test_html(graph, "heatmap.hovers.both.gaps.html")
                return nothing
            end
        end
    end

    nested_test("order") do
        nested_test("()") do
            @test graph.order.rows_order == 1:3
            @test graph.order.columns_order == 1:4
            @test graph.order.rows_hclust === nothing
            @test graph.order.columns_hclust === nothing
            return nothing
        end

        nested_test("only") do
            graph.order
            @test graph.configuration.final_order === graph.order
            graph.figure
            @test graph.configuration.final_order === graph.order
            return nothing
        end

        # The groups constrain the clustering, so they change the order - but only once the cache is reset.
        nested_test("reset") do
            graph.configuration.columns.reorder = OptimalHclust
            graph.data.columns.groups.vector = [1, 1, 2, 2]
            grouped_order = graph.order.columns_order

            graph.data.columns.groups.vector = [1, 2, 2, 1]
            @test graph.order.columns_order == grouped_order

            reset_order!(graph)
            @test graph.configuration.final_order === nothing
            @test graph.order.columns_order != grouped_order
            return nothing
        end

        nested_test("reorder") do
            graph.configuration.columns.reorder = OptimalHclust
            @test sort(graph.order.columns_order) == 1:4
            @test graph.order.columns_hclust !== nothing

            nested_test("vector") do
                other_graph = heatmap_graph(; entries = MatrixValuesData(graph.data.entries.matrix))
                other_graph.data.columns.order = graph.order.columns_order
                @test other_graph.order.columns_order == graph.order.columns_order
                @test other_graph.json == graph.json
                return nothing
            end

            nested_test("hclust") do
                other_graph = heatmap_graph(; entries = MatrixValuesData(reverse(graph.data.entries.matrix; dims = 1)))
                other_graph.data.columns.order = graph.order.columns_hclust
                @test other_graph.order.columns_order == graph.order.columns_order
                return nothing
            end

            # The order is that of the data, so it is unaffected by which corner the origin is displayed at, and can be
            # fed back into a graph with the same origin without being flipped a second time.
            nested_test("origin") do
                columns_order = graph.order.columns_order
                graph.configuration.origin = HeatmapTopLeft
                graph.configuration.final_order = nothing
                @test graph.order.columns_order == columns_order
                @test graph.order.rows_order == 1:3

                other_graph = heatmap_graph(; entries = MatrixValuesData(graph.data.entries.matrix))
                other_graph.data.columns.order = columns_order
                other_graph.configuration.origin = HeatmapTopLeft
                @test other_graph.json == graph.json
                return nothing
            end
        end

        nested_test("same") do
            graph.data.entries.matrix = [
                0 1 2;
                7 6 5;
                8 9 10;
            ]
            graph.configuration.rows.reorder = OptimalHclust
            graph.configuration.columns.reorder = SameOrder
            @test graph.order.columns_order == graph.order.rows_order
            return nothing
        end
    end

    # The distinct labels of the entries, in the order they appear in, which has one entry per label if (and only if)
    # each label covers a contiguous range of the order.
    function labels_in_order(order::AbstractVector{<:Integer}, label_per_entry::AbstractVector)::Vector
        labels = label_per_entry[order]
        return labels[[true; labels[2:end] .!= labels[1:(end - 1)]]]
    end

    nested_test("subgroups") do
        # Two columns in each of six subgroups, three subgroups in each of two groups, numbered in the opposite order
        # of their groups. The columns of the different subgroups are almost identical, so clustering them without the
        # groups interleaves the subgroups completely.
        values = reshape([Float64(1 + (index - 1) % 2) + 0.01 * ((index - 1) ÷ 2) for index in 1:12], 1, :)
        subgroups = repeat(1:6; inner = 2)
        groups = [subgroup <= 3 ? 2 : 1 for subgroup in subgroups]

        graph = heatmap_graph(; entries = MatrixValuesData(vcat(values, values .* 2)))
        graph.data.columns.groups.vector = groups
        graph.data.columns.subgroups.vector = ["S$(subgroup)" for subgroup in subgroups]
        graph.configuration.columns.reorder = OptimalHclust

        nested_test("()") do
            columns_order = graph.order.columns_order

            # Each group, and each subgroup, is contiguous; the numbered groups are in the order of their numbers, and
            # the named subgroups are wherever the clustering placed them.
            @test labels_in_order(columns_order, groups) == [1, 2]
            @test length(labels_in_order(columns_order, subgroups)) == 6
            return nothing
        end

        nested_test("numbered") do
            graph.data.columns.subgroups.vector = subgroups
            columns_order = graph.order.columns_order

            # Numbering both levels lays the columns out in the order of their (group, subgroup) pair, which is not the
            # order of the subgroups alone.
            @test labels_in_order(columns_order, groups) == [1, 2]
            @test labels_in_order(columns_order, subgroups) == [4, 5, 6, 1, 2, 3]
            return nothing
        end

        # A subgroup is nested in its group, so each group may number its own subgroups the same way.
        nested_test("reused") do
            graph.data.columns.subgroups.vector = repeat(1:3; inner = 2, outer = 2)
            columns_order = graph.order.columns_order
            @test labels_in_order(columns_order, groups) == [1, 2]
            @test length(labels_in_order(columns_order, subgroups)) == 6
            @test labels_in_order(columns_order, graph.data.columns.subgroups.vector) == [1, 2, 3, 1, 2, 3]
            return nothing
        end

        nested_test("gaps") do
            graph.configuration.columns.subgroups_gap = 1
            columns_order = graph.order.columns_order

            # A gap between the groups, and a gap between the subgroups of each group; the boundary between the groups
            # is gapped once, as a group boundary.
            test_html(graph, "heatmap.subgroups.gaps.html")

            # The gaps are drawn, but do not change the order.
            graph.configuration.columns.subgroups_gap = nothing
            reset_order!(graph)
            @test graph.order.columns_order == columns_order
            return nothing
        end

        nested_test("invalid") do
            nested_test("groups") do
                graph.data.columns.groups.vector = nothing
                @test_throws "ArgumentError: can't specify heatmap graph.data.columns.subgroups.vector without columns.groups.vector" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end

            nested_test("effect") do
                graph.configuration.columns.reorder = nothing
                @test_throws "ArgumentError: no effect for specified graph.data.columns.subgroups.vector" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end

            nested_test("gap") do
                graph.data.columns.subgroups.vector = nothing
                graph.configuration.columns.subgroups_gap = 1
                @test_throws chomp("""
                                   can't specify heatmap graph.configuration.columns.subgroups_gap
                                   without graph.data.columns.subgroups.vector
                                   """) validate(ValidationContext(["graph"]), graph)
            end

            nested_test("title") do
                graph.data.columns.groups.title = "Groups"
                @test_throws "ArgumentError: can't specify heatmap graph.data.columns.groups.title" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end
        end
    end
end
