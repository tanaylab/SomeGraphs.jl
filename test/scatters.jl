nested_test("points") do
    graph = points_graph(; points_xs = collect(0:10) .* 10, points_ys = collect(0:10) .^ 2)

    nested_test("invalid") do
        context = ValidationContext(["graph"])

        nested_test("diagonal") do
            nested_test("log") do
                graph.configuration.x_axis.log_scale = Log10Scale
                graph.configuration.x_axis.log_regularization = 1

                nested_test("configuration") do
                    graph.configuration.diagonal_bands.middle.offset = 1
                    @test_throws "diagonal bands require graph.configuration.(x_axis.log_scale == y_axis.log_scale)" validate(
                        context,
                        graph,
                    )
                end

                nested_test("data") do
                    graph.data.diagonal_bands.middle_offset = 1
                    @test_throws "diagonal bands require graph.configuration.(x_axis.log_scale == y_axis.log_scale)" validate(
                        context,
                        graph,
                    )
                end
            end

            nested_test("percent") do
                graph.configuration.x_axis.percent = true

                nested_test("configuration") do
                    graph.configuration.diagonal_bands.middle.offset = 1
                    @test_throws "diagonal bands require graph.configuration.(x_axis.percent == y_axis.percent)" validate(
                        context,
                        graph,
                    )
                end

                nested_test("data") do
                    graph.data.diagonal_bands.middle_offset = 1
                    @test_throws "diagonal bands require graph.configuration.(x_axis.percent == y_axis.percent)" validate(
                        context,
                        graph,
                    )
                end
            end
        end
    end

    nested_test("()") do
        test_html(graph, "points.html")
        return nothing
    end

    nested_test("mask") do
        graph.data.points_mask = [true, true, true, true, true, true, false, false, false, false, false]
        test_html(graph, "points.mask.html")
        return nothing
    end

    nested_test("hovers") do
        graph.data.points_hovers = ["H: $(index)" for index in 1:11]
        test_html(graph, "points.hovers.html")
        return nothing
    end

    nested_test("color") do
        graph.configuration.points.colors.fixed = "red"
        test_html(graph, "points.color.html")
        return nothing
    end

    nested_test("horizontal") do
        graph.configuration.horizontal_bands.low.offset = 25
        graph.configuration.horizontal_bands.middle.offset = 50
        graph.configuration.horizontal_bands.high.offset = 75

        nested_test("()") do
            test_html(graph, "points.horizontal.html")
            return nothing
        end

        nested_test("fill") do
            graph.configuration.horizontal_bands.low.line.is_filled = true
            graph.configuration.horizontal_bands.middle.line.is_filled = true
            graph.configuration.horizontal_bands.high.line.is_filled = true

            graph.configuration.horizontal_bands.low.line.color = "green"
            graph.configuration.horizontal_bands.middle.line.color = "red"
            graph.configuration.horizontal_bands.high.line.color = "blue"

            test_html(graph, "points.horizontal.fill.html")
            return nothing
        end
    end

    nested_test("vertical") do
        graph.configuration.vertical_bands.low.offset = 25
        graph.configuration.vertical_bands.middle.offset = 50
        graph.configuration.vertical_bands.high.offset = 75

        nested_test("()") do
            test_html(graph, "points.vertical.html")
            return nothing
        end

        nested_test("fill") do
            graph.configuration.vertical_bands.low.line.is_filled = true
            graph.configuration.vertical_bands.middle.line.is_filled = true
            graph.configuration.vertical_bands.high.line.is_filled = true

            graph.configuration.vertical_bands.low.line.color = "green"
            graph.configuration.vertical_bands.middle.line.color = "red"
            graph.configuration.vertical_bands.high.line.color = "blue"

            test_html(graph, "points.vertical.fill.html")
            return nothing
        end
    end

    nested_test("diagonal") do
        nested_test("()") do
            graph.configuration.diagonal_bands.low.offset = -25
            graph.configuration.diagonal_bands.middle.offset = 0
            graph.configuration.diagonal_bands.high.offset = +25

            nested_test("()") do
                test_html(graph, "points.diagonal.html")
                return nothing
            end

            nested_test("fill") do
                graph.configuration.diagonal_bands.low.line.is_filled = true
                graph.configuration.diagonal_bands.middle.line.is_filled = true
                graph.configuration.diagonal_bands.high.line.is_filled = true

                graph.configuration.diagonal_bands.low.line.color = "green"
                graph.configuration.diagonal_bands.middle.line.color = "red"
                graph.configuration.diagonal_bands.high.line.color = "blue"

                test_html(graph, "points.diagonal.fill.html")
                return nothing
            end
        end

        nested_test("left") do
            graph.configuration.diagonal_bands.low.offset = 25
            graph.configuration.diagonal_bands.middle.offset = 50
            graph.configuration.diagonal_bands.high.offset = 75

            nested_test("()") do
                test_html(graph, "points.diagonal.left.html")
                return nothing
            end

            nested_test("fill") do
                graph.configuration.diagonal_bands.low.line.is_filled = true
                graph.configuration.diagonal_bands.middle.line.is_filled = true
                graph.configuration.diagonal_bands.high.line.is_filled = true

                graph.configuration.diagonal_bands.low.line.color = "green"
                graph.configuration.diagonal_bands.middle.line.color = "red"
                graph.configuration.diagonal_bands.high.line.color = "blue"

                test_html(graph, "points.diagonal.left.fill.html")
                return nothing
            end
        end

        nested_test("right") do
            graph.configuration.diagonal_bands.low.offset = -75
            graph.configuration.diagonal_bands.middle.offset = -50
            graph.configuration.diagonal_bands.high.offset = -25

            nested_test("()") do
                test_html(graph, "points.diagonal.right.html")
                return nothing
            end

            nested_test("fill") do
                graph.configuration.diagonal_bands.low.line.is_filled = true
                graph.configuration.diagonal_bands.middle.line.is_filled = true
                graph.configuration.diagonal_bands.high.line.is_filled = true

                graph.configuration.diagonal_bands.low.line.color = "green"
                graph.configuration.diagonal_bands.middle.line.color = "red"
                graph.configuration.diagonal_bands.high.line.color = "blue"

                test_html(graph, "points.diagonal.right.fill.html")
                return nothing
            end
        end
    end

    nested_test("log") do
        graph.configuration.x_axis.log_scale = Log10Scale
        graph.configuration.x_axis.log_regularization = 1
        graph.configuration.y_axis.log_scale = Log10Scale
        graph.configuration.y_axis.log_regularization = 1

        nested_test("()") do
            test_html(graph, "points.log.html")
            return nothing
        end

        nested_test("diagonal") do
            graph.configuration.diagonal_bands.low.offset = 1 / sqrt(10)
            graph.configuration.diagonal_bands.middle.offset = 1
            graph.configuration.diagonal_bands.high.offset = sqrt(10)
            test_html(graph, "points.log.diagonal.html")
            return nothing
        end

        nested_test("hovers") do
            graph.data.points_hovers = ["H: $(index)" for index in 1:11]
            test_html(graph, "points.log.hovers.html")
            return nothing
        end
    end

    nested_test("edges") do
        graph.data.edges_points = [(1, 7), (2, 8), (3, 9), (4, 10), (5, 11)]
        nested_test("()") do
            test_html(graph, "points.edges.html")
            return nothing
        end

        nested_test("mask") do
            graph.data.edges_mask = [true, true, true, false, false]
            test_html(graph, "points.edges.mask.html")
            return nothing
        end

        nested_test("below") do
            graph.configuration.edges_over_points = false
            test_html(graph, "points.edges.below.html")
            return nothing
        end

        nested_test("style") do
            graph.configuration.edges_style = DashLine
            test_html(graph, "points.edges.style.html")
            return nothing
        end

        nested_test("styles") do
            graph.data.edges_styles = [SolidLine, DashLine, DashDotLine, DotLine, SolidLine]
            test_html(graph, "points.edges.styles.html")
            return nothing
        end

        nested_test("continuous") do
            graph.data.edges_colors = collect(1:5)

            nested_test("()") do
                test_html(graph, "points.edges.continuous.html")
                return nothing
            end

            nested_test("legend") do
                graph.configuration.edges.colors.show_legend = true
                test_html(graph, "points.edges.continuous.legend.html")
                return nothing
            end
        end

        nested_test("named") do
            graph.data.edges_colors = ["red", "yellow", "green", "cyan", "blue"]
            test_html(graph, "points.edges.named.html")
            return nothing
        end

        nested_test("categorical") do
            graph.configuration.edges.colors.palette = Dict("Foo" => "red", "Bar" => "green", "Baz" => "blue")
            graph.data.edges_colors = ["Foo", "Bar", "Baz", "Bar", "Foo"]

            nested_test("()") do
                test_html(graph, "points.edges.categorical.html")
                return nothing
            end

            nested_test("legend") do
                graph.configuration.edges.colors.show_legend = true
                test_html(graph, "points.edges.categorical.legend.html")
                return nothing
            end
        end

        nested_test("size") do
            graph.configuration.edges.sizes.fixed = 12
            test_html(graph, "points.edges.size.html")
            return nothing
        end

        nested_test("sizes") do
            graph.data.edges_sizes = collect(1:5)
            test_html(graph, "points.edges.sizes.html")
            return nothing
        end
    end

    nested_test("continuous") do
        graph.data.points_colors = collect(0:10)

        nested_test("()") do
            test_html(graph, "points.continuous.html")
            return nothing
        end

        nested_test("named") do
            graph.configuration.points.colors.palette = "Viridis"
            test_html(graph, "points.continuous.named.html")
            return nothing
        end

        nested_test("scale") do
            graph.configuration.points.colors.palette = [-1 => "red", 10 => "blue"]
            nested_test("()") do
                test_html(graph, "points.continuous.scale.html")
                return nothing
            end

            nested_test("legend") do
                graph.configuration.points.colors.show_legend = true
                test_html(graph, "points.continuous.scale.legend.html")
                return nothing
            end
        end

        nested_test("legend") do
            graph.configuration.points.colors.show_legend = true
            test_html(graph, "points.continuous.legend.html")
            return nothing
        end
    end

    nested_test("named") do
        graph.data.points_colors =
            ["red", "yellow", "green", "cyan", "blue", "magenta", "blue", "cyan", "green", "yellow", "red"]
        test_html(graph, "points.named.html")
        return nothing
    end

    nested_test("categorical") do
        graph.configuration.points.colors.palette = Dict("Foo" => "red", "Bar" => "green", "Baz" => "blue")
        graph.data.points_colors = ["Foo", "Bar", "Baz", "Bar", "Foo", "Bar", "Baz", "Bar", "Foo", "Bar", "Baz"]

        nested_test("()") do
            test_html(graph, "points.categorical.html")
            return nothing
        end

        nested_test("legend") do
            graph.configuration.points.colors.show_legend = true
            test_html(graph, "points.categorical.legend.html")
            return nothing
        end

        nested_test("mask") do
            graph.data.points_mask = [true, true, true, true, true, true, false, false, false, false, false]
            test_html(graph, "points.categorical.mask.html")
            return nothing
        end
    end

    nested_test("size") do
        graph.configuration.points.sizes.fixed = 12
        test_html(graph, "points.size.html")
        return nothing
    end

    nested_test("sizes") do
        graph.data.points_sizes = collect(0:10)
        test_html(graph, "points.sizes.html")
        return nothing
    end

    nested_test("borders") do
        nested_test("continuous") do
            graph.data.borders_colors = collect(0:10)

            nested_test("()") do
                test_html(graph, "points.borders.continuous.html")
                return nothing
            end

            nested_test("legend") do
                graph.configuration.borders.colors.show_legend = true
                test_html(graph, "points.borders.continuous.legend.html")
                return nothing
            end
        end

        nested_test("named") do
            graph.data.borders_colors =
                ["red", "yellow", "green", "cyan", "blue", "magenta", "blue", "cyan", "green", "yellow", "red"]
            test_html(graph, "points.borders.named.html")
            return nothing
        end

        nested_test("categorical") do
            graph.configuration.borders.colors.palette = Dict("Foo" => "red", "Bar" => "green", "Baz" => "blue")
            graph.data.borders_colors = ["Foo", "Bar", "Baz", "Bar", "Foo", "Bar", "Baz", "Bar", "Foo", "Bar", "Baz"]

            nested_test("()") do
                test_html(graph, "points.borders.categorical.html")
                return nothing
            end

            nested_test("legend") do
                graph.configuration.borders.colors.show_legend = true
                test_html(graph, "points.borders.categorical.legend.html")
                return nothing
            end
        end

        nested_test("size") do
            graph.configuration.borders.sizes.fixed = 12
            test_html(graph, "points.borders.size.html")
            return nothing
        end

        nested_test("sizes") do
            nested_test("()") do
                graph.data.borders_sizes = collect(0:10)
                test_html(graph, "points.borders.sizes.html")
                return nothing
            end

            nested_test("size") do
                graph.configuration.points.sizes.fixed = 12
                graph.data.borders_sizes = collect(0:10)
                test_html(graph, "points.borders.sizes.size.html")
                return nothing
            end

            nested_test("sizes") do
                graph.data.points_sizes = reverse(collect(0:10))
                graph.data.borders_sizes = collect(0:10)
                test_html(graph, "points.borders.sizes.sizes.html")
                return nothing
            end
        end
    end

    nested_test("offsets") do
        graph.data.edges_points = [(1, 7), (2, 8), (3, 9), (4, 10), (5, 11)]

        graph.data.figure_title = "Figure"
        graph.data.x_axis_title = "X Axis"
        graph.data.y_axis_title = "Y Axis"

        graph.data.points_colors_title = "Points"
        graph.data.borders_colors_title = "Borders"
        graph.data.edges_colors_title = "Edges"

        graph.configuration.borders.colors.show_legend = true
        graph.configuration.points.colors.show_legend = true
        graph.configuration.edges.colors.show_legend = true

        nested_test("no-colors") do
            graph.configuration.points.colors.palette = Dict("Foo-P" => "red", "Bar-P" => "green", "Baz-P" => "blue")
            graph.data.points_colors =
                ["Foo-P", "Bar-P", "Baz-P", "Bar-P", "Foo-P", "Bar-P", "Baz-P", "Bar-P", "Foo-P", "Bar-P", "Baz-P"]

            graph.configuration.borders.colors.palette = Dict("Foo-B" => "red", "Bar-B" => "green", "Baz-B" => "blue")
            graph.data.borders_colors =
                ["Foo-B", "Bar-B", "Baz-B", "Bar-B", "Foo-B", "Bar-B", "Baz-B", "Bar-B", "Foo-B", "Bar-B", "Baz-B"]

            graph.configuration.edges.colors.palette = Dict("Foo-E" => "red", "Bar-E" => "green", "Baz-E" => "blue")
            graph.data.edges_colors = ["Foo-E", "Bar-E", "Baz-E", "Bar-E", "Foo-E"]

            test_html(graph, "points.offsets.no-colors.html")
            return nothing
        end

        nested_test("one-colors") do
            graph.configuration.points.colors.palette = [0 => "red", 10 => "blue"]
            graph.data.points_colors = collect(0:10)

            graph.configuration.borders.colors.palette = Dict("Foo-B" => "red", "Bar-B" => "green", "Baz-B" => "blue")
            graph.data.borders_colors =
                ["Foo-B", "Bar-B", "Baz-B", "Bar-B", "Foo-B", "Bar-B", "Baz-B", "Bar-B", "Foo-B", "Bar-B", "Baz-B"]

            graph.configuration.edges.colors.palette = Dict("Foo-E" => "red", "Bar-E" => "green", "Baz-E" => "blue")
            graph.data.edges_colors = ["Foo-E", "Bar-E", "Baz-E", "Bar-E", "Foo-E"]

            test_html(graph, "points.offsets.one-colors.html")
            return nothing
        end

        nested_test("two-colors") do
            graph.configuration.points.colors.palette = [0 => "red", 10 => "blue"]
            graph.data.points_colors = collect(0:10)

            graph.configuration.borders.colors.palette = [0 => "blue", 10 => "green"]
            graph.data.borders_colors = collect(0:10)

            graph.configuration.edges.colors.palette = Dict("Foo-E" => "red", "Bar-E" => "green", "Baz-E" => "blue")
            graph.data.edges_colors = ["Foo-E", "Bar-E", "Baz-E", "Bar-E", "Foo-E"]

            test_html(graph, "points.offsets.two-colors.html")
            return nothing
        end

        nested_test("all-colors") do
            graph.configuration.points.colors.palette = [0 => "red", 10 => "blue"]
            graph.data.points_colors = collect(0:10)

            graph.configuration.borders.colors.palette = [0 => "blue", 10 => "green"]
            graph.data.borders_colors = collect(0:10)

            graph.configuration.edges.colors.palette = [0 => "green", 4 => "red"]
            graph.data.edges_colors = collect(0:4)

            context = ValidationContext(["graph"])
            @test_throws "can't specify show_legend in more than two continuous color configurations" validate(
                context,
                graph,
            )
        end
    end
end

nested_test("line") do
    graph = line_graph(; points_xs = collect(0:10) .* 10, points_ys = collect(0:10) .^ 2)

    nested_test("invalid") do
        context = ValidationContext(["graph"])

        nested_test("points_size") do
            graph.configuration.points_size = 6

            @test_throws "can't specify graph.configuration.points_size w/o graph.configuration.show_points" validate(
                context,
                graph,
            )
        end

        nested_test("points_color") do
            graph.configuration.points_color = "red"

            @test_throws "can't specify graph.configuration.points_color w/o graph.configuration.show_points" validate(
                context,
                graph,
            )
        end

        nested_test("diagonal") do
            nested_test("log") do
                graph.configuration.x_axis.log_scale = Log10Scale
                graph.configuration.x_axis.log_regularization = 1

                nested_test("configuration") do
                    graph.configuration.diagonal_bands.middle.offset = 1
                    @test_throws "diagonal bands require graph.configuration.(x_axis.log_scale == y_axis.log_scale)" validate(
                        context,
                        graph,
                    )
                end

                nested_test("data") do
                    graph.data.diagonal_bands.middle_offset = 1
                    @test_throws "diagonal bands require graph.configuration.(x_axis.log_scale == y_axis.log_scale)" validate(
                        context,
                        graph,
                    )
                end
            end

            nested_test("percent") do
                graph.configuration.x_axis.percent = true

                nested_test("configuration") do
                    graph.configuration.diagonal_bands.middle.offset = 1
                    @test_throws "diagonal bands require graph.configuration.(x_axis.percent == y_axis.percent)" validate(
                        context,
                        graph,
                    )
                end

                nested_test("data") do
                    graph.data.diagonal_bands.middle_offset = 1
                    @test_throws "diagonal bands require graph.configuration.(x_axis.percent == y_axis.percent)" validate(
                        context,
                        graph,
                    )
                end
            end
        end
    end

    nested_test("()") do
        test_html(graph, "line.html")
        return nothing
    end

    nested_test("width") do
        graph.configuration.line.width = 8
        test_html(graph, "line.width.html")
        return nothing
    end

    nested_test("color") do
        graph.configuration.line.color = "red"
        test_html(graph, "line.color.html")
        return nothing
    end

    nested_test("style") do
        graph.configuration.line.style = DashLine
        test_html(graph, "line.style.html")
        return nothing
    end

    nested_test("filled") do
        graph.configuration.line.is_filled = true
        test_html(graph, "line.is_filled.html")
        return nothing
    end

    nested_test("points") do
        graph.configuration.show_points = true

        nested_test("()") do
            test_html(graph, "line.points.html")
            return nothing
        end

        nested_test("size") do
            graph.configuration.points_size = 8
            test_html(graph, "line.points.size.html")
            return nothing
        end

        nested_test("color") do
            graph.configuration.line.color = "green"
            graph.configuration.points_color = "red"
            test_html(graph, "line.points.color.html")
            return nothing
        end
    end
end

nested_test("lines") do
    graph =
        lines_graph(; lines_points_xs = [collect(0:10) .* 10, [0, 90]], lines_points_ys = [collect(0:10) .^ 2, [50, 0]])

    nested_test("invalid") do
        context = ValidationContext(["graph"])

        nested_test("log") do
            graph.configuration.stacking = StackFractions
            graph.configuration.y_axis.log_scale = Log10Scale
            @test_throws "can't specify both graph.configuration.stacking and graph.configuration.y_axis.log_scale" validate(
                context,
                graph,
            )
        end

        nested_test("negative") do
            graph.configuration.stacking = StackFractions
            graph.data.lines_points_ys[1][1] = -1
            @test_throws dedent("""
                too low scaled graph.data.lines_points_ys[1][1]: -1.0
                is not at least: 0
                when using graph.configuration.stacking: StackFractions
            """) validate(context, graph)
        end

        nested_test("legend") do
            graph.configuration.show_legend = true
            @test_throws "must specify graph.data.lines_titles for graph.configuration.show_legend" validate(
                context,
                graph,
            )
        end
    end

    nested_test("()") do
        test_html(graph, "lines.html")
        return nothing
    end

    nested_test("legend") do
        graph.configuration.show_legend = true
        graph.data.lines_titles = ["Foo", "Bar"]
        test_html(graph, "lines.legend.html")
        return nothing
    end

    nested_test("fill") do
        graph.configuration.line.is_filled = true
        test_html(graph, "lines.fill.html")
        return nothing
    end

    nested_test("stacking") do
        nested_test("values") do
            graph.configuration.stacking = StackValues

            nested_test("()") do
                test_html(graph, "lines.values.html")
                return nothing
            end

            nested_test("fill") do
                graph.configuration.line.is_filled = true
                test_html(graph, "lines.values.fill.html")
                return nothing
            end
        end

        nested_test("fractions") do
            graph.configuration.stacking = StackFractions

            nested_test("()") do
                test_html(graph, "lines.fractions.html")
                return nothing
            end

            nested_test("fill") do
                graph.configuration.line.is_filled = true
                test_html(graph, "lines.fractions.fill.html")
                return nothing
            end

            nested_test("percent") do
                graph.configuration.y_axis.percent = true
                test_html(graph, "lines.fractions.percent.html")
                return nothing
            end
        end
    end

    nested_test("unify") do
        lines_points_xs = [[0.0, 1.0, 2.0], [0.25, 0.5, 1.5, 2.5]]
        lines_points_ys = [[-0.2, 1.2, 1.8], [0.1, 1.0, 0.5, 2.0]]
        unified_points_xs, unified_points_ys = SomeGraphs.Scatters.unify_lines_points(lines_points_xs, lines_points_ys)

        @test isapprox(
            unified_points_xs,
            [  #
                [0.0, 0.25, 0.5, 1.0, 1.5, 2.0, 2.0, 2.0, 2.5],  #
                [0.0, 0.0, 0.25, 0.25, 0.25, 0.5, 1.0, 1.5, 2.0, 2.5],  #
            ],
        )
        @test isapprox(
            unified_points_ys,
            [  #
                [-0.2, 0.15, 0.5, 1.2, 1.5, 1.8, 0.0, 0.0],  #
                [0.0, 0.0, 0.1, 1.0, 0.75, 0.5, 1.25, 2.0],  #
            ],
        )
    end
end
