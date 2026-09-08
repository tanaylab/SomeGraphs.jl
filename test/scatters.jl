nested_test("points") do
    graph = points_graph(; x = ValuesData(collect(0:10) .* 10), y = ValuesData(collect(0:10) .^ 2))

    nested_test("invalid") do
        context = ValidationContext(["graph"])

        nested_test("!values") do
            graph.data.x.values = nothing
            @test_throws "ArgumentError: must specify graph.data.x.values" validate(context, graph)
        end

        nested_test("~values") do
            graph.data.points.sizes.values = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"]
            @test_throws "ArgumentError: non-numeric graph.data.points.sizes.values" validate(context, graph)
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

        nested_test("continuous") do
            nested_test("explicit") do
                graph.configuration.edges.colors.palette = [0 => "red", 1 => "blue"]
                @test_throws "continuous colors for edges are not implemented" validate(context, graph)
            end

            nested_test("named") do
                graph.configuration.edges.colors.palette = "Viridis"
                @test_throws "continuous colors for edges are not implemented" validate(context, graph)
            end
        end
    end

    nested_test("()") do
        test_html(graph, "points.html")
        return nothing
    end

    nested_test("fields") do
        fields = x_fields(graph)
        @test fields.data.values === graph.data.x
        @test fields.data.entities === graph.data.points.entities
        @test fields.configuration.axis === graph.configuration.x_axis

        fields = y_fields(graph)
        @test fields.data.values === graph.data.y
        @test fields.data.entities === graph.data.points.entities
        @test fields.configuration.axis === graph.configuration.y_axis

        fields = points_colors_fields(graph)
        @test fields.data.values === graph.data.points.colors
        @test fields.data.entities === graph.data.points.entities
        @test fields.configuration.axis === graph.configuration.points.colors.axis
        @test fields.configuration.colors === graph.configuration.points.colors

        fields = points_sizes_fields(graph)
        @test fields.data.values === graph.data.points.sizes
        @test fields.data.entities === graph.data.points.entities
        @test fields.configuration.axis === graph.configuration.points.sizes.axis
        @test fields.configuration.sizes === graph.configuration.points.sizes

        fields = borders_colors_fields(graph)
        @test fields.data.values === graph.data.borders.colors
        @test fields.data.entities === graph.data.points.entities
        @test fields.configuration.colors === graph.configuration.borders.colors

        fields = borders_sizes_fields(graph)
        @test fields.data.values === graph.data.borders.sizes
        @test fields.data.entities === graph.data.points.entities
        @test fields.configuration.sizes === graph.configuration.borders.sizes

        fields = edges_colors_fields(graph)
        @test fields.data.values === graph.data.edges.colors
        @test fields.data.entities === graph.data.edges.entities
        @test fields.configuration.colors === graph.configuration.edges.colors

        fields = edges_sizes_fields(graph)
        @test fields.data.values === graph.data.edges.sizes
        @test fields.data.entities === graph.data.edges.entities
        @test fields.configuration.sizes === graph.configuration.edges.sizes

        # Hovers added through one view of the points are seen through all of them.
        add_hovers!(x_fields(graph).data.entities, string.(1:11); title = "X")
        @test points_colors_fields(graph).data.entities.hovers == "X: " .* string.(1:11)
        return nothing
    end

    nested_test("mask") do
        graph.data.points.entities.mask = [true, true, true, true, true, true, false, false, false, false, false]

        nested_test("()") do
            test_html(graph, "points.mask.html")
            return nothing
        end

        nested_test("!hidden") do
            graph.configuration.x_axis.include_hidden = false
            graph.configuration.y_axis.include_hidden = false
            graph.data.points.colors.values = collect(0:10)
            graph.data.points.sizes.values = collect(0:10)
            graph.configuration.points.colors.axis.include_hidden = false
            graph.configuration.points.sizes.axis.include_hidden = false
            test_html(graph, "points.mask.!hidden.html")
            return nothing
        end
    end

    nested_test("hovers") do
        graph.data.points.entities.hovers = ["H: $(index)" for index in 1:11]
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
            graph.data.points.entities.hovers = ["H: $(index)" for index in 1:11]
            test_html(graph, "points.log.hovers.html")
            return nothing
        end
    end

    nested_test("edges") do
        graph.data.edges.points = [(1, 7), (2, 8), (3, 9), (4, 10), (5, 11)]
        nested_test("()") do
            test_html(graph, "points.edges.html")
            return nothing
        end

        nested_test("continuous") do
            graph.data.edges.colors.values = collect(1:5)
            @test_throws "continuous colors for edges are not implemented" validate(ValidationContext(["graph"]), graph)
        end

        nested_test("mask") do
            graph.data.edges.entities.mask = [true, true, true, false, false]
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
            graph.data.edges.styles = [SolidLine, DashLine, DashDotLine, DotLine, SolidLine]
            test_html(graph, "points.edges.styles.html")
            return nothing
        end

        nested_test("hovers") do
            graph.data.edges.entities.hovers = ["E: $(index)" for index in 1:5]
            test_html(graph, "points.edges.hovers.html")
            return nothing
        end

        nested_test("named") do
            graph.data.edges.colors.values = ["red", "yellow", "green", "cyan", "blue"]
            test_html(graph, "points.edges.named.html")
            return nothing
        end

        nested_test("categorical") do
            graph.configuration.edges.colors.palette = Dict("Foo" => "red", "Bar" => "green", "Baz" => "blue")
            graph.data.edges.colors.values = ["Foo", "Bar", "Baz", "Bar", "Foo"]

            nested_test("()") do
                test_html(graph, "points.edges.categorical.html")
                return nothing
            end

            nested_test("legend") do
                graph.configuration.edges.colors.show_legend = true
                test_html(graph, "points.edges.categorical.legend.html")
                return nothing
            end

            nested_test("priorities") do
                graph.data.edges.order = reverse!(collect(1:5))
                test_html(graph, "points.edges.categorical.priorities.html")
                return nothing
            end
        end

        nested_test("size") do
            graph.configuration.edges.sizes.fixed = 12
            test_html(graph, "points.edges.size.html")
            return nothing
        end

        nested_test("sizes") do
            graph.data.edges.sizes.values = collect(1:5)
            test_html(graph, "points.edges.sizes.html")
            return nothing
        end
    end

    nested_test("continuous") do
        graph.data.points.colors.values = collect(0:10)

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
        graph.data.points.colors.values =
            ["red", "yellow", "green", "cyan", "blue", "magenta", "blue", "cyan", "green", "yellow", "red"]
        test_html(graph, "points.named.html")
        return nothing
    end

    nested_test("categorical") do
        graph.configuration.points.colors.palette = Dict("Foo" => "red", "Bar" => "green", "Baz" => "blue")
        graph.data.points.colors.values = ["Foo", "Bar", "Baz", "Bar", "Foo", "Bar", "Baz", "Bar", "Foo", "Bar", "Baz"]

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
            graph.data.points.entities.mask = [true, true, true, true, true, true, false, false, false, false, false]
            test_html(graph, "points.categorical.mask.html")
            return nothing
        end

        nested_test("priorities") do
            graph.data.points.order = reverse(collect(1:11))
            test_html(graph, "points.points.categorical.priorities.html")
            return nothing
        end
    end

    nested_test("automatic") do
        graph.configuration.points.colors.palette = AutomaticColors()
        graph.data.points.colors.values = ["Foo", "Bar", "Baz", "Bar", "Foo", "Bar", "Baz", "Bar", "Foo", "Bar", "Baz"]

        nested_test("()") do
            test_html(graph, "points.automatic.html")
            return nothing
        end

        nested_test("legend") do
            graph.configuration.points.colors.show_legend = true
            test_html(graph, "points.automatic.legend.html")
            return nothing
        end

        nested_test("~numeric") do
            graph.data.points.colors.values = collect(0:10)
            @test_throws chomp("""
                               ArgumentError: numeric graph.data.points.colors.values
                               specified for automatic graph.configuration.points.colors.palette
                               """) graph.figure
        end

        nested_test("~missing") do
            graph.data.points.colors.values = nothing
            @test_throws chomp("""
                               ArgumentError: must specify (categorical) graph.data.points.colors.values
                               for automatic graph.configuration.points.colors.palette
                               """) graph.figure
        end
    end

    nested_test("size") do
        graph.configuration.points.sizes.fixed = 12
        test_html(graph, "points.size.html")
        return nothing
    end

    nested_test("sizes") do
        graph.data.points.sizes.values = collect(0:10)
        test_html(graph, "points.sizes.html")
        return nothing
    end

    nested_test("borders") do
        nested_test("continuous") do
            graph.data.borders.colors.values = collect(0:10)

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
            graph.data.borders.colors.values =
                ["red", "yellow", "green", "cyan", "blue", "magenta", "blue", "cyan", "green", "yellow", "red"]
            test_html(graph, "points.borders.named.html")
            return nothing
        end

        nested_test("categorical") do
            graph.configuration.borders.colors.palette = Dict("Foo" => "red", "Bar" => "green", "Baz" => "blue")
            graph.data.borders.colors.values =
                ["Foo", "Bar", "Baz", "Bar", "Foo", "Bar", "Baz", "Bar", "Foo", "Bar", "Baz"]

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
                graph.data.borders.sizes.values = collect(0:10)
                test_html(graph, "points.borders.sizes.html")
                return nothing
            end

            nested_test("size") do
                graph.configuration.points.sizes.fixed = 12
                graph.data.borders.sizes.values = collect(0:10)
                test_html(graph, "points.borders.sizes.size.html")
                return nothing
            end

            nested_test("sizes") do
                graph.data.points.sizes.values = reverse(collect(0:10))
                graph.data.borders.sizes.values = collect(0:10)
                test_html(graph, "points.borders.sizes.sizes.html")
                return nothing
            end
        end
    end

    nested_test("offsets") do
        graph.data.edges.points = [(1, 7), (2, 8), (3, 9), (4, 10), (5, 11)]

        graph.data.figure_title = "Figure"
        graph.data.x.title = "X Axis"
        graph.data.y.title = "Y Axis"

        graph.data.points.colors.title = "Points"
        graph.data.borders.colors.title = "Borders"
        graph.data.edges.colors.title = "Edges"

        graph.configuration.borders.colors.show_legend = true
        graph.configuration.points.colors.show_legend = true
        graph.configuration.edges.colors.show_legend = true

        nested_test("no-colors") do
            graph.configuration.points.colors.palette = Dict("Foo-P" => "red", "Bar-P" => "green", "Baz-P" => "blue")
            graph.data.points.colors.values =
                ["Foo-P", "Bar-P", "Baz-P", "Bar-P", "Foo-P", "Bar-P", "Baz-P", "Bar-P", "Foo-P", "Bar-P", "Baz-P"]

            graph.configuration.borders.colors.palette = Dict("Foo-B" => "red", "Bar-B" => "green", "Baz-B" => "blue")
            graph.data.borders.colors.values =
                ["Foo-B", "Bar-B", "Baz-B", "Bar-B", "Foo-B", "Bar-B", "Baz-B", "Bar-B", "Foo-B", "Bar-B", "Baz-B"]

            graph.configuration.edges.colors.palette = Dict("Foo-E" => "red", "Bar-E" => "green", "Baz-E" => "blue")
            graph.data.edges.colors.values = ["Foo-E", "Bar-E", "Baz-E", "Bar-E", "Foo-E"]

            test_html(graph, "points.offsets.no-colors.html")
            return nothing
        end

        nested_test("one-colors") do
            graph.configuration.points.colors.palette = [0 => "red", 10 => "blue"]
            graph.data.points.colors.values = collect(0:10)

            graph.configuration.borders.colors.palette = Dict("Foo-B" => "red", "Bar-B" => "green", "Baz-B" => "blue")
            graph.data.borders.colors.values =
                ["Foo-B", "Bar-B", "Baz-B", "Bar-B", "Foo-B", "Bar-B", "Baz-B", "Bar-B", "Foo-B", "Bar-B", "Baz-B"]

            graph.configuration.edges.colors.palette = Dict("Foo-E" => "red", "Bar-E" => "green", "Baz-E" => "blue")
            graph.data.edges.colors.values = ["Foo-E", "Bar-E", "Baz-E", "Bar-E", "Foo-E"]

            test_html(graph, "points.offsets.one-colors.html")
            return nothing
        end

        nested_test("two-colors") do
            graph.configuration.points.colors.palette = [0 => "red", 10 => "blue"]
            graph.data.points.colors.values = collect(0:10)

            graph.configuration.borders.colors.palette = [0 => "blue", 10 => "green"]
            graph.data.borders.colors.values = collect(0:10)

            graph.configuration.edges.colors.palette = Dict("Foo-E" => "red", "Bar-E" => "green", "Baz-E" => "blue")
            graph.data.edges.colors.values = ["Foo-E", "Bar-E", "Baz-E", "Bar-E", "Foo-E"]

            test_html(graph, "points.offsets.two-colors.html")
            return nothing
        end
    end

    nested_test("density") do
        graph.data.x.values = [
            0.2698393176826803,
            0.21199888259395777,
            -1.1403772919081927,
            0.015375662421357001,
            -1.067372097104871,
            -0.05131680407322392,
            1.1476690271171557,
            0.2619998741581797,
            -0.3294624837610639,
            0.3990906575326256,
            0.016185972979333094,
            -0.6295842065710322,
            1.74273570356108,
            -1.612316716623975,
            -1.2696818434826393,
            -2.3942962323946806,
            -0.0683194741744384,
            -0.6991502371264332,
            1.3005476302710504,
            -0.3156364801379863,
        ]
        graph.data.y.values = [
            -0.1764741545510277,
            0.5007984744043152,
            -1.0092288051861404,
            0.28862095432807144,
            0.3216029374844889,
            1.1177946117474804,
            0.11865114901055787,
            -2.173777902643006,
            -0.5131646448399668,
            -0.4180196978042471,
            -1.7758801658517032,
            0.5019767811414706,
            0.6519383169746722,
            1.306115558967419,
            -0.6077449865370641,
            0.6968047575410379,
            1.7053341710917538,
            -0.6584463274588279,
            0.9034430051864035,
            -0.631083973233279,
        ]
        graph.data.points.colors.values = points_density(graph.data.x.values, graph.data.y.values)
        graph.configuration.points.colors.palette = "Viridis"
        graph.configuration.points.sizes.fixed = 16
        graph.configuration.figure.width = 200
        graph.configuration.figure.height = 200
        graph.configuration.x_axis.minimum = -3
        graph.configuration.y_axis.minimum = -3
        graph.configuration.x_axis.maximum = 3
        graph.configuration.y_axis.maximum = 3

        nested_test("()") do
            return test_html(graph, "points.density.html")
        end

        nested_test("priorities") do
            graph.data.points.order = sortperm(graph.data.points.colors.values)
            return test_html(graph, "points.density.priorities.html")
        end
    end
end

nested_test("line") do
    graph = line_graph(; x = ValuesData(collect(0:10) .* 10), y = ValuesData(collect(0:10) .^ 2))

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

    nested_test("fields") do
        fields = x_fields(graph)
        @test fields.data.values === graph.data.x
        @test fields.data.entities === graph.data.points
        @test fields.configuration.axis === graph.configuration.x_axis

        fields = y_fields(graph)
        @test fields.data.values === graph.data.y
        @test fields.data.entities === graph.data.points
        @test fields.configuration.axis === graph.configuration.y_axis
        return nothing
    end

    nested_test("hovers") do
        graph.data.points.hovers = ["H: $(index)" for index in 1:11]
        test_html(graph, "line.hovers.html")
        return nothing
    end

    nested_test("mask") do
        graph.data.points.mask = [true, true, true, true, true, true, false, false, false, false, false]

        nested_test("()") do
            test_html(graph, "line.mask.html")
            return nothing
        end

        nested_test("!hidden") do
            graph.configuration.x_axis.include_hidden = false
            graph.configuration.y_axis.include_hidden = false
            test_html(graph, "line.mask.!hidden.html")
            return nothing
        end
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

    nested_test("fill") do
        graph.configuration.line.is_filled = true
        test_html(graph, "line.fill.html")
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
    graph = lines_graph(;
        lines = [
            LineData(; x = ValuesData(collect(0:10) .* 10), y = ValuesData(collect(0:10) .^ 2)),
            LineData(; x = ValuesData([0, 90]), y = ValuesData([50, 0])),
        ],
    )

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
            graph.data.lines[1].y.values[1] = -1
            @test_throws chomp("""
                               ArgumentError: too low scaled graph.data.lines[1].y.values[1]: -1.0
                               is not at least: 0
                               when using graph.configuration.stacking: StackFractions
                               """) validate(context, graph)
        end

        nested_test("stacked_hovers") do
            graph.configuration.stacking = StackValues
            graph.data.lines[2].points.hovers = ["A", "B"]
            @test_throws chomp("""
                               ArgumentError: can't specify both graph.data.lines[2].points.hovers
                               and graph.configuration.stacking
                               """) validate(context, graph)
        end

        nested_test("legend") do
            graph.configuration.show_legend = true
            @test_throws "must specify graph.data.lines[1].name for graph.configuration.show_legend" validate(
                context,
                graph,
            )
        end

        nested_test("legend_entry") do
            graph.configuration.show_legend = true
            graph.data.lines[1].name = "Foo"
            @test_throws "must specify graph.data.lines[2].name for graph.configuration.show_legend" validate(
                context,
                graph,
            )
        end

        nested_test("~titles") do
            graph.data.lines[1].x.title = "Foo"
            graph.data.lines[2].x.title = "Bar"
            @test_throws chomp("""
                               ArgumentError: conflicting graph.data.lines[2].x.title: Bar
                               is different from graph.data.lines[1].x.title: Foo
                               """) validate(context, graph)
        end

        nested_test("~colors") do
            graph.data.lines[2].color = "Oobleck"
            @test_throws "ArgumentError: invalid graph.data.lines[2].color: Oobleck" validate(context, graph)
        end

        nested_test("~points_colors") do
            graph.data.lines[2].points_color = "Oobleck"
            @test_throws "ArgumentError: invalid graph.data.lines[2].points_color: Oobleck" validate(context, graph)
        end
    end

    nested_test("()") do
        test_html(graph, "lines.html")
        return nothing
    end

    nested_test("fields") do
        fields = x_fields(graph, 2)
        @test fields.data.values === graph.data.lines[2].x
        @test fields.data.entities === graph.data.lines[2].points
        @test fields.configuration.axis === graph.configuration.x_axis

        fields = y_fields(graph, 2)
        @test fields.data.values === graph.data.lines[2].y
        @test fields.data.entities === graph.data.lines[2].points
        @test fields.configuration.axis === graph.configuration.y_axis

        line = LineData(; name = "Baz")
        @test add_line!(graph, line) == 3
        @test graph.data.lines[3] === line
        @test x_fields(graph, 3).data.values === line.x
        @test y_fields(graph, 3).data.values === line.y

        @test add_line!(graph) == 4
        x_fields(graph, 4).data.values.values = [0, 1]
        @test graph.data.lines[4].x.values == [0, 1]
        return nothing
    end

    nested_test("legend") do
        graph.configuration.show_legend = true
        graph.data.lines[1].name = "Foo"
        graph.data.lines[2].name = "Bar"
        test_html(graph, "lines.legend.html")
        return nothing
    end

    nested_test("nothing") do
        graph.data.lines[1].name = "Foo"
        graph.data.lines[2].color = "red"
        graph.data.lines[1].width = 4
        graph.data.lines[2].style = DotLine
        graph.data.lines[2].points_size = 8
        graph.data.lines[1].points_color = "blue"
        test_html(graph, "lines.nothing.html")
        return nothing
    end

    nested_test("hovers") do
        nested_test("points") do
            graph.data.lines[1].points.hovers = ["A: $(index)" for index in 1:11]
            graph.data.lines[2].points.hovers = ["B: 1", "B: 2"]
            test_html(graph, "lines.hovers.points.html")
            return nothing
        end

        nested_test("lines") do
            graph.data.lines[1].hover = "Foo"
            graph.data.lines[2].hover = "Bar"
            test_html(graph, "lines.hovers.lines.html")
            return nothing
        end

        nested_test("both") do
            graph.data.lines[1].points.hovers = ["A: $(index)" for index in 1:11]
            graph.data.lines[1].hover = "Foo"
            graph.data.lines[2].hover = "Bar"
            test_html(graph, "lines.hovers.both.html")
            return nothing
        end
    end

    nested_test("mask") do
        nested_test("points") do
            graph.data.lines[1].points.mask = [true, true, true, true, true, true, false, false, false, false, false]

            nested_test("()") do
                test_html(graph, "lines.mask.points.html")
                return nothing
            end

            nested_test("!hidden") do
                graph.configuration.x_axis.include_hidden = false
                graph.configuration.y_axis.include_hidden = false
                test_html(graph, "lines.mask.points.!hidden.html")
                return nothing
            end
        end

        nested_test("lines") do
            graph.data.lines[2].is_shown = false
            test_html(graph, "lines.mask.lines.html")
            return nothing
        end

        nested_test("stacked") do
            graph.configuration.stacking = StackValues
            graph.configuration.line.is_filled = true
            graph.data.lines[1].is_shown = false
            test_html(graph, "lines.mask.stacked.html")
            return nothing
        end
    end

    nested_test("fill") do
        graph.configuration.line.is_filled = true

        nested_test("()") do
            test_html(graph, "lines.fill.html")
            return nothing
        end

        nested_test("priorities") do
            graph.data.order = [2, 1]
            test_html(graph, "lines.fill.priorities.html")
            return nothing
        end
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
        unified_points_xs, unified_points_ys, stacked_range =
            SomeGraphs.Scatters.unify_lines_points(lines_points_xs, lines_points_ys)

        @test isapprox(stacked_range.minimum, -0.2)
        @test isapprox(stacked_range.maximum, 3.05)

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
