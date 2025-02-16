function test_distributions(setup::Function, graph::Graph, plurality::AbstractString, kind::AbstractString)::Nothing
    nested_test(kind) do
        setup()

        for (name, orientation) in (("vertical", VerticalValues), ("horizontal", HorizontalValues))
            nested_test(name) do
                graph.configuration.distribution.values_orientation = orientation

                nested_test("()") do
                    test_html(graph, "$(plurality).$(kind).$(name).html")
                    return nothing
                end

                nested_test("line") do
                    nested_test("~style") do
                        graph.configuration.distribution.line.style = DotLine
                        @test_throws "unsupported graph.configuration.distribution.line.style: DotLine" validate(
                            ValidationContext(["graph"]),
                            graph,
                        )
                    end

                    if kind == "histogram"
                        nested_test("~width") do
                            graph.configuration.distribution.line.width = 4
                            @test_throws dedent("""
                                unsupported graph.configuration.distribution.line.width: 4
                                for graph.configuration.distribution.style: HistogramDistribution
                            """) validate(ValidationContext(["graph"]), graph)
                        end

                        nested_test("~fill") do
                            graph.configuration.distribution.line.is_filled = false
                            @test_throws dedent("""
                                unsupported graph.configuration.distribution.line.is_filled: false
                                for graph.configuration.distribution.style: HistogramDistribution
                            """) validate(ValidationContext(["graph"]), graph)
                        end
                    else
                        graph.configuration.distribution.line.color = "red"
                        graph.configuration.distribution.line.width = 4
                        graph.configuration.distribution.line.is_filled = false
                        test_html(graph, "$(plurality).$(kind).$(name).line.html")
                        return nothing
                    end
                end

                nested_test("outliers") do
                    graph.configuration.distribution.show_outliers = true
                    if contains(kind, "box")
                        test_html(graph, "$(plurality).$(kind).$(name).outliers.html")
                        return nothing
                    else
                        @test_throws dedent(
                            """
            specified graph.configuration.distribution.show_outliers
            for non-box graph.configuration.distribution.style: $(graph.configuration.distribution.style)
        """,
                        ) validate(ValidationContext(["graph"]), graph)
                    end
                end

                nested_test("!grid") do
                    graph.configuration.value_axis.show_grid = false
                    test_html(graph, "$(plurality).$(kind).$(name).!grid.html")
                    return nothing
                end

                nested_test("grid_color") do
                    graph.configuration.value_axis.grid_color = "red"
                    test_html(graph, "$(plurality).$(kind).$(name).grid_color.html")
                    return nothing
                end

                nested_test("ticks") do
                    graph.configuration.value_axis.show_ticks = false
                    test_html(graph, "$(plurality).$(kind).$(name).!ticks.html")
                    return nothing
                end

                nested_test("log") do
                    nested_test("10") do
                        graph.configuration.value_axis.log_scale = Log10Scale
                        test_html(graph, "$(plurality).$(kind).$(name).log10.html")
                        return nothing
                    end

                    nested_test("2") do
                        graph.configuration.value_axis.log_scale = Log2Scale
                        test_html(graph, "$(plurality).$(kind).$(name).log2.html")
                        return nothing
                    end
                end

                nested_test("percent") do
                    graph.configuration.value_axis.percent = true

                    nested_test("()") do
                        test_html(graph, "$(plurality).$(kind).$(name).percent.html")
                        return nothing
                    end

                    nested_test("log") do
                        nested_test("10") do
                            graph.configuration.value_axis.log_scale = Log10Scale
                            test_html(graph, "$(plurality).$(kind).$(name).percent.log10.html")
                            return nothing
                        end

                        nested_test("2") do
                            graph.configuration.value_axis.log_scale = Log2Scale
                            test_html(graph, "$(plurality).$(kind).$(name).percent.log2.html")
                            return nothing
                        end
                    end
                end

                if plurality == "distribution"
                    nested_test("value_lines") do
                        graph.configuration.value_bands.low.offset = 50
                        graph.data.value_bands.middle_offset = 80
                        graph.configuration.value_bands.high.offset = 120

                        @assert !graph.configuration.value_bands.low.line.is_filled
                        @assert !graph.configuration.value_bands.middle.line.is_filled
                        @assert !graph.configuration.value_bands.high.line.is_filled

                        test_html(graph, "$(plurality).$(kind).$(name).value_lines.html")
                        return nothing
                    end

                    nested_test("value_fills") do
                        graph.configuration.value_bands.low.line.is_filled = true
                        graph.configuration.value_bands.middle.line.is_filled = true
                        graph.configuration.value_bands.high.line.is_filled = true

                        graph.configuration.value_bands.low.line.style = DashDotLine
                        graph.configuration.value_bands.middle.line.style = nothing
                        graph.configuration.value_bands.high.line.style = DashDotLine

                        graph.configuration.value_bands.low.line.color = "green"
                        graph.configuration.value_bands.middle.line.color = "red"
                        graph.configuration.value_bands.high.line.color = "blue"

                        graph.configuration.value_bands.low.offset = 50
                        graph.data.value_bands.high_offset = 120

                        test_html(graph, "$(plurality).$(kind).$(name).value_fills.html")
                        return nothing
                    end

                elseif plurality == "distributions"
                    nested_test("!gap") do
                        graph.configuration.distributions_gap = nothing

                        if kind === "box"
                            @test_throws "overlay (no graph.configuration.distributions_gap specified) for box distributions" validate(
                                ValidationContext(["graph"]),
                                graph,
                            )
                        else
                            test_html(graph, "$(plurality).$(kind).$(name).!gap.html")
                            return nothing
                        end
                    end
                end
            end
        end
    end

    return nothing
end

nested_test("distribution") do
    graph = distribution_graph(;
        distribution_values = [
            #! format: off
            79, 54, 74, 62, 85, 55, 88, 85, 51, 85, 54, 84, 78, 47, 83, 52, 62, 84, 52, 79, 51, 47, 78, 69, 74, 83,
            55, 76, 78, 79, 73, 77, 66, 80, 74, 52, 48, 80, 59, 90, 80, 58, 84, 58, 73, 83, 64, 53, 82, 59, 75, 90,
            54, 80, 54, 83, 71, 64, 77, 81, 59, 84, 48, 82, 60, 92, 78, 78, 65, 73, 82, 56, 79, 71, 62, 76, 60, 78,
            76, 83, 75, 82, 70, 65, 73, 88, 76, 80, 48, 86, 60, 90, 50, 78, 63, 72, 84, 75, 51, 82, 62, 88, 49, 83,
            81, 47, 84, 52, 86, 81, 75, 59, 89, 79, 59, 81, 50, 85, 59, 87, 53, 69, 77, 56, 88, 81, 45, 82, 55, 90,
            45, 83, 56, 89, 46, 82, 51, 86, 53, 79, 81, 60, 82, 77, 76, 59, 80, 49, 96, 53, 77, 77, 65, 81, 71, 70,
            81, 93, 53, 89, 45, 86, 58, 78, 66, 76, 63, 88, 52, 93, 49, 57, 77, 68, 81, 81, 73, 50, 85, 74, 55, 77,
            83, 83, 51, 78, 84, 46, 83, 55, 81, 57, 76, 84, 77, 81, 87, 77, 51, 78, 60, 82, 91, 53, 78, 46, 77, 84,
            49, 83, 71, 80, 49, 75, 64, 76, 53, 94, 55, 76, 50, 82, 54, 75, 78, 79, 78, 78, 70, 79, 70, 54, 86, 50,
            90, 54, 54, 77, 79, 64, 75, 47, 86, 63, 85, 82, 57, 82, 67, 74, 54, 83, 73, 73, 88, 80, 71, 83, 56, 79,
            78, 84, 58, 83, 43, 60, 75, 81, 46, 90, 46, 74, 140, 150,
            #! format: on
        ],
    )

    nested_test("show") do
        @test "$(graph)" ==
              "Graph{DistributionGraphData, DistributionGraphConfiguration} (use .figure to show the graph)"
    end

    for (name, style) in (
        ("curve", CurveDistribution),
        ("curve_box", CurveBoxDistribution),
        ("violin", ViolinDistribution),
        ("violin_box", ViolinBoxDistribution),
        ("box", BoxDistribution),
        ("histogram", HistogramDistribution),
    )
        test_distributions(graph, "distribution", name) do
            graph.configuration.distribution.style = style
            return nothing
        end
    end
end

nested_test("distributions") do
    graph = distributions_graph(;
        distributions_values = [
            #! format: off
            [
                0.75, 5.25, 5.5, 6, 6.2, 6.6, 6.80, 7.0, 7.2, 7.5, 7.5, 7.75, 8.15, 8.15, 8.65, 8.93, 9.2, 9.5, 10,
                10.25, 11.5, 12, 16, 20.90, 22.3, 23.25,
            ], [
                79, 54, 74, 62, 85, 55, 88, 85, 51, 85, 54, 84, 78, 47, 83, 52, 62, 84, 52, 79, 51, 47, 78, 69, 74,
                83, 55, 76, 78, 79, 73, 77, 66, 80, 74, 52, 48, 80, 59, 90, 80, 58, 84, 58, 73, 83, 64, 53, 82, 59,
                75, 90, 54, 80, 54, 83, 71, 64, 77, 81, 59, 84, 48, 82, 60, 92, 78, 78, 65, 73, 82, 56, 79, 71, 62,
                76, 60, 78, 76, 83, 75, 82, 70, 65, 73, 88, 76, 80, 48, 86, 60, 90, 50, 78, 63, 72, 84, 75, 51, 82,
                62, 88, 49, 83, 81, 47, 84, 52, 86, 81, 75, 59, 89, 79, 59, 81, 50, 85, 59, 87, 53, 69, 77, 56, 88,
                81, 45, 82, 55, 90, 45, 83, 56, 89, 46, 82, 51, 86, 53, 79, 81, 60, 82, 77, 76, 59, 80, 49, 96, 53,
                77, 77, 65, 81, 71, 70, 81, 93, 53, 89, 45, 86, 58, 78, 66, 76, 63, 88, 52, 93, 49, 57, 77, 68, 81,
                81, 73, 50, 85, 74, 55, 77, 83, 83, 51, 78, 84, 46, 83, 55, 81, 57, 76, 84, 77, 81, 87, 77, 51, 78,
                60, 82, 91, 53, 78, 46, 77, 84, 49, 83, 71, 80, 49, 75, 64, 76, 53, 94, 55, 76, 50, 82, 54, 75, 78,
                79, 78, 78, 70, 79, 70, 54, 86, 50, 90, 54, 54, 77, 79, 64, 75, 47, 86, 63, 85, 82, 57, 82, 67, 74,
                54, 83, 73, 73, 88, 80, 71, 83, 56, 79, 78, 84, 58, 83, 43, 60, 75, 81, 46, 90, 46, 74, 140, 150,
            ] ./ 10.0
            #! format: on
        ],
    )
    nested_test("invalid") do
        nested_test("!values") do
            empty!(graph.data.distributions_values)
            @test_throws "empty vector graph.data.distributions_values" graph.figure
        end

        nested_test("!value") do
            empty!(graph.data.distributions_values[1])
            @test_throws "empty vector graph.data.distributions_values[1]" graph.figure
        end

        nested_test("~names") do
            graph.data.distributions_names = ["Foo"]
            @test_throws dedent("""
                invalid length of graph.data.distributions_names: 1
                is different from length of graph.data.distributions_values: 2
            """) graph.figure
        end

        nested_test("!colors") do
            graph.data.distributions_colors = ["Red", "Oobleck"]
            @test_throws "invalid graph.data.distributions_colors[2]: Oobleck" graph.figure
        end

        nested_test("~colors") do
            graph.data.distributions_colors = ["Red"]
            @test_throws dedent("""
                invalid length of graph.data.distributions_colors: 1
                is different from length of graph.data.distributions_values: 2
            """) graph.figure
        end
    end

    for (name, style) in (
        ("curve", CurveDistribution),
        ("curve_box", CurveBoxDistribution),
        ("violin", ViolinDistribution),
        ("violin_box", ViolinBoxDistribution),
        ("box", BoxDistribution),
        ("histogram", HistogramDistribution),
    )
        test_distributions(graph, "distributions", name) do
            graph.configuration.distribution.style = style
            return nothing
        end
    end
end
