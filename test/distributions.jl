function test_distributions(setup::Function, graph::Graph, plurality::AbstractString, kind::AbstractString)::Nothing
    nested_test(kind) do
        setup()

        nested_test("()") do
            test_html(graph, "$(plurality).$(kind).html")
            return nothing
        end

        nested_test("vertical") do
            graph.configuration.distribution.values_orientation = VerticalValues
            test_html(graph, "$(plurality).$(kind).vertical.html")
            return nothing
        end

        nested_test("outliers") do
            graph.configuration.distribution.show_outliers = true
            test_html(graph, "$(plurality).$(kind).outliers.html")
            return nothing
        end

        nested_test("box") do
            graph.configuration.distribution.show_box = true
            test_html(graph, "$(plurality).$(kind).box.html")
            return nothing
        end

        nested_test("log") do
            nested_test("10") do
                graph.configuration.value_axis.log_scale = Log10Scale
                test_html(graph, "$(plurality).$(kind).log10.html")
                return nothing
            end

            nested_test("2") do
                graph.configuration.value_axis.log_scale = Log2Scale
                test_html(graph, "$(plurality).$(kind).log2.html")
                return nothing
            end
        end

        nested_test("percent") do
            graph.configuration.value_axis.percent = true

            nested_test("()") do
                test_html(graph, "$(plurality).$(kind).percent.html")
                return nothing
            end

            nested_test("log") do
                nested_test("10") do
                    graph.configuration.value_axis.log_scale = Log10Scale
                    test_html(graph, "$(plurality).$(kind).percent.log10.html")
                    return nothing
                end

                nested_test("2") do
                    graph.configuration.value_axis.log_scale = Log2Scale
                    test_html(graph, "$(plurality).$(kind).percent.log2.html")
                    return nothing
                end
            end
        end

        if plurality == "distribution"
            nested_test("vertical_lines") do
                graph.configuration.value_bands.low.offset = 50
                graph.configuration.value_bands.middle.offset = 80
                graph.configuration.value_bands.high.offset = 120

                @assert !graph.configuration.value_bands.low.line.is_filled
                @assert !graph.configuration.value_bands.middle.line.is_filled
                @assert !graph.configuration.value_bands.high.line.is_filled

                return test_html(graph, "$(plurality).$(kind).vertical_lines.html")
            end

            nested_test("vertical_fills") do
                graph.configuration.value_bands.low.line.is_filled = true
                graph.configuration.value_bands.middle.line.is_filled = true
                graph.configuration.value_bands.high.line.is_filled = true

                graph.configuration.value_bands.low.line.style = nothing
                graph.configuration.value_bands.middle.line.style = nothing
                graph.configuration.value_bands.high.line.style = nothing

                graph.configuration.value_bands.low.line.color = "green"
                graph.configuration.value_bands.middle.line.color = "red"
                graph.configuration.value_bands.high.line.color = "blue"

                graph.configuration.value_bands.low.offset = 50
                graph.data.value_bands.high_offset = 120

                test_html(graph, "$(plurality).$(kind).vertical_fills.html")
                return nothing
            end

            nested_test("horizontal_lines") do
                graph.configuration.distribution.values_orientation = VerticalValues

                graph.configuration.value_bands.low.offset = 50
                graph.configuration.value_bands.middle.offset = 80
                graph.data.value_bands.high_offset = 120

                @assert !graph.configuration.value_bands.low.line.is_filled
                @assert !graph.configuration.value_bands.middle.line.is_filled
                @assert !graph.configuration.value_bands.high.line.is_filled

                return test_html(graph, "$(plurality).$(kind).horizontal_lines.html")
            end

            nested_test("horizontal_fills") do
                graph.configuration.distribution.values_orientation = VerticalValues

                graph.configuration.value_bands.low.line.is_filled = true
                graph.configuration.value_bands.middle.line.is_filled = true
                graph.configuration.value_bands.high.line.is_filled = true

                graph.configuration.value_bands.low.line.style = nothing
                graph.configuration.value_bands.middle.line.style = nothing
                graph.configuration.value_bands.high.line.style = nothing

                graph.configuration.value_bands.low.line.color = "green"
                graph.configuration.value_bands.middle.line.color = "red"
                graph.configuration.value_bands.high.line.color = "blue"

                graph.configuration.value_bands.low.offset = 50
                graph.data.value_bands.high_offset = 120

                test_html(graph, "$(plurality).$(kind).horizontal_fills.html")
                return nothing
            end

        elseif plurality == "distributions"
            nested_test("!gap") do
                graph.configuration.distributions_gap = 0.0
                return test_html(graph, "$(plurality).$(kind).!gap.html")
            end

            nested_test("gap") do
                graph.configuration.distributions_gap = 0.1
                return test_html(graph, "$(plurality).$(kind).gap.html")
            end

        else
            @assert false
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

    nested_test("invalid") do
        nested_test("!show") do
            graph.configuration.distribution.show_curve = false
            @assert !graph.configuration.distribution.show_box
            @assert !graph.configuration.distribution.show_violin
            @assert !graph.configuration.distribution.show_curve
            @test_throws (
                "must specify at least one of: " *
                "graph.configuration.distribution.show_box, " *
                "graph.configuration.distribution.show_violin, " *
                "graph.configuration.distribution.show_curve"
            ) graph.figure
        end

        nested_test("~show") do
            graph.configuration.distribution.show_violin = true
            @test_throws (
                "must not specify both of: " *
                "graph.configuration.distribution.show_violin, " *
                "graph.configuration.distribution.show_curve"
            ) graph.figure
        end
    end

    test_distributions(graph, "distribution", "curve") do
        graph.configuration.distribution.show_box = false
        return graph.configuration.distribution.show_curve = true
    end

    test_distributions(graph, "distribution", "violin") do
        graph.configuration.distribution.show_curve = false
        return graph.configuration.distribution.show_violin = true
    end

    test_distributions(graph, "distribution", "box") do
        graph.configuration.distribution.show_curve = false
        return graph.configuration.distribution.show_box = true
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

        nested_test("!distributions_gap") do
            graph.configuration.distributions_gap = -1
            @test_throws dedent("""
                too low graph.configuration.distributions_gap: -1
                is not at least: 0
            """) graph.figure
        end

        nested_test("~distributions_gap") do
            graph.configuration.distributions_gap = 1
            @test_throws dedent("""
                too high graph.configuration.distributions_gap: 1
                is not below: 1
            """) graph.figure
        end
    end

    test_distributions(graph, "distributions", "curve") do
        graph.configuration.distribution.show_box = false
        return graph.configuration.distribution.show_curve = true
    end

    test_distributions(graph, "distributions", "violin") do
        graph.configuration.distribution.show_curve = false
        return graph.configuration.distribution.show_violin = true
    end

    test_distributions(graph, "distributions", "box") do
        graph.configuration.distribution.show_curve = false
        return graph.configuration.distribution.show_box = true
    end
end
