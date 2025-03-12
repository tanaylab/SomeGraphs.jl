nested_test("bars") do
    graph = bars_graph(; bars_values = collect(0:3) .- 1)

    for (orientation_name, orientation_value) in (("vertical", VerticalValues), ("horizontal", HorizontalValues))
        nested_test(orientation_name) do
            graph.configuration.values_orientation = orientation_value

            nested_test("()") do
                test_html(graph, "bars.$(orientation_name).html")
                return nothing
            end

            nested_test("names") do
                graph.data.bars_names = ["Foo", "Bar", "Baz", "Vaz"]
                return test_html(graph, "bars.$(orientation_name).names.html")
            end

            nested_test("hovers") do
                graph.data.bars_names = ["Foo", "Bar", "Baz", "Vaz"]
                return test_html(graph, "bars.$(orientation_name).hovers.html")
            end

            nested_test("colors") do
                graph.data.bars_colors = ["red", "green", "blue", "black"]
                return test_html(graph, "bars.$(orientation_name).colors.html")
            end

            nested_test("!gap") do
                graph.configuration.bars_gap = 0
                return test_html(graph, "bars.$(orientation_name).!gap.html")
            end

            nested_test("value_lines") do
                graph.configuration.value_bands.low.offset = 0.5
                graph.data.value_bands.middle_offset = 1
                graph.configuration.value_bands.high.offset = 1.5

                @assert !graph.configuration.value_bands.low.line.is_filled
                @assert !graph.configuration.value_bands.middle.line.is_filled
                @assert !graph.configuration.value_bands.high.line.is_filled

                test_html(graph, "bars.$(orientation_name).value_lines.html")
                return nothing
            end

            nested_test("value_fills") do
                graph.configuration.value_bands.low.offset = 0.5
                graph.data.value_bands.middle_offset = 1
                graph.configuration.value_bands.high.offset = 1.5

                graph.configuration.value_bands.low.line.is_filled = true
                graph.configuration.value_bands.middle.line.is_filled = true
                graph.configuration.value_bands.high.line.is_filled = true

                graph.configuration.value_bands.low.line.style = DashDotLine
                graph.configuration.value_bands.middle.line.style = SolidLine
                graph.configuration.value_bands.high.line.style = DotLine

                graph.configuration.value_bands.low.line.color = "green"
                graph.configuration.value_bands.middle.line.color = "red"
                graph.configuration.value_bands.high.line.color = "blue"

                test_html(graph, "bars.$(orientation_name).value_fills.html")
                return nothing
            end
        end
    end
end

nested_test("series_bars") do
    foos = collect(0:10) .* 5
    bars = collect(0:10) .^ 2
    graph = series_bars_graph(; series_bars_values = [foos, bars])

    nested_test("invalid") do
        nested_test("gap") do
            graph.configuration.stacking = StackValues
            graph.configuration.series_gap = 0.05
            @test_throws "can't specify both graph.configuration.stacking and graph.configuration.series_gap" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("values") do
            graph.configuration.stacking = StackFractions
            foos[1] = -1
            @test_throws dedent("""
                too low scaled graph.data.series_bars_values[1][1]: -1.0
                is not at least: 0
                when using graph.configuration.stacking: StackFractions
            """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("title") do
            graph.data.value_axis_title = "Values"
            graph.data.series_names = ["Foo", "Bar"]
            graph.configuration.series_gap = 0.01
            @test_throws dedent("""
                can't specify both graph.data.value_axis_title and graph.data.series_names
                together with graph.configuration.series_gap
            """) validate(ValidationContext(["graph"]), graph)
        end
    end

    for (orientation_name, orientation_value) in (("vertical", VerticalValues), ("horizontal", HorizontalValues))
        nested_test(orientation_name) do
            graph.configuration.values_orientation = orientation_value

            nested_test("()") do
                test_html(graph, "series_bars.$(orientation_name).html")
                return nothing
            end

            nested_test("gap") do
                graph.configuration.series_gap = 0.05

                nested_test("()") do
                    test_html(graph, "series_bars.$(orientation_name).gap.html")
                    return nothing
                end

                nested_test("names") do
                    graph.data.series_names = ["Foo", "Bar"]
                    test_html(graph, "series_bars.$(orientation_name).gap.names.html")
                    return nothing
                end
            end

            nested_test("hovers") do
                nested_test("both") do
                    graph.data.series_hovers = ["Foo", "Bar"]
                    graph.data.bars_hovers = "B-" .* string.(collect(0:10))
                    test_html(graph, "series_bars.$(orientation_name).hovers.both.html")
                    return nothing
                end

                nested_test("bars") do
                    graph.data.bars_hovers = "B-" .* string.(collect(0:10))
                    test_html(graph, "series_bars.$(orientation_name).hovers.bars.html")
                    return nothing
                end

                nested_test("series") do
                    graph.data.series_hovers = ["Foo", "Bar"]
                    test_html(graph, "series_bars.$(orientation_name).hovers.series.html")
                    return nothing
                end
            end

            nested_test("stacking") do
                nested_test("values") do
                    nested_test("()") do
                        graph.configuration.stacking = StackValues
                        test_html(graph, "series_bars.$(orientation_name).values.html")
                        return nothing
                    end

                    nested_test("legend") do
                        graph.data.series_names = ["Foo", "Bar"]
                        test_html(graph, "series_bars.$(orientation_name).values.legend.html")
                        return nothing
                    end
                end

                nested_test("fractions") do
                    graph.configuration.stacking = StackFractions
                    test_html(graph, "series_bars.$(orientation_name).fractions.html")
                    return nothing
                end

                nested_test("percents") do
                    graph.configuration.stacking = StackFractions
                    graph.configuration.value_axis.percent = true
                    test_html(graph, "series_bars.$(orientation_name).percents.html")
                    return nothing
                end
            end
        end
    end
end
