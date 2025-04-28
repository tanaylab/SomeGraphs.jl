nested_test("bars") do
    graph = bars_graph(; bars_values = collect(0:3) .- 1)

    nested_test("invalid") do
        graph.configuration.bars_colors.show_legend = true
        graph.configuration.bars_colors.palette = Dict(["Foo" => "red"])
        graph.data.bars_colors = ["Foo", "Foo", "Foo", "Foo"]
        @test_throws chomp("""
                           ArgumentError: can't specify graph.configuration.bars_colors.show_legend
                           for a categorical graph.configuration.bars_colors.palette
                           """) validate(ValidationContext(["graph"]), graph)
    end

    for (orientation_name, orientation_value) in (("vertical", VerticalValues), ("horizontal", HorizontalValues))
        nested_test(orientation_name) do
            graph.configuration.values_orientation = orientation_value

            nested_test("()") do
                test_html(graph, "bars.$(orientation_name).html")
                return nothing
            end

            nested_test("names") do
                graph.data.bars_names = ["Foo", "Bar", "Baz", "Vaz"]
                test_html(graph, "bars.$(orientation_name).names.html")
                return nothing
            end

            nested_test("hovers") do
                graph.data.bars_names = ["Foo", "Bar", "Baz", "Vaz"]
                test_html(graph, "bars.$(orientation_name).hovers.html")
                return nothing
            end

            nested_test("colors") do
                nested_test("named") do
                    graph.data.bars_colors = ["red", "green", "blue", "black"]
                    test_html(graph, "bars.$(orientation_name).colors.named.html")
                    return nothing
                end

                nested_test("continuous") do
                    graph.data.bars_colors = [0, 1, 2, 3]

                    nested_test("()") do
                        test_html(graph, "bars.$(orientation_name).colors.continuous.html")
                        return nothing
                    end

                    nested_test("legend") do
                        graph.configuration.bars_colors.show_legend = true
                        graph.data.bars_colors_title = "Colors"
                        test_html(graph, "bars.$(orientation_name).colors.continuous.legend.html")
                        return nothing
                    end
                end

                nested_test("categorical") do
                    graph.data.bars_colors = ["Foo", "Bar", "Baz", "Bar"]
                    graph.configuration.bars_colors.palette = Dict(["Foo" => "red", "Bar" => "green", "Baz" => "blue"])

                    test_html(graph, "bars.$(orientation_name).colors.categorical.html")
                    return nothing
                end
            end

            nested_test("!gap") do
                graph.configuration.bars_gap = 0
                test_html(graph, "bars.$(orientation_name).!gap.html")
                return nothing
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

            nested_test("annotations") do
                nested_test("continuous") do
                    graph.data.bars_annotations = [AnnotationData(; title = "score", values = [1, 0.5, 0, 1])]

                    nested_test("()") do
                        test_html(graph, "bars.$(orientation_name).continuous.html")
                        return nothing
                    end

                    nested_test("legend") do
                        graph.data.bars_annotations[1].colors.show_legend = true
                        test_html(graph, "bars.$(orientation_name).continuous.legend.html")
                        return nothing
                    end
                end

                nested_test("categorical") do
                    graph.data.bars_annotations = [
                        AnnotationData(;
                            title = "is",
                            values = ["yes", "maybe", "no", "yes"],
                            colors = ColorsConfiguration(;
                                palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                            ),
                        ),
                    ]

                    nested_test("()") do
                        test_html(graph, "bars.$(orientation_name).categorical.html")
                        return nothing
                    end

                    nested_test("legend") do
                        graph.data.bars_annotations[1].colors.show_legend = true
                        test_html(graph, "bars.$(orientation_name).categorical.legend.html")
                        return nothing
                    end
                end

                nested_test("both") do
                    graph.data.bars_annotations = [
                        AnnotationData(; title = "score", values = [1, 0.5, 0, 1]),
                        AnnotationData(;
                            title = "is",
                            values = ["yes", "maybe", "no", "yes"],
                            colors = ColorsConfiguration(;
                                palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                            ),
                        ),
                    ]
                    graph.data.bars_names = ["Foo", "Bar", "Baz", "Vaz"]
                    test_html(graph, "bars.$(orientation_name).both.html")
                    return nothing
                end
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
            @test_throws chomp("""
                               ArgumentError: can't specify both graph.configuration.stacking
                               and graph.configuration.series_gap
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("annotations") do
            graph.data.bars_annotations =
                [AnnotationData(; title = "score", values = [1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0])]

            nested_test("fixed") do
                graph.data.bars_annotations[1].colors.fixed = "black"
                @test_throws "ArgumentError: can't specify graph.data.bars_annotations[1].colors.fixed" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end

            nested_test("size") do
                graph.configuration.series_gap = 0.1
                graph.configuration.bars_annotations.size = 0.9
                graph.configuration.bars_annotations.gap = 0.8
                @test_throws chomp("""
                                   ArgumentError: no space left in the value axis
                                   number of graphs: 2
                                   with gap between graphs: 0.1 (total: 0.1)
                                   number of annotations: 1
                                   with gap between annotations: 0.8 (total: 0.8)
                                   with size of each annotation: 0.9 (total: 0.9)
                                   the total overhead: 1.8
                                   is not less than: 1
                                   """) validate(ValidationContext(["graph"]), graph)
            end
        end

        nested_test("values") do
            graph.configuration.stacking = StackFractions
            foos[1] = -1
            @test_throws chomp("""
                               ArgumentError: too low scaled graph.data.series_bars_values[1][1]: -1.0
                               is not at least: 0
                               when using graph.configuration.stacking: StackFractions
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("title") do
            graph.data.value_axis_title = "Values"
            graph.data.series_names = ["Foo", "Bar"]
            graph.configuration.series_gap = 0.01
            @test_throws chomp(
                """
                ArgumentError: can't specify both graph.data.value_axis_title and graph.data.series_names
                together with graph.configuration.series_gap
                """,
            ) validate(ValidationContext(["graph"]), graph)
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

                nested_test("annotations") do
                    nested_test("continuous") do
                        graph.data.bars_annotations =
                            [AnnotationData(; title = "score", values = [1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0])]

                        nested_test("()") do
                            test_html(graph, "series_bars.$(orientation_name).gap.continuous.html")
                            return nothing
                        end

                        nested_test("legend") do
                            graph.data.bars_annotations[1].colors.show_legend = true
                            test_html(graph, "series_bars.$(orientation_name).gap.continuous.legend.html")
                            return nothing
                        end
                    end

                    nested_test("categorical") do
                        graph.data.bars_annotations = [
                            AnnotationData(;
                                title = "is",
                                values = [
                                    "yes",
                                    "maybe",
                                    "no",
                                    "maybe",
                                    "yes",
                                    "maybe",
                                    "no",
                                    "maybe",
                                    "yes",
                                    "maybe",
                                    "no",
                                ],
                                colors = ColorsConfiguration(;
                                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                                ),
                            ),
                        ]

                        nested_test("()") do
                            test_html(graph, "series_bars.$(orientation_name).gap.categorical.html")
                            return nothing
                        end

                        nested_test("legend") do
                            graph.data.bars_annotations[1].colors.show_legend = true
                            test_html(graph, "series_bars.$(orientation_name).gap.categorical.legend.html")
                            return nothing
                        end
                    end

                    nested_test("both") do
                        graph.data.bars_annotations = [
                            AnnotationData(; title = "score", values = [1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0]),
                            AnnotationData(;
                                title = "is",
                                values = [
                                    "yes",
                                    "maybe",
                                    "no",
                                    "maybe",
                                    "yes",
                                    "maybe",
                                    "no",
                                    "maybe",
                                    "yes",
                                    "maybe",
                                    "no",
                                ],
                                colors = ColorsConfiguration(;
                                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                                ),
                            ),
                        ]
                        graph.data.bars_names = "Foo-" .* string.(collect(0:10))
                        test_html(graph, "series_bars.$(orientation_name).gap.both.html")
                        return nothing
                    end
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

            nested_test("annotations") do
                nested_test("continuous") do
                    graph.data.bars_annotations =
                        [AnnotationData(; title = "score", values = [1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0])]

                    nested_test("()") do
                        test_html(graph, "series_bars.$(orientation_name).continuous.html")
                        return nothing
                    end

                    nested_test("legend") do
                        graph.data.bars_annotations[1].colors.show_legend = true
                        test_html(graph, "series_bars.$(orientation_name).continuous.legend.html")
                        return nothing
                    end
                end

                nested_test("categorical") do
                    graph.data.bars_annotations = [
                        AnnotationData(;
                            title = "is",
                            values = [
                                "yes",
                                "maybe",
                                "no",
                                "maybe",
                                "yes",
                                "maybe",
                                "no",
                                "maybe",
                                "yes",
                                "maybe",
                                "no",
                            ],
                            colors = ColorsConfiguration(;
                                palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                            ),
                        ),
                    ]

                    nested_test("()") do
                        test_html(graph, "series_bars.$(orientation_name).categorical.html")
                        return nothing
                    end

                    nested_test("legend") do
                        graph.data.bars_annotations[1].colors.show_legend = true
                        test_html(graph, "series_bars.$(orientation_name).categorical.legend.html")
                        return nothing
                    end
                end

                nested_test("both") do
                    graph.data.bars_annotations = [
                        AnnotationData(; title = "score", values = [1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0]),
                        AnnotationData(;
                            title = "is",
                            values = [
                                "yes",
                                "maybe",
                                "no",
                                "maybe",
                                "yes",
                                "maybe",
                                "no",
                                "maybe",
                                "yes",
                                "maybe",
                                "no",
                            ],
                            colors = ColorsConfiguration(;
                                palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                            ),
                        ),
                    ]
                    graph.data.bars_names = "Foo-" .* string.(collect(0:10))
                    test_html(graph, "series_bars.$(orientation_name).both.html")
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

                nested_test("annotations") do
                    graph.configuration.stacking = StackValues
                    graph.data.series_names = ["Foo", "Bar"]

                    nested_test("continuous") do
                        graph.data.bars_annotations =
                            [AnnotationData(; title = "score", values = [1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0])]

                        nested_test("()") do
                            test_html(graph, "series_bars.$(orientation_name).values.continuous.html")
                            return nothing
                        end

                        nested_test("legend") do
                            graph.data.bars_annotations[1].colors.show_legend = true
                            test_html(graph, "series_bars.$(orientation_name).values.continuous.legend.html")
                            return nothing
                        end
                    end

                    nested_test("categorical") do
                        graph.data.bars_annotations = [
                            AnnotationData(;
                                title = "is",
                                values = [
                                    "yes",
                                    "maybe",
                                    "no",
                                    "maybe",
                                    "yes",
                                    "maybe",
                                    "no",
                                    "maybe",
                                    "yes",
                                    "maybe",
                                    "no",
                                ],
                                colors = ColorsConfiguration(;
                                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                                ),
                            ),
                        ]

                        nested_test("()") do
                            test_html(graph, "series_bars.$(orientation_name).values.categorical.html")
                            return nothing
                        end

                        nested_test("legend") do
                            graph.data.bars_annotations[1].colors.show_legend = true
                            test_html(graph, "series_bars.$(orientation_name).values.categorical.legend.html")
                            return nothing
                        end
                    end

                    nested_test("both") do
                        graph.data.bars_annotations = [
                            AnnotationData(; title = "score", values = [1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0]),
                            AnnotationData(;
                                title = "is",
                                values = [
                                    "yes",
                                    "maybe",
                                    "no",
                                    "maybe",
                                    "yes",
                                    "maybe",
                                    "no",
                                    "maybe",
                                    "yes",
                                    "maybe",
                                    "no",
                                ],
                                colors = ColorsConfiguration(;
                                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                                ),
                            ),
                        ]
                        graph.data.bars_names = "Foo-" .* string.(collect(0:10))
                        test_html(graph, "series_bars.values.$(orientation_name).both.html")
                        return nothing
                    end
                end
            end
        end
    end
end
