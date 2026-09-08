nested_test("bars") do
    graph = bars_graph(; values = ValuesData(collect(0:3) .- 1))

    nested_test("invalid") do
        nested_test("legend") do
            graph.configuration.colors.show_legend = true
            graph.configuration.colors.palette = Dict(["Foo" => "red"])
            graph.data.colors.values = ["Foo", "Foo", "Foo", "Foo"]
            @test_throws chomp("""
                               ArgumentError: can't specify graph.configuration.colors.show_legend
                               for a categorical graph.configuration.colors.palette
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("~names") do
            graph.data.names.values = [1, 2, 3, 4]
            @test_throws "ArgumentError: non-string graph.data.names.values" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end
    end

    nested_test("svg") do
        test_svg(graph, "bars.svg")
        return nothing
    end

    nested_test("fields") do
        fields = values_fields(graph)
        @test fields.data.values === graph.data.values
        @test fields.data.entities === graph.data.bars
        @test fields.configuration.axis === graph.configuration.value_axis

        fields = colors_fields(graph)
        @test fields.data.values === graph.data.colors
        @test fields.data.entities === graph.data.bars
        @test fields.configuration.axis === graph.configuration.colors.axis
        @test fields.configuration.colors === graph.configuration.colors

        annotation = AnnotationData(; values = ValuesData([1, 0.5, 0, 1], "score"))
        @test add_annotation!(graph, annotation) == 1
        @test graph.data.annotations[1] === annotation
        fields = annotations_fields(graph, 1)
        @test fields.data.values === annotation.values
        @test fields.data.entities === graph.data.bars
        @test fields.configuration.colors === annotation.colors

        @test add_annotation!(graph) == 2
        fields = annotations_fields(graph, 2)
        fields.data.values.values = [0, 1, 0, 1]
        @test graph.data.annotations[2].values.values == [0, 1, 0, 1]

        fields = names_fields(graph)
        @test fields.values === graph.data.names
        @test fields.entities === graph.data.bars
        return nothing
    end

    nested_test("mask") do
        graph.data.names.values = ["Foo", "Bar", "Baz", "Vaz"]
        graph.data.annotations = [AnnotationData(; values = ValuesData([1, 0.5, 0, 1], "score"))]

        nested_test("()") do
            graph.data.bars.mask = [true, false, true, true]
            test_html(graph, "bars.mask.html")
            return nothing
        end

        nested_test("!hidden") do
            graph.data.bars.mask = [true, true, false, false]
            graph.data.colors.values = [0, 1, 2, 3]
            graph.configuration.value_axis.include_hidden = false
            graph.configuration.colors.axis.include_hidden = false
            graph.data.annotations[1].colors.axis.include_hidden = false
            test_html(graph, "bars.mask.!hidden.html")
            return nothing
        end
    end

    for (orientation_name, orientation_value) in (("vertical", VerticalValues), ("horizontal", HorizontalValues))
        nested_test(orientation_name) do
            graph.configuration.values_orientation = orientation_value

            nested_test("()") do
                test_html(graph, "bars.$(orientation_name).html")
                return nothing
            end

            nested_test("names") do
                graph.data.names.values = ["Foo", "Bar", "Baz", "Vaz"]
                test_html(graph, "bars.$(orientation_name).names.html")
                return nothing
            end

            nested_test("hovers") do
                graph.data.names.values = ["Foo", "Bar", "Baz", "Vaz"]
                graph.data.bars.hovers = ["H: $(index)" for index in 1:4]
                test_html(graph, "bars.$(orientation_name).hovers.html")
                return nothing
            end

            nested_test("colors") do
                nested_test("named") do
                    graph.data.colors.values = ["red", "green", "blue", "black"]
                    test_html(graph, "bars.$(orientation_name).colors.named.html")
                    return nothing
                end

                nested_test("continuous") do
                    graph.data.colors.values = [0, 1, 2, 3]

                    nested_test("()") do
                        test_html(graph, "bars.$(orientation_name).colors.continuous.html")
                        return nothing
                    end

                    nested_test("legend") do
                        graph.configuration.colors.show_legend = true
                        graph.data.colors.title = "Colors"
                        test_html(graph, "bars.$(orientation_name).colors.continuous.legend.html")
                        return nothing
                    end
                end

                nested_test("categorical") do
                    graph.data.colors.values = ["Foo", "Bar", "Baz", "Bar"]
                    graph.configuration.colors.palette = Dict(["Foo" => "red", "Bar" => "green", "Baz" => "blue"])

                    test_html(graph, "bars.$(orientation_name).colors.categorical.html")
                    return nothing
                end
            end

            nested_test("!gap") do
                graph.configuration.bars.gap = 0
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
                    graph.data.annotations = [AnnotationData(; values = ValuesData([1, 0.5, 0, 1], "score"))]

                    nested_test("()") do
                        test_html(graph, "bars.$(orientation_name).continuous.html")
                        return nothing
                    end

                    nested_test("legend") do
                        graph.data.annotations[1].colors.show_legend = true
                        test_html(graph, "bars.$(orientation_name).continuous.legend.html")
                        return nothing
                    end
                end

                nested_test("categorical") do
                    graph.data.annotations = [
                        AnnotationData(;
                            values = ValuesData(["yes", "maybe", "no", "yes"], "is"),
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
                        graph.data.annotations[1].colors.show_legend = true
                        test_html(graph, "bars.$(orientation_name).categorical.legend.html")
                        return nothing
                    end
                end

                nested_test("both") do
                    graph.data.annotations = [
                        AnnotationData(; values = ValuesData([1, 0.5, 0, 1], "score")),
                        AnnotationData(;
                            values = ValuesData(["yes", "maybe", "no", "yes"], "is"),
                            colors = ColorsConfiguration(;
                                palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                            ),
                        ),
                    ]
                    graph.data.names.values = ["Foo", "Bar", "Baz", "Vaz"]
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
    graph =
        series_bars_graph(; series = [SeriesData(; values = ValuesData(foos)), SeriesData(; values = ValuesData(bars))])

    nested_test("nothing") do
        graph.data.series[1].name = "Foo"
        graph.data.series[2].color = "red"
        graph.data.series[1].hover = "Foo"
        test_html(graph, "series_bars.nothing.html")
        return nothing
    end

    nested_test("fields") do
        fields = series_values_fields(graph, 2)
        @test fields.data.values === graph.data.series[2].values
        @test fields.data.entities === graph.data.series[2].bars
        @test fields.configuration.axis === graph.configuration.value_axis

        annotation = AnnotationData(; values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score"))
        @test add_annotation!(graph, annotation) == 1
        @test graph.data.annotations[1] === annotation
        fields = annotations_fields(graph, 1)
        @test fields.data.values === annotation.values
        @test fields.data.entities === graph.data.bars
        @test fields.configuration.colors === annotation.colors

        @test add_annotation!(graph) == 2
        fields = annotations_fields(graph, 2)
        fields.data.values.values = collect(0:10)
        @test graph.data.annotations[2].values.values == collect(0:10)

        series = SeriesData(; name = "Baz")
        @test add_series!(graph, series) == 3
        @test graph.data.series[3] === series
        fields = series_values_fields(graph, 3)
        @test fields.data.values === series.values
        @test fields.data.entities === series.bars

        @test add_series!(graph) == 4
        fields = series_values_fields(graph, 4)
        fields.data.values.values = collect(0:10)
        @test graph.data.series[4].values.values == collect(0:10)

        fields = names_fields(graph)
        @test fields.values === graph.data.names
        @test fields.entities === graph.data.bars
        return nothing
    end

    nested_test("mask") do
        graph.data.names.values = "Foo-" .* string.(collect(0:10))
        graph.data.annotations =
            [AnnotationData(; values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score"))]

        nested_test("bars") do
            graph.data.bars.mask = [true, false, true, true, false, true, true, false, true, true, false]
            test_html(graph, "series_bars.mask.bars.html")
            return nothing
        end

        nested_test("series") do
            graph.data.series[2].bars.mask = [true, false, true, true, false, true, true, false, true, true, false]
            test_html(graph, "series_bars.mask.series.html")
            return nothing
        end

        nested_test("both") do
            graph.data.bars.mask = [true, true, true, true, true, true, true, true, false, false, false]
            graph.data.series[2].bars.mask = [true, false, true, true, false, true, true, false, true, true, false]
            test_html(graph, "series_bars.mask.both.html")
            return nothing
        end

        nested_test("!hidden") do
            graph.data.bars.mask = [true, true, true, true, true, true, true, true, false, false, false]
            graph.configuration.value_axis.include_hidden = false
            test_html(graph, "series_bars.mask.!hidden.html")
            return nothing
        end
    end

    nested_test("invalid") do
        nested_test("!values") do
            graph.data.series[1].values.values = Float32[]
            @test_throws "ArgumentError: empty vector graph.data.series[1].values.values" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("~values") do
            graph.data.series[2].values.values = [1, 2, 3]
            @test_throws chomp("""
                               ArgumentError: invalid length of graph.data.series[2].values.values: 3
                               is different from length of graph.data.series[1].values.values: 11
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("~titles") do
            graph.data.series[1].values.title = "Foo"
            graph.data.series[2].values.title = "Bar"
            @test_throws chomp("""
                               ArgumentError: conflicting graph.data.series[2].values.title: Bar
                               is different from graph.data.series[1].values.title: Foo
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("~color") do
            graph.data.series[2].color = "Oobleck"
            @test_throws "ArgumentError: invalid graph.data.series[2].color: Oobleck" validate(
                ValidationContext(["graph"]),
                graph,
            )
        end

        nested_test("stacked_mask") do
            graph.configuration.stacking = StackValues
            graph.data.series[2].bars.mask = fill(true, 11)
            @test_throws chomp("""
                               ArgumentError: can't specify both graph.data.series[2].bars.mask
                               and graph.configuration.stacking
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("gap") do
            graph.configuration.stacking = StackValues
            graph.configuration.series_gap = 0.05
            @test_throws chomp("""
                               ArgumentError: can't specify both graph.configuration.stacking
                               and graph.configuration.series_gap
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("annotations") do
            graph.data.annotations =
                [AnnotationData(; values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score"))]

            nested_test("fixed") do
                graph.data.annotations[1].colors.fixed = "black"
                @test_throws "ArgumentError: can't specify graph.data.annotations[1].colors.fixed" validate(
                    ValidationContext(["graph"]),
                    graph,
                )
            end

            nested_test("size") do
                graph.configuration.series_gap = 0.1
                graph.configuration.annotations.size = 0.9
                graph.configuration.annotations.gap = 0.8
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
                               ArgumentError: too low scaled graph.data.series[1].values.values[1]: -1.0
                               is not at least: 0
                               when using graph.configuration.stacking: StackFractions
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("mirrored") do
            push!(graph.data.series, SeriesData(; values = ValuesData(reverse(foos))))
            graph.configuration.mirrored = true
            @test_throws chomp("""
                               ArgumentError: odd number of graph.data.series: 3
                               when using graph.configuration.mirrored
                               """) validate(ValidationContext(["graph"]), graph)
        end

        nested_test("title") do
            graph.data.series[1].values.title = "Values"
            graph.data.series[1].name = "Foo"
            graph.data.series[2].name = "Bar"
            graph.configuration.series_gap = 0.01
            @test_throws chomp(
                """
                ArgumentError: can't specify both graph.data.series[*].values.title and graph.data.series[*].name
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

            nested_test("mirrored") do
                graph.configuration.mirrored = true
                graph.data.names.values = "Foo-" .* string.(collect(0:10))

                nested_test("()") do
                    test_html(graph, "series_bars.$(orientation_name).mirrored.html")
                    return nothing
                end

                # The two sides are the only two sub-graphs, so the annotations are the spine between them.
                nested_test("annotations") do
                    graph.data.annotations =
                        [AnnotationData(; values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score"))]
                    test_html(graph, "series_bars.$(orientation_name).mirrored.annotations.html")
                    return nothing
                end

                nested_test("pairs") do
                    graph.data.series = [
                        SeriesData(; values = ValuesData(foos)),
                        SeriesData(; values = ValuesData(bars)),
                        SeriesData(; values = ValuesData(reverse(foos))),
                        SeriesData(; values = ValuesData(reverse(bars))),
                    ]

                    nested_test("()") do
                        test_html(graph, "series_bars.$(orientation_name).mirrored.pairs.html")
                        return nothing
                    end

                    # Both pairs still share the two sides, so there is still a single spine to annotate.
                    nested_test("annotations") do
                        graph.data.annotations = [
                            AnnotationData(; values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score")),
                        ]
                        test_html(graph, "series_bars.$(orientation_name).mirrored.pairs.annotations.html")
                        return nothing
                    end

                    nested_test("gap") do
                        graph.configuration.series_gap = 0.05

                        nested_test("()") do
                            test_html(graph, "series_bars.$(orientation_name).mirrored.pairs.gap.html")
                            return nothing
                        end

                        # Each pair has a middle of its own and the graph has none, so the annotations stay outside.
                        nested_test("annotations") do
                            graph.data.annotations = [
                                AnnotationData(;
                                    values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score"),
                                ),
                            ]
                            test_html(graph, "series_bars.$(orientation_name).mirrored.pairs.gap.annotations.html")
                            return nothing
                        end
                    end

                    nested_test("stacked") do
                        graph.configuration.stacking = StackValues
                        test_html(graph, "series_bars.$(orientation_name).mirrored.pairs.stacked.html")
                        return nothing
                    end

                    nested_test("fractions") do
                        graph.configuration.stacking = StackFractions
                        test_html(graph, "series_bars.$(orientation_name).mirrored.pairs.fractions.html")
                        return nothing
                    end
                end
            end

            nested_test("gap") do
                graph.configuration.series_gap = 0.05

                nested_test("()") do
                    test_html(graph, "series_bars.$(orientation_name).gap.html")
                    return nothing
                end

                nested_test("names") do
                    graph.data.series[1].name = "Foo"
                    graph.data.series[2].name = "Bar"
                    test_html(graph, "series_bars.$(orientation_name).gap.names.html")
                    return nothing
                end

                nested_test("annotations") do
                    nested_test("continuous") do
                        graph.data.annotations = [
                            AnnotationData(; values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score")),
                        ]

                        nested_test("()") do
                            test_html(graph, "series_bars.$(orientation_name).gap.continuous.html")
                            return nothing
                        end

                        nested_test("legend") do
                            graph.data.annotations[1].colors.show_legend = true
                            test_html(graph, "series_bars.$(orientation_name).gap.continuous.legend.html")
                            return nothing
                        end
                    end

                    nested_test("categorical") do
                        graph.data.annotations = [
                            AnnotationData(;
                                values = ValuesData(
                                    [
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
                                    "is",
                                ),
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
                            graph.data.annotations[1].colors.show_legend = true
                            test_html(graph, "series_bars.$(orientation_name).gap.categorical.legend.html")
                            return nothing
                        end
                    end

                    nested_test("both") do
                        graph.data.annotations = [
                            AnnotationData(; values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score")),
                            AnnotationData(;
                                values = ValuesData(
                                    [
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
                                    "is",
                                ),
                                colors = ColorsConfiguration(;
                                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                                ),
                            ),
                        ]
                        graph.data.names.values = "Foo-" .* string.(collect(0:10))
                        test_html(graph, "series_bars.$(orientation_name).gap.both.html")
                        return nothing
                    end
                end
            end

            nested_test("hovers") do
                nested_test("both") do
                    graph.data.series[1].hover = "Foo"
                    graph.data.series[2].hover = "Bar"
                    graph.data.bars.hovers = "B-" .* string.(collect(0:10))
                    test_html(graph, "series_bars.$(orientation_name).hovers.both.html")
                    return nothing
                end

                nested_test("bars") do
                    graph.data.bars.hovers = "B-" .* string.(collect(0:10))
                    test_html(graph, "series_bars.$(orientation_name).hovers.bars.html")
                    return nothing
                end

                nested_test("series") do
                    graph.data.series[1].hover = "Foo"
                    graph.data.series[2].hover = "Bar"
                    test_html(graph, "series_bars.$(orientation_name).hovers.series.html")
                    return nothing
                end

                nested_test("series_bars") do
                    graph.data.series[1].bars.hovers = "F-" .* string.(collect(0:10))
                    graph.data.series[2].bars.hovers = "B-" .* string.(collect(0:10))

                    nested_test("()") do
                        test_html(graph, "series_bars.$(orientation_name).hovers.series_bars.html")
                        return nothing
                    end

                    nested_test("bars") do
                        graph.data.bars.hovers = "S-" .* string.(collect(0:10))
                        test_html(graph, "series_bars.$(orientation_name).hovers.series_bars.bars.html")
                        return nothing
                    end

                    nested_test("series") do
                        graph.data.series[1].hover = "Foo"
                        graph.data.series[2].hover = "Bar"
                        test_html(graph, "series_bars.$(orientation_name).hovers.series_bars.series.html")
                        return nothing
                    end
                end
            end

            nested_test("annotations") do
                nested_test("continuous") do
                    graph.data.annotations =
                        [AnnotationData(; values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score"))]

                    nested_test("()") do
                        test_html(graph, "series_bars.$(orientation_name).continuous.html")
                        return nothing
                    end

                    nested_test("legend") do
                        graph.data.annotations[1].colors.show_legend = true
                        test_html(graph, "series_bars.$(orientation_name).continuous.legend.html")
                        return nothing
                    end
                end

                nested_test("categorical") do
                    graph.data.annotations = [
                        AnnotationData(;
                            values = ValuesData(
                                ["yes", "maybe", "no", "maybe", "yes", "maybe", "no", "maybe", "yes", "maybe", "no"],
                                "is",
                            ),
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
                        graph.data.annotations[1].colors.show_legend = true
                        test_html(graph, "series_bars.$(orientation_name).categorical.legend.html")
                        return nothing
                    end
                end

                nested_test("both") do
                    graph.data.annotations = [
                        AnnotationData(; values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score")),
                        AnnotationData(;
                            values = ValuesData(
                                ["yes", "maybe", "no", "maybe", "yes", "maybe", "no", "maybe", "yes", "maybe", "no"],
                                "is",
                            ),
                            colors = ColorsConfiguration(;
                                palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                            ),
                        ),
                    ]
                    graph.data.names.values = "Foo-" .* string.(collect(0:10))
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
                        graph.data.series[1].name = "Foo"
                        graph.data.series[2].name = "Bar"
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
                    graph.data.series[1].name = "Foo"
                    graph.data.series[2].name = "Bar"

                    nested_test("continuous") do
                        graph.data.annotations = [
                            AnnotationData(; values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score")),
                        ]

                        nested_test("()") do
                            test_html(graph, "series_bars.$(orientation_name).values.continuous.html")
                            return nothing
                        end

                        nested_test("legend") do
                            graph.data.annotations[1].colors.show_legend = true
                            test_html(graph, "series_bars.$(orientation_name).values.continuous.legend.html")
                            return nothing
                        end
                    end

                    nested_test("categorical") do
                        graph.data.annotations = [
                            AnnotationData(;
                                values = ValuesData(
                                    [
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
                                    "is",
                                ),
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
                            graph.data.annotations[1].colors.show_legend = true
                            test_html(graph, "series_bars.$(orientation_name).values.categorical.legend.html")
                            return nothing
                        end
                    end

                    nested_test("both") do
                        graph.data.annotations = [
                            AnnotationData(; values = ValuesData([1, 0.5, 0, 0.5, 1, 0.5, 0, 0.5, 1, 0.5, 0], "score")),
                            AnnotationData(;
                                values = ValuesData(
                                    [
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
                                    "is",
                                ),
                                colors = ColorsConfiguration(;
                                    palette = Dict("yes" => "black", "maybe" => "darkgray", "no" => "lightgray"),
                                ),
                            ),
                        ]
                        graph.data.names.values = "Foo-" .* string.(collect(0:10))
                        test_html(graph, "series_bars.values.$(orientation_name).both.html")
                        return nothing
                    end
                end
            end
        end
    end
end
