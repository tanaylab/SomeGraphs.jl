function test_distributions(  # UNTESTED
    setup::Function,
    graph::Graph,
    plurality::AbstractString,
    kind::AbstractString,
    test_kind::Maybe{AbstractString} = nothing,
)::Nothing
    if test_kind === nothing
        test_kind = kind
    end

    nested_test(test_kind) do
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
                            @test_throws chomp("""
                                               unsupported graph.configuration.distribution.line.width: 4
                                               for graph.configuration.distribution.style: HistogramDistribution
                                               """) validate(ValidationContext(["graph"]), graph)
                        end

                        nested_test("~fill") do
                            graph.configuration.distribution.line.is_filled = false
                            @test_throws chomp("""
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

                nested_test("!grid") do
                    graph.configuration.value_axis.show_grid = false
                    if startswith(kind, "cumulative")
                        graph.configuration.density_axis.show_grid = false
                    end
                    test_html(graph, "$(plurality).$(kind).$(name).!grid.html")
                    return nothing
                end

                nested_test("grid_color") do
                    graph.configuration.value_axis.grid_color = "red"
                    if startswith(kind, "cumulative")
                        graph.configuration.density_axis.grid_color = "red"
                    end
                    test_html(graph, "$(plurality).$(kind).$(name).grid_color.html")
                    return nothing
                end

                nested_test("ticks") do
                    graph.configuration.value_axis.show_ticks = false
                    if startswith(kind, "cumulative")
                        graph.configuration.density_axis.show_ticks = false
                    end
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

                if startswith(kind, "cumulative")
                    nested_test("descending") do
                        graph.configuration.distribution.cumulative_descending = true
                        test_html(graph, "$(plurality).$(kind).$(name).descending.html")
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

                # The value axis is shared by all the distributions, so the value bands apply to both pluralities. The
                # offsets have to lie in the range of the values, which is ten times smaller for the multiple
                # distributions.
                low_offset, middle_offset, high_offset = plurality == "distribution" ? (50, 80, 120) : (5, 8, 12)

                nested_test("value_lines") do
                    graph.configuration.value_bands.low.offset = low_offset
                    graph.data.value_bands.middle_offset = middle_offset
                    graph.configuration.value_bands.high.offset = high_offset

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

                    graph.configuration.value_bands.low.offset = low_offset
                    graph.data.value_bands.high_offset = high_offset

                    test_html(graph, "$(plurality).$(kind).$(name).value_fills.html")
                    return nothing
                end

                if plurality == "distribution"
                    if startswith(kind, "cumulative")
                        nested_test("cumulative_lines") do
                            graph.configuration.cumulative_bands.low.offset = 0.25
                            graph.data.cumulative_bands.middle_offset = 0.5
                            graph.configuration.cumulative_bands.high.offset = 0.75

                            @assert !graph.configuration.cumulative_bands.low.line.is_filled
                            @assert !graph.configuration.cumulative_bands.middle.line.is_filled
                            @assert !graph.configuration.cumulative_bands.high.line.is_filled

                            test_html(graph, "$(plurality).$(kind).$(name).cumulative_lines.html")
                            return nothing
                        end

                        nested_test("cumulative_fills") do
                            graph.configuration.cumulative_bands.low.line.is_filled = true
                            graph.configuration.cumulative_bands.middle.line.is_filled = true
                            graph.configuration.cumulative_bands.high.line.is_filled = true

                            graph.configuration.cumulative_bands.low.line.style = DashDotLine
                            graph.configuration.cumulative_bands.middle.line.style = nothing
                            graph.configuration.cumulative_bands.high.line.style = DashDotLine

                            graph.configuration.cumulative_bands.low.line.color = "green"
                            graph.configuration.cumulative_bands.middle.line.color = "red"
                            graph.configuration.cumulative_bands.high.line.color = "blue"

                            graph.configuration.cumulative_bands.low.offset = 0.25
                            graph.data.cumulative_bands.high_offset = 0.75

                            test_html(graph, "$(plurality).$(kind).$(name).cumulative_fills.html")
                            return nothing
                        end
                    end

                elseif plurality == "distributions"
                    nested_test("names") do
                        graph.data.distributions[1].name = "Foo"
                        graph.data.distributions[2].name = "Bar"

                        nested_test("()") do
                            test_html(graph, "$(plurality).$(kind).$(name).names.html")
                            return nothing
                        end

                        nested_test("priorities") do
                            graph.data.order = [2, 1]
                            test_html(graph, "$(plurality).$(kind).$(name).names.priorities.html")
                            return nothing
                        end
                    end

                    nested_test("!gap") do
                        graph.configuration.distributions_gap = nothing

                        if contains(kind, "box")
                            @test_throws "overlay (no graph.configuration.distributions_gap specified) for box distributions" validate(
                                ValidationContext(["graph"]),
                                graph,
                            )
                        else
                            nested_test("()") do
                                test_html(graph, "$(plurality).$(kind).$(name).!gap.html")
                                return nothing
                            end

                            nested_test("names") do
                                graph.data.distributions[1].name = "Foo"
                                graph.data.distributions[2].name = "Bar"
                                test_html(graph, "$(plurality).$(kind).$(name).!gap.names.html")
                                return nothing
                            end
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
        distribution = DistributionData(;
            values = ValuesData([
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
            ]),
        ),
    )

    nested_test("show") do
        @test "$(graph)" ==
              "Graph{DistributionGraphData, DistributionGraphConfiguration} (use .figure to show the graph)"
    end

    nested_test("mask") do
        n_values = length(graph.data.distribution.values.values)

        nested_test("()") do
            graph.data.distribution.points.mask = [index % 3 != 0 for index in 1:n_values]
            test_html(graph, "distribution.mask.html")
            return nothing
        end

        nested_test("!hidden") do
            graph.data.distribution.points.mask = [index < n_values - 1 for index in 1:n_values]
            graph.configuration.value_axis.include_hidden = false
            test_html(graph, "distribution.mask.!hidden.html")
            return nothing
        end
    end

    nested_test("hovers") do
        n_values = length(graph.data.distribution.values.values)
        graph.data.distribution.points.hovers = ["V: $(index)" for index in 1:n_values]

        nested_test("cumulative") do
            graph.configuration.distribution.style = CumulativeDistribution
            test_html(graph, "distribution.hovers.cumulative.html")
            return nothing
        end

        nested_test("box_outliers") do
            graph.configuration.distribution.style = BoxOutliersDistribution
            graph.data.distribution.hover = "Distribution"
            test_html(graph, "distribution.hovers.box_outliers.html")
            return nothing
        end
    end

    nested_test("invalid") do
        nested_test("!values") do
            graph.data.distribution.values.values = nothing
            @test_throws "ArgumentError: must specify graph.data.distribution.values.values" graph.figure
        end

        nested_test("~values") do
            graph.data.distribution.values.values = ["Foo"]
            @test_throws "ArgumentError: non-numeric graph.data.distribution.values.values" graph.figure
        end

        nested_test("~mask") do
            graph.data.distribution.points.mask = [true, false]
            @test_throws chomp("""
                               ArgumentError: invalid length of graph.data.distribution.points.mask: 2
                               is different from length of graph.data.distribution.values.values: 274
                               """) graph.figure
        end

        nested_test("~shown") do
            graph.data.distribution.is_shown = false
            @test_throws "ArgumentError: not is_shown graph.data.distribution" graph.figure
        end

        nested_test("~density_axis") do
            graph.configuration.density_axis.show_ticks = false
            @test_throws chomp("""
                               ArgumentError: specified graph.configuration.density_axis
                               for graph.configuration.distribution.style: CurveDistribution
                               """) graph.figure
        end

        nested_test("~color") do
            graph.data.distribution.color = "Oobleck"
            @test_throws "ArgumentError: invalid graph.data.distribution.color: Oobleck" graph.figure
        end

        nested_test("~normalize") do
            graph.configuration.distribution.normalize = true
            @test_throws chomp("""
                               ArgumentError: specified graph.configuration.distribution.normalize
                               for graph.configuration.distribution.style: CurveDistribution
                               """) graph.figure
        end

        nested_test("~cumulative_descending") do
            graph.configuration.distribution.style = HistogramDistribution
            graph.configuration.distribution.cumulative_descending = true
            @test_throws chomp("""
                               ArgumentError: specified graph.configuration.distribution.cumulative_descending
                               for non-cumulative graph.configuration.distribution.style: HistogramDistribution
                               """) graph.figure
        end

        nested_test("~percent") do
            graph.configuration.distribution.style = HistogramDistribution
            graph.configuration.density_axis.percent = true
            @test_throws chomp("""
                               ArgumentError: specified graph.configuration.density_axis.percent
                               without graph.configuration.distribution.normalize
                               """) graph.figure
        end

        nested_test("~log") do
            graph.configuration.distribution.style = HistogramDistribution
            graph.configuration.density_axis.log_scale = Log10Scale
            @test_throws(
                "ArgumentError: unsupported graph.configuration.density_axis.log_scale: Log10Scale",
                graph.figure
            )
        end

        nested_test("~ticks_angle") do
            graph.configuration.distribution.style = HistogramDistribution
            graph.configuration.density_axis.ticks_angle = 91
            @test_throws chomp("""
                               ArgumentError: too high graph.configuration.density_axis.ticks_angle: 91
                               is not at most: 90
                               """) graph.figure
        end

        nested_test("~bands") do
            graph.configuration.cumulative_bands.middle.offset = 0.5
            @test_throws chomp("""
                               ArgumentError: specified graph.configuration.cumulative_bands
                               for non-cumulative graph.configuration.distribution.style: CurveDistribution
                               """) graph.figure
        end
    end

    for (name, style) in (
        ("curve", CurveDistribution),
        ("curve_box", CurveBoxDistribution),
        ("violin", ViolinDistribution),
        ("violin_box", ViolinBoxDistribution),
        ("box", BoxDistribution),
        ("box_outliers", BoxOutliersDistribution),
        ("histogram", HistogramDistribution),
    )
        test_distributions(graph, "distribution", name) do
            graph.configuration.distribution.style = style
            return nothing
        end
    end

    nested_test("cumulative") do
        for name in ("fractions", "percents", "counts")
            test_distributions(graph, "distribution", "cumulative.$(name)", name) do
                graph.configuration.distribution.style = CumulativeDistribution
                if name == "fractions"
                    graph.configuration.distribution.normalize = true
                elseif name == "percents"
                    graph.configuration.distribution.normalize = true
                    graph.configuration.density_axis.percent = true
                else
                    @assert name == "counts"
                    graph.data.distribution.name = "Counts"
                end
                return nothing
            end
        end
    end

    nested_test("histogram_density") do
        graph.configuration.distribution.style = HistogramDistribution

        for (name, orientation) in (("vertical", VerticalValues), ("horizontal", HorizontalValues))
            nested_test(name) do
                graph.configuration.distribution.values_orientation = orientation

                nested_test("normalize") do
                    graph.configuration.distribution.normalize = true
                    test_html(graph, "distribution.histogram_density.$(name).normalize.html")
                    return nothing
                end

                nested_test("percent") do
                    graph.configuration.distribution.normalize = true
                    graph.configuration.density_axis.percent = true
                    test_html(graph, "distribution.histogram_density.$(name).percent.html")
                    return nothing
                end

                nested_test("ticks_angle") do
                    graph.configuration.density_axis.ticks_angle = 45
                    test_html(graph, "distribution.histogram_density.$(name).ticks_angle.html")
                    return nothing
                end

                nested_test("range") do
                    graph.configuration.density_axis.minimum = 0
                    graph.configuration.density_axis.maximum = 50
                    test_html(graph, "distribution.histogram_density.$(name).range.html")
                    return nothing
                end
            end
        end
    end
end

nested_test("distributions") do
    graph = distributions_graph(;
        distributions = [
            #! format: off
            DistributionData(; values = ValuesData([
                0.75, 5.25, 5.5, 6, 6.2, 6.6, 6.80, 7.0, 7.2, 7.5, 7.5, 7.75, 8.15, 8.15, 8.65, 8.93, 9.2, 9.5, 10,
                10.25, 11.5, 12, 16, 20.90, 22.3, 23.25,
            ])), DistributionData(; values = ValuesData([
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
            ] ./ 10.0))
            #! format: on
        ],
    )

    nested_test("nothing") do
        graph.data.distributions[1].name = "Foo"
        graph.data.distributions[2].color = "red"
        test_html(graph, "distributions.nothing.html")
        return nothing
    end

    nested_test("mask") do
        n_values = length(graph.data.distributions[2].values.values)

        nested_test("()") do
            graph.data.distributions[2].points.mask = [index % 3 != 0 for index in 1:n_values]
            test_html(graph, "distributions.mask.html")
            return nothing
        end

        nested_test("!hidden") do
            graph.data.distributions[2].points.mask = [index < n_values - 1 for index in 1:n_values]
            graph.configuration.value_axis.include_hidden = false
            test_html(graph, "distributions.mask.!hidden.html")
            return nothing
        end
    end

    nested_test("hovers") do
        graph.configuration.distribution.style = CumulativeDistribution
        n_values = length(graph.data.distributions[2].values.values)
        graph.data.distributions[1].hover = "Foo"
        graph.data.distributions[2].points.hovers = ["V: $(index)" for index in 1:n_values]
        test_html(graph, "distributions.hovers.html")
        return nothing
    end

    nested_test("!shown") do
        graph.data.distributions[1].name = "Foo"
        graph.data.distributions[2].name = "Bar"
        graph.data.distributions[1].is_shown = false
        test_html(graph, "distributions.!shown.html")
        return nothing
    end

    nested_test("invalid") do
        nested_test("~density_axis") do
            graph.configuration.density_axis.show_ticks = false
            @test_throws chomp("""
                               ArgumentError: specified graph.configuration.density_axis
                               for graph.configuration.distribution.style: CurveDistribution
                               """) graph.figure
        end

        nested_test("~percent") do
            graph.configuration.distribution.style = HistogramDistribution
            graph.configuration.density_axis.percent = true
            @test_throws chomp("""
                               ArgumentError: specified graph.configuration.density_axis.percent
                               without graph.configuration.distribution.normalize
                               """) graph.figure
        end

        nested_test("!distributions") do
            empty!(graph.data.distributions)
            @test_throws "ArgumentError: empty vector graph.data.distributions" graph.figure
        end

        nested_test("!values") do
            empty!(graph.data.distributions[1].values.values)
            @test_throws "ArgumentError: empty vector graph.data.distributions[1].values.values" graph.figure
        end

        nested_test("~order") do
            graph.data.order = [1]
            @test_throws chomp("""
                               ArgumentError: invalid length of graph.data.order: 1
                               is different from length of graph.data.distributions: 2
                               """) graph.figure
        end

        nested_test("~values_titles") do
            graph.data.distributions[1].values.title = "Foo"
            graph.data.distributions[2].values.title = "Bar"
            @test_throws chomp("""
                               ArgumentError: conflicting graph.data.distributions[2].values.title: Bar
                               is different from graph.data.distributions[1].values.title: Foo
                               """) graph.figure
        end

        nested_test("~both_titles") do
            graph.data.density_axis_title = "Density"
            graph.data.series_axis_title = "Series"
            @test_throws chomp("""
                               ArgumentError: can't specify both graph.data.density_axis_title: Density
                               and graph.data.series_axis_title: Series
                               """) graph.figure
        end

        nested_test("~series_axis_title") do
            graph.configuration.distributions_gap = nothing
            graph.data.series_axis_title = "Series"
            @test_throws chomp("""
                               ArgumentError: can't specify graph.data.series_axis_title: Series
                               for overlay (no graph.configuration.distributions_gap)
                               """) graph.figure
        end

        nested_test("~series_axis_numeric") do
            graph.configuration.series_axis.percent = true
            @test_throws chomp("""
                               ArgumentError: specified numeric or grid fields of graph.configuration.series_axis
                               (only show_ticks, ticks_angle and title apply to the cross-series names)
                               """) graph.figure
        end

        nested_test("~series_axis_grid") do
            graph.configuration.series_axis.show_grid = false
            @test_throws chomp("""
                               ArgumentError: specified numeric or grid fields of graph.configuration.series_axis
                               (only show_ticks, ticks_angle and title apply to the cross-series names)
                               """) graph.figure
        end

        nested_test("~series_axis_overlay") do
            graph.configuration.distributions_gap = nothing
            graph.configuration.series_axis.ticks_angle = 45
            @test_throws chomp("""
                               ArgumentError: specified graph.configuration.series_axis
                               for overlay (no graph.configuration.distributions_gap)
                               """) graph.figure
        end

        nested_test("~both_axis_titles") do
            graph.configuration.distribution.style = HistogramDistribution
            graph.configuration.density_axis.title = "Density"
            graph.configuration.series_axis.title = "Series"
            @test_throws chomp("""
                               ArgumentError: can't specify both graph.configuration.density_axis.title
                               and graph.configuration.series_axis.title
                               """) graph.figure
        end

        nested_test("~log") do
            graph.configuration.distribution.style = HistogramDistribution
            graph.configuration.density_axis.log_scale = Log10Scale
            @test_throws(
                "ArgumentError: unsupported graph.configuration.density_axis.log_scale: Log10Scale",
                graph.figure
            )
        end

        nested_test("~color") do
            graph.data.distributions[2].color = "Oobleck"
            @test_throws "ArgumentError: invalid graph.data.distributions[2].color: Oobleck" graph.figure
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

    nested_test("cumulative") do
        for name in ("fractions", "percents", "counts")
            test_distributions(graph, "distributions", "cumulative.$(name)", name) do
                graph.configuration.distribution.style = CumulativeDistribution
                if name == "fractions"
                    graph.configuration.distribution.normalize = true
                elseif name == "percents"
                    graph.configuration.distribution.normalize = true
                    graph.configuration.density_axis.percent = true
                else
                    @assert name == "counts"
                end
                return nothing
            end
        end
    end

    nested_test("series_axis") do
        graph.configuration.distribution.style = BoxDistribution
        graph.data.distributions[1].name = "Foo"
        graph.data.distributions[2].name = "Bar"

        for (name, orientation) in (("vertical", VerticalValues), ("horizontal", HorizontalValues))
            nested_test(name) do
                graph.configuration.distribution.values_orientation = orientation

                nested_test("ticks_angle") do
                    graph.configuration.series_axis.ticks_angle = 45
                    test_html(graph, "distributions.series_axis.$(name).ticks_angle.html")
                    return nothing
                end

                nested_test("title") do
                    graph.data.series_axis_title = "Series"
                    test_html(graph, "distributions.series_axis.$(name).title.html")
                    return nothing
                end

                nested_test("!ticks") do
                    graph.configuration.series_axis.show_ticks = false

                    nested_test("()") do
                        test_html(graph, "distributions.series_axis.$(name).!ticks.html")
                        return nothing
                    end

                    nested_test("title") do
                        graph.data.series_axis_title = "Series"
                        test_html(graph, "distributions.series_axis.$(name).!ticks.title.html")
                        return nothing
                    end
                end
            end
        end
    end

    nested_test("histogram_density") do
        graph.configuration.distribution.style = HistogramDistribution
        graph.configuration.density_axis.minimum = 0
        graph.configuration.density_axis.maximum = 50

        for (name, orientation) in (("vertical", VerticalValues), ("horizontal", HorizontalValues))
            nested_test(name) do
                graph.configuration.distribution.values_orientation = orientation
                test_html(graph, "distributions.histogram_density.$(name).range.html")
                return nothing
            end
        end
    end
end
