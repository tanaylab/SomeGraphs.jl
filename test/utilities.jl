function test_same_values(
    actual_values::Union{AbstractVector{<:Real}, AbstractVector{<:Maybe{Real}}},
    expected_values::Union{AbstractVector{<:Real}, AbstractVector{<:Maybe{Real}}},
)::Nothing
    @test length(actual_values) == length(expected_values)
    for (index, (actual_value, expected_value)) in enumerate(zip(actual_values, expected_values))
        @test (actual_value === nothing) == (expected_value === nothing)
        if actual_value !== nothing
            @assert abs(actual_value - expected_value) < 1e-7 "at $(index): $(actual_value) != $(expected_value)"
        end
    end
end

nested_test("utilities") do
    nested_test("plotly_figure") do
        figure = plotly_figure(SomeGraphs.Utilities.scatter(; x = [1, 2]), SomeGraphs.Utilities.Layout())
        @test length(figure.data) == 1
    end

    nested_test("plotly_axis_value") do
        @test plotly_axis_value(ScaleConfiguration(), nothing; is_plotly_log = false) === nothing
    end

    # A percent scale takes its log after converting the offset to percents.
    nested_test("scale_axis_offset") do
        scale_axis_offset = SomeGraphs.Utilities.scale_axis_offset
        @test scale_axis_offset(ScaleConfiguration(; percent = true, log_base = Log10Base), 0.1) ≈ 1.0
        @test scale_axis_offset(ScaleConfiguration(; log_base = Log10Base), 10.0) ≈ 1.0
    end

    nested_test("scale_axis_values") do
        values = [1, nothing]

        nested_test("default") do
            @test scale_axis_values(ScaleConfiguration(), values) == [1.0, nothing]
        end

        nested_test("log10") do
            values = [1, 10, nothing]
            return test_same_values(
                scale_axis_values(ScaleConfiguration(; log_base = Log10Base), values),
                [0.0, 1.0, nothing],
            )
        end

        nested_test("log2") do
            values = [1, 2, nothing]
            return test_same_values(
                scale_axis_values(ScaleConfiguration(; log_base = Log2Base), values),
                [0.0, 1.0, nothing],
            )
        end

        nested_test("percent") do
            nested_test("()") do
                return test_same_values(scale_axis_values(ScaleConfiguration(; percent = true), values), [100, nothing])
            end

            nested_test("log10") do
                values = [1.0, sqrt(10), 10.0, nothing]
                return test_same_values(
                    scale_axis_values(ScaleConfiguration(; log_base = Log10Base), values),
                    [0.0, 0.5, 1.0, nothing],
                )
            end

            nested_test("log2") do
                values = [1.0, sqrt(2), 2.0, nothing]
                return test_same_values(
                    scale_axis_values(ScaleConfiguration(; log_base = Log2Base), values),
                    [0.0, 0.5, 1.0, nothing],
                )
            end
        end
    end

    nested_test("scale_size_values") do
        sizes_configuration = SizesConfiguration()

        nested_test("default") do
            test_same_values(scale_size_values(sizes_configuration, [1, 2, 3]), [6, 12, 18])
            return nothing
        end

        nested_test("smallest") do
            sizes_configuration.smallest = 2
            test_same_values(scale_size_values(sizes_configuration, [1, 2, 3]), [2, 8, 14])
            return nothing
        end

        nested_test("span") do
            sizes_configuration.span = 10
            test_same_values(scale_size_values(sizes_configuration, [1, 2, 3]), [6, 11, 16])
            return nothing
        end

        nested_test("both") do
            sizes_configuration.smallest = 2
            sizes_configuration.span = 10
            test_same_values(scale_size_values(sizes_configuration, [1, 2, 3]), [2, 7, 12])
            return nothing
        end

        nested_test("same") do
            test_same_values(scale_size_values(sizes_configuration, [1]), [6])
            return nothing
        end

        nested_test("nothing") do
            @test scale_size_values(sizes_configuration, nothing) === nothing
        end

        nested_test("log") do
            sizes_configuration.scale.log_base = Log2Base
            sizes_configuration.scale.log_regularization = 1

            nested_test("()") do
                test_same_values(scale_size_values(sizes_configuration, [0, 1, 3]), [6, 12, 18])
                return nothing
            end

            nested_test("minimum") do
                sizes_configuration.scale.minimum = 0
                test_same_values(scale_size_values(sizes_configuration, [-1, 1, 3]), [6, 12, 18])
                return nothing
            end

            nested_test("maximum") do
                sizes_configuration.scale.maximum = 3
                test_same_values(scale_size_values(sizes_configuration, [0, 1, 4]), [6, 12, 18])
                return nothing
            end

            nested_test("smallest") do
                sizes_configuration.smallest = 2
                test_same_values(scale_size_values(sizes_configuration, [0, 1, 3]), [2, 8, 14])
                return nothing
            end

            nested_test("span") do
                sizes_configuration.span = 10
                test_same_values(scale_size_values(sizes_configuration, [0, 1, 3]), [6, 11, 16])
                return nothing
            end

            nested_test("both") do
                sizes_configuration.smallest = 2
                sizes_configuration.span = 10
                test_same_values(scale_size_values(sizes_configuration, [0, 1, 3]), [2, 7, 12])
                return nothing
            end
        end
    end

    nested_test("sizes_legend_entries") do
        sizes_configuration = SizesConfiguration()

        function test_entries(values, labels, pixel_sizes)::Nothing
            entries = sizes_legend_entries(sizes_configuration, values)
            @test [entry.label for entry in entries] == labels
            test_same_values([entry.pixel_size for entry in entries], pixel_sizes)
            return nothing
        end

        nested_test("linear") do
            nested_test("()") do
                return test_entries(
                    collect(0:100),
                    ["0", "20", "40", "60", "80", "100"],
                    [6, 8.4, 10.8, 13.2, 15.6, 18],
                )
            end

            nested_test("decimals") do
                return test_entries(
                    [3.2, 4.1],
                    ["3.2", "3.4", "3.6", "3.8", "4.0"],
                    6 .+ 12 .* ([3.2, 3.4, 3.6, 3.8, 4.0] .- 3.2) ./ 0.9,
                )
            end

            # The common prefix of the values costs no decimal places.
            nested_test("prefix") do
                return test_entries(
                    [1000.12, 1000.61],
                    ["1000.2", "1000.3", "1000.4", "1000.5", "1000.6"],
                    6 .+ 12 .* ([1000.2, 1000.3, 1000.4, 1000.5, 1000.6] .- 1000.12) ./ (1000.61 - 1000.12),
                )
            end

            nested_test("negative") do
                return test_entries([-5, 5], ["-4", "-2", "0", "2", "4"], [7.2, 9.6, 12, 14.4, 16.8])
            end

            nested_test("small") do
                return test_entries(
                    [0.00001, 0.00005],
                    ["0.00001", "0.00002", "0.00003", "0.00004", "0.00005"],
                    [6, 9, 12, 15, 18],
                )
            end

            # The entries must be at least 1 pixel apart.
            nested_test("span") do
                sizes_configuration.span = 3
                return test_entries(collect(0:10), ["0", "5", "10"], [6, 7.5, 9])
            end

            nested_test("single") do
                return test_entries([3.14159], ["3.14"], [6])
            end

            nested_test("minimum") do
                sizes_configuration.scale.minimum = 20
                return test_entries(collect(0:100), ["≤ 20", "40", "60", "80", "100"], [6, 9, 12, 15, 18])
            end

            # Nothing is below the minimum, so it is not marked as clamped.
            nested_test("!clamped") do
                sizes_configuration.scale.minimum = 0
                return test_entries(
                    collect(0:100),
                    ["0", "20", "40", "60", "80", "100"],
                    [6, 8.4, 10.8, 13.2, 15.6, 18],
                )
            end
        end

        nested_test("log10") do
            sizes_configuration.scale.log_base = Log10Base

            nested_test("decades") do
                return test_entries([1, 100], ["1", "10", "100"], [6, 12, 18])
            end

            nested_test("si") do
                return test_entries([1, 1e6], ["1", "100", "10k", "1M"], [6, 10, 14, 18])
            end

            nested_test("digits") do
                return test_entries([1, 5], ["1", "2", "3", "4", "5"], 6 .+ 12 .* log10.([1, 2, 3, 4, 5]) ./ log10(5))
            end

            # Within less than a decade, the steps are linear.
            nested_test("linear") do
                return test_entries(
                    [20, 30],
                    ["20", "22", "24", "26", "28", "30"],
                    6 .+ 12 .* log10.([20, 22, 24, 26, 28, 30] ./ 20) ./ log10(1.5),
                )
            end

            # The values include the regularization, as on an axis.
            nested_test("regularization") do
                sizes_configuration.scale.log_regularization = 1
                return test_entries(collect(0:100), ["1", "10", "100"], 6 .+ 12 .* log10.([1, 10, 100]) ./ log10(101))
            end

            nested_test("maximum") do
                sizes_configuration.scale.maximum = 5
                return test_entries(
                    [1, 10],
                    ["1", "2", "3", "4", "≥ 5"],
                    6 .+ 12 .* log10.([1, 2, 3, 4, 5]) ./ log10(5),
                )
            end
        end

        nested_test("log2") do
            sizes_configuration.scale.log_base = Log2Base

            nested_test("powers") do
                return test_entries(
                    [1, 1000],
                    ["<sub>2</sub>0", "<sub>2</sub>2", "<sub>2</sub>4", "<sub>2</sub>6", "<sub>2</sub>8"],
                    6 .+ 12 .* [0, 2, 4, 6, 8] ./ log2(1000),
                )
            end

            # The steps are of the log values, so they may be fractional powers of 2.
            nested_test("fractions") do
                return test_entries(
                    [3, 5],
                    ["<sub>2</sub>1.6", "<sub>2</sub>1.8", "<sub>2</sub>2.0", "<sub>2</sub>2.2"],
                    6 .+ 12 .* ([1.6, 1.8, 2.0, 2.2] .- log2(3)) ./ (log2(5) - log2(3)),
                )
            end
        end
    end

    nested_test("values") do
        data_context = ValidationContext(["values_data"])
        configuration_context = ValidationContext(["axis_configuration"])
        configuration = AxisConfiguration()
        configuration.scale.log_base = Log2Base

        validate_values(data_context, nothing, configuration_context, configuration)

        data = [0.0, 1.0]

        nested_test("negative") do
            @test_throws chomp(
                """
                ArgumentError: too low values_data.([1] + axis_configuration.scale.log_regularization): 0.0
                is not above: 0
                """,
            ) validate_values(data_context, data, configuration_context, configuration)
        end

        nested_test("positive") do
            configuration.scale.log_regularization = 1
            return validate_values(data_context, data, configuration_context, configuration)
        end
    end

    nested_test("colors") do
        data_context = ValidationContext(["colors_data"])
        data = nothing
        configuration_context = ValidationContext(["colors_configuration"])
        configuration = ColorsConfiguration()
        validate_colors(data_context, data, configuration_context, configuration)

        nested_test("fixed") do
            configuration.fixed = "red"
            data = [1.0, 2.0]
            @test_throws chomp("""
                               ArgumentError: can't specify colors_data
                               for colors_configuration.fixed: red
                               """) validate_colors(data_context, data, configuration_context, configuration)
        end

        nested_test("legend") do
            configuration.show_legend = true

            nested_test("!data") do
                @test_throws chomp("""
                                   ArgumentError: must specify colors_data
                                   for colors_configuration.show_legend
                                   """) validate_colors(data_context, data, configuration_context, configuration)
            end

            nested_test("named") do
                data = ["red", "green", "blue"]
                @test_throws chomp("""
                                   ArgumentError: can't specify colors_configuration.show_legend
                                   for named colors_data
                                   """) validate_colors(data_context, data, configuration_context, configuration)
            end
        end

        nested_test("explicit") do
            data = ["red", "Oobleck"]
            @test_throws "ArgumentError: invalid colors_data[2]: Oobleck" validate_colors(
                data_context,
                data,
                configuration_context,
                configuration,
            )
        end

        nested_test("categorical") do
            nested_test("missing") do
                configuration.palette = Dict("Foo" => "red", "Bar" => "green")
                @test_throws chomp("""
                                   ArgumentError: must specify (categorical) colors_data
                                   for categorical colors_configuration.palette
                                   """) validate_colors(data_context, data, configuration_context, configuration)
            end

            nested_test("palette") do
                configuration.palette = Dict("Foo" => "red", "Bar" => "green")
                data = ["Foo", "Bar", "", "Baz"]
                mask = [true, true, false, true]
                @test_throws chomp("""
                                   ArgumentError: invalid colors_data[4]: Baz
                                   does not exist in colors_configuration.palette
                                   """) validate_colors(data_context, data, configuration_context, configuration, mask)
            end

            nested_test("continuous") do
                configuration.palette = [0 => "red", 1 => "green"]
                data = ["Foo", "Bar", "Baz"]
                @test_throws chomp("""
                                   ArgumentError: categorical colors_data
                                   specified for continuous colors_configuration.palette
                                   """) validate_colors(data_context, data, configuration_context, configuration)
            end

            nested_test("axis") do
                configuration.scale.percent = true
                data = ["Foo", "Bar"]
                @test_throws chomp("""
                                   ArgumentError: must specify numeric colors_data
                                   when using any of colors_configuration.scale.(minimum,maximum,log_base,percent)
                                   """) validate_colors(data_context, data, configuration_context, configuration)
            end
        end

        nested_test("continuous") do
            nested_test("missing") do
                configuration.palette = [0 => "red", 1 => "green"]
                @test_throws chomp("""
                                   ArgumentError: must specify (numeric) colors_data
                                   for continuous colors_configuration.palette
                                   """) validate_colors(data_context, data, configuration_context, configuration)
            end

            data = [0, 1]

            nested_test("()") do
                configuration.palette = [0 => "red", 1 => "green"]
                return validate_colors(data_context, data, configuration_context, configuration)
            end

            nested_test("palette") do
                configuration.palette = Dict("Foo" => "red", "Bar" => "green")
                @test_throws chomp("""
                                   ArgumentError: numeric colors_data
                                   specified for categorical colors_configuration.palette
                                   """) validate_colors(data_context, data, configuration_context, configuration)
            end

            nested_test("log") do
                configuration.scale.log_base = Log2Base

                @test_throws chomp(
                    """
                    ArgumentError: too low colors_data[1].(value + colors_configuration.scale.log_regularization): 0
                    is not above: 0
                    """,
                ) validate_colors(data_context, data, configuration_context, configuration)

                configuration.scale.log_regularization = 1
                return validate_colors(data_context, data, configuration_context, configuration)
            end
        end
    end
end
