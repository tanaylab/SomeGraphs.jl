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
    nested_test("scale_axis_values") do
        values = [1, nothing]

        nested_test("default") do
            @test scale_axis_values(AxisConfiguration(), values) === values
        end

        nested_test("log10") do
            values = [1, 10, nothing]
            return test_same_values(
                scale_axis_values(AxisConfiguration(; log_scale = Log10Scale), values),
                [0, 1, nothing],
            )
        end

        nested_test("log2") do
            values = [1, 2, nothing]
            return test_same_values(
                scale_axis_values(AxisConfiguration(; log_scale = Log2Scale), values),
                [0, 1, nothing],
            )
        end

        nested_test("percent") do
            nested_test("()") do
                return test_same_values(scale_axis_values(AxisConfiguration(; percent = true), values), [100, nothing])
            end

            nested_test("log10") do
                values = [1, sqrt(10), 10, nothing]
                return test_same_values(
                    scale_axis_values(AxisConfiguration(; log_scale = Log10Scale), values),
                    [0.0, 0.5, 1.0, nothing],
                )
            end

            nested_test("log2") do
                values = [1, sqrt(2), 2, nothing]
                return test_same_values(
                    scale_axis_values(AxisConfiguration(; log_scale = Log2Scale), values),
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
            sizes_configuration.log_scale = true
            sizes_configuration.log_regularization = 1

            nested_test("()") do
                test_same_values(scale_size_values(sizes_configuration, [0, 1, 3]), [6, 12, 18])
                return nothing
            end

            nested_test("minimum") do
                sizes_configuration.minimum = 0
                test_same_values(scale_size_values(sizes_configuration, [-1, 1, 3]), [6, 12, 18])
                return nothing
            end

            nested_test("maximum") do
                sizes_configuration.maximum = 3
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

    nested_test("values") do
        data_context = ValidationContext(["values_data"])
        configuration_context = ValidationContext(["axis_configuration"])
        configuration = AxisConfiguration()
        configuration.log_scale = Log2Scale

        validate_values(data_context, nothing, configuration_context, configuration)

        data = [0.0, 1.0]

        nested_test("negative") do
            @test_throws chomp(
                """
                ArgumentError: too low values_data.([1] + axis_configuration.axis.log_regularization): 0.0
                is not above: 0
                """,
            ) validate_values(data_context, data, configuration_context, configuration)
        end

        nested_test("positive") do
            configuration.log_regularization = 1
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
                configuration.axis.percent = true
                data = ["Foo", "Bar"]
                @test_throws chomp("""
                                   ArgumentError: must specify numeric colors_data
                                   when using any of colors_configuration.axis.(minimum,maximum,log_scale,percent)
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
                configuration.axis.log_scale = Log2Scale

                @test_throws chomp(
                    """
                    ArgumentError: too low colors_data[1].(value + colors_configuration.axis.log_regularization): 0
                    is not above: 0
                    """,
                ) validate_colors(data_context, data, configuration_context, configuration)

                configuration.axis.log_regularization = 1
                return validate_colors(data_context, data, configuration_context, configuration)
            end
        end
    end
end
