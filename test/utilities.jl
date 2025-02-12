function test_same_values(
    actual_values::Union{AbstractVector{<:Real}, AbstractVector{<:Maybe{Real}}},
    expected_values::Union{AbstractVector{<:Real}, AbstractVector{<:Maybe{Real}}},
)::Nothing
    @test length(actual_values) == length(expected_values)
    for (actual_value, expected_value) in zip(actual_values, expected_values)
        @test (actual_value === nothing) == (expected_value === nothing)
        if actual_value !== nothing
            @test abs(actual_value - expected_value) < 1e-7
        end
    end
end

nested_test("utilities") do
    nested_test("axis_ticks_prefix") do
        nested_test("default") do
            @test axis_ticks_prefix(AxisConfiguration()) == nothing
        end

        nested_test("log10") do
            @test axis_ticks_prefix(AxisConfiguration(; log_scale = Log10Scale)) == "<sub>10</sub>"
        end

        nested_test("log2") do
            @test axis_ticks_prefix(AxisConfiguration(; log_scale = Log2Scale)) == "<sub>2</sub>"
        end
    end

    nested_test("axis_ticks_suffix") do
        nested_test("default") do
            @test axis_ticks_suffix(AxisConfiguration()) == nothing
        end

        nested_test("percent") do
            @test axis_ticks_suffix(AxisConfiguration(; percent = true)) == "<sub>%</sub>"
        end
    end

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
        sizes = [1, 2, 3]
        axis_configuration = AxisConfiguration()

        nested_test("default") do
            @test scale_size_values(axis_configuration, SizeConfiguration(), sizes) == [2, 6, 10]
        end

        nested_test("smallest") do
            return test_same_values(
                scale_size_values(axis_configuration, SizeConfiguration(; smallest = 0), sizes),
                [0, 4, 8],
            )
        end

        nested_test("span") do
            return test_same_values(
                scale_size_values(axis_configuration, SizeConfiguration(; span = 2), sizes),
                [2, 3, 4],
            )
        end

        nested_test("both") do
            return test_same_values(
                scale_size_values(axis_configuration, SizeConfiguration(; smallest = 10, span = 20), sizes),
                [10, 20, 30],
            )
        end

        nested_test("same") do
            sizes = [1]
            return test_same_values(scale_size_values(axis_configuration, SizeConfiguration(), sizes), [2])
        end

        nested_test("nothing") do
            sizes = nothing
            @test scale_size_values(axis_configuration, SizeConfiguration(; smallest = 0), sizes) === sizes
        end

        nested_test("log2") do
            sizes = [1, 2, 4]
            axis_configuration = AxisConfiguration(; log_scale = Log2Scale)

            nested_test("()") do
                return test_same_values(scale_size_values(axis_configuration, SizeConfiguration(), sizes), [2, 6, 10])
            end

            nested_test("smallest") do
                return test_same_values(
                    scale_size_values(axis_configuration, SizeConfiguration(; smallest = 0), sizes),
                    [0, 4, 8],
                )
            end

            nested_test("largest") do
                return test_same_values(
                    scale_size_values(axis_configuration, SizeConfiguration(; span = 2), sizes),
                    [2, 3, 4],
                )
            end

            nested_test("both") do
                return test_same_values(
                    scale_size_values(axis_configuration, SizeConfiguration(; smallest = 10, span = 20), sizes),
                    [10, 20, 30],
                )
            end
        end
    end

    nested_test("colors") do
        data_context = ValidationContext(["colors_data"])
        data = nothing
        configuration_context = ValidationContext(["colors_configuration"])
        configuration = ColorsConfiguration()
        validate_colors(data_context, data, configuration_context, configuration)

        nested_test("legend") do
            configuration.show_legend = true
            @test_throws dedent("""
                did not specify colors_data
                for colors_configuration.show_legend
            """) validate_colors(data_context, data, configuration_context, configuration)
        end

        nested_test("categorical") do
            nested_test("pallete") do
                configuration.colors_palette = Dict("Foo" => "red", "Bar" => "green")
                data = ["Foo", "Bar", "", "Baz"]
                @test_throws dedent("""
                    invalid colors_data[4]: Baz
                    does not exist in colors_configuration.colors_palette
                """) validate_colors(data_context, data, configuration_context, configuration)
            end

            nested_test("continuous") do
                configuration.colors_palette = [0 => "red", 1 => "green"]
                data = ["Foo", "Bar", "", "Baz"]
                @test_throws dedent("""
                    categorical colors colors_data
                    specified for a continuous colors_configuration.colors_palette
                """) validate_colors(data_context, data, configuration_context, configuration)
            end

            nested_test("axis") do
                configuration.color_axis.percent = true
                data = ["Foo", "Bar", ""]
                @test_throws dedent("""
                    categorical colors colors_data
                    specified for a continuous colors_configuration.color_axis
                    (specified some of minimum/maximum/log_scale/percent)
                """) validate_colors(data_context, data, configuration_context, configuration)
            end
        end

        nested_test("continuous") do
            data = [0, 1]

            nested_test("pallete") do
                configuration.colors_palette = Dict("Foo" => "red", "Bar" => "green")
                @test_throws dedent("""
                    continuous colors colors_data
                    specified for a categorical colors_configuration.colors_palette
                """) validate_colors(data_context, data, configuration_context, configuration)
            end

            nested_test("log") do
                configuration.color_axis.log_scale = Log2Scale

                @test_throws dedent("""
                    too low colors_data[1].(value + colors_configuration.color_axis.log_regularization): 0
                    is not above: 0
                """) validate_colors(data_context, data, configuration_context, configuration)

                configuration.color_axis.log_regularization = 1
                return validate_colors(data_context, data, configuration_context, configuration)
            end
        end
    end
end
