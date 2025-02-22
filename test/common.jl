function test_same_palette(actual_palette::ContinuousColors, expected_palette::ContinuousColors)::Nothing
    @test length(actual_palette) == length(expected_palette)
    for ((actual_value, actual_color), (expected_value, expected_color)) in zip(actual_palette, expected_palette)
        @test abs(actual_value - expected_value) < 1e-7
        @test actual_color == expected_color
    end
end

nested_test("common") do
    nested_test("margins") do
        margins = MarginsConfiguration()
        context = ValidationContext(["margins"])
        validate(context, margins)

        nested_test("left") do
            margins.left = -1
            @test_throws dedent("""
                too low margins.left: -1
                is not at least: 0
            """) validate(context, margins)
        end

        nested_test("right") do
            margins.right = -1
            @test_throws dedent("""
                too low margins.right: -1
                is not at least: 0
            """) validate(context, margins)
        end

        nested_test("top") do
            margins.top = -1
            @test_throws dedent("""
                too low margins.top: -1
                is not at least: 0
            """) validate(context, margins)
        end

        nested_test("bottom") do
            margins.bottom = -1
            @test_throws dedent("""
                too low margins.bottom: -1
                is not at least: 0
            """) validate(context, margins)
        end
    end

    nested_test("figure") do
        figure = FigureConfiguration()
        context = ValidationContext(["figure"])
        validate(context, figure)

        nested_test("margins") do
            figure.margins.left = -1
            @test_throws dedent("""
                too low figure.margins.left: -1
                is not at least: 0
            """) validate(context, figure)
        end

        nested_test("width") do
            figure.width = 0
            @test_throws dedent("""
                too low figure.width: 0
                is not above: 0
            """) validate(context, figure)
        end

        nested_test("height") do
            figure.height = 0
            @test_throws dedent("""
                too low figure.height: 0
                is not above: 0
            """) validate(context, figure)
        end
    end

    nested_test("size") do
        size = SizeConfiguration()
        context = ValidationContext(["size"])
        validate(context, size)

        nested_test("smallest") do
            size.smallest = 0
            @test_throws dedent("""
                too low size.smallest: 0
                is not above: 0
            """) validate(context, size)
        end

        nested_test("span") do
            size.span = 0
            @test_throws dedent("""
                too low size.span: 0
                is not above: 0
            """) validate(context, size)
        end
    end

    nested_test("axis") do
        axis = AxisConfiguration()
        context = ValidationContext(["axis"])
        validate(context, axis)

        nested_test("range") do
            axis.minimum = 0
            axis.maximum = 0
            @test_throws dedent("""
                range low limit axis.minimum: 0
                is not below high limit axis.maximum: 0
            """) validate(context, axis)
        end

        nested_test("~log_regularization") do
            axis.log_regularization = 1
            @test_throws "non-zero non-log axis.log_regularization: 1" validate(context, axis)
        end

        nested_test("log_regularization") do
            axis.log_scale = Log10Scale
            axis.log_regularization = -1
            @test_throws dedent("""
                too low axis.log_regularization: -1
                is not at least: 0
            """) validate(context, axis)
        end

        nested_test("log_regularization+minimum") do
            axis.log_scale = Log10Scale
            axis.log_regularization = 1

            axis.minimum = -3
            @test_throws dedent("""
                too low axis.(minimum + log_regularization): -2
                is not above: 0
            """) validate(context, axis)

            axis.minimum = 2
            return validate(context, axis)
        end

        nested_test("log_regularization+maximum") do
            axis.log_scale = Log10Scale
            axis.log_regularization = 1

            axis.maximum = -3
            @test_throws dedent("""
                too low axis.(maximum + log_regularization): -2
                is not above: 0
            """) validate(context, axis)

            axis.maximum = 2
            return validate(context, axis)
        end
    end

    nested_test("line") do
        line = LineConfiguration()
        context = ValidationContext(["line"])
        validate(context, line)

        nested_test("width") do
            line.width = 0
            @test_throws dedent("""
                too low line.width: 0
                is not above: 0
            """) validate(context, line)
        end

        nested_test("color") do
            line.color = "Oobleck"
            @test_throws "invalid line.color: Oobleck" validate(context, line)
        end
    end

    nested_test("band") do
        axis = AxisConfiguration()
        band = BandConfiguration()
        context = ValidationContext(["root", "band"])
        validate(context, band, axis)

        nested_test("offsets") do
            axis.log_scale = Log2Scale
            band.offset = -1
            @test_throws dedent("""
                too low root.band.offset: -1
                is not above: 0
            """) validate(context, band, axis)
        end

        nested_test("line") do
            band.line.width = 0
            @test_throws dedent("""
                too low root.band.line.width: 0
                is not above: 0
            """) validate(context, band, axis)
        end
    end

    nested_test("bands") do
        axis = AxisConfiguration()
        bands = BandsConfiguration()
        context = ValidationContext(["root", "bands"])
        validate(context, bands, axis)

        nested_test("low") do
            bands.low.line.width = 0
            @test_throws dedent("""
                too low root.bands.low.line.width: 0
                is not above: 0
            """) validate(context, bands, axis)
        end

        nested_test("fill") do
            bands.middle.line.is_filled = true

            nested_test("!high") do
                bands.low.offset = 0
                @test_throws (
                    "graph.configuration.bands.middle.line.is_filled " *
                    "requires graph.configuration.bands.high.offset " *
                    "or graph.data.bands.high_offset"
                ) SomeGraphs.Utilities.validate_graph_bands("bands", bands, BandsData(), AxisConfiguration())
            end

            nested_test("!low") do
                bands.high.offset = 0
                @test_throws (
                    "graph.configuration.bands.middle.line.is_filled " *
                    "requires graph.configuration.bands.low.offset " *
                    "or graph.data.bands.low_offset"
                ) SomeGraphs.Utilities.validate_graph_bands("bands", bands, BandsData(), AxisConfiguration())
            end
        end

        nested_test("middle") do
            bands.middle.line.width = 0
            @test_throws dedent("""
                too low root.bands.middle.line.width: 0
                is not above: 0
            """) validate(context, bands, axis)
        end

        nested_test("high") do
            bands.high.line.width = 0
            @test_throws dedent("""
                too low root.bands.high.line.width: 0
                is not above: 0
            """) validate(context, bands, axis)
        end

        nested_test("low-middle") do
            bands.low.offset = 0
            bands.middle.offset = 0
            @test_throws dedent("""
                range low limit root.bands.low.offset: 0
                is not below high limit root.bands.middle.offset: 0
            """) validate(context, bands, axis)
        end

        nested_test("middle-high") do
            bands.middle.offset = 0
            bands.high.offset = 0
            @test_throws dedent("""
                range low limit root.bands.middle.offset: 0
                is not below high limit root.bands.high.offset: 0
            """) validate(context, bands, axis)
        end

        nested_test("low-high") do
            bands.low.offset = 0
            bands.high.offset = 0
            @test_throws dedent("""
                range low limit root.bands.low.offset: 0
                is not below high limit root.bands.high.offset: 0
            """) validate(context, bands, axis)
        end
    end

    nested_test("palette") do
        palette = [(0, "red"), (0.5, "green"), (1, "blue")]

        nested_test("r") do
            return test_same_palette(
                SomeGraphs.Common.reverse_color_scale(palette),
                [(0, "blue"), (0.5, "green"), (1, "red")],
            )
        end

        nested_test("z") do
            return test_same_palette(
                SomeGraphs.Common.zero_color_scale(palette, 0.2, 0.4),
                [(0.0, "white"), (0.2 - 1e-6, "white"), (0.2, "#7C7300"), (1 / 3, "green"), (1.0, "blue")],
            )
        end

        nested_test("c") do
            return test_same_palette(
                SomeGraphs.Common.center_color_scale(palette, 0.2, 0.4),
                [
                    (0, "red"),
                    (0.4, "#AA6500"),
                    (0.4 + 1e-6, "white"),
                    (0.6 - 1e-6, "white"),
                    (0.6, "#0065AA"),
                    (1, "blue"),
                ],
            )
        end

        nested_test("o") do
            return test_same_palette(
                SomeGraphs.Common.overflow_color_scale(palette, 0.2, "magenta"),
                [(0, "red"), (0.4, "green"), (0.8, "blue"), (0.8 + 1e-6, "magenta"), (1, "magenta")],
            )
        end

        nested_test("u") do
            return test_same_palette(
                SomeGraphs.Common.underflow_color_scale(palette, 0.2, "magenta"),
                [(0, "magenta"), (0.2 - 1e-6, "magenta"), (0.2, "red"), (0.6, "green"), (1, "blue")],
            )
        end
    end

    nested_test("colors") do
        colors = ColorsConfiguration()
        context = ValidationContext(["colors"])
        validate(context, colors)

        nested_test("fixed") do
            nested_test("()") do
                colors.fixed = "red"
                return validate(context, colors)
            end

            nested_test("invalid") do
                colors.fixed = "Oobleck"
                @test_throws "invalid colors.fixed: Oobleck" validate(context, colors)
            end

            nested_test("legend") do
                colors.fixed = "red"
                colors.show_legend = true
                @test_throws dedent("""
                    can't specify both colors.fixed
                    and colors.show_legend
                """) validate(context, colors)
            end

            nested_test("precent") do
                colors.fixed = "red"
                colors.axis.percent = true
                @test_throws dedent("""
                    can't specify both colors.fixed
                    and any of colors.axis.(minimum,maximum,log_scale,percent)
                """) validate(context, colors)
            end

            nested_test("palette") do
                colors.fixed = "red"
                colors.palette = "Viridis"
                @test_throws dedent("""
                    can't specify both colors.fixed
                    and colors.palette
                """) validate(context, colors)
            end
        end

        nested_test("builtin") do
            colors.palette = "Viridis"
            validate(context, colors)

            colors.palette = "Oobleck_r"
            @test_throws "invalid colors.palette: Oobleck" validate(context, colors)
        end

        nested_test("cached") do
            nested_test("()") do
                colors.palette = "Reds"
                validate(context, colors)
                return test_same_palette(
                    SomeGraphs.Common.CACHED_COLOR_SCALES["Reds"],
                    [
                        0 => "rgb(220,220,220)",
                        1 / 3 => "rgb(245,195,157)",
                        2 / 3 => "rgb(245,160,105)",
                        1 => "rgb(178,10,28)",
                    ],
                )
            end

            nested_test("r") do
                colors.palette = "Reds_r"
                validate(context, colors)
                return test_same_palette(
                    SomeGraphs.Common.CACHED_COLOR_SCALES["Reds_r"],
                    [
                        (0, "rgb(178,10,28)"),
                        (1 / 3, "rgb(245,160,105)"),
                        (2 / 3, "rgb(245,195,157)"),
                        (1, "rgb(220,220,220)"),
                    ],
                )
            end

            nested_test("z") do
                colors.palette = "Reds_z:0.2:0.4"
                validate(context, colors)
                return test_same_palette(
                    SomeGraphs.Common.CACHED_COLOR_SCALES["Reds_z:0.2:0.4"],
                    [
                        (0, "white"),
                        (0.2 - 1e-6, "white"),
                        (0.2, "#F5BD94"),
                        (5 / 9, "rgb(245,160,105)"),
                        (1, "rgb(178,10,28)"),
                    ],
                )
            end

            nested_test("c") do
                colors.palette = "RdBu_r_c:0.2:0.4"
                validate(context, colors)
                return test_same_palette(
                    SomeGraphs.Common.CACHED_COLOR_SCALES["RdBu_r_c:0.2:0.4"],
                    [
                        (0, "rgb(178,10,28)"),
                        (0.8 / 3, "rgb(230,145,90)"),
                        (0.4, "#E19E71"),
                        (0.4 + 1e-6, "white"),
                        (0.6 - 1e-6, "white"),
                        (0.6, "#9BA6DD"),
                        (2.2 / 3, "rgb(106,137,247)"),
                        (1, "rgb(5,10,172)"),
                    ],
                )
            end

            nested_test("o") do
                colors.palette = "Reds_o:0.2:magenta"
                validate(context, colors)
                return test_same_palette(
                    SomeGraphs.Common.CACHED_COLOR_SCALES["Reds_o:0.2:magenta"],
                    [
                        (0, "rgb(220,220,220)"),
                        (0.8 / 3, "rgb(245,195,157)"),
                        (1.6 / 3, "rgb(245,160,105)"),
                        (0.8, "rgb(178,10,28)"),
                        (0.8 + 1e-6, "magenta"),
                        (1, "magenta"),
                    ],
                )
            end

            nested_test("u") do
                colors.palette = "Reds_u:0.2:magenta"
                validate(context, colors)
                return test_same_palette(
                    SomeGraphs.Common.CACHED_COLOR_SCALES["Reds_u:0.2:magenta"],
                    [
                        (0, "magenta"),
                        (0.2 - 1e-6, "magenta"),
                        (0.2, "rgb(220,220,220)"),
                        (1.4 / 3, "rgb(245,195,157)"),
                        (2.2 / 3, "rgb(245,160,105)"),
                        (1.0, "rgb(178,10,28)"),
                    ],
                )
            end

            nested_test("invalid") do
                colors.palette = "Reds_z:0.2:magenta"
                @test_throws "invalid colors.palette: Reds_z:0.2:magenta" validate(context, colors)
            end
        end

        nested_test("order") do
            colors.palette = [0 => "red", 1 => "green", 1 => "blue", 0 => "red"]
            @test_throws dedent("""
                palette value colors.palette[3].value: 1
                is above value colors.palette[4].value: 0
            """) validate(context, colors)
        end

        nested_test("color") do
            colors.palette = [0 => "red", 1 => "green", 1 => "Oobleck"]
            @test_throws "invalid colors.palette[3].color: Oobleck" validate(context, colors)
        end

        nested_test("range") do
            colors.palette = [0 => "red", 0 => "green", 0 => "blue"]
            @test_throws dedent("""
                range low limit colors.palette[1].value: 0
                is not below high limit colors.palette[end].value: 0
            """) validate(context, colors)
        end

        nested_test("log_regularization+cmin") do
            colors.palette = [0 => "red", 1 => "green", 1 => "blue"]
            colors.axis.log_scale = Log10Scale
            @test_throws dedent("""
                too low colors.(palette[1].value + axis.log_regularization): 0
                is not above: 0
            """) validate(context, colors)

            colors.axis.log_regularization = 1
            return validate(context, colors)
        end

        nested_test("categorical") do
            nested_test("()") do
                colors.palette = Dict(["foo" => "red", "bar" => "green"])
                return validate(context, colors)
            end

            nested_test("empty") do
                colors.palette = Dict{String, String}()
                @test_throws "empty dict colors.palette" validate(context, colors)
            end

            nested_test("invalid") do
                colors.palette = Dict(["baz" => "Oobleck"])
                @test_throws "invalid colors.palette[baz].color: Oobleck" validate(context, colors)
            end

            nested_test("percent") do
                colors.palette = Dict(["foo" => "red", "bar" => "green"])
                colors.axis.percent = true
                @test_throws dedent("""
                    can't specify both categorical colors.palette
                    and any of colors.axis.(minimum,maximum,log_scale,percent)
                """) validate(context, colors)
            end
        end
    end
end
