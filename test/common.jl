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

        nested_test("grid_color") do
            figure.grid_color = "Oobleck"
            @test_throws "invalid figure.grid_color: Oobleck" validate(context, figure)
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
        end

        nested_test("log_regularization+maximum") do
            axis.log_scale = Log10Scale
            axis.log_regularization = 1
            axis.maximum = -3
            @test_throws dedent("""
                too low axis.(maximum + log_regularization): -2
                is not above: 0
            """) validate(context, axis)
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
        validate(context, band, "axis", axis)

        nested_test("log_regularization+offsets") do
            axis.log_scale = Log2Scale
            band.offset = -1
            @test_throws dedent("""
                too low root.band.offset + root.axis.log_regularization: -1
                is not above: 0
            """) validate(context, band, "axis", axis)
        end

        nested_test("line") do
            band.line.width = 0
            @test_throws dedent("""
                too low root.band.line.width: 0
                is not above: 0
            """) validate(context, band, "axis", axis)
        end
    end

    nested_test("bands") do
        axis = AxisConfiguration()
        bands = BandsConfiguration()
        context = ValidationContext(["root", "bands"])
        validate(context, bands, "axis", axis)

        nested_test("low") do
            bands.low.line.width = 0
            @test_throws dedent("""
                too low root.bands.low.line.width: 0
                is not above: 0
            """) validate(context, bands, "axis", axis)
        end

        nested_test("middle") do
            bands.middle.line.width = 0
            @test_throws dedent("""
                too low root.bands.middle.line.width: 0
                is not above: 0
            """) validate(context, bands, "axis", axis)
        end

        nested_test("high") do
            bands.high.line.width = 0
            @test_throws dedent("""
                too low root.bands.high.line.width: 0
                is not above: 0
            """) validate(context, bands, "axis", axis)
        end

        nested_test("low-middle") do
            bands.low.offset = 0
            bands.middle.offset = 0
            @test_throws dedent("""
                range low limit root.bands.low.offset: 0
                is not below high limit root.bands.middle.offset: 0
            """) validate(context, bands, "axis", axis)
        end

        nested_test("middle-high") do
            bands.middle.offset = 0
            bands.high.offset = 0
            @test_throws dedent("""
                range low limit root.bands.middle.offset: 0
                is not below high limit root.bands.high.offset: 0
            """) validate(context, bands, "axis", axis)
        end

        nested_test("low-high") do
            bands.low.offset = 0
            bands.high.offset = 0
            @test_throws dedent("""
                range low limit root.bands.low.offset: 0
                is not below high limit root.bands.high.offset: 0
            """) validate(context, bands, "axis", axis)
        end
    end

    nested_test("colors") do
        colors = ColorsConfiguration()
        context = ValidationContext(["colors"])
        validate(context, colors)

        nested_test("builtin") do
            colors.colors_palette = "Viridis"
            validate(context, colors)

            colors.colors_palette = "Viridis_r"
            validate(context, colors)

            colors.colors_palette = "Oobleck"
            @test_throws "invalid colors.colors_palette: Oobleck" validate(context, colors)
        end

        nested_test("order") do
            colors.colors_palette = [0 => "red", 1 => "green", 1 => "blue", 0 => "red"]
            @test_throws dedent("""
                pallete value colors.colors_palette[3].value: 1
                is above value colors.colors_palette[4].value: 0
            """) validate(context, colors)
        end

        nested_test("color") do
            colors.colors_palette = [0 => "red", 1 => "green", 1 => "Oobleck"]
            @test_throws "invalid colors.colors_palette[3].color: Oobleck" validate(context, colors)
        end

        nested_test("range") do
            colors.colors_palette = [0 => "red", 0 => "green", 0 => "blue"]
            @test_throws dedent("""
                range low limit colors.minimum(colors_palette[*].value): 0
                is not below high limit colors.maximum(colors_palette[*].value): 0
            """) validate(context, colors)
        end

        nested_test("minimum") do
            colors.colors_palette = [0 => "red", 1 => "green", 1 => "blue"]
            colors.color_axis.minimum = 0.5
            @test_throws dedent("""
                must not specify both: explicit continuous colors colors.colors_palette
                and explicit colors.color_axis.minimum: 0.5
            """) validate(context, colors)
        end

        nested_test("maximum") do
            colors.colors_palette = [0 => "red", 1 => "green", 1 => "blue"]
            colors.color_axis.maximum = 0.5
            @test_throws dedent("""
                must not specify both: explicit continuous colors colors.colors_palette
                and explicit colors.color_axis.maximum: 0.5
            """) validate(context, colors)
        end

        nested_test("log_regularization+cmin") do
            colors.colors_palette = [0 => "red", 1 => "green", 1 => "blue"]
            colors.color_axis.log_scale = Log10Scale
            @test_throws dedent("""
                too low colors.(colors_palette[1].value + color_axis.log_regularization): 0
                is not above: 0
            """) validate(context, colors)
        end
    end
end
