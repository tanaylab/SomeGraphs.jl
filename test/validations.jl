nested_test("validations") do
    nested_test("location") do
        nested_test("member") do
            @test location(ValidationContext(["foo"])) == "foo"
        end

        nested_test("vector") do
            @test location(ValidationContext(["foo", 1])) == "foo[1]"
        end

        nested_test("matrix") do
            @test location(ValidationContext(["foo", 1, 2])) == "foo[1, 2]"
        end

        nested_test("path") do
            @test location(ValidationContext(["foo", "bar", 1, 2, "baz"])) == "foo.bar[1, 2].baz"
        end
    end

    nested_test("bounds") do
        context = ValidationContext(["foo"])

        nested_test("at_least") do
            validate_is_at_least(context, 0, 0)
            @test_throws dedent("""
                too low foo: -1
                is not at least: 0
            """) validate_is_at_least(context, -1, 0)
        end

        nested_test("above") do
            validate_is_above(context, 1, 0)
            @test_throws dedent("""
                too low foo: 0
                is not above: 0
            """) validate_is_above(context, 0, 0)
        end

        nested_test("at_most") do
            validate_is_at_most(context, 1, 1)
            @test_throws dedent("""
                too high foo: 2
                is not at most: 1
            """) validate_is_at_most(context, 2, 1)
        end

        nested_test("below") do
            validate_is_below(context, 0, 1)
            @test_throws dedent("""
                too high foo: 1
                is not below: 1
            """) validate_is_below(context, 1, 1)
        end
    end

    nested_test("color") do
        context = ValidationContext(["foo_color"])

        validate_is_color(context, "red")
        validate_is_color(context, "#FF0000")
        validate_is_color(context, "#FF0000FF")
        @test_throws "invalid foo_color: Oobleck" validate_is_color(context, "Oobleck")
    end

    nested_test("vector") do
        context = ValidationContext(["foo"])

        vector = [0, 1]

        nested_test("length") do
            validate_vector_length(context, vector, 2)
            @test_throws dedent("""
                invalid length of foo: 2
                is different from expected length: 1
            """) validate_vector_length(context, vector, 1)
        end

        nested_test("entries") do
            validate_vector_entries(context, vector) do index, value
                return validate_is_at_most(context, value, 1)
            end

            @test_throws dedent("""
                too high foo[2]: 1
                is not below: 1
            """) begin
                validate_vector_entries(context, vector) do index, value
                    return validate_is_below(context, value, 1)
                end
            end
        end
    end

    nested_test("matrix") do
        context = ValidationContext(["foo"])

        matrix = [5 4 3; 2 1 0]

        nested_test("size") do
            validate_matrix_size(context, matrix, (2, 3))
            @test_throws dedent("""
                invalid size of foo: (2, 3)
                is different from expected size: (3, 2)
            """) validate_matrix_size(context, matrix, (3, 2))
        end

        nested_test("entries") do
            validate_matrix_entries(context, matrix) do row, column, value
                return validate_is_at_least(context, value, 0)
            end

            @test_throws dedent("""
                too low foo[2, 3]: 0
                is not above: 0
            """) begin
                validate_matrix_entries(context, matrix) do row, column, value
                    return validate_is_above(context, value, 0)
                end
            end
        end
    end
end
