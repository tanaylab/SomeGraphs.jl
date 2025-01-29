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
end
