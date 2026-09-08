nested_test("sources") do
    nested_test("hovers") do
        nested_test("vector") do
            entities = EntitiesData()
            add_hovers!(entities, ["a", "b"])
            @test entities.hovers == ["a", "b"]
            add_hovers!(entities, ["1", "2"]; title = "N")
            @test entities.hovers == ["a<br>N: 1", "b<br>N: 2"]
            @test_throws chomp("""
                               ArgumentError: invalid size of added hovers: (3,)
                               is different from size of existing hovers: (2,)
                               """) add_hovers!(entities, ["x", "y", "z"])
        end

        nested_test("matrix") do
            entities = MatrixEntitiesData()
            add_hovers!(entities, ["a" "b"; "c" "d"]; title = "L")
            @test entities.hovers == ["L: a" "L: b"; "L: c" "L: d"]
            add_hovers!(entities, ["1" "2"; "3" "4"])
            @test entities.hovers == ["L: a<br>1" "L: b<br>2"; "L: c<br>3" "L: d<br>4"]
        end
    end
end
