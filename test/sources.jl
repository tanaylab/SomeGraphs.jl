nested_test("sources") do
    nested_test("hovers") do
        nested_test("vector") do
            entities = VectorEntitiesData()
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

    nested_test("sinks") do
        graph = heatmap_graph()
        graph.data.entries.matrix = [1.0 2.0; 3.0 4.0]
        annotation = columns_annotations_colors_vector_fields(graph, add_columns_annotation!(graph))
        groups = columns_groups_vector_data_fields(graph)

        # The name of a type, for comparing what was visited.
        function visited_names(visit::Function, sinks::Sinks)::Vector{Symbol}
            names = Symbol[]
            visit(sinks) do sink
                push!(names, typeof(sink).name.name)
                return nothing
            end
            return names
        end

        nested_test("data") do
            nested_test("()") do
                @test visited_names(visit_data_sinks, annotation) == [:VectorValuesData, :VectorEntitiesData]
                return nothing
            end

            nested_test("shared") do
                # Both views are of the columns, so they share one entities, which is visited once.
                @test visited_names(visit_data_sinks, (annotation, groups)) ==
                      [:VectorValuesData, :VectorEntitiesData, :VectorValuesData]
                return nothing
            end

            nested_test("repeated") do
                @test visited_names(visit_data_sinks, [annotation, annotation]) ==
                      [:VectorValuesData, :VectorEntitiesData]
                return nothing
            end

            nested_test("configuration") do
                # A configuration struct holds no data, so it is not visited at all.
                @test visited_names(visit_data_sinks, graph.configuration.entries.colors) == Symbol[]
                return nothing
            end

            nested_test("matrix") do
                # The rows and columns entities belong to their axes, so the entries are all that is visited.
                @test visited_names(visit_data_sinks, entries_matrix_fields(graph)) ==
                      [:MatrixValuesData, :MatrixEntitiesData]
                return nothing
            end
        end

        nested_test("configuration") do
            nested_test("()") do
                @test visited_names(visit_configuration_sinks, annotation) == [:ColorsConfiguration]
                return nothing
            end

            nested_test("mixed") do
                # The groups have no configuration, so only the annotation's is visited.
                @test visited_names(visit_configuration_sinks, [annotation, groups]) == [:ColorsConfiguration]
                return nothing
            end

            nested_test("data") do
                @test visited_names(visit_configuration_sinks, graph.data.columns.entities) == Symbol[]
                return nothing
            end

            # A role which isn't colors reaches a different configuration, through a different view.
            nested_test("axis") do
                points = points_graph()
                @test visited_names(visit_configuration_sinks, x_axis_vector_fields(points)) == [:AxisConfiguration]
                return nothing
            end

            nested_test("sizes") do
                points = points_graph()
                @test visited_names(visit_configuration_sinks, points_sizes_vector_fields(points)) ==
                      [:SizesConfiguration]
                return nothing
            end
        end

        nested_test("invalid") do
            @test_throws "not a graph data or configuration struct: String" visit_data_sinks(identity, ["not a sink"])
        end

        nested_test("compound") do
            # A source written the intended way: a compound method which walks, a method per struct it writes, and an
            # explicit no-op for the structs it ignores. A struct it says nothing about is an error, not a loop.
            function source!(
                sinks::Union{AnyContainer, ConfigurationLeaf, Tuple, AbstractVector},
                values::AbstractVector{<:Real},
            )::Nothing
                visit_data_sinks(sinks) do sink
                    return source!(sink, values)
                end
                return nothing
            end

            function source!(values_data::VectorValuesData, values::AbstractVector{<:Real})::Nothing
                values_data.vector = values
                return nothing
            end

            function source!(::VectorEntitiesData, ::AbstractVector{<:Real})::Nothing
                return nothing
            end

            points = points_graph()
            source!(x_axis_vector_fields(points), [1.0, 2.0])
            @test points.data.x.vector == [1.0, 2.0]

            source!((points.data.y, points.data.points.entities), [3.0, 4.0])
            @test points.data.y.vector == [3.0, 4.0]

            # A configuration leaf contains no data, so it is walked and nothing is found.
            source!(points.configuration.x_axis, [5.0, 6.0])
            @test points.data.x.vector == [1.0, 2.0]

            @test_throws MethodError source!(graph.data.entries, [5.0, 6.0])
            @test_throws MethodError source!(entries_matrix_fields(graph), [5.0, 6.0])
        end
    end
end
