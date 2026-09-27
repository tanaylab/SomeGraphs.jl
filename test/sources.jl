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
                # The rows and columns entities belong to their sides, so the entries are all that is visited.
                @test visited_names(visit_data_sinks, entries_matrix_fields(graph)) ==
                      [:MatrixValuesData, :MatrixEntitiesData]
                return nothing
            end

            nested_test("side") do
                @test visited_names(visit_data_sinks, columns_side(graph)) == [:VectorEntitiesData, :ArrangementData]
                return nothing
            end

            nested_test("side+groups") do
                # The groups view shares the entities of the side, which is visited once.
                @test visited_names(visit_data_sinks, (columns_side(graph), groups)) ==
                      [:VectorEntitiesData, :ArrangementData, :VectorValuesData]
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

    nested_test("puts") do
        points = points_graph()
        heatmap = heatmap_graph()
        heatmap.data.entries.matrix = [1.0 2.0; 3.0 4.0]

        # A value per entry goes to the values of a role and to a hover line, and only the values half is a vector.
        nested_test("halves") do
            put_vector_data!(points.data.x, Float32[1.0, 2.0, 3.0, 4.0]; title = "ignored")
            @test points.data.x.vector == Float32[1.0, 2.0, 3.0, 4.0]
            @test points.data.points.entities.hovers === nothing

            put_vector_data!(points_entities(points), Float32[1.0, 2.0, 3.0, 4.0]; title = "X")
            @test points.data.points.entities.hovers[1] == "X: 1.0"
            return nothing
        end

        nested_test("strings") do
            put_vector_data!(x_axis_vector_fields(points), ["a", "b"]; title = "S")
            @test points.data.x.vector == ["a", "b"]
            @test points.data.points.entities.hovers == ["S: a", "S: b"]
            return nothing
        end

        nested_test("names") do
            put_vector_names_data!(x_axis_vector_fields(points), ["a", "b"])
            @test points.data.points.entities.names == ["a", "b"]
            @test points.data.x.vector === nothing
            return nothing
        end

        nested_test("mask") do
            put_vector_mask_data!(x_axis_vector_fields(points), [true, false, true, false])
            @test points.data.points.entities.mask == [true, false, true, false]
            return nothing
        end

        nested_test("order") do
            put_vector_order_data!(x_axis_vector_fields(points), [2, 1, 4, 3])
            @test points.data.points.entities.order == [2, 1, 4, 3]
            return nothing
        end

        nested_test("matrix") do
            put_matrix_data!(entries_matrix_fields(heatmap), [5.0 6.0; 7.0 8.0]; title = "M")
            @test heatmap.data.entries.matrix == [5.0 6.0; 7.0 8.0]
            @test heatmap.data.cells.hovers == ["M: 5.0" "M: 6.0"; "M: 7.0" "M: 8.0"]
            return nothing
        end

        # A side of a heatmap reaches its entities and its arrangement; the puts write the entities and pass over the
        # arrangement.
        nested_test("side") do
            side = rows_side(heatmap)
            put_vector_names_data!(side, ["A", "B"])
            put_vector_mask_data!(side, [true, false])
            put_vector_order_data!(side, [2, 1])
            put_vector_data!(side, Float32[1.0, 2.0]; title = "X")
            @test heatmap.data.rows.entities.names == ["A", "B"]
            @test heatmap.data.rows.entities.mask == [true, false]
            @test heatmap.data.rows.entities.order == [2, 1]
            @test heatmap.data.rows.entities.hovers == ["X: 1.0", "X: 2.0"]
            @test heatmap.data.rows.arrangement.groups.vector === nothing
            @test_throws "can't name the rows and columns of a vector sink" put_matrix_names_data!(side, ["A"], ["B"])
            return nothing
        end

        # A configuration leaf is walked and nothing is found.
        nested_test("configuration") do
            put_vector_data!(points.configuration.x_axis, [1.0, 2.0])
            put_matrix_names_data!(heatmap.configuration.entries.colors, ["a", "b"], ["c", "d"])
            @test points.data.x.vector === nothing
            @test heatmap.data.rows.entities.names === nothing
            return nothing
        end

        # A leaf a put says nothing about (the other shape) is an error.
        nested_test("mismatched") do
            @test_throws MethodError put_vector_data!(entries_matrix_fields(heatmap), [1.0, 2.0])
            @test_throws MethodError put_vector_names_data!(entries_matrix_fields(heatmap), ["a", "b"])
            @test_throws MethodError put_matrix_data!(x_axis_vector_fields(points), [1.0 2.0; 3.0 4.0])
            @test_throws "can't name the rows and columns of a vector sink" put_matrix_names_data!(
                x_axis_vector_fields(points),
                ["a", "b"],
                ["c", "d"],
            )
            return nothing
        end

        nested_test("matrix_names") do
            put_matrix_names_data!(entries_matrix_fields(heatmap).data, ["r1", "r2"], ["c1", "c2"])
            @test heatmap.data.rows.entities.names == ["r1", "r2"]
            @test heatmap.data.columns.entities.names == ["c1", "c2"]

            other = heatmap_graph()
            put_matrix_names_data!(
                (entries_matrix_fields(other), heatmap.configuration.entries.colors),
                ["r3", "r4"],
                ["c3", "c4"],
            )
            @test other.data.rows.entities.names == ["r3", "r4"]
            @test other.data.columns.entities.names == ["c3", "c4"]
            return nothing
        end
    end
end
