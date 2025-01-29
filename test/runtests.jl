using Test

using SomeGraphs
using NestedTests

test_prefixes(ARGS)
abort_on_first_failure(true)

# NOTE: This is replicated from DataAxesFormats to avoid making it a dependency.
function dedent(string::AbstractString; indent::AbstractString = "")::String
    lines = split(string, "\n")
    while !isempty(lines) && isempty(lines[1])
        @views lines = lines[2:end]  # untested
    end
    while !isempty(lines) && isempty(lines[end])
        @views lines = lines[1:(end - 1)]
    end

    first_non_space = nothing
    for line in lines
        line_non_space = findfirst(character -> character != ' ', line)
        if first_non_space === nothing || (line_non_space !== nothing && line_non_space < first_non_space)
            first_non_space = line_non_space
        end
    end  # NOJET

    if first_non_space === nothing
        return indent * string  # untested NOJET
    else
        return join([indent * line[first_non_space:end] for line in lines], "\n")  # NOJET
    end
end

CSS_ID_REGEX = r"""id="([^"]+)"""
TRACE_REGEX = r"""trace([-_a-zA-Z0-9]+)"""
CLASS_REGEX = r"""class="([-_a-zA-Z]*[0-9][-_a-zA-Z0-9]*)"""
HTML_ID_REGEX = r"""id=([-_a-zA-Z0-9]+)"""

function normalize_ids(
    text::AbstractString,
    replace_prefix::AbstractString,
    capture_regex::Regex,
    match_prefix::AbstractString,
)::AbstractString
    seen = Dict{AbstractString, Int}()
    for id in eachmatch(capture_regex, text)
        index = get(seen, id.captures[1], nothing)
        if index === nothing
            index = length(seen) + 1
            seen[id.captures[1]] = index
        end
    end
    replacements = sort(
        ["$(match_prefix)$(id)" => "$(replace_prefix)$(index)" for (id, index) in seen];
        by = (pair) -> length(pair.first),  # UNTESTED
        rev = true,
    )
    return replace(text, replacements...)
end

function normalize_svg(svg::AbstractString)::AbstractString  # UNTESTED
    svg = normalize_ids(svg, "id-", CSS_ID_REGEX, "")
    svg = normalize_ids(svg, "class-", CLASS_REGEX, "")
    svg = normalize_ids(svg, "trace-", TRACE_REGEX, "trace")
    svg = replace(svg, " style=\"\"" => "", ">" => ">\n")
    return svg
end

function normalize_html(html::AbstractString)::AbstractString
    html = normalize_ids(html, "id-", HTML_ID_REGEX, "")
    html = replace(html, ",\"" => ",\n\"", ",{" => ",\n{", ",[" => ",\n[")
    return html
end

struct ResultFile
    path::AbstractString
    content::AbstractString
end

function Base.show(io::IO, result_file::ResultFile)::Nothing  # untested
    print(io, result_file.path)
    return nothing
end

function Base.:(==)(left_file::ResultFile, right_file::ResultFile)::Bool
    return left_file.content == right_file.content
end

function test_svg(graph::Graph, path::AbstractString)::Nothing  # UNTESTED
    save_graph(graph, "actual.svg")
    actual_svg = open("actual.svg", "r") do file
        return read(file, String)
    end
    rm("actual.svg")
    actual_svg = normalize_svg(actual_svg)

    actual_path = "actual/" * path
    open(actual_path, "w") do file
        write(file, actual_svg)
        return nothing
    end
    actual_result = ResultFile("test/" * actual_path, actual_svg)

    expected_path = "expected/" * path
    expected_svg = open(expected_path, "r") do file
        return read(file, String)
    end
    expected_result = ResultFile("test/" * expected_path, expected_svg)

    @test actual_result == expected_result
    return nothing
end

function test_html(graph::Graph, path::AbstractString)::Nothing
    save_graph(graph, "actual.html")
    actual_html = open("actual.html", "r") do file
        return read(file, String)
    end
    rm("actual.html")
    actual_html = normalize_html(actual_html)

    actual_path = "actual/" * path
    open(actual_path, "w") do file
        write(file, actual_html)
        return nothing
    end

    actual_result = ResultFile("test/" * actual_path, actual_html)

    expected_path = "expected/" * path
    expected_html = open(expected_path, "r") do file
        return read(file, String)
    end
    expected_result = ResultFile("test/" * expected_path, expected_html)

    @test actual_result == expected_result
    return nothing
end

function test_legend(set_title::Function, graph::Graph, path_prefix::AbstractString)::Nothing
    nested_test("()") do
        test_html(graph, path_prefix * ".legend.html")
        return nothing
    end

    nested_test("title") do
        set_title()
        test_html(graph, path_prefix * ".legend.title.html")
        return nothing
    end

    return nothing
end

mkpath("actual")

include("validations.jl")
include("common.jl")
include("distributions.jl")
