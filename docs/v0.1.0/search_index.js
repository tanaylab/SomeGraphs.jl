var documenterSearchIndex = {"docs":
[{"location":"validations.html#Validations","page":"Validations","title":"Validations","text":"","category":"section"},{"location":"validations.html","page":"Validations","title":"Validations","text":"SomeGraphs.Validations\nSomeGraphs.Validations.ValidationContext\nSomeGraphs.Validations.location","category":"page"},{"location":"validations.html#SomeGraphs.Validations","page":"Validations","title":"SomeGraphs.Validations","text":"Validate graph data.\n\nRendering graphs requires two objects: data and configuration. Both objects need to be internally consistent, as does their combination. This is especially relevant for the graph configuration. When creating UI for filling in these objects, we can do limited validation of each field on its own based on its type (e.g. (e.g., ensure that a \"color\" field contains a valid color name). Some restrictions, however, are not easily deduced from the field type, or relate different fields to each other. Replicating all the restrictions in the UI is tedious and error prone.\n\nWe therefore provide a framework here for validating the objects and generate a hopefully informative error message if anything is wrong. This can be used by the UI to validate the data without worrying about the details.\n\n\n\n\n\n","category":"module"},{"location":"validations.html#SomeGraphs.Validations.ValidationContext","page":"Validations","title":"SomeGraphs.Validations.ValidationContext","text":"A context (path of field names and/or indices) leading to a validated value. A string indicates access of a data member, an integer indicates accessing a vector or matrix element.\n\n\n\n\n\n","category":"type"},{"location":"validations.html#SomeGraphs.Validations.location","page":"Validations","title":"SomeGraphs.Validations.location","text":"stringify_context(context::ValidationContext)::AbstractString\n\nConvert a ValidationContext to a string for error messages.\n\n\n\n\n\n","category":"function"},{"location":"validations.html#Index","page":"Validations","title":"Index","text":"","category":"section"},{"location":"validations.html","page":"Validations","title":"Validations","text":"Pages = [\"validations.md\"]","category":"page"},{"location":"index.html#SomeGraphs","page":"SomeGraphs","title":"SomeGraphs","text":"","category":"section"},{"location":"index.html","page":"SomeGraphs","title":"SomeGraphs","text":"SomeGraphs.SomeGraphs","category":"page"},{"location":"index.html#SomeGraphs.SomeGraphs","page":"SomeGraphs","title":"SomeGraphs.SomeGraphs","text":"Wrappers around Plotly to generate some types of graphs.\n\n\n\n\n\n","category":"module"},{"location":"index.html#Index","page":"SomeGraphs","title":"Index","text":"","category":"section"},{"location":"index.html","page":"SomeGraphs","title":"SomeGraphs","text":"","category":"page"}]
}
