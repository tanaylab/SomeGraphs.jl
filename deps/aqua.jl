push!(LOAD_PATH, ".")

using Aqua
using SomeGraphs
Aqua.test_ambiguities([SomeGraphs])
Aqua.test_all(SomeGraphs; ambiguities = false, unbound_args = false, deps_compat = false, persistent_tasks = false)
