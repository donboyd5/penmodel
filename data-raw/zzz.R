.onLoad <- function(libname, pkgname) {
  delayedAssign("data", load_frs_data(), assign.env = frs)
}

# The zzz.R file is traditionally used to define special hook functions that run
# when a package is loaded or attached. The .onLoad() function specifically
# executes when the package namespace is loaded, before any package code is run.

# Delayed Data Loading

# The code creates a delayed assignment for a variable named "data" with several
# important characteristics:

  # The data loading is lazy evaluated, meaning it won't execute load_frs_data()
  # until the first time the "data" variable is actually accessed.

  # The assignment happens in the frs environment rather than the global
  # environment, keeping the package namespace clean.

  # This approach is particularly useful for:
    # Avoiding loading large datasets until they're actually needed
    # Ensuring the data is fresh each time it's accessed
    # Preventing unnecessary memory usage during package startup2=

# Best Practices

# Using .onLoad() for this purpose follows R package development best practices
# because:

  # It runs during package loading rather than attachment, making it available
  # even when the package is loaded via :: operator

  # It avoids modifying the global environment, which is considered good
  # practice for packages

  # It provides a way to initialize package-level objects without immediate
  # resource consumption
