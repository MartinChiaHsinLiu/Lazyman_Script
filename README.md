# LazymanScript

`LazymanScript` is an R package containing robust utility functions to seamlessly convert clinical data tables between wide and long formats. Unlike standard pivot functions, `LazymanScript` preserves exact data types (numeric, character, logical) during transformations, making it ideal for messy biomedical datasets. It also includes a robust identity checker to verify that data integrity is maintained during these transformations.

## Installation

You can install the development version of LazymanScript directly from GitHub using the `devtools` package:

```R
# Install devtools if you haven't already
if (!require(devtools)) install.packages("devtools")

# Install LazymanScript from GitHub
devtools::install_github("MartinChiaHsinLiu/Lazyman_Script")
```

## Usage and Documentation

Once installed, you can read the comprehensive package vignette which explains how to melt, cast, and verify datasets using `LazymanScript`:

```R
library(LazymanScript)
vignette("LazymanScript")
```
