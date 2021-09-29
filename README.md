
# (R) MFDP: Multi Fleet Deterministic Projection in R

<!-- badges: start -->
<!-- badges: end -->

The goal of R MFDP is to create an R package that can emulate the Multi 
Fleet Deterministic Projection (MFDP) program made by Lowestoft in 2000.

In the end the R MFDP program is so much more than the original MFDP:

1. In addition of the "original" MFDP input, R MFDP also supports a single
text file input data (cf. `inst/sample` directory)

2. R MFDP supports not only **Ftarget** for running the forecast, but also
**Fpa** and **Flim**. The detailed results for these different **F** values
will be written into separate output files and their summary will be collated in
a single summary table.

3. R MFDP also can enforce some rules in doing the forecasts, such as **the 3 year rule**,  **maximum/minimum yearly TAC variability**, and **Bpa**.

4. Input and output data in R MFDP are utilizing the `FLStock` object from the
excellent FLR suite <https://flr-project.org>. This flexible format enable
further experiment possibilities.

For more detailed information on the above features, please have a look at the package vignettes.

## Installation

You can install the package directly from Github with:

``` r
remotes::install_github("REDUS-IMR/mfdp")
```

## Example

There are two ways of running R MFDP:

### 1. Using the original MFDP input data

The original MFDP input data usually consist of several files that
are grouped by a single index file. Some example of these files are
available in the `inst/sample/nea-had-2020` directory.

``` r
library(mfdp)
input <- system.file("sample/nea-had-2020", "Fpa2020ind.txt", package = "mfdp")
output <- mfdp(input, run_name = "test", out_dir = "out")
```

### 2. Using the single MFDP input file

In addition to the original MFDP input file, R MFDP also support
a simplified single-file input (cf. `inst/sample/nea-had-2020.input.csv`
file). This is file is actually identical to the `.prd` file from
the original MFDP run outputs.

However, to use the above input type, user must also provide a configuration
file (cf. `inst/sample/nea-had-2020.params.txt` file) that contains the
F and TAC values, and rules, among others, that will control the forecast run.

``` r
library(mfdp)

input_file <- system.file("sample", "nea-had-2020.input.csv", package = "mfdp")
config_file <- system.file("sample", "nea-had-2020.params.txt", package = "mfdp")

# Reading a single file input must be done in two steps
input <- read_mfdp_input_table(input_file)
output <- mfdp(input, configs = config_file, run_name = "test", out_dir = "out")
```