
# Testing "vanilla" input of MFDP

# Getting all the index files
index_file <- list.files("../sample/nea-had-2020", pattern = "*ind*.txt", full.names = TRUE)

# Process all
for(fidx in index_file) {
    run_name <- tools::file_path_sans_ext(basename(fidx))
    output <- mfdp(fidx, run_name = run_name)
    expect_true(is(output[[1]]$stk, "FLStock"))
    expect_true(is(output[[2]]$stk, "FLStock"))
}

