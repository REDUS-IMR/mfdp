
# Test reading single file input

# Test first form of input
input <- read_mfdp_input_table("../sample/nea-had-2020.input.csv")
output <- mfdp(input, "../sample/nea-had-2020.params.txt")

expect_true(is(output[[1]][["fmgmt"]]$stk, "FLStock"))
expect_true(is(output[[2]][["fmgmt"]]$stk, "FLStock"))


# Test second form of input
input <- read_mfdp_input_table("../sample/nea-had-2020.input2.csv")
output <- mfdp(input, "../sample/nea-had-2020.params.txt")

expect_true(is(output[[1]][["fmgmt"]]$stk, "FLStock"))
expect_true(is(output[[2]][["fmgmt"]]$stk, "FLStock"))
