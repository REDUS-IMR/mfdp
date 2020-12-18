#' Run MFDP
#'
#' @param input_file A path to the MFDP index file
#' @param run_name A name for the run. This will be applied in the result file names.
#' @return A list of result objects
#' @importFrom utils head read.csv tail
#' @export
mfdp <- function(input_file, run_name = "mfdp") {

    # Read MFDP index file
    raw <- read_mfdp(input_file)

    # get stk
    stk <- raw$stk

    # Parameters
    parameters <- list()
    # Prepare flag parameter
    parameters$flag <- raw$flag
    # Prepare target parameter
    parameters$target <- raw$target

    # management.options.table
	res1 <- doForecast(stk, parameters, management.options.table = TRUE)
	table1 <- generateTable1(res1, paste0(run_name , "-prm.pdf"))

    # single.option.prediction
	res2 <- doForecast(stk, parameters, single.option.prediction = TRUE)
	table2 <- generateTable2(res2, paste0(run_name , "-prs.pdf"))

    # Disable applying rules for now
    res3 <- NA #applyRules(res2)
	table3 <- NA

    # Create XLSX
	generateXlsx(table1, table2, table3, run_name)

    # Create plots
	generatePlots(res1, res2, res3, run_name)

    # Retun list
	return(list(res1, res2, res3))
}