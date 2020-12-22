#' Run MFDP
#'
#' @param input_file A path to the MFDP index file
#' @param run_name A name for the run. This will be applied in the result file
#'          names. Default is 'mfdp'.
#' @param out_dir Path for the generated output files. Default is `tempdir()`.
#' @return A list of result objects
#' @importFrom utils head read.csv tail
#' @export
mfdp <- function(input_file, run_name = "mfdp", out_dir = tempdir()) {

    # Read MFDP index file
    raw <- read_mfdp(input_file)

    # Prepare out_dir (if not exists)
    if(!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE)
    }

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
	table1 <- generateTable1(res1, run_name, out_dir)

    # single.option.prediction
	res2 <- doForecast(stk, parameters, single.option.prediction = TRUE)
	table2 <- generateTable2(res2, run_name, out_dir)

    # Disable applying rules for now
    res3 <- NA #applyRules(res2)
	table3 <- NA

    # Create XLSX
	generateXlsx(table1, table2, table3, run_name, out_dir)

    # Create plots
	generatePlots(res1, res2, res3, run_name, out_dir)

    # Retun list
	return(list(res1, res2, res3))
}