#' Run MFDP
#'
#' @param input Input to MFDP. It can be either 1) A path to the MFDP index file, or 2) An FLStock object
#'          from `read_mfdp_input_table()`.
#' @param configs Path to extra configuration file for MFDP. This may contain TAC,
#'          HCR function, among others.
#' @param run_name A name for the run. This will be applied in the result file
#'          names. Default is 'mfdp'.
#' @param out_dir Path for the generated output files. Default is `tempdir()`.
#' @return A list of result objects
#' @importFrom utils head read.csv tail
#' @importFrom methods is
#' @importFrom FLCore trim
#' @export
mfdp <- function(input, configs = NULL, run_name = "mfdp", out_dir = tempdir()) {

    # Check input type
    if(is(input, "FLStock")) {
        if(is.null(configs)) {
            print("Specifiying FLStock object as input must be accompanied by the config object.")
            return(NA)
        }
        raw <- list(stk = input)
    } else {
        # Read MFDP index file
        raw <- read_mfdp(input)
    }

    # Check reading
    if(length(raw) == 0 || is.na(raw)) {
        print("Error parsing input.")
        return(NULL)
    }

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

    # Process extra config (overrides the mfdp input)
    if(!is.null(configs)) {
        years <- c(as.numeric(dims(stk)$minyear):as.numeric(dims(stk)$maxyear))
        extraConf <- process_config(configs)
        parameters$flag <- c(1, rep(3, length(years) - 1))
        parameters$target <- c(extraConf$hcrObj$args$tac, rep(extraConf$hcrObj$args$ftgt, length(years) - 1))
        parameters$hcrObj <- extraConf$hcrObj
        # TODO: Fbar overrides??
    }

    # management.options.table
    res1 <- doForecast(stk, parameters, management.options.table = TRUE)
    table1 <- generateTable1(res1, run_name, out_dir)

    # single.option.prediction
    res2 <- doForecast(stk, parameters, single.option.prediction = TRUE)
    table2 <- generateTable2(res2, run_name, out_dir)

    # Create XLSX
    generateXlsx(table1, table2, filename = run_name, out_dir = out_dir)

    # Create plots
    generatePlots(res1, res2, filename = run_name, out_dir = out_dir)

    # Applying rules if we found extra configs
    if(!is.null(parameters$hcrObj) && !is.null(extraConf$hcrObj$args$threeYrRule) && extraConf$hcrObj$args$threeYrRule == TRUE) {
        revisedTarget <- applyRules(res2, 1)
        parameters$flag[2] <- 1
        parameters$target[2] <- revisedTarget
        stk <- raw$stk

        # Just do short term for now
        stk <- trim(stk, year = as.numeric(dims(stk)$minyear):as.numeric(dims(stk)$minyear + 3))

        # Redo management.options.table and single.option.prediction
        # management.options.table
        res1_1 <- doForecast(stk, parameters, management.options.table = TRUE)
        table1_1 <- generateTable1(res1_1, paste0(run_name, "_3yrRule"), out_dir)

        # single.option.prediction
        res2_1 <- doForecast(stk, parameters, single.option.prediction = TRUE)
        table2_1 <- generateTable2(res2_1, paste0(run_name, "_3yrRule"), out_dir)

        # Create XLSX
        generateXlsx(table1_1, table2_1, filename = paste0(run_name, "_3yrRule"), out_dir = out_dir)

        # Create plots
        generatePlots(res1_1, res2_1, filename = paste0(run_name, "_3yrRule"), out_dir = out_dir)
    }

    # Retun list
    return(list(res1, res2))
}
