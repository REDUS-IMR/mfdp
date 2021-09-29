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
#' @importFrom FLCore trim iter fbar ssb catch
#' @importFrom data.table rbindlist setnames
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
    if(is.na(raw$stk) || !is(raw$stk, "FLStock")) {
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

    # single.option.prediction
    res2 <- doForecast(stk, parameters, single.option.prediction = TRUE)

    # Applying extra rules if we found extra config file
    if(is.null(parameters$hcrObj)) {
        # Create tables in PDF
        table1 <- generateTable1(res1, run_name, out_dir)
        table2 <- generateTable2(res2, run_name, out_dir)

        # Create XLSX
        generateXlsx(table1, table2, filename = run_name, out_dir = out_dir)

        # Create plots
        generatePlots(res1, res2, filename = run_name, out_dir = out_dir)

        # Return results
        return(list(prm = res1, prs = res2))
    } else {
        # For summary table
        tac0 <- extraConf$hcrObj$args$tac # TODO: check this and below
        adv0 <- extraConf$hcrObj$args$tac

        summary <- list()

        # Loop through different F
        suffixes <- c("fmgmt", "f0", "fsq", "fpa", "flim")
        newTargets <- c(NA, 0, as.numeric(iter(res1$ftgt[1,1,],1)), extraConf$hcrObj$args$fpa, extraConf$hcrObj$args$flim)
        res1_1 <- list()
        res2_1 <- list()
        table1_1 <- list()
        table2_1 <- list()
        for (idx in seq_along(suffixes)) {
            suffix <- suffixes[idx]
            revisedTarget <- applyRules(res2, 1)
            if(is.na(newTargets[idx])) {
                parameters$flag[2] <- 1
                parameters$target[2] <- revisedTarget
            } else {
                parameters$flag[2] <- 3
                parameters$target[2] <- newTargets[idx]
            }
            stk <- raw$stk

            # Just do short term for now
            future <- min(length(years) - 1, 3)
            stk <- trim(stk, year = as.numeric(dims(stk)$minyear):(as.numeric(dims(stk)$minyear)+ future))

            # Redo management.options.table and single.option.prediction
            # management.options.table
            res1_1[[suffix]] <- doForecast(stk, parameters, management.options.table = TRUE)
            table1_1[[suffix]] <- generateTable1(res1_1[[suffix]], paste0(run_name, "_", suffix), out_dir)

            # single.option.prediction
            res2_1[[suffix]] <- doForecast(stk, parameters, single.option.prediction = TRUE)
            table2_1[[suffix]] <- generateTable2(res2_1[[suffix]], paste0(run_name, "_", suffix), out_dir)

            # Create XLSX
            generateXlsx(table1_1[[suffix]], table2_1[[suffix]], filename = paste0(run_name, "_", suffix), out_dir = out_dir)

            # Create plots
            generatePlots(res1_1[[suffix]], res2_1[[suffix]], filename = paste0(run_name, "_", suffix), out_dir = out_dir)

            # Summary
            # F yr+1
            fbar1 <- as.numeric(fbar(res2_1[[suffix]]$stk)["all", 2])
            # SSB yr+1
            ssb0 <- as.numeric(ssb(res2_1[[suffix]]$stk)["all", 2])
            # SSB yr+2
            ssb1 <- as.numeric(ssb(res2_1[[suffix]]$stk)["all", 3])
            # Catch yr+1
            tac1 <- as.numeric(catch(res2_1[[suffix]]$stk)["all", 2])
            # Make list
            summary[[suffix]] <- list(suffix, tac1, fbar1, ssb1,
                                        100*(ssb1-ssb0)/ssb0,
                                        100*(tac1-tac0)/tac0,
                                        100*(tac1-adv0)/adv0)
        }

        # Collate table
        summary <- rbindlist(summary)
        setnames(summary, c("Basis",
                paste0("Total catch (", years[2], ")"),
                paste0("F ages ", range(stk)["minfbar"], "-", range(stk)["maxfbar"], " (", years[2], ")"),
                paste0("SSB (", years[3], ")"),
                "% SSB change",
                "% TAC change",
                "% Advice change"))
        generateSummaryXlsx(summary, filename = paste0(run_name, "_summary"), out_dir = out_dir)

        # Return results
        return(list(prm = res1_1, prs = res2_1, summary = summary))
    }
}
