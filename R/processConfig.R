# Read a config file
#' @importFrom data.table fread
process_config <- function(inputfile) {

    # Check input file
    if(!file.exists(inputfile)) {
        return(NA)
    } else {
        raw <- fread(inputfile, fill = T)
    }

    # Getting the values
    ftgt <- as.numeric(raw[tolower(V1) == "ftarget"][[2]])
    fbar <- as.numeric(unlist(raw[tolower(V1) == "fagerange", c(2,3)]))
    bpa <- as.numeric(raw[tolower(V1) == "bpa"][[2]])
    threeYrRule <- ifelse(raw[tolower(V1) == "3yearrule"][[2]] == "yes", TRUE, FALSE)
    tac <- as.numeric(raw[tolower(V1) == "tacpreviousyear"][[2]])
    maxup <- as.numeric(raw[tolower(V1) == "maxchangeup"][[2]])/100
    maxdown <- as.numeric(raw[tolower(V1) == "maxchangedown"][[2]])/100

    return(list(
                hcrObj = list(args = list(
                                tac = tac,
                                ftgt = ftgt,
                                fbar = fbar,
                                bpa = bpa,
                                threeYrRule = threeYrRule,
                                maxTACVar = maxup,
                                minTACVar = maxdown
                            )
                        )
            )
    )
}