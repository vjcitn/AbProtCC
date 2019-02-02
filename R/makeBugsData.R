#' create input for barca from a csvfile that has a particular structure
#' @param csvfile character(1) path to a CSV file with specific layout, see note
#' @param prefix character(1) used to prefix ".jagsdata" for output
#' @param dodump logical(1) if true, will write data in jags format to [prefix].jagsdata
#' @note Some csv files used in this application had a 'Median' summary as last line.  This must be removed.
#' @examples
#' infile = system.file(paste0("csv/", "rewriteV", ".csv"), package="barca") # use existing test file
#' tdir = tempdir()
#' od = getwd()
#' setwd(tdir)
#' makeBugsData(infile)
#' readLines("test.jagsdata")
#' setwd(od)
#' @export
makeBugsData = function (csvfile, prefix="test", dodump = TRUE) 
{
    sspec = read.csv(csvfile, stringsAsFactors = FALSE)
    N = nrow(sspec)
    A = sspec$Control.Maternal.Ab
    A[seq(1, N, 4)] = sspec[seq(1, N, 4), 3]
    str = rep(1:(N/4), each = 4)
    Nstrat = N/4
    Nsubg = 2
    d = rep(c(1, 0, 0, 0), Nstrat)
    if (dodump) 
        dump(c("N", "Nstrat", "Nsubg", "str", "d", "A"), paste0(prefix, 
            ".jagsdata"))
    else list(N = N, Nstrat = Nstrat, Nsubg = Nsubg, str = str, 
        d = d, A = A)
}

