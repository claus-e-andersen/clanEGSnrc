#' @title Version function for the clanOptim library
#' @description Version function for the clanOptim library.
#' @usage
#' clanEGSnrc()
#' @name clanEGSnrc
#' @author Claus E. Andersen
#' @return A list of information about the version and functions within clanEGSnrc.
#' @export
clanOptim <- function(){
  list(name="clanEGSnrc",
       version=0.008,
       date="August 3, 2014 (uploaded Aug. 6, 2016)",
       functions=sort(c("clanEGSnrc",
                        "read.flurznrc.lst","read.egsnrc.plot"
       )))
}