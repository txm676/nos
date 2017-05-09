#' Summarising the results of the five main NOSM functions
#'
#' @description S3 method for class 'NOSM'. summary.NOSM creates summary
#'   statistics for objects of class NOSM. The exact summary statistics computed
#'   depends on the 'Type' attribute (e.g. 'bip') of the NOSM object (see
#'   below). The summary method generates more useful information for the user
#'   than the standard NOSM functions. Another S3 method (print.summary.NOSM;
#'   not documented) is used to print the output.
#' @usage summary(x, y = 3)
#' @param x An object of class 'NOSM'.
#' @param y (default of 3) The adjustment value for the computation of the z
#'   value (see Strona & Veech, 2015).
#' @return Returns object of class 'summary.NOSM' with a Type attribute (e.g.
#'   'bip') which is inherited. For NOSM objects of Type 'Pot_dir', 'bip' or
#'   'Dir', the summary.NOSM method returns the mean of the overlap values for
#'   the "in nodes" (NOS_In), the mean of the overlap values for the "out nodes"
#'   (NOS_Out), the mean of Nos In and Nos Out (NOS; N_bar), the standard
#'   deviation of the overlap values for the "in nodes" (MOD_In), the SD of the
#'   overlap values for the "out nodes" (MOD_Out), and the SD of the combined
#'   set of overlap values (MOD; network modularity).
#'
#'   For NOSM objects of Type 'Dir' and 'Undir', the summary.NOSM method returns
#'   just the NOS (N_bar) and MOD values (network modularity).
#'
#'   For all types of NOSM object, the z value and associated p value are also
#'   provided (see Strona & Veech, 2015).
#' @seealso \code{\link{NOSM_bip}}, \code{\link{NOSM_POT_dir}},
#'   \code{\link{NOSM_POT_undir}}, \code{\link{NOSM_dir}},
#'   \code{\link{NOSM_undir}}
#' @references Strona, G. & Veech, J. A. (2015). A new measure of ecological
#'   network structure based on node overlap and segregation. Methods in Ecology
#'   and Evolution, 6(8), 907-915.
#' @examples
#' data(boreal)
#' x <- NOSM_bip(boreal, perc = 1, sl = 1)
#' summary(x, y = 3)
#' @export


summary.NOSM <- function(x, y = 3){
  if (attributes(x)$Type == "Pot_dir" || attributes(x)$Type == "bip" ||
      attributes(x)$Type == "Dir"){
    a <- round(mean(x$ov_in),3)
    b <- round(mean(x$ov_out),3)
    d <- round(mean(c(a, b)), 3)
    e <- round(sd(x$ov_in),3)
    f <- round(sd(x$ov_out),3)
    g <- round(sd(c(x$ov_in, x$ov_out)), 3)
    z <- mean(c(x$ov_in, x$ov_out)) / ((sd(c(x$ov_in, x$ov_out))) / sqrt(y))
    p <- z2p(z)
    res <- c(a, b, d, e, f, g, z, p)
  }
  if (attributes(x)$Type == "Pot_undir" || attributes(x)$Type == "Undir"){
    nes <- mean(x)
    mod <- sd(x)
    z <- nes / (mod/sqrt(y))
    p <- z2p(z)
    res <- c(nes, mod, z, p)
  }
  class(res) <- "summary.NOSM"
  attr(res, "Type") <- attributes(x)$Type
  attr(res, "z_tune") <- y
  return(res)
}




