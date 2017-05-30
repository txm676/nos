#' Summarising the results of the five main NOSM functions
#'
#' @description S3 method for class 'NOSM'. summary.NOSM creates summary
#'   statistics for objects of class NOSM. The exact summary statistics computed
#'   depends on the 'Type' attribute (e.g. 'bip') of the NOSM object (see
#'   below). The summary method generates more useful information for the user
#'   than the standard NOSM functions. Another S3 method (print.summary.NOSM;
#'   not documented) is used to print the output.
#' @param object An object of class 'NOSM'.
#' @param y (default of 3) The adjustment value for the computation of the z
#'   value (see Strona & Veech, 2015).
#' @param ...	further arguments passed to or from other methods.
#' @return Returns object of class 'summary.NOSM' with a Type attribute (e.g.
#'   'bip') which is inherited. For NOSM objects of Type 'Pot_dir', 'bip' or
#'   'Dir', the summary.NOSM method returns the mean of the overlap values for
#'   the "in nodes" (NOS_In), the mean of the overlap values for the "out nodes"
#'   (NOS_Out), the mean of Nos In and Nos Out (NOS), the standard
#'   deviation of the overlap values for the "in nodes" (MOD_In), the SD of the
#'   overlap values for the "out nodes" (MOD_Out), and the SD of the combined
#'   set of overlap values (MOD; network modularity).
#'
#'   For NOSM objects of Type 'Dir' and 'Undir', the summary.NOSM method returns
#'   just the NOS and MOD values (network modularity).
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
#' z <- boreal[sample(rownames(boreal), 400, FALSE),] #subset for speed
#' x <- NOSM_bip(z, perc = 1, sl = 1)
#' summary(x, y = 3)
#' @importFrom stats sd
#' @export


summary.NOSM <- function(object, ..., y = 3){

  if (attributes(object)$Type == "Pot_dir" || attributes(object)$Type == "bip" ||
      attributes(object)$Type == "Dir"){
    a <- round(mean(object$ov_in),3)
    b <- round(mean(object$ov_out),3)
    d <- round(mean(c(a, b)), 3)
    e <- round(sd(object$ov_in),3)
    f <- round(sd(object$ov_out),3)
    g <- round(((e + f) / 2), 3)
    z <- mean(c(object$ov_in, object$ov_out)) / ((sd(c(object$ov_in, object$ov_out))) / sqrt(y))
    p <- z2p(z)
    res <- c(a, b, d, e, f, g, z, p)
  }
  if (attributes(object)$Type == "Pot_undir" || attributes(object)$Type == "Undir"){
    nes <- mean(object)
    mod <- sd(object)
    z <- nes / (mod/sqrt(y))
    p <- z2p(z)
    res <- c(nes, mod, z, p)
  }
  class(res) <- "summary.NOSM"
  attr(res, "Type") <- attributes(object)$Type
  attr(res, "z_tune") <- y
  return(res)
}




