#' Compute NOS using a directed network without a user provided network
#' of potential interactions
#'
#' @description Computation of NOS using a directed network (e.g. food
#'   web) and without a user provided network of potential interactions. In a
#'   directed, unimode network, all nodes having at least an in-coming link will
#'   be considered as potential partners for nodes having at least an out-going
#'   link and vice-versa.
#' @usage NOSM_dir(net, perc = 1, sl = 1)
#' @param net A network, in the form of an edge list. This should be a matrix or
#'   dataframe with two columns. Each value in a column is a node (e.g. a food
#'   item in a trophic-web). Nodes can be identified using numbers or
#'   characters. For each row (i.e. node pair), the value in the first column is
#'   'consumed' (or pollinated, parasitized etc) by the value in the second
#'   column.Data can also be in the format of a frequency interaction matrix,
#'   as used in the \link[bipartite]{bipartite} R package. In these cases
#'   \code{\link{freqMat_2_edge}} should be used first, to convert the
#'   interaction matrix to an edge list.
#' @param perc (default to 1) - the fraction of node pair comparisons to be
#'   performed to compute NOS. We recommend performing all possible pair
#'   comparisons (perc = 1). However, for exploratory analyses on large sets of
#'   networks (or for very large networks), the possibility of using a lower
#'   fraction of pair comparisons is a useful option.
#' @param sl (default is 1) Specifies whether cannibalistic interactions should
#'   be considered as possible and therefore taken into account and removed
#'   during computation ('1') or not ('0').
#' @return A list (two elements) of class 'NOSM' with a 'Type' attribute 'Dir'.
#'   The first element in the list is a vector of overlap values for the "in
#'   nodes" and the second element is a vector of overlap values for the "out
#'   nodes".
#'
#'   The \code{\link{summary.NOSM}} methods provides more useful summary
#'   statistics.
#' @examples
#' data(boreal)
#' y <-  boreal[sample(rownames(boreal), 100, FALSE),] #subset 100 rows for speed
#' x <- NOSM_dir(y, perc = 1, sl = 1)
#' summary(x)
#' @export



NOSM_dir <- function(net, perc = 1, sl = 1){
  x <- adj(net)
  net <- form(net)
  Dvin <- sort(unique(net[,2]))
  Dvout <- sort(unique(net[,1]))
  vall <- sort(union(Dvin, Dvout))
  pot_in <- rep(list(vall), length(x$adj_in))
  pot_out <- rep(list(vall), length(x$adj_out))
  ov_in <- OV(x$adj_in, pot_in, perc, sl)
  ov_out <- OV(x$adj_out, pot_out, perc, sl)
  y <- list("ov_in" = ov_in, "ov_out" = ov_out)
  class(y) <- "NOSM"
  attr(y, "Type") <- "Dir"
  return(y)
}
