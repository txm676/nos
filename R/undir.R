#' Compute NOS using an undirected network without a user provided
#' network of potential interactions
#'
#' @description Computation of NOS using an undirected network (e.g. a
#'   social co-occurence network) and without a user provided network of potential
#'   interactions. In an undirected network, all nodes are considered as
#'   potential interacting partners.
#' @usage NOSM_undir(net, perc = 1, sl = 1)
#' @param net A network, in the form of an edge list. This should be a matrix or
#'   dataframe with two columns. Each value in a column is a node. Nodes can be
#'   identified using numbers or characters.
#' @param perc (default to 1) - the fraction of node pair comparisons to be
#'   performed to compute NOS. We recommend performing all possible pair
#'   comparisons (perc = 1). However, for exploratory analyses on large sets of
#'   networks (or for very large networks), the possibility of using a lower
#'   fraction of pair comparisons is a useful option.
#' @param sl (default is 1) Specifies whether cannibalistic interactions should
#'   be considered as possible and therefore taken into account and removed
#'   during computation ('1') or not ('0').
#' @return A list of class 'NOSM' with a 'Type' attribute 'Undir', containing a
#'   vector of overlap values. The \code{\link{summary.NOSM}} methods provides
#'   more useful summary statistics.
#' @examples
#' data(boreal)
#' y <-  boreal[sample(rownames(boreal), 100, FALSE),] #subset 100 rows for speed
#' x <- NOSM_undir(y, perc = 1, sl = 1)
#' summary(x)
#' @export






NOSM_undir <- function(net, perc = 1, sl = 1){
  x <- adj(net)
  net <- form(net)
  Dvin <- sort(unique(net[,2]))
  Dvout <- sort(unique(net[,1]))
  vall <- sort(union(Dvin, Dvout))
  pot_all <- rep(list(vall), length(x$adj_all))
  y <- OV(x$adj_all, pot_all, perc, sl)
  class(y) <- "NOSM"
  attr(y, "Type") <- "Undir"
  return(y)
}
