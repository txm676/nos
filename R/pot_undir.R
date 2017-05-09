#' Compute N_bar using an undirected network and with a user provided
#' network of potential interactions
#'
#' @description Computation of N_bar (NOS) using an undirected network (e.g. a
#'   social network) and with a user provided network of potential interactions.
#'   In an undirected network, all nodes are considered as potential interacting
#'   partners.
#' @usage NOSM_POT_undir(net, pot_net, perc = 1, sl = 1)
#' @param net A network, in the form of an edge list. This should be a matrix or
#'   dataframe with two columns. Each value in a column is a node. Nodes can be
#'   identified using numbers or characters.
#' @param pot_net A network of all potential interactions. These should include,
#'   as a minimum, all the observed interactions (i.e. all links in net),plus
#'   any other possible interaction (such as all those permitted by a certain
#'   trophic rule). pot_net should have the same structure as net (e.g. it
#'   should be a data frame or matrix).
#' @param perc (default to 1) - the fraction of node pair comparisons to be
#'   performed to compute N_bar. We recommend performing all possible pair
#'   comparisons (perc = 1). However, for exploratory analyses on large sets of
#'   networks (or for very large networks), the possibility of using a lower
#'   fraction of pair comparisons is a useful option.
#' @param sl (default is 1) Specifies whether cannibalistic interactions should
#'   be considered as possible and therefore taken into account and removed
#'   during computation ('1') or not ('0').
#' @return A list of class 'NOSM' with a 'Type' attribute 'Pot_undir',
#'   containing a vector of overlap values. The \code{\link{summary.NOSM}}
#'   methods provides more useful summary statistics.
#'
#' @examples
#' data(boreal)
#' d <- sample(nrow(boreal), 1000, replace = FALSE) #create a random pot_net
#' pot_net <- boreal[d,] #by randomly sampling 1000 rows from boreal
#' x <- NOSM_POT_undir(boreal, pot_net, perc = 1, sl = 1)
#' summary(x)
#' @export



NOSM_POT_undir <- function(net, pot_net, perc = 1, sl = 1){
  x <- adj(net, pot_net)
  y <- OV(x$adj_all, x$pot_all, perc, sl)
  class(y) <- "NOSM"
  attr(y, "Type") <- "Pot_undir"
  return(y)
}
