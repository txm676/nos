#' Compute NOS using a directed network and with a user provided network
#' of potential interactions
#'
#' @description Computation of NOS using an directed network (e.g. a
#'   food web) and with a user provided network of potential interactions.
#' @usage NOSM_POT_dir(net, pot_net, perc = 1, sl = 1)
#' @param net A network, in the form of an edge list. This should be a matrix or
#'   dataframe with two columns. Each value in a column is a node. Nodes can be
#'   identified using numbers or characters.
#' @param pot_net A network of all potential interactions. These should include,
#'   as a minimum, all the observed interactions (i.e. all links in net),plus
#'   any other possible interaction (such as all those permitted by a certain
#'   trophic rule). pot_net should have the same structure as net (e.g. it
#'   should be a data frame or matrix).
#' @param perc (default to 1) - the fraction of node pair comparisons to be
#'   performed to compute NOS. We recommend performing all possible pair
#'   comparisons (perc = 1). However, for exploratory analyses on large sets of
#'   networks (or for very large networks), the possibility of using a lower
#'   fraction of pair comparisons is a useful option.
#' @param sl (default is 1) Specifies whether cannibalistic interactions should
#'   be considered as possible and therefore taken into account and removed
#'   during computation ('1') or not ('0').
#' @return A list (two elements) of class 'NOSM' with a 'Type' attribute
#'   'Pot_dir'. The first element in the list is a vector of overlap values for
#'   the "in nodes" and the second element is a vector of overlap values for the
#'   "out nodes".
#'
#'   The \code{\link{summary.NOSM}} methods provides more useful summary
#'   statistics.
#' @examples
#' data(boreal)
#' y <-  boreal[sample(rownames(boreal), 600, FALSE),] #subset 600 rows for speed
#' d <- sample(nrow(y), 400, replace = FALSE) #create a random pot_net
#' pot_net <- y[d,] #by randomly sampling 400 rows from boreal
#' x <- NOSM_POT_dir(y, pot_net, perc = 1, sl = 1)
#' summary(x)
#' @export



NOSM_POT_dir <- function(net,pot_net,perc = 1, sl = 1){
  x <- adj(net, pot_net)
  OV_in <- OV(x$adj_in, x$pot_in, perc, sl)
  OV_out <- OV(x$adj_out, x$pot_out, perc, sl)
  y <- list("ov_in" = OV_in, "ov_out" = OV_out)
  class(y) <- "NOSM"
  attr(y, "Type") <- "Pot_dir"
  return(y)
}
