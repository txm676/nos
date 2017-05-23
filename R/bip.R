#' Compute NOS using a bipartite network
#'
#' @description Computation of NOS using a bipartite network (e.g.
#'   plant-pollinator network), where nodes can be formally categorized into two
#'   distinct categories (e.g. plant-pollinators). All nodes in one category
#'   will be considered as potential partners for the nodes in the other
#'   category (and vice-versa).
#' @usage NOSM_bip(net, perc = 1, sl = 1)
#' @param net A network, in the form of an edge list. This should be a matrix or
#'   dataframe with two columns. Each value in a column is a node (e.g. a food
#'   item in a trophic-web). Nodes can be identified using numbers or
#'   characters. For each row (i.e. node pair), the value in the first column is
#'   'consumed' (or pollinated, parasitized etc) by the value in the second
#'   column.
#' @param perc (default to 1) - the fraction of node pair comparisons to be
#'   performed to compute NOS. We recommend performing all possible pair
#'   comparisons (perc = 1). However, for exploratory analyses on large sets of
#'   networks (or for very large networks), the possibility of using a lower
#'   fraction of pair comparisons is a useful option.
#' @param sl (default is 1) Specifies whether cannibalistic interactions should
#'   be considered as possible and therefore taken into account and removed
#'   during computation ('1') or not ('0').
#' @return A list (two elements) of class 'NOSM' with a 'Type' attribute 'bip'.
#'   The first element in the list is a vector of overlap values for the "in
#'   nodes" and the second element is a vector of overlap values for the "out
#'   nodes".
#'
#'   The \code{\link{summary.NOSM}} methods provides more useful summary
#'   statistics.
#' @examples
#' data(boreal)
#' x <- NOSM_bip(boreal, perc = 1, sl = 1)
#' summary(x)
#' @export


NOSM_bip <- function(net,perc = 1, sl = 1){
  x <- adj(net)
  net <- form(net)
  vin <- sort(unique(net[, 2]))
  vout <- sort(unique(net[, 1]))
  pot_in <- rep(list(vout), length(x$adj_in))
  pot_out <- rep(list(vin), length(x$adj_out))
  ov_in <- OV(x$adj_in, pot_in, perc, sl)
  ov_out <- OV(x$adj_out, pot_out, perc, sl)
  y <- list("ov_in" = ov_in, "ov_out" = ov_out)
  class(y) <- "NOSM"
  attr(y, "Type") <- "bip"
  return(y)
}
