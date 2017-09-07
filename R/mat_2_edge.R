#' Convert a frequency interaction matrix to an edge list
#'
#' @description Converts frequency interaction matrices, as used with the
#'   \link[bipartite]{bipartite} R package, into edge lists. As the functions in the 'nos' R
#'   package are based on presence-absence data, all interactions greater than
#'   or equal to 1 are set to 1.
#' @usage freqMat_2_edge(x)
#' @param x A frequency interaction matrix, in which rows are species that are
#'   consumed (or pollinated etc) and columns are those species that are the
#'   consumers (or pollinators etc). For example, for a plant-pollinator
#'   interaction matrix, the rows would be the plant species, and the columns
#'   the pollinator species. The interaction matrix can be a dataframe or a
#'   matrix. Each value should be numeric, indicating the number of interactions
#'   between two species. The names of the species must be set as the rownames
#'   and column names of the interaction matrix prior to running the function.
#'   As the functions in the 'nos' R package are based on presence-absence data,
#'   all interactions greater than or equal to 1 are set to 1. Cannibalistic
#'   interactions are allowed, i.e. the same species can be in a row and column.
#' @return An edge list in the form of a dataframe. Each value in a column is a
#'   node (e.g. a food item in a trophic-web). Nodes are set as numeric values.
#'   For each row (i.e. node pair), the value in the first column is 'consumed'
#'   (or pollinated, parasitized etc) by the value in the second column.
#'
#' @examples
#' sim_dat <- matrix(c(0, 2, 1, 0, 1, 2, 0, 1), ncol = 4) #simulate food web matrix
#' rownames(sim_dat) <- c("A", "B") #name the consumed species
#' colnames(sim_dat) <- c("C", "D", "A", "E") #name the consumer species (with one cannibal interaction)
#' freqMat_2_edge(sim_dat)

#' @export


freqMat_2_edge <- function(x){
  if (anyNA(x)) stop("Nas present in interaction matrix")
  if (!all(vapply(x, is.numeric, logical(1)))) stop("interaction matrix should be numeric")
  if (any(x < 0) || all(x == 0)) stop("interaction matrix contains incorrect values")

  #work out numeric rownames: allows for cannibalistic (i.e. same sp in row and col)
  xx <- union(rownames(x), colnames(x))
  rownames(x) <- match(rownames(x), xx)
  colnames(x) <- match(colnames(x), xx)

  nc <- ncol(x)
  rn <- mapply(rep, rownames(x), times = nc, simplify = F)
  rn2 <- as.vector(matrix(rn, ncol = 1, byrow = T))
  cn <- rep(colnames(x), nrow(x))
  c3 <- as.vector(t(as.matrix(x)))#transpose to get by rows
  df <- data.frame("V1" = rn2, "V2" = cn, "V3" = c3)
  df2 <- dplyr::filter(df, df$V3 > 0)
  df2 <- dplyr::select(df2, V1, V2)

  if (nrow(df2) != length(which(x > 0)) || ncol(df2) != 2) stop("conversion of interaction matrix
                                                                to edge list has failed")

  df2 <- apply(df2, 2, as.numeric)
  return(df2)

}
