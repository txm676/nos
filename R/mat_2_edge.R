#' Convert a frequency interaction matrix to an edge list
#'
#' @description Converts frequency interaction matrices, as used with the
#'   \link[bipartite]{bipartite} R package, into edge lists. The input matrix
#'   can be either a matrix representing a bipartite graph or a squared
#'   adjacency matrix. As the functions in the 'nos' R package are based on
#'   presence-absence data, all interactions greater than or equal to 1 are set
#'   to 1.
#' @usage freqMat_2_edge(x, bip = FALSE, sp_nam = FALSE)
#' @param x A frequency interaction matrix, in which rows and columns represent
#'   species and each value should be numeric, indicating the number of
#'   interactions between two species. The input frequency interaction matrix
#'   can be in the form of a squared adjacency matrix (e.g. a food web, where
#'   the rows and columns represent the same set of species) or representing a
#'   bipartite graph where rows and columns correspond to different entities. In
#'   regards to the latter, the rows must correspond to species that are
#'   consumed (or pollinated etc) and columns are those species that are the
#'   consumers (or pollinators etc). For example, for a plant-pollinator
#'   interaction matrix, the rows would be the plant species, and the columns
#'   the pollinator species. The input interaction matrix can be a dataframe or
#'   a matrix. The names of the species can be set as the rownames and column
#'   names of the interaction matrix prior to running the function; if names are
#'   not provided, the function automatically names the species (see below). As
#'   the functions in the 'nos' R package are based on presence-absence data,
#'   all interactions greater than or equal to 1 are set to 1. Cannibalistic
#'   interactions are allowed, i.e. the same species can be in a row and column.
#' @param bip A logical value describing whether the input matrix represents a
#'   bipartite graph (bip = TRUE), or a squared adjaceny matrix (bip = FALSE: the
#'   default).
#' @param sp_nam A logical value describing whether the user has provided.
#'   species names i.e. row and column names (sp_nam = TRUE) or not (sp_nam = FALSE:
#'   the default).If species names are not provided and the input matrix is a
#'   squared adjaceny matrix, the rows and columns are given the same identity
#'   values (i.e. numbers). If the input matrix represent a bipartite graph,
#'   rows and columns are assumed to correspond to different entities and are
#'   given different unique values.
#' @return An edge list in the form of a dataframe. Each value in a column is a
#'   node (e.g. a food item in a trophic-web). Nodes are set as numeric values.
#'   For each row (i.e. node pair), the value in the first column is 'consumed'
#'   (or pollinated, parasitized etc) by the value in the second column.
#'
#' @examples
#' sim_dat <- matrix(c(0, 2, 1, 0, 1, 2, 0, 1), ncol = 4) #simulate bipartite matrix
#' rownames(sim_dat) <- c("A", "B") #name the consumed species
#' colnames(sim_dat) <- c("C", "D", "A", "E") #name the consumer species
#' freqMat_2_edge(sim_dat, bip = TRUE, sp_nam = TRUE)

#' @export


freqMat_2_edge <- function(x, bip = FALSE, sp_nam = FALSE){
  if (anyNA(x)) stop("Nas present in interaction matrix")
  if (!all(vapply(x, is.numeric, logical(1)))) stop("interaction matrix should be numeric")
  if (any(x < 0) || all(x == 0)) stop("interaction matrix contains incorrect values")

  # if bip == F the matrix is expected to be an adjacency matrix, which should be squared
  if (bip == FALSE && dim(x)[1] != dim(x)[2]) stop("adjacency matrix must be squared")


  # if the input is an adjacency matrix, and rows and cols name are not provided, those
  #are assigned automatically. If row names are provided but not col names or viceversa,
  #available name are used (since rows and columns should represent the same set of species)

  if (bip == FALSE && sp_nam == FALSE){
      rownames(x) <- 1:dim(x)[1]
      colnames(x) <- 1:dim(x)[1]
  }

  # if the input is a bipartite matrix and row or col names are not provided, those
  #are assigned automatically; since the two sets of names should be disjoint, to
  #circumvent Murphy's law, both row and column names are always assigned from
  #scratch

  if (bip != FALSE && sp_nam == FALSE){
      rownames(x) <- 1:dim(x)[1]
      colnames(x) <- (dim(x)[1]+1):(dim(x)[1]+dim(x)[2])
  }

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
  df2 <- data.frame(df2$V1, df2$V2)
  colnames(df2) <- c("V1", "V2")

  if (nrow(df2) != length(which(x > 0)) || ncol(df2) != 2) stop("conversion of interaction matrix
                                                                to edge list has failed")

  df2 <- apply(df2, 2, as.numeric)
  return(df2)

}
