
#this creates three lists of neighbors for each node; one including all
#neighbors, one including those consuming the node, and one including those
#consumed by the node. If a potential network of interactions is provided
#(pot_net), the function generates three analogous lists for potential
#neighbors. 'net' is a network in the form of a edge list, [[0,1],[1,2],...].

#' @importFrom stats runif
#' @importFrom dplyr filter

adj <- function(net, pot_net = NULL){
  if (!is.matrix(net) & !is.data.frame(net)) stop("net should be a matrix or dataframe")
  if (any(is.na(net))) stop("NAs present in user's data")
  if (!is.null(pot_net)) if(any(is.na(pot_net))) stop("NAs present in user's data")
  if (is.factor(net[, 1]) || is.factor(net[, 2])) warning("data include factors: converted to numeric")
  if (is.matrix(net)) net <- as.data.frame(net)
  if (!is.null(pot_net) & is.matrix(pot_net)) pot_net <- as.data.frame(pot_net)

  net <- form(net)
  vall2 <- sort(union(net[, 1], net[, 2]))
  adj_in <- list()
  adj_out <- list()
  for (i in vall2){
    n2 <- dplyr::filter(net, net$X2 == i)
    n1 <- dplyr::filter(net, net$X1 == i)
    if (length(n2$X2) != 0){
      adj_in[[i]] <- sort(unique(n2$X1))
    } else {
      adj_in[[i]] <- 0
    }
    if (length(n1$X1) != 0){
      adj_out[[i]] <- sort(unique(n1$X2))
    }  else {
      adj_out[[i]] <- 0
    }
  }
  adj_all <- mapply(c, adj_in, adj_out, SIMPLIFY = FALSE)
  if (any(lapply(adj_all, function(x) if (length(x) == 1 & 0 %in% x) {1} else {0}) == 1)) warning("Some nodes have no edges")
  if (length(adj_all) != length(adj_in)) stop("Length of adjacency lists do not match")
  adj_all <- lapply(adj_all, function(x) if (0 %in% x) {x[x > 0]} else {x})
  if (length(pot_net) == 0) return(list("adj_in" = adj_in, "adj_out" = adj_out, "adj_all" = adj_all))
  pot_net <- form(pot_net)
  pot_in <- list()
  pot_out <- list()
  for (i in vall2){
    Pn2 <- dplyr::filter(pot_net, pot_net$X2 == i)
    Pn1 <- dplyr::filter(pot_net, pot_net$X1 == i)
    if (length(Pn2$X2) != 0){
      pot_in[[i]] <- sort(unique(Pn2$X1))
    }  else {
      pot_in[[i]] <- 0
    }
    if (length(Pn1$X1) != 0){
      pot_out[[i]] <- sort(unique(Pn1$X2))
    }  else {
      pot_out[[i]] <- 0
    }
  }
  pot_all <- mapply(c, pot_in, pot_out, SIMPLIFY=FALSE)
  if (any(lapply(pot_all, function(x) if (length(x) == 1 & 0 %in% x) {1} else {0}) == 1)) warning("Some nodes have no edges in pot_net")
  if (length(pot_all) != length(pot_in)) stop("Length of potential adjacency lists do not match")
  pot_all <- lapply(pot_all, function(x) if (0 %in% x) {x[x > 0]} else {x})
  return(list("adj_in" = adj_in, "adj_out" = adj_out, "adj_all" = adj_all,
              "pot_in" = pot_in, "pot_out" = pot_out, "pot_all" = pot_all))
}



#this is the main internal function that computes the list of pairwise overlap values
#(whose average is Nbar; their standard deviation is modularity).
#LL is the list of neighbors for each node
#LL_pot is the list of neighbors for each node in the potential network
#perc is the fraction of node pairs to be evaluated for overlap
#if sl == 1 remove self loops (cannibalistic interactions)

OV <- function(LL, LL_pot, perc = 1, sl = 1){
  CO <- c()
  for (i in 1:length(LL)){
    for (j in 1:length(LL)){
      if (perc == 1 | ((runif(1)) < perc)){
        if (j > i){
          pot <- intersect(LL_pot[[i]], LL_pot[[j]])
          if (sl == 1){
            pot <- setdiff(pot, c(i,j))
           }
          N <- length(pot)
          rr1 <- intersect(LL[[i]], pot)
          rr2 <- intersect(LL[[j]], pot)
          N1 <- length(rr1)
          N2 <- length(rr2)
          if (min(N1,N2)==0){
            N_ij <- 0
          } else {
            S <- length(intersect(rr1, rr2))
            co <- 0
            for (k in seq(0, (min(N1, N2)))){
              ccc <- cooc(k, N, N1, N2)
              co <- co + (ccc * k)
            }
            lowercase_omega_ij <- (S-co)/min(N1,N2) #equation 1
            if (S > co){
              capital_omega_ij <- (min(N1,N2)-co)/min(N1,N2) #equation 3
            } else if (S < co){
              if ((N1+N2-N) <0 ){
                capital_omega_ij <- co / min(N1,N2) #equation 4
              } else {
                capital_omega_ij <- (co-(N1+N2-N)) / min(N1,N2) #equation 5
              }
            } else {
              capital_omega_ij <- 1
            }
            N_ij <- lowercase_omega_ij / capital_omega_ij #equation 6
          }
          CO <- c(CO,N_ij)
        }
      }
    }
  }
  return(CO)
}
