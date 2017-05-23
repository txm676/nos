#' nos: A package for calculating node overlap and segregation in ecological
#'   networks
#'
#' @name nos-package
#' @docType package

#'
#' @description A package to calculate NOS (node overlap and segregation) and
#'   the associated metrics described in Strona and Veech (2015) and Strona et
#'   al. (2017). The functions provided in the package enable assessment of
#'   structural patterns ranging from complete node segregation to perfect
#'   nestedness in a variety of network types. In addition, they provide a
#'   measure of network modularity.
#'
#' @details There are five main S3 generics that the user can choose depending
#'   on whether a directed, bipartite or undirected network is provided as
#'   input, and whether a network of potential interactions (pot_net) is
#'   available from the user. These five functions are: 1) NOSM_dir - for
#'   directed unimode networks (e.g. food webs) when pot_net is not provided, 2)
#'   NOSM_undir - for undirected networks (e.g. co-occurrence networks) when
#'   pot_net is not provided, 3) NOSM_bip - for bipartite networks (e.g.
#'   plant-pollinator networks), 4) NOSM_pot_dir - for directed unimode networks
#'   (e.g. food webs) when pot_net is provided, and 5) NOSM_pot_undir - for
#'   undirected networks (e.g. co-occurrence networks) when pot_net is provided.
#'
#'  Each of these five main functions produces an output with class 'NOSM'. An
#'  S3 method (summary.NOSM) provides the summary statistics of interest (e.g..
#'  NOS, Mod/network modularity, effect size Z, and p-value).
#'
#'  If a network of potential interactions is not provided, the computation will
#'  be made according to the following criteria: in an undirected network, all
#'  nodes will be considered as potential interacting partners; in a directed,
#'  unimode network, all nodes having at least an in-coming link will be
#'  considered as potential partners for nodes having at least an out-going link
#'  and vice-versa; in a bipartite network, where nodes can be formally
#'  categorized into two distinct categories (e.g. plant-pollinators) all nodes
#'  in one category will be considered as potential partners for the nodes in
#'  the other category (and vice-versa).
#'
#'  The internal function comb calculates factorials. If the user provided
#'  n or k are too large (> roughly 170) factorial(n || k) produces Inf.
#'  In these cases gmp::factorialZ is used instead. See the help documentation
#'  for the 'gmp' package for further information.
#'
#' @author Thomas J. Matthews and Giovanni Strona
#' @references Strona, G., Matthews, T.J., Kortsch, S. and Veech, J.A. (2017)
#'   NOS: A software suite to compute node overlap and segregation in
#'   ecological networks. Ecography. In review.
NULL


