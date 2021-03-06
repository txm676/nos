% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pot_dir.R
\name{NOSM_POT_dir}
\alias{NOSM_POT_dir}
\title{Compute NOS using a directed network and with a user provided network
of potential interactions}
\usage{
NOSM_POT_dir(net, pot_net, perc = 1, sl = 1)
}
\arguments{
\item{net}{A network, in the form of an edge list. This should be a matrix or
dataframe with two columns. Each value in a column is a node. Nodes can be
identified using numbers or characters.Data can also be in the format of a
frequency interaction matrix, as used in the \link[bipartite]{bipartite} R
package. In these cases \code{\link{freqMat_2_edge}} should be used first,
to convert the interaction matrix to an edge list.}

\item{pot_net}{A network of all potential interactions. These should include,
as a minimum, all the observed interactions (i.e. all links in net),plus
any other possible interaction (such as all those permitted by a certain
trophic rule). pot_net should have the same structure as net (e.g. it
should be a data frame or matrix).}

\item{perc}{(default to 1) - the fraction of node pair comparisons to be
performed to compute NOS. We recommend performing all possible pair
comparisons (perc = 1). However, for exploratory analyses on large sets of
networks (or for very large networks), the possibility of using a lower
fraction of pair comparisons is a useful option.}

\item{sl}{(default is 1) Specifies whether cannibalistic interactions should
be considered as possible and therefore taken into account and removed
during computation ('1') or not ('0').}
}
\value{
A list (two elements) of class 'NOSM' with a 'Type' attribute
  'Pot_dir'. The first element in the list is a vector of overlap values for
  the "in nodes" and the second element is a vector of overlap values for the
  "out nodes".

  The \code{\link{summary.NOSM}} methods provides more useful summary
  statistics.
}
\description{
Computation of NOS using an directed network (e.g. a
  food web) and with a user provided network of potential interactions.
}
\examples{
data(boreal)
y <-  boreal[1:300,] #subset 300 rows for speed
d <- sample(nrow(y), 200, replace = FALSE) #create a random pot_net
pot_net <- y[d,] #by randomly sampling 200 rows from boreal
x <- NOSM_POT_dir(y, pot_net, perc = 1, sl = 1)
summary(x)
}
