
#' @export

print.summary.NOSM <- function(x, ...){
  if (attributes(x)$Type == "Pot_dir") cat("Data = Directed network; network of potential interactions provided by user", "\n",
                                           "Z-tuning parameter =", attributes(x)$z_tune, "\n")
  if (attributes(x)$Type == "Pot_undir") cat("Data = Undirected network; network of potential interactions provided by user", "\n",
                                             "Z-tuning parameter =", attributes(x)$z_tune, "\n")
  if (attributes(x)$Type == "bip") cat("Data = Directed bipartite network; network of potential interactions NOT provided by user", "\n",
                                       "Z-tuning parameter =", attributes(x)$z_tune, "\n")
  if (attributes(x)$Type == "Undir") cat("Data = Undirected network; network of potential interactions NOT provided by user", "\n",
                                         "Z-tuning parameter =", attributes(x)$z_tune, "\n")
  if (attributes(x)$Type == "Dir") cat("Data = Directed unimode network; network of potential interactions NOT provided by user", "\n",
                                       "Z-tuning parameter =", attributes(x)$z_tune, "\n")
  cat("\n")
  if (attributes(x)$Type == "Pot_dir" || attributes(x)$Type == "bip" || attributes(x)$Type == "Dir"){
    cat("NOS_In =", x[1], "\n")
    cat("NOS_Out =", x[2], "\n")
    cat("NOS =", x[3], "\n") ##giovanni's check
    cat("MOD_In =", x[4], "\n")
    cat("MOD_Out =", x[5], "\n")
    cat("MOD =", x[6], "\n")
    cat("\n")
    cat("z =", x[7], "\n")
    cat("P =", x[8])
  }
  if (attributes(x)$Type == "Pot_undir" || attributes(x)$Type == "Undir"){
    cat("NOS =", x[1], "\n")
    cat("MOD =", x[2], "\n")
    cat("\n")
    cat("z =", x[3], "\n")
    cat("P =", x[4])
  }
}
