#'\code{append_binaries}
#'
#' Appending the binaries for each condition
#' Marking delta variables as binary
#'
#' Enio Gjerga, 2020

append_binaries = function(binaries = binaries, variables = variables){

  n_nodes <- length(variables$reaction_variables$explanation)
  n_measurements <- length(variables)-1

  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = n_nodes)
  pb$message("append_binaries")

  temp_binaries = matrix(nrow = n_nodes, ncol = n_measurements)

  for(ii in 1:n_nodes){

    # get source node for y variable
    ss <- strsplit(x =
                     strsplit(
                       x = variables$reaction_variables$explanation[ii],
                       split = " ", fixed = TRUE)[[1]][2],
                   split = "=", fixed = TRUE)[[1]][1]

    # get target node for y variable
    tt <- strsplit(x =
                     strsplit(
                       x = variables$reaction_variables$explanation[ii],
                       split = " ", fixed = TRUE)[[1]][2],
                   split = "=", fixed = TRUE)[[1]][2]


    for(jj in 1:(n_measurements)){

      # delta^+_(i,t)
      temp_binaries[ii, jj] <- paste0("\tandP_", ss, "_", tt, "_", jj)

      # delta^-_(i,t)
      temp_binaries[ii, jj] <- paste0("\tandM_", ss, "_", tt, "_", jj)

    }
    pb$tick()

  }

  binaries <- c(binaries, as.character(temp_binaries))
  return(binaries)

}
