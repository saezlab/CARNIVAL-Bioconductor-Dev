#'\code{append_binaries}
#'
#' Appending the binaries for each condition
#' Marking delta variables as binary
#'

append_binaries = function(binaries = binaries, variables = variables){

  for(ii in 1:length(variables$reaction_variables$explanation)){

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

    for(jj in 1:(length(variables)-1)){

      # delta^+_(i,t)
      binaries <- c(binaries, paste0("\tandP_", ss, "_", tt, "_", jj))

      # delta^-_(i,t)
      binaries <- c(binaries, paste0("\tandM_", ss, "_", tt, "_", jj))

    }

  }

  return(binaries)

}
