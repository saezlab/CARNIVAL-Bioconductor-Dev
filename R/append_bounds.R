#'\code{append_bounds}
#'
#' Appending the bounds for each condition
#'
#' Enio Gjerga, 2020

append_bounds = function(bounds = bounds, variables = variables){

  for(ii in 1:length(variables$reaction_variables$explanation)){

    # get source node for y variable
    ss <- strsplit(x =
                     strsplit(
                       x = variables$reaction_variables$explanation[ii],
                       split = " ", fixed = TRUE)[[1]][2], split = "=",
                   fixed = TRUE)[[1]][1]

    # get target node for y variable
    tt <- strsplit(x =
                     strsplit(
                       x = variables$reaction_variables$explanation[ii],
                       split = " ", fixed = TRUE)[[1]][2],
                   split = "=", fixed = TRUE)[[1]][2]

    for(jj in 1:(length(variables)-1)){

      # 0 <= delta^+_(i,t) <= 1
      bounds <- c(bounds,
                  paste0("\t", "0 <= andP_", ss, "_", tt, "_", jj, " <= 1"))

      # 0 <= delta^-_(i,t) <= 1
      bounds <- c(bounds,
                  paste0("\t", "0 <= andM_", ss, "_", tt, "_", jj, " <= 1"))

    }

  }

  return(bounds)

}
