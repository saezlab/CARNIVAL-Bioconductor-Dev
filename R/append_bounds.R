#'\code{append_bounds}
#'
#' Appending the bounds for each condition
#'
#' Enio Gjerga, 2020


append_bounds = function(bounds = bounds, variables = variables){


  n_nodes <- length(variables$reaction_variables$explanation)
  n_measurements <- length(variables)-1

  # pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = n_nodes)
  # pb$message("append_bounds")

  temp_bounds = matrix(nrow = n_nodes, ncol = n_measurements)

  for(ii in 1:n_nodes){

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

    for(jj in 1:(n_measurements)){

      # 0 <= delta^+_(i,t) <= 1
      temp_bounds[ii, jj] <- paste0("\t", "0 <= andP_", ss, "_", tt, "_", jj, " <= 1")

      # 0 <= delta^-_(i,t) <= 1
      temp_bounds[ii, jj] <- paste0("\t", "0 <= andM_", ss, "_", tt, "_", jj, " <= 1")

    }
    # pb$tick()

  }

  bounds <- c(bounds, as.character(temp_bounds))
  return(bounds)

}
