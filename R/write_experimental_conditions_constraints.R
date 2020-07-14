#'\code{write_experimental_conditions_constraints}
#'
#' Writing the additional constraints when considering the multiple experimental
#' conditions.
#'
#'Enio Gjerga, 2020

write_experimental_conditions_constraints <- function(variables = variables,
                                                      mulT = mulT){

  constraint1 = c()
  time_constraint = c()

  n_measurements <- length(variables)-1


  for(ii in 1:(n_measurements)){
    n_up_nodes <- length(variables[[ii]]$idxEdgesUp)

    cc1 <- matrix(nrow = n_up_nodes, ncol = 9)

    # pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = n_up_nodes)
    # pb$message(paste("experimental_conditions: up_edges", ii, "of", n_measurements))

    for(jj in 1:n_up_nodes){
      # pb$tick()

      # current_exp <- variables$reaction_variables$explanation[[jj]]
      # ii <- strsplit(x = current_exp,
      #                split = " ",
      #                fixed = TRUE)[[1]]
      # ii <- as.integer(ii[[length(ii)]])


      # for(ii in 1:(length(variables)-1)){

      # Get nodes for upregulated edge number jj for t == ii
      # nodes[1] == upstream; nodes[2] == downstream
      nodes <- strsplit(x = strsplit(x = variables[[ii]]$exp[variables[[ii]]$idxEdgesUp[jj]],
                                     split = " ",
                                     fixed = TRUE)[[1]][2],
                        split = "=", fixed = TRUE)[[1]]

      # tt <-
      #   strsplit(x = ,
      #            split = "=", fixed = TRUE)[[1]][2]
      #
      # # Get upstream node for upregulated edge number jj for t == ii
      # ss <-
      #   strsplit(x = strsplit(x = variables[[ii]]$exp[variables[[ii]]$idxEdgesUp[jj]],
      #                         split = " ",
      #                         fixed = TRUE)[[1]][2],
      #            split = "=", fixed = TRUE)[[1]][1]

      # Get CPLEX variables for u^(+)_(i,k) and x^(+)_(i,k)
      var_u_p <- variables[[ii]]$variables[variables[[ii]]$idxEdgesUp[jj]]
      var_x_p <- variables[[ii]]$variables[
        which(variables[[ii]]$exp==paste0("SpeciesUP ",
                                          nodes[2],
                                          " in experiment ",
                                          ii))
        ]

      # CPLEX variable names for delta_(i, k)
      delta_plus <- paste0("andP_", nodes[1], "_", nodes[2], "_", ii)
      delta_minus <- paste0("andM_", nodes[1], "_", nodes[2], "_", ii)

      # + delta_(i,k) - u_(i,k) - x_(i,k) >= -1
      cc1[ii, 1] = paste0(delta_plus, " - ",
                             var_u_p, " - ", var_x_p, " >= -1")

      # + delta_(i,k) - u_(i,k) <= 0
      cc1[ii, 2] = paste0(delta_plus, " - ", var_u_p, " <= 0")

      # + delta_(i,k) - x_(i,k) <= 0
      cc1[ii, 3] = paste0(delta_plus, " - ", var_x_p, " <= 0")

      # Get CPLEX variables for u^(-)_(i,k) and x^(-)_(i,k)
      var_u_m <- variables[[ii]]$variables[variables[[ii]]$idxEdgesDown[jj]]
      var_x_m <- variables[[ii]]$variables[
        which(variables[[ii]]$exp == paste0("SpeciesDown ",
                                            nodes[2],
                                            " in experiment ",
                                            ii))
        ]

      # - delta_(i,k) - u_(i,k) - x_(i,k) >= -1
      cc1[ii, 4] = paste0(delta_minus, " - ",
                             var_u_m, " - ", var_x_m, " >= -1")

      # - delta_(i,k) - u_(i,k) <= 0
      cc1[ii, 5] = paste0(delta_minus, " - ", var_u_m, " <= 0")

      # - delta_(i,k) - x_(i,k) <= 0
      cc1[ii, 6] = paste0(delta_minus, " - ", var_x_m, " <= 0")

      # constraint1 = c(constraint1, c(c1, c2, c3, c4, c5, c6))

      if (mulT) {
        # get current reaction variable (y_(i,t))
        y_it <- variables$reaction_variables$variables[
          which(variables$reaction_variables$explanation ==
                  paste0("Reaction ", nodes[1], "=", nodes[2], " in experiment ", ii))
          ]

        #
        cc1[ii, 7] <- paste0(y_it, " - ", delta_plus, " >= 0")
        cc1[ii, 8] <- paste0(y_it, " - ", delta_minus, " >= 0")

        # All reactions in previous time point have to be included in current
        # y_(i,t) >= y_(i,t-1) for all t > 1
        # expressed as y_(i,t) - y_(i,t-1) >= 0 for all t > 1
        if (ii > 1) {
          y_it_before <- variables$reaction_variables$variables[
            which(variables$reaction_variables$explanation ==
                    paste0("Reaction ", nodes[1], "=", nodes[2], " in experiment ", ii - 1))
            ]

          # y_(i,t) - y_(i,t-1) >= 0
          cc1[ii, 9] <- paste0(y_it, " - ", y_it_before, " >= 0")

          # Adding constraints
          # time_constraint <- c(time_constraint, c(c7, c8, c9))

        # } else {
          # time_constraint <- c(time_constraint, c(c7, c8))
        }
      }
    }
  }

  constraint1 <- as.character(cc1[!is.na(cc1)])

  if (mulT) {
    constraints <- constraint1
  } else {
    constraint2 = c()
    for(ii in 1:(length(variables)-1)){
      for(jj in 1:length(variables[[ii]]$idxEdgesUp)){

          # Get downstream node for upregulated edge number jj for t == ii
          ss <-
            strsplit(x = strsplit(x = variables[[ii]]$exp[variables[[ii]]$idxEdges[jj]],
                                  split = " ",
                                  fixed = TRUE)[[1]][2],
                     split = "=", fixed = TRUE)[[1]][1]

          # Get upstream node for upregulated edge number jj for t == ii
          tt <-
            strsplit(x = strsplit(x = variables[[ii]]$exp[variables[[ii]]$idxEdges[jj]],
                                  split = " ",
                                  fixed = TRUE)[[1]][2],
                     split = "=", fixed = TRUE)[[1]][2]

          c1 <- paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]],
                       " - andP_", ss, "_", tt, "_", ii,
                       " - andM_", ss, "_", tt, "_", ii,
                       " <= 0")

          c2 <- paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]],
                       " - andP_", ss, "_", tt, "_", ii, " >= 0")

          c3 <- paste0(variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]],
                       " - andM_", ss, "_", tt, "_", ii, " >= 0")

          constraint2 = c(constraint2, c(c1, c2, c3))



      }

    }


    constraint3 = c()
    for(jj in 1:length(variables$reaction_variables$explanation)){

      c1 = variables$reaction_variables$variables[jj]
      c2 = c()

      for(ii in 1:(length(variables)-1)){

        c1 = paste0(c1, " - ",
                    variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]])
        c2 = c(c2, paste0(variables$reaction_variables$variables[jj], " - ",
                          variables[[ii]]$variables[variables[[ii]]$idxEdges[jj]],
                          " >= ", 0))

      }

      c1 <- paste0(c1, " <= 0")
      constraint3 <- c(constraint3, c(c1, c2))

    }

    constraints = c(constraint1, constraint2, constraint3)
  }

  return(constraints)
}
