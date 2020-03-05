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

  for(ii in 1:(length(variables)-1)){
    for(jj in 1:length(variables[[ii]]$idxEdgesUp)){
      # current_exp <- variables$reaction_variables$explanation[[jj]]
      # ii <- strsplit(x = current_exp,
      #                split = " ",
      #                fixed = TRUE)[[1]]
      # ii <- as.integer(ii[[length(ii)]])


      # for(ii in 1:(length(variables)-1)){

      # Get downstream node for upregulated edge number jj for t == ii
      tt <-
        strsplit(x = strsplit(x = variables[[ii]]$exp[variables[[ii]]$idxEdgesUp[jj]],
                              split = " ",
                              fixed = TRUE)[[1]][2],
                 split = "=", fixed = TRUE)[[1]][2]

      # Get upstream node for upregulated edge number jj for t == ii
      ss <-
        strsplit(x = strsplit(x = variables[[ii]]$exp[variables[[ii]]$idxEdgesUp[jj]],
                              split = " ",
                              fixed = TRUE)[[1]][2],
                 split = "=", fixed = TRUE)[[1]][1]

      # Get CPLEX variables for u^(+)_(i,k) and x^(+)_(i,k)
      var_u_p <- variables[[ii]]$variables[variables[[ii]]$idxEdgesUp[jj]]
      var_x_p <- variables[[ii]]$variables[
        which(variables[[ii]]$exp==paste0("SpeciesUP ",
                                          tt,
                                          " in experiment ",
                                          ii))
        ]

      # CPLEX variable names for delta_(i, k)
      delta_plus <- paste0("andP_", ss, "_", tt, "_", ii)
      delta_minus <- paste0("andM_", ss, "_", tt, "_", ii)

      # + delta_(i,k) - u_(i,k) - x_(i,k) >= -1
      c1 = paste0(delta_plus, " - ",
                  var_u_p, " - ", var_x_p, " >= -1")

      # + delta_(i,k) - u_(i,k) <= 0
      c2 = paste0(delta_plus, " - ", var_u_p, " <= 0")

      # + delta_(i,k) - x_(i,k) <= 0
      c3 = paste0(delta_plus, " - ", var_x_p, " <= 0")

      # Get CPLEX variables for u^(-)_(i,k) and x^(-)_(i,k)
      var_u_m <- variables[[ii]]$variables[variables[[ii]]$idxEdgesDown[jj]]
      var_x_m <- variables[[ii]]$variables[
        which(variables[[ii]]$exp == paste0("SpeciesDown ",
                                            tt,
                                            " in experiment ",
                                            ii))
        ]

      # - delta_(i,k) - u_(i,k) - x_(i,k) >= -1
      c4 = paste0(delta_minus, " - ",
                  var_u_m, " - ", var_x_m, " >= -1")

      # - delta_(i,k) - u_(i,k) <= 0
      c5 = paste0(delta_minus, " - ", var_u_m, " <= 0")

      # - delta_(i,k) - x_(i,k) <= 0
      c6 = paste0(delta_minus, " - ", var_x_m, " <= 0")

      constraint1 = c(constraint1, c(c1, c2, c3, c4, c5, c6))

      if (mulT) {
        # get current reaction variable (y_(i,t))
        y_it <- variables$reaction_variables$variables[
          which(variables$reaction_variables$explanation ==
                  paste0("Reaction ", ss, "=", tt, " in experiment ", ii))
          ]

        #
        c7 <- paste0(y_it, " - ", delta_plus, " >= 0")
        c8 <- paste0(y_it, " - ", delta_minus, " >= 0")

        # All reactions in previous time point have to be included in current
        # y_(i,t) >= y_(i,t-1) for all t > 1
        # expressed as y_(i,t) - y_(i,t-1) >= 0 for all t > 1
        if (ii > 1) {
          y_it_before <- variables$reaction_variables$variables[
            which(variables$reaction_variables$explanation ==
                    paste0("Reaction ", ss, "=", tt, " in experiment ", ii - 1))
            ]

          # y_(i,t) - y_(i,t-1) >= 0
          c9 <- paste0(y_it, " - ", y_it_before, " >= 0")

          # Adding constraints
          time_constraint <- c(time_constraint, c(c7, c8, c9))

        } else {
          time_constraint <- c(time_constraint, c(c7, c8))
        }
      }
    }
  }

  if (mulT) {
    constraints <- c(constraint1, time_constraint)
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
