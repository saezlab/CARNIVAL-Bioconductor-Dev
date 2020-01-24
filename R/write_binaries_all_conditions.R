#'\code{write_binaries_all_conditions}
#'
#'@param variables Contains the list of variables as used to formulate the ILP problem, explanations for each variable and a list of useful indices.
#'
#'@return This code writes the list of binary variables (xp, xm, up & um).

write_binaries_all_conditions <- function(variables=variables,
                                          dt = FALSE){

  binaries <- c()

  for(i in 1:length(variables)){

    if(i != length(variables)){

      # Marking CPLEX variables as binary in the lp file

      # x^+_(j,k)
      binaries <- c(binaries, paste0("\t", variables[[i]]$variables[variables[[i]]$idxNodesUp]))

      # x^-_(j,k)
      binaries <- c(binaries, paste0("\t", variables[[i]]$variables[variables[[i]]$idxNodesDown]))


      # u^+_(i,k)
      binaries <- c(binaries, paste0("\t", variables[[i]]$variables[variables[[i]]$idxEdgesUp]))

      # u^-_(i,k)
      binaries <- c(binaries, paste0("\t", variables[[i]]$variables[variables[[i]]$idxEdgesDown]))

      if (!dt) {
        # additional edges in case not dt
        binaries <- c(binaries, paste0("\t", variables[[i]]$variables[variables[[i]]$idxEdges]))
      }

    } else {

      # y_(i,t)
      binaries <- c(binaries, paste0("\t", variables[[i]]$variables))

    }

  }

  return(binaries)

}
