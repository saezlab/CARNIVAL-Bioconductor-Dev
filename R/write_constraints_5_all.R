#'\code{write_constraints_5_all}
#'
#' This code writes the list of constraints (5) of the ILP problem for all the
#' conditions.
#'

write_constraints_5_all <- function(variables=variables,
                                    dt = FALSE) {

  constraints5 <- c()

  for(i in 1:length(variables)){

    var <- variables[[i]]

    constraints5 <- c(constraints5, write_constraints_5(variables = var,
                                                        conditionIDX = i))

    # if (dt) {
    #   break
    # }

  }

  return(constraints5)

}
