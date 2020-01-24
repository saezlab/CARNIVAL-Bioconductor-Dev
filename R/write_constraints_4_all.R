#'\code{write_constraints_4_all}
#'
#' This code writes the list of constraints (4) of the ILP problem for all the
#' conditions.
#'

write_constraints_4_all <- function(variables=variables,
                                    dt = FALSE) {

  constraints4 <- c()

  for(i in 1:length(variables)){

    var <- variables[[i]]

    constraints4 <- c(constraints4, write_constraints_4(variables = var,
                                                        conditionIDX = i))

    # if (dt) {
    #   break
    # }

  }

  return(constraints4)

}
