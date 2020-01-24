#'\code{write_constraints_1_all}
#'
#' This code writes the list of constraints (1) of the ILP problem for all the
#' conditions.
#'
#' u^+_(i,k) >= sigma_i * x_(j,k)
#'

write_constraints_1_all <- function(variables=variables){

  constraints1 <- c()

  # for each experiment / time
  for(i in 1:length(variables)){

    var <- variables[[i]]

    # create constraint 1
    constraints1 <- c(constraints1, write_constraints_1(variables = var,
                                                        conditionIDX = i))

  }

  return(constraints1)

}
