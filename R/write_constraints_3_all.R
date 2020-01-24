#'\code{write_constraints_3_all}
#'
#'This code writes the list of constraints (3) of the ILP problem for all the
#'conditions.
#'
#' u^+_(i,k) <= 1 - u^-_(i,k)
#' u^+_(i,k) + u^-(i,k) <= 1
#'

write_constraints_3_all <- function(variables=variables, dt = FALSE) {

  ## ======================================= ##
  ## ====== Load write_constraints_3.R ===== ##
  ## ======================================= ##

  write_constraints_3 <- function(variables=variables) {

    constraints3 <- paste0(

      # u^+_(i,k)
      variables$variables[variables$idxEdgesUp],

      " + ",

      # u^-_(i,k)
      variables$variables[variables$idxEdgesDown],

      " <= 1")

    return(constraints3)

  }

  # ======================================= #
  # ======================================= #
  # ======================================= #

  constraints3 <- c()
  # dt <- FALSE

  # Add this constraint only in the first timepoint in case dt
  if (dt) {

    constraints3 <- c(constraints3,
                      write_constraints_3(variables = variables[[1]]))

  } else {

    for(i in 1:length(variables)){

      var <- variables[[i]]

      constraints3 <- c(constraints3, write_constraints_3(variables = var))

    }

  }

  return(constraints3)

}
