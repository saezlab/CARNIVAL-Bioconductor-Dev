#'\code{write_constraints_2}
#'
#' This code writes the list of constraints (2) of the ILP problem for one
#' condition.
#'
#' u^-_(i,k) >= sigma_i * x_(j,k)
#'

write_constraints_2 <- function(variables=variables,
                                conditionIDX=conditionIDX){

  constraints1 <- rep("", length(variables$idxEdgesDown))

  # get sign of reaction (sigma_i)
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)

  # create constraints for sigma_i == 1
  # u^-_(i,k) + sigma * x_(j,k) >= 0
  # since sigma == 1:
  # u^-_(i,k) + x_(j,k) >= 0

  constraints1[idx1] <- paste0(

    # u^-_(i,k)
    variables$variables[variables$idxEdgesDown[idx1]],

    " + ",

    # x_(j,k)
    variables$variables[match(
      paste0(
        "Species ",
        unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesDown[idx1]],
                                  pattern = "ReactionDown ",
                                  replacement = ""),
                             pattern = paste0(" in experiment ",
                                              conditionIDX),
                             replacement = ""),
                        split = "="))[c(TRUE, FALSE)],
        " in experiment ", conditionIDX), variables$exp)],

    " >= 0")

  # create constraints for sigma_i == -1
  # u^-_(i,k) + sigma * x_(j,k) >= 0
  # since sigma == -1:
  # u^-_(i,k) - x_(j,k) >= 0

  constraints1[idx2] <- paste0(

    # u^-_(i,k)
    variables$variables[variables$idxEdgesDown[idx2]],

    " - ",

    # x_(j,k)
    variables$variables[match(
      paste0(
        "Species ",
        unlist(strsplit(gsub(gsub(variables$exp[variables$idxEdgesDown[idx2]],
                                  pattern = "ReactionDown ",
                                  replacement = ""),
                             pattern = paste0(" in experiment ",
                                              conditionIDX),
                             replacement = ""),
                        split = "="))[c(TRUE, FALSE)],
        " in experiment ", conditionIDX), variables$exp)],

    " >= 0")

  return(constraints1)

}
