#'\code{write_constraints_5}
#'
#' This code writes the list of constraints (5) of the ILP problem for one
#' condition.
#'
#' u^-_(i,k) <= sigma_i * x_(j,k) + u^-_(i,k)
#'
#' u^-_(i,k) - sigma_i * x_(j,k) - u^-_(i,k) <= 0
#'

write_constraints_5 <- function(variables=variables,
                                conditionIDX=conditionIDX) {

  constraints1 <- rep("", length(variables$idxEdgesDown))

  # get sign of reaction (sigma_i)
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)

  # sigma_i == 1
  constraints1[idx1] <- paste0(

    # u^-_(i,k)
    variables$variables[variables$idxEdgesDown[idx1]],

    " + ",

    # x_(j,k)
    variables$variables[match(
      paste0("Species ",
             unlist(
               strsplit(
                 gsub(
                   gsub(variables$exp[variables$idxEdgesDown[idx1]],
                        pattern = "ReactionDown ",
                        replacement = ""),
                   pattern = paste0(
                     " in experiment ",
                     conditionIDX),
                   replacement = ""),
                 split = "="))[c(TRUE, FALSE)],
             " in experiment ",
             conditionIDX),
      variables$exp)],

    " - ",

    # u^+_(i,k)
    variables$uTable[match(variables$variables[variables$idxEdgesDown[idx1]],
                           # get associated u^+_(i,k) from left side of uTable
                           variables$uTable[, 2]), 1], " <= 0")


  # sigma_i == -1
  constraints1[idx2] <- paste0(

    # u^-(i,k)
    variables$variables[variables$idxEdgesDown[idx2]],

    " - ",

    # x_(j,k)
    variables$variables[match(
      paste0("Species ",
             unlist(
               strsplit(
                 gsub(
                   gsub(variables$exp[variables$idxEdgesDown[idx2]],
                        pattern = "ReactionDown ",
                        replacement = ""),
                   pattern = paste0(" in experiment ",
                                    conditionIDX),
                   replacement = ""),
                 split = "="))[c(TRUE, FALSE)],
             " in experiment ",
        conditionIDX),
      variables$exp)],

    " - ",

    # u^+_(i,k)
    variables$uTable[match(variables$variables[variables$idxEdgesDown[idx2]],
                           # get associated u^+_(i,k) from right side of uTable
                           variables$uTable[, 2]), 1], " <= 0")

  return(constraints1)

}
