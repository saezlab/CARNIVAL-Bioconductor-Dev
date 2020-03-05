#'\code{write_constraints_4}
#'
#' This code writes the list of constraints (4) of the ILP problem for one
#' condition.
#'
#' u^+_(i,k) <= sigma_i * x_(j,k) + u^-_(i,k)
#'
#' u^+_(i,k) - sigma_i * x_(j,k) - u^-_(i,k) <= 0
#'
#' Enio Gjerga, 2020

write_constraints_4 <- function(variables=variables,
                                conditionIDX=conditionIDX) {

  constraints4 <- rep("", length(variables$idxEdgesUp))

  # get sign of reaction (sigma_i)
  idx1 <- which(variables$signs==1)
  idx2 <- which(variables$signs==-1)

  # sigma_i == 1
  constraints4[idx1] <- paste0(

    # u^+_(i,k)
    variables$variables[variables$idxEdgesUp[idx1]],

    " - ",

    # x_(j,k)
    variables$variables[match(
      paste0("Species ",
             unlist(
               strsplit(
                 gsub(
                   gsub(variables$exp[variables$idxEdgesUp[idx1]],
                        pattern = "ReactionUp ",
                        replacement = ""),
                   pattern = paste0(" in experiment ",
                                    conditionIDX),
                   replacement = ""),
                 split = "="))[c(TRUE, FALSE)],
             " in experiment ",
             conditionIDX),
      variables$exp)],

    " - ",

    # u^-_(i,k)
    variables$uTable[match(variables$variables[variables$idxEdgesUp[idx1]],
                           # get associated u^-(i,k) from right side of uTable
                           variables$uTable[, 1]), 2],

    " <= 0")

  # sigma_i == -1
  constraints4[idx2] <- paste0(

    # u^+_(i,k)
    variables$variables[variables$idxEdgesUp[idx2]],

    " + ",

    # x_(j,k)
    variables$variables[match(
      paste0("Species ",
             unlist(
               strsplit(
                 gsub(
                   gsub(variables$exp[variables$idxEdgesUp[idx2]],
                        pattern = "ReactionUp ",
                        replacement = ""),
                   pattern = paste0(" in experiment ",
                                    conditionIDX),
                   replacement = ""),
                 split = "="))[c(TRUE, FALSE)],
             " in experiment ",
             conditionIDX),
      variables$exp)],

    " - ",

    # u^-_(i,k)
    variables$uTable[match(variables$variables[variables$idxEdgesUp[idx2]],
                           # get associated u^-_(i,k) from right side of uTable
                           variables$uTable[, 1]), 2],

    " <= 0")

  return(constraints4)

}
