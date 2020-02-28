#'\code{write_constraints_8}
#'
#' This code writes the list of constraints (8) of the ILP problem for all the
#' conditions.
#'
#' x_(j,k) = x^+_(j,k) - x^-_(j,k) + B_(j_k)
#' as
#' x^+_(j,k) - x^-_(j,k) + B_(j_k) - x_(j,k) = 0
#'
#'
#'
#' Enio Gjerga, 2020

write_constraints_8 <- function(variables = variables,
                                inputs = inputs,
                                pknList = pknList,
                                mulT = FALSE){

  constraints8 <- c()

  if (mulT) {
    iterator <- seq_along(variables[-1])
  } else {
    iterator <- 1
  }

  for(ii in iterator){

    ## x^+_(j,k) - x^-_(j,k) + B_(j_k) - x_(j,k) = 0
    # only
    if ((mulT && (ii == 1)) || !mulT)  {
      cc <- paste0(

        # x+_(j,k)
        variables[[ii]]$variables[variables[[ii]]$idxNodesUp],

        " - ",

        # x^-_(j,k)
        variables[[ii]]$variables[variables[[ii]]$idxNodesDown],

        " + ",

        # B_(j,k)
        variables[[ii]]$variables[variables[[ii]]$idxB],

        " - ",

        # x_(j,k)
        variables[[ii]]$variables[variables[[ii]]$idxNodes],

        " = 0")

      constraints8 <- c(constraints8, cc)
    }

    # print(variables)

    if (ii > 1) {
    # if (FALSE) {
      # print(variables)

      # get input species (perturbation nodes)
      kk <- paste0("Species ", colnames(inputs), " in experiment ", ii)
      # # get measurement species (measured nodes)
      # mm <- paste0("Species ", colnames(measurements), " in experiment ", ii)

      # Getting indices of nodes that are neither in inputs nor measurements
      # nodes not %in% input - nodes %in% measurements
      # idxUndetNodes <- setdiff(
      #
      #   # Get indices of not input nodes
      #   which(!(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk)),
      #
      #   # Get indices of measurement nodes
      #   which((variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% mm)))

      idxUndetNodes <- which(!(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk))

      cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxNodes][idxUndetNodes],
                   " - ",
                   variables[[ii]]$variables[variables[[ii]]$idxNodesUp][idxUndetNodes],
                   " <= 0")
      constraints8 <- c(constraints8, cc)

      cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxNodes][idxUndetNodes],
                   " + ",
                   variables[[ii]]$variables[variables[[ii]]$idxNodesDown][idxUndetNodes],
                   " >= 0")
      constraints8 <- c(constraints8, cc)

    }

    ## B_(j,k) = 0 for non input species
    # get input species (perturbation nodes)
    kk <- paste0("Species ", colnames(inputs), " in experiment ", ii)

    # set B_(j,k) = 0 for all non input species
    cc <- paste0(

      # B_(j,k)
      variables[[ii]]$variables[variables[[ii]]$idxB[

        # for all nodes not in input species
        which(!(variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk))]],

        " = 0")

    constraints8 <- c(constraints8, cc)

    ## x_(j,k) = I_(j,k) Setting x to the input
    # get input species (perturbation nodes)
    kk <- paste0("Species ", colnames(inputs), " in experiment ", ii)

    cc = c()

    for(jj in 1:length(kk)){

      # in case mulT there are no perturbations over time in the inputs
      # thus, always take the first first row in the input
      if (mulT) {
        input_row <- 1
      } else {
        input_row <- ii
      }

      # input_row <- ii

      # get species name
      cName = strsplit(x = kk[jj], split = " ")[[1]][2]

      # x_(j,k) - I_(j,k) = 0
      cc = c(cc, paste0(
        # get input node x_(j,k)
        variables[[ii]]$variables[
          which(variables[[ii]]$exp == paste0("Species ",
                                              cName,
                                              " in experiment ",
                                              ii)
                )
          ],

        " = ",

        # get node state from input
        inputs[input_row, jj]))

    }
    constraints8 <- c(constraints8, cc)

    ## x_(j,k) - B_(j,k) = 0 for perturbation nodes
    ## (nodes that are not simultaneous source and target nodes)

    # get nodes that are not simultaneous source and target nodes from PKN
    if (length(setdiff(as.character(pknList[, 1]),
                       as.character(pknList[, 3]))) > 0) {

      # get their names
      kk <- paste0("Species ",
                   setdiff(as.character(pknList[, 1]),
                           as.character(pknList[, 3])),
                   " in experiment ",
                   ii)

      # x_(j,k)
      cc <- paste0(variables[[ii]]$variables[variables[[ii]]$idxNodes[which(
        variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk)]],

        " - ",

        # B_(j,k)
        variables[[ii]]$variables[variables[[ii]]$idxB[which(
          variables[[ii]]$exp[variables[[ii]]$idxNodes] %in% kk)]],

        " = 0")

      constraints8 <- c(constraints8, cc)
    }

  }

  return(constraints8)

}
