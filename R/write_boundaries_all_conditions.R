#'\code{write_boundaries}
#'
#'
#' This code writes the boundaries of each variable.
#'
#' Enio Gjerga, 2020

write_boundaries_all_conditions <- function(variables=variables, oF=oF){

  M <- 100

  bounds <- c()

  for(i in 1:length(variables)){

    if(i != length(variables)){

      # -1 <= x_(j,k) <= 1
      bounds <- c(bounds,
                  paste0("\t",
                    "-1 <= ",
                    variables[[i]]$variables[variables[[i]]$idxNodes],
                     " <= 1"))

      # 0 <= x^+_(j,k) <= 1
      bounds <- c(bounds,
                  paste0("\t",
                    "0 <= ",
                    variables[[i]]$variables[variables[[i]]$idxNodesUp],
                    " <= 1"))

      # 0 <= x^-_(j,k) <= 1
      bounds <- c(bounds,
                  paste0("\t",
                    "0 <= ",
                    variables[[i]]$variables[variables[[i]]$idxNodesDown],
                    " <= 1"))

      # 0 <= u^+_(i,k) <= 1
      bounds <- c(bounds,
                  paste0("\t",
                    "0 <= ",
                    variables[[i]]$variables[variables[[i]]$idxEdgesUp],
                    " <= 1"))

      # 0 <= u^-_(i,k) <= 1
      bounds <- c(bounds,
                  paste0("\t",
                         "0 <= ",
                         variables[[i]]$variables[variables[[i]]$idxEdgesDown],
                         " <= 1"))

      # -1 <= B_(j,k) <= 1
      bounds <- c(bounds,
                  paste0("\t",
                         "-1 <= ",
                         variables[[i]]$variables[variables[[i]]$idxB],
                         " <= 1"))

      # 0 <= d_j <= M
      bounds <- c(bounds,
                  paste0("\t",
                         "0 <= ",
                         variables[[i]]$variables[variables[[i]]$idxDist],
                         " <= ", M))

      # 0 <= A_(j,k) <= 2
      bounds <- c(bounds,
                  paste0("\t",
                         "0 <= ",
                         unique(
                           strsplit(
                             oF,
                             split = " ")[[1]][grep(
                               pattern = "absDiff",
                               x = strsplit(oF, split = " ")[[1]])]),
                         " <= 2"))

      # Redundant 0 <= d_j <= M
      # bounds <- c(bounds,
      #             paste0("\t",
      #                    "0 <= ",
      #                    variables[[i]]$variables[variables[[i]]$idxDist],
      #                    " <= ", M))

      # Disabled not needed at the moment
      # Was used for additional variables for each node in multi cond CARNIVAL
      # bounds <- c(bounds,
      #             paste0("\t",
      #                    "0 <= ",
      #                    variables[[i]]$variables[variables[[i]]$idxEdges],
      #                    " <= 1"))

    } else {

      # 0 <= y_(i,t) <= 1
      bounds <- c(bounds, paste0("\t", "0 <= ",
                                 variables[[i]]$variables, " <= 1"))

    }

  }

  return(bounds)

}
