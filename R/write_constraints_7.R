#'\code{write_constraints_7}
#'
#' This code writes the list of constraints (7) of the ILP problem for all the
#' conditions.
#'
#' x^-_(j,k) <= sum_all_incoming(u^-_(i,k))
#'
#' x^-_(j,k) - (sum_all_incoming(u^-_(i,k))) <= 0
#'
#' Enio Gjerga, 2020

write_constraints_7 <- function(variables=variables,
                                pknList=pknList) {

  library(igraph)
  constraints7 <- c()

  for(ii in 1:length(variables)){

    # Redundant
    # source <- unique(variables[[ii]]$reactionSource)
    # target <- unique(variables[[ii]]$reactionTarget)

    gg <- graph_from_data_frame(d = pknList[, c(3, 1)])
    adj <- get.adjacency(gg)
    adj <- as.matrix(adj)

    idx1 <- which(rowSums(adj)==0)
    idx2 <- setdiff(1:nrow(adj), idx1)

    if (length(idx1)>0) {

      # Find all x^-_(i,k) with no incoming u^+_(i,k) from upstream
      # Constrain their x^-_(i,k) <= 0
      constraints7 <- c(constraints7,
                        paste0(variables[[ii]]$variables[which(
                          variables[[ii]]$exp %in%
                            paste0("SpeciesDown ",
                                   rownames(adj)[idx1],
                                   " in experiment ", ii))],
                          " <= 0"))
    }

    # x^-_(j,k) - (sum_all_incoming(u^-_(i,k))) <= 0
    for(i in 1:length(idx2)){

      cc <- paste0(

        # x^-_(j,k)
        variables[[ii]]$variables[which(variables[[ii]]$exp ==
                                          paste0("SpeciesDown ",
                                                 rownames(adj)[idx2[i]],
                                                 " in experiment ",
                                                 ii))],

        # subtract all incoming u^-_(i,k) for current x^-_(j,k)
        paste(
          paste0(" - ",

                 # get all incoming u^-_(i,k)
                 variables[[ii]]$variables[which(
                   variables[[ii]]$exp %in%
                     paste0("ReactionDown ",
                            colnames(adj)[which(adj[idx2[i], ] > 0)],
                            "=",
                            rownames(adj)[idx2[i]],
                            " in experiment ",
                            ii)
                 )]
          ),
          collapse = ""),

        " <= 0")

      constraints7 <- c(constraints7, cc)

    }

  }

  return(constraints7)
}
