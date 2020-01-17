#'\code{append_general_variables}
#'
#' Appending the general variables for each condition
#'

append_general_variables <- function(variables = variables,
                                     dt = dt){

  for(ii in 1:length(variables)){

    cnt <-
      length(variables[[ii]]$idxNodes) +
      length(variables[[ii]]$idxNodesUp) +
      length(variables[[ii]]$idxNodesDown) +
      length(variables[[ii]]$idxEdgesUp) +
      length(variables[[ii]]$idxEdgesDown) + 1

    # Add an additional CPLEX variable for all EdgesUp
    var2append <- paste0("xb",
                         cnt:(cnt+length(variables[[ii]]$idxEdgesUp)-1),
                         "_", ii)
    # Explanations for the newly created variables
    exp2append <- gsub(pattern = "ReactionUp ",
                       replacement = "Reaction ",
                       x = variables[[ii]]$exp[variables[[ii]]$idxEdgesUp],
                       " in experiment ",
                       ii)

    # Get indices of the newly created variables in the large variables field
    # and write the indices to the new field idxEdges
    idxEdges <-
      (length(variables[[ii]]$variables) + 1):(length(variables[[ii]]$variables) +
                                                 length(var2append))
    namesVar <- names(variables[[ii]])
    variables[[ii]]$variables <- c(variables[[ii]]$variables, var2append)
    variables[[ii]]$exp <- c(variables[[ii]]$exp, exp2append)

    variables[[ii]][[length(variables[[ii]])+1]] <- idxEdges

    names(variables[[ii]]) <- c(namesVar, "idxEdges")

  }

  var = variables

  # append a field for reaction variables
  var[[length(var)+1]] <- list()

  if(dt){

    y_cplex <- list()
    exp_y <- list()

    for(ii in 1:length(variables)){
      # create y reaction variables from number of reactions
      y_cplex <- list(y_cplex,
                      paste0("y",
                             1:length(variables[[ii]]$idxEdgesUp),
                             "_",
                             ii))

      # add explanation to y reaction variables
      exp_y <- list(exp_y,
                    gsub(pattern = "ReactionUp ",
                         replacement = "Reaction ",
                         x = variables[[ii]]$exp[variables[[ii]]$idxEdgesUp]))
    }
    var[[length(var)]][[length(var[[length(var)]]) + 1]] <- unlist(y_cplex)
    var[[length(var)]][[length(var[[length(var)]]) + 1]] <- unlist(exp_y)

  } else {
    # create y reaction variables from number of reactions
    var[[length(var)]][[length(var[[length(var)]])+1]] <-
      paste0("y", 1:length(variables$Condition_1$idxEdgesUp))

    # add explanation to y reaction variables
    var[[length(var)]][[length(var[[length(var)]])+1]] <-
      gsub(pattern = " in experiment 1", replacement = "",
           x = gsub(
             pattern = "ReactionUp ", replacement = "Reaction ",
             x = variables$Condition_1$exp[variables$Condition_1$idxEdgesUp]))
  }

  # Label newly created fields
  names(var) <- c(names(variables), "reaction_variables")
  names(var$reaction_variables) <- c("variables", "explanation")

  return(var)

}
