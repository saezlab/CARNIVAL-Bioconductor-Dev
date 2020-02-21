#'\code{create_variables_all}
#'
#'This function returns the identifiers of all the variables used in the ILP
#'formulation together with an explanation about the meaning of each of them.
#'Also it returns a list of useful identifiers.
#'
#'Enio Gjerga, 2020

create_variables_all <- function(pknList=pknList, dataMatrix=dataMatrix){

  ## ======================================= ##
  ## ======= Load create_variables.R ======= ##
  ## ======================================= ##

  create_variables <- function(pknList=pknList, dataMatrix = dataMatrix,
                               conditionIDX=conditionIDX){

    colnames(pknList) <- c("X1", "X2", "X3")

    # CPLEX variables for each species in PKN
    # x_(j,k)
    nodes <- paste0("xb", 1:(nrow(dataMatrix$dataMatrix)*
                               ncol(dataMatrix$dataMatrix)),
                    "_", conditionIDX)

    # CPLEX variable for potential of each PKN species to be upregulated
    # x^+_(j,k)
    nodesUp <- paste0("xb", (nrow(dataMatrix$dataMatrix)*
                               ncol(dataMatrix$dataMatrix)+1):
                        (2*nrow(dataMatrix$dataMatrix)*
                           ncol(dataMatrix$dataMatrix)),
                      "_", conditionIDX)

    # CPLEX variable for potential of each PKN species to be upregulated
    # x^+_(j,k)
    nodesDown <- paste0("xb", (2*nrow(dataMatrix$dataMatrix)*
                                 ncol(dataMatrix$dataMatrix)+1):
                          (3*nrow(dataMatrix$dataMatrix)*
                             ncol(dataMatrix$dataMatrix)),
                        "_", conditionIDX)

    # Verbose & reduced explanations for the CPLEX variables
    expNodes <- c()
    expNodesUp <- c()
    expNodesDown <- c()
    expNodesReduced <- c()
    expNodesReducedUpSource <- c()
    expNodesReducedDownSource <- c()
    expNodesReducedUpTarget <- c()
    expNodesReducedDownTarget <- c()
    idxExperimentNodes <- c()
    for(i in 1:nrow(dataMatrix$dataMatrix)){

      # Names according to column names of dataMatrix for nodes
      expNodes <- c(expNodes, paste0("Species ", dataMatrix$species,
                                     " in experiment ", conditionIDX))

      # Short names according to column names of dataMatrix for nodes
      expNodesReduced = c(expNodesReduced, paste0("Species ",
                                                  dataMatrix$species))

      # Names according to column names of dataMatrix for nodesUp
      expNodesUp <- c(expNodesUp, paste0("SpeciesUP ", dataMatrix$species,
                                         " in experiment ", conditionIDX))

      # Names according to column names of dataMatrix for nodesDown
      expNodesDown <- c(expNodesDown, paste0("SpeciesDown ", dataMatrix$species,
                                             " in experiment ", conditionIDX))

      # Names for upregulating reaction's source according to pknList
      expNodesReducedUpSource <- c(expNodesReducedUpSource,
                                   as.character(pknList$X1))

      # Names for downregulating reaction's source according to pknList
      expNodesReducedDownSource <- c(expNodesReducedDownSource,
                                     as.character(pknList$X1))

      # Names for upregulating reaction's target according to pknList
      expNodesReducedUpTarget <- c(expNodesReducedUpTarget,
                                   as.character(pknList$X3))

      # Names for downregulating reaction's target according to pknList
      expNodesReducedDownTarget <- c(expNodesReducedDownTarget,
                                     as.character(pknList$X3))

      # Number of species
      idxExperimentNodes <- c(idxExperimentNodes, length(expNodes))

    }

    # Concatenate all CPLEX variables for the species
    nodesALL <- c(nodes, nodesUp, nodesDown)

    # Concatenate all associated explanations
    expNodesALL <- c(expNodes, expNodesUp, expNodesDown)

    # Get indices of the individual CPLEX variables in nodesAll
    # Indices correspond to CPLEX variable name: xbINDEX_k
    # for nodes x_(j,k)
    idxNodes <- 1:(nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))

    # for node potential to be upregulated x^+_(j,k)
    idxNodesUp <- (nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):
      (2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))

    # for node potential to be downregulate x^-_(j,k)
    idxNodesDown <- (2*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix)+1):
      (3*nrow(dataMatrix$dataMatrix)*ncol(dataMatrix$dataMatrix))

    # CPLEX variables for each reaction's potential for up/downregulation
    # u^+_(i,k)
    edgesUp <- paste0("xb", (length(nodesALL)+1):(length(nodesALL)+
                                                    nrow(pknList)*
                                                    nrow(dataMatrix$dataMatrix)),
                      "_", conditionIDX)
    # u^-_(i,k)
    edgesDown <- paste0("xb", (length(nodesALL)+
                                 nrow(pknList)*nrow(dataMatrix$dataMatrix)+1):
                          (length(nodesALL)+nrow(pknList)*
                             nrow(dataMatrix$dataMatrix)+nrow(pknList)*
                             nrow(dataMatrix$dataMatrix)), "_", conditionIDX)

    # Verbose & reduced explanations for the CPLEX variables
    expEdgesUp <- c()
    expEdgesDown <- c()
    expEdgesReducedSource <- c()
    expEdgesReducedTarget <- c()
    idxExperimentEdges <- c()
    expNodesReducedUp <- c()
    expNodesReducedDown <- c()
    for(i in 1:nrow(dataMatrix$dataMatrix)){

      # Names for u^+_(i,k)
      # ReactionUp NameSource(i)=NameTarget in experiment k"
      expEdgesUp <- c(expEdgesUp, paste0("ReactionUp ",
                                         as.character(pknList$X1), "=",
                                         as.character(pknList$X3),
                                         " in experiment ", conditionIDX))
      # Names for u^-_(i,k)
      # ReactionDown NameSource(i)=NameTarget(other i) in experiment k"
      expEdgesDown <- c(expEdgesDown, paste0("ReactionDown ",
                                             as.character(pknList$X1), "=",
                                             as.character(pknList$X3),
                                             " in experiment ", conditionIDX))

      # ReactionSource NameSource(i)
      expEdgesReducedSource <- c(expEdgesReducedSource,
                                 paste0("ReactionSource ",
                                        as.character(pknList$X1)))

      # ReactionTarget NameTarget(other i)
      expEdgesReducedTarget <- c(expEdgesReducedTarget,
                                 paste0("ReactionTarget ",
                                        as.character(pknList$X3)))

      # Name of the source node: NameSource(i)
      expNodesReducedUp <- c(expNodesReducedUp, pknList$X1)

      # Name of the target node: NameTarget(other i)
      expNodesReducedDown <- c(expNodesReducedDown, pknList$X3)

      # Number of reactions
      idxExperimentEdges <- c(idxExperimentEdges, length(expEdgesUp))

    }

    # Concatenate all CPLEX variables for the reactions
    edgesALL <- c(edgesUp, edgesDown)

    # Concatenate all associated explanations
    expEdgesALL <- c(expEdgesUp, expEdgesDown)

    # Get CPLEX variable indices
    # for the potential of reactions to up/downregulate
    # Indices correspond to CPLEX variable name: xbINDEX_k
    # for nodes u^+_(i,k)
    idxEdgesUp <- (length(nodesALL)+1):(length(nodesALL)+
                                          1+
                                          nrow(pknList)*
                                          nrow(dataMatrix$dataMatrix)-1)

    # for nodes u^+_(i,k)
    idxEdgesDown <- (length(nodesALL)+
                       1+nrow(pknList)*nrow(dataMatrix$dataMatrix)):
      (length(nodesALL)+1+nrow(pknList)*nrow(dataMatrix$dataMatrix)+
         nrow(pknList)*nrow(dataMatrix$dataMatrix)-1)

    # signs, reactionSouce and reactionTarget represent the PKN
    signs <- pknList$X2
    reactionSource <- as.character(pknList$X1)
    reactionTarget <- as.character(pknList$X3)
    if(nrow(dataMatrix$dataMatrix) > 1){

      for(i in 2:nrow(dataMatrix$dataMatrix)){

        signs <- c(signs, pknList$X2)
        reactionSource <- c(reactionSource, as.character(pknList$X1))
        reactionTarget <- c(reactionTarget, as.character(pknList$X3))

      }

    }

    ##
    #Introducing distance variables

    # create one distance variable for each node j (d_j)
    # only once regardless of number of experiments
    dist <- paste0("dist_", sapply(strsplit(expNodesALL[idxNodes],
                                            split = " "), "[[", 2))

    # associated explanations
    distExp <- paste0("Distance ", sapply(strsplit(expNodesALL[idxNodes],
                                                   split = " "), "[[", 2))

    ##
    #Introducing B variables
    # Create B_(j,i) variable for each
    varB <- paste0("B_", sapply(strsplit(expNodes, split = " "),
                                function(x) x[2]), "_", conditionIDX)

    # associated explanation
    expVarB <- paste0("B variable for ", sapply(strsplit(expNodes, split = " "),
                                                function(x) x[2]),
                      " in experiment ", conditionIDX)

    ##
    # Matching table for u variables
    # each line contains [u^+_(i,k), u^-_(i,k)]
    uTable <- matrix(data = , nrow = length(idxEdgesUp), ncol = 2)
    uTable[, 1] <- c(nodesALL, edgesALL, varB, dist)[idxEdgesUp]
    uTable[, 2] <- c(nodesALL, edgesALL, varB, dist)[idxEdgesDown]


    # output
    res <- list(variables=c(nodesALL, edgesALL, varB, dist),
                exp=c(expNodesALL, expEdgesALL, expVarB, distExp),
                idxNodes=idxNodes, idxNodesUp=idxNodesUp,
                idxNodesDown=idxNodesDown, idxEdgesUp=idxEdgesUp,
                idxEdgesDown=idxEdgesDown, signs=signs,
                reactionSource=reactionSource, reactionTarget=reactionTarget,
                expNodesReduced=expNodesReduced,
                expNodesReducedUpSource=expNodesReducedUpSource,
                expNodesReducedDownSource=expNodesReducedDownSource,
                expNodesReducedDownTarget=expNodesReducedDownTarget,
                expNodesReducedUpTarget=expNodesReducedUpTarget,
                expEdgesReducedSource=expEdgesReducedSource,
                expEdgesReducedTarget=expEdgesReducedTarget,
                idxExperimentNodes=idxExperimentNodes,
                idxExperimentEdges=idxExperimentEdges,
                expNodesReducedUp=expNodesReducedUp,
                expNodesReducedDown=expNodesReducedDown,
                idxB = (length(c(nodesALL, edgesALL))+1):
                  (length(c(nodesALL, edgesALL))+length(varB)),
                idxDist = (length(c(nodesALL, edgesALL))+length(varB)+1):
                  (length(c(nodesALL, edgesALL))+length(varB)+length(dist)),
                uTable = uTable)

    return(res)

  }

  res <- list()
  namesRes <- c()

  # Create variables for each row in dataMatrix$dataMatrix
  # => for each experiment / timepoint
  for(i in 1:nrow(dataMatrix$dataMatrix)){

    dM <- dataMatrix

    # get current row of dataMatrix to feed create_variables
    dM$dataMatrix <- as.matrix(t(dataMatrix$dataMatrix[i, ]))
    dM$dataMatrixSign <- as.matrix(t(dataMatrix$dataMatrixSign[i, ]))

    res[[length(res)+1]] <- create_variables(pknList = pknList,
                                             dataMatrix = dM,
                                             conditionIDX = i)

    namesRes <- c(namesRes, paste0("Condition_", i))

  }

  names(res) <- namesRes

  return(res)

}
