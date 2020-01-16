#'\code{runCARNIVAL}
#'
#'Run CARNIVAL pipeline using to the user-provided list of inputs or run CARNIVAL built-in examples
#'Note: The pipeline requires either all required user-defined input variables (netObj and measObj) are set to NULL or CARNIVAL_example is set to NULL to execute
#'
#'@param solverPath Path to executable cplex file - always required
#'@param netObj Filename of the prior knowledge network - always required or set as NULL to run CARNIVAL built-in example
#'@param measObj Filename of the measurement file (here DoRothEA normalised enrichment scores) - always required or set as NULL to run CARNIVAL built-in example
#'@param inputObj Filename of the list for target of perturbation - optional or set as NULL to run CARNIVAL built-in example
#'@param weightObj Filename of the additional weight (here PROGENy pathway score) - optional or set as NULL to run CARNIVAL built-in example
#'@param Result_dir Specify directory name to store results
#'@param inverseCR Execute the inverse CARNIVAL pipeline (logical T/F)
#'@param parallelIdx1 First index number suitable for parallelisation (numeric - set to 1 by default)
#'@param parallelIdx2 Second index number suitable for parallelisation (numeric - set to 1 by default)
#'@param nodeID Define the input format of nodes in the network (either 'uniprot' or 'gene' symbol)
#'@param UP2GS For plotting: define if Uniprot ID will be converted to gene symbols for better readability (logical T/F)
#'@param DOTfig For plotting: define if DOT figure will be exported in the result folder (logical T/F)
#'@param timelimit CPLEX parameter: Time limit of CPLEX optimisation (in seconds)
#'@param mipGAP CPLEX parameter: Allowed gap of accepted solution comparing to the best solution (fraction; default: 0.05 = 5 percents)
#'@param poolrelGAP CPLEX parameter: Allowed relative gap of accepted solution comparing within the pool of accepted solution (fraction; default: 0.0001)
#'@param limitPop CPLEX parameter: Allowed number of solutions to be generated (default: 500)
#'@param poolCap CPLEX parameter: Allowed number of solution to be kept in the pool of solution (default: 100)
#'@param poolIntensity CPLEX parameter: Intensity of solution searching (0,1,2,3,4 - default: 4)
#'@param poolReplace CPLEX parameter: Replacement strategy of solutions in the pool (0,1,2 - default: 2 = most diversified solutions)
#'@param alphaWeight Objective function: weight for mismatch penalty (default: 1 - will only be applied once measurement file only contains discrete values)
#'@param betaWeight Objective function: weight for node penalty (defaul: 0.2)
#'
#'@return The networks and predicted node activities from the CARNIVAL pipeline as a variable which are also saved in the destined result folder
#'
#'@import doParallel
#'@import igraph
#'@import CARNIVAL
#'@import readr
#'@import dplyr
#'@import readxl
#'
#'@export

runCARNIVAL <- function(solverPath=NULL,
                        netObj=NULL,
                        measObj=NULL,
                        inputObj=NULL,
                        weightObj=NULL,
                        parallelIdx1=1,
                        parallelIdx2=1,
                        nodeID="uniprot",
                        UP2GS=NULL,
                        DOTfig=T,
                        timelimit=600,
                        mipGAP=0.05,
                        poolrelGAP=0.0001,
                        limitPop=500,
                        poolCap=100,
                        poolIntensity=4,
                        poolReplace=2,
                        alphaWeight=1,
                        betaWeight=0.2,
                        dir_name=paste0(getwd(), "/DOTfigures"),
                        solver="cplex",
                        dt=FALSE,
                        experimental_conditions = NULL)
{

  res = checkInputs(solverPath = solverPath, netObj = netObj, measObj = measObj,
                    inputObj = inputObj, weightObj = weightObj,
                    parallelIdx1 = parallelIdx1, parallelIdx2 = parallelIdx2,
                    nodeID = nodeID, UP2GS = UP2GS, DOTfig = DOTfig,
                    timelimit = timelimit, mipGAP = mipGAP,
                    poolrelGAP = poolrelGAP, limitPop = limitPop,
                    poolCap = poolCap, poolIntensity = poolIntensity,
                    poolReplace = poolReplace, alphaWeight = alphaWeight,
                    betaWeight = betaWeight, dir_name = dir_name,
                    solver = solver, dt = dt
                    experimental_conditions = experimental_conditions)

  cleanupCARNIVAL(condition = res$condition, repIndex = res$repIndex)

  result = solveCARNIVAL(solverPath = solverPath, netObj = res$inputs$network,
                         measObj = res$measurements,
                         inputObj = res$inputs$inputs,
                         weightObj = res$weights, parallelIdx1 = parallelIdx1,
                         parallelIdx2 = parallelIdx2, nodeID = nodeID,
                         UP2GS = UP2GS, DOTfig = DOTfig, timelimit = timelimit,
                         mipGAP = mipGAP, poolrelGAP = poolrelGAP,
                         limitPop = limitPop, poolCap = poolCap,
                         poolIntensity = poolIntensity,
                         poolReplace = poolReplace, alphaWeight = alphaWeight,
                         betaWeight = betaWeight, dir_name = dir_name,
                         solver = solver, dt = dt,
                         experimental_conditions = experimental_conditions,
                         condition = res$condition, repIndex = res$repIndex)

  return(result)

}
