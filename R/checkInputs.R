#'\code{checkInputs}
#'
#'@return Error message in case of errors in the inputs
#'
#'@export

checkInputs <- function(solverPath=NULL,
                        netObj=NULL,
                        measObj=NULL,
                        inputObj=NULL,
                        weightObj=NULL,
                        parallelIdx1=1,
                        parallelIdx2=1,
                        nodeID="uniprot",
                        UP2GS=FALSE,
                        DOTfig=TRUE,
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
                        solver="cbc", 
                        experimental_conditions = NULL){
  
  returnList = list()
  checkSolver(solverPath = solverPath, solver = solver, dir_name = dir_name)
  netObj = checkNetwork(netObj = netObj)
  measObj = checkMeasObj(measObj = measObj, netObj = netObj)
  inputObj = checkInputObj(inputObj = inputObj, netObj = netObj)
  weightObj = checkWeightObj(weightObj = weightObj, netObj = netObj)
  pp = checkSolverParam(parallelIdx1=parallelIdx1, parallelIdx2=parallelIdx2,
                   DOTfig=DOTfig, timelimit=timelimit, mipGAP=mipGAP,
                   poolrelGAP=poolrelGAP, limitPop=limitPop, poolCap=poolCap,
                   poolIntensity=poolIntensity, poolReplace=poolReplace,
                   alphaWeight=alphaWeight, betaWeight=betaWeight, UP2GS=UP2GS,
                   experimental_conditions=experimental_conditions)
  
  returnList[[length(returnList)+1]] = inputObj$network
  returnList[[length(returnList)+1]] = measObj
  returnList[[length(returnList)+1]] = inputObj
  returnList[[length(returnList)+1]] = weightObj
  returnList[[length(returnList)+1]] = pp$condition
  returnList[[length(returnList)+1]] = pp$repIndex
  names(returnList) = c("network", "measurements", "inputs", 
                        "weights", "condition", "repIndex")
  
  return(returnList)
  
}