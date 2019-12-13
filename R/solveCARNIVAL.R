#'\code{solveCARNIVAL}
#'
#'@return result object after solving with CARNIVAL
#'
#'@export
#'

solveCARNIVAL <- function(solverPath = solverPath, 
                          netObj = netObj, 
                          measObj = measObj, 
                          inputObj = inputObj, 
                          weightObj = weightObj, 
                          parallelIdx1 = parallelIdx1, 
                          parallelIdx2 = parallelIdx2, 
                          nodeID = nodeID, 
                          UP2GS = UP2GS, 
                          DOTfig = DOTfig, 
                          timelimit = timelimit, 
                          mipGAP = mipGAP, 
                          poolrelGAP = poolrelGAP, 
                          limitPop = limitPop, 
                          poolCap = poolCap, 
                          poolIntensity = poolIntensity, 
                          poolReplace = poolReplace, 
                          alphaWeight = alphaWeight, 
                          betaWeight = betaWeight, 
                          dir_name = dir_name, 
                          solver = solver, 
                          experimental_conditions = experimental_conditions,
                          condition = condition,
                          repIndex = repIndex){
  
  ## Write constraints as ILP inputObj
  ptm <- proc.time()
  print("Writing constraints...")
  
  pknList <- as.data.frame(netObj)
  colnames(pknList) <- c("Node1", "Sign", "Node2")
  
  ## Extracted sign of measurement for ILP fitting
  measurements <- sign(measObj)
  measWeights <- abs(measObj)
  
  ## Check the weight
  if(weightObj=="NULL"){weightObj=NULL}
  
  pknList <<- pknList
  
  if(is.null(experimental_conditions)){
    
    variables <- writeLPFile(data=measurements,pknList=pknList,inputs=inputObj,
                             betaWeight=betaWeight, scores=weightObj,
                             mipGAP=mipGAP,poolrelGAP=poolrelGAP,
                             limitPop=limitPop,poolCap=poolCap,
                             poolIntensity=poolIntensity,
                             poolReplace=poolReplace,timelimit=timelimit,
                             measWeights=measWeights,repIndex=repIndex,
                             condition = condition)
    Elapsed_1 <- proc.time() - ptm
    
    ## Solve ILP problem with cplex, remove temp files, 
    ## and return to the main directory
    ptm <- proc.time()
    print("Solving LP problem...")
    
    if(solver=="cplex"){
      
      if (Sys.info()[1]=="Windows") {
        file.copy(from = solverPath,to = getwd())
        system(paste0("cplex.exe -f cplexCommand_", 
                      condition,"_",repIndex,".txt"))
        file.remove("cplex.exe")
        Elapsed_2 <- proc.time() - ptm
      } else {
        system(paste0(solverPath, " -f cplexCommand_", 
                      condition,"_",repIndex,".txt"))
        Elapsed_2 <- proc.time() - ptm
      }
      
      # cleanupCARNIVAL(condition = condition, repIndex = repIndex)
      
      # Write result files in the results folder
      ptm <- proc.time()
      print("Writing result files...")
      resList <- list()
      # if (file.exists(paste0("results/",dir_name,"/results_cplex.txt"))) {
      if (file.exists(paste0("results_cplex_",condition,"_",repIndex,".txt"))) {
        for(i in 1:length(variables)){
          res <- exportResult(cplexSolutionFileName = paste0("results_cplex_",
                                                             condition,"_",
                                                             repIndex,".txt"),
                              variables = variables, 
                              pknList = pknList, 
                              conditionIDX = i,
                              inputs=inputObj,
                              measurements=measObj)
          resList[[length(resList)+1]] <- res
        }
        if (!is.null(res)) {
          if(!is.null(UP2GS)){if (UP2GS) {res <- Uniprot2GeneSymbol(res)}}
          if (DOTfig) {WriteDOTfig(res=res,
                                   dir_name=dir_name,
                                   inputs=inputObj,
                                   measurements=measObj,
                                   UP2GS=UP2GS)}
        }
      } else {
        print("No result to be written")
        return(NULL)
      }
      Elapsed_3 <- proc.time() - ptm
      
      cleanupCARNIVAL(condition = condition, repIndex = repIndex)
      
      # Remove global variable 
      objs <- ls(pos = ".GlobalEnv")
      rm(list = objs[grep("pknList", objs)], pos = ".GlobalEnv")
      
      print(" ")
      print("--- End of the CARNIVAL pipeline ---")
      print(" ")
      
      result = resList[[1]]
      
      return(result)
      
    } else {
      
      resFile = paste0("results_cbc_", parallelIdx1, "_", parallelIdx2, ".txt")
      
      cbc_command <- paste0(solverPath, " testFile_", parallelIdx1, "_", 
                            parallelIdx2, ".lp -seconds ", timelimit,
                            " -ratio ", poolrelGAP, 
                            " solve printi csv solu ", resFile)
      
      system(cbc_command)
      
      res <- exportResult(cplexSolutionFileName = resFile, 
                          variables = variables, 
                          conditionIDX = parallelIdx1,
                          pknList = pknList, 
                          inputs=inputObj, 
                          measurements=measObj, 
                          solver = "cbc")
      
      if (!is.null(res)) {
        if(!is.null(UP2GS)){if (UP2GS) {res <- Uniprot2GeneSymbol(res)}}
        if (DOTfig) {WriteDOTfig(res=res,
                                 dir_name=dir_name,
                                 inputs=inputObj,
                                 measurements=measObj,
                                 UP2GS=UP2GS)}
      }
      
      # cleanupCARNIVAL(condition = condition, repIndex = repIndex)
      
      return(res)
      
    }
    
  } else {
    
    if(class(experimental_conditions) != "numeric"){
      stop("Error with the assignment of the experimental conditions. 
           Please provide this input as a numeric vector")
    }
    
    variables <- writeLPFileMulti(data=measurements,pknList=pknList,
                                  inputs=inputObj,betaWeight=betaWeight,
                                  scores=weightObj,mipGAP=mipGAP,
                                  poolrelGAP=poolrelGAP,limitPop=limitPop,
                                  poolCap=poolCap,poolIntensity=poolIntensity,
                                  poolReplace=poolReplace,timelimit=timelimit,
                                  measWeights=measWeights,repIndex=repIndex,
                                  condition = condition, 
                                  experimental_conditions)
    Elapsed_1 <- proc.time() - ptm
    
    ## Solve ILP problem with cplex, remove temp files, and 
    ## return to the main directory
    ptm <- proc.time()
    print("Solving LP problem...")
    
    if(solver=="cplex"){
      
      ## Solve ILP problem with cplex, remove temp files, 
      ## and return to the main directory
      ptm <- proc.time()
      print("Solving LP problem...")
      
      if (Sys.info()[1]=="Windows") {
        file.copy(from = solverPath,to = getwd())
        system(paste0("cplex.exe -f cplexCommand_", 
                      condition,"_",repIndex,".txt"))
        file.remove("cplex.exe")
        Elapsed_2 <- proc.time() - ptm
      } else {
        system(paste0(solverPath, " -f cplexCommand_", 
                      condition,"_",repIndex,".txt"))
        Elapsed_2 <- proc.time() - ptm
      }
      
      cleanupCARNIVAL(condition = condition, repIndex = repIndex)
      
      # Write result files in the results folder
      ptm <- proc.time()
      print("Writing result files...")
      resList <- list()
      if (file.exists(paste0("results_cplex_",condition,"_",repIndex,".txt"))) {
        res <- exportResultAllConditions(cplexSolutionFileName = 
                                           paste0("results_cplex_",condition,
                                                  "_",repIndex,".txt"),
                                         variables = variables, 
                                         pknList = pknList,
                                         inputs=inputObj,
                                         measurements=measObj, 
                                         solver = "cplex")
        if (!is.null(res)) {
          if(!is.null(UP2GS)){if (UP2GS) {res <- Uniprot2GeneSymbol(res)}}
          if (DOTfig) {warning("Writing DOT figures not implemented when 
                               considering multiple experimental conditions")}
        }
        
        cleanupCARNIVAL(condition = condition, repIndex = repIndex)
        
        return(res)
        
      }
      
    } else {
      
      resFile = paste0("results_cbc_", parallelIdx1, "_", parallelIdx2, ".txt")
      
      cbc_command <- paste0(solverPath, " testFile_", parallelIdx1, "_", 
                            parallelIdx2, ".lp -seconds ", timelimit,
                            " -ratio ", poolrelGAP, 
                            " solve printi csv solu ", resFile)
      
      system(cbc_command)
      
      res <- exportResultAllConditions(cplexSolutionFileName = resFile, 
                                       variables = variables,
                                       pknList = pknList, 
                                       inputs=inputObj, 
                                       measurements=measObj, 
                                       solver = "cbc")
      
      if (!is.null(res)) {
        if(!is.null(UP2GS)){if (UP2GS) {res <- Uniprot2GeneSymbol(res)}}
        if (DOTfig) {warning("Writing DOT figures not implemented when 
                             considering multiple experimental conditions")}
      }
      
      cleanupCARNIVAL(condition = condition, repIndex = repIndex)
      
      return(res)
      
    }
    
  }
}