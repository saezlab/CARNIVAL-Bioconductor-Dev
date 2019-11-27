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
#'@param CARNIVAL_example Number of built-in CARNIVAL example (1=Toy Model,2=SBVimprove-EGF,3=TG-GATEs-APAP) or set as NULL to use user-defined input files
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
                        solver="cbc", 
                        experimental_conditions = NULL)
{

  # @param parallelCR Execute the parallelised version of CARNIVAL pipeline (logical T/F)
  
  # Clean working environment
  # rm(list=ls());cat("\014") # clean screen and variables
  # if(length(dev.list())>0){dev.off()} # clean figure

  # library(devtools);load_all()

  # print("-----------")
  # print(parallelIdx1)
  # print("-----------")
  # print(parallelIdx2)
  # print("-----------")
  
  # QC step of provided inputs
  if (!file.exists(solverPath)) {stop("Please provide a valid path to interactive solver")}
  if (is.null(netObj)) {stop("Please provide a valid network object.")}
  if (is.null(measObj)) {stop("Please provide a valid measurement object.")}
  if (!is.numeric(parallelIdx1) | !is.numeric(parallelIdx2)) {stop("Please set numbers on the parameters 'parallelIdx1' and 'parallelIdx2' for running CARNIVAL in parallelisation ")}
  if (!is.logical(DOTfig)) {stop("For plotting: please choose whether to plot DOT figure as an output with a logical value T/F")}
  if (!is.numeric(timelimit)) {stop("CPLEX parameter: Please set a time limit for CPLEX optimisation in seconds")}
  if (!is.null(mipGAP)) {if (!is.numeric(mipGAP)) {stop("CPLEX parameter: Please set the allowed mipGAP parameter or leave it as NULL for CPLEX default value (1e-04)")}}; if (is.null(mipGAP)) {mipGAP=1e-04}
  if (!is.null(poolrelGAP)) {if (!is.numeric(poolrelGAP)) {stop("CPLEX parameter: Please set the allowed pool relative GAP parameter or leave it as NULL for CPLEX default value (1e75)")}}; if (is.null(poolrelGAP)) {poolrelGAP=1e75}
  if (!is.null(limitPop)) {if (!is.numeric(limitPop)) {stop("CPLEX parameter: Please set the allowed population limit of solution to be generated or leave it as NULL for CPLEX default value (20)")}}; if (is.null(limitPop)) {limitPop=20}
  if (!is.null(poolCap)) {if (!is.numeric(poolCap)) {stop("CPLEX parameter: Please set the allowed number of solutions to be kept or leave it as NULL for CPLEX default value (2.1e9)")}}; if (is.null(poolCap)) {poolCap=2.1e9}
  if (!is.null(poolIntensity)) {if (!(poolIntensity %in% c(0,1,2,3,4))) {stop("CPLEX parameter: Please set the level of intensity for solution searching [0,1,2,3,4] or leave it as NULL for CPLEX default value (0) - to be decided by CPLEX")}}; if (is.null(poolIntensity)) {poolIntensity=0}
  if (!is.null(poolReplace)) {if (!(poolReplace %in% c(0,1,2))) {stop("CPLEX parameter: Please set the replacement strategy of solution [0,1,2] or leave it as NULL for CPLEX default value (0) - First In First Out")}}; if (is.null(poolReplace)) {poolReplace=0}
  if (!is.numeric(alphaWeight)) {stop("Objective Function: Please set a weight for mismatch penalty (will be applied only when the weight of measurement is not defined)")}
  if (!is.numeric(betaWeight)) {stop("Objective Function: Please set a weight for node penalty")}
  
  # checking for solver validity (cplex/cbc)
  valid_solver_list <- c("cplex", "cbc")
  if (!(solver %in% valid_solver_list)){
    stop(paste0("Select a valid solver option (", paste(valid_solver_list, collapse=", "), ")"))
  }

  # Load necessary packages and functions
  load(file = system.file("progenyMembers.RData",package="CARNIVAL"))
  current_dir = getwd()

  if(is.null(inputObj)){
    inverseCR = T
  } else {
    inverseCR = F
  }
  
  if (parallelIdx1==1 & parallelIdx2==1) { # default case
    repIndex=1;condition=1
  } else {
    condition=parallelIdx1;repIndex=parallelIdx2
  }
  
  # Removing measurements not present in the network
  if(ncol(measObj)>0){
    mSpecies = colnames(measObj)
  } else {
    stop("Something wrong with your measurements object. Please check.")
  }
  
  if(ncol(netObj)==3){
    nSpecies = unique(c(as.character(as.matrix(netObj)[, 1]), as.character(as.matrix(netObj)[, 3])))
  } else {
    stop("Something wrong with your network object. Please check.")
  }
  
  idx = which(mSpecies%in%nSpecies)
  idx2rem = setdiff(1:length(mSpecies), idx)
  
  if(length(idx2rem)==length(mSpecies)){
    stop("Something wrong with your measurements object/network object. No measurements is present in the network")
  } else {
    if(length(idx2rem)>0){
      measObj = measObj[, -idx2rem]
      measObj = t(as.matrix(measObj))
    }
  }
  
  # Removing inputs not present in the network
  if(!is.null(inputObj)){
    if(ncol(inputObj)>0){
      mSpecies = colnames(inputObj)
    } else {
      stop("Something wrong with your measurements object. Please check.")
    }
    
    if(ncol(netObj)==3){
      nSpecies = unique(c(as.character(as.matrix(netObj)[, 1]), as.character(as.matrix(netObj)[, 3])))
    } else {
      stop("Something wrong with your network object. Please check.")
    }
    
    idx = which(mSpecies%in%nSpecies)
    idx2rem = setdiff(1:length(mSpecies), idx)
    
    if(length(idx2rem)==length(mSpecies)){
      stop("Something wrong with your measurements object/network object. No input is present in the network")
    } else {
      if(length(idx2rem)>0){
        inputObj = inputObj[, -idx2rem]
      }
    } 
  }

  # Input processing
  network <- netObj
  measWeights <- measObj
  measurements <- sign(measWeights) # Extracted sign of measurement for ILP fitting
  measWeights <- abs(measWeights) # Weights are all positives
  if (!is.null(inputObj)) {
    inputs <- inputObj
  }
  if (!is.null(weightObj)) {
    edgeWeights <- weightObj
    scores <- assignPROGENyScores(progeny = edgeWeights, progenyMembers = progenyMembers, id = nodeID)
  } else {
    scores <- NULL
  }
  
  # Making every input as a matrix class object
  if(any(class(network)%in%c("matrix", "data.frame"))){
    network = as.matrix(network)
    network[, 2] = as.character(as.numeric(network[, 2]))
  } else {
    stop("netObj should either be provided as a data frame or a matrix object")
  }
  
  if(any(class(measWeights)%in%c("matrix", "data.frame"))){
    measWeights = as.matrix(measWeights)
    measurements = as.matrix(measurements)
  } else {
    stop("measObj should either be provided as a data frame or a matrix object")
  }
  
  if(!is.null(inputObj)){
    if(any(class(inputObj)%in%c("matrix", "data.frame"))){
      inputs = as.matrix(inputs)
    } else {
      stop("inputObj should either be provided as a data frame or a matrix object")
    }
  }
  
  if(!is.null(weightObj)){
    if(any(class(weightObj)%in%c("matrix", "data.frame"))){
      edgeWeights = as.matrix(edgeWeights)
    } else {
      stop("weightObj should either be provided as a data frame or a matrix object")
    }
  }

  # Adding perturbation node for the case of inverse causal reasoning
  if (inverseCR) {
    MappedPertNode <- AddPerturbationNode(network)
    inputs <- MappedPertNode$inputs
    network <- MappedPertNode$network
  }

  data <- measurements

  pknList <- as.data.frame(network)
  colnames(pknList) <- c("Node1", "Sign", "Node2")

  # Remove intermediate cplex files (if any)
  AllFiles <- list.files()
  CloneFiles <- which(grepl(pattern = "clone",x = AllFiles,fixed = T))
  if (length(CloneFiles)>0) {
    for (counter in 1:length(CloneFiles)) {
      file.remove(AllFiles[CloneFiles[counter]])
    }
  }

  # Remove redundant files prior to optimisation
  if (file.exists(paste0("testFile_",condition,"_",repIndex,".lp"))) {file.remove(paste0("testFile_",condition,"_",repIndex,".lp"))} # might be useful for debugging
  if (file.exists(paste0("results_cplex_",condition, "_",repIndex,".txt"))) {file.remove(paste0("results_cplex_",condition,"_",repIndex,".txt"))}
  if (file.exists("cplex.log")) {file.remove("cplex.log")}
  if (file.exists(paste0("cplexCommand_", condition,"_",repIndex,".txt"))) {file.remove(paste0("cplexCommand_", condition,"_",repIndex,".txt"))}

  # Write constraints as ILP inputs
  ptm <- proc.time()
  print("Writing constraints...")

  pknList <<- pknList

  if(is.null(experimental_conditions)){
    
    variables <- writeLPFile(data=measurements,pknList=pknList,inputs=inputs,betaWeight=betaWeight,
                             scores=scores,mipGAP=mipGAP,poolrelGAP=poolrelGAP,limitPop=limitPop,
                             poolCap=poolCap,poolIntensity=poolIntensity,poolReplace=poolReplace,
                             timelimit=timelimit,measWeights=measWeights,
                             repIndex=repIndex,condition = condition)
    Elapsed_1 <- proc.time() - ptm
    
    # Solve ILP problem with cplex, remove temp files, and return to the main directory
    ptm <- proc.time()
    print("Solving LP problem...")
    
    if(solver=="cplex"){
      
      if (Sys.info()[1]=="Windows") {
        file.copy(from = solverPath,to = getwd())
        system(paste0("cplex.exe -f cplexCommand_", condition,"_",repIndex,".txt"))
        file.remove("cplex.exe")
        Elapsed_2 <- proc.time() - ptm
      } else {
        system(paste0(solverPath, " -f cplexCommand_", condition,"_",repIndex,".txt"))
        Elapsed_2 <- proc.time() - ptm
      }
      
      # Move result files to result folder and remove redundant files after the optimisation
      if (file.exists(paste0("testFile_",condition,"_",repIndex,".lp"))) {file.remove(paste0("testFile_",condition,"_",repIndex,".lp"))} # might be useful for debugging
      
      if (file.exists("cplex.log")) {file.remove("cplex.log")}
      
      if (file.exists(paste0("cplexCommand_", condition,"_",repIndex,".txt"))) {file.remove(paste0("cplexCommand_", condition,"_",repIndex,".txt"))}
      AllFiles <- list.files()
      
      CloneFiles <- which(grepl(pattern = "clone",x = AllFiles,fixed = T))
      if (length(CloneFiles)>0) {
        for (counter in 1:length(CloneFiles)) {
          file.remove(AllFiles[CloneFiles[counter]])
        }
      }
      
      # Write result files in the results folder
      ptm <- proc.time()
      print("Writing result files...")
      resList <- list()
      # if (file.exists(paste0("results/",dir_name,"/results_cplex.txt"))) {
      if (file.exists(paste0("results_cplex_",condition,"_",repIndex,".txt"))) {
        for(i in 1:length(variables)){
          res <- exportResult(cplexSolutionFileName = paste0("results_cplex_",condition,"_",repIndex,".txt"),
                              variables = variables, pknList = pknList, conditionIDX = i,
                              inputs=inputs,measurements=measurements)
          resList[[length(resList)+1]] <- res
          # res <- files2res(counterlist) # retrieve results from previously generated result files
        }
        if (!is.null(res)) {
          if(!is.null(UP2GS)){if (UP2GS) {res <- Uniprot2GeneSymbol(res)}}
          if (DOTfig) {WriteDOTfig(res=res,dir_name=dir_name,
                                   inputs=inputs,measurements=measurements,UP2GS=UP2GS)}
        }
      } else {
        print("No result to be written")
        return(NULL)
      }
      Elapsed_3 <- proc.time() - ptm
      
      if (file.exists(paste0("results_cplex_",condition, "_",repIndex,".txt"))) {file.remove(paste0("results_cplex_",condition,"_",repIndex,".txt"))} # optional; remove cplex results (to save space)
      
      # Remove global variable 
      objs <- ls(pos = ".GlobalEnv")
      rm(list = objs[grep("pknList", objs)], pos = ".GlobalEnv") # remove pknList
      
      print(" ")
      print("--- End of the CARNIVAL pipeline ---")
      print(" ")
      
      result = resList[[1]]
      
      return(result)
      
    } else {
      
      resFile = paste0("results_cbc_", parallelIdx1, "_", parallelIdx2, ".txt")
      
      cbc_command <- paste0(solverPath, " testFile_", parallelIdx1, "_", parallelIdx2, ".lp -seconds ", timelimit,
                            " -ratio ", poolrelGAP, " solve printi csv solu ", resFile)
      
      system(cbc_command)
      
      res <- exportResult(cplexSolutionFileName = resFile, variables = variables, conditionIDX = parallelIdx1,
                          pknList = pknList, inputs=inputs, measurements=measurements, solver = "cbc")
      
      if (!is.null(res)) {
        if(!is.null(UP2GS)){if (UP2GS) {res <- Uniprot2GeneSymbol(res)}}
        if (DOTfig) {WriteDOTfig(res=res,dir_name=dir_name,
                                 inputs=inputs,measurements=measurements,UP2GS=UP2GS)}
      }
      
      if (file.exists(paste0("testFile_",condition,"_",repIndex,".lp"))) {file.remove(paste0("testFile_",condition,"_",repIndex,".lp"))} # might be useful for debugging
      if (file.exists(paste0("results_cbc_",condition, "_",repIndex,".txt"))) {file.remove(paste0("results_cbc_",condition,"_",repIndex,".txt"))}
      if (file.exists("cplex.log")) {file.remove("cplex.log")}
      if (file.exists(paste0("cplexCommand_", condition,"_",repIndex,".txt"))) {file.remove(paste0("cplexCommand_", condition,"_",repIndex,".txt"))}
      
      return(res)
      
    }
    
  } else {
    
    if(class(experimental_conditions) != "numeric"){
      stop("Error with the assignment of the experimental conditions. Please provide this input as a numeric vector")
    }
    
    variables <- writeLPFileMulti(data=measurements,pknList=pknList,inputs=inputs,betaWeight=betaWeight,
                             scores=scores,mipGAP=mipGAP,poolrelGAP=poolrelGAP,limitPop=limitPop,
                             poolCap=poolCap,poolIntensity=poolIntensity,poolReplace=poolReplace,
                             timelimit=timelimit,measWeights=measWeights,
                             repIndex=repIndex,condition = condition, experimental_conditions)
    Elapsed_1 <- proc.time() - ptm
    
    # Solve ILP problem with cplex, remove temp files, and return to the main directory
    ptm <- proc.time()
    print("Solving LP problem...")
    
    if(solver=="cplex"){
      
      # Solve ILP problem with cplex, remove temp files, and return to the main directory
      ptm <- proc.time()
      print("Solving LP problem...")
      
      if (Sys.info()[1]=="Windows") {
        file.copy(from = solverPath,to = getwd())
        system(paste0("cplex.exe -f cplexCommand_", condition,"_",repIndex,".txt"))
        file.remove("cplex.exe")
        Elapsed_2 <- proc.time() - ptm
      } else {
        system(paste0(solverPath, " -f cplexCommand_", condition,"_",repIndex,".txt"))
        Elapsed_2 <- proc.time() - ptm
      }
      
      # Move result files to result folder and remove redundant files after the optimisation
      if (file.exists(paste0("testFile_",condition,"_",repIndex,".lp"))) {file.remove(paste0("testFile_",condition,"_",repIndex,".lp"))} # might be useful for debugging
      
      if (file.exists("cplex.log")) {file.remove("cplex.log")}
      
      if (file.exists(paste0("cplexCommand_", condition,"_",repIndex,".txt"))) {file.remove(paste0("cplexCommand_", condition,"_",repIndex,".txt"))}
      AllFiles <- list.files()
      
      CloneFiles <- which(grepl(pattern = "clone",x = AllFiles,fixed = T))
      if (length(CloneFiles)>0) {
        for (counter in 1:length(CloneFiles)) {
          file.remove(AllFiles[CloneFiles[counter]])
        }
      }
      
      # Write result files in the results folder
      ptm <- proc.time()
      print("Writing result files...")
      resList <- list()
      # if (file.exists(paste0("results/",dir_name,"/results_cplex.txt"))) {
      if (file.exists(paste0("results_cplex_",condition,"_",repIndex,".txt"))) {
        res <- exportResultAllConditions(cplexSolutionFileName = paste0("results_cplex_",condition,"_",repIndex,".txt"),
                                         variables = variables, pknList = pknList,
                                         inputs=inputs,measurements=measurements, solver = "cplex")
        if (!is.null(res)) {
          if(!is.null(UP2GS)){if (UP2GS) {res <- Uniprot2GeneSymbol(res)}}
          if (DOTfig) {warning("Writing DOT figures not implemented when considering multiple experimental conditions")}
        }
        
        if (file.exists(paste0("testFile_",condition,"_",repIndex,".lp"))) {file.remove(paste0("testFile_",condition,"_",repIndex,".lp"))} # might be useful for debugging
        if (file.exists(paste0("results_cplex_",condition, "_",repIndex,".txt"))) {file.remove(paste0("results_cplex_",condition,"_",repIndex,".txt"))}
        if (file.exists("cplex.log")) {file.remove("cplex.log")}
        if (file.exists(paste0("cplexCommand_", condition,"_",repIndex,".txt"))) {file.remove(paste0("cplexCommand_", condition,"_",repIndex,".txt"))}
        
        return(res)
        
      }
    
    } else {
    
      resFile = paste0("results_cbc_", parallelIdx1, "_", parallelIdx2, ".txt")
      
      cbc_command <- paste0(solverPath, " testFile_", parallelIdx1, "_", parallelIdx2, ".lp -seconds ", timelimit,
                            " -ratio ", poolrelGAP, " solve printi csv solu ", resFile)
      
      system(cbc_command)
      
      res <- exportResultAllConditions(cplexSolutionFileName = resFile, variables = variables,
                          pknList = pknList, inputs=inputs, measurements=measurements, solver = "cbc")
      
      if (!is.null(res)) {
        if(!is.null(UP2GS)){if (UP2GS) {res <- Uniprot2GeneSymbol(res)}}
        if (DOTfig) {warning("Writing DOT figures not implemented when considering multiple experimental conditions")}
      }
      
      if (file.exists(paste0("testFile_",condition,"_",repIndex,".lp"))) {file.remove(paste0("testFile_",condition,"_",repIndex,".lp"))} # might be useful for debugging
      if (file.exists(paste0("results_cbc_",condition, "_",repIndex,".txt"))) {file.remove(paste0("results_cbc_",condition,"_",repIndex,".txt"))}
      if (file.exists("cplex.log")) {file.remove("cplex.log")}
      if (file.exists(paste0("cplexCommand_", condition,"_",repIndex,".txt"))) {file.remove(paste0("cplexCommand_", condition,"_",repIndex,".txt"))}
      
      return(res)
      
    }
    
  }

}
