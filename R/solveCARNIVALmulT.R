#'\code{solveCARNIVAL}
#'
#' Returning result object after solving with CARNIVAL
#'


library(progress)
# library(doParallel)
library(future)

solveCARNIVALmulT <- function(data = measurements,
                              pknList = pknList,
                              inputs = inputObj,
                              betaWeight = betaWeight,
                              scores = weightObj,
                              mipGAP = mipGAP,
                              poolrelGAP = poolrelGAP,
                              limitPop = limitPop,
                              poolCap = poolCap,
                              poolIntensity = poolIntensity,
                              poolReplace = poolReplace,
                              timelimit = timelimit,
                              measWeights = measWeights,
                              alphaWeight = alphaWeight,
                              threads = threads,
                              mulT = mulT,
                              repIndex = repIndex,
                              condition = condition,
                              solver = solver,
                              solverPath = solverPath,
                              variables = variables,
                              measObj = measObj,
                              inputObj = inputObj,
                              DOTfig = DOTfig,
                              dir_name = dir_name) {

  variables <- writeLPFileMulT(data = data,
                               pknList = pknList,
                               inputs = inputObj,
                               alphaWeight = alphaWeight,
                               betaWeight = betaWeight,
                               scores = scores,
                               mipGAP = mipGAP,
                               poolrelGAP = poolrelGAP,
                               limitPop = limitPop,
                               poolCap = poolCap,
                               poolIntensity = poolIntensity,
                               poolReplace = poolReplace,
                               timelimit = timelimit,
                               measWeights = measWeights,
                               repIndex = repIndex,
                               mulT = mulT,
                               threads = threads,
                               condition = condition)

  ## Solve ILP problem with cplex, remove temp files,
  ## and return to the main directory
  ptm <- proc.time()
  print("Solving LP problem...")

  if (solver == "cplex") {

    if (Sys.info()[1] == "Windows") {
      file.copy(from = solverPath, to = getwd())
      system(paste0("cplex.exe -f cplexCommand_",
                    condition, "_", repIndex, ".txt"))
      file.remove("cplex.exe")
      Elapsed_2 <- proc.time() - ptm
    } else {
      system(paste0(solverPath, " -f cplexCommand_",
                    condition, "_", repIndex, ".txt"))
      Elapsed_2 <- proc.time() - ptm
    }

    ## Write result files in the results folder
    ptm <- proc.time()
    print("Writing result files...")
    resList <- list()
    if (file.exists(paste0("results_cplex_", condition, "_", repIndex, ".txt"))) {
      res <- exportResult(cplexSolutionFileName = paste0("results_cplex_",
                                                         condition, "_",
                                                         repIndex, ".txt"),
                          variables = variables,
                          pknList = pknList,
                          conditionIDX = length(variables) - 1,
                          inputs = inputObj,
                          measurements = measObj)
      resList[[length(resList) + 1]] <- res

      if (!is.null(res)) {
        if (DOTfig) {
          WriteDOTfig(res = res,
                      dir_name = dir_name,
                      inputs = inputObj,
                      measurements = measObj)
        }
      }
    } else {
      print("No result to be written")
      return(NULL)
    }
    Elapsed_3 <- proc.time() - ptm

    cleanupCARNIVAL(condition = condition, repIndex = repIndex)

    ## Remove global variable
    objs <- ls(pos = ".GlobalEnv")
    rm(list = objs[grep("pknList", objs)], pos = ".GlobalEnv")

    print(" ")
    print("--- End of the CARNIVAL pipeline ---")
    print(" ")

    result = resList[[1]]
    result$variables <- variables

    return(result)

  } else {
    stop("CARNIVAL mulT not yet implemented with solvers other than CPLEX")
  }
}
