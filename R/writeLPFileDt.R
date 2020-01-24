#'\code{writeLPFileDt}
#'
#' Same as the 'writeLPFileMulti', but this one takes multiple timepoints
#' into consitderation.
#'


writeLPFileDt <-  function(data = data,
                           pknList = pknList,
                           inputs = inputs,
                           alphaWeight = 1,
                           betaWeight = 0.2,
                           scores = scores,
                           mipGAP = 0.1,
                           poolrelGAP = 0.01,
                           limitPop = 100,
                           poolCap = 100,
                           poolIntensity = 0,
                           poolReplace = 2,
                           timelimit = 1800,
                           measWeights = NULL,
                           repIndex,
                           condition = "",
                           dt = dt,
                           experimental_conditions) {

  dataMatrix <- buildDataMatrix(data = data,
                                pknList = pknList,
                                inputs = inputs)
  # Leftovers from writeLPFileMulti
  # dataMatrix$dataMatrix <- dataMatrix$dataMatrix[experimental_conditions, ]
  # dataMatrix$dataMatrixSign <-
  #   dataMatrix$dataMatrixSign[experimental_conditions, ]

  variables <- create_variables_all(pknList = pknList, dataMatrix = dataMatrix)
  variables <- append_general_variables(variables = variables, dt = dt)

  oF <- write_objective_function_all(dataMatrix = dataMatrix,
                                     variables =
                                       variables[1:(length(variables) - 1)],
                                     alphaWeight = alphaWeight,
                                     betaWeight = betaWeight,
                                     scores = scores,
                                     measWeights = measWeights)

  bounds <- write_boundaries_all_conditions(variables = variables, oF = oF)
  bounds <- append_bounds(bounds = bounds, variables = variables)
  binaries <- write_binaries_all_conditions(variables = variables, dt = dt)
  binaries <- append_binaries(binaries = binaries, variables = variables)
  generals <- write_generals_all_conditions(variables = variables, oF = oF)

  c0 <-
    write_constraints_objFunction_all(variables =
                                        variables[1:(length(variables) - 1)],
                                      dataMatrix = dataMatrix)
  c1 <-
    write_constraints_1_all(variables = variables[1:(length(variables) - 1)])
  c2 <-
    write_constraints_2_all(variables = variables[1:(length(variables) - 1)])
  c3 <-
    write_constraints_3_all(variables = variables[1:(length(variables) - 1)],
                            dt = dt)
  c4 <-
    write_constraints_4_all(variables = variables[1:(length(variables) - 1)],
                            dt = dt)
  c5 <-
    write_constraints_5_all(variables = variables[1:(length(variables) - 1)],
                            dt = dt)
  c6 <-
    write_constraints_6(variables = variables[1:(length(variables) - 1)],
                        dataMatrix = dataMatrix,
                        inputs = inputs)
  c7 <-
    write_constraints_7(variables = variables[1:(length(variables) - 1)],
                        dataMatrix = dataMatrix,
                        inputs = inputs)
  c8 <-
    write_constraints_8(variables = variables[1:(length(variables) - 1)],
                        inputs = inputs,
                        pknList = pknList,
                        dt = dt)
  c9 <-
    write_loop_constraints(variables = variables[1:(length(variables) - 1)],
                           pknList = pknList,
                           inputs = inputs,
                           dt = dt)

  c10 <- write_experimental_conditions_constraints(variables = variables,
                                                   dt = dt)

  # c3 <- NULL
  # c8 <- NULL
  # c9 <- NULL

  allC <-all_constraints_wLoop(c0 = c0,
                               c1 = c1,
                               c2 = c2,
                               c3 = c3,
                               c4 = c4,
                               c5 = c5,
                               c6 = c6,
                               c7 = c7,
                               c8 = c8,
                               c9 = c(c9, c10))

  writeSolverFiles(condition = condition,
                   repIndex = repIndex,
                   oF = oF,
                   allC = allC,
                   bounds = bounds,
                   binaries = binaries,
                   generals = generals,
                   mipGAP = mipGAP,
                   poolrelGAP = poolrelGAP,
                   poolReplace = poolReplace,
                   limitPop = limitPop,
                   poolCap = poolCap,
                   poolIntensity = poolIntensity,
                   timelimit = timelimit)

  return(variables)

}
