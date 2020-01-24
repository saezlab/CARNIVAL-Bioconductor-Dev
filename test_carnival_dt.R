# Title     : CARNIVAL example to test dt function
# Objective : Runs CARNIVAL-dt toy model
# Created by: Nicolas Peschke
# Created on: 16.01.2020

# Script to try CARNIVAL
library(CARNIVAL)
library(dplyr) # load dplyr library
library(readr) # load readr library
library(igraph) # load igraph library
library(readxl) # load readxl library
library(tidyr)

# base_directory <- "D:/MoBi_Studium/Master/internships/saez/gitRepos/CARNIVAL_dt/"
# data_directory <- "D:/MoBi_Studium/Master/internships/saez/gitRepos/CARNIVAL_dt/data/test_data/"
solver_path <- "C:/Program Files/IBM/ILOG/CPLEX_Studio1210/cplex/bin/x64_win64/cplex.exe"

# test_network <- readr::read_tsv(paste0(data_directory, "test_network.tsv"))
# test_measurements <- readr::read_tsv(paste0(data_directory, "test_measurements.tsv"))
# test_input <- readr::read_tsv(paste0(data_directory, "test_input.tsv"))

# test_network <- readr::read_tsv(paste0(data_directory, "test_network_big.tsv"))
# test_measurements <- readr::read_tsv(paste0(data_directory, "test_measurements_big.tsv"))
# test_input <- readr::read_tsv(paste0(data_directory, "test_input_big.tsv"))

test_network <- tibble(Source = c("P", "N1", "M1", "N2", "M2", "N3"),
                       Effect = c(1, 1, -1, 1, 1, 1),
                       Target = c("N1", "M1", "N2", "M2", "N3", "N1"))

test_measurements <- tibble(M1 = c(1, -1),
                            M2 = c(-1, 1))

test_input <- tibble(P = c(1))

# cplex solver
toy_result <- runCARNIVAL(solverPath = solver_path,
                          netObj = test_network,
                          measObj = test_measurements,
                          inputObj = test_input,
                          mipGAP = 0,
                          poolrelGAP = 0,
                          DOTfig = TRUE,
                          # experimental_conditions = c(1, 2),
                          dt = TRUE,
                          solver = "cplex")

debug <- tibble(variable = character(),
                explanation = character(),
                time = integer())

for (i in 1:length(toy_result$variables)) {
  debug <- add_row(debug,
                   variable = toy_result$variables[[i]][[1]],
                   explanation = toy_result$variables[[i]][[2]],
                   time = i)
}

