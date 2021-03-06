% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assignPROGENyScores.R
\name{assignPROGENyScores}
\alias{assignPROGENyScores}
\title{\code{assignPROGENyScores}}
\usage{
assignPROGENyScores(progeny = progeny, 
                                progenyMembers = progenyMembers, 
                                id = "gene", access_idx = 1)
}
\arguments{
\item{progeny}{contains the progeny scores as obtained from \code{runPROGENy}}

\item{progenyMembers}{Contains the list of members for each PROGENy pathway}

\item{id}{Contains the members identifiers (default: gene)}

\item{access_idx}{index of the sample to consider (default set to 
access_idx = 1 - assign progeny scores the first sample (if all, set 
access_idx = 1:nrow(progeny)))}
}
\value{
This function is used to account for the PROGENy scores in the objective 
function. It creates a list object with progeny scores for a selected set of 
samples (by default for all of them)
}
\description{
\code{assignPROGENyScores}
}
\examples{
library(CARNIVAL)
library(progeny)

expr <- as.matrix(read.csv(system.file("extdata", "human_input.csv", 
                                              package = "progeny"), 
                                              row.names = 1))

human_def_act <- progeny(expr, scale = TRUE,  organism = "Human", top = 100, 
                         perm = 10000, z_scores = FALSE)


## loading the progeny members to assign the weights
load(file = system.file("progenyMembers.RData",package="CARNIVAL"))
# load(file = "progenyMembers.RData")

## now assigning the PROGENy weights to pathway members only for the first 
## sample which we can consider for the CARNIVAL analysis
weightObj <- assignPROGENyScores(progeny = human_def_act, 
                                 progenyMembers = progenyMembers, 
                                 id = "gene", access_idx = 1)

}
\author{
Enio Gjerga
}