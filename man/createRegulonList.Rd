% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createRegulonList.R
\name{createRegulonList}
\alias{createRegulonList}
\title{\code{createRegulonList}}
\usage{
createRegulonList(regulon_table = regulon_table)
}
\arguments{
\item{regulon_table}{the regulon matrix with columns the source, sign and
target of the TF interaction.}
}
\value{
This function creates a regulon list similar to vipper from an interaction
matrix. The interaction matrix contains as columns the source, sign and the 
target of the interaction
}
\description{
\code{assignPROGENyScores}
}
\examples{
library(CARNIVAL)
library(OmnipathR)

regulon_df <- 
  import_TFregulons_Interactions(select_organism = 9606)

regulon_df <- 
  regulon_df[which((regulon_df$is_stimulation+regulon_df$is_inhibition)==1), ]

regulon_table <- matrix(data = , nrow = nrow(regulon_df), ncol = 3)
regulon_table[, 1] <- regulon_df$source_genesymbol
regulon_table[which(regulon_df$is_stimulation==1), 2] = "1"
regulon_table[which(regulon_df$is_inhibition==1), 2] = "-1"
regulon_table[, 3] <- regulon_df$target_genesymbol

regulons <- createRegulonList(regulon_table = regulon_table)

}
\author{
Enio Gjerga
}