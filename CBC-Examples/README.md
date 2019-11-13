# CBC implementation of CARNIVAL


Here we show two simple exmples (one toy and one real-case study) about how to run CARNIVAL with the free [CBC](https://projects.coin-or.org/Cbc) solver.

**This repository it is not at it's final version and it will be constantly updated in the following weeks before merging it to the main
CARNIVAL branch.**

## Installation

Before running CARNIVAL with cbc implementation, please make sure first to download and install this branch as follows:

```R
# Install CARNIVAL from Github using devtools
# install.packages('devtools') # in case devtools hasn't been installed
library(devtools)
install_github('saezlab/CARNIVAL', ref = 'cbc')
# or download the source file from GitHub and install from source
install.packages('path_to_extracted_CARNIVAL_directory', repos = NULL, type="source")
```

## Cbc COIN-OR

The *cbc executables* can be downloaded [here](https://bintray.com/coin-or/download/Cbc/) for different OS platforms (Windows, MAC and Linux). 
The shown examples have only been tested with MAC-OS, but for any problems running on different platforms, please create a new 
[GitHub Issue](https://github.com/saezlab/CARNIVAL/issues) and we will be prompt to help.

After downloading the compressed files, please unzip the downloaded documents, while the executables can be found in `bin/cbc` directory. The
executables can then be stored at any directory in the machine that the user wishes.

## Examples

The `example.R` script contains two examples about how to run CARNIVAL with the free CBC solver implementation.

The first example consists on how tu run CARNIVAL-CBC with known perturbation inputs over a toy case study:

+ [1] Loading CARNIVAL `R` package after having it installed as explained above.
+ [5] Setting name of the inputs file for the first example (modelling the effects of perturbation over specific proteins in the cell).
+ [6] Setting the name of the measurements file for the first example.
+ [7] Setting the name of the file containing signed and directed protein interactions.
+ [8] Setting the name of the directory where we can store the results we obtain.
+ [10] Running CARNIVAL-CBC for the first case-study (we make sure to specify the cbc solver as our solver to use: `solver = "cbc"` and also specify the path where we have stored our cbc solver: `solverPath = "~/Desktop/cbc"`).

The second example consists on running CARNIVAL-CBC without having to know the perturbation effects and also considering the PROGENy weights
after havung run PROGENy to estimate pathway activities.0

+ [14] Setting the name of the measurements file for the first example (in this case inferred TF activities from DOROTHeA).
+ [15] Setting the name of the file containing signed and directed protein interactions (in this case the interactions were retrieved from [OmniPath](http://omnipathdb.org/) with proteins as uniprot ID's).
+ [16] Setting the name of the file containing our PROGENy scores.
+ [16] Setting the name of the directory where we can store the results we obtain.
+ [17] Running CARNIVAL-CBC for the first case-study (we make sure to specify we are using the inverse CARNIVAL through setting `inverseCR = TRUE`).

More on CARNIVAL can be found in the [main branch of the package](https://github.com/saezlab/CARNIVAL) and the [paper](https://www.biorxiv.org/content/10.1101/541888v2.full).
