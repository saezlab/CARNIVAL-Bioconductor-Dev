#'\code{buildDataMatrix}
#'
#'This function returns the data matrix containing the data for running CARNIVAL
#'and a set of identifiers for Targets, Measured and Un-measured nodes.
#'

buildDataMatrix <- function(data = data, pknList = pknList, inputs = inputs) {

  colnames(pknList) <- c("X1", "X2", "X3")

  # get all unique species in the PKN
  allSpecies <- unique(c(as.character(pknList$X1), as.character(pknList$X3)))

  if(ncol(inputs) > 0){

    # Every species that is in inputs and in the PKN
    ts <- intersect(colnames(inputs), allSpecies)

  }

  # Every measured species that is in the PKN
  ds <- intersect(colnames(data), allSpecies)

  # PKN species that are not measured
  dn <- setdiff(allSpecies, ds)

  # Matrix with columns named by nodes and their type
  # (ds if measured, dn otherwise)
  # rows represent the quantized measured state
  dataMatrix <- matrix(0, nrow = nrow(data), ncol = length(allSpecies))

  dnNames <- paste0("DN:", dn)
  dsNames <- paste0("DS:", ds)

  colnames(dataMatrix) <- c(dnNames, dsNames)

  # If a measured species is not in the PKN ignore it
  if(length(which(is.element(el = colnames(data),
                             set = setdiff(colnames(data), ds)))) > 0){

    dataMatrix[, (length(dn)+1):length(allSpecies)] <-
      as.matrix(data[, -which(is.element(el = colnames(data),
                                         set = setdiff(colnames(data), ds)))])

  }
  else{

    # Add the measurements to the matrix
    dataMatrix[, (length(dn)+1):length(allSpecies)] <- as.matrix(data)

  }

  # Quantize measurements based on their sign
  # => only values === 0 will be 0, anything else either -1 or 1
  dataMatrixSign <- sign(dataMatrix)

  # Indices for non-measured PKN species
  dnID <- 1:length(dn)

  # Indices for measured PKN species
  dsID <- (length(dn)+1):length(allSpecies)

  # Indices for input species
  tsID <- which(is.element(el = c(dn, ds), set = ts))

  # Concatenate result & return
  res <- list(dataMatrix=dataMatrix, dataMatrixSign=dataMatrixSign, dnID=dnID,
              dsID=dsID, tsID=tsID, species=c(dn, ds))

  return(res)

}
