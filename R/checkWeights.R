#'\code{checkWeights}
#'
#'@param weightObj Weight object
#'
#'@return Error message in case of errors in the inputs
#'
#'@export

checkWeights <- function(weightObj = weightObj, netObj = netObj){
  
  nSpecies = unique(c(as.character(as.matrix(netObj)[, 1]), 
                      as.character(as.matrix(netObj)[, 3])))
  
  if (is.null(weightObj)) {
    return(NULL)
  } else {
    allowedClass = c("matrix", "data.frame")
    if(!(any(class(weightObj)%in%allowedClass))){
      stop("Measurement object should either be of matrix or data.frame class")
    } else {
      if(ncol(weightObj)>0){
        mSpecies = colnames(weightObj)
        
        idx = which(mSpecies%in%nSpecies)
        idx2rem = setdiff(1:length(mSpecies), idx)
        
        if(length(idx2rem)==length(mSpecies)){
          stop("Something wrong with your measurements object/network object. 
               No measurements is present in the network")
        } else {
          if(length(idx2rem)>0){
            if((nrow(weightObj)==1) && (class(weightObj)=="matrix")){
              weightObj = weightObj[, -idx2rem]
              weightObj = t(as.matrix(weightObj))
            } else {
              weightObj = weightObj[, -idx2rem]
            }
          }
        }
    } else {
      stop("Something wrong with your measurements object. Please check.")
    }
  }
  }
  
  return(weightObj)
  
}