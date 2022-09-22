#' Calculate RSI index (Menzel and Maicher, 2017)
#' 
#' Calculate RSI index which is an alternative but related specialisation index derived from incidence matrices
#'
#' @param data An incidence matrix with agent in rows and activities in columns.
#' @param binary Logical if index should be binarised with 1 signalling specialisation. Default is binary.
#' @param threshold Threshold for binarisation, Two strings; either standard deviations ("std") plus the factor with which std is multiplied (second element)
#'  or "0" for regular binarising at zero cutoff. Default is "std","1".
#' @param shift Logical indicating whether the index should be shifted to range between 0 and 2 instead of -1 and 1
#'
#' @return A sparse matrix (agent x activities) with RSI values
#' @export
#'


RSI <- function(data, binary = T, threshold = c("std","1"), shift = F) 
{

  #calculate own share of agents activities
  RSI_individual <- (data / Matrix::rowSums(data))
  
  #calculating second part: total activities to field c excluding agent a / total activities excluding agents a's
  RSI_excluding_numerator <- matrix(rep(Matrix::colSums(data), dim(data)[1]), nrow = dim(data)[1], byrow = T)  - data     
  
  RSI_excluding_denominator <-  sum(Matrix::colSums(data)) - data
  
  RSI <- RSI_individual - (RSI_excluding_numerator / RSI_excluding_denominator)
  
  
  if(binary == T & shift == T){
    warning("Shift makes no sense when binarising the values of RSI!")
    stop()
  }
  
  if(shift == T){#if shift is true, shifted RSI values are returned
    
    result <- RSI + 1
    
  }
  
  
  if(binary == F & shift == F){#if shift and binary is not true
    
    result <- RSI 
    
  }
  
  if(binary == T & shift == F){#if binarised
    
    if(threshold[1] == "std"){#regular case with agent specific std as specialisation threshold
      
      RSI[RSI<0] <- NA
      stds <- matrixStats::rowSds(as.matrix(RSI), na.rm = T)
      
      result <- RSI - stds * as.numeric(threshold[2])
      result[result <= 0] <- 0
      result[result > 0] <- 1
      
      #set na to 0
      result[is.na(result)] <- 0
      
      #give warning if a row or column has no specialisation at all
      if(sum(Matrix::rowSums(result)<1)>0 | sum(Matrix::colSums(result)<1)>0){
        warning("Either a row or column has no specialisation at all!")
      }
     
    }else{#if not decided by standard deviation but just zero cutoff
      
      result <- RSI 
      
      result[result <= 0] <- 0
      result[result > 0] <- 1
      
      #give warning if a row or column has no specialisation at all
      if(sum(Matrix::rowSums(result)<1)>0 | sum(Matrix::colSums(result)<1)>0){
        warning("Either a row or column has no specialisation at all!")
      }
      
      }
     }#end binarised
  
  
  
  
####END####
  
  result <- Matrix::Matrix(result, sparse = T)
  
  
  
  
  return(result)
}





