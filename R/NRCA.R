#' Calculate normalised revealed comparative advantage index (NRCA, Yu et al. 2009)
#' 
#' Calculate NRCA index which is an alternative specialisation index derived from incidence matrices
#'
#' @param data An incidence matrix with agent in rows and activities in columns.
#' @param binary Logical if index should be binarised with 1 signalling specialisation. Default is non-binary.
#' @param transform String indicating whether the index should be transformed to 
#' range between 0 and 2 (in theoretical extremes) instead of -1/4 & 1/4 (default)
#' scaled between 0 and 1 with actual min and max values (scaled)
#' be in standard deviations, centered at 1 (std), cut at 0 lower bound
#' @return A sparse matrix (agent x activities) with NRCA values
#' @export
#'


NRCA <- function(data, binary = F, transform = 'regular') 
{
  
  #stop if binary and transform is chosen
  if(binary == T & transform == T){
    
    stop("Don't transform when binarising!")
    
  }
  
  
  #following Yu et al. 2009 notation
  
  #set as regular matrix
  data <- as.matrix(data)
  
  #total sum of activites, E 
  E <- sum(data)
  
  #total of activities by all agents
  E_j <- colSums(data)
  
  #total of agents by all activites
  E_i <- rowSums(data)
  
  #global share of total activites, E_ij / E
  share <- data/E
  
  #create matrix of second term in equation (6), comparative-advantage-neutral level, activity sum * agent sum / E^2, 
  neutral <-  ((E_i %*% t(E_j))/E^2)  
  dimnames(neutral) <- dimnames(data)
  
  
  NRCA <- share - neutral
  
  
  #binarise, then it's the same as RCA 
  if(binary == T){
    
    NRCA_bin <- NRCA
    
    NRCA_bin[NRCA <= 0] <- 0
    NRCA_bin[NRCA > 0] <- 1
    
    NRCA <- NRCA_bin
    
  }
  
  
  #transform
  if(transform == 'regular' & binary == F){
    
    NRCA_trans <- (NRCA / (1/4)) + 1
    
    NRCA <- NRCA_trans
    
  }
  
  if(transform == 'scaled' & binary == F){
    
    NRCA_trans <- NRCA + 1 #so no negative values
    
    NRCA_trans <- (((NRCA_trans - min(NRCA_trans)) / (max(NRCA_trans) - min(NRCA_trans)))) *1
    
    NRCA <- NRCA_trans

  }
  
  if(transform == 'std' & binary == F){
      
      
      std <- matrixStats::rowSds(NRCA)
      
      NRCA_trans <- ((NRCA) / (std))  + 1
      
      #cut off zero
      NRCA_trans[NRCA_trans < 0] <- 0
      
      
      NRCA <- NRCA_trans
      
      #get only positive values
      #NRCA <- NRCA + abs(min(NRCA))
      
      #divide by mean
      #NRCA <- NRCA / mean(NRCA)
      
      #NRCA <- (NRCA - min(NRCA) / (max(NRCA) - min(NRCA)))
      
    }
   
    
    
  
  
  
  
  #check distribution
  # df <- as.matrix(NRCA_trans)
  # df <- as_tibble(df)
  # 
  # longdf <- pivot_longer(df, cols = 1:35, names_to = "tech")
  # 
  # 
  # distr <- ggplot(data = longdf %>% filter(value >0), aes(x = value)) +
  #   geom_density()
  # 
  # distr
  
  
  
  
####END####
  
  result <- Matrix::Matrix(NRCA, sparse = T)
  
  
  
  return(result)
}





