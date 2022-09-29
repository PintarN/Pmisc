#' Calculate the Economic Complexity Index (ECI) 
#' 
#' Calculate the ECI as in Hidalgo and Hausman (2009) but using the eigenvector method introduced by Caldarelli et al. (2012).
#' I also adapt the ECI in the following way:
#' - I first calculate ECI and PCI as in Cardarelli et al. (2012)
#' - Because the direction of an eigenvector is not defined, it might be the case that we get "flipped" results where the ECI we get is actually -ECI (from what we expect)
#' - To remedy this, I calculate the pearson correlation of PCI with the ubiquity of the activity (product etc.). In case we get a positive correlation (which is against theoretical expectations), we multiply the eigenvector with -1 to "flip" the result
#' - After that, I calculate the spearman correlation between PCI rank (of activities) and the ECI rank. This needs to be positive. If not, I "flip" the ECI  
#' 
#' 
#' @param data A (named) adjacency matrix of specialisations, with location in rows and activities in columns. These can be specialisations according to RCA or any other metric of specialisation that yields values that signal specialisations of locations in activites. This might be discreet (e.g. RCA, RSI) or real-valued (e.g. NRCA). See EconGeo::RCA e.g. for RCA or other functions in this package for other metrics of specialisations. See also Pintar and Essletzbichler (2022).   
#' @param description  Two strings indicating the name of dimensions (locations and activities, in this order), e.g. reg_code and tech_class (default).
#' @param scale A string indicating if the results should be scaled ("scale") between 0 and 100 or left as is. 
#' @return A list with tibble dataframes of complexity values for locations (ECI) and activities (PCI)
#' @export
#'


ECI <- function(data, description = c('reg_code', 'tech_class'), scale = 'scale'){
  
  
###plausibility checks####  
  
  #make sparse matrix
  M <- Matrix::Matrix(data, sparse = T)
  
  #check if named adjacency matrix is given
  
  if( length(M@Dimnames[[1]]) != M@Dim[1] |
      length(M@Dimnames[[2]]) != M@Dim[2]){
    stop('Dimension names in adjacency matrix are not correct. Check!')
  }  
  
  #check that there are multiple locations and activities
  
  if(M@Dim[1]<2 | M@Dim[2]<2){
    stop('Need to have at least two locations and activities!')
  }
  

  
###body of function####
  
  ##calculate ECI with eigenvector method, see Pintar and Essletzbichler (2022)
  
  
  #preparation
  M.hat <- M / Matrix::rowSums(M)
  M.t.hat <- t(M) / Matrix::rowSums(t(M))
  M.tilde <- M.hat %*% M.t.hat
  T.tilde <- M.t.hat %*% M.hat
  
  #create location and activities df
  
  #activities
  indicators.act <- tidyr::tibble(!!sym(description[2]) := colnames(M))
  
  #locations
  indicators.loc <- tidyr::tibble(!!sym(description[1]) := rownames(M))
  
  #get ubiquity from M matrix
  indicators.act$ubi <- as.numeric(Matrix::colSums(M >= 1))
  
  #get div  from M matrix
  indicators.loc$div <- as.numeric(Matrix::rowSums(M >= 1))
  
  
  #eigenvector method for PCI first
  
  spectral.T.tilde <- eigen(T.tilde)
  
  #use second eigenvector
  indicators.act$PCI <- as.numeric(spectral.T.tilde$vectors[,2])
  
  #check if PCI is "correct", cor with ubiquity is negative (as expected from theory)
  if(cor(indicators.act$PCI, indicators.act$ubi, use = "pairwise.complete.obs") > 0){
    indicators.act$PCI <- indicators.act$PCI * -1}
  
  #scale if needed
  if(scale == 'scale'){
    indicators.act$PCI_scaled <- 100 * ((indicators.act$PCI - min(indicators.act$PCI)) / (max(indicators.act$PCI) - min(indicators.act$PCI)))    
  }
  
  #calculate rank
  indicators.act$rank_PCI <- rank(-indicators.act$PCI, ties.method = 'random')
  
  
  #eigenvector method for ECI
  
  spectral.M.tilde <- eigen(M.tilde)
  
  #use second eigenvector
  indicators.loc$ECI <- as.numeric(spectral.M.tilde$vectors[,2])
  
  #scale if needed
  if(scale == 'scale'){
    indicators.loc$ECI_scaled <- 100 * ((indicators.loc$ECI - min(indicators.loc$ECI)) / (max(indicators.loc$ECI) - min(indicators.loc$ECI)))    
  }
  
  #calculate rank
  indicators.loc$rank_ECI <- rank(-indicators.loc$ECI, ties.method = 'random')
  
  
  
  #check if ECI is "correct", check if cor with PCI is positive
  #need to bring info of ECI and PCI together
  M.df <- as_tibble(t(as.matrix(M)))
  M.long.df <- M.df %>% tidyr::pivot_longer(cols = tidyr::everything(), names_to = description[1], values_to = 'spec')
  M.long.df <- M.long.df %>% dplyr::mutate( !!sym(description[2]) := rep(row.names(t(M)), dim(M)[1]))
  
  M.long.df <- M.long.df %>% dplyr::left_join(indicators.loc %>% select(!!sym(description[1]), rank_ECI), by = description[1])
  M.long.df <- M.long.df %>% left_join(indicators.act %>% select(!!sym(description[2]), rank_PCI), by = description[2])
  M.long.df <- M.long.df %>% filter(spec >= 1)
  
  
  #calculate correlation between ranke ECI and rank PCI
  indicators.loc$cor <- (M.long.df %>% select(rank_ECI, rank_PCI) %>% distinct()  %>%
                                                                 as.matrix() %>% cor(method = 'spearman'))[2,1]
  
  #change sign of ECI if correlation is negative
  if(indicators.loc$cor[1] < 0){
    indicators.loc$ECI <- indicators.loc$ECI * -1
    
    #calculate rank and scaled again
    indicators.loc$rank_ECI <- rank(-indicators.loc$ECI, ties.method = 'random')
    indicators.loc$ECI_scaled <- 100 * ((indicators.loc$ECI - min(indicators.loc$ECI)) / (max(indicators.loc$ECI) - min(indicators.loc$ECI)))    
  }
    
  
  #create output file
  output.list <- list(indicators.loc,
                      indicators.act)
  
  names(output.list) <- (description)
  
  
  return(output.list)
  
}#end function
###END####







