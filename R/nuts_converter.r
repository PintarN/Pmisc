#' Convert NUTS classifications
#'
#' Convert data between 2013 and 2016 nuts classification, only NUTS3 so far and
#' only works for one variable at a time
#' 
#' 
#'
#' @param data A dataframe that includes regional data, codes in first column, data in second column, all other columns after second 
#' will be retained but not translated (just copied)
#' @param from Text that identifies the nuts classification the data comes in ("2013" or "2016")
#' @param to Text that identifies the nuts classification the data should be translated to ("2013" or "2016")
#' @param merged Either "sum" or "average" depending on what is appropriate for merged regions
#' @param weight A dataframe that gives a weight for target NUTS codes which are split, defaults to equal weighing, 
#' optional set first element 'same' to give all split regions same value as old region (appropriate for relative variables)
#' 
#' @return A dataframe with converted nuts classification
#' @export
#'



nuts_converter <- function(data, from = "2016", to = "2013", weight = tibble::tibble("equal","dummy"), merged = "sum")  
{


#checks
  if(is.character(weight)==F &   dim.data.frame(weight)[2] >2){#if weight is not correct format
    stop("Please give weight dataframe in correct format!")
  }
  
  if(dim.data.frame(data)[1]<10 | is.data.frame(data)==F){#if no data is loaded
    stop("It seems there is no data loaded!")
  }
  
  # if(nchar(data[1,1])!=5 ){#if no NUTS3  code loaded in first column
  #   stop("Please load NUTS3 codes in character in the first column of the dataframe!")
  # }
  # if(dim.data.frame(data)[2]>2){#if more that one variable and one weight variable are loaded
  #   warning("Please only load one variable! All other columns will be deleted")
  # }

  
#add 2013 code to 2016 codes
nuts_from <- from
nuts_to <- to
  

  
#load starting point
data_og <- data
names(data)[1] <- "from"
names(data)[2] <- "value"
NUTS_output <- data  %>% dplyr::select(1:2)


#NUTS_output_og <- NUTS_output

if(weight[1,1] != "equal" & dim.data.frame(weight)[1]> 1){
  names(weight)[1:2] <- c("to", "weight")
}

#dummy generating
# NUTS_output <- NUTS_input[1]
# NUTS_output[2] <- rep(100,1430)
# weight <- NUTS_input
# weight[2] <- sample(c(10,20,50,90),1430, replace = T, prob = c(0.1,0.1,0.1,0.1))
# 
# weight <- weight %>% add_row(to = c("DE915","DE919") ,weight = c(50,60))



#limit to certain NUTS level
nuts_limit <- 5

#make regions codes character
NUTS_output$from <- as.character(NUTS_output$from)

NUTS_output <- NUTS_output %>% dplyr::filter(nchar(from)== nuts_limit) %>% dplyr::distinct()


#start here for new version of output
NUTS_output <- NUTS_output  %>% dplyr::mutate(to = NA_character_)


#recode when easy
#join info from concordance table matched on from variable
NUTS_output <- NUTS_output %>% dplyr::full_join(nuts_concordance, by = c("from" = 
      as.character(names(nuts_concordance)[endsWith(names(nuts_concordance),nuts_from)]))) %>%
  dplyr::rename(code_to := paste0("Code_",nuts_to))


#get same code if nothing changed (not found in concordance)
NUTS_output <- NUTS_output %>% dplyr::mutate(to = case_when( 
  is.na(to) & is.na(code_to) & is.na(Label) & is.na(Change) ~ from,
  TRUE ~ to))


#change the codes where it was a simple recoding                                                                  
NUTS_output <- NUTS_output %>% dplyr::mutate(to = case_when(
  substr(Change,1,4) == "reco" ~ code_to,
  TRUE ~ to
)) 

#change the codes where it was a "simple" boundary shift
NUTS_output <- NUTS_output %>% dplyr::mutate(to = case_when(
  substr(Change,1,5) == "bound" ~ code_to,
  TRUE ~ to
)) 


#copy code_to value to to if available
NUTS_output <- NUTS_output %>% dplyr::mutate(to = case_when(
  is.na(code_to)==F & is.na(to) ~ code_to,
  TRUE ~ to
)) 



#need to remove Göttingen row because it is covered in row of old regions
NUTS_output <- NUTS_output %>% dplyr::mutate(delete = case_when(
  from == "DE91C" & is.na(to) ~ 1,
  TRUE ~ 0)) %>% filter(delete == 0) %>% select(-delete)



#include corresponding "to" code to "from" where regions changed quite a bit
#works for direction NUTS 2016 to NUTS 2013
if(nuts_from == "2016" & nuts_to == "2013"){
NUTS_output <- NUTS_output %>% dplyr::mutate(from = case_when(
  #Göttingen
  to == "DE915"  ~ "DE91C",
  to == "DE919"  ~ "DE91C",
  #UK regions
  to == "UKN02" ~ "UKN14", #only approximately, sufficient for metro 2013 regions I believe, the other changed UK regions cannot easily be recoded
  TRUE ~ from))

NUTS_output <- NUTS_output %>% mutate(to = case_when(
#polish regions
from == "PL926"  ~ "PL12A",
TRUE ~ to))


}else if(nuts_to == "2016" & nuts_from == "2013"){#works for direction NUTS 2013 to NUTS 2016
  NUTS_output <- NUTS_output %>% mutate(to = case_when(
  #Göttingen
  from == "DE915"  ~ "DE91C",
  from == "DE919"  ~ "DE91C",
  #UK regions
  from == "UKN02" ~ "UKN14", #only approximately, sufficient for metro 2013 regions I believe, the other changed UK regions cannot easily be recoded
  TRUE ~ to))
  
  NUTS_output <- NUTS_output %>% mutate(from = case_when(
  #polish regions
  to == "PL926"  ~ "PL12A",
  TRUE ~ from))
} 



#remove nas that are still left
NUTS_output <- NUTS_output %>% dplyr::filter(!is.na(from))
NUTS_output <- NUTS_output %>% dplyr::mutate(to = case_when(
  is.na(to) ~ "no_match", #if not match found it's discontinued or boundary changed too much
  TRUE ~ to
))


#now we want to translate codes and distribute values where regions where split etc.
#get regions that were split 
NUTS_output <-  NUTS_output %>% dplyr::group_by(from) %>% dplyr::filter(to != "no_match")  %>% dplyr::count(to) %>% dplyr::count(n, name = 'ncount', wt = n) %>% dplyr::right_join(NUTS_output, by = "from") %>%
  dplyr::rename(split_in_n = ncount) %>% dplyr::select(-n)  %>% dplyr::ungroup()
#set no_match to na again
NUTS_output <- NUTS_output %>% dplyr::mutate(split_in_n = case_when(
  to == "no_match" ~ as.integer(NA),
  TRUE ~ split_in_n
))


#add value from old regions for regions that were split
value_rem <- NUTS_output %>% dplyr::filter(is.na(value)) %>% dplyr::select(from) %>% dplyr::distinct()
value_rem <- value_rem %>% dplyr::left_join(data, by = "from")  %>% dplyr::rename(value_rem = value)
NUTS_output <- NUTS_output %>% dplyr::left_join(value_rem, by = "from")
NUTS_output <- NUTS_output %>% dplyr::mutate(value = case_when(
  is.na(value) ~ value_rem,
  TRUE ~ value
)) %>% dplyr::select(-value_rem)



#get regions that were merged
NUTS_output <-  NUTS_output %>% dplyr::group_by(to) %>% dplyr::count(from) %>% dplyr::count(n, name = 'ncount', wt = n) %>% dplyr::right_join(NUTS_output, by = "to") %>%
  dplyr::rename(merged_n = ncount) %>% dplyr::select(-n)  %>% dplyr::ungroup()
#set no_match to na again
NUTS_output <- NUTS_output %>% dplyr::mutate(merged_n = case_when(
  to == "no_match" ~ as.integer(NA),
  TRUE ~ merged_n
))


#bring dataframe in order
NUTS_output <- NUTS_output %>% dplyr::select(-merged_n,-split_in_n,merged_n,split_in_n)



#sum up weight to calculate share weight 
#rename weight column to weight
if(weight[1,1] != "equal" & dim.data.frame(weight)[1]> 1){#if we have a weight variable given
  
  
  #merge weight
  NUTS_output <- NUTS_output %>% dplyr::left_join(weight, by = "to")
  
  #remove not needed
  NUTS_output <- NUTS_output %>% dplyr::select(from,to,value,weight,merged_n,split_in_n)
  
  
  #lose all regions that don't have a weight given
  NUTS_lost <- NUTS_output
  NUTS_output <- NUTS_output %>% dplyr::filter(is.na(weight)==F)
  NUTS_lost <- dplyr::setdiff(NUTS_lost$from, NUTS_output$from)

NUTS_output <- NUTS_output %>% dplyr::group_by(from) %>% dplyr::summarise(weightsum = sum(weight)) %>% dplyr::right_join(NUTS_output, by = "from") %>%
  dplyr::select(-weightsum,weightsum)


#sum up value for merged regions, if that is appropriate
if(merged == "sum"){
NUTS_output <- NUTS_output %>% dplyr::group_by(to) %>% dplyr::summarise(valuesum = sum(value)) %>% dplyr::right_join(NUTS_output, by = "to") %>%
  dplyr::select(-valuesum,valuesum)
}
if(merged == "average"){#average value for merged regions, if that is appropriate
  NUTS_output <- NUTS_output %>% dplyr::group_by(to) %>% dplyr::summarise(valuesum = mean(value)) %>% dplyr::right_join(NUTS_output, by = "to") %>%
    dplyr::select(-valuesum,valuesum)
}

#now redistribute value according to weight
NUTS_output <- NUTS_output %>% dplyr::mutate(value_new = case_when(
  weight != weightsum ~ value * (weight/weightsum), #in case of split regions
  value != valuesum ~ valuesum, #in case of merged regions
  to == "no_match" ~ as.double(NA),
  TRUE ~ value
)) %>% dplyr::select(-value_new, value_new)

#give warning if some regions dont have a weight but are not merged
warning <- NUTS_output %>% dplyr::filter(is.na(weight) & split_in_n != 1)
if(dim.data.frame(warning)[1]>0){
  warning("There seem to be regions that are split that don't have a given weight to split them!")
}


#join old data
old_names <- names(data_og)

NUTS_output <- NUTS_output %>% dplyr::left_join(data_og %>% dplyr::select(1,3:length(old_names)), by = c('from' = old_names[1]))

return_list <- list(NUTS_output = NUTS_output,
                    NUTS_lost = NUTS_lost)


}else if(weight[1,1] == "equal"){#end weighing which is not equal, do equal weighing 

#calculate equal weighing if no weight given

  NUTS_output <- NUTS_output %>% dplyr::mutate(weight = case_when(
    to == "no_match" ~ as.double(NA),
    TRUE ~ 1
  ))
  
  #remove not needed
  NUTS_output <- NUTS_output %>% dplyr::select(from,to,value,weight,merged_n,split_in_n)
  
  
  #create weightsum for equal weighing
  NUTS_output <- NUTS_output %>% dplyr::group_by(from) %>% dplyr::summarise(weightsum = sum(weight)) %>% dplyr::right_join(NUTS_output, by = "from") %>%
    dplyr::select(-weightsum,weightsum)
  
  
  #sum up value for merged regions, if that is appropriate
  if(merged == "sum"){
    NUTS_output <- NUTS_output %>% dplyr::group_by(to) %>% dplyr::summarise(valuesum = sum(value)) %>% dplyr::right_join(NUTS_output, by = "to") %>%
      dplyr::select(-valuesum,valuesum)
    
    #set valuesum na for no match
    NUTS_output <- NUTS_output %>% dplyr::mutate(valuesum = case_when(
      to == "no_match" ~ as.double(NA),
      TRUE ~ valuesum
    ))
    
  }
  if(merged == "average"){#average value for merged regions, if that is appropriate
    NUTS_output <- NUTS_output %>% dplyr::group_by(to) %>% dplyr::summarise(valuesum = mean(value)) %>% dplyr::right_join(NUTS_output, by = "to") %>%
      dplyr::select(-valuesum,valuesum)
    
    #set valuesum na for no match
    NUTS_output <- NUTS_output %>% dplyr::mutate(valuesum = case_when(
      to == "no_match" ~ as.double(NA),
      TRUE ~ valuesum
    ))
  }
  
  #now redistribute value according to equal weight
  NUTS_output <- NUTS_output %>% dplyr::mutate(value_new = case_when(
    weight != weightsum ~ value * (weight/weightsum), #in case of split regions
    value != valuesum ~ valuesum, #in case of merged regions
    to == "no_match" ~ as.double(NA),
    TRUE ~ value
  )) %>% dplyr::select(-value_new, value_new)
  
  
  #rename to keep variable name
  NUTS_output <- NUTS_output %>% dplyr::rename(!!paste0(colnames(data_og)[2], '_old') := value,
                                               !!paste0(colnames(data_og)[2], '_new') := value_new)
  
  #join old data
  old_names <- names(data_og)
  
  NUTS_output <- NUTS_output %>% dplyr::left_join(data_og %>% dplyr::select(1,3:length(old_names)), by = c('from' = old_names[1]))
  
  
  return_list <- list(NUTS_output = NUTS_output)
  
}else if(weight[1,1] == "same"){#end equal weighting , do if split is giving the same value  to each region

  #calculate with no weighing, giving each regions that is split the same value as previous bigger region 
  
  NUTS_output <- NUTS_output %>% dplyr::mutate(weight = case_when(
    to == "no_match" ~ as.double(NA),
    TRUE ~ 1
  ))
  
  #remove not needed
  NUTS_output <- NUTS_output %>% dplyr::select(from,to,value,weight,merged_n,split_in_n)
  
  
  #create weightsum for equal weighing
  NUTS_output <- NUTS_output %>% dplyr::group_by(from) %>% dplyr::summarise(weightsum = sum(weight)) %>% dplyr::right_join(NUTS_output, by = "from") %>%
    dplyr::select(-weightsum,weightsum)
  
  
  #sum up value for merged regions, if that is appropriate
  if(merged == "sum"){
    NUTS_output <- NUTS_output %>% dplyr::group_by(to) %>% dplyr::summarise(valuesum = sum(value)) %>% dplyr::right_join(NUTS_output, by = "to") %>%
      dplyr::select(-valuesum,valuesum)
    
    #set valuesum na for no match
    NUTS_output <- NUTS_output %>% dplyr::mutate(valuesum = case_when(
      to == "no_match" ~ as.double(NA),
      TRUE ~ valuesum
    ))
    
  }
  if(merged == "average"){#average value for merged regions, if that is appropriate
    NUTS_output <- NUTS_output %>% dplyr::group_by(to) %>% dplyr::summarise(valuesum = mean(value)) %>% dplyr::right_join(NUTS_output, by = "to") %>%
      dplyr::select(-valuesum,valuesum)
    
    #set valuesum na for no match
    NUTS_output <- NUTS_output %>% dplyr::mutate(valuesum = case_when(
      to == "no_match" ~ as.double(NA),
      TRUE ~ valuesum
    ))
  }
  
  #now redistribute value according to no weight
  NUTS_output <- NUTS_output %>% dplyr::mutate(value_new = case_when(
    weight != weightsum ~ value, #in case of split regions, do nothing
    value != valuesum ~ valuesum, #in case of merged regions
    to == "no_match" ~ as.double(NA),
    TRUE ~ value
  )) %>% dplyr::select(-value_new, value_new)
  
  #rename to keep variable name
  NUTS_output <- NUTS_output %>% dplyr::rename(!!paste0(colnames(data_og)[2], '_old') := value,
                                               !!paste0(colnames(data_og)[2], '_new') := value_new)
  
  #join old data
  old_names <- names(data_og)
  
  NUTS_output <- NUTS_output %>% dplyr::left_join(data_og %>% dplyr::select(1,3:length(old_names)), by = c('from' = old_names[1]))
  
  
  return_list <- list(NUTS_output = NUTS_output)
   
}
  

return(return_list)
}#end function