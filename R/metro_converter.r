#' Convert NUTS codes to Metropolitan regions
#'
#' Convert NUTS3 (NUTS 2013) regions to metropolitan regions (2013)
#' 
#' 
#' @param data A list of NUTS3 codes that should be translated to metro regions, make sure you don't leave out codes that belong to metro regions

#' 
#' @return A dataframe with converted nuts codes to metro codes
#' @export
#'



metro_converter <- function(data)  
{

  metro_output <- data[1]
  names(metro_output)[1] <- "Reg_code"
  
  
  #some codes might need to be corrected
  metro_output <- metro_output  %>% dplyr::mutate(Reg_code = case_when(
    Reg_code == "UKI11" ~  "UKI31", 
    Reg_code == "UKI12" ~  "UKI41",
    Reg_code == "UKI21" ~  "UKI51",
    Reg_code == "UKI22" ~  "UKI61",
    Reg_code == "UKI23" ~  "UKI71",
    Reg_code == "UKJ23" ~  "UKJ25",
    Reg_code == "UKJ24" ~  "UKJ27",
    Reg_code == "UKJ33" ~  "UKJ35", 
    Reg_code == "UKJ42" ~  "UKJ43",
    Reg_code == "LIZZZ" ~  "LI000",
    substr(Reg_code,1,4) == "IS01" ~ "IS001",
    substr(Reg_code,1,4) == "IS02" ~ "IS002",
    TRUE ~ Reg_code
  ))
  
  
  Encoding(metro_concordance$metro_name) <- "UTF-8"
  
  
  #new way of metro matching that keeps non-matched
  metro_output <- metro_output %>% dplyr::left_join(dplyr::select(metro_concordance, -country_code), by = c("Reg_code" = "NUTS_ID"))
  
  
  

}#end function