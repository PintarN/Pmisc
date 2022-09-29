#' Load REGPAT data and prepare for further handling 
#' 
#' Load REGPAT data from text files and creates a merged dataframe with some optional adjustments
#'
#' @param path Vector of two (three with CPC) paths to two REGPAT text files (year_tech and inventor_region), starting with year_tech. For delim "|" should be used. If CPC classes are used, you also need to give the path to the CPC file (in third element).
#' @param region Text or number input to set regionalisation. Options are: "0" - "3" for NUTS regions and "metro" for matching with metro regions by EUROSTAT.
#' @param disc_periphery A string indicating if periphery (when metro matching) data is deleted or not, defaults to "yes".
#' @param techclass Three strings indicating which tech class to use (IPC or CPC or Schmoch (2008)), the digit cutoff (or name of classification level (e.g. subclass)) for main tech class (1-8, not used in Schmoch matching) and thirdly the name of level for long tech class (e.g. 'maingroup'). Defaults to no cutting.
#' @param years Vector of year intervals used (like so: c(2000,2012). Default is "all" years.
#' @param periodisation Text to set which periodisation should be used. Options are: "prio_year" (default) or "app_year" for new naming convention
#' @param regionlist A list of regions which should be used, all others will be removed. Column name for region codes should be "reg_code. Needs to match with regionalisation. Defaults to "no", meaning all regions are retained.
#' @param disc_unclass A string indicating whether unclassified regions should be removed, defaults to "yes".
#' 
#' @return A list of dataframes of REGPAT data
#' @export
#'


load_regpat <- function(path, region, disc_periphery = "yes", regionlist = "no", techclass = c("all","all") , years = "all", periodisation = "prio_year", disc_unclass = "yes") 
{

    
#Load patent data; first text file with year, tech info

if(techclass[1]=="IPC" | techclass[1]=="Schmoch"){#if IPC classes or Schmoch classification is used (not CPC) 
year_tech_df <-  readr::read_delim(file = path[1], delim = "|", col_names = T)
}else{
if(techclass[1]=="CPC"){
  year_tech_df <-  readr::read_delim(file = path[3], delim = "|", col_names = T)
}else{
  warning("No correct technicial classification given!")
}
}
  
  
  
#Load patent data; second text file with region and inventor info
#region_inv_df <- as.data.frame(read.table(path[2], sep=";", header=T, stringsAsFactors = T))  
region_inv_df <-  readr::read_delim(file = path[2], delim = "|", col_names = T)





#drop years not in period interval given, if available
if(years[1] == "all"){#if no year period given, use all years
  
  }else{
    if(periodisation == "prio_year"){
      year_tech_df <- year_tech_df %>% dplyr::filter(prio_year >= years[1] & prio_year <= years[2])
    }else{
  year_tech_df <- year_tech_df %>% dplyr::filter(app_year >= years[1] & app_year <= years[2])
    }
  }

  
#merge
merged_df <- region_inv_df %>% dplyr::group_by(appln_id) %>% dplyr::inner_join(year_tech_df, by = "appln_id")
merged_df <- dplyr::as_tibble(merged_df)
merged_og_df <- merged_df

#count number of unique patens before removing any
no_patents_full <- length(unique(merged_df$appln_id))

#rename IPC to tech_class so it is generic
if(techclass[1]=="IPC" | techclass[1]=="Schmoch"){
merged_df <- merged_df %>% dplyr::rename(tech_class = IPC) #rename generic
}else{
  merged_df <- merged_df %>% dplyr::rename(tech_class = CPC) #rename generic
}

#remove blanks in IPC/CPC codes
merged_df$tech_class <- gsub(" ","",merged_df$tech_class)


#drop patents with tech_class "" 
notechclass_df <- merged_df %>% dplyr::filter(tech_class == "" | is.na(tech_class))


#lose rows without IPC/CPC code
merged_df <- dplyr::anti_join(merged_df, notechclass_df, by = "appln_id") 


#count number of unique patents after removing no IPCS
no_patents_afternotechclass <- length(unique(merged_df$appln_id))


#give warning if more than 1% no ipcs/cpcs
if((1-(no_patents_afternotechclass / no_patents_full)) > 0.01 ){ 
  warning("More than 1% of patents have no IPC/CPC code")
}


####technological classification cleaning/matching####


#if not IPC/CPC cutting nor Schmoch matching is used
#create dummy tech_name because we don't match it with names
merged_df$tech_name <- rep("atechhasnoname",dim.data.frame(merged_df)[1])
#just create tech class long
merged_df$tech_class_long <- merged_df$tech_class
merged_df <- dplyr::distinct(merged_df) #get rid of duplicates (also to save memory)

#Cut IPC/CPC classes, if desired
if(techclass[1] == "IPC"){#if IPC cutting is used
  
  
  #this needs to be done regardless of tech class long cut level
  merged_df$tech_class_long <- merged_df$tech_class
  merged_df$tech_class <-  substr(merged_df$tech_class, 1, as.numeric(techclass[2]))  #cut to desired IPC level
  
  
  #lose old tech names
  merged_df <- merged_df %>% dplyr::select(-tech_name)
  
  #get tech names
  merged_df <- merged_df %>% dplyr::left_join(IPC_names, by = "tech_class")
  merged_df <- dplyr::distinct(merged_df) #get rid of duplicates (also to save memory)
  
  
  #cut for tech class long depending on level
  
  if(techclass[3] == 'maingroup'){#in case main group is long tech
    
    #get number of slash / in tech class
    slash_temp <- regexpr("/",merged_df$tech_class_long)
    merged_df <- merged_df %>% dplyr::mutate(slash = slash_temp)
    #write tech class long for maingroups 
    merged_df <- merged_df %>% mutate(tech_class_long = substr(tech_class_long, 1, slash))
    
  }else  if(techclass[3] == 'subclass' | (!is.na(as.numeric(techclass[3])) & as.numeric(techclass[3]) ==4 )){#if subclass is tech long
    
    merged_df <- merged_df %>% mutate(tech_class_long = substr(tech_class_long, 1, 4))
    
  }
  
}#end IPC cutting


if(techclass[1] == "CPC"){#if CPC cutting is used
 
  
  #this needs to be done regardless of tech class long cut level
  merged_df$tech_class_long <- merged_df$tech_class
  merged_df$tech_class <-  substr(merged_df$tech_class, 1, as.numeric(techclass[2]))  #cut to desired CPC level
  
  
  #lose old tech names
  merged_df <- merged_df %>% dplyr::select(-tech_name)
  
  #get tech names
  merged_df <- merged_df %>% dplyr::left_join(CPC_names, by = "tech_class")
  merged_df <- dplyr::distinct(merged_df) #get rid of duplicates (also to save memory)
  
  
  #cut for tech class long depending on level
  
  if(techclass[3] == 'maingroup'){#in case main group is long tech
  
    #get number of slash / in tech class
    slash_temp <- regexpr("/",merged_df$tech_class_long)
    merged_df <- merged_df %>% dplyr::mutate(slash = slash_temp)
    #write tech class long for maingroups 
    merged_df <- merged_df %>% mutate(tech_class_long = substr(tech_class_long, 1, slash))
    
  }else  if(techclass[3] == 'subclass' | (!is.na(as.numeric(techclass[3])) & as.numeric(techclass[3]) ==4 )){#if subclass is tech long
    
    merged_df <- merged_df %>% mutate(tech_class_long = substr(tech_class_long, 1, 4))
    
  }
}#end CPC cutting


no_patents_aftertech <- length(unique(merged_df$appln_id))#define before Schmoch in case there is no Schmoch matching


if(techclass[1] == "Schmoch"){ #if Schmoch (2008) classification is used
  
  #create long tech column according to chosen
  if(techclass[3] == 'subclass' | (!is.na(as.numeric(techclass[3])) & as.numeric(techclass[3]) ==4 )){#if subclass is tech long
    
    merged_df <- merged_df %>% mutate(tech_class_long = substr(tech_class_long, 1, 4))
    
  }else if(techclass[3] == 'maingroup'){#if subclass is tech long
    
    #get number of slash / in tech class
    slash_temp <- regexpr("/",merged_df$tech_class_long)
    merged_df <- merged_df %>% dplyr::mutate(slash = slash_temp)
    #write tech class long for maingroups 
    merged_df <- merged_df %>% mutate(tech_class_long = substr(tech_class_long, 1, slash))
  }
  
  
  #lose old tech names
  merged_df <- merged_df %>% dplyr::select(-tech_name)
  
  merged_df$tech_class <-   substr(merged_df$tech_class, 1, 7)
  merged_df$IPC4 <- substr(merged_df$tech_class, 1, 4)
  #make IPC7 codes comparable to concordance table
  merged_df$IPC8 <- paste(substr(merged_df$tech_class, 1, 4), "0", substr(merged_df$tech_class, 5, 7), "/%", sep = "")
  
  #match different lengths of IPC codes
  
  merged_df <- merged_df %>% dplyr::left_join(IPC_schmoch_concordance, by = c("IPC4" = "IPC_code")) %>% 
    dplyr::rename(Field_n1 = Field_number, Field_en1 = Field_en) %>% 
    dplyr::left_join(IPC_schmoch_concordance, by = c("IPC8" = "IPC_code")) %>% 
    dplyr::rename(Field_n2 = Field_number, Field_en2 = Field_en)
  
  #take field number which is not na
  merged_df <- merged_df %>% dplyr::mutate(tech_class = dplyr::case_when(is.na(Field_n1)==T ~ Field_n2,
                                                               is.na(Field_n2)==T ~ Field_n1,
                                                               TRUE ~ Field_n1)) %>%
                              dplyr::mutate(tech_name = case_when(is.na(Field_en1)==T ~ Field_en2,
                                      is.na(Field_en2)==T ~ Field_en1,
                                      TRUE ~ Field_en1)) %>%
    dplyr::select(-IPC4,-IPC8,-Field_n1,-Field_n2,-Field_en1,-Field_en2)
    
  
  #check how many patents did not get a tech field and if those are completely removed from the dataset
  #number of patent ids which are not matched with tech fields (at least partially), we don't do now
  
  
  #just remove rows that have no tech but leave patent in
  merged_df <- merged_df %>% dplyr::filter(is.na(tech_class) == F) 
  
  
  
  #give warning if more than 1% of rows no techf
  # if((dim.data.frame(no_techf_df)[1] / dim.data.frame(merged_df)[1]) > 0.01 ){ 
  #   warning("More than 1% of rows does not have a techfield associated!")
  # }
  
  #remove rows without tech field found, old way
  #merged_df <- dplyr::anti_join(merged_df, no_techf_df, by = "appln_id")
  
  merged_df$tech_class  <- as.factor(merged_df$tech_class)
  
  merged_df <- dplyr::distinct(merged_df)#get rid of duplicates (also to save memory)
  
  
  
  #count number of unique patens after IPC schmoch matching/cleaning
  no_patents_aftertech <- length(unique(merged_df$appln_id))
  no_patents <- no_patents_aftertech
  
  
  #give warning if more than 1% of patents lost with techfield matching
  if(1-(no_patents_aftertech / no_patents_afternotechclass) > 0.01 ){ 
    warning("More than 1% of patents were removed with tech matching")
  }
  
}#end schmoch matching 






####NUTS adjustements####


#merged_df <- merged_df %>% dplyr::select(appln_id,App_nbr,Pub_nbr,tech_class,tech_class_long,tech_name,reg_code,Ctry_code,Reg_share,Inv_share,prio_year,app_year)
merged_df$reg_code <- as.character(merged_df$reg_code)




#change LONDON NUTS codes because they did not use the correct codes in REGPAT
#NUTS3 from 2010 used, so new NUTS2 codes correspond to old NUTS3 codes because they used older NUTS3 codes for new NUTS2 codes in London
#NUTS3 in London is INCORRECT because REGPAT does not include information about new NUTS 2013 London NUTS3
#for Metro regions it does not matter because there is only one London Metro area 

#Liechtenstein, unclassified LIZZZ changed to LI000 because there is only one region and patents are for no reason classified to LIZZZ
#Iceland NUTS codes are wrong in REGPAT, there should only be two NUTS-3 regions, recoding

merged_df <- merged_df  %>% dplyr::mutate(reg_code = case_when(
  reg_code == "UKI11" ~  "UKI31", 
  reg_code == "UKI12" ~  "UKI41",
  reg_code == "UKI21" ~  "UKI51",
  reg_code == "UKI22" ~  "UKI61",
  reg_code == "UKI23" ~  "UKI71",
  reg_code == "UKJ23" ~  "UKJ25",
  reg_code == "UKJ24" ~  "UKJ27",
  reg_code == "UKJ33" ~  "UKJ35", 
  reg_code == "UKJ42" ~  "UKJ43",
  reg_code == "LIZZZ" ~  "LI000",
  substr(reg_code,1,4) == "IS01" ~ "IS001",
  substr(reg_code,1,4) == "IS02" ~ "IS002",
  TRUE ~ reg_code
  ))


no_patents_after_zz <- length(unique(merged_df$appln_id)) #define no patents for comparison later if unclassified are not removed


#get rid of regions that end with Z (which might be included when not matching for NUTS regions before) when NUTS regions are chosen (not NUTS0) --> not classified

#unclassified_df <- merged_df[grep("Z$",merged_df$reg_code),]
#unclassified_df <- droplevels(unclassified_df)

#create dummy variable for unclassified
merged_df$unclassified <- 0

merged_df[grep("Z$",merged_df$reg_code),]$unclassified <- 1

unclassified_df <- merged_df %>% dplyr::filter(unclassified==1)
  
#give warning if more than 1% of rows have ZZ region
if( (dim.data.frame(unclassified_df)[1] / dim.data.frame(merged_df)[1]) > 0.01 ){
  warning("More than 1% rows have unclassified region info!")
  
  if((dim.data.frame(unclassified_df)[1] / dim.data.frame(merged_df)[1]) < 0.03 ){
    warning("Less than 3% rows have unclassified region info!")
  }else{
    warning("More than 3% rows have unclassified region info!")
  }
}


if(disc_unclass == "yes"){#if we drop unclassified
  
  merged_df <- merged_df %>% dplyr::filter(unclassified==0)
  merged_df <- droplevels(merged_df)
  no_patents_after_zz <- length(unique(merged_df$appln_id))
  
}#end removing unclassified


#drop rows that have not classified region
#merged_df <- merged_df %>%  dplyr::anti_join(unclassified_df, by = "reg_code")



#give warning if more than 1% of patents are lost with ZZ correction
if( (1- (no_patents_after_zz / no_patents_aftertech) ) > 0.01 ){
  warning("More than 1% of patents are dropped in non-classified regions!")
  
  if( (1- (no_patents_after_zz / no_patents_aftertech) ) < 0.03 ){
    warning("Less than 3% of patents are dropped in non-classified regions!")
  }else{
    warning("More than 3% of patents are dropped in non-classified regions!")
  }
}



#cut to desired NUTS level or match with metro regions
if(region != "metro"){#if not metro matching, just cut NUTS code
  merged_df$reg_code <-  substr(merged_df$reg_code, 1, as.numeric(region) + 2)
  merged_df$reg_code <- as.factor(merged_df$reg_code)
  merged_df<- merged_df %>% droplevels() %>% dplyr::distinct() %>% dplyr::as_tibble()

  
  
}else{ #if metro region matching

  ####METRO####
  
  Encoding(metro_concordance$metro_name) <- "UTF-8"
  
  #merge with metro codes and rename metro_code to reg_code for consistent naming
  # merged_df <- metro_concordance  %>% dplyr::right_join(merged_df, by = c("NUTS_ID" = "reg_code" )) %>% 
  #   dplyr::select(appln_id,prio_year,app_year,metro_code,tech_class,tech_class_long,metro_name,Ctry_code,Reg_share,Inv_share,tech_name) %>%  dplyr::rename(reg_code = metro_code)
  # 
  
  #lose countries outside EU and EFTA
  countries_included <- c( # better way to define countries wanted
    "BE", 		"EL", "GR" 	 	,"LT" 	,"PT"
    ,"BG" 	 	,"ES" 	 	,"LU" 	,"RO"
    ,"CZ" 		,"FR" 		,"HU" 	 	,"SI"
    ,"DK" 		,"HR" 		,"MT" 	 	,"SK"
    ,"DE" 	,"IT" 		,"NL" 	,"FI"
    ,"EE" 	,"CY" 	 	,"AT" 		,"SE"
    ,"IE" 		,"LV" 		,"PL" 	 	,"UK", "GB"  #EU 28 till here
    ,"IS" 			,"NO"
    ,"LI" 			,"CH" ) # EFTA here
  
  merged_df <- merged_df %>% filter(ctry_code %in% countries_included == T)
  
  no_patents_beforemetro <- length(unique(merged_df$appln_id))
  
  #new way of metro matching that keeps non-matched
  merged_df <- merged_df %>% dplyr::left_join(dplyr::select(metro_concordance, -country_code), by = c("reg_code" = "NUTS_ID"))
  
  if(disc_periphery == "yes"){#old type where periphery is deleted
    
    #create metro dropped file
    metro_nonmatch_df <- merged_df %>% dplyr::filter(is.na(metro_code)==T)
    
    #rename metro code to Reg code and lose metro code
    merged_df <- merged_df %>% dplyr::select(-reg_code) %>% rename(reg_code = metro_code)
    
    #number of rows (information) before removing non metros
    no_rows_beforemetro <- dim.data.frame(merged_df)[1]
    
    #remove rows without metro regions and check how much information is lost
    merged_df <- merged_df %>% dplyr:: filter(is.na(reg_code)==F)#remove non matches
    
    no_patents_aftermetro <- length(unique(merged_df$appln_id))#number of patents after removing non metros
    no_rows_aftermetro <- dim.data.frame(merged_df)[1]#number of rows (information) after removing non metros
    
    merged_df$reg_code <- as.factor(merged_df$reg_code)
    merged_df$tech_class <- as.factor(merged_df$tech_class)
    
    #shouldn't do anything
    merged_df <- merged_df %>% dplyr::distinct() %>% dplyr::as_tibble()
    
    no_patents <- no_patents_aftermetro
    
    #give information about metro matching
    if(is.data.frame(regionlist)==F){
      cat(" Number of patents before removing non EU countries:", no_patents_after_zz,"\n",
          "Number of patents before metro matching:", no_patents_beforemetro,"\n",
          "Number of patents after metro matching:", no_patents_aftermetro,"\n",
          "Number of patents lost after metro matching:", no_patents_beforemetro - no_patents_aftermetro,"\n",
          "Relative:",round(1-no_patents_aftermetro / no_patents_beforemetro,2),"\n",
          "Number of rows (information) lost after metro matching:",no_rows_beforemetro - no_rows_aftermetro,"\n",
          "Relative:",round(1-no_rows_aftermetro / no_rows_beforemetro,2),"\n")
    }
    
  }else{
    
    
    #number of rows (information) before removing non metros
    no_rows_beforemetro <- dim.data.frame(merged_df)[1]
    
    #keeps periphery
    #create periphery dummy
    merged_df <- merged_df %>% dplyr::mutate(periphery = case_when(
      unclassified == 1 ~ as.double(NA),
      is.na(metro_code) == T ~ 1,
      TRUE ~ 0)) 
    
    #get NUTS code for peripheries
    merged_df <- merged_df %>% dplyr::mutate(metro_code = case_when(
        is.na(metro_code) == T ~ reg_code,
        TRUE ~ metro_code ))
      
    #rename metro code to Reg code 
    merged_df <- merged_df %>% dplyr::select(-reg_code) %>% rename(reg_code = metro_code)
    
    no_patents_aftermetro <- length(unique(merged_df$appln_id))#number of patents after removing non metros
    no_rows_aftermetro <- dim.data.frame(merged_df)[1]#number of rows (information) after removing non metros 
    
    merged_df$reg_code <- as.factor(merged_df$reg_code)
    merged_df$tech_class <- as.factor(merged_df$tech_class)
    
    #shouldn't do anything
    merged_df <- merged_df %>% dplyr::distinct() %>% dplyr::as_tibble()
    
    no_patents <- no_patents_aftermetro
    
    #give information about metro matching
    if(is.data.frame(regionlist)==F){
      cat(" Number of patents before removing non EU countries:", no_patents_after_zz,"\n",
          "Number of patents before metro matching:", no_patents_beforemetro,"\n",
          "Number of patents after metro matching:", no_patents_aftermetro,"\n",
          "Number of patents lost after metro matching:", no_patents_beforemetro - no_patents_aftermetro,"\n",
          "Relative:",round(1-no_patents_aftermetro / no_patents_beforemetro,2),"\n",
          "Number of rows (information) lost after metro matching:",no_rows_beforemetro - no_rows_aftermetro,"\n",
          "Relative:",round(1-no_rows_aftermetro / no_rows_beforemetro,2),"\n")
    }
    
    
  }
  
  
}#end metro matching

#match with given region list, remove all other regions
if(is.data.frame(regionlist)==T){
 merged_df <- merged_df %>% dplyr::inner_join(regionlist, by = "reg_code")
 merged_df <- dplyr::distinct(merged_df) #shouldnt do anything
 merged_df$reg_code <- as.factor(merged_df$reg_code)
}

#shouldn't do anything
merged_df <- merged_df %>% droplevels() %>% dplyr::distinct() 
result <- merged_df

#create list to export if metro matching
if(region == "metro" & disc_periphery == "yes"){
result <- list(result_df = merged_df, metro_nonmatch_df = metro_nonmatch_df, unclassified_regions = unclassified_df)
}

if(region == "metro" & disc_periphery != "yes"){#if we keep periphery
  result <- list(result_df = merged_df,  unclassified_regions = unclassified_df)
}


####END####
  
  return(result)
}





