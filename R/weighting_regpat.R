#' Calculate weights for REGPAT patent data
#' 
#' Calculate (fractional, quality adjusted) weights for REGPAT patent data
#'
#' @param data A dataframe with prepared (load.regpat) REGPAT data.
#' @param path A vector of paths to OECD citation database files (EP_citation,EP_eqv,WO_citation) 
#' and REGPAT files (EPO and PCT) if within_cits is "drop", or alternatively paths to the saved .rda files, as last entry please give the version YYYYMM
#' @param quality Two strings determining if and which quality weight adjustment should be used, defaults to no quality adjustment.
#' One of those:
#' "times_cits" for old multiplication of patent weight with number of forward citations
#' "times_cits1" for old multiplication of patent weight with number of forward citations + 1
#' "relative_cits" for Pintar (2018) version, cits relative to cohort mean
#' "relative_cits1" for Pintar (2018) version, cits + 1 relative to cohort mean + 1
#' and the number of months that should be taken as max citation lag
#' @param within_cits Text (leave or drop) to set whether citations from within region should be excluded from the citation count, default is leave in
#' @param periodisation Text to set which periodisation should be used. Options are: "prio_year" (default) or "app_year" for forward citation quality adjusted
#' @param years Vector of year intervals (like so: c(2000,2012)) used. Default is all years
#' @param smallnumber Two strings (corresponding to tech classes and then regions) giving the absolute patent count cutoff or 
#' defining which relative cutoff is to be used."10th_perc" or "20th_perc" for e.g. relative. Techs are cut at level given as input.
#' @param smallnumber_count String determining how small techs or regions should be excluded, based on quality or non-quality adjusted (fractional) patent count
#' Default is non quality adjusted
#' 
#' @return A dataframe of REGPAT data with corresponding weights
#' @export
#'


weighting_regpat <- function(data, path = "no", quality = "no", within_cits = "leave", periodisation = "prio_year", years = "all", smallnumber = c("no","no"), smallnumber_count = "non_qu") 
{

final_df <- data #define dataframe for later use   

#drop years not in period interval given, if available
if(years[1] == "all"){#if no year period given, use all years
  
}else{
  if(periodisation == "prio_year"){
    final_df <- final_df %>% dplyr::filter(prio_year >= years[1] & prio_year <= years[2])
  }else{
    final_df <- final_df %>% dplyr::filter(app_year >= years[1] & app_year <= years[2])
  }
}


#number of unique patents before doing anything
no_patents_full <- length(unique(final_df$appln_id))
names(no_patents_full) <- "no_patents_full"

#stop if no patents loaded
if(is.na(no_patents_full) | no_patents_full < 100){
  stop("Please check input file, no patents loaded!")
}


#####QUALITY ADJUSTMENTS####
#in this version from OECD citation database
#If quality adjustments are used, adjust frac weight of each row with quality weight
if(quality[1] != "no" & path[1] != "no"){#if some form of quality adjustement is needed
  
  if(periodisation != "app_year"){#if prio_year is used for periodisation, give warning because citations are based on "filing"="app_year"
    warning("You should use app_year as periodisation when adjusting with forward citations!") 
  }
  
  #check if correct paths  are there
  if(within_cits == "drop" & length(path) != 6){#if not four paths are given
    stop("Please give correct paths to OECD citation database files and regpat files (EPO and PCT), as given in package help!") 
  }
  
  #check if correct paths  are there
  if(within_cits == "leave" & length(path) != 4){#if not four paths are given
    stop("Please give correct paths to OECD citation database files, as given in package help!") 
  }
  
  
  #check if correct text is given for within regions dro
  if(within_cits %in% c("leave", "drop")==F){#if the correct term in not given
    stop("Please set if within regions citations should be dropped, as given in package help!") 
  }
  
  
  # load EP citation data from OECD citation database
  #check if rda can be loaded, if not read new file
  try(load(file = path[1]), silent = T)
  #create dummy object
  EP_cit <- "dummy"
  try(assign("EP_cit", get(paste0("EP_cit_",tail(path,n=1)))), silent = T)
  if(is.data.frame(EP_cit)==F){
  EP_cit <- readr::read_delim(file = path[1], delim = "|",col_names = T)}
  #EP_cit <-  readr::read_delim(file = path[1], delim = "|",col_names = T)
  # load EP equivalents data from OECD citation database
  try(load(file = path[2]), silent = T)
  EP_eqv <- "dummy"
  try(assign("EP_eqv", get(paste0("EP_cit_eqv_",tail(path,n=1)))), silent = T)
  if(is.data.frame(EP_eqv)==F){
  EP_eqv <- readr::read_delim(file = path[2], delim = "|",col_names = T)}
  #rename cited_appln_id there to distinguish 
  EP_eqv <- EP_eqv %>% dplyr::rename(Cited_eqv_appln_id = Cited_appln_id)
  # load EP citation count data from OECD citation database
  #read 10 rows to extract cols
  # EP_count <-  readr::read_delim(file = path[3], delim = "|",col_names = T, n_max = 10)
  # col_types <- attributes(EP_count)$spec
  # col_types$cols[4]$WO_Pub_nbr <- col_character()
  # col_types$cols[5]$WO_appln_id <- col_character()
  # #now load all rows
  # EP_count <-  readr::read_delim(file = path[3], delim = "|", col_types = col_types)
  # load WO citation data from OECD citation database
  #WO_cit <-  readr::read_delim(file = path[4], delim = "|",col_names = T)
  try(load(file = path[3]), silent = T)
  WO_cit <- "dummy"
  try(assign("WO_cit", get(paste0("WO_cit_",tail(path,n=1)))), silent = T)
  if(is.data.frame(WO_cit)==F){
  WO_cit <-readr::read_delim(file = path[3], delim = "|",col_names = T)}

  
  #set which kind of citations count: examiner given citation, as in citation count file
  EP_cit <- EP_cit %>% dplyr::filter(Citn_origin %in% c("SEA", "ISR", "APP"))
  WO_cit <- WO_cit %>% dplyr::filter(Citn_origin %in% c("SEA", "ISR", "APP"))
  
  #set which cited authority should be counted, e.g. EP
  #EP_cit <- EP_cit %>% dplyr::filter(Cited_App_auth == "EP")
  #WO_cit <- WO_cit %>% dplyr::filter(Cited_App_auth == "EP")
  
  
  
  #set the maximum and min citation lag to filter before the linking is done
  EP_cit <- EP_cit %>% dplyr::filter(Citn_lag_month >= -360  &  Citn_lag_month <= as.numeric(quality[2]))
  WO_cit <- WO_cit %>% dplyr::filter(Citn_lag_month >= -360  &  Citn_lag_month <= as.numeric(quality[2]))
  
  
  
  #create quality_df that has all citations of patents in there to then filter for location
  quality_df <- final_df %>% dplyr::select(appln_id) %>% dplyr::distinct()
  
  #Four ways to get cites, as in OECD patent quality database 

  #1) EPO patent cited directly in another EPO patent, filter for only EP cited authority
  quality_df <- quality_df %>% dplyr::left_join(dplyr::select(EP_cit, Citing_appln_id,Cited_appln_id) 
                                                  ,by = c("appln_id"  = "Cited_appln_id")) 
  
  #2) #EPO patent cited in another EPO patent as eqv
  #get equivalents in there
  quality_df <- quality_df %>% dplyr::left_join(dplyr::select(EP_eqv, Cited_eqv_appln_id, EP_eqv_appln_id), 
                                                by = c("appln_id" = "EP_eqv_appln_id"))
  #add info when cited as equivalent, as new column
  quality_df <- quality_df %>% dplyr::left_join(dplyr::select(EP_cit, Cited_appln_id, Citing_appln_id) %>% 
                                                dplyr::rename(Citing_appln_id_EP_eqv = Citing_appln_id),
                                                by = c("Cited_eqv_appln_id" = "Cited_appln_id"))
  
  
  #3) #EPO patent cited only in PCT app and not in subsequent EPO
  quality_df  <- quality_df %>% dplyr::left_join(dplyr::select(WO_cit, Citing_appln_id, Cited_appln_id) %>%
                                                 dplyr::rename(Citing_appln_id_WO = Citing_appln_id), 
                                                 by = c("appln_id" = "Cited_appln_id"))
  
  
  #4) #EPO patent cited in PCT app as non-EPO eqv
  #add info when cited as equivalent, as new column
  quality_df <- quality_df %>% dplyr::left_join(dplyr::select(WO_cit, Cited_appln_id, Citing_appln_id) %>% 
                                                  dplyr::rename(Citing_appln_id_WO_eqv = Citing_appln_id),
                                                by = c("Cited_eqv_appln_id" = "Cited_appln_id"))
  
  
  #now get it in right format, drop cited_eqv_appln_id for counting
  quality_long_df <- quality_df %>% dplyr::select(-Cited_eqv_appln_id)%>%
    tidyr::gather("origin", "Citing_appln_id", -appln_id) %>% arrange(desc(-appln_id))
  #drop nas and distinct
  quality_long_df <- quality_long_df %>% dplyr::filter(!is.na(Citing_appln_id)) %>% dplyr::distinct()
  
  #calculate number of cits total
  quality_long_df <- quality_long_df %>% dplyr::group_by(appln_id) %>% dplyr::count(Citing_appln_id) %>%
    dplyr::count(n) %>% dplyr::select(-n) %>% dplyr::rename(n_cits = nn) %>%
    dplyr::right_join(quality_long_df, by = "appln_id") %>% dplyr::distinct()
  
  #create quality file only with number of cits
  quality_appln_df <- tibble::tibble(appln_id = unique(quality_long_df$appln_id)) %>% 
    dplyr::left_join(dplyr::select(quality_long_df, appln_id, n_cits), by = "appln_id") %>% dplyr::distinct()
  
  
  #remove na cits
  quality_appln_df <- quality_appln_df %>% dplyr::mutate(n_cits = as.double(n_cits)) %>% dplyr::mutate(n_cits = case_when(
    is.na(n_cits) ~0, 
    TRUE ~ n_cits
  ))
  
  
  #count number of citations included now before removing within metro cits
  no_cites_all <- quality_appln_df %>% dplyr::select(appln_id,n_cits) %>% dplyr::distinct() %>%
    pull(n_cits) %>% sum()
  names(no_cites_all) <- "no_cites_all"
  
  
  
  if(within_cits == "drop"){#start loop for removing within metro citations
  #if we need location info to remove citations
  
  quality_reg_df <- quality_long_df %>% dplyr::left_join(dplyr::select(final_df, appln_id, reg_code) %>% dplyr::distinct() , 
                                                         by = "appln_id")
  
  #translate to NUTS3
  quality_reg_df <- quality_reg_df %>% dplyr::left_join(dplyr::select(metro_concordance, NUTS_ID, metro_code),
                                                        by = c("reg_code"= "metro_code"))
  
  #load location info of citing patents
  # EPO_region_inv_df <-  readr::read_delim(file = path[5], delim = "|", col_names = T, 
  #                                         col_types =  cols_only(appln_id = col_double(),reg_code = col_character())) %>%
  #                                         dplyr::distinct() %>% dplyr::rename(reg_code_citing_EPO = reg_code)
  #try to load rda file if possible
  try(load(file = path[4]), silent = T)
  EPO_region_inv_df <- "dummy"
  try(assign("EPO_region_inv_df", get(paste0("Regpat_EP_reg_",tail(path,n=1)))), silent = T)
  if(is.data.frame(EPO_region_inv_df)==F){
  EPO_region_inv_df <- readr::read_delim(file = path[4], delim = "|", col_names = T, 
                        col_types =  cols_only(appln_id = col_double(),reg_code = col_character())) %>%
                       dplyr::distinct() %>% dplyr::rename(reg_code_citing_EPO = reg_code)}
  
  # WO_region_inv_df <-  readr::read_delim(file = path[6], delim = "|", col_names = T, 
  #                                         col_types =  cols_only(appln_id = col_double(),reg_code = col_character())) %>%
  #                                         dplyr::distinct() %>% dplyr::rename(reg_code_citing_WO = reg_code)
  # 
  try(load(file = path[5]), silent = T)
  WO_region_inv_df <- "dummy"
  try(assign("WO_region_inv_df", get(paste0("Regpat_WO_reg_",tail(path,n=1)))), silent = T)
  if(is.data.frame(WO_region_inv_df)==F){
    WO_region_inv_df <- readr::read_delim(file = path[5], delim = "|", col_names = T, 
                                           col_types =  cols_only(appln_id = col_double(),reg_code = col_character())) %>%
      dplyr::distinct() %>% dplyr::rename(reg_code_citing_WO = reg_code)}
  
  
  #merge location data into quality file
  #first from EPO
  quality_reg_df <- quality_reg_df %>% dplyr::left_join(EPO_region_inv_df, by = c("Citing_appln_id" = "appln_id"))
  #then from WO
  quality_reg_df <- quality_reg_df %>% dplyr::left_join(WO_region_inv_df, by = c("Citing_appln_id" = "appln_id"))
  
  #remove within citations
  #get number of patents that were cited
  no_cited_patents <- length(unique(quality_reg_df$appln_id))
  names(no_cited_patents) <- "no_cited_patents"
  
  #delete rows where no location info was found in either file
  quality_reg_na_df <- quality_reg_df %>% dplyr::filter(is.na(reg_code_citing_EPO) & is.na(reg_code_citing_WO))
  quality_reg_final_df <- quality_reg_df %>% dplyr::setdiff(quality_reg_na_df)
  
  #count number of patents left
  no_cited_after_na <- length(unique(quality_reg_final_df$appln_id))
  names(no_cited_after_na) <- "no_cited_after_na"
  
  #create unclassified df
  #remove ZZ regions
  unclassified_EPO_df <- quality_reg_final_df[grep("Z$",quality_reg_final_df$reg_code_citing_EPO),]
  #unclassified_EPO_df <- droplevels(unclassified_EPO_df)
  #only drop cites that come from same country unclassified
  unclassified_EPO_df <- unclassified_EPO_df %>% dplyr::filter(substr(NUTS_ID,1,2) == substr(reg_code_citing_EPO,1,2))
  
  unclassified_WO_df <- quality_reg_final_df[grep("Z$",quality_reg_final_df$reg_code_citing_WO),]
  #unclassified_WO_df <- droplevels(unclassified_WO_df)
  #only drop cites that come from same country unclassified
  unclassified_WO_df <- unclassified_WO_df %>% dplyr::filter(substr(NUTS_ID,1,2) == substr(reg_code_citing_WO,1,2))
  
  #drop rows that have not classified region
  quality_reg_final_df <- quality_reg_final_df %>%  dplyr::anti_join(unclassified_EPO_df, by = "reg_code_citing_EPO")
  quality_reg_final_df <- quality_reg_final_df %>%  dplyr::anti_join(unclassified_WO_df, by = "reg_code_citing_WO")
  
  no_cited_after_z <- length(unique(quality_reg_final_df$appln_id))
  names(no_cited_after_z) <- "no_cited_after_z"
  
  
  #flag any region codes that are within metro but only for citing id within appln id
  quality_reg_flag_df <- quality_reg_final_df %>%  dplyr::group_by(appln_id,Citing_appln_id) %>%
    dplyr::mutate(flag = dplyr::case_when(
    any(NUTS_ID == reg_code_citing_EPO) ~ 1,
    any(NUTS_ID == reg_code_citing_WO) ~ 1,
    TRUE ~0)) 
  
  
  
 #remove any flagged citations
  quality_reg_final_df <- quality_reg_flag_df %>% dplyr::filter(flag ==0) %>% dplyr::ungroup()
  
  
  #count number of cits without metro
  quality_reg_final_df <- quality_reg_final_df %>% dplyr::group_by(appln_id) %>% dplyr::count(Citing_appln_id) %>%
    dplyr::count(Citing_appln_id) %>% dplyr::count(n) %>% dplyr::select(-n) %>% dplyr::rename(n_cits_excl = nn) %>%
    dplyr::right_join(quality_reg_final_df, by = "appln_id") %>% dplyr::ungroup() %>% dplyr::distinct()
  
  quality_reg_long_df <- quality_reg_final_df
  
  
  #count number of citations included now after removing within metro citations
  no_cites_excl_within <- quality_reg_long_df %>% dplyr::select(appln_id,n_cits_excl) %>% dplyr::distinct() %>%
    pull(n_cits_excl) %>% sum()
  names(no_cites_excl_within) <- "no_cites_excl_within"
  
  #create end df
  quality_appln_df <- quality_appln_df %>% dplyr::left_join(dplyr::select(quality_reg_long_df, appln_id, n_cits_excl),
                                                            by = "appln_id") %>% dplyr::distinct()
  
  #remove na cits
  quality_appln_df <- quality_appln_df %>% dplyr::mutate(n_cits_excl = as.double(n_cits_excl)) %>% dplyr::mutate(n_cits_excl = case_when(
    is.na(n_cits_excl) ~0, 
    TRUE ~ n_cits_excl
  ))
  
  
  #check how many patents lost
  no_cited_after_reg <- length(unique(quality_reg_long_df$appln_id))
  names(no_cited_after_reg) <- "no_cited_after_reg"
  
  
  #tell me about it
  cat(" Number of patents cited before starting within metro removal:", no_cited_patents,"\n",
      "Number of patents after removing not localised:", no_cited_after_na,"\n",
      "Removing relative to total:", round(1-no_cited_after_na / no_cited_patents,3),"\n",
      "Number of patents cited after removing not classified that are in same country:", no_cited_after_z,"\n",
      "Removing relative to total:",round(1-no_cited_after_z / no_cited_patents,3),"\n",
      "Number of patents cited after excluding within metro citation:", no_cited_after_reg,"\n",
      "Removing relative to total:",round(1-no_cited_after_reg / no_cited_patents,3),"\n",
      "Number of citations before excluding within metro citation:", no_cites_all,"\n",
      "Number of citations after excluding within metro (and not localised):", no_cites_excl_within,"\n",
      "Removing relative to total:",round(1-no_cites_excl_within / no_cites_all,3),"\n")
  
  }#end within metro removal
  
  
  
  #merge with final df and recode nas to 0 
  
  final_df <-  final_df %>% dplyr::left_join(quality_appln_df, by = "appln_id") %>% dplyr::mutate(n_cits = case_when(
    is.na(n_cits) ~0, 
    TRUE ~ n_cits
  )) 
  
  #create cohorts
  cohorts_df <-  final_df %>%  dplyr::select(appln_id,tech_class,app_year) %>% dplyr::distinct() %>%
    dplyr::left_join(dplyr::select(quality_appln_df, appln_id, n_cits), by = "appln_id") %>% dplyr::distinct() 
  
  cohorts_df <- cohorts_df %>% dplyr::mutate(n_cits = case_when(
    is.na(n_cits) ~0, 
    TRUE ~ n_cits
  )) 
  
  #now calculate cohorts mean
  cohorts_table_df <- cohorts_df %>% dplyr::group_by(app_year,tech_class) %>% dplyr::summarise(mean = mean(n_cits)) %>%
    dplyr::ungroup()
  
  
  if(within_cits == "drop"){#if we also have without metro cits
    
    final_df <- final_df %>% dplyr::select(-n_cits) %>%  dplyr::mutate(n_cits_excl = case_when(
      is.na(n_cits_excl) ~0, 
      TRUE ~ n_cits_excl
    )) %>% dplyr::rename(n_cits = n_cits_excl)
    
    #create with n_cits_excl
    cohorts_df <-  final_df %>%  dplyr::select(appln_id,tech_class,app_year) %>% dplyr::distinct() %>%
      dplyr::left_join(dplyr::select(quality_appln_df, appln_id, n_cits_excl), by = "appln_id") %>% dplyr::distinct() 
    
    cohorts_df <- cohorts_df %>% dplyr::mutate(n_cits_excl = case_when(
      is.na(n_cits_excl) ~0, 
      TRUE ~ n_cits_excl
    )) 
    
    
    #now calculate cohorts mean
    cohorts_table_df <- cohorts_df %>% dplyr::group_by(app_year,tech_class) %>% dplyr::summarise(mean = mean(n_cits_excl)) %>%
      dplyr::ungroup()
    
  }
  
  #get cohort data in final_df
  final_df <- final_df %>% dplyr::left_join(cohorts_table_df, by = c("app_year", "tech_class"))
  #if a patent has multiple tech classes we need to calculate an average mean of all these so each patent gets a unique weight
  final_df <- final_df %>% dplyr::select(appln_id,tech_class,mean) %>% dplyr::distinct() %>% 
    dplyr::group_by(appln_id) %>% dplyr::count(tech_class) %>% dplyr::count(n) %>% dplyr::select(-n) %>%
    dplyr::left_join(final_df %>% dplyr::select(appln_id,tech_class,mean), by = "appln_id") %>% 
    dplyr::distinct() %>% dplyr::mutate(mean = mean/nn) %>% dplyr::summarise(mean_unique = sum(mean)) %>%
    right_join(final_df, by = "appln_id") %>% dplyr::select(-mean) %>% dplyr::rename(mean = mean_unique) %>% dplyr::ungroup()
  
  
  
  if(quality[1] == "times_cits"){#if old adjustment of just multiplying patent by number of cits
    
    final_df <- final_df %>% dplyr::mutate(qual_weight = n_cits) 
    
  }
  
  if(quality[1] == "times_cits1"){#if adjustment of multiplying patent by number of cits + 1 to account for majority of non-cited patents
    
    final_df <- final_df %>% dplyr::mutate(qual_weight = n_cits + 1) 
    
  }
  
  if(quality[1] == "relative_cits"){#if like Pintar (2018), number of cits / mean of cohort
    
    final_df <- final_df %>% dplyr::mutate(qual_weight = n_cits / mean) 
    
  }
  
  if(quality[1] == "relative_cits1"){#if like Pintar (2018), number of cits +1 / mean of cohort + 1
    
    final_df <- final_df %>% dplyr::mutate(qual_weight = (n_cits + 1) / (mean + 1)) 
                                               
  }
  
  
  
  
  #check how many patents no qual weight, either because they have no cohort mean (because no tech field) or because otherwise not found
  na_qualweight_df <- final_df %>% dplyr::filter(is.na(qual_weight))
  
  #remove rows with no quality weight
  final_df <- final_df %>% dplyr::filter(is.na(qual_weight)==F)
  
  
  #give warning of how many unique patents have "na" quality weight
  if(is.null(dplyr::setdiff(unique(na_qualweight_df$appln_id), unique(final_df$appln_id))) == FALSE){
    patentsqualna <- dim.data.frame(dplyr::setdiff(unique(na_qualweight_df$appln_id), unique(final_df$appln_id)))[2]
    cat("\n",
        "There are", patentsqualna, "patents that have na quality weight!")
  }
  
  
  #should end with final_df where a qual_weight variable is calculated for each patent which is then multiplied by frac weight later
}  

  
  #calculate number of different regions per patent id - to get correct fractional weight
  #alternative (faster) way to calculate
  final_df <- final_df %>% 
      dplyr::group_by(appln_id) %>% dplyr::count(reg_code) %>% dplyr::count(appln_id) %>% dplyr::right_join(final_df, by = "appln_id") %>%
      dplyr::rename(no_reg = n)
  
  

  #alternative (faster) way to calculate
  final_df <- final_df %>% 
    dplyr::group_by(appln_id) %>% dplyr::count(tech_class_long) %>% dplyr::count(appln_id) %>% dplyr::right_join(final_df, by = "appln_id") %>%
    dplyr::rename(no_tech = n)
  
  
  #calculation of reg share with original share variable to account for lost regions
  final_df <- final_df %>% 
    dplyr::group_by(appln_id) %>%
    dplyr::summarise(no_reg_share =  sum(reg_share)) %>% 
    dplyr::right_join(final_df, by = "appln_id") %>% dplyr::group_by(appln_id) %>% dplyr::mutate(no_reg_share = no_reg_share/no_tech) 
  
  
  #get fractional count variable and include in dataframe
  
  if(quality[1] == "no"){#if not quality adjustment
    final_df <- final_df %>%
      dplyr::mutate(frac_weight =  ((1/no_reg_share) * (1/no_tech) * reg_share),
                    frac_weight_non_qu = frac_weight)
  }else{#if any form of quality adjustment, multiply frac weight with quality weight to get quality adjusted patent count
    
    #include non-quality adjusted frac weight also
    #moreover, include number of patents with at least one citation
    
    final_df <- final_df %>%
      dplyr::mutate(frac_weight =  (((1/no_reg_share) * (1/no_tech) * reg_share)) * qual_weight,
                    frac_weight_non_qu = (1/no_reg_share) * (1/no_tech) * reg_share)
    
  }
     
  
  #sum up fractional count variable over appln_id and include in dataframe
  no_patents_before <- length(unique(final_df$appln_id))
  
  
  #alternative (faster) way to calculate
  final_df <- final_df %>% 
    dplyr::group_by(appln_id) %>% dplyr::count(appln_id, wt = frac_weight) %>% dplyr::right_join(final_df, by = "appln_id") %>%
    dplyr::rename(frac_count = n)  %>% dplyr::ungroup()
  
  final_df$frac_count <- round(final_df$frac_count,10)
  
  
  before_qual_remove_df <- final_df
  
  
  #drop all rows that have zero frac count, i.e. all patents that have no patent quality (cites)
  
  final_df <- final_df %>% dplyr::filter(frac_count != 0) 
  
  #lose useless factors
  final_df <- dplyr::distinct(final_df)
  final_df <- droplevels(final_df)
  
  #check how many regions and techs are excluded because patents with no qual are dropped
  tech_dropped_no_qual <- length(levels(before_qual_remove_df$tech_class)) - length(levels(final_df$tech_class))
  reg_dropped_no_qual <- length(levels(before_qual_remove_df$reg_code)) - length(levels(final_df$reg_code))
  
  cat("\n",
      "There are", tech_dropped_no_qual, "techs lost after quality adjustment!", "\n",
      "There are", reg_dropped_no_qual, "regions lost after quality adjustment!")
  
  
  # final_df <- final_df%>% 
  #   dplyr::group_by(appln_id) %>%
  #   dplyr::summarise(frac_count = round(sum(frac_weight),10)) %>%
  #   dplyr::right_join(final_df, by = "appln_id") %>% dplyr::filter(frac_count != 0)
  # 
  
  #give warning of how many unique patents are lost with quality adjustment
  if(quality[1] != "no"){
    no_patent_after_quality <- length(unique(final_df$appln_id))
    cat("\n",
    "There are", no_patents_before - no_patent_after_quality, "patents lost after quality adjustment!")
  }
  
  
  
  #now, weights should count to one for each patent without quality adjustments (even if periphery regions are lost)
  if(quality[1] == "no" & dim.data.frame(final_df %>% dplyr::filter(round(frac_count,10) != 1))[1] > 0 ) {
    warning("Fractional counting does not sum to 1 for each patent!")
  }
  
    
    
#reorder
  if(quality[1] != "no"){#if any form of quality adjustment
final_df <- final_df %>% dplyr::select(appln_id,frac_weight,qual_weight,frac_count,n_cits,mean,prio_year,app_year,reg_code,no_reg,tech_class,tech_name,tech_class_long,no_tech,no_reg_share,metro_name,city,ctry_code,reg_share,inv_share,frac_weight_non_qu)
  }else{
    final_df <- final_df %>% dplyr::select(appln_id,frac_weight,frac_weight_non_qu,frac_count,prio_year,app_year,reg_code,no_reg,tech_class,tech_name,tech_class_long,no_tech,no_reg_share,metro_name,city,ctry_code,reg_share,inv_share)
  }


####Small number adjustment####    
#remove tech classes and regions that have too few patents to remedy for small number problem
#but retain weights calculated before, so patents where tech classes or regions are removed here have less weight.
if(smallnumber[1] != "no"){#if any form of small number adjustment for tech classes
  
  # #set digit to cut for smallnumber cut
  # suppressWarnings(cutoff_digit_tech <- as.numeric(smallnumber[2]))
  # 
  # 
  # if(smallnumber[2] != "no"){
  # #cut IPCs to digit given for small number adjustment
  # final_df$tech_class_cut <-   substr(final_df$tech_class, 1, cutoff_digit_tech)
  # }
  
  #remove techs first
  #based on non quality adjusted by default
  removed_tech_df <- final_df %>%
    dplyr::group_by(tech_class) %>% dplyr::summarise(no_pat_tech = sum(frac_weight_non_qu))
  
  if(smallnumber_count != "non_qu"){
    
    removed_tech_df <- final_df %>% dplyr::select(appln_id,tech_class,n_cits) %>% dplyr::distinct() %>%
      dplyr::group_by(tech_class) %>% dplyr::summarise(no_pat_tech = sum(n_cits>0))
    
  }
  
  if(substr(smallnumber[1],3,4) == "th"){#if e.g. 10th or 20th perc. is used 
    cutoff_relative <- as.numeric(substr(smallnumber[1],1,2)) / 100
    #get techs with low number of patents
    cutoff_number_tech <- stats::quantile(removed_tech_df$no_pat_tech, cutoff_relative, na.rm = T)
    removed_tech_df <- removed_tech_df %>% dplyr::filter(no_pat_tech < cutoff_number_tech)
  }
  
  if(suppressWarnings(is.na(as.numeric(smallnumber[1]))) == F){#remove everything below given absolute number
    #get techs with low number of patents
    cutoff_number_tech <- as.numeric(smallnumber[1])
    removed_tech_df <- removed_tech_df %>% dplyr::filter(no_pat_tech < cutoff_number_tech)
  }
  
  #remove techs from final_df
  final_before_techremove_df <- final_df
  # final_before_techremove_df$tech_class_cut <- as.factor(final_before_techremove_df$tech_class_cut)
  final_df <- final_df %>% dplyr::anti_join(removed_tech_df, by = "tech_class")
  
  final_df$tech_class <- as.factor(final_df$tech_class)
  # final_df$tech_class_cut <- as.factor(final_df$tech_class_cut)
  final_df <- dplyr::distinct(final_df)
  final_df <- droplevels(final_df)
  
  #tell how many techs and how many patents lost
  patentslost <- dim.data.frame(dplyr::setdiff(unique(final_before_techremove_df$appln_id), unique(final_df$appln_id)))[2]
  no_patents_before <- length(unique(final_before_techremove_df$appln_id))
  cat("\n",
      "There are", patentslost, "patents lost with removal of small number techs!","\n",
      "Relative:",round(patentslost / no_patents_before,3),"\n")
  
}

if(smallnumber[2] != "no"){  
  
  #remove regions now
  
  #based on non quality adjusted count
  removed_regions_df <- final_df %>%
    dplyr::group_by(reg_code) %>% dplyr::summarise(no_pat_reg = sum(frac_weight_non_qu))
  
  if(smallnumber_count != "non_qu"){
    
    removed_regions_df <- final_df %>% dplyr::select(appln_id,reg_code,n_cits) %>% dplyr::distinct() %>%
      dplyr::group_by(reg_code) %>% dplyr::summarise(no_pat_reg = sum(n_cits>0))
    
  }
  
  if(substr(smallnumber[2],3,4) == "th"){#if e.g. 10th or 20th perc. is used 
    cutoff_relative <- as.numeric(substr(smallnumber[2],1,2)) / 100
    cutoff_number_reg <- stats::quantile(removed_regions_df$no_pat_reg, cutoff_relative, na.rm = T)
    removed_regions_df <- removed_regions_df %>% dplyr::filter(no_pat_reg < cutoff_number_reg)
  }
  
  
  if(suppressWarnings(is.na(as.numeric(smallnumber[2]))) == F){#remove everything below given absolute number
    #get techs with low number of patents
    cutoff_number_reg <- as.numeric(smallnumber[2])
    removed_regions_df <- removed_regions_df %>% dplyr::filter(no_pat_reg < cutoff_number_reg)
  }
  
  #remove regions from final_df
  final_before_regremove_df <- final_df
  

  final_df <- final_df %>% dplyr::anti_join(removed_regions_df, by = "reg_code")
  
  final_df$reg_code <- as.factor(final_df$reg_code)
  final_df <- dplyr::distinct(final_df)
  final_df <- droplevels(final_df)
  
  #tell how many regions and how many patents lost
  patentslost <- dim.data.frame(dplyr::setdiff(unique(final_before_regremove_df$appln_id), unique(final_df$appln_id)))[2]
  no_patents_before <- length(unique(final_before_regremove_df$appln_id))
  regions_lost <- length(unique(removed_regions_df$reg_code))
  names(regions_lost) <- "regions_lost"
  cat("\n",
      "There are", patentslost, "patents lost with removal of small number regions!","\n",
      "Relative:",round(patentslost / no_patents_before,3),"\n",
      "There are", regions_lost, "regions excluded because too few patents!", "\n" )
  
}
  
  #if techs or regions removed after calculation of frac weight
  if(smallnumber[1] != "no" | smallnumber[2] != "no"){
#calculate new frac count after removing some techs or regions
  final_df <- final_df %>% 
    dplyr::group_by(appln_id) %>% dplyr::count(appln_id, wt = frac_weight) %>% dplyr::right_join(final_df, by = "appln_id") %>%
    dplyr::select(-frac_count) %>% dplyr::rename(frac_count = n) %>% dplyr::ungroup()
  
  final_df$frac_count <- round(final_df$frac_count,10)
  
  final_df <- final_df %>% dplyr::filter(frac_count != 0)
  
  
  }
  
  
  #get number of patents left after everything
  no_patents_final <- length(unique(final_df$appln_id))
  names(no_patents_final) <- "no_patents_final"
  
  
  cat("\n",
      "There were", no_patents_full, "patents at the start, now we have:", no_patents_final,"\n",
      "There are", no_patents_full - no_patents_final, "patents removed in total.","\n",
      "Patents removed relative:", round(1- no_patents_final / no_patents_full,3),"\n")
  
  
  #create input text file
  input <- list(quality , 
                   within_cits , 
                   periodisation , 
                   years , 
                   smallnumber , 
                   smallnumber_count)
  
  names(input) <- c("quality" , 
                    "within_cits" , 
                    "periodisation" , 
                    "years ", 
                    "smallnumber" , 
                    "smallnumber_count")
  
  
  
  if(quality[1] != "no" & within_cits == "drop"){
    return_list <- list(result_df = final_df,
                        input = input,
                        citation_df = quality_reg_long_df,
                        cohorts_df = cohorts_table_df,
                        numbers = c(no_cites_all,no_cites_excl_within,no_cited_patents, no_cited_after_reg,
                                             regions_lost, no_patents_full, no_patents_final))
  }else if(quality[1] != "no" & within_cits == "leave"){
  return_list <- list(result_df = final_df,
                      input = input,
                      citation_df = quality_long_df,
                      cohorts_df = cohorts_table_df,
                      numbers = c(no_cites_all,
                                  regions_lost, no_patents_full, no_patents_final))
  }else{
    return_list <- list(result_df = final_df,
                        input = input,
                        numbers = c(regions_lost, no_patents_full, no_patents_final))
  }
####END####
  
  return(return_list)
}





