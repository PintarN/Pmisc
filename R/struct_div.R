#' Calculate structural diversity as in Broekel (2019)
#' 
#' Calculate structural diversity index of activities from an activities dataframe where each activity needs to be 
#' associated with a location.
#'
#' @param data A dataframe activity and location data (e.g. techs on patents) for step 1. The output of the previous step (list) 
#' for steps 2 and 3.
#' @param step An integer indicating which step of calculating structural diversity is to be done. 
#' All three need to be executed in order and are prerequisites of the following step. 1 for creating combinatorial networks,
#' 2 for creating subnetworks of combinatorial network on which step 3 will carry out final calculations for and report results.
#' @param cutoff An integer of the minimum number of locations (or patents e.g.) an activity is present to be included in the calculations.
#' Default is 10.
#' @param subnetwork_number A numeric setting the number of subnetworks that should be drawn from full combinatorial networks. 
#' Default is 50, as in Broekel (2019).
#' @param walktrap_steps A numeric setting the number of steps the random walktrap should go when drawing the subnetworks. 
#' Default is 150, as in Broekel (2019). Alternatively, a share (0,1) of the total number of nodes in the combinatorial network can be given. A maximum of 500 steps is set, when doing relative. 
#' @param graphlets Logical indicating whether graphlets (T) (as in Broekel (2019)) or motifs (F) (as in Emmert-Streib, Dehmer 2012) should be used.
#' Default is motifs.  
#' @param activity A string giving the variable name of activity in the data. Default is tech_class.
#' @param location A string giving the variable name of location in the data. Default is appln_id.
#' @param min_edges A numeric giving the minimum number of edges a combinatorial network needs to have to be considered. Default is 5.
#' @param parallel A string with two elements giving the preferred method for parallel computing (in step 2). "sequential", "multisession" (windows local pc) 
#' and number of cores that should be engaged.
#' 
#' @return A list with combinatorial network for each tech for step 1. A list with subnetworks and metrics for each tech for step 2.
#' A list with a tibble with unnested metrics of structural diversity for each tech (i) and subnetwork (j). 
#' 
#' @export
#'



struct_div <- function(data, step, cutoff = 10, subnetwork_number = 50, walktrap_steps = 150, graphlets = F, activity = "tech_class", activity_long = "tech_class_long",
                       location = "appln_id", min_edges = 5, parallel = c('sequential','1')) 
{
  

  
  ####Checks####
  if(!(as.numeric(step) %in% c(1,2,3))){#check if step is correctly given
    warning("No correct step given!")
    stop()
  }
  
  if(!is.numeric(cutoff) & !(is.numeric(cutoff)>0)){#check whether cutoff is given and positive
    warning("No correct cutoff given, needs to be numeric and positive.")
    stop()
  }
  
  if(!is.numeric(subnetwork_number) & !(is.numeric(subnetwork_number)>0)){#check whether subnetwork_number is given and positive
    warning("No correct subnetwork_number given, needs to be numeric and positive.")
    stop()
  }
  
  if(!is.numeric(walktrap_steps) & !(is.numeric(walktrap_steps)>0)){#check whether walktrap_steps is given and positive
    warning("No correct walktrap_steps given, needs to be numeric and positive.")
    stop()
  }
  
  
  
  #renaming tech class and appln_id
  if(activity != 'tech_class'){
    data <- data %>% dplyr::rename_at(.vars = activity, .funs = ~paste0('tech_class'))
  }
  if(activity_long != 'tech_class_long'){
    data <- data %>% dplyr::rename_at(.vars = activity_long, .funs = ~paste0('tech_class_long'))
  }
  if(location != 'appln_id'){
    data <- data %>% dplyr::rename_at(.vars = location, .funs = ~paste0('appln_id'))
  }
  
  #define if no calculations are done
  result <- data
  
  
  
  ###Step1####
  #if step 1 is to be executed, meaning creating the combinatorial network
  if(step == 1){
    
    #check whether dataframe given
    if(is.data.frame(data) == F){
      warning("No dataframe given as input!")
      stop()
    }
    
    
    #creating result list
    tech_n <- data$tech_class %>% unique() %>% length()
    techs <- data$tech_class %>% unique() %>% forcats::fct_drop() %>% sort()
    
    result_comb <- as.list(data$tech_class %>% unique() %>% sort() %>% as.character())
    names(result_comb) <- techs
    for(i in 1:tech_n){
      result_comb[[i]][1] <- as.list(NA)
      result_comb[[i]][2] <- as.list(NA)
      result_comb[[i]][3] <- as.list(NA)
      result_comb[[i]][4] <- as.list(NA)
      names(result_comb[[i]]) <- c("pats_T", "G_T", "main_G_T", "nodes_T")
    }
    
    
    for(i in 1:length(result_comb)){#tech loop
    
    #get list of patents belonging to tech class for all tech classes
    result_comb[[i]]$pats_T <- data %>% dplyr::filter(tech_class == names(result_comb)[i]) %>% 
      dplyr::pull(appln_id) %>% unique() %>% as.double() 
    
    
    #only proceed when tech has at least 10 patents
    if(length(result_comb[[i]]$pats_T) > cutoff){
      
      #create temporary incidence matrix of techs and patents
      incid_df_temp <- data %>% dplyr::filter(appln_id %in% result_comb[[i]]$pats_T) %>%
        dplyr::count(appln_id, tech_class_long)
      incid_df_temp <- droplevels(incid_df_temp)
      incid_df_temp$tech_class_long <- forcats::as_factor(incid_df_temp$tech_class_long)
      incid_df_temp$appln_id <- forcats::as_factor(incid_df_temp$appln_id)
      incid_df_temp$tech_class_long <- forcats::fct_explicit_na(incid_df_temp$tech_class_long, na_level = "missing") 
      incid_df_temp$appln_id <- forcats::fct_explicit_na(incid_df_temp$appln_id, na_level = "missing") 
      
      
      #create temp incidence matrix between patents and tech classes
      incid_temp <- Matrix::sparseMatrix(i = as.integer(incid_df_temp$appln_id), 
                                 j = as.integer(incid_df_temp$tech_class_long),
                                 x = incid_df_temp$n,
                                 dimnames = list(levels(incid_df_temp$appln_id),
                                                 levels(incid_df_temp$tech_class_long)))
      
      
      
      
      #save cooc matrix, combinatorial network of tech T, G_T in Broekel (2019)
      #and dichotomize combinatorial matrix cause NDS not defined on valued network
      result_comb[[i]]$G_T <-  Matrix::t(incid_temp) %*% incid_temp

      #binarise, test because above is not working for large network
      result_comb[[i]]$G_T <- result_comb[[i]]$G_T > 0
      
      #create tidygraph object of combinatorial network
      result_comb[[i]]$G_T <- tidygraph::as_tbl_graph(igraph::graph_from_adjacency_matrix(result_comb[[i]]$G_T, mode = "undirected",diag = F)) 
      
      
      
      #if combinatorial network has no edges (different techs are never on same patent), we need to skip this tech
      if(igraph::gsize(result_comb[[i]]$G_T) > min_edges){#only go ahead if there is a network, setting five otherwise it's too little
        
        #now we need to apply NDS measure to networks G_Ts but it needs to be made on connected networks so we need to take largest component
        #then for each main component a set of 50 random nodes is selected and from each node a sample network is drawn by
        #a random walktrap of 150 steps starting from node --> iNDS is calculated for each of the 50 networks and then averaged 
        #to get measure for G_T 
        
        #getting main component of G_Ts
        
        #create tidygraph object of combinatorial network
        #get main component, with igraph because doesnt work sometimes with tidygraph
        
        result_comb[[i]]$main_G_T <- igraph::decompose(result_comb[[i]]$G_T, mode = "weak", min.vertices = 2, max.comps = 5)
        length_main_gt <- length(result_comb[[i]]$main_G_T)
        #check which is the bigger component if there are two
        if(length_main_gt==1){
          #do nothing if one element
        }else{
          gorder_main_G_Ts <- purrr::map_dbl(result_comb[[i]]$main_G_T, igraph::gorder)
          #result_comb[[i]]$main_G_T[[1]] <- result_comb[[i]]$main_G_T[[which.max(c(igraph::gorder(result_comb[[i]]$main_G_T[[1]]), igraph::gorder(result_comb[[i]]$main_G_T[[2]])))]]
          result_comb[[i]]$main_G_T[[1]] <- result_comb[[i]]$main_G_T[[which.max(gorder_main_G_Ts)]]
        }
        
        #convert to tidygraph
        result_comb[[i]]$main_G_T <- tidygraph::as_tbl_graph(result_comb[[i]]$main_G_T[[1]])
        
        #calculate degree and add
        result_comb[[i]]$main_G_T <- result_comb[[i]]$main_G_T %>%  tidygraph::activate(nodes) %>% tidygraph::mutate(degree = tidygraph::centrality_degree())
        
        
        #sample random_nodes_set_size random nodes from main component, for computational reasons
        result_comb[[i]]$n_nodes <- igraph::gorder(result_comb[[i]]$main_G_T)
        #number of nodes chosen, try with fixed number of draws
        #number_nodes_drawn <- c(number_nodes_temp, subnetwork_number)[which.min(c(number_nodes_temp, subnetwork_number))]
        
        
       #sample random nodes from combinatorial network
        result_comb[[i]]$nodes_T <- sample(size = subnetwork_number, x = 1:result_comb[[i]]$n_nodes, replace = T)
      
      }else {#throw here if combinatorial network too little
      
        result_comb[[i]]$main_G_T <- 'min_edge'
        
      }
    }else {#throw here if too little patents
     
      result_comb[[i]]$main_G_T <- 'cutoff'
       
    }
    
    }#end tech loop
    
    result <- list(result = result_comb,
                   info = list(step = step,
                               cutoff = cutoff,
                               subnetwork_number = subnetwork_number,
                               walktrap_steps = walktrap_steps,
                               graphlets = graphlets,
                               min_edges = min_edges,
                               technames = names(result_comb)
                               ))
    
  }#end step 1
  
  
  ###Step2####
  #if step 2 is to be executed, creating the random subnetworks
  if(step == 2){
    
    #check whether output of step 1 is given
    if(is.list(data) == F){
      warning("No list given as input, maybe you haven't supplied output of step 1?")
      stop()
    }else if(data$info$step != 1){
      warning('You have not given output of step 1 as input!')
      stop()
    }
    
    
    
    result_step1 <- data
    result_sub <- data

    
    #take parameters from step 1
    if(subnetwork_number != result_step1$info$subnetwork_number){
      warning("Subnetwork number given in step 2 is different from output of step 1 given, changing to old value!")
    subnetwork_number <- result_step1$info$subnetwork_number
    }
    if(walktrap_steps != result_step1$info$walktrap_steps){
      warning("Walktrap steps given in step 2 is different from output of step 1 given, working with new value!")
    }
    
    walktrap_input <- walktrap_steps
    
    #get tech names from output step 1
    technames <- names(result_step1$result)
    #create subnetwork names
    subnames <- purrr::map_chr(.x = seq(1,subnetwork_number), .f = paste0, ..1= 'sub' )
    
    
    if(walktrap_input > 1){#if walktrap steps is absolute and not changing for each network
    #create edgelist which is then used to draw subgraph in combination with vertex sequence from random walk
    edgelist_nodes_sequ <- tidyr::tibble(from = seq(1,walktrap_steps-1,1), to = seq(2,walktrap_steps,1), weight = rep(1,walktrap_steps-1))
    
    }
    
    
    #start up parallel
    RhpcBLASctl::blas_set_num_threads(1)
    
    #change  number of cores to names vector
    no_cores <- as.numeric(parallel[2])
    names(no_cores) <- 'system'
    par_type <- parallel[1]
    
    doFuture::registerDoFuture()
    future::plan(par_type, workers = no_cores) #multisession  sequential
    
    options(future.globals.maxSize = +Inf, future.fork.multithreading.enable =
              FALSE)
    
    
    #no_cores <- availableCores() - 1
    #plan(parallel, workers = no_cores)
    
    #doFuture::registerDoFuture()
    #doParallel::registerDoParallel()
    #no_cores <- availableCores() - 1
    #plan(multiprocess, workers = no_cores)
    #plan(multiprocess)
    #future::plan(multisession)
    
    #start tech loop but to be parallelised 
    #tic("execution time")
    
    #outer loop iterator, techs
    outer <- 1:length(data[[1]])
    #inner loop, subnetworks
    inner <- 1: subnetwork_number
    
    #for progress bar
    #library(progressr)
   # progressr::handlers("progress", "beepr")
    #progressr::with_progress({
     # p <- progressr::progressor(along = outer)
    
    result_sub <- foreach::foreach(i = outer, ii = icount(), .errorhandling="pass") %:%  #start outer loop for techs length(data[[1]])
      
      
      #start nested loop of random subnetworks per technology, number of subnetworks, given
      foreach::foreach(j = inner, jj = icount(), .errorhandling="pass") %dopar% { #min(length(result_step1[[1]][[i]]$nodes_T), subnetwork_number)
        
        #progress
        #p(sprintf("i=%g", i))
        
      #create subgraphs from each of the random_nodes_set_size sampled nodes
        #result_sub[[1]][[i]]$subnetwork_nodes_T[[j]]
      
        #only do these calculation if combinatorial network actually created (big enough)
        if(result_step1[[1]][[ii]]$main_G_T[[1]] != 'min_edge' & result_step1[[1]][[ii]]$main_G_T[[1]] != 'cutoff'){
        
          if(walktrap_input < 1){#if share of nodes given for walktrap steps
        #define edgelist sequence and walktrap steps for relative walktrap steps
        
        #set max 500  
        walktrap_steps <- round(walktrap_input * result_step1[[1]][[ii]]$n_nodes,0)        
        
        if(walktrap_steps > 500){
          walktrap_steps <- 500
        } 
        
        edgelist_nodes_sequ <- tidyr::tibble(from = seq(1,walktrap_steps-1,1), to = seq(2,walktrap_steps,1), weight = rep(1,walktrap_steps-1))
          
          }else{#if walktrap is absolute, define again in loop
            walktrap_steps <- walktrap_input
            #create edgelist which is then used to draw subgraph in combination with vertex sequence from random walk
            edgelist_nodes_sequ <- tidyr::tibble(from = seq(1,walktrap_steps-1,1), to = seq(2,walktrap_steps,1), weight = rep(1,walktrap_steps-1))
          }
          
        subnetwork_nodes_T <- result_step1[[1]][[ii]]$main_G_T %>% igraph::random_walk(
        start = result_step1[[1]][[ii]]$nodes_T[[jj]],
        steps = walktrap_steps, #as in Emmert-streib
        stuck = "return",
        mode = "all") %>% igraph::as_ids()
      
        
        #create graph from nodes where we first create graph, then export edgelist and create again to get rid of duplicate nodes
        #result_sub[[1]][[i]]$subnetwork_T[[j]]
        subnetwork_T <-  tidygraph::tbl_graph(nodes = tidyr::tibble(name = subnetwork_nodes_T), 
                                                                        edges = edgelist_nodes_sequ, directed = F)%>% igraph::get.edgelist() %>%
                              igraph::graph_from_edgelist(directed = F) %>% tidygraph::as_tbl_graph() %>% tidygraph::convert(tidygraph::to_simple)
      
      
        
        #add weight for graphlets
        E(subnetwork_T)$weight <- rep(1, igraph::gsize(subnetwork_T))
        
        #calculate degree and add
        subnetwork_T <- subnetwork_T %>%  tidygraph::activate(nodes) %>% tidygraph::mutate(degree = tidygraph::centrality_degree())
        
        
        #get modules (communities) for subnetworks
        modules <-  subnetwork_T  %>% 
          igraph::cluster_walktrap(steps = 4, modularity = T)
        
        #get other metrics and save in tibble
        metrics <- tibble::enframe(modules$membership, name = NULL) %>%
          dplyr::count(value) %>% dplyr::mutate(n_nodes = gorder(subnetwork_T),
                                                mod_count = length(value),
                                                mod_var = var(n),
                                                mod_mean = mean(n)) %>%  #mod mean
                                                dplyr::select(-value, -n) %>% slice(1)
        
        
        #get laplacian metrics
        lap_eigen <- eigen(igraph::laplacian_matrix(subnetwork_T))$values
        
        
        
        if(graphlets == F){#in case we calculate motifs (as in Emmert-Streib, Dehmer)
          #get motifs and remaining metrics                                                     
          metrics <- metrics %>%
            dplyr::mutate(walktrap = walktrap_steps,
                   lap_var = var(lap_eigen),
                   lap_mean = mean(lap_eigen),
                   motif_3 = igraph::count_motifs(subnetwork_T, size = 3),
                   motif_4 = igraph::count_motifs(subnetwork_T, size = 4),
                   alpha_mod = mod_count / n_nodes,
                   r_motif = motif_3 / motif_4,
                   v_mod = mod_var / mod_mean,
                   v_lambda = lap_var / lap_mean,
                   iNDS = (alpha_mod * r_motif) / (v_mod * v_lambda))                       
        }else if(graphlets == T){ #in case we use graphlets, as in Broekel (to save time)
          
          warning("Graphlet calculation not yet defined, using motifs!")
          
          metrics <- metrics %>%
            dplyr::mutate(lap_var = var(lap_eigen),
                          lap_mean = mean(lap_eigen),
                          motif_3 = igraph::count_motifs(subnetwork_T, size = 3),
                          motif_4 = igraph::count_motifs(subnetwork_T, size = 4),
                          alpha_mod = mod_count / n_nodes,
                          r_motif = motif_3 / motif_4,
                          v_mod = mod_var / mod_mean,
                          v_lambda = lap_var / lap_mean,
                          iNDS = (alpha_mod * r_motif) / (v_mod * v_lambda))
        }
        
        
        }else{  #throw here if comb network cutoff or min edge
        
          #fake results
          subnetwork_nodes_T <- NA_character_
          metrics <- tidyr::tibble(reason = result_step1[[1]][[ii]]$main_G_T[[1]])
          subnetwork_T <- NA
        }
          
      #create final object to be exported
        #only save one subnetwork 
        if(jj == 1){
      list(subnetwork_nodes_T = subnetwork_nodes_T, metrics = metrics, i = ii, j = jj, subnetwork_T = subnetwork_T)
        }else {
          list(subnetwork_nodes_T = subnetwork_nodes_T, metrics = metrics, i = ii, j = jj)
        }
        
        
        
      }#end parallelised  loop 
      
    
    
    
   # })#end progress
    # beepr::beep(sound = 3)
    # time <- toc()
    # print(time / 60)
      
    #name techs in list
    names(result_sub) <- technames
    
    #name subs
    for(i in 1:length(technames)){
      
      names(result_sub[[i]]) <- subnames
      
    }
    
    
    result <- list(result = result_sub,
                   info = list(step = step,
                               cutoff = cutoff,
                               subnetwork_number = subnetwork_number,
                               walktrap_steps = walktrap_input,
                               graphlets = graphlets,
                               min_edges = min_edges,
                               technames = technames))
    
    
  #  doFuture::registerDoFuture()
  #  #
  #  future::plan(multicore)
  #  #
  #  testrefer <- seq(1, 40, by = 2)
  #  #
  # outer <-  1:50
  # inner <-  1:100
  # #library(progressr)
  #   progressr::handlers("progress", "beepr")
  #   
  #   progressr::with_progress({
  #     p <- progressr::progressor(along = outer)
  #   test1 <- foreach::foreach (i = outer) %:%
  #     foreach::foreach(j = inner) %dopar% {
  #       
  #       p(sprintf("i=%g", i))
  #       
  #       result_step1[[1]][[j]]$nodes_T[1:5] %>% sum(i)
  # 
  #       subnetwork_test <- seq(j,i)
  #       test <- 'test'
  # 
  #       # subnetwork_nodes_T <- result_step1[[1]][[i]]$main_G_T %>% igraph::random_walk(
  #       #   start = result_step1[[1]][[i]]$nodes_T[[j]],
  #       #   steps = walktrap_steps, #as in Emmert-streib
  #       #   stuck = "return",
  #       #   mode = "all") %>% igraph::as_ids()
  #       # 
  #       # subnetwork_T <-  tidygraph::tbl_graph(nodes = tidyr::tibble(name = subnetwork_nodes_T),
  #       #                                       edges = edgelist_nodes_sequ, directed = F)%>% igraph::get.edgelist() %>%
  #       #   igraph::graph_from_edgelist(directed = F) %>% tidygraph::as_tbl_graph() %>% tidygraph::convert(tidygraph::to_simple)
  # 
  # 
  # 
  #       list( a = subnetwork_test, b = test)
  #     }
  #   
  #   })#end progress
    
    
    
  }#end step 2
  
  
  ###Step3####
  #if step 3 is to be executed, creating a tibble with metrics
  if(step == 3){
    
    #check whether output of step 1 is given
    if(is.list(data) == F){
      warning("No list given as input, maybe you haven't supplied output of step 2?")
      stop()
    }else if(data$info$step != 2){
      warning('You have not given output of step 2 as input!')
      stop()
    }
    
    result_step2 <- data
  
    #take parameters from step 2
    # if(subnetwork_number != result_step2$info$subnetwork_number){
    #   warning("Subnetwork number given in step 3 is different from output of step 2 given, changing to old value!")
    #   subnetwork_number <- result_step2$info$subnetwork_number
    # }
    
    technames <- result_step2$info$technames
    technames_df <- tidyr::tibble(technames = technames, i = seq(1,length(technames)))
    
    
    #unnest metrics and create a tibble with all plus calculate means and sds
    unnest <- tidyr::tibble(data = data$result)
    unnest2 <- unnest %>% tidyr::unnest_wider(1) 
    unnest3 <- unnest2 %>% tidyr::pivot_longer(dplyr::everything())
    unnest4 <- unnest3  %>% tidyr::hoist(value, 'metrics','i','j') %>% dplyr::select(name,metrics,i,j)
    #unnest4 <- unnest3  %>% tidyr::unnest_wider('value') %>% dplyr::select(name,metrics,i,j)
    unnest5 <- unnest4 %>% unnest_wider('metrics') %>% dplyr::left_join(technames_df, by = 'i')
    unnest5 <- unnest5 %>% dplyr::mutate(iNDS = dplyr::case_when(
      is.infinite(iNDS) == T ~ NA_real_,
      TRUE ~ iNDS
    ))
    
    #change inf iNDS to NA (can happen)
    #unnest5$iNDS[is.infinite(unnest5$iNDS)] <- NA
    unnest6 <- unnest5 %>% dplyr::group_by(i) %>% dplyr::summarise(structdiv = -log(mean(iNDS, na.rm = T)), mean_inds =mean(iNDS, na.rm = T),  sd_inds = sd(iNDS, na.rm = T), mean_nodes = mean(n_nodes, na.rm = T), sd_nodes = sd(n_nodes, na.rm = T), infs = sum(is.na(iNDS))) %>%
      dplyr::ungroup() %>% dplyr::right_join(unnest5, by = 'i')
    
    
    
    result <- list(result = unnest6,
                   info = result_step2$info)
    
    result$info$step <- 3
    
    
  }#end step 3
    
####END####
  
  
  if(identical(result , data)){
    stop('Check input, no calculations done.')
  }

  return(result)
}





