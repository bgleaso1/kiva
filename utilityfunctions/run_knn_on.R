run_knn_on <- function(df, kval) {
  
  # Some required lingering data cleaning
  df <-
    df %>%
    modify_if(is.integer, as.double) %>%
    modify_at("underfunded", as.logical) %>%
    na.omit()
  
  # Pull a random sample of 50k
  df <-
    df %>%
    sample_n(
      size = 50000,
      replace = F
    )
  
  # Split  into predictors and target variable sets
  predictors <-
    df %>%
    select(-underfunded)
  
  target <-
    df %>%
    select(underfunded)
  
  train_size <- nrow(df)
  
  # Get random indices for train and test
  train <-
    sample(
      x = 1:train_size, 
      size = floor(0.7*train_size)
    )
  
  test <-
    sample(
      x = 1:train_size, 
      size = floor(0.3*train_size)
    )
  
  neighbors <-
    knn(
      train = predictors[train,], # Apply random index
      test = predictors[test,],
      cl = target$underfunded[train],
      k = kval
    )  
  
  test_result <-
    tibble(
      predicted = neighbors,
      actual = target$underfunded[test],
      correct_pred = (predicted == actual),
      error_type = case_when(
        predicted == F & actual == T ~ "FN",
        predicted == T & actual == T ~ "TP",
        predicted == T & actual == F ~ "FP",
        predicted == F & actual == F ~ "TN"
      )
    )
  
  confusion <-
    test_result %>%
    group_by(error_type) %>%
    count()
  
  # Check that each error type is present; if not, add with 0 cases
  fp_row <-
    confusion %>% 
    filter(error_type == "FP") %>% 
    nrow
  
  if (fp_row == 0) {
    
    confusion <-
      confusion %>%
      bind_rows( list(error_type = "FP", n = 0) )
    
  }
  
  fn_row <-
    confusion %>% 
    filter(error_type == "FN") %>% 
    nrow  
  
  if (fn_row == 0) {
    
    confusion <-
      confusion %>%
      bind_rows( list(error_type = "FN", n = 0) )
    
  }  
  
  tp_row <-
    confusion %>% 
    filter(error_type == "TP") %>% 
    nrow   

  if (tp_row == 0) {
    
    confusion <-
      confusion %>%
      bind_rows( list(error_type = "TP", n = 0) )
    
  }  
    
  tn_row <-
    confusion %>% 
    filter(error_type == "TN") %>% 
    nrow

  if (tn_row == 0) {
    
    confusion <-
      confusion %>%
      bind_rows( list(error_type = "TN", n = 0) )
    
  }  
   
  confusion2<-
    confusion %>%
    mutate(
      pct = n/length(test)
    ) %>%
    select(-n) %>%
    spread(
      key = error_type,
      value = pct
    )
     
  # Get misclassification rate
  misclass_rate <- ( 1 - mean(test_result$correct_pred) )
  
  result <- 
    list(
      raw = test_result, 
      misclass = misclass_rate, 
      confusion = confusion,
      confusion2 = confusion2
    )
  
  return(result)
  
}
