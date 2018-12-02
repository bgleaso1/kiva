iterate_kivanums_knn <- function(n_iter, kmin, kmax) {
  
  results_kvals <-
    tibble(
      kval = integer(0),
      avg_misclass = double(0),
      avg_senst = double(0),
      avg_specf = double(0)
    )
  
  for (k in kmin:kmax) {
    
    kmeans_kval <- k
    
    out <-
      tibble(
        FN = double(0),
        FP = double(0),
        TN = double(0),
        TP = double(0)
      )
    
    for (i in 1:n_iter) {
      
      confusion_row <-
        kiva_nums_pre %>%
        run_knn_on(kval = kmeans_kval) %>%
        .$confusion2
      
      out <-
        out %>%
        bind_rows(confusion_row)
      
    }
    
    # Assess the knn model -------------
    
    avgs <-
      out %>%
      summarize(
        avg_tp = mean(TP),
        avg_tn = mean(TN),
        avg_fp = mean(FP),
        avg_fn = mean(FN)
      )
    
    ( sensitivity <- avgs[["avg_tp"]]/(avgs[["avg_tp"]] + avgs[["avg_fn"]]) )
    ( specificity <- avgs[["avg_tn"]]/(avgs[["avg_tn"]] + avgs[["avg_fp"]]) )
    ( misclassification <- (avgs[["avg_fn"]] + avgs[["avg_fp"]]) )
    
    results_kvals <-
      results_kvals %>%
      bind_rows(
        list(
          kval = k,
          avg_misclass = misclassification,
          avg_senst = sensitivity,
          avg_specf = specificity        
        )
      )
    
  }
  
  return(results_kvals)
  
}