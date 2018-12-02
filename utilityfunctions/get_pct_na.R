get_pct_na <- function(df) {

  # Store number of rows for later
  n_rows <- nrow(df)
  
  # Check size
  sz <- object.size(df)
  
  # Get number of partitions
  n_partitions <-
    ceiling(sz / 499e6) %>% # 499MB is max partition size
      as.integer()
  
  # How many rows is that per partition?
  rows_per_partition <- n_rows/n_partitions 
  
  plan(multiprocess)
  
  # Create empty data frame to hold NA counts for each column
  res <- df[0,]
  res <- map(res, as.integer) # Convert all columns to integer data types
  res <- as_tibble(res)
  
  # Partition the dataset
  df <- 
    # https://stackoverflow.com/questions/14164525/splitting-a-large-data-frame-into-smaller-segments
    split(
      df, (
        rownames(df) %>%
          as.numeric() %>%
          - 1 %>% 
          divide_by_int(rows_per_partition)
      )
    )
  
  # Count NAs for each partition
  for (i in seq_along(df)) {
    
    this_sect <-
      df %>% 
      .[[i]] %>%
      future_map(is.na) %>%
      future_map(sum) %>%
      as_tibble()
    
    res <- bind_rows(res, this_sect)  
    
  }
  
  # Roll up NA counts for each column
  res <- 
    res %>%
    map(sum) %>% 
    as_tibble
  
  # Convert result to a key-value dictionary
  colnames_res <- colnames(res) # column name (key) vector
  nacounts_res <- # count of NAs (value) vector
    res %>%
    t %>%
    .[,1] %>%
    unname
  
  # Put resulting dictionary into a data frame
  res <-
    tibble(
      colname = colnames_res,
      na_count = nacounts_res
    )
  
  # ------------------------------
  
  # Convert counts to percents
  
  res <-
    res %>%
      mutate(
        record_count = n_rows,
        pct_nas = na_count/record_count
      )
  
  return(res)
  
} 