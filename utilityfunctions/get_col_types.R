library(tibble)
library(purrr)

get_col_types <- function(df) {
  
  named_vec_df <-
    map(df, class) %>%
    unlist
  
  tibble(
    colname = names(named_vec_df),
    type = unname(named_vec_df)
  )
  
}