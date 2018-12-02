show_activity_like <- function(dataset, string) {
  dataset %>%
    filter(str_detect(str_to_lower(activity_name), str_to_lower(string))) %>%
    select(activity_name, everything()) %>%
    arrange(activity_name) %>%
    View()
}

