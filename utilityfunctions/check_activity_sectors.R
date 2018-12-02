check_activity_sectors <- function(dataset, string) {
  dataset %>%
    select(activity_name, sector_name) %>%
    filter(str_detect(str_to_lower(activity_name), str_to_lower(string))) %>%
    unique() %>%
    View()
}