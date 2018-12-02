# Input: a string subset of an activity (e.g., "clothing")
# Output: dataframe of activities whose titles match the string
#         and their tally in the dataset

get_num_activity_like <- function(dataset, string) {
  dataset %>%
    filter(str_detect(str_to_lower(activity_name), str_to_lower(string))) %>%
    group_by(activity_name) %>%
    summarize(record_count = n())
}