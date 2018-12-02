verify_group_consistency <- function(dataset) {

commas <-
  dataset %>%
  mutate(
    gender_has_comma = case_when(
      str_detect(borrower_genders, ",") ~ T,
      T ~ F
    ),
    brwrPict_has_comma = case_when(
      str_detect(borrower_pictured, ",") ~ T,
      T ~ F
    )
  ) 

groups_are_consistent <-
  commas %>% 
  group_by(gender_has_comma, brwrPict_has_comma) %>%
  count()

rm(commas) # Clear up memory
View(groups_are_consistent)

}