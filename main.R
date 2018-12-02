library(R.utils)

setwd("C:/Users/gleas/Google Drive/Coding/R/team 17 predictive final")
sourceDirectory(path = "utilityfunctions", pattern = "*.R")

load_libraries()

# Load data ----------------------

kiva <- 
  read_csv(file = "C:/Users/gleas/Google Drive/Grad School/Predictive Final/loans/loans.csv")

kiva_coltypes <-
  get_col_types(kiva)

kiva_nas <- 
  get_pct_na(kiva)

# Look at missing values ------------

kiva_nas %>%
  inner_join(kiva_coltypes, by = "colname") %>%
  select(colname, type, pct_nas) %>%
  filter(pct_nas > 0) %>%
  ggplot(aes(x = fct_reorder(colname, pct_nas), y = pct_nas*100, fill = type)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Measure", 
    y = "% Missing Values",
    fill = "Data Type",
    caption = "*Columns with 0% missing values excluded"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(face = "italic", color = "#6b6b6b"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# Merge (coalesce) description and description_translated into one column ----

kiva <-
  kiva %>%
    mutate_at(
      vars(description_translated), 
      funs(coalesce(.,description))
    ) %>%
    select(-description) %>%
    rename(description = description_translated)

# Revisit missing values --------

kiva_nas <- 
  get_pct_na(kiva)

kiva_coltypes <-
  get_col_types(kiva)

kiva_nas %>%
  inner_join(kiva_coltypes, by = "colname") %>%
  select(colname, type, pct_nas) %>%
  filter(pct_nas > 0) %>%
  ggplot(aes(x = fct_reorder(colname, pct_nas), y = pct_nas*100, fill = type)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Measure", 
    y = "% Missing Values",
    fill = "Data Type",
    caption = "*Columns with 0% missing values excluded"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(face = "italic", color = "#6b6b6b"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# Planned expiration time ----------------

# View
kiva %>%
  select(
    loan_id,
    posted_time,
    planned_expiration_time,
    disburse_time,
    raised_time
  ) %>%
  filter(
    is.na(posted_time)|
    is.na(planned_expiration_time)|
    is.na(disburse_time)|
    is.na(raised_time)
  ) %>%
  View()
  
# What does the average time viewable look like?
# funded_amount < loan_amount => "expired" ?

kiva_uf <-
  kiva %>%
  mutate(
    underfunded = ifelse((funded_amount < loan_amount),1,0)
  )

rm(kiva) # Clear up some working memory

uf_statuses <-
  kiva_uf %>% 
    select(
      status,
      underfunded
    ) %>%
    group_by(status, underfunded) %>%
    summarize(ct = n()) 

# Logical impossibilities:
# 
# Expired & funded (7 cases)
# Funded & underfunded (2 cases)

kiva_c <-
  kiva_uf %>%
  filter(
    !(status == "expired" & underfunded == F)
  ) %>%
  filter(
    !(status == "funded" & underfunded == T)
  )

rm(kiva_uf)

# kiva_c %>%
#   filter(
#     status == "fundRaising"
#   ) %>%
#   View

# Look for typos in activity name -----

# What are the distinct activities & their counts
get_num_activity_like(kiva_c, "") %>% View

# Clothing is the same as Clothing Sales (but not "Cloth & Dressmaking")
get_num_activity_like(kiva_c, "cloth")

show_activity_like(kiva_c, "cloth")

check_activity_sectors(kiva_c, "cloth")

# Is Computer is the same as Computers?
get_num_activity_like(kiva_c, "computer")

check_activity_sectors(kiva_c, "computer")
# No - different sectors

# Food
get_num_activity_like(kiva_c, "food") %>% View

check_activity_sectors(kiva_c, "food")

get_num_activity_like(kiva_c, "") %>% View

# On the whole, the activity column looks pretty clean. No typos/dupes.
# I'll assume that the remainder of the entries are okay.

# Are all country codes two letters? -------

kiva %>%
  transmute(len = nchar(country_code)) %>%
  filter(len != 2) 

# Yes

# Distinguish borrower groups from individuals ----------

# Check that groups are consistent
verify_group_consistency(kiva_c)

# Check that all records are consistent
kiva_c %>%
  mutate(
    num_comma_gender = 1 + str_count(borrower_genders, ","),
    num_comma_pictured = 1 + str_count(borrower_pictured, ",")
  ) %>%
  filter(
    (num_comma_gender != num_comma_pictured)
  )

# Append borrower in-group count
kiva_c <-
  kiva_c %>%
  mutate(num_group_members = 1 + str_count(borrower_genders, ","))

# Append group gender composition ---------
kiva_c <-
  kiva_c %>%
    mutate(
      group_gender_composition = case_when(
        (str_detect(borrower_genders, "female") & str_detect(borrower_genders, "(?<!fe)male")) ~ "mixed",
        (!str_detect(borrower_genders, "female") & str_detect(borrower_genders, "(?<!fe)male")) ~ "only male",
        (str_detect(borrower_genders, "female") & !str_detect(borrower_genders, "(?<!fe)male")) ~ "only female",
        is.na(borrower_genders) ~ "missing",
        TRUE ~ "uncaught" # uncaught case
      )
    )

# Convert to group gender composition to factor
kiva_c <-
  kiva_c %>%
  mutate_at(vars(group_gender_composition), funs(factor(., ordered = F))) %>%
  
# Drop borrower genders
kiva_c <- select(kiva_c, -borrower_genders)

# Append group picture composition -------
# none, some, all

kiva_c <-
  kiva_c %>%
    mutate(
      members_pictured = factor(case_when(
        (str_detect(str_to_lower(borrower_pictured), "true") & str_detect(str_to_lower(borrower_pictured), "false")) ~ "some",
        (str_detect(str_to_lower(borrower_pictured), "true") & !str_detect(str_to_lower(borrower_pictured), "false")) ~ "all",
        (!str_detect(str_to_lower(borrower_pictured), "true") & str_detect(str_to_lower(borrower_pictured), "false")) ~ "none",
        TRUE ~ "uncaught"
      ), ordered = T, levels = c("none", "some", "all"))
    ) %>%
    select(-borrower_pictured) 
# Handle countries columns ----------

kiva_c <-
  kiva_c %>%
    select(-country_code)

# Check for duplicate countries
unique(kiva_c$country_name) %>% View

# Repayment interval ---------------------

kiva_c %>%
  select(repayment_interval) %>%
  group_by(repayment_interval) %>%
  count()

# Funding: attempt 2 ---------------------

kiva_c <-
  kiva_c %>%
  mutate(
    funding_deficit = (loan_amount - funded_amount)
  ) %>%
  filter(!is.na(underfunded))

# write_csv(x = kiva_c, path = "kiva_v3.csv", na = "")
  
# Linear Model & Scatterplots ----------------------------------------

kiva_lm <- 
  lm(
    data = filter(kiva_c, funding_deficit > 0 & loan_amount < 20000), 
    formula = log(funding_deficit) ~ loan_amount + lender_term + 
      num_lenders_total + members_pictured
  )

# plot(kiva_lm)
summary(kiva_lm)

ggplot(
    filter(kiva_c, funding_deficit > 0 & loan_amount < 20000), 
    aes(loan_amount, log(funding_deficit))
  ) +
  geom_bin2d()

is_dblint <- function(x) { ( is_integer(x) | is_double(x) ) & !all(is.factor(x)) }

kiva_c %>%
  select(-loan_id) %>%
  select_if(is_dblint) %>%
  cor %>%
  ggcorrplot()

kiva_nums <-
  kiva_c %>%
  select(-loan_id) %>%
  select_if(is_dblint)

kiva_nums_u5k <-
  kiva_nums %>%
  filter(loan_amount <= 5000)

is_id <- function(column_name) {
  
  str_detect(column_name, "_id")
  
}

kv_nm_strs <- names(kiva_nums_u5k)
colors <- rainbow(length(kv_nm_strs))
ggplots <- list()

for (i in seq_along(kv_nm_strs)) {
  
  color_i <- colors[i]  
  name_i <- kv_nm_strs[i]
  
  for (j in seq_along(kv_nm_strs)) {

    name_j <- kv_nm_strs[j]
    
    if(name_i != name_j & !is_id(name_i) & !is_id(name_j)) {
      
      ggplots[[paste0("plot",i,".",j)]] <-
        ggplot(kiva_nums_u5k, aes_string(x = name_i, y = name_j)) +
        geom_bin2d() +
        geom_smooth(method = "lm", color = color_i) +
        # labs(
        #   title = paste0(name_j, " vs ", name_i)
        # ) +
        theme_minimal()
      
    }
    
  }
}

# Cleanup
rm(kiva_nums, kiva_nums_u5k)

# Loop KNN model for 50k row, 100 times for different K values and look at misclass distribution ------

kiva_nums_pre <-
  kiva_c %>%
  select(
    loan_amount, currency_exchange_coverage_rate, partner_id, lender_term,
    num_journal_entries, num_bulk_entries, underfunded, num_group_members
  )

# # (FOR DEBUGGING - SINGLE ITERATION FOR k=1 to k=2)
# 
# knn_outs <- 
#   iterate_kivanums_knn(
#     n_iter = 100, 
#     kmin = 1, 
#     kmax = 2
#   )

# Parameters to loop
minmax <-
  tibble(
    n_iter = 100,
    kmin = seq(from = 1, to = 15, by = 2),
    kmax = seq(from = 2, to = 16, by = 2)
  )

# Setup the parallel processing
plan(multiprocess(workers = 8, gc = TRUE))

start <- Sys.time() # for tracking duration

# Do the work
fmap_res <-
  future_pmap(minmax, iterate_kivanums_knn)

te <- Sys.time() - start # time elapsed

# Aggregate knn ensemble results & plot ----------

res_df <-
  tibble(
    kval = integer(0),
    avg_misclass = double(0),
    avg_senst = double(0),
    avg_specf = double(0)
  )

for(entry in seq_along(fmap_res)) {
  
  res_df <-
    res_df %>%
    bind_rows(fmap_res[[entry]])
  
}

# Plot results
res_df %>%
  gather(key = "metric", value = "measurement", avg_misclass, avg_senst, avg_specf) %>%
  ggplot(aes(x = kval, y = measurement, color = metric)) +
  geom_point() +
  geom_line()

# -------------------------------------------------