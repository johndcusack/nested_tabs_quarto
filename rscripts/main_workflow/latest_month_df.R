# getting the max data period as we are going to be focussing on the 
# latest freeze month for the section these tables will be used in

max_month <- max(outpatient_capacity_data$dimension_name)

latest_month <- outpatient_capacity_data |> 
  filter(dimension_name == max_month)


# now we need to calculate the percentage of an organisation's total activity 
# accounted for by each specialty 

latest_month_denom <- latest_month |> 
  select(org_code,
         treatment_function_code,
         denominator)

latest_month_total <- latest_month_denom |> 
  filter(treatment_function_code == 'Total') |> 
  rename(total = denominator) |> 
  select(-treatment_function_code)

latest_month_denom <- latest_month_denom |> 
  filter(treatment_function_code != 'Total')

latest_month_both <- left_join(latest_month_denom,
                          latest_month_total,
                          by = 'org_code',
                          keep = FALSE)

latest_month_vol <- latest_month_both |> 
  mutate(vol_perc = denominator/total) |> 
  select(org_code,
         treatment_function_code,
         vol_perc)

latest_month <- left_join(latest_month,
                          latest_month_vol,
                          join_by(org_code,
                                  treatment_function_code))

rm(latest_month_denom,
   latest_month_total,
   latest_month_both,
   latest_month_vol)

### how we identify all of the rows where the volume is at least 1% of that 
# reporter's activity. We are getting rid of the smallest specialties because 
# small volumes can throw off ranking by percentage which we are going to do in 
# next

largest_specs <- latest_month |> filter(vol_perc >= 0.01)

### now we rank the specialties for each organisation, 
# with both ascending and descending ranks
# the ranking is based on the variance from the ambition target
# ranking under has 1 as the furthest below target
# ranking over has 1 as the most above target

ranked_specs <- largest_specs |> 
  mutate(ranking_under = dense_rank(variance), .by = treatment_function_code) |> 
  mutate(ranking_over = dense_rank(desc(variance)), .by = treatment_function_code)

## top performers per specialty

highest_performers <- ranked_specs |> filter(ranking_over <= 5, .by = treatment_function_code)

## lowest performers per specialty

furthest_off <- ranked_specs |> filter(ranking_under <= 5, .by = treatment_function_code)
  
## now we are looking at the median percentage for each specialty, 
# getting the variance each provider trust has from that median,
# we then rank the variances grouped by specialty in descending order. This will mean that number 1 will be the 
# trust doing the highest proportion of first and procedure activity we will use that to identify who to 
# point the worst performers to for a conversation

ranked_percents <- largest_specs |> 
  filter(str_sub(org_code,1,1) == 'R') |> 
  mutate(median_perc = median(perc), .by = treatment_function_code) |> 
  mutate(var_from_median = perc - median_perc) |> 
  mutate(spec_top_var = dense_rank(desc(var_from_median)),.by = treatment_function_code)

## now we create a top 3 performers per specialty and trim out unneeded fields

highest_percents <- ranked_percents |> 
  filter(spec_top_var <= 3) |> 
  select(icb_short_name,
         org_short_name,
         treatment_function_code,
         tfc_short_name,
         perc,
         median_perc,
         var_from_median,
         spec_top_var) |> 
  group_by(treatment_function_code) |> 
  arrange(spec_top_var, .by_group = TRUE) |> 
  ungroup()

tbl_highest_percents <- highest_percents |> 
  filter(tfc_short_name != 'Diagnostic Imaging') |> 
  select(tfc_short_name,
         org_short_name,
         spec_top_var) |> 
  rename(specialty = tfc_short_name) |> 
  mutate(spec_top_var = paste0('Trust ',spec_top_var)) |> 
  pivot_wider(values_from = org_short_name,
              names_from = spec_top_var)
  
tbl_highest_percents <- datatable(tbl_highest_percents,
                                  class = 'cell-border stripe',
                                  options = list(
                                    pageLength = nrow(tbl_highest_percents),
                                    dom = 'f,t'
                                    ),
                                  rownames = FALSE,
                                  escape = FALSE)
  
  # looking at the same distance from median information, but this time we are looking for the trust's biggest areas where they could 
# improve. We do this by finding the median variance per specialty, the distance from the median for each row, 
# filter the data to just variances of less than 0, this means that no-one's list of challenges will include specialties where they are
# at the median value. 
# We rank the remaining variances grouped by trust. We go smallest to largest, so that the most negative value is 1
# this gives us the specialties where the trust is showing the "worst" performance compared to the regional median 

challenges_by_trust <- largest_specs |> 
  filter(str_sub(org_code,1,1) == 'R') |> 
  mutate(median_perc = median(perc), .by = treatment_function_code) |> 
  mutate(var_from_median = perc - median_perc) |>
  filter(var_from_median < 0) |> 
  mutate(ranked_variance = dense_rank(var_from_median), .by = org_code) 


## now we trim down the challenges by trust to just give the 5 areas with highest opportunity for improvement

challenges_by_trust <- challenges_by_trust |> 
  group_by(icb_short_name, org_short_name) |> 
  arrange(ranked_variance, .by_group = TRUE) |> 
  ungroup()

