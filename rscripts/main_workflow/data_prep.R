outpatient_capacity_data <- clean_names(outpatient_capacity_data)
target_data <- clean_names(target_data)

# clean up names of organisations
outpatient_capacity_data <- outpatient_capacity_data |> 
  fn_name_cleanup(system_name)

outpatient_capacity_data <- outpatient_capacity_data |> 
  fn_name_cleanup(organisation_name)


# placeholder code to replace a cleanup function used in real process

outpatient_capacity_data<- outpatient_capacity_data |> mutate(
  org_short_name = organisation_name,
  icb_short_name = system_name
)

target_data <- target_data |> 
  mutate(org_short_name = name)


# blend target data into outpatient capacity data

target <- target_data |> select(org_code,percentage) |> rename(target = percentage)

outpatient_capacity_data <- left_join(outpatient_capacity_data,
                                      target,
                                      by = 'org_code')

rm(target)

# Adding variance and rag columns to data

outpatient_capacity_data <- outpatient_capacity_data |> 
  mutate(variance = perc-target)

# create rag column using colours from rag colours
outpatient_capacity_data <- outpatient_capacity_data |> 
  mutate(rag = case_when(perc <= target*0.95 ~ red_rag,
                         perc >= target ~ green_rag,
                         .default = amber_rag))