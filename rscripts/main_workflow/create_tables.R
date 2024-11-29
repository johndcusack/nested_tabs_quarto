
####################
## capacity table ##
####################

op_cap_total <- outpatient_capacity_data |> 
  filter(tfc_short_name == 'Total') |> 
  select(icb_short_name,
         org_short_name,
         dimension_name,
         perc,
         rag) |> 
  mutate(month_commencing = format(dimension_name, '%b-%Y')) |> 
  rename(system_name = icb_short_name,
         organisation_name = org_short_name)

# we are going to use sprintf to create some dynamic html and CSS code
# this code will be used to create a formatted column that will be used as the values 
# in the tables that will go into our report

# the html is <span> </span> we are then adding CSS key value pairs to tell datatable
# what we would like the cells to look like. This is mostly for conditional formatting but
# also lets us 

op_cap_total <- op_cap_total |> 
  mutate(formatted_value = case_when(rag == red_rag ~ sprintf(
    '<span style="display: block; padding: 0 4px; text-align: center; border-radius: 4px; color: white; background-color: %s;">%.1f%% </span>',
    rag,perc * 100),
    .default = sprintf(
      '<span style="display: block; padding: 0 4px; text-align: center; border-radius: 4px; background-color: %s;">%.1f%% </span>',
      rag,perc * 100))) |> 
  arrange(system_name,
          organisation_name,
          dimension_name)

tbl_overall_cap <- op_cap_total |> 
  select(-c(perc,
            rag,
            dimension_name)) |> 
  pivot_wider(names_from = month_commencing, values_from = formatted_value) |> 
  rename(c(System = system_name,Organisation = organisation_name ))

tbl_overall_cap <- datatable(tbl_overall_cap,
                              class = 'cell-border stripe',
                              options = list(
                                pageLength = nrow(tbl_overall_cap),
                                dom = 'ft'
                              ),
                              rownames = FALSE,
                              escape = FALSE)

#####################
## Ambition Table  ##
#####################

ambition_table <- target_data |> 
  filter(str_sub(org_code,1,1) == 'Q') |> 
  select(name,percentage) |> 
  rename(c(System = name,
           Ambition = percentage)) |> 
  arrange(System)

ambition_table <- formattable(ambition_table,
                               align = c('l','c'),
                               list(
                                 Ambition = formatter('span',
                                                        x ~ sprintf("%.1f%%", x * 100))  # Format as percentage text
))

ambition_table <- as.datatable(
  ambition_table,
  class = 'cell-border stripe',
  rownames = FALSE,
  options = list(paging = FALSE,
                 dom = 't'))

