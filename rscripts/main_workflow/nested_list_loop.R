# Initialize the output lists
opps_ls <- list()

# Loop over each ICB code
for (icb in unique(challenges_by_trust$icb_code)) {
  icbcode <- icb
  
  # Filter data for the current ICB
  child_data <- challenges_by_trust |> 
    filter(icb_code == icb)
  
  # Initialize a list to store tables for each organization within this ICB
  out_orgs <- list()
  
  # Loop over each organization code within the ICB
  for (org in unique(child_data$org_code)) {
    orgcode <- org
    
    # Create the table for the current organization
    tbl_child <- child_data |> 
      filter(org_code == orgcode) |>
      select(
        tfc_short_name,
        perc,
        median_perc,
        var_from_median,
        vol_perc
      ) |>
      mutate(
        perc = formattable::percent(perc),
        median_perc = formattable::percent(median_perc),
        var_from_median = formattable::percent(var_from_median),
        vol_perc = formattable::percent(vol_perc)
      ) |> 
      rename(
        'specialty' = tfc_short_name,
        'trust percentage' = perc,
        'regional median' = median_perc,
        'variance' = var_from_median,
        'percentage of total activity' = vol_perc
      )
    
    # Convert to a formattable and datatable object
    tbl_child <- formattable::formattable(tbl_child)
    tbl_child <- formattable::as.datatable(tbl_child,
                                           class = 'cell-border stripe',
                                           options = list(
                                             pageLength = nrow(tbl_child),
                                             dom = 't'
                                           ),
                                           rownames = FALSE,
                                           escape = FALSE)
    
    # Append the datatable for this organization to out_orgs list
    out_orgs[[orgcode]] <- tbl_child
  }
  
  # Store the list of organization tables for the current ICB in the out list
  opps_ls[[icbcode]] <- out_orgs
}

# The opps_ls list will now contain tables for each ICB and each organization within each ICB

