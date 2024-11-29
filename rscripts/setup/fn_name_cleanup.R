# This creates a function to clean up the names for systems and trusts 
# it is needed because the standard lookup table in NCDR has the names in all caps 
# with some inconsistencies. 

# enquo is used to retain the column name 
# !! turns the column name into a call for the data in the column allowing the 
# function to dynamically use a column name that was passed to it as an argument

# := is for name injection and is used to assign values to columns, in this case it
# ensures that the column specified by namefield is updated correctly
 
fn_name_cleanup <- function(df,namefield){
  namefield <- enquo(namefield)

  df <- df |> 
    mutate(!!namefield := str_to_title(!!namefield)) |>
    mutate(!!namefield := gsub('And','and',!!namefield)) |> 
    mutate(!!namefield := gsub('Nhs','NHS',!!namefield)) |> 
    mutate(!!namefield := gsub("Of","of",!!namefield)) |> 
    mutate(!!namefield := gsub("Integrated Care Board","ICB",!!namefield))
  
  return(df)
}
