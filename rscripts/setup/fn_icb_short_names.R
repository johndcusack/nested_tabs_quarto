fn_icb_short_names <- function(df, code_column){
names_lookup <- data.frame(
  icb_code = c(
    #bob
    'QU9',
    #Frimley
    'QNQ',
    #HIOW
    'QRL',
    #KM
    'QKS',
    #Surrey
    'QXU',
    #Sussex
    'QNX',
    #Region
    'Y59'),
  icb_short_name = c(
    #bob
    'BOB ICS',
    #frimley
    'Frimley ICS',
    #HIOW
    'HIOW ICS',
    #KM 
    'KM ICS',
    #Surrey
    'Surrey ICS',
    #Sussex
    'Sussex ICS',
    #Region
    'SE Region'))

df <- left_join(df,
                names_lookup,
                by = setNames('icb_code',code_column))

return(df)

}