fn_short_org_names <- function(df, code_column){
names_lookup <- data.frame(
  org_code = c(
    #BOB
    'QU9','RHW','RTH','RWX','RXQ','RNU',
    #Frimley
    'QNQ','RDU',
    #HIOW
    'QRL','R1F','RHM','RHU','RN5','RW1','R1C','RYE',
    #KM
    'QKS','RN7','RPA','RVV','RWF','RXY','RYY',
    #Surrey
    'QXU','RA2','RTK','RTP','RXX','RYD',
    #Sussex
    'QNX','RPC','RXC','RYR','RX2',
    #Region
    'Y59'),
  org_short_name = c(
    #BOB
    'BOB ICS','RBH','OUH','Berks Health','BHT','OHealth',
    #Frimley
    'Frimley ICS','Frimley',
    #HIOW
    'HIOW ICS','IOW','UHSotn','PHU','HHFT','SouthernHealth','Solent','SCAS',
    #KM
    'KM ICS','DGT','MedwayFT','EKH','MTW','KM SCP','KCH',
    #Surrey
    'Surrey ICS','RSCH','ASP','SASH','SBorders','SECAMB',
    #Sussex
    'Sussex ICS','QVH','ESH','UHSx','SxPartnership',
    #Region
    'SE Region'))

df <- left_join(df,
                names_lookup,
                by = setNames('org_code',code_column))
return(df)
}