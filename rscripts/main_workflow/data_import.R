outpatient_capacity_data <- read_excel("data/randomised_capacity_data.xlsx", 
                                       col_types = c("numeric", "text", "text", 
                                                     "text", "text", "date", "text", "text", 
                                                     "numeric", "numeric", "numeric"),
                                       sheet = "data_table")
target_data <- read_excel("data/randomised_capacity_data.xlsx", 
                                       sheet = "target_table")
