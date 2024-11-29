library(tidyverse)
library(readxl)
source('rscripts/main_workflow/data_import.R')

report_month <- max(unique(outpatient_capacity_data$dimension_name))

quarto::quarto_render(input = 'nested_tabs_quarto.qmd',
                      output_file = paste0('nested_tabs_',report_month,'.html'))