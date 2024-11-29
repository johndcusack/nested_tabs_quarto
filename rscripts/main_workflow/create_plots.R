source('rscripts/data_prep.R')
source('rscripts/nhs_colour_palette.R')

##################
## plot objects ##
##################

ggobj <- outpatient_capacity_data |> 
  filter(org_code == 'Y59' & tfc_short_name == 'Total') |> 
  select(dimension_name,
         organisation_name,
         perc,
         target)

ggobj |> 
  ggplot(aes(x = dimension_name)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(color = NHS_midgrey),
        panel.background = element_rect(fill = "transparent"),
        legend.position = 'bottom'
  )+
  guides(col = guide_legend(nrow = 1))+
  geom_col(aes(y = perc, fill = '% first or procedure')) +
  geom_line(aes(y = target, color = 'target'),
            linewidth = 1) +
  scale_y_continuous(limits = c(0,0.7),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "", values = c('% first or procedure' = NHS_aquablue))+
  scale_color_manual(name = "",values = c('target' = NHS_blue))