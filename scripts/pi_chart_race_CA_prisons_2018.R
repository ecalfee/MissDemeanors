library(dplyr)
library(ggplot2)
library(tidyr)
library(here)

# 2018 census estimates (Hispanic is any race and inviduals who are not Hispanic are counted in White/Black race categories)
# accessed from https://data.census.gov/cedsci/table?q=B03002&g=0400000US06&tid=ACSDT1Y2018.B03002&hidePreview=true
pop_residents <- data.frame(group = c("White", "Black", "Hispanic", "Other/Unknown"),
                  number = c(14495479, 2179078, 15540142, 139116+5741886+143302+100696+1217346)) %>%
  dplyr::mutate(percent = number/sum(number)*100,
                population = "CA residents")
# 2018 prisoner counts from CA Dept. of Corrections and Rehabilitation 'Offender Data Points 2018'
# Table 1.17 accessed from https://www.cdcr.ca.gov/research/wp-content/uploads/sites/174/2020/01/201812_DataPoints.pdf
pop_prisoners <- data.frame(group = c("White", "Black", "Hispanic", "Other/Unknown"),
                     number = c(26819, 36183, 56275, 8432)) %>%
  dplyr::mutate(percent = number/sum(number)*100,
                population = "CA prisoners")

pie <- bind_rows(pop_residents, pop_prisoners) %>%
  # order legend
  dplyr::mutate(group = factor(group, ordered = T, 
                               levels = c("Black", "White", "Hispanic", "Other/Unknown")),
                population = factor(population, ordered = T,
                                    levels = c("CA residents", "CA prisoners"))) %>%
  # piechart
  ggplot(., aes(x = "", y = percent, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~population) +
  theme_void() +
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 12))

# save pie chart
ggsave(filename = here("other_plots/pie_chart_race_ethnicity_CA_prisons_2018.png"),
       plot = pie,
       height = 3, width = 5, units = "in",
       dpi = 300, device = "png",
       bg = "transparent")
