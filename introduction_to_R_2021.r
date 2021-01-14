# Introduction to R 
library(tidyverse)
library(lubridate)
library(viridis)
library(viridisLite)


tp_01 <- 
  readxl::read_excel(
    path = "total_population_01.xlsx", 
    sheet = "Data"
  )

tp_01_tidy <- 
  tp_01 %>% 
  tidyr::pivot_longer(
    cols = 
      c(
        "total", 
        "female", 
        "male"
      ),
    names_to = "gender",
    values_to = "population"
  ) %>% 
  dplyr::mutate(
    gender = factor(gender),
    year = lubridate::ymd(
      paste(
        year, "-01-01", 
        sep = ""
      )
    )
  )


tp_01_lineplot_01 <- 
  tp_01_tidy %>% 
  ggplot(
    aes(
      x = year, 
      y = population, 
      color = gender
    )
  ) +
  geom_line() +
  geom_text() +
  labs(
    title = "Population in Asian Countries",
    subtitle = "Source: UN World Population Prospects (https://population.un.org/wpp/)", 
    x = "Year (1950-2100)", 
    y = "Population (Unit: 1,000Pax.)", 
    color = "Gender"
  ) +
  scale_color_viridis(
    option = "plasma",
    discrete = TRUE
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.7, 0.2),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12)
  )



