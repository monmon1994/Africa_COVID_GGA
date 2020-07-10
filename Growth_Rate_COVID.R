### Grwoth rate graphs for newsletter GGA

library(utils)
library(tidyverse)
library(lubridate)
library(countrycode)
library(tidycovid19)
library(scales)
library(extrafont)
library(ggrepel)
library(zoo)
library(ggthemes)
library(dplyr)

df <- tidycovid19::download_jhu_csse_covid19_data(silent = T, cached = F)

df_afr <- df %>% 
    filter(iso3c %in% c("CMR", "ETH", "NGA", "KEN", "ZWE", "ZAF")) %>% 
    mutate(
        new_cases = confirmed - lag(confirmed)
    ) 

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

recover_c <- "#7FBC41"
confirm_c <- "#4393C3" 

df_days_since <- df_afr %>%
    filter(confirmed > 100) %>% 
    group_by(country) %>%
    mutate(days_since_100 = as.numeric(date - min(date))) %>%
    ungroup 

# Growth rate graph ZAF

df_days_since %>% 
    filter(iso3c == "ZAF") %>% 
    ggplot() +
    geom_line(aes(days_since_100, confirmed), col = "#0072B2", size = 1) +
    scale_y_log10(labels = comma_format()) +
    scale_x_continuous(expand = expansion(add = c(0,1))) +
    theme(panel.background = element_rect(fill = "#f5f5f5"),
          text = element_text(family = "Helvetica"),
          panel.grid.major = element_line(colour = "grey87"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#f5f5f5"),
          plot.background = element_rect(fill = "#f5f5f5"),
          strip.text.x = element_text(hjust = 0),
          plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
    labs(title = "Growth rate of confirmed cases in South Africa", x = "Days since the 100th confirmed case", y = "log scale",
         caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 31 May, 2020
  Graphic: Monique Bennett at Good Governance Africa")

ggsave("ZAF_growthrate.png",  plot = last_plot(), dpi = 400, height = 8, width = 8)

## Kenya Grwoth rate

df_days_since %>% 
    filter(iso3c == "KEN") %>% 
    ggplot() +
    geom_line(aes(days_since_100, confirmed), col = "#0072B2", size = 1) +
    scale_y_log10(labels = comma_format()) +
    scale_x_continuous(expand = expansion(add = c(0,1))) +
    theme(panel.background = element_rect(fill = "#f5f5f5"),
          text = element_text(family = "Helvetica"),
          panel.grid.major = element_line(colour = "grey87"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#f5f5f5"),
          plot.background = element_rect(fill = "#f5f5f5"),
          strip.text.x = element_text(hjust = 0),
          plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
    labs(title = "Growth rate of confirmed cases in Kenya", x = "Days since the 100th confirmed case", y = "log scale",
         caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 31 May, 2020
  Graphic: Monique Bennett at Good Governance Africa")

## ETH growth

df_days_since %>% 
    filter(iso3c == "ETH") %>% 
    ggplot() +
    geom_line(aes(days_since_100, confirmed), col = "#0072B2", size = 1) +
    scale_y_log10(labels = comma_format()) +
    scale_x_continuous(expand = expansion(add = c(0,1))) +
    theme(panel.background = element_rect(fill = "#f5f5f5"),
          text = element_text(family = "Helvetica"),
          panel.grid.major = element_line(colour = "grey87"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#f5f5f5"),
          plot.background = element_rect(fill = "#f5f5f5"),
          strip.text.x = element_text(hjust = 0),
          plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
    labs(title = "Growth rate of confirmed cases in Ethiopia", x = "Days since the 100th confirmed case", y = "log scale",
         caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 31 May, 2020
  Graphic: Monique Bennett at Good Governance Africa")

## ZWE

df_days_since %>% 
    filter(iso3c == "ZWE") %>% 
    ggplot() +
    geom_line(aes(days_since_100, confirmed), col = "#0072B2", size = 1) +
    scale_y_log10(labels = comma_format()) +
    scale_x_continuous(expand = expansion(add = c(0,1))) +
    theme(panel.background = element_rect(fill = "#f5f5f5"),
          text = element_text(family = "Helvetica"),
          panel.grid.major = element_line(colour = "grey87"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#f5f5f5"),
          plot.background = element_rect(fill = "#f5f5f5"),
          strip.text.x = element_text(hjust = 0),
          plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
    labs(title = "Growth rate of confirmed cases in Zimbabwe", x = "Days since the 100th confirmed case", y = "log scale",
         caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 31 May, 2020
  Graphic: Monique Bennett at Good Governance Africa")

## NGA 

df_days_since %>% 
    filter(iso3c == "NGA") %>% 
    ggplot() +
    geom_line(aes(days_since_100, confirmed), col = "#0072B2", size = 1) +
    scale_y_log10(labels = comma_format()) +
    scale_x_continuous(expand = expansion(add = c(0,1))) +
    theme(panel.background = element_rect(fill = "#f5f5f5"),
          text = element_text(family = "Helvetica"),
          panel.grid.major = element_line(colour = "grey87"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#f5f5f5"),
          plot.background = element_rect(fill = "#f5f5f5"),
          strip.text.x = element_text(hjust = 0),
          plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
    labs(title = "Growth rate of confirmed cases in Nigeria", x = "Days since the 100th confirmed case", y = "log scale",
         caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 31 May, 2020
  Graphic: Monique Bennett at Good Governance Africa")

## CMR

df_days_since %>% 
    filter(iso3c == "CMR") %>% 
    ggplot() +
    geom_line(aes(days_since_100, confirmed), col = "#0072B2", size = 1) +
    scale_y_log10(labels = comma_format()) +
    scale_x_continuous(expand = expansion(add = c(0,1))) +
    theme(panel.background = element_rect(fill = "#f5f5f5"),
          text = element_text(family = "Helvetica"),
          panel.grid.major = element_line(colour = "grey87"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#f5f5f5"),
          plot.background = element_rect(fill = "#f5f5f5"),
          strip.text.x = element_text(hjust = 0),
          plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
    labs(title = "Growth rate of confirmed cases in Ethiopia", x = "Days since the 100th confirmed case", y = "log scale",
         caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 31 May, 2020
  Graphic: Monique Bennett at Good Governance Africa")
