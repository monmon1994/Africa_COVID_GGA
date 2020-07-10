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
library(ggtext)
library(patchwork)
library(ggplot2)
library(dplyr)

loadfonts(device = "pdf", quiet = T)

data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

## Filter out africa Kenya, Ethiopia, Nigeria, Zimbabwe, South Africa, Cameroon


df_africa  <- data %>% 
  filter(countryterritoryCode %in% c("CMR", "ETH", "NGA", "KEN", "ZWE", "ZAF")) %>% 
  mutate(date = lubridate::dmy(dateRep),
         iso2 = geoId,
         iso3 = countryterritoryCode)

## tidycovid19

df <- tidycovid19::download_jhu_csse_covid19_data(silent = T, cached = F)

df_afr <- df %>% 
  filter(iso3c %in% c("CMR", "ETH", "NGA", "KEN", "ZWE", "ZAF")) %>% 
  mutate(
    new_cases = confirmed - lag(confirmed)
  ) 

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

recover_c <- "#7FBC41"
confirm_c <- "#4393C3" 

breaks <- c(0, 150, 300, 450, 600, 750, 900, 1050, 1200)

## facewrap 

df_afr %>% 
  filter(date > "2020-02-28") %>% 
  ggplot(aes(group = country)) +
  geom_line(aes(x = date, y = confirmed), size = 1, linetype = 1, col = confirm_c) +
  geom_line(aes(x = date, y = recovered), size = 1, linetype = 1, col = recover_c) + 
  gghighlight::gghighlight(use_direct_label = FALSE,unhighlighted_params = list(size=0.4))+
  #scale_x_date(date_breaks = "7 days", date_labels = "%e-%b") +
  scale_y_log10(labels = comma_format()) +
  labs(x = "", y = "", caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)
       Data obtained on 22 May, 2020") +
  theme(panel.background = element_rect(fill = "#f5f5f5"),
        text = element_text(family = "Helvetica"),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0)) +
  facet_wrap(~country, ncol = 2, scales = "free") 

## Restart: seperate plots for each country first

# Kenya 

df_afr %>% 
  filter(iso3c == "KEN") %>% 
  filter(date > "2020-03-12") %>% 
  ggplot() +
  geom_line(aes(x = date, y = confirmed), size = 1, linetype = 1, col = confirm_c) +
  geom_line(aes(x = date, y = recovered), size = 1, linetype = 5, col = recover_c) +
  scale_x_date(date_breaks = "7 days", date_labels = "%e-%b") +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  annotate("text", x = as.Date("2020-07-09"), y = 8975, label = "Confirmed",
           hjust = 0.8, vjust = -0.5, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-07-09"), y = 2657, label = "Recoveries",
           hjust = 0.8, vjust = -0.5, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-03-27"), y = 150, label = "Restriction on internal movement",
           family = "Helvetica", size = 3.5, vjust = -0.5, hjust = 0.65) +
  annotate(geom = "segment", x = as.Date("2020-03-27"), y = 150, 
           xend = as.Date("2020-03-27"), yend = 32, 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("text", x = as.Date("2020-04-06"), y = 300, label = "Stay-at-home (required with few exceptions)",
           family = "Helvetica", size = 3.5, vjust = -0.5) +
  annotate(geom = "segment", x = as.Date("2020-04-06"), y = 299, 
           xend = as.Date("2020-04-06"), yend = 158, 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  #annotate("text", x = as.Date("2020-03-28"), y = 7500, 
  #label = paste(strwrap("Testing only for those who both (a) have symptoms AND 
   #                     (b) meet specific criteria. Limited contact tracing.", 40), collapse = "\n"),
  #       family = "Helvetica", size = 4) +
  #annotate("rect", xmin = as.Date("2020-03-13"), ymin = 870, xmax = as.Date('2020-04-12'), ymax = 1038,
   #        alpha = 0.4, color = "gray", fill = "#666666") +
  theme(panel.background = element_rect(fill = "#f5f5f5"),
        text = element_text(family = "Helvetica"),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
  labs(title = "Kenya", x = "", y = "", 
  caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 25 May, 2020
  Graphic: Monique Bennett at @GoodGovernanceAfrica")

ggsave("KEN.png", plot = last_plot(), dpi = 400, height = 8, width = 8)

## Cameroon

df_afr %>% 
  filter(iso3c == "CMR") %>% 
  filter(date > "2020-03-05") %>% 
  ggplot() +
  geom_line(aes(x = date, y = confirmed), size = 1, linetype = 1, col = confirm_c) +
  geom_line(aes(x = date, y = recovered), size = 1, linetype = 5, col = recover_c) +
  scale_x_date(date_breaks = "6 days", date_labels = "%e-%b") +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  annotate("text", x = as.Date("2020-05-21"), y = 4890, label = "Confirmed",
           hjust = 0.5, vjust = -0.5, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-05-24"), y = 1865, label = "Recoveries",
           hjust = 0.8, vjust = -0.4, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-03-18"), y = 200, label = "Recommended movement restrictions",
           family = "Helvetica", size = 3.5, vjust = -0.5, hjust = 0.5) +
  annotate(geom = "segment", x = as.Date("2020-03-18"), y = 200, 
           xend = as.Date("2020-03-18"), yend = 10, 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("text", x = as.Date("2020-05-09"), y = 3055, label = "No lockdown or stay-at-home requirements",
           family = "Helvetica", size = 3.5, vjust = -0.3, hjust = 0.9) +
  annotate(geom = "curve", x = as.Date("2020-05-09"), y = 3000, xend = as.Date("2020-05-13"), yend = 2800, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  annotate("text", x = as.Date("2020-03-19"), y = 4000, 
           label = paste(strwrap("Testing of anyone showing COVID-19 symptoms. No contact tracing.", 40), collapse = "\n"),
           family = "Helvetica", size = 4) +
  annotate("rect", xmin = as.Date("2020-03-03"), ymin = 3700, xmax = as.Date('2020-04-04'), ymax = 4200,
           alpha = 0.4, color = "gray", fill = "#666666") +
  theme(panel.background = element_rect(fill = "#f5f5f5"),
        text = element_text(family = "Helvetica"),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
  labs(title = "Cameroon", x = "", y = "", 
  caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 25 May, 2020
  Graphic: Monique Bennett at @GoodGovernanceAfrica")

ggsave("CRM.png", plot = last_plot(), dpi = 400, height = 8, width = 8)

## Ethiopia

df_afr %>% 
  filter(iso3c == "ETH") %>% 
  filter(date > "2020-03-12") %>% 
  ggplot() +
  geom_line(aes(x = date, y = confirmed), size = 1, linetype = 1, col = confirm_c) +
  geom_line(aes(x = date, y = recovered), size = 1, linetype = 5, col = recover_c) +
  scale_x_date(date_breaks = "7 days", date_labels = "%e-%b") +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  annotate("text", x = as.Date("2020-05-21"), y = 582, label = "Confirmed",
           hjust = 0.5, vjust = -0.5, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-05-24"), y = 152, label = "Recoveries",
           hjust = 0.8, vjust = -0.3, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-03-26"), y = 50, label = "Restriction on internal movement",
           family = "Helvetica", size = 3.5, vjust = -0.3, hjust = 0.5) +
  annotate(geom = "segment", x = as.Date("2020-03-18"), y = 50, 
           xend = as.Date("2020-03-26"), yend = 12, 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("text", x = as.Date("2020-04-08"), y = 150, label = "Stay-at-home recommended and state of emergency declared",
           family = "Helvetica", size = 3.5, vjust = -0.3) +
  annotate(geom = "curve", x = as.Date("2020-04-08"), y = 150, xend = as.Date("2020-04-08"), yend = 55, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  annotate("text", x = as.Date("2020-03-28"), y = 500, 
           label = paste(strwrap("Testing only for those who both (a) have symptoms AND 
                        (b) meet specific criteria. Comprehensive contact tracing (all cases).", 40), collapse = "\n"),
          family = "Helvetica", size = 4) +
  annotate("rect", xmin = as.Date("2020-03-13"), ymin = 450, xmax = as.Date('2020-04-12'), ymax = 550,
           alpha = 0.4, color = "gray", fill = "#666666") +
  theme(panel.background = element_rect(fill = "#f5f5f5"),
        text = element_text(family = "Helvetica"),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
  labs(title = "Ethiopia", x = "", y = "", 
       caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 25 May, 2020
  Graphic: Monique Bennett at @GoodGovernanceAfrica")

ggsave("ETH.png", plot = last_plot(), dpi = 400, height = 8, width = 8)

## Nigeria

df_afr %>% 
  filter(iso3c == "NGA") %>% 
  filter(date > "2020-02-28") %>% 
  ggplot() +
  geom_line(aes(x = date, y = confirmed), size = 1, linetype = 1, col = confirm_c) +
  geom_line(aes(x = date, y = recovered), size = 1, linetype = 5, col = recover_c) +
  scale_x_date(date_breaks = "6 days", date_labels = "%e-%b") +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  annotate("text", x = as.Date("2020-05-21"), y = 7839, label = "Confirmed",
           hjust = 0.5, vjust = -0.5, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-05-24"), y = 2263, label = "Recoveries",
           hjust = 0.8, vjust = -0.4, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-03-27"), y = 500, label = "Restriction on internal movement",
          family = "Helvetica", size = 3.5, vjust = -0.5, hjust = 0.9) +
  annotate(geom = "segment", x = as.Date("2020-03-27"), y = 500, 
          xend = as.Date("2020-03-29"), yend = 111, 
         arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("text", x = as.Date("2020-03-31"), y = 1000, label = "Stay-at-home required (except essentials) and total lockdown declared",
          family = "Helvetica", size = 3.5, vjust = -0.3, hjust = 0.5) +
  annotate(geom = "curve", x = as.Date("2020-03-31"), y = 1000, xend = as.Date("2020-03-31"), yend = 135, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  annotate("text", x = as.Date("2020-04-26"), y = 2000, label = "Relaxing of lockdown",
           family = "Helvetica", size = 3.5, vjust = -0.3, hjust = 0.8) +
  annotate(geom = "segment", x = as.Date("2020-04-26"), y = 2000, xend = as.Date("2020-04-28"), yend = 1532, 
           arrow =  arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("text", x = as.Date("2020-03-16"), y = 7000, 
        label = paste(strwrap("Testing only for those who both (a) have symptoms AND 
                      (b) meet specific criteria. Comprehensive contact tracing (all cases).", 40), collapse = "\n"),
       family = "Helvetica", size = 4) +
  annotate("rect", xmin = as.Date("2020-02-27"), ymin = 6300, xmax = as.Date('2020-04-03'), ymax = 7700,
           alpha = 0.4, color = "gray", fill = "#666666") +
  theme(panel.background = element_rect(fill = "#f5f5f5"),
        text = element_text(family = "Helvetica"),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
  labs(title = "Nigeria", x = "", y = "",
       caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 25 May, 2020
  Graphic: Monique Bennett at @GoodGovernanceAfrica")

ggsave("NGA.png", plot = last_plot(), dpi = 400, height = 8, width = 8)

## Zimbabwe

df_afr %>% 
  filter(iso3c == "ZWE") %>% 
  filter(date > "2020-03-20") %>% 
  ggplot() +
  geom_line(aes(x = date, y = confirmed), size = 1, linetype = 1, col = confirm_c) +
  geom_line(aes(x = date, y = recovered), size = 1, linetype = 5, col = recover_c) +
  scale_x_date(date_breaks = "7 days", date_labels = "%e-%b") +
  scale_y_continuous(breaks = scales::pretty_breaks(10)) +
  annotate("text", x = as.Date("2020-05-21"), y = 56, label = "Confirmed",
           hjust = 0.5, vjust = -0.5, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-05-24"), y = 25, label = "Recoveries",
           hjust = 0.8, vjust = -0.4, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-03-23"), y = 10, label = "Restriction on internal movement",
           family = "Helvetica", size = 3.5, vjust = -0.3, hjust = 0.4) +
  annotate(geom = "segment", x = as.Date("2020-03-18"), y = 10, 
           xend = as.Date("2020-03-23"), yend = 3, 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  annotate("text", x = as.Date("2020-03-27"), y = 15, label = "Stay-at-home required and lockdown announced",
           family = "Helvetica", size = 3.5, vjust = -0.4, hjust = 0.4) +
  annotate(geom = "curve", x = as.Date("2020-03-27"), y = 15, xend = as.Date("2020-03-27"), yend = 5, 
           curvature = .2, arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  annotate("text", x = as.Date("2020-04-01"), y = 50, 
           label = paste(strwrap("Testing only for those who both (a) have symptoms AND 
                        (b) meet specific criteria. Limited tracing (only some cases).", 40), collapse = "\n"),
           family = "Helvetica", size = 4) +
  annotate("rect", xmin = as.Date("2020-03-19"), ymin = 44, xmax = as.Date('2020-04-14'), ymax = 55,
           alpha = 0.4, color = "gray", fill = "#666666") +
  theme(panel.background = element_rect(fill = "#f5f5f5"),
        text = element_text(family = "Helvetica"),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
  labs(title = "Zimbabwe", x = "", y = "", 
       caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 25 May, 2020
  Graphic: Monique Bennett at @GoodGovernanceAfrica")

ggsave("ZWE.png", plot = last_plot(), dpi = 400, height = 8, width = 8)

## South Africa

df_afr %>% 
  filter(iso3c == "ZAF") %>% 
  filter(date > "2020-03-07") %>% 
  ggplot() +
  geom_line(aes(x = date, y = confirmed), size = 1, linetype = 1, col = confirm_c) +
  geom_line(aes(x = date, y = recovered), size = 1, linetype = 5, col = recover_c) +
  scale_x_date(date_breaks = "8 days", date_labels = "%e-%b") +
  scale_y_continuous(breaks = scales::pretty_breaks(20)) +
  annotate("text", x = as.Date("2020-06-01"), y = 32700, label = "Confirmed",
           hjust = 0.7, vjust = -0.3, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-06-01"), y = 16809, label = "Recoveries",
           hjust = 0.7, vjust = -0.3, family = "Helvetica", size = 6) +
  annotate("text", x = as.Date("2020-03-23"), y = 2000, 
           label =  paste(strwrap("Restriction on internal movement and 21-day lockdown announced", 40), collapse = "\n"),
           family = "Helvetica", size = 3.5, vjust = -0.2, hjust = 0.6
           ) +
  annotate(geom = "segment", x = as.Date("2020-03-23"), y = 2000, 
           xend = as.Date("2020-03-23"), yend = 402, 
           arrow = arrow(length = unit(0.2, "cm"), type = "closed")
           ) +
  annotate("text", x = as.Date("2020-04-09"), y = 4500, label = "Stay-at-home required and lockdown extended",
           family = "Helvetica", size = 3.5, vjust = -0.3, hjust = 0.7
           ) +
  annotate(geom = "curve", x = as.Date("2020-04-01"), y = 4300, xend = as.Date("2020-04-09"), yend = 1934, 
           curvature = .2, arrow = arrow(length = unit(2, "mm"), type = "closed")
           ) +
  annotate("text", x = as.Date("2020-05-01"), y = 8000, label = "Move to level 4 lockdown regulations",
           family = "Helvetica", size = 3.5, vjust = -0.3, hjust = 0.8
           ) +
  annotate(geom = "curve", x = as.Date("2020-04-29"), y = 8000, xend = as.Date("2020-05-01"), yend = 5951, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"), type = "closed")
            ) +
  annotate("text", x = as.Date("2020-05-25"), y = 31000, label = "Move to level 3 lockdown regulations",
           family = "Helvetica", size = 3.5, vjust = -0.3, hjust = 1
  ) +
  annotate(geom = "curve", x = as.Date("2020-05-25"), y = 31000, xend = as.Date("2020-05-31"), yend = 32683, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
  annotate("text", x = as.Date("2020-03-25"), y = 30000, 
           label = paste(strwrap("Open public testing and comprehensive contact tracing (all cases).", 40), collapse = "\n"),
           family = "Helvetica", size = 4) +
  annotate("rect", xmin = as.Date("2020-03-06"), ymin = 28000, xmax = as.Date('2020-04-13'), ymax = 31500,
           alpha = 0.4, color = "gray", fill = "#666666") +
  theme(panel.background = element_rect(fill = "#f5f5f5"),
        text = element_text(family = "Helvetica"),
        panel.grid.major = element_line(colour = "#f0f0f0"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), plot.title = element_text(size = 20)) +
  labs(title = "South Africa", x = "", y = "",
       caption = "Source: Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE) 31 May, 2020
  Graphic: Monique Bennett at @GoodGovernanceAfrica")

ggsave("ZAF.png", plot = last_plot(), dpi = 400, height = 8, width = 8)


df_days_since <- df_afr %>%
  filter(confirmed > 100) %>% 
  group_by(country) %>%
  mutate(days_since_100 = as.numeric(date - min(date))) %>%
  ungroup 


# Growth rate graph ZAF

breaks = c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 40000)

df_days_since %>% 
  filter(iso3c == "ZAF") %>% 
  ggplot() +
  geom_line(aes(days_since_100, confirmed), col = "#0072B2", size = 1) +
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks = breaks, labels = breaks) +
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
  Graphic: Monique Bennett at @GoodGovernanceAfrica")

ggsave("ZAF_2.png",  plot = last_plot(), dpi = 400, height = 8, width = 8)

