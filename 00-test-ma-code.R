
# Preliminary -------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, hrbrthemes,
               scales, gganimate, gapminder, gifski, png, tufte, plotly, OECD,
               ggrepel, here, xaringanExtra, webshot)



# Import data -------------------------------------------------------------
full.ma.data <- readRDS("data/output/full_ma_data.rds")
service.area <- readRDS("data/output/contract_service_area.rds")



# Figures -----------------------------------------------------------------
full.ma.data %>% group_by(fips, year) %>% select(fips, year) %>% summarize(plan_count=n()) %>%
  ggplot(aes(x=as.factor(year),y=plan_count)) + 
  stat_summary(fun.y="mean", geom="bar") +
  labs(
    x="Year",
    y="Number of Plans",
    title="Average Number of Plans per County"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()

full.ma.data %>% 
  group_by(fips, year) %>% 
  select(fips, year) %>% 
  summarize(plan_count=n()) %>%
  ggplot(aes(x=as.factor(year),y=plan_count)) + 
  stat_summary(fun.y="mean", geom="bar") +
  labs(x="Year", y="Number of Plans", title="Average Number of Plans per County") +
  scale_y_continuous(labels=comma) +
  theme_bw()

full.ma.data %>% 
  filter(snp=="No" & eghp=="No") %>%   #<<
  group_by(fips, year) %>% 
  select(fips, year) %>% 
  summarize(plan_count=n()) %>%
  ggplot(aes(x=as.factor(year),y=plan_count)) + 
  stat_summary(fun.y="mean", geom="bar") +
  labs(x="Year", y="Number of Plans", title="Average Number of Plans per County") +
  scale_y_continuous(labels=comma) +
  theme_bw()
  
full.ma.data %>% 
  filter(snp=="No" & eghp=="No") %>%
  filter(planid < 800 | planid >= 900) %>% #<<
  filter(!is.na(planid)) %>% #<<
  group_by(fips, year) %>% 
  select(fips, year) %>% 
  summarize(plan_count=n()) %>%
  ggplot(aes(x=as.factor(year),y=plan_count)) + 
  stat_summary(fun.y="mean", geom="bar") +
  labs(x="Year", y="Number of Plans", title="Average Number of Plans per County") +
  scale_y_continuous(labels=comma) +
  theme_bw()

full.ma.data %>% 
  filter(snp=="No" & eghp=="No") %>%
  filter(planid < 800 | planid >= 900) %>%
  filter(!is.na(planid)) %>%
  inner_join(service.area %>% #<<
             select(contractid, fips, year), #<<
             by=c("contractid", "fips", "year")) %>% #<<
  group_by(fips, year) %>% 
  select(fips, year) %>% 
  summarize(plan_count=n()) %>%
  ggplot(aes(x=as.factor(year),y=plan_count)) + 
  stat_summary(fun.y="mean", geom="bar") +
  labs(x="Year", y="Number of Plans", title="Average Number of Plans per County") +
  scale_y_continuous(labels=comma) +
  theme_bw()

## Count of plans by type (across years)
ma.plot <- full.ma.data %>% group_by(fips, year, plan_type) %>% 
  select(fips, year, plan_type) %>% summarize(plan_count=n())

ma.plot.means <- ma.plot %>% group_by(plan_type, year) %>%
  summarize(mean_count=mean(plan_count)) %>% filter(mean_count>6) %>%
  ungroup()

ma.plot.means <- ma.plot.means %>%
  mutate(plan_type = as.factor(plan_type)) %>%
  mutate(plan_type = fct_recode(plan_type,
                                "PDP Only" = "Medicare Prescription Drug Plan",
                                "Employer PDP" = "Employer/Union Only Direct Contract PDP"))

plot.int <- plot_ly(ma.plot.means,
                    y=~as.factor(plan_type), 
                    x=~mean_count, 
                    frame=~as.factor(year), 
                    type='bar',
                    width=700,
                    height=400) %>%
  animation_slider(
    currentvalue = list(prefix = "Year ", font = list(color="blue"))
  ) %>%
  layout(xaxis = list(title = "Count per County"), 
         yaxis = list(title = ""))

htmltools::save_html(plot.int, file="plan-plot.html")
