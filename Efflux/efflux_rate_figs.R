
#Create figures of the rate of change in efflux for CR2025 samples

#Authors: Gabriela Shirkey

#Library
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(patchwork)


getwd() #Should be the ELEVATE project

#Read in dataset
efflux <- read.csv("Efflux/efflux_cleaned/efflux_fulldataset.csv", stringsAsFactors = FALSE)
head(efflux)
str(efflux)

#We're really just interested in the following variables from this dataset
efflux <- efflux%>%select(c("Site", "Treatment", "Species", "REMARK", 
                            "CO2_flux.umol.m2.s", "CH4_flux.umol.m2.s", 
                            "time_CST", "time_elapsed", "CO2", "CH4"))
str(efflux)

#Change time_CST to a POSIXct
efflux$time_CST <- ymd_hms(efflux$time_CST, tz = "America/Costa_Rica")
str(efflux$time_CST)

#crop time elapsed to 20 minutes and rename species
efflux <- efflux %>%
  filter(time_elapsed <= 20) %>%
  mutate(Species = toupper(Species))

#rename species


# Basic plots -------------------------------------------------------------
names(efflux)

rateco2 <- ggplot(aes(y=CO2, x=time_elapsed), data=efflux)+
  geom_point(size=0.3, shape=1, color="darkgrey")+
  geom_smooth(method = lm, color = "black")+
  facet_wrap(~Species)+
  labs(x= "Time (minutes)",
       y = expression("CO"[2]*" (ppm)"))+
  scale_y_continuous(expand =c(0,0), limits=c(0, 3300), 
                     breaks=seq(from = 0, to = 3300, by = 500))+
  theme_bw()
 



ratech4 <- ggplot(aes(y=CH4, x=time_elapsed), data=efflux)+
  geom_point(size=0.3, shape=1, color="darkgrey")+
  geom_smooth(method = lm, color = "black")+
  facet_wrap(~Species)+
  labs(x= "Time (minutes)",
       y = expression("CH"[4]*" (ppb)"))+
  # scale_y_continuous(expand =c(0,0), limits=c(1500, 2100), 
  #                    breaks=seq(from = 1500, to = 2100, by = 500))+
  theme_bw()
ratech4

#combine them together
combined_plot <- (ratech4 / rateco2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Display the combined plot
print(combined_plot)
ggsave("Efflux/efflux_figures/efflux_rates.jpg", combined_plot, dpi = 300, units = "in", width = 7, height = 5)


# Get rise over run rates -------------------------------------------------
library(purrr)
library(broom)
library(tidyr)

# Fit linear models of CO2 ~ time_elapsed for each chamber measurement
co2_flux_estimates <- efflux %>%
  filter(!is.na(CO2)) %>%
  group_by(Species) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(CO2 ~ time_elapsed, data = .x)),
         results = map(model, tidy)) %>%
  unnest(results) %>%
  filter(term == "time_elapsed") %>%
  rename(co2_slope_ppm_per_min = estimate) %>%
  select(Species, co2_slope_ppm_per_min)
co2_flux_estimates


ch4_flux_estimates <- efflux %>%
  filter(!is.na(CH4)) %>%
  group_by(Species) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(CH4 ~ time_elapsed, data = .x)),
         results = map(model, tidy)) %>%
  unnest(results) %>%
  filter(term == "time_elapsed") %>%
  rename(ch4_slope_ppm_per_min = estimate) %>%
  select(Species, ch4_slope_ppm_per_min)
ch4_flux_estimates


# Same models, more summary lm() ------------------------------------------

brycra <- efflux%>%filter(Species=="BRYCRA")
summary(lm(CO2 ~ time_elapsed, data = brycra))
summary(lm(CH4 ~ time_elapsed, data = brycra))
