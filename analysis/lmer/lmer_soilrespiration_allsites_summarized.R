

#LME Mixed effects model

#Run this model on the aggregated dataset, with multiple soil respiration samples per site-treatment
#lmer(soil_respiration ~ ambient_CO2 + (1 | site), data = df)

library(lmerTest)
library(dplyr)

# Read in disaggregated data for the lmer --------------------------------
getwd()
#setwd("C:/Users/shirkey/OneDrive - Chapman University/CostaRica/Gabriela/RespirationPaper/analysis/lmer")
df <- read.csv("../../combine_dataset/CRdataset.unaggregated.aranet.soilr.csv", stringsAsFactors = FALSE)

#pick just the 24 hour
df <- df%>%filter(time_of_day=="24hr")

#check it out, see how many samples per site we have
head(df)
df <-df%>%group_by(Site, Treatment, 
                   aranet_co2_mean,  aranet_co2_sd ,   aranet_co2_median,
                   aranet_t_mean ,    aranet_t_sd    ,   aranet_t_median  , aranet_hpa_mean ,  aranet_hpa_sd,
                   aranet_hpa_median,   aranet_rh_mean,    aranet_rh_sd ,     aranet_rh_median
                   )%>%dplyr::summarize(soilco2_umolm2s_mean = mean(soilco2_umolm2s), 
                                                      soilco2_umolm2s_median = median(soilco2_umolm2s), 
                                                      soilco2_umolm2s_sd = sd(soilco2_umolm2s), 
                                                      soilch4_umolm2s_mean = mean(soilch4_umolm2s), 
                                                      soilch4_umolm2s_sd = sd(soilch4_umolm2s), 
                                                      soilch4_umolm2s_median = median(soilch4_umolm2s)) 
nrow(df)
df%>%group_by(Site, Treatment)%>%tally()



# modeling ----------------------------------------------------------------
#from the corrplot the top variables that had the strongest relationships were: 
#positive: aranet_t_sd = 0.21
#negative: aranet_co2_median (-0.25), aranet_co2_mean (-0.21), soilch4_umolm2s (-0.34)
#LMER
model <- lmer(soilco2_umolm2s_mean ~ aranet_t_sd * aranet_co2_mean + soilch4_umolm2s_mean + (1 | Site), data = df)
summary(model)
#A |t value| > ~2 generally suggests p < 0.05, which is typically considered statistically significant.

vif(model)



#LM -- can we really treat each sample as an independent, not grouped by site?
model <- lm(soilco2_umolm2s_mean ~ aranet_t_sd + aranet_co2_mean + soilch4_umolm2s_mean, data = df)
summary(model)
# lm(formula = soilco2_umolm2s_mean ~ aranet_t_sd * aranet_co2_mean + 
#      soilch4_umolm2s_mean, data = df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.2329 -2.4132 -0.3115  3.1590  6.4606 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 193.04549  114.37598   1.688   0.1039    
# aranet_t_sd                  -2.52973   58.29928  -0.043   0.9657    
# aranet_co2_mean              -0.44002    0.25126  -1.751   0.0922 .  
# soilch4_umolm2s_mean        -13.47278    2.09104  -6.443 9.54e-07 ***
#   aranet_t_sd:aranet_co2_mean   0.01633    0.12708   0.129   0.8988    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.257 on 25 degrees of freedom
# Multiple R-squared:  0.7917,	Adjusted R-squared:  0.7584 
# F-statistic: 23.76 on 4 and 25 DF,  p-value: 3.315e-08


# Add nutrients here ------------------------------------------------------

str(df)
#read nutrients in, you'll see there are multiple samples for roots and litter as well as soil, so the number of rows is very long
nutrients <- read.csv("C:/Users/Gabriela Shirkey/OneDrive - Chapman University/CostaRica/Gabriela/RespirationPaper/combine_dataset/CR_aranet.nutrients_2025.csv")
str(nutrients)
nutrients$depth <- as.factor(nutrients$depth)

#we need to aggregate this by site-treatment, so all the samples taken at each spot is summarized for 1 value
nutrients <- nutrients%>%group_by(Site, Treatment, site_treatment, depth)%>%summarize(
  total_c_ug.soil.mean = mean(total_c_ug.soil, na.rm = TRUE),
  total_c_ug.soil.median = median(total_c_ug.soil, na.rm = TRUE),
  total_c_ug.soil.sd = sd(total_c_ug.soil, na.rm = TRUE), 
  
  total_n_ug.soil.mean = mean(total_n_ug.soil, na.rm = TRUE),
  total_n_ug.soil.median = median(total_n_ug.soil, na.rm = TRUE),
  total_n_ug.soil.sd = sd(total_n_ug.soil, na.rm = TRUE),
  
  total_c_ug.roots.litter.mean = mean(total_c_ug.roots.litter, na.rm = TRUE),
  total_c_ug.roots.litter.median = median(total_c_ug.roots.litter, na.rm = TRUE),
  total_c_ug.roots.litter.sd = sd(total_c_ug.roots.litter, na.rm = TRUE),
  
  total_n_ug.roots.litter.mean = mean(total_n_ug.roots.litter, na.rm = TRUE),
  total_n_ug.roots.litter.median = median(total_n_ug.roots.litter, na.rm = TRUE),
  total_n_ug.roots.litter.sd = sd(total_n_ug.roots.litter, na.rm = TRUE)
  )
head(nutrients)
nutrients$Treatment[nutrients$Treatment=="CON"] <- "Control"
nutrients$Treatment[nutrients$Treatment=="CO2"] <- "Elevated"

#Because we want each column to represent a different depth, we need to pivot wider
nutrients_wide <- nutrients %>%
  select(Site, Treatment, site_treatment, depth, total_c_ug.soil.mean, total_n_ug.soil.mean, total_c_ug.roots.litter.mean, total_n_ug.roots.litter.mean) %>%
  pivot_wider(
    names_from = depth,
    values_from = c(total_c_ug.soil.mean, total_n_ug.soil.mean, total_c_ug.roots.litter.mean, total_n_ug.roots.litter.mean),
    names_sep = "_"
  )
head(nutrients_wide)

#remove soil columns labeled as litter
nutrients_wide <- nutrients_wide%>%select(-c("total_n_ug.soil.mean_litter", "total_c_ug.soil.mean_litter"))
names(nutrients_wide)

#add to df
df <- df%>%select(-c())
df <- merge(df, nutrients_wide, by=c("Site", "Treatment"))
head(df)
nrow(df)
names(df)


# Test nutrients and climate on soil respiration --------------------------

df%>%group_by(Site, Treatment)%>%tally()

model <- lm(soilco2_umolm2s_mean ~ aranet_t_sd + aranet_co2_mean + 
              total_n_ug.soil.mean_15 +  
              total_c_ug.soil.mean_15+
              total_c_ug.roots.litter.mean_15 + 
              total_n_ug.roots.litter.mean_15+ 
              total_c_ug.roots.litter.mean_litter+ 
              total_n_ug.roots.litter.mean_litter, data = df)
summary(model)

unique(df$Site)

#make a stepwise model
vars_to_use <- c(
  "aranet_t_sd" , 
  "aranet_t_mean", 
    "aranet_co2_mean" , 
    "total_n_ug.soil.mean_15" ,  
    "total_c_ug.soil.mean_15",
    "total_c_ug.roots.litter.mean_15" , 
    "total_n_ug.roots.litter.mean_15", 
    "total_c_ug.roots.litter.mean_litter", 
    "total_n_ug.roots.litter.mean_litter"
)

# Select just the needed columns + response variable
model_data <- df %>%
  select(soilco2_umolm2s_mean, site_treatment, all_of(vars_to_use)) %>%
  drop_na()  # drop rows with any missing values
# Full model
full_model <- lm(soilco2_umolm2s_mean ~ ., data = model_data)

# Stepwise model: both directions
step_model <- step(full_model, direction = "both", trace = TRUE)
summary(step_model)
# 
# lm(formula = soilco2_umolm2s_mean ~ aranet_t_mean + aranet_co2_mean + 
#      total_n_ug.soil.mean_15 + total_c_ug.roots.litter.mean_15 + 
#      total_n_ug.roots.litter.mean_15 + total_n_ug.roots.litter.mean_litter, 
#    data = model_data)

# Residual standard error: 4.845 on 9 degrees of freedom
# Multiple R-squared:  0.7122,	Adjusted R-squared:  0.5203 
# F-statistic: 3.712 on 6 and 9 DF,  p-value: 0.03858

# Create a linear model fig---------------------------------------------------

library(ggplot2)

# Add predictions and residuals to your data
model_data$predicted <- predict(step_model)
model_data$residuals <- resid(step_model)

ggplot(model_data, aes(x = predicted, y = soilco2_umolm2s_mean, label = site_treatment)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue", fill = "lightblue") +  # regression line + CI
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  geom_text(vjust = -0.5, size = 3) +  # offset text a little above points
  labs(title = "Soil Respiration: Observed vs Predicted",
       x = expression("Predicted soil " * CO[2] * " (μmol/m²/s)"),
       y = expression("Observed soil " * CO[2] * " (μmol/m²/s)")
  ) +
  coord_cartesian(xlim = c(0, max(model_data$predicted, model_data$soilco2_umolm2s_mean)),
                  ylim = c(0, max(model_data$predicted, model_data$soilco2_umolm2s_mean))) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))
ggsave("SoilR.predicted.fig.png", units = "in", height=5, width =7)
