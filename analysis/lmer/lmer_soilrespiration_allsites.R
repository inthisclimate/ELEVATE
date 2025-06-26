

#LME Mixed effects model

#Run this model on disaggregated dataset, with multiple soil respiration samples per site-treatment
#lmer(soil_respiration ~ ambient_CO2 + (1 | site), data = df)

library(lmerTest)

# Read in disaggregated data for the lmer --------------------------------
getwd()
#setwd("C:/Users/shirkey/OneDrive - Chapman University/CostaRica/Gabriela/RespirationPaper/analysis/lmer")
df <- read.csv("../../combine_dataset/CRdataset.unaggregated.aranet.soilr.csv", stringsAsFactors = FALSE)

#check it out, see how many samples per site we have
head(df)
nrow(df)
df%>%group_by(Site, Treatment)%>%tally()
# Site  Treatment     n
# <chr> <chr>     <int>
#   1 N3    Control       5
# 2 N3    Elevated      5
# 3 N4    Control       5
# 4 N4    Elevated      5
# 5 N7    Control       5
# 6 N7    Elevated      5
# 7 S1    Control      15
# 8 S1    Elevated     15
# 9 S3    Control      15
# 10 S3    Elevated     15
# 11 S4    Control      14
# 12 S4    Elevated     15
# 13 S5    Control      15
# 14 S5    Elevated     15
# 15 S6    Control      15
# 16 S6    Elevated     15

head(df) 
df$site.treatment <- paste0(df$Site, "-", df$Treatment)

# modeling ----------------------------------------------------------------
#from the corrplot the top variables that had the strongest relationships were: 
#positive: aranet_t_sd = 0.21
#negative: aranet_co2_median (-0.25), aranet_co2_mean (-0.21), soilch4_umolm2s (-0.34)
#LMER
model <- lmer(soilco2_umolm2s ~ aranet_t_sd * aranet_co2_mean + soilch4_umolm2s + (1 | Site), data = df)
summary(model)
#A |t value| > ~2 generally suggests p < 0.05, which is typically considered statistically significant.

vif(model)



#LM -- can we really treat each sample as an independent, not grouped by site?
model <- lm(soilco2_umolm2s ~ aranet_t_sd * aranet_co2_mean + soilch4_umolm2s, data = df)
summary(model)
# (Intercept)       36.17176   25.07610   1.442    0.151    
# aranet_t_sd        3.35824    2.15500   1.558    0.121    
# aranet_co2_median -0.07512    0.04831  -1.555    0.122    
# soilch4_umolm2s   -3.54132    0.78872  -4.490 1.29e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 11.27 on 174 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.1855,	Adjusted R-squared:  0.1715 
# F-statistic: 13.21 on 3 and 174 DF,  p-value: 8.243e-08

