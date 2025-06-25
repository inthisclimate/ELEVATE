#Combine and plot the efflux across sites that were processed
#Export a single dataframe to connect with the larger dataset

#Authors: Gabriela Shirkey, Coco (Christine) O'Connell

#Library
library(dplyr)
library(ggplot2)
getwd() #Should be the ELEVATE project


# Import and merge dataset ------------------------------------------------

#Import and update data. Efflux was taken at 3-5 trees in S3, S4, and S5 each
s3 <- read.csv("Efflux/efflux_cleaned/S3.elevated.control.fluxes.csv", stringsAsFactors = FALSE)
s3$Species <- "Zanmon"

S4.con <- read.csv("Efflux/efflux_cleaned/S4.control.fluxes.csv", stringsAsFactors = FALSE)
S4.con$Species <- "Vacgua"
S4.el <- read.csv("Efflux/efflux_cleaned/S4.elevated.fluxes.csv", stringsAsFactors = FALSE)
  S4.el$REMARK_ID <- S4.el$efflux_tree
  S4.el<-S4.el%>%select(-efflux_tree)
  S4.el$Species <- "Vacgua"

s5 <- read.csv("Efflux/efflux_cleaned/S5.elevated.control.fluxes.csv", stringsAsFactors = FALSE)
s5$Species <- "Brycra"

#merge into 1 and keep just the good stuff
df <- rbind(s3, S4.con, S4.el, s5)
df.co2 <- df%>%filter(is_outlier_co2=="FALSE")%>%select(c("Site",
                                                          "Treatment",
                                                          "CO2_flux.umol.m2.s", 
                                                          "Species"))
df.ch4 <- df%>%filter(is_outlier_ch4=="FALSE")%>%select(c("Site",
                                                          "Treatment",
                                                          "CH4_flux.umol.m2.s", 
                                                          "Species"))

# Create two dataframes of summary stats ---------------------

# get mean and sd by group for site and treatment
efflux_summary_site_species.co2 <- df.co2 %>%
  group_by(Site, Species) %>%
  summarise(mean_co2flux = mean(CO2_flux.umol.m2.s, na.rm = TRUE),
            median_co2flux = median(CO2_flux.umol.m2.s, na.rm = TRUE),
            sd_co2flux = sd(CO2_flux.umol.m2.s, na.rm = TRUE)) 
head(efflux_summary_site_species.co2)

efflux_summary_site_species.ch4 <- df.ch4 %>%
  group_by(Site, Species) %>%
  summarise(mean_ch4_flux = mean(CH4_flux.umol.m2.s, na.rm = TRUE),
            median_ch4flux = median(CH4_flux.umol.m2.s, na.rm = TRUE),
            sd_ch4flux = sd(CH4_flux.umol.m2.s, na.rm = TRUE)) 
head(efflux_summary_site_species.ch4)

efflux_summary_site_species <- merge(efflux_summary_site_species.ch4, 
                                     efflux_summary_site_species.co2, 
                                     by=c("Site", "Species"))




# get mean and sd by group for species only, ignoring site
efflux_summary_species.co2 <- df.co2 %>%
  group_by(Species) %>%
  summarise(mean_co2flux = mean(CO2_flux.umol.m2.s, na.rm = TRUE),
            median_co2flux = median(CO2_flux.umol.m2.s, na.rm = TRUE),
            sd_co2flux = sd(CO2_flux.umol.m2.s, na.rm = TRUE)) 
head(efflux_summary_species.co2)

efflux_summary_species.ch4 <- df.ch4 %>%
  group_by(Species) %>%
  summarise(mean_ch4_flux = mean(CH4_flux.umol.m2.s, na.rm = TRUE),
            median_ch4flux = median(CH4_flux.umol.m2.s, na.rm = TRUE),
            sd_ch4flux = sd(CH4_flux.umol.m2.s, na.rm = TRUE)) 
head(efflux_summary_species.ch4)

efflux_summary_species <- merge(efflux_summary_species.co2, 
                                efflux_summary_species.ch4, 
                                     by="Species")



# Stats to test for significance between species and CO2 efflux--------------------------

# Kruskal-Wallis CO2flux difference between Species 
# (Does not require normality or equal variance)
kruskal.test(CO2_flux.umol.m2.s ~ Species, data = df.co2)
  # data:  CO2_flux.umol.m2.s by Species
  # Kruskal-Wallis chi-squared = 350.44, df = 2, p-value < 2.2e-16
  #There is a statistically significant difference in CO₂ flux medians among the three species.

#Which species are statistically different?
pairwise.wilcox.test(df$CO2_flux.umol.m2.s, df$Species, p.adjust.method = "bonferroni")
    #This is the non-parametric equivalent of Tukey’s HSD, using Wilcoxon rank-sum tests for each pair of species,
    #with Bonferroni correction for multiple testing.
    #Vacgua is statistically different than Brycra and Zanmon (p<2e-16 each).
    #Brycra and Zanmon are not statistically different, however. 
    # data:  df$CO2_flux.umol.m2.s and df$Species 
    # 
    #         Brycra Vacgua
    # Vacgua <2e-16 -     
    #   Zanmon 1      <2e-16

# Stats to test for significance between species and Ch4 efflux--------------------------

# Kruskal-Wallis CO2flux difference between Species 
# (Does not require normality or equal variance)
kruskal.test(CH4_flux.umol.m2.s ~ Species, data = df.ch4)
# data:  CO2_flux.umol.m2.s by Species
# Kruskal-Wallis chi-squared = 350.44, df = 2, p-value < 2.2e-16
#There is a statistically significant difference in CO₂ flux medians among the three species.

#Which species are statistically different?
pairwise.wilcox.test(df$CH4_flux.umol.m2.s, df$Species, p.adjust.method = "bonferroni")
#This is the non-parametric equivalent of Tukey’s HSD, using Wilcoxon rank-sum tests for each pair of species,
#with Bonferroni correction for multiple testing.
#Vacgua is statistically different than Brycra (p=1e-05) and Zanmon (p=0.027).
#Brycra and Zanmon are not statistically different, however. 
  # data:  df$CH4_flux.umol.m2.s and df$Species 
  # 
  #         Brycra Vacgua
  # Vacgua 1e-05  -     
  # Zanmon 0.216  0.027 
  # 
  # P value adjustment method: bonferroni 




# Boxplots of efflux ------------------------------------------------------


#Plot boxplots of each site-species' co2 flux
ggplot(df.co2, 
       aes(x = Species, y = CO2_flux.umol.m2.s, fill = Species)) +
  
  # Boxplots: grouped by Species × Treatment
  geom_boxplot(aes(group = interaction(Species, Treatment)),
               outlier.shape = 1,
               position = position_dodge(width = 0.75)) +
  
  # White mean dots: also grouped by Species × Treatment
  stat_summary(aes(group = interaction(Species, Treatment)),
               fun = mean, geom = "point", 
               shape = 21, size = 2.5, fill = "white", color = "black",
               position = position_dodge(width = 0.75)) +
  
  labs(y = expression("CO"[2]*" flux (µmol m"^{-2}*" s"^{-1}*")"), 
       x = "Species") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Dark2")



  #annotate("text",x=1,y=105,label="Treatment sig diff (2-way \n ANOVA on log-transformed \n  response var): \n Treatment p-val < 0.01 \n Site and interaction p-val < 0.1 \n n = 4 per location",size = 3)
#ggsave("Efflux/efflux_figures/efflux_species-treatment_boxplot.png",width = 8, height = 6)


# Save data as 1 dataframe ------------------------------------------------

write.csv(df, "Efflux/efflux_cleaned/efflux_fulldataset.csv", row.names = FALSE)
write.csv(stem_efflux_mean_sd_site_treatment, "Efflux/efflux_cleaned/efflux_by_site.treatment.csv", row.names = FALSE)

