#Author/Date
#Gabriela Shirkey 4/12/2025
#Isotope comparison between zones (north/south), and treatment (CO2/CON)


#Library
library(dplyr)
library(plotrix) # for std.error
library(ggplot2)
library(multcompView)

#read in data
setwd("C:/Users/Gabriela Shirkey/OneDrive - Chapman University/CostaRica/Gabriela/RespirationPaper/litter_roots_nutrients/")
df <- read.csv("roots_leaves_isotopes.csv", stringsAsFactors = FALSE)
str(df)



# Boxplots by site_id or "zone" (i.e., north/south) -------------------------------------------------------


figc <- ggplot(df, aes(x = Site, y = Total.C, fill=Treatment), na.rm= TRUE) +
  geom_boxplot(position = "dodge", show.legend = T, width = 0.6) + 
  labs(y = expression("Total C" ~(mu*g)), x = "Site") + 
  #labs(y = expression(delta^13*C ~ "\U2030"), x = "Site") + 
  theme_bw() + 
  facet_wrap(~Type.of.Material+root_depth)+
  #geom_errorbar(aes(ymin = total_c_ug_mean - df_summary$total_c_ug_se, ymax = total_c_ug_mean + df_summary$total_c_ug_se), position = position_dodge(width = 0.6), linetype = 1,width=0.2) + 
  theme(legend.position="bottom") + 
  scale_fill_manual(values=c("grey", "darkgreen")) 
figc

figc <- ggplot(df, aes(x = Site, y = X13CVPDB , fill=Treatment), na.rm= TRUE) +
  geom_boxplot(position = "dodge", show.legend = T, width = 0.6) + 
  #labs(y = expression("Total C" ~(mu*g)), x = "Site") + 
  labs(y = expression(delta^13*C ~ "\U2030"), x = "Site") + 
  theme_bw() + 
  facet_wrap(~Type.of.Material+root_depth)+
  #geom_errorbar(aes(ymin = total_c_ug_mean - df_summary$total_c_ug_se, ymax = total_c_ug_mean + df_summary$total_c_ug_se), position = position_dodge(width = 0.6), linetype = 1,width=0.2) + 
  theme(legend.position="bottom") + 
  scale_fill_manual(values=c("grey", "darkgreen")) 
figc


fign <- ggplot(df, aes(x = Site, y = Total.N , fill=Treatment), na.rm= TRUE) +
  geom_boxplot(position = "dodge", show.legend = T, width = 0.6) + 
  labs(y = expression("Total N" ~(mu*g)), x = "Site") + 
  #labs(y = expression(delta^15*N ~ "\U2030"), x = "Site") + 
  theme_bw() + 
  facet_wrap(~Type.of.Material+root_depth)+
  #geom_errorbar(aes(ymin = total_c_ug_mean - df_summary$total_c_ug_se, ymax = total_c_ug_mean + df_summary$total_c_ug_se), position = position_dodge(width = 0.6), linetype = 1,width=0.2) + 
  theme(legend.position="bottom") + 
  scale_fill_manual(values=c("grey", "darkgreen")) 
fign

fign <- ggplot(df, aes(x = Site, y = X15NAir , fill=Treatment), na.rm= TRUE) +
  geom_boxplot(position = "dodge", show.legend = T, width = 0.6) + 
  #labs(y = expression("Total N" ~(mu*g)), x = "Site") + 
  labs(y = expression(delta^15*N ~ "\U2030"), x = "Site") +
  theme_bw() + 
  facet_wrap(~Type.of.Material+root_depth)+
  #geom_errorbar(aes(ymin = total_c_ug_mean - df_summary$total_c_ug_se, ymax = total_c_ug_mean + df_summary$total_c_ug_se), position = position_dodge(width = 0.6), linetype = 1,width=0.2) + 
  theme(legend.position="bottom") + 
  scale_fill_manual(values=c("grey", "darkgreen")) 
fign


# Save csv ----------------------------------------------------------------

df <- na.omit(df)

df$Site <- as.factor(df$Site)
df$Treatment <- as.factor(df$Treatment)
df$root_depth <- as.factor(df$root_depth) 

write.csv(df, "litter_root_df_cleaned.csv", row.names = FALSE)

# 3-way ANOVA N isotope-----------------------------------------------------------
#where zone is either north or south side

model_n <- aov(X15NAir~ Site*Treatment*root_depth, data=df)
summary(model_n)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# Site                        9  277.2  30.795  17.604  < 2e-16 ***
# Treatment                   1    2.3   2.275   1.300  0.25478    
# root_depth                  5   29.6   5.920   3.384  0.00518 ** 
# Site:Treatment              9  132.7  14.744   8.428 1.13e-11 ***
# Site:root_depth            19   46.7   2.457   1.405  0.11914    
# Treatment:root_depth        3    3.4   1.148   0.656  0.57942    
# Site:Treatment:root_depth  18   43.6   2.424   1.386  0.13321    
# Residuals                 452  790.7   1.749                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 3-way ANOVA Total N -----------------------------------------------------


model_n <- aov(Total.N~ Site*Treatment*root_depth, data=df)
summary(model_n)

# 3-way ANOVA C isotope-----------------------------------------------------------
#where zone is either north or south side

model_c <- aov(X13CVPDB ~ Site*Treatment*root_depth, data=df)
summary(model_c)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# Site                        9  486.4   54.04   9.582 2.01e-13 ***
# Treatment                   1   75.3   75.30  13.350 0.000289 ***
# root_depth                  5   34.0    6.81   1.207 0.304580    
# Site:Treatment              9  325.3   36.14   6.408 1.39e-08 ***
# Site:root_depth            19  127.6    6.72   1.191 0.260381    
# Treatment:root_depth        3   16.6    5.55   0.983 0.400316    
# Site:Treatment:root_depth  18  159.7    8.87   1.573 0.062797 .  
# Residuals                 452 2549.3    5.64                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
