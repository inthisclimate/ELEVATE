#Author/Date
#Gabriela Shirkey 4/3/2025
#Isotope comparison between zones (north/south), soil depth, and treatment (CO2/CON)


#Library
library(dplyr)
library(plotrix) # for std.error
library(ggplot2)
library(multcompView)

#read in data
df <- read.csv("isotopes.csv", stringsAsFactors = FALSE)
str(df)

#remove duplicates (assuming that is what soil depths ending with a D are)
df <- df%>%filter(soil_depth !="-D" & soil_depth !="D1" & soil_depth !="D2"& 
                    soil_depth !="il"& soil_depth !="90"& soil_depth !="40"& 
                    soil_depth !="35"& soil_depth !="20")
unique(df$soil_depth)


# Boxplots by site_id or "zone" (i.e., north/south) -------------------------------------------------------


figc <- ggplot(df, aes(x = site_id, y = total_c_ug, fill=treatment), na.rm= TRUE) +
  geom_boxplot(position = "dodge", show.legend = T, width = 0.6) + 
  labs(y = expression("Total C" ~(mu*g)), x = "Site") + 
  #labs(y = expression(delta^13*C ~ "\U2030"), x = "Site") + 
  theme_bw() + 
  facet_wrap(~soil_depth)+
  #geom_errorbar(aes(ymin = total_c_ug_mean - df_summary$total_c_ug_se, ymax = total_c_ug_mean + df_summary$total_c_ug_se), position = position_dodge(width = 0.6), linetype = 1,width=0.2) + 
  theme(legend.position="bottom") + 
  scale_fill_manual(values=c("grey", "darkgreen")) 
figc

figc <- ggplot(df, aes(x = zone, y = delta_c13 , fill=treatment), na.rm= TRUE) +
  geom_boxplot(position = "dodge", show.legend = T, width = 0.6) + 
  labs(y = expression("Total C" ~(mu*g)), x = "Site") + 
  labs(y = expression(delta^13*C ~ "\U2030"), x = "Site") + 
  theme_bw() + 
  facet_wrap(~soil_depth)+
  #geom_errorbar(aes(ymin = total_c_ug_mean - df_summary$total_c_ug_se, ymax = total_c_ug_mean + df_summary$total_c_ug_se), position = position_dodge(width = 0.6), linetype = 1,width=0.2) + 
  theme(legend.position="bottom") + 
  scale_fill_manual(values=c("grey", "darkgreen")) 
figc


fign <- ggplot(df, aes(x = site_id, y = total_n_ug , fill=treatment), na.rm= TRUE) +
  geom_boxplot(position = "dodge", show.legend = T, width = 0.6) + 
  labs(y = expression("Total N" ~(mu*g)), x = "Site") + 
  #labs(y = expression(delta^15*N ~ "\U2030"), x = "Site") + 
  theme_bw() + 
  facet_wrap(~soil_depth)+
  #geom_errorbar(aes(ymin = total_c_ug_mean - df_summary$total_c_ug_se, ymax = total_c_ug_mean + df_summary$total_c_ug_se), position = position_dodge(width = 0.6), linetype = 1,width=0.2) + 
  theme(legend.position="bottom") + 
  scale_fill_manual(values=c("grey", "darkgreen")) 
fign

fign <- ggplot(df, aes(x = zone, y = delta_n15 , fill=treatment), na.rm= TRUE) +
  geom_boxplot(position = "dodge", show.legend = T, width = 0.6) + 
  #labs(y = expression("Total N" ~(mu*g)), x = "Site") + 
  labs(y = expression(delta^15*N ~ "\U2030"), x = "Site") +
  theme_bw() + 
  facet_wrap(~soil_depth)+
  #geom_errorbar(aes(ymin = total_c_ug_mean - df_summary$total_c_ug_se, ymax = total_c_ug_mean + df_summary$total_c_ug_se), position = position_dodge(width = 0.6), linetype = 1,width=0.2) + 
  theme(legend.position="bottom") + 
  scale_fill_manual(values=c("grey", "darkgreen")) 
fign


# 3-way ANOVA N isotope-----------------------------------------------------------
#where zone is either north or south side

model_n <- aov(delta_n15~ zone*treatment*soil_depth, data=df)
summary(model_n)
# zone                        1  111.9  111.87  53.903 8.26e-13 ***
# treatment                   1   46.5   46.53  22.419 2.84e-06 ***
# soil_depth                  2  249.1  124.57  60.024  < 2e-16 ***
# zone:treatment              1    5.7    5.66   2.728   0.0992 .  
# zone:soil_depth             2    1.6    0.80   0.387   0.6793    
# treatment:soil_depth        2    2.1    1.03   0.495   0.6097    
# zone:treatment:soil_depth   2    0.4    0.18   0.088   0.9161    
# Residuals                 516 1070.9    2.08                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey_n <- TukeyHSD(model_n) 
plot(tukey_n)
# compact letter display
cld_n <- multcompLetters4(model_n, tukey_n)
cld_n


# 3-way ANOVA C isotope-----------------------------------------------------------
#where zone is either north or south side

model_c <- aov(delta_c13~ zone*treatment*soil_depth, data=df)
summary(model_c)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# zone                        1    377   377.1  13.501 0.000263 ***
# treatment                   1    251   250.8   8.977 0.002865 ** 
# soil_depth                  2    730   364.9  13.063 2.92e-06 ***
# zone:treatment              1   1144  1144.0  40.958 3.50e-10 ***
# zone:soil_depth             2     60    29.8   1.068 0.344445    
# treatment:soil_depth        2      3     1.5   0.055 0.946281    
# zone:treatment:soil_depth   2     31    15.5   0.556 0.573653    
# Residuals                 516  14413    27.9                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 


tukey_c <- TukeyHSD(model_c) 
print(tukey_c)
plot(tukey_c)
# compact letter display
cld_c <- multcompLetters4(model_c, tukey_c)
cld_c$`zone:treatment:soil_depth`


# Creating a table with posthoc compact letter display ----------------------------

#Carbon Isotopes
Tk_c<- df%>%group_by(zone, soil_depth, treatment)%>%summarise(mean=mean(delta_c13), quant=quantile(delta_c13, probs=0.75))%>%
  arrange(desc(mean))
head(Tk_c)

cld_c <- as.data.frame.list(cld_c$`zone:treatment:soil_depth`)
Tk_c$letters <- cld_c$Letters

#Nitrogen Isotopes
Tk_n<- df%>%group_by(zone, soil_depth, treatment)%>%summarise(mean=mean(delta_n15), quant=quantile(delta_n15, probs=0.75))%>%
  arrange(desc(mean))
head(Tk_n)

cld_n <- as.data.frame.list(cld_n$`zone:treatment:soil_depth`)
Tk_n$letters <- cld_n$Letters


# Adding the letters to the boxplot figures -------------------------------

#C differences between treatments*zone*soildepth
figc <- ggplot(df, aes(x = zone, y = delta_c13 , fill=treatment), na.rm= TRUE) +
  geom_boxplot(position = "dodge", show.legend = T, width = 0.6) +
  labs(y = expression(delta^13*C ~ "\U2030"), x = "Site") + 
  theme_bw() + 
  facet_wrap(~soil_depth)+
  geom_text(data = Tk_c, aes(x = zone, y = quant+1.5, label = letters),position = position_dodge(0.55))+
  theme(legend.position="bottom") + 
  scale_fill_manual(values=c("grey", "darkgreen")) 
figc
ggsave("C_delta_isotopes_tukey.png",width = 8, height = 6, figc)


#N differences between treatments*zone*soildepth
fign <- ggplot(df, aes(x = zone, y = delta_n15 , fill=treatment), na.rm= TRUE) +
  geom_boxplot(position = "dodge", show.legend = T, width = 0.6) + 
  labs(y = expression(delta^15*N ~ "\U2030"), x = "Site") +
  theme_bw() + 
  facet_wrap(~soil_depth)+
  geom_text(data = Tk_n, aes(x = zone, y = quant+0.5, label = letters),position = position_dodge(0.7))+
  theme(legend.position="bottom") + 
  scale_fill_manual(values=c("grey", "darkgreen")) 
fign
ggsave("N_delta_isotopes_tukey.png",width = 8, height = 6, fign)
