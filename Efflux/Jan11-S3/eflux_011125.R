#Plotting and cleaning data
#efflux S3 January 11, 2025
#saving cleaned data to "efflux_cleaned".

#Note on converting CO2 from Licor to umol/m2/s
#CO2 from licor is measured in ppm; CH4 is in ppb
#Use the molar ratio to conver from ppb and ppm using the ideal gas law, where at ideal temperature and pressure the mole of the gas can occupy the same volume.
#CO2 is 1 ppm = umol/mol = 1 * 10^-6 mol/mol (this is important when calculating fluxes below)
#CH4 from licor is measured in ppb, volume per volume like ppm, because atmospheric concentrations of CH4 are less than that of CO2 and it's easier to discuss it in whole numbers than tiny numbers
# 1ppb = 1 nmol/mol = 1*10^-9 mol/mol (i.e., 1750 ppb=1750×10^−9=1.75×10−6 mol/mol)

#library
library(dplyr)
library(ggplot2)
library(lubridate)

##test script for eflux
getwd()
df <-read.csv("Efflux/Jan11-S3/TG10-01977-2025-01-11T061100.data.csv")

head(df)
names(df)
str(df$DATE)
str(df$TIME)

# Convert to POSIXct
# df$DATE <- as.POSIXct(df$DATE, format = "%Y-%m-%d", tz = "UTC")
# df$TIME <- as.POSIXct(df$TIME, format = "%H:%M:%S")

# Combine the DATE and TIME columns into one combined datetime column
df$DATETIME <- as.POSIXct(paste(df$DATE, df$TIME), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
str(df$DATETIME)
df$time_CST <- ymd_hms(df$DATETIME, tz = "America/Costa_Rica") #add in CR time too

#Remove values <400 ppm, as they're likely from moving equipment
df <- df%>%filter(CO2>400)

#what unique remarks are there to evaluate? 

unique(df$REMARK)
# [1] ""                "S3-co2-z7"       "S3-co2-z8"       "S3-co2-z1"       "S3-co2-efflux-z"
# [6] "S2-co2-efflux-z" "S3-co2-ef-z7-r2" "S3-con-ef-z2"    "S3-con-ef-z-us1" "S3-con-ef-z-us2"
# [11] "S3-con-ef-z-us3" "S3-conefz-us2re"

#IGNORE "S2.co2.efflux.z" & "S3-co2-z7" & "S3-con-ef-z-us2"
#CORRECTION "S3.co2.efflux.z" is Z5


# S3 CO2 ------------------------------------------------------------------

S3.co2.efflux.z <- df %>%
  filter(
    REMARK=="S3-co2-efflux-z" 
  )%>%mutate(REMARK_ID = "ZANMON 5", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), units = "mins")))

S3.co2.z1 <- df %>%
  filter(
    REMARK=="S3-co2-z1" & 
    TIME!="09:56:02" & TIME!="09:56:03" & TIME!="09:56:04" &
    DATETIME <= as.POSIXct("2025-01-11 10:11:01", tz = "UTC"))%>%
  mutate(REMARK_ID = "ZANMON 1", 
         time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), units = "mins")))
  

S3.co2.z8 <- df %>%
  filter(
    REMARK=="S3-co2-z8"
  )%>%mutate(REMARK_ID = "ZANMON 8", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), units = "mins")))

S3.co2.z7 <- df %>%
  filter(
    REMARK=="S3-co2-ef-z7-r2"
  )%>%mutate(REMARK_ID = "ZANMON 7", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), units = "mins")))

S3.CO2 <- rbind(S3.co2.efflux.z, S3.co2.z1, S3.co2.z8, S3.co2.z7)
head(S3.CO2)

# Rates?
S3.CO2$RATE_CO2 <- c(NA, 
                    diff(S3.CO2$CO2) / diff(S3.CO2$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S3.CO2$RATE_CO2, na.rm=TRUE) #avg 0.24ppm/sec
S3.CO2$RATE_CH4 <- c(NA, 
                     diff(S3.CO2$CH4) / diff(S3.CO2$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S3.CO2$RATE_CH4, na.rm=TRUE) #avg 0.01ppb/sec

S3.CO2$Site <- "S3"
S3.CO2$Treatment <- "Elevated"

#ggplot

#all samples in S3CO2
ggplot(aes(time_elapsed, CO2, color=REMARK_ID), data= S3.CO2)+ #z5
  #geom_point()+
  geom_line()+
  labs(title="S3 Elevated | Zanmon", 
       x="Time (min)", 
       y = expression(CO[2] ~ "ppm"),
       color = "Tree")+
  theme_bw()

#all samples in S3CO2
ggplot(aes(time_elapsed, CH4, color=REMARK_ID), data= S3.CO2)+ #z5
  #geom_point()+
  geom_line()+
  labs(title="S3 Elevated | Zanmon", 
       x="Time (min)", 
       y = "CH4 ppb",
       color = "Tree")+
  theme_bw()

#select samples in S3CO2

ggplot(aes(DATETIME, CO2), data= S3.co2.efflux.z)+ #z5
  #geom_point()+
  geom_line()+
  labs(title="S3-CO2-z5 | Zanmon", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))

ggplot(aes(DATETIME, CH4), data= S3.co2.efflux.z)+
  #geom_point()+
  geom_line()+
  labs(title="S3.co2.efflux.z | Zanmon", 
       x="Time ETC/Universal", 
       y = "CH4 ppb")+
  theme_minimal()
range(S3.co2.efflux.z$DATETIME)


ggplot(aes(TIME, CO2), data= S3.co2.z8)+
  #geom_point()+
  geom_line()+
  labs(title="S3-CO2-z8 | Zanmon", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))

ggplot(aes(TIME, CO2), data= S3.co2.z7)+
  #geom_point()+
  geom_line()+
  labs(title="S3-CO2-z7 | Zanmon", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))

ggplot(aes(TIME, CO2), data= S3.co2.z1)+
  #geom_point()+
  geom_line()+
  labs(title="S3-CO2-z1 | Zanmon", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))




#What is the difference in CO2ppm between start/end? (magnitude)
# (first <- z1[1, ])
# (last <- z1[nrow(z1), ])
# z1.mag <- last$CO2 - first$CO2
# z1.mag



# S3 CON ------------------------------------------------------------------

S3.con.ef.z2 <- df %>%
  filter(
    REMARK=="S3-con-ef-z2"
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%
  mutate(REMARK_ID = "ZANMON 2", 
         time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), units = "mins")))

model <- lm(CO2 ~ time_CST, data = S3.con.ef.z2)
summary(model)

S3.con.ef.z.us1 <- df %>%
  filter(
    REMARK=="S3-con-ef-z-us1"
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%
  mutate(REMARK_ID = "ZANMON US1", 
         time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), units = "mins")))

S3.con.ef.z.us2 <- df %>%
  filter(
    REMARK=="S3-conefz-us2re"
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%
  mutate(REMARK_ID = "ZANMON US2redo", 
         time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), units = "mins")))

S3.con.ef.z.us3 <- df %>%
  filter(
    REMARK=="S3-con-ef-z-us3" 
  )%>%
  mutate(REMARK_ID = "ZANMON US3", 
         time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), units = "mins")))

#combine all S3 CON
S3.CON <- rbind(S3.con.ef.z2,S3.con.ef.z.us1, S3.con.ef.z.us2,S3.con.ef.z.us3)
S3.CON$Site <- "S3"
S3.CON$Treatment <-"Control"

# Rates?
S3.CON$RATE_CO2 <- c(NA, 
                     diff(S3.CON$CO2) / diff(S3.CON$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S3.CO2$RATE_CO2, na.rm=TRUE) #avg 0.24ppm/sec

S3.CON$RATE_CH4 <- c(NA, 
                     diff(S3.CON$CH4) / diff(S3.CON$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S3.CON$RATE_CH4, na.rm=TRUE) #avg 0.009ppb/sec


ggplot(aes(TIME, CO2), data= S3.con.ef.z2)+
  #geom_point()+
  geom_line()+
  labs(title="S3-CON-z2 | Zanmon", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))


ggplot(aes(TIME, CO2), data= S3.con.ef.z.us1)+
  #geom_point()+
  geom_line()+
  labs(title="S3-CO2-z.us1 | Zanmon", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))

ggplot(aes(TIME, CO2), data= S3.con.ef.z.us2)+ #this was the redo after cleaning, not the reading with 2500 after 40 minutes
  #geom_point()+
  geom_line()+
  labs(title="S3-CO2-z.us2 | Zanmon", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))

ggplot(aes(TIME, CO2), data= S3.con.ef.z.us3)+
  #geom_point()+
  geom_line()+
  labs(title="S3-CO2-z.us3 | Zanmon", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))


# Create a single S3 df --------------------
df <- rbind(S3.CON, S3.CO2)

# Create rate of change in concentration (mol/mol/s) ---------------------------------------
#remember that ppm = 1umol/mol; ppb = 1nmol/mol

#CO2 is in ppm
df$RATE_CO2_molmol <- df$RATE_CO2 * 1e-6
summary(df$RATE_CO2_molmol, na.rm=TRUE) #avg 1.00e-07 mol/mol/sec

#CH4 is in ppb
df$RATE_CH4_molmol <- df$RATE_CH4 * 1e-9
summary(df$RATE_CH4_molmol, na.rm=TRUE) #avg  0.0 mol/mol/sec

# Calculate the fluxes: volume ----------------------------------------------------

##Total efflux volume = licor volume + tubing volume + chamber volume

#Licor volume
licor_volume <- 28 #cm^3

#Tubing volume = (length of 2 lines + length of inlets and outlet line) * licor's tubing estimate;  Licor estimates 0.079 cm^3 per cm of tubing
inset_tubes <- 7+7.5 #length of two inset tubes: 7cm + 7.5cm
tubing_volume <- ((2*304.8) + (inset_tubes))*0.079 #two tube lengths + inset tube length * volume scalar from licor

#Chamber volume
radius <- 9.5/2 #cm
depth <- 6 #cm
chamber_volume <- 3.1416*(radius^2)*depth #cm^3

## Total efflux volume
total_volume <- licor_volume+tubing_volume+chamber_volume #cm^3
total_volume #502.598 cm^3


# Calculate the fluxes: Climate variables  ----------------------------------------------------

# define air pressure, system volume, and air temp using aranet data
aranet.jan <- read.csv("Aranet-January/January 2025/CR_jan2025_aranet.csv", stringsAsFactors = FALSE)
# Convert aranet.jan$time to POSIXct assuming it's already in local time (UTC−6)
aranet.jan$time_CST <- ymd_hms(aranet.jan$time, tz = "America/Costa_Rica")
aranet.jan <- aranet.jan[!is.na(aranet.jan$time_CST), ]
aranet.jan$time_CST <- round_date(aranet.jan$time_CST, unit = "minute")

#filter aranet to site-treatment-time
range(df$time_CST[df$Treatment=="Elevated"]) #use this range to filter aranet below
climate.elev <- aranet.jan%>%filter(site=="S3"& treatment=="Elevated"& 
                                      time_CST >"2025-01-11 09:06:35 CST" & 
                                      time_CST < "2025-01-11 11:28:11 CST") 
range(df$time_CST[df$Treatment=="Control"]) #use this range to filter aranet below
#this range doesn't exist in the aranet, but the day before looks good
TEST <- aranet.jan%>%filter(site=="S3" & treatment=="Control")
plot(TEST$time_CST, TEST$Temperature..C.)
plot(TEST$time_CST, TEST$Atmospheric.pressure.hPa.)
climate.cont <- aranet.jan%>%filter(site=="S3"& treatment=="Control"& 
                                      time_CST > "2025-01-10 11:50:57 CST" & 
                                      time_CST < "2025-01-10 14:37:23 CST") 
summary(climate.elev)
summary(climate.cont)

#set the climate variables
A.Pressure.elev <- mean(climate.elev$Atmospheric.pressure.hPa.) # define for your system
T.Air.elev <- mean(climate.elev$Temperature..C.) # define for your system
A.Pressure.cont <- mean(climate.cont$Atmospheric.pressure.hPa.) # define for your system
T.Air.cont <- mean(climate.cont$Temperature..C.) # define for your system
Volume <- total_volume # define for your system

A.Pressure.elev # 920.3408 hPa
A.Pressure.cont #918.6617
Volume #502.598 cm^3
T.Air.elev #24.69859 degC
T.Air.cont #23.41617

# Calculate the fluxes: Create new column with equation -------------------
# The formula uses Ideal Gas Law to find how many moles of air are inside the chamber.
# Then multiplies that by the rate of CO₂ or CH₄ concentration change per mole of air per second.
# The result is the rate of moles of CO₂ or CH₄ produced or consumed per second inside the chamber (mol/s).

# (P*V)/(R*T)
#P: convert air pressure from hPA to PA -> (A.Pressure*100)
#V: convert volume from cm^3 to m^3 -> (Volume / 1000000)
#R, T: ideal gas law denominator (8.314) * (T.Air + 273.15), where 8.314 J/(mol·K) is the universal gas constant, R; TAir+273.15 converts temperature from Celsius to Kelvin.

df.fluxes <- df %>%
  mutate(
    CO2_flux.mol.s = if_else(
      Treatment == "Elevated",
      ((A.Pressure.elev * 100) * (Volume / 1e6) / (8.314 * (T.Air.elev + 273.15))) * RATE_CO2_molmol,
      ((A.Pressure.cont * 100) * (Volume / 1e6) / (8.314 * (T.Air.cont + 273.15))) * RATE_CO2_molmol
    ),
    CH4_flux.mol.s = if_else(
      Treatment == "Elevated",
      ((A.Pressure.elev * 100) * (Volume / 1e6) / (8.314 * (T.Air.elev + 273.15))) * RATE_CH4_molmol,
      ((A.Pressure.cont * 100) * (Volume / 1e6) / (8.314 * (T.Air.cont + 273.15))) * RATE_CH4_molmol
    ))

summary(df.fluxes)


# we want this in units of mol/m2/s, currently only mol/s
# thus, standardize by area
radius_m<-radius/100 #convert from cm to m
stem_area_m2 <- 3.1416*(radius_m^2) # use the surface area of the tree stem chamber

# standardize by area
df.fluxes <- df.fluxes %>%
  mutate(
    CO2_flux.mol.m2.s = CO2_flux.mol.s/(stem_area_m2),
    CH4_flux.mol.m2.s = CH4_flux.mol.s/(stem_area_m2)
  )

summary(df.fluxes$CH4_flux.mol.m2.s)
summary(df.fluxes$CO2_flux.mol.m2.s)


# and potentially actually want micromol/m2/s
df.fluxes <- df.fluxes %>%
  mutate(CO2_flux.umol.m2.s = CO2_flux.mol.m2.s * 1e6,
         CH4_flux.umol.m2.s = CH4_flux.mol.m2.s * 1e6)

summary(round(df.fluxes$CH4_flux.umol.m2.s, 2))
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max.       NA's 
# -0.0115393 -0.0003390  0.0000052  0.0000128  0.0003446  0.0093351          1 

summary(round(df.fluxes$CO2_flux.umol.m2.s, 2))
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -47.3564  -2.5580   0.3540   0.4522   3.4178  57.4500        1 

# Run an IQR to remove outliers -------------------------------------------

df.fluxes <- df.fluxes %>%
  group_by(REMARK_ID) %>%
  mutate(
    Q1 = quantile(CH4_flux.mol.m2.s, 0.25, na.rm = TRUE),
    Q3 = quantile(CH4_flux.mol.m2.s, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    is_outlier_ch4 = CH4_flux.mol.m2.s < (Q1 - 2 * IQR) |
      CH4_flux.mol.m2.s > (Q3 + 2 * IQR)
  ) %>%
  ungroup()

df.fluxes <- df.fluxes %>%
  group_by(REMARK_ID) %>%
  mutate(
    Q1 = quantile(CO2_flux.umol.m2.s, 0.25, na.rm = TRUE),
    Q3 = quantile(CO2_flux.umol.m2.s, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    is_outlier_co2 = CO2_flux.umol.m2.s < (Q1 - 2 * IQR) |
      CO2_flux.umol.m2.s > (Q3 + 2 * IQR)
  ) %>%
  ungroup()

write.csv(df.fluxes, "Efflux/efflux_cleaned/S3.elevated.control.fluxes.csv", row.names = FALSE)


# Plot the CO2 and CH4 fluxes! --------------------------------------------

ggplot(df.fluxes, aes(x = CO2_flux.umol.m2.s)) +
  geom_histogram(fill = "forestgreen", color = "black", bins = 30) +
  facet_wrap(~Treatment+REMARK_ID)+
  labs(
    title = "Distribution of CO2 Flux",
    x = expression("CO"[2]*" flux (µmol m"^{-2}*" s"^{-1}*")"),
    y = "Count"
  ) +
  theme_minimal()
ggsave("Efflux/efflux_cleaned/S3.co2hist.jpg")

ggplot(df.fluxes, aes(x = CH4_flux.umol.m2.s)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  facet_wrap(~Treatment+REMARK_ID)+
  labs(
    title = "Distribution of CH4 Flux",
    x = expression("CH"[4]*" flux (µmol m"^{-2}*" s"^{-1}*")"),
    y = "Count"
  ) +
  theme_minimal()
ggsave("Efflux/efflux_cleaned/S3.ch4hist.jpg")

ggplot(df.fluxes, aes(x = time_elapsed, y = CO2_flux.umol.m2.s)) +
  geom_line(size = 0.5, color = "forestgreen") +
  geom_point(data = df.fluxes %>% filter(is_outlier_co2 == TRUE), 
             aes(color = is_outlier_co2), 
             size = 1, 
             shape = 4)+
  facet_wrap(~Treatment+REMARK_ID)+
  labs(
    title = "S4 Elevated: CO2 Flux ",
    x = "Time elapsed (min)",
    y = expression("CO"[2]*" flux (µmol m"^{-2}*" s"^{-1}*")")
  ) +
  theme_grey()
ggsave("Efflux/efflux_cleaned/S3.co2.jpg")

ggplot(df.fluxes, aes(x = time_elapsed, y = CH4_flux.umol.m2.s)) +
  geom_line(size = 0.25, color = "skyblue") +
  geom_point(data = df.fluxes %>% filter(is_outlier_ch4 == TRUE), 
             aes(color = is_outlier_ch4), 
             size = 1, 
             shape = 4)+
  facet_wrap(~Treatment+REMARK_ID)+
  labs(
    title = "S4 Elevated: CH4 Flux ",
    x = "Time elapsed (min)",
    y = expression("CH"[4]*" flux (µmol m"^{-2}*" s"^{-1}*")")
  ) +
  theme_grey()
ggsave("Efflux/efflux_cleaned/S3.ch4.jpg")




summary(df.fluxes$CO2_flux.umol.m2.s[df.fluxes$Treatment=="Elevated"& df.fluxes$is_outlier_co2 =="FALSE"])
summary(df.fluxes$CO2_flux.umol.m2.s[df.fluxes$Treatment=="Control"& df.fluxes$is_outlier_co2 =="FALSE"])

summary(df.fluxes$CH4_flux.umol.m2.s[df.fluxes$Treatment=="Elevated"& df.fluxes$is_outlier_ch4 =="FALSE"])
summary(df.fluxes$CH4_flux.umol.m2.s[df.fluxes$Treatment=="Control"& df.fluxes$is_outlier_ch4 =="FALSE"])
