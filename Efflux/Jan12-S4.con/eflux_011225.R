
#Plotting and cleaning data
#efflux S4 January 10, 2025
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
df <-read.csv("Efflux/Jan12-S4.con/TG10-01977-2025-01-12T064800.data.csv")

head(df)
names(df)
str(df)

# Combine the DATE and TIME columns into one combined datetime column
df$DATETIME <- as.POSIXct(paste(df$DATE, df$TIME), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
str(df$DATETIME)
df$time_CST <- ymd_hms(df$DATETIME, tz = "America/Costa_Rica") #add in CR time too

#Remove values <400 ppm, as they're likely from moving equipment
df <- df%>%filter(CO2>400)

#what unique remarks are there to evaluate? 

unique(df$REMARK)
#""               "S4CONVEF1"      "S4CONVEF2"      "S4CONVEF4"      "S4CONVEF1-long"

#CORRECTION missing S4CONVEF3: Time start/end : 11:02-11:22 CR timezone


# S4 con ------------------------------------------------------------------

S4CONVEF1 <- df %>%
  filter(
    REMARK=="S4CONVEF1" 
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%mutate(REMARK_ID = "1", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                                units = "mins")))


S4CONVEF2 <- df %>%
  filter(
    REMARK=="S4CONVEF2" & 
    DIAG!="32" &#somehow this marked the negative gaps
      DATETIME >= as.POSIXct("2025-01-12 09:38:00", tz = "UTC")
  )%>%mutate(REMARK_ID = "2", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                                units = "mins")))

S4CONVEF3 <- df %>%
  filter( DATETIME >= as.POSIXct("2025-01-12 11:04:00", tz = "UTC") &
            DATETIME <= as.POSIXct("2025-01-12 11:23:00", tz = "UTC"))%>%
  mutate(REMARK_ID = "3",
         REMARK="S4CONVEF3",
     time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                        units = "mins")))

S4CONVEF4 <- df %>%
  filter(
    REMARK=="S4CONVEF4"
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%
  mutate(REMARK_ID = "4",
         time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                            units = "mins")))




# Combine and get rates ---------------------------------------------------

S4.CON <- rbind(S4CONVEF1, S4CONVEF2, S4CONVEF3, S4CONVEF4)

S4.CON$RATE_CO2 <- c(NA, 
                     diff(S4.CON$CO2) / diff(S4.CON$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S4.CON$RATE_CO2, na.rm=TRUE) #avg 1.31ppm/sec
S4.CON$RATE_CH4 <- c(NA, 
                     diff(S4.CON$CH4) / diff(S4.CON$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S4.CON$RATE_CH4, na.rm=TRUE) #avg  0.037ppb/sec

S4.CON$Site <- "S4"
S4.CON$Treatment <- "Control"

df<- S4.CON

# plot --------------------------------------------------------------------

#all samples in S3CO2
ggplot(aes(time_elapsed, CO2, color=REMARK_ID), data= S4.CON)+ #z5
  #geom_point()+
  geom_line()+
  labs(title="S4 CON | VOCGUA", 
       x="Time (min)", 
       y = expression(CO[2] ~ "ppm"),
       color = "Tree")+
  theme_bw()

#all samples in S3CO2
ggplot(aes(time_elapsed, CH4, color=REMARK_ID), data= S4.CON)+ #z5
  #geom_point()+
  geom_line()+
  labs(title="S4 CON | VOCGUA", 
       x="Time (min)", 
       y = expression(CO[2] ~ "ppm"),
       color = "Tree")+
  theme_bw()


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
range(df$time_CST) #use this range to filter aranet below
climate <- aranet.jan%>%filter(site=="S4"& treatment=="Control"& 
                                 time_CST >"2025-01-12 09:08:45 CST" & 
                                 time_CST < "2025-01-12 11:23:00 CST") 
summary(climate)

#set the climate variables
A.Pressure <- mean(climate$Atmospheric.pressure.hPa.) # define for your system
Volume <- total_volume # define for your system
T.Air <- mean(climate$Temperature..C.) # define for your system

A.Pressure # 914.6328 hPa
Volume #502.598 cm^3
T.Air #24.86418 degC


# Calculate the fluxes: Create new column with equation -------------------
# The formula uses Ideal Gas Law to find how many moles of air are inside the chamber.
# Then multiplies that by the rate of CO₂ or CH₄ concentration change per mole of air per second.
# The result is the rate of moles of CO₂ or CH₄ produced or consumed per second inside the chamber (mol/s).

# (P*V)/(R*T)
#P: convert air pressure from hPA to PA -> (A.Pressure*100)
#V: convert volume from cm^3 to m^3 -> (Volume / 1000000)
#R, T: ideal gas law denominator (8.314) * (T.Air + 273.15), where 8.314 J/(mol·K) is the universal gas constant, R; TAir+273.15 converts temperature from Celsius to Kelvin.

df.fluxes <- df %>% #updated from results_combo_treatmentinfo_minidf used in og example
  mutate(
    CO2_flux.mol.s = ((A.Pressure*100)*(Volume/1000000)/((8.314)*(T.Air + 273.15)))*RATE_CO2_molmol , # updated from TIME_CO2_mol_per_mol_per_s used in original example
    CH4_flux.mol.s = ((A.Pressure*100)*(Volume/1000000)/((8.314)*(T.Air + 273.15)))*RATE_CH4_molmol # updated from TIME_CH4_mol_per_mol_per_s used in original example 
  )
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
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -1.0e-02  0.0e+00  0.0e+00 -2.1e-06  0.0e+00  0.0e+00        1 

summary(round(df.fluxes$CO2_flux.umol.m2.s, 2))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -50.080   1.680   3.430   3.592   5.340  22.640       1 


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


write.csv(df.fluxes, "Efflux/efflux_cleaned/S4.control.fluxes.csv", row.names = FALSE)



# Plot the CO2 and CH4 fluxes! --------------------------------------------


ggplot(df.fluxes, aes(x = CO2_flux.umol.m2.s)) +
  geom_histogram(fill = "forestgreen", color = "black", bins = 30) +
  facet_wrap(~REMARK_ID)+
  labs(
    title = "Distribution of CO2 Flux",
    x = expression("CO"[2]*" flux (µmol m"^{-2}*" s"^{-1}*")"),
    y = "Count"
  ) +
  theme_minimal()
ggsave("Efflux/efflux_cleaned/S4.control.co2hist.jpg")

ggplot(df.fluxes, aes(x = CH4_flux.umol.m2.s)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  facet_wrap(~REMARK_ID)+
  labs(
    title = "Distribution of CH4 Flux",
    x = expression("CH"[4]*" flux (µmol m"^{-2}*" s"^{-1}*")"),
    y = "Count"
  ) +
  theme_minimal()
ggsave("Efflux/efflux_cleaned/S4.control.ch4hist.jpg")

ggplot(df.fluxes, aes(x = time_elapsed, y = CO2_flux.umol.m2.s)) +
  geom_line(size = 0.25, color = "forestgreen") +
  geom_point(data = df.fluxes %>% filter(is_outlier_co2 == TRUE), 
             aes(color = is_outlier_co2), 
             size = 1, 
             shape = 4)+
  facet_wrap(~REMARK_ID)+
  labs(
    title = "S4 Elevated: CO2 Flux ",
    x = "Time elapsed (min)",
    y = expression("CO"[2]*" flux (µmol m"^{-2}*" s"^{-1}*")")
  ) +
  theme_grey()
ggsave("Efflux/efflux_cleaned/S4.control.co2.jpg")

ggplot(df.fluxes, aes(x = time_elapsed, y = CH4_flux.umol.m2.s)) +
  geom_line(size = 0.25, color = "skyblue") +
  geom_point(data = df.fluxes %>% filter(is_outlier_ch4 == TRUE), 
             aes(color = is_outlier_ch4), 
             size = 1, 
             shape = 4)+
  facet_wrap(~REMARK_ID)+
  labs(
    title = "S4 Elevated: CH4 Flux ",
    x = "Time elapsed (min)",
    y = expression("CH"[4]*" flux (µmol m"^{-2}*" s"^{-1}*")")
  ) +
  theme_grey()
ggsave("Efflux/efflux_cleaned/S4.control.ch4.jpg")



summary(df.fluxes$CO2_flux.umol.m2.s[df.fluxes$Treatment=="Control"& df.fluxes$is_outlier_co2 =="FALSE"])
summary(df.fluxes$CH4_flux.umol.m2.s[df.fluxes$Treatment=="Control"& df.fluxes$is_outlier_ch4 =="FALSE"])




