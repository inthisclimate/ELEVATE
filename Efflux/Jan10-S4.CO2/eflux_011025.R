
library(dplyr)
library(ggplot2)

##test script for eflux

getwd()
setwd("C:/Users/Gabriela Shirkey/OneDrive - Chapman University/CostaRica/Christine")

#df <- read.csv("C:/Users/shirkey/Downloads/TG10-01977-2025-01-10T083000.data.csv")
df <-read.csv("CR_2025_eflux_temporary/CR_2025_eflux_temporary/Jan10-S4.CO2/TG10-01977-2025-01-10T083000.data.csv")

head(df)
names(df)
str(df)

# Combine the DATE and TIME columns into one combined datetime column
df$DATETIME <- as.POSIXct(paste(df$DATE, df$TIME), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
str(df$DATETIME)


#Remove values <400 ppm, as they're likely from moving equipment
df <- df%>%filter(CO2>400)



#what unique remarks are there to evaluate? 
unique(df$REMARK)
# [1] ""                "S4ELEMYRCOR1"    "S4ELCOMM1"       "S4-co2-v1@12.37"
# [5] "S4-co2-v3"       "S4-co2-v3r-1333" "S4-co2-eflux-v4"



# V1 ----------------------------------------------------------------------
#filter out the good stuff 30min
v1 <- df %>%
  filter(
    DATETIME >= as.POSIXct("2025-01-10 11:34:01", tz = "UTC") &
    DATETIME <= as.POSIXct("2025-01-10 12:04:01", tz = "UTC")&
    TIME!="11:47:09" & TIME!="11:47:46"
        # TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #   TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%mutate(REMARK_ID = "1", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), units = "mins")))

#ggplot V1 --filtered
ggplot(aes(DATETIME, CO2), data= v1)+
  #geom_point()+
  geom_line()+
  labs(title="S4-CO2-V1", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))


# V2 ----------------------------------------------------------------------
#filter out the good stuff 30min
v2 <- df %>%
  filter(
    DATETIME >= as.POSIXct("2025-01-10 12:23:00 ", tz = "UTC") &
    DATETIME <= as.POSIXct("2025-01-10 12:54:00", tz = "UTC"))%>%mutate(REMARK_ID = "2", 
              time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                                 units = "mins")))
  

#ggplot V2 --filtered
ggplot(aes(DATETIME, CO2), data= v2)+
  #geom_point()+
  geom_line()+
  labs(title="S4-CO2-V2", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))



# V3 ----------------------------------------------------------------------
#filter out the good stuff 30min
v3 <- df %>%
  filter(
    DATETIME >= as.POSIXct("2025-01-10 13:33:00 ", tz = "UTC") &
    DATETIME <= as.POSIXct("2025-01-10 14:00:00", tz = "UTC"))%>%mutate(REMARK_ID = "3", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), units = "mins")))

#ggplot V3 --filtered
ggplot(aes(DATETIME, CO2), data= v3)+
  #geom_point()+
  geom_line()+
  labs(title="S4-CO2-V3", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))



# V4 ----------------------------------------------------------------------


v4<- df%>%
  filter(REMARK=="S4-co2-eflux-v4" &
           DATETIME <= as.POSIXct("2025-01-10 14:50:00", tz = "UTC"))%>%
  mutate(REMARK_ID = "4", 
         time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), units = "mins")))

#ggplot V4
ggplot(aes(DATETIME, CO2), data=v4)+
  geom_line()+
  labs(title="S4-CO2-V4", 
       x="Time ETC/Universal", 
       y = expression(CO[2] ~ "ppm"))


# Combine for 1 csv S4 CO2 ------------------------------------------------
S4.CO2 <- rbind(v1, v2, v3, v4)

S4.CO2$RATE_CO2 <- c(NA, 
                     diff(S4.CO2$CO2) / diff(S4.CO2$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S4.CO2$RATE_CO2, na.rm=TRUE) #avg 0.1297ppm/sec
S4.CO2$RATE_CH4 <- c(NA, 
                     diff(S4.CO2$CH4) / diff(S4.CO2$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S4.CO2$RATE_CH4, na.rm=TRUE) #avg  0.0036ppb/sec


S4.CO2$site_treatment <-"S4_CO2"

#write.csv(S4.CO2, "S4.CO2.finalsamples_031125.csv", row.names = FALSE)


# Calculate fluxes with Coco 5/27/25 --------------------------------------------------------

#If you use these two chunks of code, you can convert the tree stem fluxes 
#from the raw output from the LICOR into a reasonable flux rate, and then standardize by tree stem area:

  
# define air pressure, system volume, and air temp
A.Pressure <- 925 # define for your system
Volume <-  401.79 # define for your system #cm3,
T.Air <- 25 # define for your system, Cdeg


# now convert from change in mol/mol/s to flux rate (mol/s)
  # TIME_CO2_mol_per_mol_per_s was the linear model slope of a flux fit, so the same output that we have currently gotten our data to!  
  #   All this does is convert units using the ideal gas constant and then standardize by area.
results_combo_FINAL_FLUXES <- S4.CO2%>% #replacing results_combo_treatmentinfo_minidf
  mutate(
    finalC02_flux = ((A.Pressure*133.322)*(Volume/1000000)/((8.314)*(T.Air + 273.15)))*RATE_CO2, #replacing TIME_CO2_mol_per_mol_per_s
    finalCH4_flux = ((A.Pressure*133.322)*(Volume/1000000)/((8.314)*(T.Air + 273.15)))*RATE_CH4 #replacing TIME_CH4_mol_per_mol_per_s
  )



# other things beyond the unit conversion?
# standardizing by area vs. mass - no one right answer
# area is fine, mass is often used for unevenly shaped objects (like an animal!)

# we want this in units of mol/m2/s, currently only mol/s
# and potentially actually want micromol/m2/s

# thus, standardize by area
temp_value <- 3.14*(0.0635^2) #2.5 inches to m
surface_area_jar <- temp_value # this is for an incubation experiment, need to use the surface area of the tree stem chamber

# should we bother trying to account for the curve of the tree?



# standardize by area

results_combo_FINAL_FLUXES <- results_combo_FINAL_FLUXES %>%
    mutate(
    finalC02_flux_byarea = finalC02_flux/(surface_area_jar),
    finalCH4_flux_byarea = finalCH4_flux/(surface_area_jar)
  )

head(results_combo_FINAL_FLUXES)

summary(results_combo_FINAL_FLUXES$finalC02_flux_byarea)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -21.4402  -1.1581   0.1603   0.2047   1.5474  26.0100        1 

summary(results_combo_FINAL_FLUXES$finalCH4_flux_byarea)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# -5.224330 -0.153497  0.002368  0.005804  0.156023  4.226386         1 