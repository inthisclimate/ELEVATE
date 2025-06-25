
library(dplyr)
library(ggplot2)

#install.packages("drc")
library(drc)


##test script for eflux

getwd()
df <- read.csv("C:/Users/shirkey/Downloads/TG10-01977-2025-01-11T061100.data.csv")
#df <- read.csv("C:/Users/shirkey/Downloads/TG10-01977-2025-01-11T061100.data.csv")
df <-read.csv("CR_2025_eflux_temporary/CR_2025_eflux_temporary/Jan11-S3/TG10-01977-2025-01-11T061100.data.csv")

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

# Rates?
S3.CON$RATE_CO2 <- c(NA, 
                     diff(S3.CON$CO2) / diff(S3.CON$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S3.CO2$RATE_CO2, na.rm=TRUE) #avg 0.24ppm/sec

S3.CON$RATE_CH4 <- c(NA, 
                     diff(S3.CON$CH4) / diff(S3.CON$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S3.CON$RATE_CH4, na.rm=TRUE) #avg 0.009ppb/sec


ggplot(aes(time, CO2), data= S3.con.ef.z2)+
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


# Export single df with all select samples' timeframes --------------------
S3.CON$site_treatment <-"S3_CON"
S3.CO2$site_treatment <-"S3_CO2"

S3 <- rbind(S3.CON, S3.CO2)
write.csv(S3, "S3_finalsamples_031125.csv", row.names = FALSE)
