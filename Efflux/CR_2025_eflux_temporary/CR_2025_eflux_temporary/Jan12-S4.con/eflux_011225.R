
library(dplyr)
library(ggplot2)




##test script for eflux

getwd()

#df <- read.csv("C:/Users/shirkey/Downloads/TG10-01977-2025-01-12T064800.data.csv")
df <-read.csv("CR_2025_eflux_temporary/CR_2025_eflux_temporary/Jan12-S4.con/TG10-01977-2025-01-12T064800.data.csv")

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


# Write the final dataset -------------------------------------------------

S4.CON$site_treatment <-"S4_CON"

write.csv(S4.CON, "S4.CON_finalsamples_031124.csv", row.names = FALSE)






