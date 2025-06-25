
library(dplyr)
library(ggplot2)




##test script for eflux

getwd()

#df <- read.csv("C:/Users/shirkey/Downloads/TG10-01977-2025-01-12T064800.data.csv")
df <-read.csv("CR_2025_eflux_temporary/CR_2025_eflux_temporary/Jan14-S5/TG10-01977-2025-01-14T071100.data.csv")

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
# ""              "S5ELEBYRCRA5"  "S5ELEB#8REDO"  "S5ELEBYRCRA9"  "S5ELEBYRCRA10" "s5conbyrcra4"  "s5conbyrcra5" 
#  "s5conbyrcra6"  "s5conbyrcra8"  "s5conbyrcra7" 


# S5 elevated CO2 ---------------------------------------------------------

bc5 <- df %>%
  filter(
    REMARK=="S5ELEBYRCRA5" 
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%mutate(REMARK_ID = "BYRCRA#5", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                                units = "mins")))
bc8 <- df %>%
  filter(
    REMARK=="S5ELEB#8REDO" 
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%mutate(REMARK_ID = "BYRCRA#8REDO", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                                units = "mins")))
bc9 <- df %>%
  filter(
    REMARK=="S5ELEBYRCRA9" 
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%mutate(REMARK_ID = "BYRCRA#9", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                                units = "mins")))

bc10 <- df %>%
  filter(
    REMARK=="S5ELEBYRCRA10" 
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%mutate(REMARK_ID = "BYRCRA#10", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                                units = "mins")))

# Combine and get rates ---------------------------------------------------

S5.CO2 <- rbind(bc5, bc8, bc9, bc10) #bc8 redo is just called bc8 in the original hand-written efflux df, likely because it was interpreted as the good value. Here I call it the redo to clarify that it was the second take

S5.CO2$RATE_CO2 <- c(NA, 
                     diff(S5.CO2$CO2) / diff(S5.CO2$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S5.CO2$RATE_CO2, na.rm=TRUE) #avg 0.22ppm/sec
S5.CO2$RATE_CH4 <- c(NA, 
                     diff(S5.CO2$CH4) / diff(S5.CO2$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S5.CO2$RATE_CH4, na.rm=TRUE) #avg  -0.0003ppb/sec

# plot --------------------------------------------------------------------

#all samples
ggplot(aes(time_elapsed, CO2, color=REMARK_ID), data= S5.CO2)+ #z5
  #geom_point()+
  geom_line()+
  labs(title="S5 CO2 | BYRCRA", 
       x="Time (min)", 
       y = expression(CO[2] ~ "ppm"),
       color = "Tree")+
  theme_bw()

#all samples
ggplot(aes(time_elapsed, CH4, color=REMARK_ID), data= S5.CO2)+ #z5
  #geom_point()+
  geom_line()+
  labs(title="S5 CO2 | BYRCRA", 
       x="Time (min)", 
       y = "CH4 ppb",
       color = "Tree")+
  theme_bw()





# S5 con ------------------------------------------------------------------

bc5 <- df %>%
  filter(
    REMARK=="s5conbyrcra5" 
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%mutate(REMARK_ID = "BYRCRA#5", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                                units = "mins")))
bc6 <- df %>%
  filter(
    REMARK=="s5conbyrcra6" 
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%mutate(REMARK_ID = "BYRCRA#6", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                                units = "mins")))

bc7 <- df %>%
  filter(
    TIME <= "14:31:32" &
    REMARK=="s5conbyrcra7")%>%mutate(REMARK_ID = "BYRCRA#7", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                                units = "mins")))

bc8 <- df %>%
  filter(
    REMARK=="s5conbyrcra8" 
    #TIME >= as.POSIXct("2025-01-10 11:34:01 PST" ) &
    #TIME <= as.POSIXct("2025-01-10 12:04:01 PST")
  )%>%mutate(REMARK_ID = "BYRCRA#8", 
             time_elapsed = as.numeric(difftime(DATETIME, first(DATETIME), 
                                                units = "mins")))



# Combine and get rates ---------------------------------------------------

S5.CON <- rbind(bc5, bc6, bc7) #kick out bc8 bc it looks terrible

S5.CON$RATE_CO2 <- c(NA, 
                     diff(S5.CON$CO2) / diff(S5.CON$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S5.CON$RATE_CO2, na.rm=TRUE) #avg 0.3836ppm/sec
S5.CON$RATE_CH4 <- c(NA, 
                     diff(S5.CON$CH4) / diff(S5.CON$SECONDS)) #Estimate the rate of change with the difference between CO2 & time passing; add NA to fix mismatch
summary(S5.CON$RATE_CH4, na.rm=TRUE) #avg  0.007ppb/sec


# plot --------------------------------------------------------------------

#all samples 
ggplot(aes(time_elapsed, CO2, color=REMARK_ID), data= S5.CON)+ #z5
  #geom_point()+
  geom_line()+
  labs(title="S5 CON | BYRCRA", 
       x="Time (min)", 
       y = expression(CO[2] ~ "ppm"),
       color = "Tree")+
  theme_bw()

#all samples 
ggplot(aes(time_elapsed, CH4, color=REMARK_ID), data= S5.CON)+ #z5
  #geom_point()+
  geom_line()+
  labs(title="S5 CON | BYRCRA", 
       x="Time (min)", 
       y = "CH4 ppb",
       color = "Tree")+
  theme_bw()


# Write the final dataset -------------------------------------------------
S5.CON$site_treatment <-"S5_CON"
S5.CO2$site_treatment <-"S5_CO2"

S5 <- rbind(S5.CON, S5.CO2)
write.csv(S5, "S5_finalsamples_031124.csv", row.names = FALSE)






