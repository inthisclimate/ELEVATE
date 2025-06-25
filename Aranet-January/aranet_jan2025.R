
#combine aranets 2025 from January 
#IQR removes outliers by site.
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)


# Set your folder path
path <- getwd()
folder_path <- paste0(path, "/January 2025")

# List all CSV files
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Extract first 5 characters from the filename to group by
file_info <- tibble(
  file = csv_files,
  prefix = basename(csv_files) %>% str_sub(2, 6),
  treatment = basename(csv_files) %>% str_sub(7, 10)  # Extracts first digit 6–9
)

# Function to read each CSV and add columns for ID and treatment
read_and_tag <- function(file, prefix, treatment) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(id = prefix, treatment = treatment)
}

# Read and combine all files with their id
#all_data <- map2_dfr(file_info$file, file_info$prefix, read_and_tag)

# Read and bind CSVs by matching file_info entries
df_all <- map2_dfr(file_info$file, seq_len(nrow(file_info)), function(file, i) {
  read_and_tag(file, file_info$prefix[i], file_info$treatment[i])
})

# remove duplicates
df <- distinct(df_all)
df$id <- gsub(" ", "", df$id)

# subset id column into site number and aranet number
df <- df %>%
  mutate(
    site = str_extract(id, "[A-Z]\\d$"),         # Extract letter-digit at the end, e.g., "N4", "S3"
    aranet = str_extract(id, "^\\d{1,2}")         # Extract 1–2 digits at the start
  )

df$treatment <- gsub(" ", "", df$treatment)
unique(df$treatment)

df <- df %>%
  mutate(treatment = recode(treatment,
                            "cont" = "Control",
                            "Cont" = "Control",
                            "con" = "Control",
                            "Con" = "Control",
                            "sou" = "Source",
                            "co2"= "Elevated",
                            "CO2"= "Elevated"))

# Make time/date as positX ------------------------------------------------
head(df)
str(df)

# Rename the time column for easier access
df <- df %>%
  rename(time = `Time(DD/MM/YYYY h:mm:ss A)`)

# Convert to POSIXct datetime
df <- df %>%
  mutate(time = dmy_hms(time))

df <- df %>%
  mutate(
    hour = hour(time),
    time_of_day = case_when(
      hour >= 6 & hour < 12 ~ "Morning", # 6 AM - 12 PM
      hour >= 12 & hour < 16 ~ "Afternoon", # 12 PM - 5 PM
      hour >= 16 & hour < 18 ~ "Evening", # 5 PM - 6 PM
      TRUE ~ "Night"
    )
  )
head(df)
write.csv(df, paste0(folder_path, "/CR_jan2025_aranet.csv"), row.names = FALSE)
df <- read.csv(paste0(folder_path, "/CR_jan2025_aranet.csv"), stringsAsFactors = FALSE)
str(df)

# Create columns that are morning, afternoon, evening, and night ----------

summary_df <- df %>%
  group_by(time_of_day, site, aranet, treatment) %>%
  summarise(
    mean_co2 = mean(Carbon.dioxide.ppm., na.rm = TRUE),
    mean_temp = mean(Temperature..C., na.rm = TRUE),
    mean_rh = mean(Relative.humidity..., na.rm = TRUE),
    mean_pressure = mean(Atmospheric.pressure.hPa., na.rm = TRUE),
    
    median_co2 = median(Carbon.dioxide.ppm., na.rm = TRUE),
    median_temp = median(Temperature..C., na.rm = TRUE),
    median_rh = median(Relative.humidity..., na.rm = TRUE),
    median_pressure = median(Atmospheric.pressure.hPa., na.rm = TRUE),
    
    sd_co2 = sd(Carbon.dioxide.ppm., na.rm = TRUE),
    sd_temp = sd(Temperature..C., na.rm = TRUE),
    sd_rh = sd(Relative.humidity..., na.rm = TRUE),
    sd_pressure = sd(Atmospheric.pressure.hPa., na.rm = TRUE),
    .groups = "drop"
  )

head(summary_df)

write.csv(summary_df, paste0(folder_path, "/CR_jan2025_aranet.summary.csv"), row.names = FALSE)

# Boxplots  ---------------------------------------------------------------

# Combine site and aranet into one label for plotting
df <- df %>%
  mutate(site_aranet = paste0(site, "-", aranet),
         time_of_day = factor(time_of_day, levels = c("Morning", "Afternoon", "Evening", "Night")))

# Create the boxplot CO2
ggplot(df, aes(x = site_aranet, y = `Carbon dioxide(ppm)`, fill = time_of_day)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  labs(
    title = "CO2 Levels by Site-Aranet and Time of Day",
    x = "Site-Aranet",
    y = "CO2 (ppm)",
    fill = "Time of Day"
  ) +
  facet_wrap(~treatment, scales = "free_x")+
 
  coord_cartesian(ylim = c(400, 5000))+  # Adjust visualization
  theme_light(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
ggsave(paste0(folder_path, "/aranet-co2.TOD.jpg"), dpi=300, units = "in", height = 4)


#Outliers in boxplots are points beyond 1.5×IQR



# Create the boxplot pressure
ggplot(df, aes(x = site_aranet, y = `Atmospheric pressure(hPa)`, fill = time_of_day)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  labs(
    title = "Atmospheric pressure by Site-Aranet and Time of Day",
    x = "Site-Aranet",
    y = "Atmospheric pressure (hPa)",
    fill = "Time of Day"
  ) +
  facet_wrap(~treatment, scales = "free_x")+
  
  #coord_cartesian(ylim = c(400, 800))+  # Adjust visualization
  theme_light(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
ggsave(paste0(folder_path, "/aranet-airpressure.TOD.jpg"), dpi=300, units = "in", height = 4)


# Create the boxplot pressure
ggplot(df, aes(x = site_aranet, y = `Relative humidity(%)`, fill = time_of_day)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  labs(
    title = "Relative humidity by Site-Aranet and Time of Day",
    x = "Site-Aranet",
    y = "Relative humidity (%)",
    fill = "Time of Day"
  ) +
  facet_wrap(~treatment, scales = "free_x")+
  
  #coord_cartesian(ylim = c(400, 800))+  # Adjust visualization
  theme_light(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
ggsave(paste0(folder_path, "/aranet-co2.rh.jpg"), dpi=300, units = "in", height = 4)



# Create the boxplot pressure
ggplot(df, aes(x = site_aranet, y = `Temperature(°C)`, fill = time_of_day)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  labs(
    title = "Temperature by Site-Aranet and Time of Day",
    x = "Site-Aranet",
    y = "Temperature (°C)",
    fill = "Time of Day"
  ) +
  facet_wrap(~treatment, scales = "free_x")+
  
  #coord_cartesian(ylim = c(400, 800))+  # Adjust visualization
  theme_light(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
ggsave(paste0(folder_path, "/aranet-co2.temp.jpg"), dpi=300, units = "in", height = 4)


# Corrplot ----------------------------------------------------------------

#is there any correlation between CO2 and other variables in southern sites? 
library(corrplot)
library(tidyr)

south <- df%>%filter(site =="S5" | site== "S3" | site == "S4")

# Select only numeric variables
south_numeric <- south %>%
  select(where(is.numeric)) %>%  # Keep only numeric columns
  drop_na()                      # Drop rows with NAs to avoid issues in correlation

# Create correlation matrix
south_matrix <- cor(south_numeric)

# Plot the correlation matrix
corrplot(south_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

#what about across all sites? 

# Select only numeric variables
df.cor <- df %>%
  select(where(is.numeric) & -c(hour)) %>%  # Keep only numeric columns
  drop_na()                      # Drop rows with NAs to avoid issues in correlation

# Create correlation matrix
all_matrix <- cor(df.cor)
all_matrix
cor.test(df.cor$`Temperature(°C)`, df.cor$`Carbon dioxide(ppm)`)
cor.test(df.cor$`Relative humidity(%)`, df.cor$`Carbon dioxide(ppm)`)
cor.test(df.cor$`Atmospheric pressure(hPa)`, df.cor$`Carbon dioxide(ppm)`)


# Plot the correlation matrix
corrplot(all_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)


#plot both south and all
corrplot(south_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
corrplot(all_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)






# Create CST time and remove outliers -------------------------------------
#as needed
aranet.jan <- read.csv(paste0(folder_path, "/CR_jan2025_aranet.csv")) #OR
#aranet.jan <-df

# Convert aranet.jan$time to POSIXct assuming it's already in local time (UTC−6)
aranet.jan$time_CST <- ymd_hms(aranet.jan$time, tz = "America/Costa_Rica")
# Remove na's created by parsing
aranet.jan <- aranet.jan[!is.na(aranet.jan$time_CST), ]
aranet.jan$time_CST <- round_date(aranet.jan$time_CST, unit = "minute")

range(aranet.jan$time_CST)
aranet.jan$Site <- aranet.jan$site
aranet.jan$Treatment <- aranet.jan$treatment

aranet.jan<- aranet.jan%>%select(-time)
aranet.jan$time_of_day_CST <- format(aranet.jan$time_CST, "%H:%M:%S")

str(aranet.jan)


# Fig. CO2 over time ------------------------------------------------------


#Look at the CO2 concentration over time by site
## see how some dates have high peaks
ggplot(aranet.jan, aes(x = time_CST, y = Carbon.dioxide.ppm.)) +
  geom_point(size=0.4) +
  #geom_line()+
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%b %d"
  ) +
  facet_wrap(~Site)+
  labs(x = "Time", y = "CO2 (ppm)", title = "Jan 2025 CO2 Concentration Over Time") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Aranet-January/aranet.jan.dataavailability.jpg", units="in", width = 8)

summary(aranet.jan$Carbon.dioxide.ppm.)


# Remove aranet outliers with the IQ approach -----------------------------
#if something is 1.5* greater or lesser than the Q3 or Q1, respectively, it is removed. 
# Group by Site and calculate IQR bounds for each group
aranet.filtered <- aranet.jan %>%
  group_by(Site) %>%
  mutate(
    q1 = quantile(Carbon.dioxide.ppm., 0.25, na.rm = TRUE), # Calculate Q1
    q3 = quantile(Carbon.dioxide.ppm., 0.75, na.rm = TRUE), # Calculate Q3
    iqr = q3 - q1, # Calculate IQR
    upper = q3 + 1.5 * iqr, # Upper bound
    lower = q1 - 1.5 * iqr  # Lower bound
  ) %>%
  ungroup() %>%
  filter(Carbon.dioxide.ppm. <= upper & Carbon.dioxide.ppm. >= lower) # Filter outliers
summary(aranet.jan$Carbon.dioxide.ppm.)
summary(aranet.filtered$Carbon.dioxide.ppm.)

write.csv(aranet.filtered, "Aranet-January/aranet.filtered.IQR.bySite.csv", row.names = FALSE)


#plot and compare the timeline now 
ggplot(aranet.filtered, aes(x = time_CST, y = Carbon.dioxide.ppm.)) +
  geom_point(size=0.4, alpha=0.3, aes(color=Treatment)) +
  scale_color_brewer(palette = "Dark2")+
  scale_x_datetime(
    date_breaks = "2 day",
    date_labels = "%b %d"
  ) +
  facet_wrap(~Site, ncol=4)+
  labs(x = "Time", y = "CO2 (ppm)", title = "Jan 2025 CO2 Concentration IQR filter") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
ggsave("Aranet-January/aranet.jan.dataavailability-IQRoutliersremoved-bySite.jpg", units="in", width = 8)


# Aggregate aranet to mean and median per TIME OF DAY (TOD)------------------

aranet.tod.summary <- aranet.filtered%>%group_by(Site, Treatment, time_of_day)%>%
  summarise(aranet_co2_mean = mean(Carbon.dioxide.ppm., na.rm=TRUE),
            aranet_co2_sd = sd(Carbon.dioxide.ppm., na.rm=TRUE),
            aranet_co2_median = median(Carbon.dioxide.ppm., na.rm=TRUE),
            aranet_t_mean = mean(Temperature..C., na.rm=TRUE),
            aranet_t_sd = sd(Temperature..C., na.rm=TRUE),
            aranet_t_median = median(Temperature..C., na.rm=TRUE),
            aranet_hpa_mean = mean(Atmospheric.pressure.hPa., na.rm=TRUE),
            aranet_hpa_sd = sd(Atmospheric.pressure.hPa., na.rm=TRUE),
            aranet_hpa_median = median(Atmospheric.pressure.hPa., na.rm=TRUE),
            aranet_rh_mean = mean(Relative.humidity..., na.rm=TRUE),
            aranet_rh_sd = sd(Relative.humidity..., na.rm=TRUE),
            aranet_rh_median = median(Relative.humidity..., na.rm=TRUE))

# Aggregate aranet to mean and median per TIME OF DAY (TOD)------------------

aranet.daily.summary <- aranet.filtered%>%group_by(Site, Treatment)%>%
  summarise(aranet_co2_mean = mean(Carbon.dioxide.ppm., na.rm=TRUE),
            aranet_co2_sd = sd(Carbon.dioxide.ppm., na.rm=TRUE),
            aranet_co2_median = median(Carbon.dioxide.ppm., na.rm=TRUE),
            aranet_t_mean = mean(Temperature..C., na.rm=TRUE),
            aranet_t_sd = sd(Temperature..C., na.rm=TRUE),
            aranet_t_median = median(Temperature..C., na.rm=TRUE),
            aranet_hpa_mean = mean(Atmospheric.pressure.hPa., na.rm=TRUE),
            aranet_hpa_sd = sd(Atmospheric.pressure.hPa., na.rm=TRUE),
            aranet_hpa_median = median(Atmospheric.pressure.hPa., na.rm=TRUE),
            aranet_rh_mean = mean(Relative.humidity..., na.rm=TRUE),
            aranet_rh_sd = sd(Relative.humidity..., na.rm=TRUE),
            aranet_rh_median = median(Relative.humidity..., na.rm=TRUE))
aranet.daily.summary$time_of_day <- "24hr"

#combine TOD and daily summaries
aranet.summary <- rbind(aranet.tod.summary, aranet.daily.summary)

#save for later
write.csv(aranet.summary, "aranet.TODsummary.IQRfiltered-bySite.csv", row.names = FALSE)

#plot the difference in means
ggplot(aranet.summary, aes(x = Site, y = aranet_co2_mean, fill = time_of_day)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(
    aes(ymin = aranet_co2_mean - aranet_co2_sd,
        ymax = aranet_co2_mean + aranet_co2_sd, 
        group = time_of_day ),
    position = position_dodge(width = 0.9),
    
    width = 0.2
  ) +
  coord_cartesian(ylim = c(350, NA)) +  # Start y-axis at 350, let it auto-scale max
  facet_wrap(~Treatment, scales = "free_x")+
  labs(y = "Mean CO2 (ppm)", x = "Site") +
  scale_fill_brewer(palette = "Set2",name = "Time of Day") +
  theme_minimal()
ggsave("aranetCO2.TODmeans.barchart.jpg")

#Plot difference in means just for 24 hours
aranet.summary.24hr <- aranet.summary%>%filter(time_of_day=="24hr")
ggplot(aranet.summary.24hr, aes(x = Site, y = aranet_co2_mean, fill=Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(
    aes(ymin = aranet_co2_mean - aranet_co2_sd,
        ymax = aranet_co2_mean + aranet_co2_sd, 
        group = time_of_day ),
    position = position_dodge(width = 0.9),
    
    width = 0.2
  ) +
  coord_cartesian(ylim = c(350, NA)) +  # Start y-axis at 350, let it auto-scale max
  facet_wrap(~Treatment, scales = "free_x")+
  labs(y = expression("Mean"~CO[2]~"(ppm)", x = "Site")) +
         
  scale_fill_brewer(palette = "Set2",name = "Time of Day") +
  theme_minimal()+
  theme(legend.position = "bottom")
ggsave("aranetCO2.TODmeans.barchart.jpg")


#plot the difference in means
ggplot(aranet.summary, aes(x = Site, y = aranet_t_mean, fill = time_of_day)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(
    aes(ymin = aranet_t_mean - aranet_t_sd,
        ymax = aranet_t_mean + aranet_t_sd, 
        group = time_of_day ),
    position = position_dodge(width = 0.9),
    
    width = 0.2
  ) +
  coord_cartesian(ylim = c(15, NA)) +  # Start y-axis at 350, let it auto-scale max
  facet_wrap(~Treatment, scales = "free_x")+
  labs(y = "Mean Temperature (°C)", x = "Site") +
  scale_fill_brewer(palette = "Set2",name = "Time of Day") +
  theme_minimal()
ggsave("aranetT.TODmeans.barchart.jpg")


#plot the difference in means
ggplot(aranet.summary, aes(x = Site, y = aranet_rh_mean, fill = time_of_day)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(
    aes(ymin = aranet_rh_mean - aranet_rh_sd,
        ymax = aranet_rh_mean + aranet_rh_sd, 
        group = time_of_day ),
    position = position_dodge(width = 0.9),
    
    width = 0.2
  ) +
  coord_cartesian(ylim = c(30, NA)) +  # Start y-axis at 350, let it auto-scale max
  facet_wrap(~Treatment, scales = "free_x")+
  labs(y = "Mean Rh (%)", x = "Site") +
  scale_fill_brewer(palette = "Set2",name = "Time of Day") +
  theme_minimal()
ggsave("aranetRh.TODmeans.barchart.jpg")


#Is there is distinct difference by time of day (TOD)
ggplot(aranet.filtered, aes(x = Site, y = Carbon.dioxide.ppm., color=time_of_day)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +  
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~Site, scales = "free_x")+
  labs(x = "Time of Day", y = "CO2 (ppm)", title = "Jan 2025 CO2 Concentration IQR filter") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(aranet.filtered, aes(x = Treatment, y = Carbon.dioxide.ppm., color=Treatment)) +
  geom_violin() +
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +  
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~Site)+
  labs(x = "Subplot", y = "CO2 (ppm)", title = "Jan 2025 CO2 Concentration IQR filter") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Aranet-January/aranet.jan.dataavailability-boxplot-IQRoutliersremoved-bySite.jpg", units="in", width = 8)


# So was there a significant differences in atmospheric co2 betwee --------
library(tidyr)
# Ensure treatment is a factor
aranet.filtered$treatment <- as.factor(aranet.filtered$treatment)
# Filter to just control/treatment
aranet.filtered.test <- aranet.filtered %>%
  filter(treatment %in% c("Control", "Elevated"))

# Group by site and run Wilcoxon test
site_results <- aranet.filtered.test %>%
  group_by(site) %>%
  filter(n_distinct(treatment) == 2) %>%  # make sure both treatments exist per site
  nest() %>%
  mutate(
    test = map(data, ~ wilcox.test(Carbon.dioxide.ppm. ~ treatment, data = .x)),
    tidy_test = map(test, broom::tidy)
  ) %>%
  unnest(tidy_test) %>%
  select(site, p.value, statistic, method)
site_results
write.csv(site_results, "significance.test.aranetco2.site.treatment.csv", row.names = FALSE)
