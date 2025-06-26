
#combine aranets 2025 from January 
#IQR removes outliers by site.
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(FSA)

# Set your folder path
folder_path <- "Aranet-January/January 2025/"

# List all CSV files
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
csv_files

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

#Change names and treatments
df$treatment <- gsub(" ", "", df$treatment)
unique(df$treatment)

df <- df %>%
  mutate(treatment = recode(treatment,
                            "cont" = "A",
                            "Cont" = "A",
                            "con" = "A",
                            "Con" = "A",
                            "sou" = "Source",
                            "co2"= "B",
                            "CO2"= "B"))%>%
  rename(aranetco2_ppm = `Carbon dioxide(ppm)`, 
         tempc = `Temperature(°C)`, 
         rh_pct = `Relative humidity(%)`, 
         pressure_hpa = `Atmospheric pressure(hPa)`,
         time = `Time(DD/MM/YYYY h:mm:ss A)`
         )
names(df)

# Make time/date as positX ------------------------------------------------
head(df)
str(df)

# Convert to POSIXct datetime
df <- df %>%
  mutate(
    #Parse datetime (day/month/year hour:minute:second AM/PM)
    #Assuming aranets are in local time
    time = dmy_hms(time, tz = "America/Costa_Rica")
  )
str(df)
        
#Create times of day
df <- df %>%
  mutate(
    hour = hour(time),
    time_of_day = case_when(
      hour >= 6 & hour < 18 ~ "Daytime",   # 6 AM - 6 PM
      hour < 6 | hour >= 18 ~ "Nighttime", # 6 PM - 6 AM
      TRUE ~ "NA"
    )
  )

unique(df$time_of_day)

write.csv(df, "Aranet-January/CR_jan2025_aranet_noIQR.csv", row.names = FALSE)
#df <- read.csv("Aranet-January/CR_jan2025_aranet_noIQR.csv", stringsAsFactors = FALSE)

# Create columns and average for time of day ----------
summary_df <- df %>%
  group_by(time_of_day, site, aranet, treatment) %>%
  summarise(
    mean_co2 = mean(aranetco2_ppm, na.rm = TRUE),
    mean_temp = mean(tempc, na.rm = TRUE),
    mean_rh = mean(rh_pct , na.rm = TRUE),
    mean_pressure = mean(pressure_hpa , na.rm = TRUE),
    
    median_co2 = median(aranetco2_ppm, na.rm = TRUE),
    median_temp = median(tempc, na.rm = TRUE),
    median_rh = median(rh_pct , na.rm = TRUE),
    median_pressure = median(pressure_hpa ),
    
    sd_co2 = sd(aranetco2_ppm, na.rm = TRUE),
    sd_temp = sd(tempc, na.rm = TRUE),
    sd_rh = sd(rh_pct , na.rm = TRUE),
    sd_pressure = sd(pressure_hpa , na.rm = TRUE),
    .groups = "drop"
  )

head(summary_df)

write.csv(summary_df, "Aranet-January/aranet.jan2025.TOD.summary.csv", row.names = FALSE)

# Boxplots  ---------------------------------------------------------------
# Combine site and aranet into one label for plotting
df <- df %>%
  mutate(site_aranet = paste0(site, "-", aranet),
         time_of_day = factor(time_of_day), 
         site_plot = paste0(site, "-", treatment))

# Create the boxplot CO2. Outliers in boxplots are points beyond 1.5×IQR
ggplot(df, aes(x = site_plot, y = aranetco2_ppm, fill = time_of_day)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  labs(
    title = "CO2 Levels by Site-Aranet and Time of Day", subtitle = "y-axis limit 400-5000",
    x = "Site-Plot",
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
ggsave("Aranet-January/aranet-co2.noIQR.TOD.jpg", dpi=300, units = "in", height = 4)


# Create the boxplot pressure.Outliers in boxplots are points beyond 1.5×IQR
ggplot(df, aes(x = site_plot, y = pressure_hpa, fill = time_of_day)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  labs(
    title = "Atmospheric pressure by Site-Aranet and Time of Day",
    x = "Site-Plot",
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
ggsave("Aranet-January/aranet-airpressure.noIQR.TOD.jpg", dpi=300, units = "in", height = 4)


# Create the boxplot pressure
ggplot(df, aes(x = site_plot, y = rh_pct, fill = time_of_day)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  labs(
    title = "Relative humidity by Site-Plot and Time of Day",
    x = "Site-Plot",
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
ggsave("Aranet-January/aranet-co2.rh.jpg", dpi=300, units = "in", height = 4)



# Create the boxplot pressure
ggplot(df, aes(x = site_plot, y = tempc, fill = time_of_day)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  labs(
    title = "Temperature by Site-Plot and Time of Day",
    x = "Site-Plot",
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
ggsave("Aranet-January/aranet-co2.temp.jpg", dpi=300, units = "in", height = 4)

# Corrplot ----------------------------------------------------------------
#this will need cleaning so the column/variable names are correct in the corrplot()
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






# Remove outliers -------------------------------------

aranet.jan <- df
aranet.jan$timestamp <- format(aranet.jan$time, "%H:%M:%S")
str(aranet.jan)

#create a site-plot column
aranet.jan <- aranet.jan %>% mutate(site_plot = paste0(site, "-", treatment))
         
# Fig. CO2 over time, by site-plot
## see how some dates have high peaks
ggplot(aranet.jan, aes(x = time, y = aranetco2_ppm)) +
  geom_point(size=0.4) +
  #geom_line()+
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%b %d"
  ) +
  facet_wrap(~site_plot)+
  labs(x = "Time", y = "CO2 (ppm)", title = "Jan 2025 CO2 Concentration Over Time") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Aranet-January/aranet.jan.dataavailability.jpg", units="in", width = 8)


# Remove aranet outliers with the IQ approach -----------------------------
#if something is 1.5* greater or lesser than the Q3 or Q1, respectively, it is removed. 
# Group by Site and calculate IQR bounds for each group
aranet.filtered <- aranet.jan %>%
  group_by(site_plot) %>%
  mutate(
    q1 = quantile(aranetco2_ppm, 0.25, na.rm = TRUE), # Calculate Q1
    q3 = quantile(aranetco2_ppm, 0.75, na.rm = TRUE), # Calculate Q3
    iqr = q3 - q1, # Calculate IQR
    upper = q3 + 1.5 * iqr, # Upper bound
    lower = q1 - 1.5 * iqr  # Lower bound
  ) %>%
  ungroup() %>%
  filter(aranetco2_ppm <= upper & aranetco2_ppm >= lower) # Filter outliers

summary(aranet.jan$aranetco2_ppm)
summary(aranet.filtered$aranetco2_ppm)

write.csv(aranet.filtered, "Aranet-January/aranet.IQRfiltered.site_plots.csv", row.names = FALSE)


#plot and compare the timeline now 
ggplot(aranet.filtered, aes(x = time, y = aranetco2_ppm)) +
  geom_point(size=0.2, alpha=0.3) +
  scale_color_brewer(palette = "Dark2")+
  scale_x_datetime(
    date_breaks = "2 day",
    date_labels = "%b %d"
  ) +
  facet_wrap(~site_plot, ncol=4)+
  labs(x = "Time", y = "CO2 (ppm)", title = "Jan 2025 CO2 Concentration IQR filter") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
ggsave("Aranet-January/aranet.IQRfiltered.site_plots.jpg", units="in", width = 8)


# Aggregate aranet to mean and median per TIME OF DAY (TOD)------------------
aranet.tod.summary <- aranet.filtered%>%group_by(site_plot, time_of_day)%>%
  summarise(aranet_co2_mean = mean(aranetco2_ppm, na.rm=TRUE),
            aranet_co2_sd = sd(aranetco2_ppm, na.rm=TRUE),
            aranet_co2_median = median(aranetco2_ppm, na.rm=TRUE),
            aranet_t_mean = mean(tempc, na.rm=TRUE),
            aranet_t_sd = sd(tempc, na.rm=TRUE),
            aranet_t_median = median(tempc, na.rm=TRUE),
            aranet_hpa_mean = mean(pressure_hpa, na.rm=TRUE),
            aranet_hpa_sd = sd(pressure_hpa, na.rm=TRUE),
            aranet_hpa_median = median(pressure_hpa, na.rm=TRUE),
            aranet_rh_mean = mean(rh_pct, na.rm=TRUE),
            aranet_rh_sd = sd(rh_pct, na.rm=TRUE),
            aranet_rh_median = median(rh_pct, na.rm=TRUE),
            )

# Aggregate aranet to mean and median per TIME OF DAY (TOD)------------------

aranet.daily.summary <- aranet.filtered%>%group_by(site_plot)%>%
  summarise(aranet_co2_mean = mean(aranetco2_ppm, na.rm=TRUE),
            aranet_co2_sd = sd(aranetco2_ppm, na.rm=TRUE),
            aranet_co2_median = median(aranetco2_ppm, na.rm=TRUE),
            aranet_t_mean = mean(tempc, na.rm=TRUE),
            aranet_t_sd = sd(tempc, na.rm=TRUE),
            aranet_t_median = median(tempc, na.rm=TRUE),
            aranet_hpa_mean = mean(pressure_hpa, na.rm=TRUE),
            aranet_hpa_sd = sd(pressure_hpa, na.rm=TRUE),
            aranet_hpa_median = median(pressure_hpa, na.rm=TRUE),
            aranet_rh_mean = mean(rh_pct, na.rm=TRUE),
            aranet_rh_sd = sd(rh_pct, na.rm=TRUE),
            aranet_rh_median = median(rh_pct, na.rm=TRUE))
aranet.daily.summary$time_of_day <- "24hr"

#combine TOD and daily summaries
aranet.summary <- rbind(aranet.tod.summary, aranet.daily.summary)

#save for later
write.csv(aranet.summary, "aranet.IQRfiltered.summary.byTOD-site_plot.csv", row.names = FALSE)


# Boxplot: CO2 site-plot for daytime and nighttime, IQR filtered -------------------------

#plot the difference in means between daytime and nighttime
ggplot(aranet.filtered, aes(x = site_plot, y = aranetco2_ppm, fill = time_of_day)) +
  geom_boxplot() +
  labs(y = "Mean CO2 (ppm)", x = "Site") +
  #facet_wrap(~site_plot)+
  scale_fill_brewer(palette = "Set2",name = "Time of Day") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("aranet.CO2.TOD.boxplot.jpg")

#Get just the three south sites
south <- aranet.filtered%>%filter((site %in% c("S3", "S4", "S5")) & treatment != "Source")
unique(south$treatment)

#plot the difference in means between daytime and nighttime
ggplot(south, aes(x = site_plot, y = aranetco2_ppm, fill = time_of_day)) +
  geom_boxplot() +
  labs(y = "Mean CO2 (ppm)", x = "Site") +
  #facet_wrap(~site_plot)+
  scale_fill_brewer(palette = "Set2",name = "Time of Day") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("aranet.CO2.TOD.boxplot-S3S4S5.jpg")



# So was there a significant differences in atmospheric co2 --------

# Ensure treatment is a factor
aranet.filtered$treatment <- as.factor(aranet.filtered$treatment)

# Filter to just plots, not sources
aranet.filtered.test <- aranet.filtered %>%
  filter(treatment %in% c("A", "B"))

# Kruskal Wallice test (non-parametric
kruskal.test(aranetco2_ppm ~ site_plot, data = aranet.filtered.test)
# data:  aranetco2_ppm by site_plot
# Kruskal-Wallis chi-squared = 49505, df = 17, p-value < 2.2e-16

dunn <- dunnTest(aranetco2_ppm ~ site_plot, data = aranet.filtered.test, method = "bonferroni")
sig_dunn <- dunn$res %>% 
  filter(P.adj < 0.05)
write.csv(sig_dunn, "dunn_test_sign.differe.site_plots.csv", row.names = FALSE)


# Get the statements ------------------------------------------------------

#save for later
aranet.summary <- read.csv("aranet.IQRfiltered.summary.byTOD-site_plot.csv", stringsAsFactors = FALSE)
south <- aranet.summary%>% 
  filter(site_plot %in% c("S5_A", "S5_B", "S4_A", "S4_B", "S3_A", "S3_B"))

#What is the range of CO2 daily across S3, S4, S5
range(south$aranet_co2_mean[south$time_of_day=="Daytime"]) #442.9552 533.4960
range(south$aranet_co2_mean[south$time_of_day=="Nighttime"]) #449.8061 547.0487

#What is the range of Temp daily across S3, S4, S5
range(south$aranet_t_mean [south$time_of_day=="Daytime"]) #21.28275 23.18830
range(south$aranet_t_mean [south$time_of_day=="Nighttime"]) #19.45759 22.34426

#What is the range of Rh daily across S3, S4, S5
range(south$aranet_rh_mean  [south$time_of_day=="Daytime"]) #442.9552 533.4960
range(south$aranet_rh_mean  [south$time_of_day=="Nighttime"]) #449.8061 547.0487

#What is the range of air pressure daily across S3, S4, S5
range(south$aranet_hpa_mean   [south$time_of_day=="Daytime"]) #442.9552 533.4960
range(south$aranet_hpa_mean   [south$time_of_day=="Nighttime"]) #449.8061 547.0487
