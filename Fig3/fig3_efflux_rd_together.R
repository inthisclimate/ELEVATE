
#Figure 3 of the efflux rd paper


#Authors: Gabriela Shirkey (figures); Gaku (lead data curator)

library(ggplot2)
library(dplyr)
library(patchwork)

# Add Dark Respiration data -----------------------------------------------

dr <- read.csv("leaf_respiration/LI-6800_dR.csv", stringsAsFactors = FALSE)

#Check it out!
head(dr)
#Rd = dark respiration, where 400 is 400ppm and 800 is 800ppm
names(dr)

#Remove unknown species
dr <- dr%>%filter(Species!="")
unique(dr$Species)

#update names
dr <- dr%>%select(Site, Treatment, Rd_400, Rd_800, Plant_ID, Species)%>%
  mutate(Treatment = recode(Treatment,
                            "Control" = "A",
                            "Elevated" = "B",
                            "cont" = "A",
                            "Cont" = "A",
                            "con" = "A",
                            "Con" = "A",
                            "CON" = "A",
                            "sou" = "Source",
                            "co2"= "B",
                            "CO2"= "B"))%>%
  filter(Treatment!="Source")
str(dr)
dr$site_plot <- paste0(dr$Site, "-", dr$Treatment)

#summarize
dr <- dr%>%group_by(site_plot, Plant_ID, Species)%>%summarize(Rd_400 = mean(Rd_400, na.rm=TRUE),
                                           Rd_800 = mean(Rd_800, na.rm=TRUE))


# Add efflux data ---------------------------------------------------------

efflux <- read.csv("Efflux/efflux_cleaned/efflux_fulldataset.csv", stringsAsFactors = FALSE)
str(efflux)

#update names
efflux <- efflux%>%select(Site, Treatment, REMARK, CO2_flux.umol.m2.s, CH4_flux.umol.m2.s, Species)%>%
  mutate(Treatment = recode(Treatment,
                            "Control" = "A",
                            "Elevated" = "B",
                            "cont" = "A",
                            "Cont" = "A",
                            "con" = "A",
                            "Con" = "A",
                            "CON" = "A",
                            "sou" = "Source",
                            "co2"= "B",
                            "CO2"= "B"))%>%
  filter(Treatment!="Source")
str(efflux)

efflux$site_plot <-   paste0(efflux$Site, "-", efflux$Treatment)

#summarize
efflux <- efflux%>%group_by(site_plot, REMARK, Species)%>%summarize(CO2_flux.umol.m2.s = mean(CO2_flux.umol.m2.s, na.rm=TRUE),
                                                   CH4_flux.umol.m2.s = mean(CH4_flux.umol.m2.s, na.rm=TRUE))

# Add aranet data ---------------------------------------------------------

aranet <- read.csv("Aranet-January/aranet.IQRfiltered.site_plots.csv", stringsAsFactors = FALSE)
str(aranet)

#keep essentials
aranet <- aranet%>%select(c(site, aranetco2_ppm, site_plot, treatment, time_of_day))%>%
  rename(Site = site,
         Treatment = treatment)%>%
  filter(time_of_day == "Daytime")
str(aranet)

#summarize
aranet <- aranet%>%group_by(site_plot)%>%
  summarize(aranetco2_ppm = mean(aranetco2_ppm, na.rm=TRUE))


# get soil respiration data -----------------------------------------------

soilr <- read.csv("soil_respiration/ELEVATE-DataProcessed/ELEVATEsoilflux_df.csv", stringsAsFactors = FALSE)
head(soilr)

#update names
soilr <- soilr%>%select(Site, Treatment, Collar, FCO2_DRY_umolm2s, FCH4_DRY_nmolm2s)%>%
  mutate(Treatment = recode(Treatment,
                            "Control" = "A",
                            "Elevated" = "B",
                            "cont" = "A",
                            "Cont" = "A",
                            "con" = "A",
                            "Con" = "A",
                            "CON" = "A",
                            "sou" = "Source",
                            "co2"= "B",
                            "CO2"= "B"))%>%
  filter(Treatment!="Source")
str(soilr)

soilr$site_plot <-   paste0(soilr$Site, "-", soilr$Treatment)

#summarize
soilr <- soilr%>%group_by(site_plot, Collar)%>%summarize(FCO2_DRY_umolm2s = mean(FCO2_DRY_umolm2s, na.rm=TRUE),
                                                         FCH4_DRY_nmolm2s = mean(FCH4_DRY_nmolm2s, na.rm=TRUE))



# merge data --------------------------------------------------------------

df.dr <- merge(dr, aranet, by="site_plot")
df.efflux <- merge(aranet, efflux, by="site_plot")
df.soilr <- merge(aranet, soilr, by="site_plot")

#rename species to uppercase
df.efflux <- df.efflux%>%
  mutate(Species = case_when(
  Species=="Zanmon" ~ "ZANMON",
  Species=="Vacgua" ~ "VACGUA",
  Species=="Brycra" ~ "BRYCRA",
  TRUE ~ Species ))

# Scatterplots ----------------------------------------------------------------

# get from both datasets
unique(df.dr$Species)#"ZANMON" "MYRCOR" "BRYCRA"
unique(df.efflux$Species) # "ZANMON" "VACGUA" "BRYCRA"
species_levels <- c("ZANMON", "VACGUA", "BRYCRA", "MYRCOR")  # Replace with actual names


#create a dummy row to convince ggplot there are four species in each dataset
dummy_rows <- data.frame(
  site_plot = NA_character_,
  aranetco2_ppm = NA_real_,
  REMARK = NA_character_,
  CO2_flux.umol.m2.s = NA_real_,
  CH4_flux.umol.m2.s = NA_real_,
  Species = "MYRCOR"
)

df.efflux <- rbind(df.efflux, dummy_rows)

dummy_rows <- data.frame(
  site_plot = NA_character_,
  Plant_ID = NA_character_,
  Species = "BRYCRA",
  Rd_400 = NA_real_,
  Rd_800 = NA_real_,
  aranetco2_ppm = NA_real_,
  stringsAsFactors = FALSE
)

df.dr <- rbind(df.dr, dummy_rows)


# Assign named colors (these can be hex codes or R color names)
species_colors <- c(
  "ZANMON" = "#1b9e77",
  "VACGUA" = "#d95f02",
  "BRYCRA" = "#7570b3",
  "MYRCOR" = "#e7298a"
)

# Define manual shape mapping for species
species_shapes <- c(
  "ZANMON" = 16,
  "VACGUA" = 17,
  "BRYCRA" = 15,
  "MYRCOR" = 18
)

rd.400 <- ggplot(df.dr, aes(x = aranetco2_ppm, y = Rd_400)) +
  geom_point(aes(color = Species, shape = Species), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", show.legend = FALSE) +  # one blue line only
  labs(
    y = expression("R"[d]*" "[400][ppm]*" (µmol m"^{-2}*" s"^{-1}*")"),
    x = expression("CO"[2]*" ppm")
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_manual(values = species_colors) +
  scale_shape_manual(values = species_shapes)
rd.400

rd.800 <- ggplot(df.dr, aes(x = aranetco2_ppm, y = Rd_800)) +
  geom_point(aes(color = Species, shape = Species), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", show.legend = FALSE)+
  
  labs(y = expression("R"[d]*" "[800][ppm]*" (µmol m"^{-2}*" s"^{-1}*")"), 
       x = expression("CO"[2]*" ppm")) + 
  theme_bw() + 
  theme(legend.position = "") + 
  scale_color_manual(values = species_colors) +
  scale_shape_manual(values = species_shapes)
rd.800

efflux.co2 <- ggplot(df.efflux, aes(x = aranetco2_ppm, y = CO2_flux.umol.m2.s)) +
  geom_point(aes(color = Species, shape = Species), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", show.legend = FALSE)+
  
  labs(y = expression("CO"[2]*" (µmol m"^{-2}*" s"^{-1}*")"),
       x = expression("CO"[2]*" ppm")) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  scale_color_manual(values = species_colors) +
  scale_shape_manual(values = species_shapes)
efflux.co2

efflux.ch4 <- ggplot(df.efflux, aes(x = aranetco2_ppm, y = (1000*CH4_flux.umol.m2.s))) +
  geom_point(aes(color = Species, shape = Species), size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", show.legend = FALSE)+
  
  labs(y = expression("CH"[4]*" (nmol m"^{-2}*" s"^{-1}*")"),
       x = expression("CO"[2]*" ppm")) + 
  theme_bw() + 
  theme(legend.title = element_text(face = "bold"), legend.position = "bottom") + 
  scale_color_manual(values = species_colors) +
  scale_shape_manual(values = species_shapes)
efflux.ch4

df.soilr <- df.soilr%>%filter(site_plot %in% c("S3-A", "S3-B", "S4-A", "S4-B", "S5-A", "S5-B"))
soil.co2 <- ggplot(df.soilr, aes(x = aranetco2_ppm, y = FCO2_DRY_umolm2s)) +
  geom_point(aes(color=site_plot), shape=1, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black",show.legend = FALSE)+
  
  labs(y = expression("CO"[2]*" (µmol m"^{-2}*" s"^{-1}*")"),
       x = expression("CO"[2]*" ppm"),
       color = "Site-Plot") + 
  theme_bw() + 
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",      # Aligns items in one row
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(nrow = 1)) +  # Forces one-row legend
  scale_color_brewer(palette = "Set3")
soil.co2

soil.ch4 <- ggplot(df.soilr, aes(x = aranetco2_ppm, y = FCH4_DRY_nmolm2s)) +
  geom_point(aes(color=site_plot), shape=1, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black",show.legend = FALSE)+
  
  labs(y = expression("CH"[4]*" (nmol m"^{-2}*" s"^{-1}*")"),
       x = expression("CO"[2]*" ppm")) + 
  theme_bw() + 
  theme(legend.position = "bottom")+
  scale_color_brewer(palette="Dark2")
soil.ch4


# Fig3. efflux paper--> Stitch the four together -------------------------------------------------

# Combine the plots into a 2x2 layout:
combined_plot <- (efflux.ch4 | efflux.co2) / (rd.400 | rd.800) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Display the combined plot
print(combined_plot)

# ATBC/ESA poster--> Stitch the six together -------------------------------------------------
# Combine the plots into a 2x2 layout:
combined_plot <- (efflux.ch4 | efflux.co2) / (rd.400 | rd.800) / (soil.ch4 | soil.co2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Display the combined plot
print(combined_plot)
ggsave("fig3/combined_plot.jpg", combined_plot, dpi = 300, units = "in", width = 7, height = 10)
