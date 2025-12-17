

#install and load necessary packages, geodata for soils and terra for working with spatrasters 
install.packages("ggplot2")
install.packages("geodata")
install.packages("terra")
install.packages("ggpubr")
install.packages("sf")

library(geodata)
library(terra)
library(ggplot2)
library(ggpubr)
library(sf)

#GET SOILS DATA

#read in specimen location dataset
dataset = read.csv("tuco_tuco_soils.csv")
#truncate into lat_long dataframe
lat_long = dataset[, 5:6]

#transform into a point spatial vector so extract() knows what to do with it 
lat_long <- vect(lat_long, geom = c("lon", "lat"), crs = "epsg:4326", keepgeom=FALSE)

#extract spatial rasters with each of five soil variables at 30cm depth across entire globe
bulk_density = soil_world(var = "bdod", depth = 30, stat = "mean", VSI = TRUE)
#extract soil data values from specific lattitudes and longitudes 
bulk_density_df = extract(bulk_density, lat_long)
#repeat for remaining 4 variables 
clay = soil_world(var = "clay", depth = 30, stat = "mean")
clay_df = extract(clay, lat_long)
sand = soil_world(var = "sand", depth = 30, stat = "mean")
sand_df = extract(sand, lat_long)
silt = soil_world(var = "silt", depth = 30, stat = "mean")
silt_df = extract(silt, lat_long)
soil_org_carbon = soil_world(var = "soc", depth = 30, stat = "mean")
soc_df = extract(org_carb_stock, lat_long)

#add soil variables into columns from initial dataset
dataset$Bulk_soil_density = bulk_density_df[, 2]
dataset$Sand_content = sand_df[, 2]
dataset$Clay_content = clay_df[, 2]
dataset$Silt_content = silt_df[, 2]
dataset$Organic_content = soc_df[, 2]

#load in individual specimen data 
tuco_data = read.csv("BIOS967_FINAL_DATASET.csv")
tuco_data = tuco_data[, 1:6]
#merge specimen data and lat-long soil data 
data = merge(dataset, tuco_data)
#remove row that didn't work (NA value)
data = na.omit(data)
data = data[, c(1, 5:16)]
write.csv(data, file = "BIOS967_COMPLETE_DATA.csv")


#STATISTICAL ANALYSIS 
##BULK DENSITY VS HFL
bd_vs_hfl = lm(Bulk_soil_density~Hind_foot_length, data = data)
summary(bd_vs_hfl)
#slope 0.003658, p value <0.001, R-squared = 0.07784 
#slight positive statistically significant relationship

##CLAY VS HFL
c_vs_hfl = lm(Clay_content~Hind_foot_length, data = data)
summary(c_vs_hfl)
#slope -0.36341, p value <0.001, R-squared = 0.2047
#slight negative statistically significant relationship

##SAND VS HFL
sa_vs_hfl = lm(Sand_content~Hind_foot_length, data = data)
summary(sa_vs_hfl)
#slope 0.44242, p value <0.001, R-squared = 0.1397
#positive statistically significant relationship

#SILT VS HFL
si_vs_hfl = lm(Silt_content~Hind_foot_length, data = data)
summary(si_vs_hfl)
#slope -0.07291, p value 0.1311, R-squared = 0.00398
#slightly negative insignificant relationship

#ORGANIC CARBON VS HFL
oc_vs_hfl = lm(Organic_content~Hind_foot_length, data = data)
summary(oc_vs_hfl)
#slope -0.17974, p value <0.001, R-squared = 0.05174
#slightly negative statistically significant relationship 


#FIGURES
##Fig 1 - Bulk density vs. HFL
bd_stat = paste0("P-value < 0.001, ", " ", 
                 "R-squared = 0.07784")
fig1 = ggplot(data = data, mapping = aes(y = Hind_foot_length, x = Bulk_soil_density)) + 
  geom_point() + 
  geom_smooth(method="lm", se = FALSE, color = "tomato") + 
  xlab("Bulk Soil Density (g/cm^2)") + 
  ylab("Hind Foot Length (mm)") + 
  theme_classic() + 
  ggtitle("Bulk Soil Density vs. HFL") + 
  annotate("text", x = 1.1, y = 58, 
           label = bd_stat, hjust = 0, vjust = 1, size = 3)

##Fig 2 - Clay content vs. HFL
cl_stat = paste0("P-value < 0.001,", " ", 
                 "R-squared = 0.2047")
fig2 = ggplot(data = data, mapping = aes(y = Hind_foot_length, x = Clay_content)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "tomato") + 
  xlab("Clay Content (%) ") + 
  ylab("Hind Foot Length (mm)") + 
  theme_classic()+ 
  theme_classic() + 
  ggtitle("Clay Content vs. HFL") + 
  annotate("text", x = 22, y = 58, 
           label = cl_stat, hjust = 0, vjust = 1, size = 3)

##Fig 3 - Sand content vs. HFL 
sd_stats = paste0("P-value < 0.001", " ", 
                  "R-squared = 0.1397")
fig3 = ggplot(data = data, mapping = aes(y = Hind_foot_length, x = Sand_content)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "tomato") + 
  xlab("Sand content (%) ") + 
  ylab("Hind Foot Length (mm)") + 
  theme_classic()+ 
  ggtitle("Sand Content vs. HFL") + 
  annotate("text", x = 30, y = 58, 
           label = sd_stats, hjust = 0, vjust = 1, size = 3)

##Fig 4 - Silt content vs. HFL
si_stat = paste0("P-value = 0.1311", " ",
                  "R-squared = 0.00398")
fig4 = ggplot(data = data, mapping = aes(y = Hind_foot_length, x = Silt_content)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "tomato") +
  xlab("Silt Content (%) ") + 
  ylab("Hind Foot Length (mm)") + 
  theme_classic()+
  ggtitle("Silt Content vs. HFL") + 
  annotate("text", x = 15, y = 58, 
           label = si_stat, hjust = 0, vjust = 1, size = 3)


##Fig 5 - Organic carbon content vs. HFL
oc_stat = paste0("P-value < 0.001, ", " ",
                 "R-squared = 0.05174")
fig5 = ggplot(data = data, mapping = aes(y = Hind_foot_length, x = Organic_content)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "tomato") + 
  xlab("Organic Carbon Content (%)") +
  ylab("Hind Foot Length (mm)") + 
  theme_classic() + 
  ggtitle("Organic Carbon Content vs. HFL") + 
  annotate("text", x = 8, y = 58, 
           label = oc_stat, hjust = 0, vjust = 1, size = 3)

ggpubr::ggarrange(fig1, fig2, fig3, fig4, fig5, ncol = 3, nrow = 2)
ggsave("bios967_fig1.png", width = 10, height = 10)

##add statistics to figure? 

##Fig 6 - Clay content figure 
#bin the hind foot lengths by average over one location, merge with original dataset
average_by_latlong = aggregate(Hind_foot_length~lat, data = data, FUN = mean)
colnames(average_by_latlong)[2] = "av_HFL"
data_av = merge(data, average_by_latlong)

#load in bolivia shape file
bolivia = st_read("bo.shp")

#create plot 
ggplot(data = bolivia) +
  geom_sf(col = "black") + 
  geom_point(data = data_av, mapping = aes(x = lon, y = lat, size = av_HFL, colour = Clay_content)) +
  scale_color_binned(palette = c("darkblue", "slateblue","orange2","tomato")) +
  theme_classic(base_size = 12) +
  ggtitle("Average HFL vs. Clay Content") + 
  xlab("Lattitude") + 
  ylab("Longitude") + 
  labs(color = "Clay Content (%)", size = "Average Hind Foot Length (mm)")

ggsave("clay_content_map.png", width = 8, height = 7)

  
