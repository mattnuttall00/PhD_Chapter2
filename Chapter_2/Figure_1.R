library(tidyverse)
library(sf)
library(viridis)
library(ggrepel)
library(pals)

# load in cambodia shapefile
country.shp <- st_read('Spatial_data/Country Boundary.shp')
plot(country.shp)
str(country.shp)

ggplot(country.shp, aes(group=AREA, fill=AREA))+
  geom_sf(show.legend = F)

# Load in Province and Commune shapefiles
prov.shp <- st_read('Spatial_data/boundary_khet.shp')
comm.shp <- st_read('Spatial_data/boundary_khum.shp')

str(prov.shp)
ggplot(prov.shp, aes(group=KHETTRN))+
  geom_sf(show.legend = F)

str(comm.shp)
ggplot(comm.shp, aes(group=KHUM_NAME))+
  geom_sf(show.legend = F)

# find centroid locations for labels
prov.shp <- prov.shp %>% mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]),
                                lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))


# assign 25 colours manually
c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)


# plot together
custom.cols <- ggplot()+
              geom_sf(data=prov.shp[prov.shp$KHETTRN != "Tonle Sap",], 
                      aes(group=KHETTRN, fill=KHETTRN), show.legend = F)+
              scale_fill_manual(values = c25)+
              geom_sf(data=comm.shp, aes(group=KHUM_NAME), show.legend = F, alpha = 0.2)+
              theme(panel.background = element_rect(fill = 'white'))

ggsave("Figures/Fig_1_custom_cols.png", custom.cols,
       height=20, width = 20, unit="cm", dpi=300)


# plot with colour pallete from "pals", using ggrepel, and switching the order of the layers so that the colours are faded and the labels are on top of the commune lines 
pals.cols.25 <- ggplot()+
                geom_sf(data=comm.shp, aes(group=KHUM_NAME), show.legend = F)+
                geom_sf(data=prov.shp[prov.shp$KHETTRN != "Tonle Sap",], 
                        aes(group=KHETTRN, fill=KHETTRN), show.legend = F, alpha = 0.4)+
                geom_text_repel(data=prov.shp[prov.shp$KHETTRN != "Tonle Sap",],
                                aes(label=KHETTRN, x=lon, y=lat), size=6)+
                scale_fill_manual(values = cols25(n=24))+
                theme(panel.background = element_rect(fill = 'white'))

# The plot is too crowded with the full province names, so I will use numbers (excluding Tonle Sap) and then label the numbers in the figure caption
prov.shp$Index <- NA
index <- 1:24
for(i in 1:nrow(prov.shp)){
  prov.shp$Index[i] <- ifelse(prov.shp$KHETTRN[i]=="Tonle Sap",NA,index[i])
}
# the above is't working becasue the numbers are not consecutive because of Tonle Sap

# manually assign numbers
prov.shp$Index <- ifelse(prov.shp$KHETTRN=="Rotanak Kiri", "1", 
                  ifelse(prov.shp$KHETTRN=="Stung Treng", "2",
                  ifelse(prov.shp$KHETTRN=="Oddar Meanchey", "3",      
                  ifelse(prov.shp$KHETTRN=="Preah Vihear", "4",
                  ifelse(prov.shp$KHETTRN=="Banteay Meanchey", "5",
                  ifelse(prov.shp$KHETTRN=="Siem Reap", "6",
                  ifelse(prov.shp$KHETTRN=="Kampong Thom", "7",
                  ifelse(prov.shp$KHETTRN=="Mondulkiri", "8",
                  ifelse(prov.shp$KHETTRN=="Kratie", "9",
                  ifelse(prov.shp$KHETTRN=="Tonle Sap", NA,
                  ifelse(prov.shp$KHETTRN=="Kampong Chhnang", "10",
                  ifelse(prov.shp$KHETTRN=="Pursat", "11",
                  ifelse(prov.shp$KHETTRN=="Kampong Speu", "12",
                  ifelse(prov.shp$KHETTRN=="Prey Veng", "13",
                  ifelse(prov.shp$KHETTRN=="Svay Rieng", "14",
                  ifelse(prov.shp$KHETTRN=="Takeo", "15",
                  ifelse(prov.shp$KHETTRN=="Kampot", "16",
                  ifelse(prov.shp$KHETTRN=="Koh Kong", "17",
                  ifelse(prov.shp$KHETTRN=="Kep", "18",
                  ifelse(prov.shp$KHETTRN=="Sihanouk", "19",
                  ifelse(prov.shp$KHETTRN=="Battambang", "20",
                  ifelse(prov.shp$KHETTRN=="Pailan", "21",
                  ifelse(prov.shp$KHETTRN=="Kampong Cham", "22",
                  ifelse(prov.shp$KHETTRN=="Kandal", "23",
                  ifelse(prov.shp$KHETTRN=="Phnom Penh", "24", 
                         NA)))))))))))))))))))))))))


# plot with number labels
pals.cols.25.num <- ggplot()+
                geom_sf(data=comm.shp, aes(group=KHUM_NAME), show.legend = F)+
                geom_sf(data=prov.shp[prov.shp$KHETTRN != "Tonle Sap",], 
                        aes(group=KHETTRN, fill=KHETTRN), show.legend = F, alpha = 0.4)+
                geom_text_repel(data=prov.shp[prov.shp$KHETTRN != "Tonle Sap",],
                                aes(label=Index, x=lon, y=lat), size=8)+
                scale_fill_manual(values = cols25(n=24))+
                theme(panel.background = element_rect(fill = 'white'),
                      axis.title = element_blank(),
                      axis.text = element_text(size=12))

ggsave("Figures/Fig_1_numbers.png", pals.cols.25.num,
       height=25, width = 25, unit="cm", dpi=300)

# plot with legend
pals.cols.25.leg <- ggplot()+
                  geom_sf(data=comm.shp, aes(group=KHUM_NAME), show.legend = F)+
                  geom_sf(data=prov.shp[prov.shp$KHETTRN != "Tonle Sap",], 
                          aes(group=KHETTRN, fill=KHETTRN), show.legend = T, alpha = 0.4)+
                  scale_fill_manual(values = glasbey(n=24))+
                  theme(panel.background = element_rect(fill = 'white'))


ggsave("Figures/Fig_1_pals_25.png", pals.cols.25,
       height=20, width = 20, unit="cm", dpi=300)


pals.cols_alph <- ggplot()+
                  geom_sf(data=prov.shp[prov.shp$KHETTRN != "Tonle Sap",], 
                          aes(group=KHETTRN, fill=KHETTRN), show.legend = F)+
                  scale_fill_manual(values = alphabet(n=24))+
                  geom_sf(data=comm.shp, aes(group=KHUM_NAME), show.legend = F, alpha = 0.2)+
                  theme(panel.background = element_rect(fill = 'white'))