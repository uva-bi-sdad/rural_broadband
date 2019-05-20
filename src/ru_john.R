library(sf)
library(dplyr)
library(furniture)
library(naniar)
library(ggplot2)
library(maps)
library(ggthemes)
library(fiftystater)

#
# Read in data ------------------------------------------------------------------------------------------
#

rus_approved <- st_read("data/rus_broadband_servicearea/RD_BB_ApprovedSA.shp")
rus_infraborr <- st_read("data/rus_broadband_servicearea/RD_BB_InfraBorrCLEC.shp")
rus_infrastructure <- st_read("data/rus_broadband_servicearea/RD_BB_InfrastructureBorr.shp")


#
# Inspect ------------------------------------------------------------------------------------------
#

glimpse(rus_approved)
glimpse(rus_infraborr)
glimpse(rus_infrastructure)

plot(st_geometry(rus_approved))
plot(st_geometry(rus_infraborr))
plot(st_geometry(rus_infrastructure))

# Get US map
usa <- st_as_sf(map("state", fill = TRUE, plot = FALSE))

ggplot() +
  geom_sf(data = usa, color = "#2b2b2b", fill = "white", size = 0.125) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
  ggthemes::theme_map()


geom_sf(data = USA, color = "#2b2b2b", fill = "white", size  =0.125)


#
# Basic descriptives ------------------------------------------------------------------------------------------
#

st_crs(usa)
st_crs(rus_approved)

# If not supplied, coord_sf() will take the CRS from the first layer and automatically transform all other layers to use that CRS. This ensures that all data will correctly line up

# rus_approved
gg_miss_var(rus_approved)
table1(rus_approved, rus_approved$STATUS, rus_approved$SERVICETYP, rus_approved$SERVICELEV, rus_approved$PROGRAMTYP, rus_approved$PROGRAMSER, rus_approved$SERVICEA_1,
       rus_approved$APPSTATUSS, rus_approved$SUM_HOUSEH, rus_approved$SUM_HOUSIN, rus_approved$SUM_AREA_L, rus_approved$UL_SPEED, rus_approved$TECHTYPE, rus_approved$SPEED_CODE,
       rus_approved$PCT_HISPEE, rus_approved$DL_SPEED, na.rm = FALSE, export = "rus_approved")

ggplot() +
  geom_sf(data = usa, color = "#2b2b2b", fill = "white", size = 0.12) +
  geom_sf(data = rus_approved, aes(fill = SERVICETYP), lwd = 0) +
  coord_sf(xlim = fifty_states$long, ylim = fifty_states$lat, expand = FALSE, datum = NA) +
  labs(title = "Approved RUS applications by program", fill = "Program type", subtitle = "Note: Data shown only for contiguous US states.") +
  theme(legend.position = "bottom", panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))

# rus_infraborr
gg_miss_var(rus_infraborr)
table1(rus_infraborr, rus_infraborr$BORROWER, rus_infraborr$CITY, rus_infraborr$STATE, rus_infraborr$NTCA, rus_infraborr$SPEED_CODE, rus_infraborr$TECHTYPE,
       rus_infraborr$DL_SPEED, rus_infraborr$UL_SPEED, rus_infraborr$PCT_HISPEE, na.rm = FALSE, export = "rus_infraborr")

ggplot() +
  geom_sf(data = usa, color = "#2b2b2b", fill = "white", size = 0.12) +
  geom_sf(data = rus_infraborr, aes(fill = BORROWER), lwd = 0) +
  coord_sf(xlim = fifty_states$long, ylim = fifty_states$lat, expand = FALSE, datum = NA) +
  labs(title = "RUS borrower companies?", fill = "Borrower", subtitle = "Note: Data shown only for contiguous US states.") +
  theme(legend.position = "right", legend.direction = "vertical", panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 20))

# rus_infrastructure
gg_miss_var(rus_infrastructure)
table1(rus_infrastructure, rus_infrastructure$CITY, rus_infrastructure$NTCA, rus_infrastructure$STATE, rus_infrastructure$BORROWER, rus_infrastructure$HOUSING_UN,
       rus_infrastructure$HOUSEHOLDS, rus_infrastructure$TOTAL_POPU, rus_infrastructure$SPEED_CODE, rus_infrastructure$TECHTYPE, rus_infrastructure$DL_SPEED, 
       rus_infrastructure$UL_SPEED, rus_infrastructure$PCT_HISPEE, na.rm = FALSE, export = "rus_infrastructure")

rus_infrastructure$DL_SPEED1 <- rus_infrastructure$DL_SPEED
levels(rus_infrastructure$DL_SPEED1) <- list("1 Gbps" = "1 Gbps", "500 Mbps" = "500 Mbps", "250 Mbps" = "250 Mbps", "100 Mbps" = "100 Mbps", 
                                            "50 Mbps" = "50 Mpbs", "25 Mbps" = "25 Mbps", "20 Mbps" = "20 Mbps", "15 Mbps" = "15 Mbps", "10 Mbps" = "10 Mbps",
                                            "4 Mbps" = "4 Mbps", "2 Mbps" = "2 Mbps", "1.5 Mbps" = "1.5 Mbps", "1 Mbps" = "1 Mbps", "512 Kbps" = "512 Kbps")

ggplot() +
  geom_sf(data = usa, color = "#2b2b2b", fill = "white", size = 0.12) +
  geom_sf(data = rus_infrastructure, aes(fill = DL_SPEED1), lwd = 0) +
  coord_sf(xlim = fifty_states$long, ylim = fifty_states$lat, expand = FALSE, datum = NA) +
  labs(title = "RUS borrowers by download speed", fill = "Download speed", subtitle = "Note: Data shown only for contiguous US states.") +
  theme(legend.position = "bottom", panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))  +
  scale_fill_viridis_d(option = "viridis")