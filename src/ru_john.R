library(sf)
library(dplyr)
library(furniture)
library(naniar)
library(ggplot2)


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


#
# Basic descriptives ------------------------------------------------------------------------------------------
#

# rus_approved
gg_miss_var(rus_approved)
table1(rus_approved, rus_approved$STATUS, rus_approved$SERVICETYP, rus_approved$SERVICELEV, rus_approved$PROGRAMTYP, rus_approved$PROGRAMSER, rus_approved$SERVICEA_1,
       rus_approved$APPSTATUSS, rus_approved$SUM_HOUSEH, rus_approved$SUM_HOUSIN, rus_approved$SUM_AREA_L, rus_approved$UL_SPEED, rus_approved$TECHTYPE, rus_approved$SPEED_CODE,
       rus_approved$PCT_HISPEE, rus_approved$DL_SPEED, na.rm = FALSE, export = "rus_approved")

ggplot(data = rus_approved) +
  geom_sf(aes(fill = SERVICETYP), lwd = 0) +
  coord_sf(xlim = c(-15000000, -7000000), ylim = c(2500000, 7000000), expand = FALSE)

ggplot(data = rus_approved) +
  geom_sf(aes(fill = PROGRAMTYP), lwd = 0)  +
  coord_sf(ylim = c(2500000, 10000000), expand = FALSE)  + 
  labs(title = "Approved RUS applications by program", fill = "Program type")

ggplot(data = rus_approved) +
  geom_sf(aes(fill = PROGRAMTYP), lwd = 0) +
  coord_sf(xlim = c(-14200000, -7500000), ylim = c(2500000, 7100000), expand = FALSE) + 
  labs(title = "Approved RUS applications by program", fill = "Program type", subtitle = "Note: Data shown only for contiguous US states.") +
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))

# rus_infraborr
gg_miss_var(rus_infraborr)
table1(rus_infraborr, rus_infraborr$BORROWER, rus_infraborr$CITY, rus_infraborr$STATE, rus_infraborr$NTCA, rus_infraborr$SPEED_CODE, rus_infraborr$TECHTYPE,
       rus_infraborr$DL_SPEED, rus_infraborr$UL_SPEED, rus_infraborr$PCT_HISPEE, na.rm = FALSE, export = "rus_infraborr")

ggplot(data = rus_infraborr) +
  geom_sf(aes(fill = BORROWER), lwd = 0)  + 
  labs(title = "RUS borrower companies?", fill = "Borrower") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
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
  
ggplot(data = rus_infrastructure) +
  geom_sf(aes(fill = DL_SPEED1), lwd = 0)  + 
  coord_sf(xlim = c(-14200000, -7500000), ylim = c(2500000, 6800000), expand = FALSE) + 
  labs(title = "RUS borrowers by download speed", fill = "Download speed", subtitle = "Note: Data shown only for contiguous US states.") +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20)) +
        scale_fill_viridis_d(option = "viridis")
