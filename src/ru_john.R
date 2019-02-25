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


