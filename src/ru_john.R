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
       rus_approved$PCT_HISPEE, rus_approved$DL_SPEED, na.rm = FALSE)

ggplot(data = rus_approved) +
  geom_sf(aes(fill = SUM_HOUSEH), lwd = 0) +
  coord_sf(xlim = c(-15000000, -7000000), ylim = c(2500000, 7000000), expand = FALSE)

