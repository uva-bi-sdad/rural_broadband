library(sf)

sp <- st_read("data/rus_broadband_ineligibility/RD_BROADBAND_IELG_PRODUCTION.shp", stringsAsFactors = FALSE)

plot(sp)

us_states <- st_read("data/census_geo_cb/cb_2016_us_state_20m.shp", stringsAsFactors = FALSE)
va <- us_states[us_states$STATEFP=="51",]
plot(va)

d <- st_crs(va)
sp2 <- st_transform(sp, d)

jn <- st_join(sp2, va, join = st_intersects)

plot(jn)
