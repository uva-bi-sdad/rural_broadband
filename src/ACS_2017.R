# Get 2013-2017 5-year ACS estimates on broadband availability
# Compare ACS to FCC data; is ACS a useful surrogate at the block group level?

library(rgeos)
library(ggplot2)
library(tigris)
library(dplyr)
library(tidycensus)
library(xml2)
library(acs)
library(viridis)
library(rgdal)

options(scipen = 999)
census_api_key("853b2a1e71aa0aff0f3db966545e8898c73f0772")
state_fips <- unique(fips_codes$state)[1:51]

# start by making these three maps:
# (q8) no computer? B28003_006 / B28003_001
# (q9) no internet subscription?  B28003_005 / B28003_001
# (q10) broadband subscription? B28003_004 / B28003_001

# more detailed internet breakdown:
# -%broadband (any including cell and satellite) B28002_004 / B28002_001
# -%cell data B28002_006 / B28002_001
# -%broadband (cable, fiberoptic, DSL)*** B28002_007 / B28002_001
# -%satellite B28002_009 / B28002_001
# -%dailup only B28002_003 / B28002_001
# -no access to internet?  B28002_13 / B28002_001




# get estimates for all census tracts from a state (loop over state_fips, and loop over variables to join)
acs_vars <- c("B28003_001","B28003_004","B28003_005","B28003_006")

acs_est <- get_acs(geography="tract",state=state_fips[1],endyear=2017,span=5,variables=acs_vars,cache_table=TRUE,output="wide")
for(i in 2:length(state_fips)){
  tmp <- get_acs(geography="tract",state=state_fips[i],endyear=2017,span=5,variables=acs_vars,cache_table=TRUE,output="wide")
  acs_est <- rbind(acs_est,tmp)
}


save.image("data/acs2017_tract.RData")

# To do: call by Census tract for now (note: Block Group data was on FactFinder but did not work with the Census API using acs.fetch)
# -include questions (old slides)
# -scatterplot of the ACS broadband subscription rate vs FCC subscription @10Mbps by Census tract (and report correlation)
# -three plots over U.S. by census tract
load("data/acs2017_tract.RData")

# join to FCC data by Census tract
tract_data <- read.csv("data/broadband_acs_by_census_tract_2015.csv")
table(tract_data$subscription_continuous)

acs_est2 <- acs_est %>% transmute(GEOID=GEOID,
                                  no_computer_acs=B28003_006E/B28003_001E,
                                  no_internet_acs=B28003_005E/B28003_001E,
                                  broadband_acs=B28003_004E/B28003_001E)
acs_est2$GEOID <- as.numeric(acs_est2$GEOID)
# (q8) no computer? B28003_006 / B28003_001
# (q9) no internet subscription?  B28003_005 / B28003_001
# (q10) broadband subscription? B28003_004 / B28003_001

tract_data2 <- tract_data %>% left_join(acs_est2,by="GEOID")


# also join subscription_continuous_200k from the FCC data
fcc_subscription <- read.csv("data/fcc_subscription/tract_map_dec_2015.csv")
tract_data3 <- tract_data2 %>% left_join(fcc_subscription %>% select(GEOID=tractcode,subscription_200k=pcat_all), by="GEOID")

# turn subscription quintiles into a continuous response variable by taking the center of each bin
tract_data3$subscription_continuous_200k <- pmax(0,tract_data3$subscription_200k/5-0.1)



png("output/broadband_subscription_rate.png",height=400,width=600)
hist(tract_data2$subscription_continuous,breaks = c(0,0.2,0.4,0.6,0.8,1.0),freq=FALSE,ylim=c(0,3),xlab="Broadband Subscription Rate",main="",xaxt="n")
lines(density(tract_data2$broadband_acs,na.rm=TRUE),col=2)
axis(1, at=c(0,0.2,0.4,0.6,0.8,1), labels=c("0%","20%","40%","60%","80%","100%"))
dev.off()

png("output/broadband_subscription_rate_200k.png",height=400,width=600)
hist(tract_data3$subscription_continuous_200k,breaks = c(0,0.2,0.4,0.6,0.8,1.0),freq=FALSE,ylim=c(0,3),xlab="Broadband Subscription Rate",main="",xaxt="n")
lines(density(tract_data2$broadband_acs,na.rm=TRUE),col=2)
axis(1, at=c(0,0.2,0.4,0.6,0.8,1), labels=c("0%","20%","40%","60%","80%","100%"))
dev.off()


# boxplot showing association between variables
sum(tract_data2$broadband_acs==0,na.rm=T) # only 19 observations; remove
sum(tract_data2$broadband_acs<=0.2,na.rm=T) # 63 observations

png("output/acs_vs_FCC_subscription.png",height=500,width=700)
boxplot(100*broadband_acs~subscription_continuous,data=(tract_data2 %>% filter(subscription_continuous>0)),
        xlab="FCC Broadband Subscription above 10 Mbps", ylab="ACS Q10 %Broadband Subscription",
        xaxt="n")
axis(1, at=1:5, labels=c("<20%","20-40%","40-60%","60-80%",">80%"))
dev.off()

png("output/acs_vs_FCC_subscription_200k.png",height=500,width=700)
boxplot(100*broadband_acs~subscription_continuous_200k,data=(tract_data3 %>% filter(subscription_continuous_200k>0)),
        xlab="FCC Broadband Subscription above 200 kbps", ylab="ACS Q10 %Broadband Subscription",
        xaxt="n")
axis(1, at=1:5, labels=c("<20%","20-40%","40-60%","60-80%",">80%"))
dev.off()



cor(tract_data2$broadband_acs, tract_data2$subscription_continuous,use="complete.obs")
# pearson correlation = 0.6373016

cor(tract_data3$broadband_acs, tract_data3$subscription_continuous_200k,use="complete.obs")
# pearson correlation = 0.7110025


# -------------------------------------------------
# plot ACS broadband by Census Tract for VA
# -------------------------------------------------

#VA_tracts <- tracts(state="VA",year=2016) # error: cannot open data source
load("data/VA_tract_shapefile.RData")
# plot(VA_tracts)

tract_df <- fortify(VA_tracts,region="GEOID")
tract_df$id <- as.numeric(tract_df$id)
tract_df2 <- tract_df %>% left_join(acs_est2, by=c("id"="GEOID"))

pdf("output/acs_broadband_tract.pdf",width=6,height=4)
ggplot() +
  geom_polygon(data=tract_df2, aes(x=long, y=lat, group=group, fill=broadband_acs), color="grey70", size=0.3) +
  theme_void() +
  coord_map() +
  scale_fill_viridis_c("Percent of Households with Broadband Access") +
  theme(legend.position="bottom") +
  ggtitle("Broadband Accessibility by Public Use Microdata Area (PUMA)") + labs(subtitle="Source: 2013-2017 American Community Survey, 5-Year Estimates")
dev.off()

# -------------------------------------------------
# TESTING CENSUS API FOR 2017 DATA (doesn't work for block group)
# -------------------------------------------------

# B28002 PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD
# B28003 PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28011 INTERNET SUBSCRIPTIONS IN HOUSEHOLD
#View( acs.lookup(endyear=2017, span = 5, dataset = "acs", table.name = "population", case.sensitive=F)@results )
View( acs.lookup(endyear=2017, span = 5, dataset = "acs", table.name = "subscription", case.sensitive=F)@results )
View( acs.lookup(endyear=2017, span = 5, dataset = "acs", table.name = "broadband", case.sensitive=F)@results )
View( acs.lookup(endyear=2017, span = 5, dataset = "acs", table.name = "access", case.sensitive=F)@results )
# Internet Access Variables are not available on acs.lookup...get_acs is NA

# In many cases, acs.lookup is called internally by acs.fetch, to determine the variable codes to use for a given
# table.name or table.number. Since each lookup involves a search of static XML tables (provided by the census for each
# endyear/span combination, and included by the acs package in /extdata), searches involving more recent years
# (e.g., for version 2.0, endyears > 2014) may fail. In such situations, users may wish to call acs.fetch with the
# "variable=" option, perhaps reusing variables from a saved acs.lookup search for a previous year.
test <- acs.fetch(endyear=2017,span=5,geography=geo.make(state="AL",county="Autauga",tract="*"),variable="B28002_001",
                  key="853b2a1e71aa0aff0f3db966545e8898c73f0772")
test
# works at Census tract level

acs.fetch(endyear=2017,span=5,geography=geo.make(state="AL",county="Autauga",tract="*",block.group="*"),variable="B28002_001",
          key="853b2a1e71aa0aff0f3db966545e8898c73f0772")
# fails at block group level, although block group level is available on FactFinder

acs.fetch(endyear=2017,span=5,geography=geo.make(state="AL",county="Autauga",tract="*",block.group="*"),variable="B01001_001",
          key="853b2a1e71aa0aff0f3db966545e8898c73f0772")
# population works for block group...





test <- acs.fetch(endyear=2017,span=5,geography=geo.make(state=state_fips[[1]],county=geo.lookup(state=state_fips[[1]],county="*")$county.name[-1],tract="*"),
                  variable=c(acs_vars[1]), key="853b2a1e71aa0aff0f3db966545e8898c73f0772")
dat <- data.frame(tract=test@geography$tract,test@estimate)
for(i in 2:length(state_fips)){
  test <- acs.fetch(endyear=2017,span=5,geography=geo.make(state=state_fips[[1]],county=geo.lookup(state=state_fips[[1]],county="*")$county.name[-1],tract="*"),
                    variable=acs_vars[1], key="853b2a1e71aa0aff0f3db966545e8898c73f0772")
  tmp <- data.frame(tract=test@geography$tract,test@estimate)
  dat <- rbind(dat,tmp)
}
acs_est <- dat

for(j in 2:length(acs_vars)){
  test <- acs.fetch(endyear=2017,span=5,geography=geo.make(state=state_fips[[1]],county=geo.lookup(state=state_fips[[1]],county="*")$county.name[-1],tract="*"),
                    variable=c(acs_vars[j]), key="853b2a1e71aa0aff0f3db966545e8898c73f0772")
  dat <- data.frame(tract=test@geography$tract,test@estimate)
  for(i in 2:length(state_fips)){
    test <- acs.fetch(endyear=2017,span=5,geography=geo.make(state=state_fips[[1]],county=geo.lookup(state=state_fips[[1]],county="*")$county.name[-1],tract="*"),
                      variable=acs_vars[j], key="853b2a1e71aa0aff0f3db966545e8898c73f0772")
    tmp <- data.frame(tract=test@geography$tract,test@estimate)
    dat <- rbind(dat,tmp)
  }
  acs_est <- cbind(acs_est,dat[,2])
}


