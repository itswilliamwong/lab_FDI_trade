#################################
#### Lab FDI and Trade Data
#### May 25, 2026 
#### William Wong
#### Version 1.2 



# clear environment
rm(list = ls())


# new packages we need for Census and BEA

#install.packages("censusapi")
library(censusapi)
#install.packages('bea.R')
library(bea.R)

library(fredr)
library(tidyverse)


## Part 1. Basic Inward/Outward FDI data ----

#FRED has some data that it sources from Federal Reserve Board

#Flow of Inward FDI
#Rest of the World; Foreign Direct Investment in U.S.; Asset (Current Cost),
#Transactions (ROWFDIQ027S)
inward_flow<-fredr(series_id = "ROWFDIQ027S")
summary(inward_flow)
inward_flow%>%ggplot(aes(x=date,y=value))+geom_line()

#Stock of Inward FDI
#Rest of the World; Foreign Direct Investment in U.S.; Asset (Current Cost), Level (ROWFDNQ027S)
inward_stock<-fredr(series_id = "ROWFDNQ027S")
summary(inward_stock)
inward_stock%>%ggplot(aes(x = date, y = value))+geom_line()

#But wait, what is this other series
#Rest of the World; Foreign Direct Investment in U.S.: Equity; Asset (Market Value), Level (BOGZ1FL263092141Q)
inward_stock2<-fredr(series_id = "BOGZ1FL263092141Q")
summary(inward_stock2)
plot2<-inward_stock2%>%ggplot(aes(x = date, y = value))+geom_line()
plot2

#why are these different???
plot2+geom_line(data=inward_stock,aes(x = date, y = value),color="blue")

#Note that FRED also has other FDI measures in flows and stock at 
#market value and historical cost, but we are going to dig into the BEA data now


## Part 2 BEA data on FDI ----

#I have my BEA key stored in my .Renviron file
#this command tells R to go get it.
#some of the other packages we have used check for this automatically, e.g. fredr()
beaKey<-Sys.getenv("beaKey")

#Get a key here: https://apps.bea.gov/API/signup/
#Guide is here: https://apps.bea.gov/API/bea_web_service_api_user_guide.htm

#We want to get something like the data on this news release
# https://www.bea.gov/news/2023/direct-investment-country-and-industry-2022

#this gives a list
params<-beaParams(beaKey,'ITA')
#we can make it a dataframe by asking for hwere the params are stored
params<-beaParams(beaKey,'ITA')$Parameter

#what are the values of the different indicators
indicators<-beaParamVals(beaKey,'ITA',"Indicator")
indicators<-indicators$ParamValue
#we might want 
# DiInvInwardDirectionalBasis: Financial transactions for inward direct investment (foreign d

# and

#DiInvOutward: Financial transactions for outward direct investment (U.S. direc

outward<-list('UserID' = beaKey,
              'Method' = 'GetData',
              'DatasetName' = 'ITA',
              'Indicator'='DiInvOutward',
              'AreaOrCountry' = 'All',
              'Year' = '2020,2021,2022',
              'Frequency' = 'A',
              'ResultFormat' = 'xml')

outward<-beaGet(outward,asWide=FALSE)

# one would then think you could get country data

ctry_params<-beaParamVals(beaKey,'ITA',"AreaOrCountry")

ctry_params<-ctry_params$ParamValue

outwardctry<-list('UserID' = beaKey,
                  'Method' = 'GetData',
                  'DatasetName' = 'ITA',
                  'Indicator'='DiInvOutward',
                  'AreaOrCountry' = 'Australia',
                  'Year' = '2022',
                  'Frequency' = 'A',
                  'ResultFormat' = 'xml')

# you cannot get the country data this way
outwardctry<-beaGet(outwardctry,asWide=FALSE)



# where is the country data
# it's in yet another table

#Take a look at all the possible param values for MNE table#

beaParams(beaKey,'MNE')
beaParamVals(beaKey,'MNE',"DirectionOfInvestment")
series<-beaParamVals(beaKey,'MNE',"SeriesID")
series<-series$ParamValue
beaParamVals(beaKey,'MNE',"Classification")
beaParamVals(beaKey,'MNE',"Country")
beaParamVals(beaKey,'MNE',"Industry")

# this will give a list of each indicator but stored as a list

#this gives us a list with the values inside the list
ctrylist<-beaParamVals(beaKey,'MNE',"Country")

#we can extract the paramvalue list as follows

#overwrite ctrylist with the DF stored in the list
ctrylist<-ctrylist$ParamValue



#I am going to store this list to use it again below
ctrylist<-ctrylist%>%rename(ccode = key)
summary(ctrylist)
head(ctrylist)


# filter on regional codes to check#
descriptions <- ctrylist %>%
  filter(substr(ccode, 2, 2) == "9") %>%
  select(desc)

# There are a bunch of regional codes

# 299 Latin Am & other w. hemisphere
# 399 Europe
# 499 Africa
# 599 Middle East
# 699 Asia-Pac
# 100 Canada

# We want to recreate this data
#https://www.bea.gov/news/2023/direct-investment-country-and-industry-2022

#we are going to pass this over to beaGet()
# we want inward investment
# we want the regional codes plus Canada from the new release at link above
# We want seriesID=22
# 22
#Foreign Direct Investment Position in the United States on a Historical-Cost
#Basis - The foreign direct investment position in the United States is the value 
#of foreign direct investors' equity in their U.S. affiliates plus the value of
#net outstanding loans to their U.S. affiliates. It may be viewed as the direct
#investors' net financial claims on their affiliates.

replication_DI<-list('UserID' = beaKey,
                     'Method' = 'GetData',
                     'DatasetName' = 'MNE',
                     'DirectionOfInvestment'='Inward',
                     'Country' = '299,399,499,599,699,100',
                     'Year' = '2021,2022',
                     'SeriesID' = 22,
                     'Classification'='Country',
                     'ResultFormat' = 'xml')


inwardFDI<-(beaGet(replication_DI,asWide=FALSE))
head(inwardFDI)
# Rename Row to Country (it's a region, but country works)
inwardFDI<-inwardFDI%>%rename(Country = Row)

inwardFDI$DataValue<-inwardFDI$DataValue/1000

#plot to replicate BEA release
ggplot(data = inwardFDI, aes(x = reorder(Country, DataValue), y = DataValue, fill = factor(Year))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +  # Flip coordinates to make it horizontal
  labs(x = "Country", y = "U.S. Inward FDI (billions of dollars)", title = "U.S.Inward FDI by Region", fill="Year") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")  #this just makes colors nice


#replicate but with values at end of bars

#plot to replicate BEA release
ggplot(data = inwardFDI, aes(x = reorder(Country, DataValue), y = DataValue, fill = factor(Year))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(DataValue, 1), y=DataValue+175), position = position_dodge(width = 0.9)) +  #Add text labels
  #above you need to use y=DataValue since we flip coordinates below, +175 is just about right, depends on your scale though
  coord_flip() +  # Flip coordinates to make it horizontal
  labs(x = "Country", y = "U.S. Inward FDI (billions of dollars)", title = "U.S. Intward FDI by Region", fill="Year") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")  #this just makes colors nice


# Now try to get all inward FDI top and rank by country#
allinward<-list('UserID' = beaKey,
                'Method' = 'GetData',
                'DatasetName' = 'MNE',
                'DirectionOfInvestment'='Inward',
                'Country' = 'all',
                'Year' = '2021,2022',
                'SeriesID' = 22,
                'Classification'='Country',
                'ResultFormat' = 'xml')
allinward<-(beaGet(allinward,asWide=FALSE))
# now filter out the regional codes#
country <- ctrylist %>%
  filter(substr(ccode, 2, 2) != "9") %>%
  select(desc)

country<-country%>%rename(Country=desc)

allinward<-inner_join(country, allinward,join_by(Country==Row)) 
# convert values to billions like BEA release
inwardFDI$DataValue <- inwardFDI$DataValue / 1000

# Note that avbove we renamed Row to Country (it's a region, but country works)
#inwardFDI<-inwardFDI%>%rename(Country = Row)



#plot top 10 countries
#make a top 10 list using order()
# here we order the column in descending value, the second term in brackets only keeps the 
# top 10
allinward$DataValue <- allinward$DataValue / 1000

#this is one way to do it, we'll use another for census trade data#
top_countries <- allinward%>%filter(Year==2022)
top_countries <- top_countries[order(-top_countries$DataValue),][1:10,]

ggplot(top_countries, aes(x = reorder(Country, DataValue), y = DataValue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  #makes the bar chart horizontal and orders from highest to lowest
  labs(x = "Country", y = "Total Inward FDI", title = "Top 10 Source Countries by Inward FDI") +
  theme_minimal()  # Adds a minimal theme to make it cleaner

## Part 3: Census Trade Data ----

# YOU MUST SPEND SOME TIME READING MANUAL HERE #

#Trade data details https://www.census.gov/data/developers/data-sets/international-trade.html


## Part 3: Census Trade Data - U.S. Exports ----

# Assignment task:
# Use Census export data to get the top 10 destinations for U.S. exports
# for 2015 and 2025, then save a graph.

# Trade data details:
# https://www.census.gov/data/developers/data-sets/international-trade.html

# Pull annual U.S. export data by country.
# ALL_VAL_YR is cumulative exports by year.
# MONTH = "12" gives the full annual total.
exports_cty_yr <- getCensus(
  name = "timeseries/intltrade/exports/naics",
  vars = c("ALL_VAL_YR", "YEAR", "CTY_CODE", "CTY_NAME"),
  time = "from 2015",
  MONTH = "12",
  show_call = TRUE
)

head(exports_cty_yr)

# Filter out region and aggregation codes.
exports_cty_yr_clean <- exports_cty_yr %>%
  filter(!(substr(CTY_CODE, 1, 1) == "0" |
             substr(CTY_CODE, 2, 2) == "X" |
             substr(CTY_CODE, 1, 1) == "-"))

# Convert export values to billions of dollars and year to numeric.
exports_cty_yr_clean <- exports_cty_yr_clean %>%
  mutate(
    ALL_VAL_YR = as.numeric(ALL_VAL_YR) / 1000000000,
    YEAR = as.numeric(YEAR)
  )

# Keep only the required assignment years.
exports_2015_2025 <- exports_cty_yr_clean %>%
  filter(YEAR %in% c(2015, 2025))

# Get the top 10 U.S. export destinations for each year.
top10_exports <- exports_2015_2025 %>%
  group_by(YEAR) %>%
  slice_max(order_by = ALL_VAL_YR, n = 10, with_ties = FALSE) %>%
  arrange(YEAR, desc(ALL_VAL_YR))

print(top10_exports)

# Add rank within each year.
top10_exports <- top10_exports %>%
  group_by(YEAR) %>%
  arrange(desc(ALL_VAL_YR), CTY_NAME) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# Make graph for 2015 and 2025.
top10_exports_plot <- ggplot(top10_exports, aes(group = CTY_NAME, y = rank)) +
  geom_tile(
    aes(
      x = ALL_VAL_YR / 2,
      width = ALL_VAL_YR,
      height = 0.5,
      color = CTY_NAME,
      fill = CTY_NAME
    ),
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = ALL_VAL_YR, y = rank, label = CTY_NAME),
    nudge_x = 25,
    show.legend = FALSE
  ) +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL) +
  facet_wrap(~ YEAR) +
  labs(
    x = "Export Value (billions USD)",
    y = "Ranking by Exports",
    title = "Top 10 Destinations for U.S. Exports: 2015 and 2025"
  ) +
  theme_minimal()

top10_exports_plot

# Save graph to your repo folder.
ggsave(
  filename = "top10_us_exports_2015_2025.png",
  plot = top10_exports_plot,
  width = 12,
  height = 7,
  dpi = 300
)






## Part 5: Animated plot by year - U.S. Exports ----

# Install these once if needed:
# install.packages("gifski")
# install.packages("gganimate")

library(gifski)
library(gganimate)

top10_exports_all_years <- exports_cty_yr_clean %>%
  group_by(YEAR) %>%
  slice_max(order_by = ALL_VAL_YR, n = 10, with_ties = FALSE) %>%
  arrange(YEAR, desc(ALL_VAL_YR)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

p <- ggplot(top10_exports_all_years, aes(group = CTY_NAME, y = rank)) +
  geom_tile(
    aes(
      x = ALL_VAL_YR / 2,
      width = ALL_VAL_YR,
      height = 0.5,
      color = CTY_NAME,
      fill = CTY_NAME
    ),
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = ALL_VAL_YR, y = rank, label = CTY_NAME),
    nudge_x = 25,
    show.legend = FALSE
  ) +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL) +
  labs(
    x = "Export Value (billions USD)",
    y = "Ranking by Exports",
    title = "Top 10 Destinations for U.S. Exports: Year {closest_state}"
  ) +
  theme_minimal()

animated_plot <- p +
  transition_states(YEAR, transition_length = 2, state_length = 2, wrap = FALSE) +
  ease_aes("linear")

anim <- animate(
  animated_plot,
  nframes = 100,
  fps = 10,
  width = 800,
  height = 600,
  start_pause = 10,
  end_pause = 10,
  renderer = gifski_renderer()
)

anim_save("top10_exports_over_time.gif", animation = anim)