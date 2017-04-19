## Heat Mapping Problem
# Question: Where are the nonpartisan / independent Ohio voters who voted in the 2016 election located?
# Note the same concepts apply to park visitors provided that I have billing addresses

# packages
library(devtools)
# ggmap has a couple bug problems with ggplot2 - need to use this version of ggmap
devtools::install_github("dkahle/ggmap")
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)

# First - I got the data from here: https://www6.sos.state.oh.us/ords/f?p=111:1:0::NO:RP:P1_TYPE:STATE
df1 <- read.table("SWVF_1_44.txt", header = TRUE, sep = ",")
df2 <- read.table('SWVF_45_88.txt', header = TRUE, sep = ",")
voter_data <- rbind(df1, df2)
Counties <- read_excel("Ohio_Counties.xlsx")

## Data Wrangling. Follow closely. Going to pull out those independent / nonpartisan voters.

voter_data2 <- voter_data %>% select(c(1:2, 10:16, 18, 96, 99))

# For whatever reason - county names are not listed. I need the names for later.
voter_data3 <- voter_data2 %>% left_join(Counties, by = "COUNTY_NUMBER") 

# Back to wrangling
voter_data3$helper <- voter_data3$PRIMARY.03.15.2016 == "D" | voter_data3$PRIMARY.03.15.2016 == "G" | voter_data3$PRIMARY.03.15.2016 == "R" | voter_data3$PRIMARY.03.15.2016 == "C"

voter_data4 <- voter_data3 %>% filter(GENERAL.11.08.2016 == "X") %>%
                              filter(helper == FALSE)

# Let's check the numbers
voter_data4 %>% group_by(PARTY_AFFILIATION) %>% summarise(n_distinct(SOS_VOTERID))

# Now I have all the voter who did not vote in the 2016 primary BUT did vote in the general

# Note: I could do a further in-depth look at new voters or inactive voters who turned out for Trump in the 2016 primary - 
# but that is not necessary for this example.

# Now let's get rid of all the partisans who simply didn't vote in the primary

voter_data4$helper2 <- voter_data4$PARTY_AFFILIATION == "C" | 
                        voter_data4$PARTY_AFFILIATION == "D" | 
                        voter_data4$PARTY_AFFILIATION == "G" |
                        voter_data4$PARTY_AFFILIATION == "L" |
                        voter_data4$PARTY_AFFILIATION == "R"

voter_data5 <- voter_data4 %>% filter(helper2 == FALSE)

# Let's do another count check
voter_data5 %>% group_by(PARTY_AFFILIATION) %>% summarise(n_distinct(SOS_VOTERID))

## Now I have the correct population of voters. Let's map 'em!

# We are going to make a couple of maps.
# First - A map of total of our subset populations
# Second - A map of our subset as a percentage of all voters who voted in the general
# Third - A heat map using the addresses --> lat / lon

data1 <- voter_data5 %>% group_by(County) %>% summarise(n_distinct(SOS_VOTERID))

names(data1) <- c("county", "Voters")
data1$county <- tolower(data1$county)

map <- map_data("county")
map <- map %>% filter(region == "ohio")
map <- map %>% select(long, lat, subregion)
names(map) <- c("long", "lat", "id")

k <- ggplot(data1, aes(fill = Voters))
p <- k + geom_map(aes(map_id = county), map = map) + 
                  expand_limits(x = map$long, y = map$lat)
p

# Nifty. Obviously the high dense areas are Franklin (Columbus), Cuyahoga (Cleveland) & Hamilton (Cincinnati)
# Let's find out proportions

voter_data3.5 <- voter_data3 %>% filter(GENERAL.11.08.2016 == "X")
Votes_2016_General <- voter_data3.5 %>% group_by(County) %>%
                          summarise(n_distinct(SOS_VOTERID))
Votes_2016_General$County <- tolower(Votes_2016_General$County)
names(Votes_2016_General) <- c("county", "All_Votes")
data2 <- data1 %>% left_join(Votes_2016_General, by = "county") %>%
                      mutate(Percent_Indep = Voters / All_Votes) %>%
                      select(county, Percent_Indep)

k2 <- ggplot(data2, aes(fill = Percent_Indep))
p2 <- k2 + geom_map(aes(map_id = county), map = map) + 
  expand_limits(x = map$long, y = map$lat)
p2

# Tells a different Story. Again, nifty. Each county probably has a seperate rationale.
# But if the job is to target specific group of independent voters within certain markets; 
# I think Columbus, Cincinnati and Toledo would be ideal media markets
# This does not answer WHO. Only WHERE.

# So - now for the fun stuff. a heat density map! 
# This is complicated so follow along closely; usually there is software that can do this
# but since most of that costs $$ I'll have to do it manually with R code!

# This will be broken down into 2 main steps. 
# First - need to geocode my addresses (get latitudes and longitudes) using the Google Maps API
# Second - we will then map these lat/long into a heat density map using ggplot2 & ggmap

voter_data5$FULL_ADDRESS <- paste(voter_data5$RESIDENTIAL_ADDRESS1, voter_data5$RESIDENTIAL_CITY, voter_data5$RESIDENTIAL_STATE, sep = ", ")

# Note without a paid subscription; I can only geocode 2,500 address per day, so let's look into a small town called Buckeye Lake
voter_data6 <- voter_data5 %>% filter(RESIDENTIAL_CITY == "BUCKEYE LAKE")

geocodes <- geocode(voter_data6$FULL_ADDRESS)

Buckeye_Lake_Map <- get_map(location = "Buckeye Lake", maptype = "roadmap", zoom = 14)

ggmap(Buckeye_Lake_Map) + geom_point(aes(x = lon, y = lat), 
                                                        colour = "red",
                                                        alpha = 0.1,
                                                        size = 2,
                                                        data = geocodes)

# Nice! Notice the neighborhoods that don't have dots. 
# It is important to note that 

ggmap(Buckeye_Lake_Map) + geom_density2d(data = geocodes, 
                                        aes(x = lon, y = lat), size = 0.3) + 
                          stat_density2d(data = geocodes, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                                        bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
                          scale_alpha(range = c(0, 0.3), guide = FALSE)

# Sweet! That's the good stuff right there.
# Whelp that concludes this project.


