library(tidyverse)
library(nycflights13)
library(maps)
library(fueleconomy)
library(forcats)

##############################################################
# 13.4.6 #1-4
##############################################################

#1
# Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. Here’s an easy way to draw a map of the United States:
#   
#   airports %>%
#   semi_join(flights, c("faa" = "dest")) %>%
#   ggplot(aes(lon, lat)) +
#   borders("state") +
#   geom_point() +
#   coord_quickmap()
# (Don’t worry if you don’t understand what semi_join() does — you’ll learn about it next.)
# 
# You might want to use the size or colour of the points to display the average delay for each airport.

averageDelay <- flights %>% group_by(dest) %>% summarise(avgDelay = mean(arr_delay,na.rm = TRUE))
delayAirports <- inner_join(averageDelay,airports, c("dest" = "faa"))
delayAirports %>%
  semi_join(flights, c("dest" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point(aes(colour = avgDelay)) +
  coord_quickmap() 

#2
#Add the location of the origin and destination (i.e. the lat and lon) to flights.
relevantColumns<- airports %>% select(faa,lat,lon)
addingDest <- flights %>% inner_join(relevantColumns,c("dest" = "faa")) %>% select(year:dest,dest_lat = "lat", dest_lon = "lon",air_time:time_hour)
allTogetherNow <- addingDest %>% inner_join(relevantColumns,c("origin" = "faa")) %>% select(year:origin,origin_lat = "lat", origin_lon = "lon",dest:time_hour)

#3
#Is there a relationship between the age of a plane and its delays?
planes_flights <- inner_join(flights,planes,by = "tailnum")
ageCalc <- planes_flights %>% mutate(age = year.x - year.y)
ageDelay <- ageCalc %>% group_by(age) %>% summarise(avgDelay = mean(arr_delay+dep_delay,na.rm = TRUE))
ageDelay %>% ggplot(aes(age,avgDelay)) + geom_line()
#No
  
#4
#What weather conditions make it more likely to see a delay?
flights_weather <- inner_join(flights,weather,c("year","month","day","hour","origin"))
precipitation_delay <- flights_weather %>% group_by(precip) %>% summarise(delay = mean(arr_delay,na.rm = TRUE))
ggplot(precipitation_delay,aes(precip,delay)) + geom_line()
#It looks like any amount of precipitation causes a delay, but there's not really a strong trend of how much precipitation causes a big delay


##############################################################
# 13.5.1 #1-6
##############################################################
#1
#What does it mean for a flight to have a missing tailnum? 
#What do the tail numbers that don’t have a matching record in planes have in common? (Hint: one variable explains ~90% of the problems.)
flights_wo_match <- flights %>% anti_join(planes,by = "tailnum")
flights_wo_match %>% group_by(carrier) %>% count(carrier,sort = TRUE)
#It's only two carriers that are the majority of the cases - AA and MQ


#2 
#Filter flights to only show flights with planes that have flown at least 100 flights.
flights100 <- flights %>% group_by(tailnum) %>% count()  %>% filter(n >= 100)
semi_join(flights,flights100,by = "tailnum")

#3
#Combine fueleconomy::vehicles and fueleconomy::common to find only the records for the most common models.
head(fueleconomy::vehicles)
head(fueleconomy::common)
common_models <- semi_join(fueleconomy::common, fueleconomy::vehicles, c("make","model"))
common_models

#4
#Find the 48 hours (over the course of the whole year) that have the worst delays.
#Cross-reference it with the weather data. Can you see any patterns?
worstDelays <- flights %>% group_by(month,day,hour) %>% summarise(avg_delay = mean(arr_delay,na.rm = TRUE)) %>% arrange(desc(avg_delay))
worstDelays48 <- worstDelays[1:48,]
delays_weather <- flights %>% inner_join(worstDelays,c("month","day","hour")) %>% left_join(weather,c("month","day","hour","year","origin"))
delays_weather %>% group_by(precip) %>% summarise(total_delay = sum(avg_delay)) %>% ggplot(aes(precip,total_delay)) + geom_line()
#It looks like there's a higher total delay when there's only a bit of precipitation, which is odd
delays_weather %>% group_by(temp) %>% summarise(total_delay = sum(avg_delay)) %>% ggplot(aes(temp,total_delay)) + geom_line()
#Temperature doesn't look like there's really a pattern

#5
#What does anti_join(flights, airports, by = c("dest" = "faa")) tell you?
anti_join(flights, airports, by = c("dest" = "faa"))
#It shows only the flights that have a destination that isn't listed in the airports data

#What does anti_join(airports, flights, by = c("faa" = "dest")) tell you?
anti_join(airports, flights, by = c("faa" = "dest"))
#It shows the airports that flights didn't fly to in 2013

#6
#You might expect that there’s an implicit relationship between plane and airline, because each plane is flown by a single airline.
#Confirm or reject this hypothesis using the tools you’ve learned above.
names(flights)
flights %>%select(tailnum,carrier) %>%distinct(tailnum,carrier) %>%   group_by(tailnum) %>% count() %>% filter(n > 1)
#There are several tailnums with multiple carriers - probably because planes can be sold between carriers

##############################################################
# 15.3.1 #1-3
##############################################################
#1
#Explore the distribution of rincome (reported income). What makes the default bar chart hard to understand? How could you improve the plot?
gss_cat %>% count(rincome)
ggplot(gss_cat,aes(rincome)) + geom_bar()
#The labels overlap - it could be fixed by switching the axes or making the labels vertical instead of horizontal

#2
#What is the most common relig in this survey? What’s the most common partyid?
gss_cat %>% count(relig,sort=TRUE)
#Protestant is the most common religion
gss_cat %>% count(partyid,sort=TRUE)
#Independent is the most common partyid

#3
#Which relig does denom (denomination) apply to? How can you find out with a table? How can you find out with a visualisation?
gss_cat %>% group_by(relig) %>% count(denom,sort = TRUE)
#It applies to Protestant. See above for figuring out with a table
gss_cat %>% count(relig,denom) %>% ggplot(aes(x=relig,y=denom,size=n)) + geom_point()

##############################################################
# 15.4.1 #1-3
##############################################################
#1
#There are some suspiciously high numbers in tvhours. Is the mean a good summary?
summary(gss_cat["tvhours"])
gss_cat %>% 
  ggplot() +
  geom_density(aes(tvhours))
#I think it could be - the mean appears to fall right in the middle of the densest part of the plot and it is close to the median

#2
#For each factor in gss_cat identify whether the order of the levels is arbitrary or principled.
head(gss_cat)
#So marital, race, rincome, partyid, relig, and denom are factors
levels(gss_cat$marital) #Seems arbitrary to me - there's no quantitative way to order these
levels(gss_cat$race) #Arbitrary
levels(gss_cat$rincome) #Principled
levels(gss_cat$partyid) #Principled - ordered from right to left on the political spectrum
levels(gss_cat$relig) #Arbitrary - if it was principled, the different types of Christian would be near each other
levels(gss_cat$denom) #Principled - denominations with similar names are near each other

#3
#Why did moving “Not applicable” to the front of the levels move it to the bottom of the plot?
#It gives "NA" the value of 1
