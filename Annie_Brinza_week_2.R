
#########################################################################
#
# Chapter 4
#
########################################################################
#4.4.1
#Why does this code not work?
  my_variable <- 10
  my_varıable
#It doesn't work because the i isn't actually an i
  
#4.4.2
#Tweak each of the following R commands so that they run correctly:
    
library(tidyverse)
  
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
  
filter(mpg, cyl == 8)
filter(diamonds, carat > 3)


#4.4.3
#Press Alt + Shift + K. What happens? How can you get to the same place using the menus?

#A translucent image pops up with all the RStudio shortcuts.
#Tools -> Keyboard Shortcuts Help


#########################################################################
#
# Chapter 5.2
#
########################################################################

#5.2.4.1
# Find all flights that:
# 
# 1. Had an arrival delay of two or more hours
# 2. Flew to Houston (IAH or HOU)
# 3. Were operated by United, American, or Delta
# 4. Departed in summer (July, August, and September)
# 5. Arrived more than two hours late, but didn’t leave late
# 6. Were delayed by at least an hour, but made up over 30 minutes in flight
# 7. Departed between midnight and 6am (inclusive)

delayed <- filter(flights, arr_delay >= 120)
houston <- filter(flights, dest %in% c('IAH','HOU'))
operated <- filter(flights, carrier %in% c('UA', 'AA','DL'))
summerDepartures <- filter(flights, month %in% c(7,8,9))
delayedArrival <- filter(flights, dep_delay <= 0 & arr_delay > 120)
delayedCaughtUp <- filter(flights, dep_delay >= 60 & arr_delay < (dep_delay - 30))
redeyes <- filter(flights, dep_time >= 000 & dep_time <= 600)

#5.2.4.2
#Another useful dplyr filtering helper is between(). 
#What does it do? 
#It determines if values in a vector fall into a certain range

#Can you use it to simplify the code needed to answer the previous challenges?
#Yes
#Example:
redeyesSimpler <- filter(flights,between(dep_time,000,600))

#5.2.4.3
#How many flights have a missing dep_time?
missingDepTime <- filter(flights, is.na(dep_time))
totalMissingFlights <- nrow(missingDepTime)
#Total flights with no departure time: 8255

#What other variables are missing? 
#dep_delay, arr_time, arr_delay, air_time, 

#What might these rows represent?
#Cancelled flights

#5.2.4.4
#Why is NA ^ 0 not missing? 
#Anything to the 0th power is 1.

#Why is NA | TRUE not missing? 
#Since an expression evaluated using "|" (or OR) returns true if any part of it is true, this expression returns TRUE

#Why is FALSE & NA not missing? 
#Since an expression evaluated using "&" (or AND) returns FALSE if any part of it is false, this expression returns false.

#Can you figure out the general rule? (NA * 0 is a tricky counterexample!)
#NA * 0 != 0 because NA * infinity would result in NAN rather than NA. 
#In general, a logical operator used in conjunction with NA will only return NA if the value of NA matters


#########################################################################
#
# Chapter 5.3
#
########################################################################
#5.3.1.1
#How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
df <- tibble(x = c(5, 2, NA))
arrange(df, -is.na(x))

#5.3.1.2
#Sort flights to find the most delayed flights. Find the flights that left earliest.
mostDelayedFlights <- arrange(flights,-dep_delay)
earliestFlights <-  arrange(flights,dep_delay)

#5.3.1.3
#Sort flights to find the fastest flights.
fastestFlights <- arrange(flights,arr_delay)


#5.3.1.4
#Which flights travelled the longest? Which travelled the shortest?
longestTravel <- arrange(flights,-distance)
shortestTravel <- arrange(flights,distance)


#########################################################################
#
# Chapter 5.4
#
########################################################################

#5.4.1.2
#What happens if you include the name of a variable multiple times in a select() call?
select(flights,dep_time,dep_time)
#It only shows up once

#5.4.1.3
#What does the one_of() function do? Why might it be helpful in conjunction with this vector?
?one_of()
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights,one_of(vars))
#It selects the columns whose names match the vector
#It's helpful when you want to repeat column names over and over to help reduce code duplication

#5.4.1.4
#Does the result of running the following code surprise you? How do the select helpers deal with case by default? How can you change that default?
select(flights, contains("TIME"))
#The default is set to ignoring the case, which isn't the usual for R.
select(flights,contains("TIME", ignore.case = FALSE))
