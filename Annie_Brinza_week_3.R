#########################################################################
#
# Chapter 5.5
#
#########################################################################

#5.5.2.1
#Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they’re not really continuous numbers.
#Convert them to a more convenient representation of number of minutes since midnight.

timeSinceMidnight <- transmute(flights,dep_time, dep_time_minutes = (dep_time %/% 100) * 60 + dep_time %%100,sched_dep_time, sched_dep_time_minutes = (sched_dep_time %/% 100) * 60 + sched_dep_time %%100 )


#5.5.2.2
#Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it?
(airTimeComparison <- transmute(flights, air_time, arr_time - dep_time))
#They're not the same because arr_time - dep_time doesn't take in hours and minutes
airTimeComparisonTake2 <- transmute(flights, air_time,((arr_time %/% 100)*60 + (arr_time %% 100))- ((dep_time %/% 100)*60 + (dep_time %% 100)) )
#Still not right - need to account for time zone, which isn't in this data set.




#5.5.2.3
#Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?
transmute(flights, dep_time, sched_dep_time, dep_delay)
#I'd expect dep_time = sum(sched_dep_time,dep_delay) but with sched_dep_time converted to minutes and then the whole equation converted back
transmute(flights, dep_time, sched_dep_time, dep_delay,dep_time_check = (((sched_dep_time %/% 100)*60 + (sched_dep_time %% 100)) + dep_delay)*100 %/%60)



#5.5.2.4
#Find the 10 most delayed flights using a ranking function. How do you want to handle ties? Carefully read the documentation for min_rank().
mostDelayed <- arrange(flights,-dep_delay)
tenMostDelayed <- slice(mostDelayed,1:10)

#5.5.2.5
#What does 1:3 + 1:10 return? Why?
1:3 + 1:10
#It returns a vector with 1:3 repeatedly added to 1:10 because the lengths are different



#########################################################################
#
# Chapter 5.6
#
#########################################################################
#5.6.7.2
#Come up with another approach that will give you the same output as not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance) (without using count()).
not_cancelled <-
  flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% count(dest) #original
not_cancelled %>%   group_by(dest) %>%
  summarise(n = n()) #new version

not_cancelled %>% count(tailnum, wt = distance)  #original
not_cancelled %>% group_by(tailnum) %>%
  summarise(group_distance = sum(distance)) #new version

#5.6.7.3 Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay) ) is slightly suboptimal. Why? Which is the most important column?
#If a flight never left, then it was cancelled, so instead of dep_delay we could use dep_time.
#We also don't need both dep_time and arr_time

#5.6.7.4
#Look at the number of cancelled flights per day. 
#Is there a pattern? Is the proportion of cancelled flights related to the average delay?
dailyDelay <- flights %>% group_by(year,month,day)
flightsProportion <- summarise(dailyDelay,cancelled_flights = sum(is.na(dep_time)),cancelled_flights_proportion = mean(is.na(dep_time)),mean_dep_del = mean(dep_delay, na.rm = TRUE),mean_arr_del = mean(arr_delay,na.rm = TRUE))
flightsProportion
#The proportion of cancelled flights is related to the average delay

#5.6.7.5
#Which carrier has the worst delays? 
carriers <- flights %>% group_by(carrier)
carrierDelay <- carriers %>% summarise(avg_dep_delay = mean(dep_delay,na.rm = TRUE),avg_arr_delay = mean(arr_delay,na.rm = TRUE)) %>% arrange(desc(avg_dep_delay),desc(avg_arr_delay))
carrierDelay
#F9 has the worst average delays

#Challenge: can you disentangle the effects of bad airports vs. bad carriers? Why/why not? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))
#I'll come back to this

#5.6.7.6
#What does the sort argument to count() do. When might you use it?
#It helps order the count by sorting it in a descending order
count(flights, carrier, sort = T)

#########################################################################
#
# Chapter 5.7
#
#########################################################################
#5.7.1.1
#Refer back to the lists of useful mutate and filtering functions. Describe how each operation changes when you combine it with grouping.
#mutate() - when combined with grouping, it'll ad a new column to the value that was grouped
  #the new column will be the same length as the df but there will be repeated values per groups

#filter() will filter each group separately and potentially only return some of the results

#5.7.1.2
#Which plane (tailnum) has the worst on-time record?
worstRecord <- flights %>% group_by(tailnum) %>% 
  summarise(onTimeDelay = max(arr_delay)) %>%
  top_n(1,onTimeDelay)
worstRecord

#5.7.1.3
#What time of day should you fly if you want to avoid delays as much as possible?
delayTOD <- flights %>% group_by(hour) %>% summarise(avg_del = mean(dep_delay,na.rm = TRUE))
arrange(delayTOD,avg_del)
#You should fly in the morning - 5 am is best

#5.7.1.4
#For each destination, compute the total minutes of delay.
destDelay <- flights %>% group_by(dest) %>% summarise(sum(arr_delay,na.rm=TRUE))
#For each flight, compute the proportion of the total delay for its destination
flightDelayProp <- flights %>% group_by(dest) %>% mutate(total_delay = sum(arr_delay, na.rm = TRUE))  %>%
    group_by(flight,add = TRUE) %>% summarise(proportion = sum(arr_delay)/unique(total_delay))
flightDelayProp

#5.7.1.5
#Delays are typically temporally correlated: even once the problem that caused the initial delay has been resolved, later flights are delayed to allow earlier flights to leave. 
#Using lag(), explore how the delay of a flight is related to the delay of the immediately preceding flight.
lagExploration <- flights %>% group_by(dest) %>% mutate(prevFlight = lag(dep_delay)) %>%
  filter(!is.na(prevFlight))
select(lagExploration,dep_time, dep_delay, prevFlight) %>% arrange(dest,dep_time)

#5.7.1.6
#Look at each destination. Can you find flights that are suspiciously fast? (i.e. flights that represent a potential data entry error).
suspiciousFlights <- flights %>% group_by(dest) %>% mutate(speed = distance/air_time) %>% arrange(-speed)
select(suspiciousFlights,flight,speed)

#Compute the air time of a flight relative to the shortest flight to that destination. Which flights were most delayed in the air?
airTimeProp <- flights %>% group_by(dest) %>% mutate(air_time_rel = air_time - min(air_time, na.rm = TRUE)) %>% select(dest,flight,air_time,air_time_rel)
arrange(airTimeProp, -air_time_rel)

#5.7.1.7
#Find all destinations that are flown by at least two carriers. Use that information to rank the carriers.
multiCarrier <- flights %>% group_by(dest) %>% filter(length(unique(carrier)) >= 2) %>% summarise(numCarriers = length(unique(carrier)) )
arrange(multiCarrier,-numCarriers)

#5.7.1.8
#For each plane, count the number of flights before the first delay of greater than 1 hour.
oneHour <- flights %>% group_by(tailnum) %>% summarise(before_one_hour = sum((cumall(dep_delay < 60))))
oneHour


#########################################################################
#
# Chapter 19.3
#
#########################################################################
#19.3.1.1
#Read the source code for each of the following three functions, puzzle out what they do, and then brainstorm better names.
#So the first function takes in a string and substring and returns if the substring begins the string
#New name = prefix_check

#The second function checks to see if the argument is a vector
#If so, returns null. If not, returns the vector missing the last element
#New name = shorten_vector

#The third function takes in a vector x and int (or vector) y and outputs a vector of length x that is comprised only of y
#New name = repetition_vector

#19.3.1.2
#I haven't written any functions recently

#19.3.1.3
#Compare and contrast rnorm() and MASS::mvrnorm(). How could you make them more consistent?
#So rnorm is a normal distribution function that takes in a mean and standard deviation and generates random deviates
#mvrnorm is a normal distribution function for a multivariate normal distribution, rather than univariate for rnorm
#It takes in a number of samples, means of the variables, and a covariance matrix of the variables, which is very different from rnorm
#Could rename it to be rnorm_mv so that they have the same prefix and therefor can easily find both variants of the rnorm function

#19.3.1.4
#Make a case for why norm_r(), norm_d() etc would be better than rnorm(), dnorm(). Make a case for the opposite.
#The two functions beginning with the same prefix would make it easier for a user to search for the functions and be reminded there were multiple options
#However, it might be confusing to users to have them named that similarly because their ultimate outputs are different

#########################################################################
#
# Chapter 19.4
#
#########################################################################
#19.4.4.1
#What’s the difference between if and ifelse()? Carefully read the help and construct three examples that illustrate the key differences.
#if tests only one condition, while ifelse tests each element
#Example one:
x <- 1:10
if(x == 1){
  print("Great")
}
ifelse(x==1,yes = "Great", no = "Boo")

#Example two:
y <- c(1,1,1,1)
if(sum(y) ==4){
  print("4")
}
ifelse(sum(y) == 4,yes="4", no = "not 4")

#Example three:
z <- rnorm(5)
if(z < .5){
  print("Less than half")
}
ifelse(z < .5, yes = "less than half", no = "not less than half")

#19.4.4.2
#Write a greeting function that says “good morning”, “good afternoon”, or “good evening”, depending on the time of day. (Hint: use a time argument that defaults to lubridate::now(). That will make it easier to test your function.)
timeOfDay <- lubridate::now()
timeOfDayHour <- lubridate::hour(timeOfDay)
timeOfDayHour < 12
if(timeofDayHour < 12){
  print("Good morning!")
}else if(timeOfDayHour >= 12 && timeOfDayHour < 5){
  print("Good afternoon")
}else{
  print("Good evening")
}

#19.4.4.3
#Implement a fizzbuzz function. 
#It takes a single number as input. 
#If the number is divisible by three, it returns “fizz”. 
#If it’s divisible by five it returns “buzz”. 
#If it’s divisible by three and five, it returns “fizzbuzz”. Otherwise, it returns the number. Make sure you first write working code before you create the function.
fizzbuzz <- function(x){
  if(x%%3 == 0 && x%%5 == 0){
    "fizzbuzz"
  }else if(x%%3 == 0){
    "fizz"
  }else if(x%%5 == 0){
    "buzz"
  }else{
    x
  }
  
}

fizzbuzz(5)
fizzbuzz(3)
fizzbuzz(15)
fizzbuzz(13)
#BOOMshackalacka

#19.4.4.4
#How could you use cut() to simplify this set of nested if-else statements?
# 
# if (temp <= 0) {
#   "freezing"
# } else if (temp <= 10) {
#   "cold"
# } else if (temp <= 20) {
#   "cool"
# } else if (temp <= 30) {
#   "warm"
# } else {
#   "hot"
# }
temp = 20
tempCut <- cut(temp,c(-Inf,0,10,20,30,Inf),labels = c("freezing","cold","cool","warm","hot"))

#How would you change the call to cut() if I’d used < instead of <=?
#I'd change the numbers to reflect that change: -Inf, -1, 9, 19, 29, Inf or could change the default value of right = FALSE

#What is the other chief advantage of cut() for this problem? (Hint: what happens if you have many values in temp?)
#It works on vectors vs if which only works on a single argument

#19.4.4.5
#What happens if you use switch() with numeric values?
?switch
#It is coerced into an int and then evaluated. If the int is between 1 and nargs()-1, then the corresponding element of the switch statement is evaluated
#and the result is returned

#19.4.4.6
#What does this switch() call do? What happens if x is “e”?
x = "b"

switch(x,
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)
#It evaluates the expression and returns one of the arguments
#Nothing happens if x is "e"