# 12.2.1 #2-3

#12.2.1.2
#Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
# 
# Extract the number of TB cases per country per year.
# Extract the matching population per country per year.
# Divide cases by population, and multiply by 10000.
# Store back in the appropriate place.
# Which representation is easiest to work with? Which is hardest? Why?

#table2 rate:
table2_fixed <- table2 %>% spread(key = type, value = count)
table2_fixed
rate <- table2_fixed$cases/table2_fixed$population * 10000
table2_with_rate <- table2_fixed %>% mutate(rate)

#Table 4a + 4b rate:
table4a_fixed <- table4a %>% gather(`1999`,`2000`, key = "year", value = "cases")
table4b_fixed <- table4b %>% gather(`1999`,`2000`, key = "year", value = "population")
table4 <- left_join(table4a_fixed,table4b_fixed)
table4
rate4 <- table4$cases/table4$population
table4_with_rate <- table4 %>% mutate(rate4)

#table2 was easiest to work with because less data manipulation had to take place

#12.2.1.3
#Recreate the plot showing change in cases over time using table2 instead of table1. What do you need to do first?
table2_with_rate
table2_with_rate %>% count(year, wt = cases)
library(ggplot2)
ggplot(table1, aes(year,cases)) +  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

#I used the table manipulations from the first question to accomplish this
#I had to calculate the rate by manipulating the table2 table in order to get cases and population on the same line

################################################################################################
#
#12.3
#
################################################################################################
# 12.3.3 #1-4
#12.3.3.1
# Why are gather() and spread() not perfectly symmetrical?
#   Carefully consider the following example:
#   
  stocks <- tibble(
    year   = c(2015, 2015, 2016, 2016),
    half  = c(   1,    2,     1,    2),
    return = c(1.88, 0.59, 0.92, 0.17)
  )
  stocks %>%
    spread(year, return) %>%
    gather("year", "return", `2015`:`2016`)
  # (Hint: look at the variable types and think about column names.)
stocks
#They're not perfectly symmetrical because the type of column isn't passed when either function is used


#   Both spread() and gather() have a convert argument. What does it do?
# It converts the columns to the proper data type because they'll be character type otherwise

#12.3.3.2
#Why does this code fail?

table4a %>% 
  gather(1999, 2000, key = "year", value = "cases")
#Because 1999 and 2000 are numbers - they need to be surrounded by ``

#12.3.3.3
#Why does spreading this tibble fail? How could you add a new column to fix the problem?
people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
people %>% spread(key,value)
#It fails because Phillip Woods has 2 ages
#You can add a middle name column if in fact it is two different Phillips or an IsCurrent column to determine if it's a current age

#12.3.3.4
#Tidy the simple tibble below. Do you need to spread or gather it? What are the variables?

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
preg_fixed <- preg %>% gather(male,female,key = "gender", value = "count")

#The variables are gender, pregnant, and count


# 12.5.1 #1-2
#12.5.1.1
#Compare and contrast the fill arguments to spread() and complete()
?spread
#If set, missing values will be replaced with this value. Note that there are two types of missingness in the input: explicit missing values (i.e. NA), and implicit missings, rows that simply aren't present. Both types of missing value will be replaced by fill.
?complete()
#A named list that for each variable supplies a single value to use instead of NA for missing combinations.

#So both fill arguments will replace NA with a value. The spread() fill will also replace implicit missing values, but the complete() fill will not

#12.5.1.2
#What does the direction argument to fill() do?
?fill()
#It determines the order in which the data that's being filled in gets placed
