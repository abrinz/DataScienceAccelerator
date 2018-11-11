#########################################################################
#
# Chapter 20.3
#
#########################################################################
#20.3.5.1
#Describe the difference between is.finite(x) and !is.infinite(x).
#The is.finite() function checks to see if the number is finite i.e. not Inf or -Inf. It also checks to see if the value is missing
#The is.infinite() function checks to see if the number is infinite i.e. Inf or -Inf. 
#So !is.infinite() would return TRUE for missing values, but is.finite() would return FALSE

#20.3.5.2
#Read the source code for dplyr::near() (Hint: to see the source code, drop the ()). How does it work?
?dplyr::near
#It compares two numeric vectors. Useful because it has a tolerance limit unlike ==

#20.3.5.3
#A logical vector can take 3 possible values. How many possible values can an integer vector take? 
#2^31 - 1
#How many possible values can a double take? Use google to do some research.
#2^64 - 4 (4 for -Inf, Inf, NA_real_ and NAN)

#20.3.5.4
#Brainstorm at least four functions that allow you to convert a double to an integer. How do they differ? Be precise.
round(1.5)
as.integer(1.5)
type.convert()
trunc(1.5)
#They differ by dealing with rounding differently. Some round up, some down.

#########################################################################
#
# Chapter 20.4
#
#########################################################################
#20.4.6.1
#What does mean(is.na(x)) tell you about a vector x?
#It describes the proportion of null elements in the vector

#What about sum(!is.finite(x))?
#It counts the number of elements that are Inf or -Inf

#20.4.6.2
#Carefully read the documentation of is.vector(). 
?is.vector()
#What does it actually test for? 
#It checks to see if the argument is a vector of a certain mode with no attributes other than names

#Why does is.atomic() not agree with the definition of atomic vectors above?
#It only checks to see if it is an atomic type
#It's different from is.vector() because it doesn't care about attributes

#20.4.6.3
#Compare and contrast setNames() with purrr::set_names().
?setNames()
#It sets the names on the object and returns the obect
?purrr::set_names()
#On the other hand, purrr::set_names() can take multiple arguments and the names can be set in many different ways

#20.4.6.4
#Create functions that take a vector as input and returns:
# 
# The last value. Should you use [ or [[?
lastValue <- function(x){
  last <- length(x)
  x[[last]]
}
x <- 1:10

#The elements at even numbered positions.
evenElements <- function(x){
  x[seq(2,length(x),by = 2)]
}
evenElements(x)
#Every element except the last value.
everythingButLast <- function(x){
  x[1:length(x)-1]
}
everythingButLast(x)

#Only even numbers (and no missing values).
evenNumbers <- function(x){
  x[x%%2 == 0]
}
evenNumbers(x)

#20.4.6.5
#Why is x[-which(x > 0)] not the same as x[x <= 0]
x<-c(1,-1,1,-1,4,-1,0,NA,NaN,Inf)
x[-which(x > 0)]
x[x <= 0]
#They return different values when incorporating the special values - the second one converts NaN to NA

#20.4.6.6
#What happens when you subset with a positive integer that’s bigger than the length of the vector? 
x[11]
#It returns NA
#What happens when you subset with a name that doesn’t exist?
#It returns NA



#########################################################################
#
# Chapter 20.5
#
#########################################################################
#20.5.4.1
#Draw the following lists as nested sets:
#SEE ATTACHMENT IN GIT

#20.5.4.2
#What happens if you subset a tibble as if you’re subsetting a list? 
#It works the same way
#What are the key differences between a list and a tibble?
#Lists can contain columns/sets of different lengths - tibbles cannot.

#########################################################################
#
# Chapter 21.2
#
#########################################################################
#21.2.1.1
#Write for loops to:
# Think about the output, sequence, and body before you start writing the loop.
# Compute the mean of every column in mtcars.
meanMtcars <- vector("double",ncol(mtcars))
for(i in seq_along(mtcars)){
  meanMtcars[[i]] <- mean(mtcars[[i]])
}
meanMtcars
# Determine the type of each column in nycflights13::flights.
columnTypes <- vector("character",ncol(flights))
for(i in seq_along(flights)){
  columnTypes[[i]] <- typeof(flights[[i]])
}
columnTypes

# Compute the number of unique values in each column of iris.
uniqueValues <- vector("integer",ncol(iris))
for(i in seq_along(iris)){
  uniqueValues[[i]] <- length(unique(iris[[i]]))
  
}
uniqueValues

# Generate 10 random normals for each of  μ= −10,0,10, and 100
mus = c(-10,0,10,100)
randomNormals <- vector("list",length(mus))
for(i in seq_along(mus)){
  randomNormals[[i]] <- rnorm(10,mean = mus[i])
  
}

#21.2.1.2
#Eliminate the for loop in each of the following examples by taking advantage of an existing function that works with vectors:
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}

#My version
stringr::str_c(out,collapse = "")


x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))

#My version
sd(x)


x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}

#My version
cumsum(x)

#21.2.1.3
# Combine your function writing and for loop skills:
#Write a for loop that prints() the lyrics to the children’s song “Alice the camel”.
aliceTheCamel <- function(number){
  lyrics <- vector("character",number+1)
  for(i in 1:number){
    lyrics[[i]] <- print(paste("Alice the camel has",number,"humps.","Alice the camel has",number,"humps.","Alice the camel has",number,"humps.","So go, Alice,go"))
    number <- number - 1
  }
  lyrics[[number + 1]] <- print(paste("Alice the came has no humps. Alice the camel has no humps. Alice the camel has no humps. Now Alice is a horse"))
}
aliceTheCamel(5)
# Convert the nursery rhyme “ten in the bed” to a function. Generalise it to any number of people in any sleeping structure.
nurseryRhyme <-  function(number,structure){
  lyrics <- vector("character",number)
  for(i in 1:(number-1)){
    lyrics[[i]] <- print(paste("There were",number,"in a",structure, "and the little one said 'Roll over, roll over' so they all rolled over and one fell out"))
    number <- number - 1
  }
  lyrics[[number]] <- print(paste("There was",number,"in a",structure,"and the little one said 'Good night!'"))
}
nurseryRhyme(5,"hammock")

# Convert the song “99 bottles of beer on the wall” to a function. Generalise to any number of any vessel containing any liquid on any surface.
numberVesselLiquidSurface <- function(number,vessel,liquid,surface){
  result <- vector("character",number)
  for(i in 1:number){
   result[[i]] <- print(paste(number,vessel,"of",liquid,"on the",surface,",",number,vessel,"of",liquid,". Take one down, pass it around,",number-1,vessel,"of",liquid,"on the",surface)) 
   number = number - 1
  }
}
numberVesselLiquidSurface(5,"bottles","ale","table")

#21.2.1.4
#It’s common to see for loops that don’t preallocate the output and instead increase the length of a vector at each step:

#It increases the length of execution from O(1) to O(n^2)
x <- runif(100000)
newVec <- vector("double")

for(i in seq_along(x)){
  newVec <- c(newVec,x[[i]])
}

newVec2 <- vector("double",length(x))
for(i in seq_along(x)){
  newVec[[i]] <- x[[i]]
}

#########################################################################
#
# Chapter 21.3
#
#########################################################################
#21.3.5.1
#Imagine you have a directory full of CSV files that you want to read in.
#You have their paths in a vector, files <- dir("data/", pattern = "\\.csv$", full.names = TRUE), and now want to read each one with read_csv(). Write the for loop that will load them into a single data frame.

output <- vector("list",length(files))

for(names in seq_along(files)){
  output[[names]] <- read_csv(files[[names]])
}
df <- dplyr::bind_rows(output)

#21.3.5.2
#What happens if you use for (nm in names(x)) and x has no names? 
#Nothing happens because it seems like the number of iterations for the loop is 0

#What if only some of the elements are named? 
#An error will be thrown when trying to access a nameless element

#What if the names are not unique?
#It'll return the first element with that name

#21.3.5.3
#Write a function that prints the mean of each numeric column in a data frame, along with its name. For example, show_mean(iris) would print:
output <- vector("list",length(df))
for(i in seq_along(df)){
  if(is.numeric(df[[i]])){
    output[[i]] <- mean(df[[i]])
  }
}
output <- dplyr::bind_rows(output)



#########################################################################
#
# Chapter 21.5
#
#########################################################################
#21.5.3.1
#Write code that uses one of the map functions to:
# Compute the mean of every column in mtcars.
meanMtcars2 <- map(mtcars,mean)
# Determine the type of each column in nycflights13::flights.
columnTypes2 <- map_chr(flights,typeof)
# Compute the number of unique values in each column of iris.
uniqueValues2 <- map_int(iris,function(x) length(unique(x)))
# Generate 10 random normals for each of  μ= −10,0,10, and 100
randomNormals2 <- map(c(-10,0,10,100),function(x) rnorm(10))

#How can you create a single vector that for each column in a data frame indicates whether or not it’s a factor?
map_lgl(df,is.factor)