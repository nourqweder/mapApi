vehicle <- setRefClass("Vehicle",
                       fields = list(tires = 'numeric', doors = 'numeric'),
                       methods = list(
                         steal_all_tires = function(x) {
                           tires <<- 0
                         },
                         seal_one_door = function(x) {
                           doors <<- doors - 1
                         },
                         convert_vehicle = function(x){
                           tires <<- tires + 1
                         }
                       ))
buses <- setRefClass("Buses",
                     contains = "Vehicle",
                     fields = list(people_onboard = 'numeric'),
                     methods = list(
                       add_people = function(x){
                         people_onboard <<- people_onboard + x
                       }
                     )
)
bus_480 <- buses$new(tires = 8, doors = 2, people_onboard = 25)
bus_480 <- buses$new(tires = 8, doors = 2, people_onboard = 25)

bus_480$people_onboard
bus_480$add_people(3)
otype(bus_480)




busa<-buses$new(tires=3,doors=4,people=2)
bus_480 <- bus$new(tires = 8, doors = 2, people_onboard = 25)
#1.2.1 my moving median()
#'Create a function that will create a rolling median from a given numeric vector. Call the function
#my moving median(). The functions should accept a given numeric vector x, and a numeric scalar n.
#The function should check if the arguments is a numeric vector (x) and a numeric scalar (n) and if
#not, it should stop the function (with the stop() or stopifnot() function). Otherwise the function
#should return the following vector:
my_moving_median <- function(x, n,...)
{
  stopifnot(is.numeric(x),is.numeric(n))

}
my_moving_median(x = c(1:10) , n = 2)
library(pryr)
df <- data.frame(num = 1:10, letter = letters[1:10])
otype(df)

num <- structure(list(x = 1:10), class= 'custom_num')
class(num)
inherits(num, 'custom_num')
as.data.frame(num)
