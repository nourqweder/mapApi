MGB<- setRefClass("MGB",
                  fields = list(
                    mgb_object="data.frame",
                    name="character",
                    speed = "numeric",
                    status="character"
                  ),
                  methods = list(
                    #MGB_504 <- build_mgb(name="Hopewell",speed=46)

                    initialize=function(name,speed)
                    {
                      mgb_object <<- data.frame(weight_of_cargo = numeric(), travel_time = numeric(), stringsAsFactors = FALSE )
                      status <<- "in service"
                      name<<- name
                      speed <<- speed
                    },
                    #---------------------------------------------------------
                    simulate_home_run=function()
                    {
                      weight <- rnorm(n = 10,mean=40, sd=2)
                      rate <- 1/((weight ) * (900/speed)/250)
                      time<- rexp(1,rate = rate)
                      if (status== "in service") {
                        if (time > 10) {
                          status <<- "SUNK!"

                          print( paste0("[Boat ",name,"] is reported lost with status: ",status))
                        }
                        else
                        {
                          df <- c(weight_of_cargo = weight , travel_time = time, stringsAsFactors = FALSE )
                          mgb_object <<- rbind(mgb_object, df)
                          names(mgb_object)<<- c("weight_of_cargo" ,"travel_time")
                          return(list("mgb"= name,"status"= status,"weight_of_cargo" = weight , "travel_time" = time ))
                        }
                      }
                    },
                    #---------------------------------------------------------
                    plot = function(){
                      weight <- mgb_object$weight_of_cargo
                      time <- mgb_object$travel_time

                      plot.default(x = time, y = weight, type = "p")
                    },
                    #---------------------------------------------------------
                    print=function()
                    {


                    },
                    plot=function()
                    {
                      weight <- mgb_object$weight_of_cargo
                      time <- mgb_object$travel_time
                      plot(x = weight, y =  time,xlim = weight_of_cargo,type = "p")
                    }


                  ))


MGB$methods()

MGB$methods(plot = function(){
  weight <- mgb_object$weight_of_cargo
  time <- mgb_object$travel_time

  plot.default(x = time, y = weight, type = "p")
})



build_mgb=function(name,speed)
{
  bb<- MGB$new(name,speed)
  return(bb)
}

m1<-MGB$new(name="Hopewell",speed=46)
build_mgb(name="test",speed=1)

MGB_504 <- build_mgb(name="Hopewell",speed=46)
m$simulate_home_run()
m$plot()



m<-mgb$new("122",42)
for (i in 1:103) {
  print(i)
  print(m$status)
  m$simulate_home_run()
}

#rm(list=ls())

#---------------------------PROBLEM3

#problem3:
#find max value<-function(vector x)
# return list with two elements(max_value, max_value_position)
#if multi max value the max_value_position at the last occurrence.
#x<-c(1,2,3,56,4,5)
#> find_max_value(x)
#$max_value
#[1] 56
#$max_value_position
#[1] 4

find_max_value<-function(x)
{
  temp <- 0
  temp_position <-0
  for(i in 1:(length(x)-1)) {
    print(x[i])
    if (x[i] > x[i+1])
    {
      temp<- x[i]
      temp_position <- i
    }
  }
  return(list("max_value"= temp , "max_value_position"= temp_position))
}
x<-c(1,2,3,56,4,5)
find_max_value(x)
x<-c(1,2,3,56,4,56,5)
find_max_value(x)




#maxfx1; : : : ; xng = (􀀀1)  minf􀀀x1; : : : ;􀀀xng
find_max_value2<-function(x)
{
  return(list("max_value"= max(x) , "max_value_position"= which.max(x) ))
}


x<-c(1,2,3,56,4,5)
x[1]
find_max_value2(x)
x<-c(1,2,3,56,4,56,5)
find_max_value2(x)


# c
test_that("The output is incorrect.", {
  expect_that(find_max_value(x), find_max_value2(x))
})




