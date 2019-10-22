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
