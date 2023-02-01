install.packages("ggplot2")
install.packages("deSolve")
library(ggplot2)
library(deSolve)

algeriacases <- read.csv(file = 'C:\\Users\\husto\\Documents\\SIRFINAL.csv')

algeriasubset<-algeriacases[algeriacases$time<250,]
algeriasubset$logconfirmedcases<-log(algeriasubset$confirmedcases)
algeriasubset$logconfirmedcases[algeriasubset$logconfirmedcases<0]<-0

#This plot shows the log transformed total cases over time in days from 1/22/2020
plot<- ggplot(algeriasubset, aes(time,logconfirmedcases)) +
  geom_point() +
  ggtitle("Days since Recording Began Vs ln(Cases)") + xlab("Days") + ylab("ln(Confirmed Cases)")
plot

#Finding m, make sure the subset below is on a stable regime. 
subset2<-algeriasubset<-algeriacases[algeriacases$time<=202 & algeriacases$time>=196,]
subset2$logcon<-log(subset2$confirmedcases)

plot<- ggplot(subset2, aes(time,logcon)) +
  geom_point() +
  ggtitle("days Vs ln(confirmed)") + xlab("time") + ylab("ln(confirmed)")
plot

lmwow<-lm(subset2$logcon~subset2$time)
#The slope from the summary below is m.
summary(lmwow)
m<-.0150845

#Now find gamma
#We know gamma is approximately equal to the recovery rate, which is 1 divided by
#the amount of time it takes someone to recover. Looking in this dataset and online, 
#We say that it takes approximately 17 days to recover, as also suggested by https://www.frontiersin.org/articles/10.3389/fams.2020.571544/full
#Therefore gamma is
gamma<-1/17
#Now use m and gamma to find beta
beta<-m+gamma

#Now that we've found the parameters to define the SIR model in Algeria, run it in Matlab as that's what my model is in and it 
#will be easy to just plug in the parameters.
#Cite the code below to: https://rpubs.com/choisy/sir
sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    #N<- 4.3851044*10^7
    dS <- -beta * I * S/(4.3851044*10^7)
    dI <-  beta * I * S /(4.3851044*10^7) - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}
parameters_values <- c(
  #beta  = 0.07390803/(4.3851044*10^7), # infectious contact rate (/person/day)
  beta  = 0.0561215, # infectious contact rate (/person/day)
  #gamma = 0.05882353    # recovery rate (/day)
  gamma = .045533092863033    # recovery rate (/day)
)
initial_values <- c(
  S = 4.3851044*10^7,  # number of susceptibles at time = 0
  I =   10,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
  
)

time_values <- seq(0, 2000)

sir_values_1 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)

sir_vals <- as.data.frame(sir_values_1)

with(sir_vals, {
  # plotting the time series of susceptible:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "Population of Algeria", ylim=c(0,4.3851044*10^7))
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  lines(time, R, col = "green")

})
title("SIR model for COVID in Algeria.")
grid()
legend("right", c("susceptibles", "infectious", "recovered"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

#Now deduce some findings from the model
16590338/43851044 #this is the total proportion of people who got COVID as predicted by our model 

