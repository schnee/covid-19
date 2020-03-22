library(tidyverse)
library(deSolve)

## Create an SIR function
sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    return(list(c(dS, dI, dR)))
  })
}
### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
us_pop = 330000000
init_infected = 1
init       <- c(S = 1-(init_infected/us_pop), I = init_infected/us_pop, R = 0.0)
## beta: infection parameter; gamma: recovery parameter - recover in 7 days
parameters <- c(beta = 0.2857, gamma = 1/7)
R0 = parameters["beta"] / parameters["gamma"]
print(R0)
## 4 month time frame
times      <- seq(0, 365, by = 1)
## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## change to data frame
out %>% as_tibble() %>% 
  gather(key, value, S:R) %>% 
  ggplot(aes(time, value, color= key )) + geom_line()



               