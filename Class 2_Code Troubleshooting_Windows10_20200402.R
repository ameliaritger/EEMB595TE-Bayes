##download "rethinking" from github page: https://github.com/rmcelreath/statrethinking_winter2019
install.packages(c("mvtnorm","loo","coda"), repos="https://cloud.r-project.org/",dependencies=TRUE)
options(repos=c(getOption('repos'), rethinking='http://xcelab.net/R'))
install.packages('rethinking',type='source')

## R code 0.5
install.packages(c("coda","mvtnorm","devtools","dagitty" ,"tidyverse","lubridate"))

##install stan
install.packages("Rcpp", repos = "https://rcppcore.github.io/drat")
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

#load stan
library("rstan")  
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(tidyverse)
#listed conflicts: tidyr::extract() masks rstan::extract(); dplyr::filter() masks stats::filter(); dplyr::lag() masks stats::lag()


##Test rethinking
## R code 9.11
library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )

## R code 9.12
m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )
precis( m8.3 , depth=2 )

## R code 9.13
dat_slim <- list(
  log_gdp_std = dd$log_gdp_std,
  rugged_std = dd$rugged_std,
  cid = as.integer( dd$cid )
)
str(dat_slim)
view(dat_slim)

## R code 9.14
## This bit calls stan, here's where you'll have trouble if your c compiler isn't working
m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dat_slim , chains=1 )
#Some simple output from a simple stan model
print(m9.1)


#test stan 
#This is just example code that defines a stan program for the often used "8 schools" dataset

schools.code <- ' data {
                    int<lower=0> J;         // number of schools 
                    real y[J];              // estimated treatment effects
                    real<lower=0> sigma[J]; // standard error of effect estimates 
                  }
                  parameters {
                    real mu;                // population treatment effect
                    real<lower=0> tau;      // standard deviation in treatment effects
                    vector[J] eta;          // unscaled deviation from mu by school
                  }
                  transformed parameters {
                    vector[J] theta = mu + tau * eta;        // school treatment effects
                  }
                  model {
                    target += normal_lpdf(eta | 0, 1);       // prior log-density
                    target += normal_lpdf(y | theta, sigma); // log-likelihood
                    
                  }
                  '


schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <-stan(model_code = schools.code, data = schools_dat , chains = 4 , iter = 10000,control = list(adapt_delta = 0.99))
print(fit)
plot(fit , pars=c("eta"))


