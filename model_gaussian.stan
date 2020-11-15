
    
// the data block is where we specify how data will be passed to stan.
  data {
  int<lower=1> N; // N is the number of samples. It cannot be less than 1, and is an integer.
  real<lower=0> height[N]; // the data is a vector of length N, they are real numbers, but cannot be negative.
  }
// the parameter block is where we specify the parameters that stan will fit. In our previous examples, this would be the 
// parameters that we make a grid to compute likelihoods over.
parameters {
  real  mu; // the mean of height is a real number. It probably shouldn't be negative, but....
  real <lower=0, upper=50> sigma; // the standard deviation is bounded between 0 and 50, this automatically sets up
  // a uniform prior between 0 and 50.
}

// the model block is where likelihoods get calculated. Later, with multi-level models, additional likelihood calculations would go here. The model block also includes priors.
model {

mu ~ normal(178,20); // we assume mu is somewhere around 178 
sigma ~ uniform(0,50); // we did not have to put this here, it is implied from the parameter definition above, but let's be explicit
for( i in 1:N){  
    height[i] ~ normal(mu,sigma); // finally we calculate the likelihood of each datapoint. We actually could write this without an explicit loop.
}

}

    
