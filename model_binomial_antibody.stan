
    
//I would like to add the sensitivity, but for now just assume all positives are positive    
// the data block is where we specify how data will be passed to stan.
  data {
  int <lower=1> n_pre; // pre covid sample size
  int<lower=0, upper=n_pre> pos_pre; // pre covid number positive
  int <lower=0> n_sample ;// number sampled in the current post-covid pop
  int <lower=0, upper=n_sample> pos_pos; // number positive post covid
  real <lower=0, upper=1> prior_prev[2]; // bounds for the prior prevalence, we'll roll this into a uniform prior but include what we know, which is that at a minimum we know how many sick identified patients there are.
  }
// the parameter block is where we specify the parameters that stan will fit. In our previous examples, this would be the 
// parameters that we make a grid to compute likelihoods over.
parameters {
  real <lower=0, upper=1> mu; // the proportion of the population that is positive
  real <lower=0, upper=1> gamma ; // the specificity, the probability of a false positive
}

 
// the model block is where likelihoods get calculated. Later, with multi-level models, additional likelihood calculations would go here. The model block also includes priors.
model {
//some new variables we will need to convolute the binomials
real log_prob1=0;
real log_prob2=0;
real nat_sum_probs=0;


mu ~ uniform(prior_prev[1], prior_prev[2]); // apply our uniform prior.
gamma ~ beta(18,2); // really we already know it is better than this, but I guess we are using all of the data available to show that


#gamma ~ uniform(0.9,1); // really we already know it is better than this, but I guess we are using all of the data available to show that

(n_pre-pos_pre)  ~ binomial(n_pre, gamma) ; 

for( i in 0:pos_pos){ //we have to consider every possible scenario, where there are 0 true positives or as many as pos_pos true positives

log_prob1 = binomial_lpmf(i | n_sample, mu);

#  i ~ binomial(n_sample, mu) ; // prob that i are actually sick

log_prob2 = binomial_lpmf( (pos_pos-i) | (n_sample-i), (1-gamma));

nat_sum_probs += exp(log_prob1 + log_prob2) ;
  
# (pos_pos-i) ~ binomial((n_sample-i), (1-gamma)) ; // prob that exactly the remaining positive tests are false positives
 
}

target += log(nat_sum_probs);
    
}
 
    
    
    
