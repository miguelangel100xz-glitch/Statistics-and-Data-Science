##########################################################
# Choose Monte Carlo sample size for a randomization #
# test. Estimate p (p-value of permutation test) with #
# p-hat. For a given true p (default = 0.04) and #
# a given alpha (default = 0.05), returns the MC sample #
# size needed to get p-hat < alpha with probability cc #
# (default = .99). #
##########################################################
randm <- function(p=.01,alpha=0.05,cc=.99){
  

  randm <- qnorm(cc)^2 * p*(1-p) / (alpha-p)^2
  randm <- trunc(randm + 1) # Round up to next integer
  randm
} # End of function randm
 probs <- c(.01,.02,.03,.0363,.04,.045,.049)
 cbind(probs,randm(p=probs)) # Use default values of alpha and cc
