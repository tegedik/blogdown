library(tidyverse)
library(optimx) # or optim depending on the optimization method used
df <- car::Mroz

outcome <- df %>% 
  select(lfp)  %>%
  pull(lfp) %>% 
  fct_recode("0" = "no",
             "1" = "yes") %>% 
  as.character() %>% 
  as.numeric()

predictors <- df %>% 
  select(k5, age, inc) # selected predictors

predictors_int <- predictors %>% 
  mutate(int=rep(1, nrow(df))) %>% # column of 1 (intercept)
  select(int, everything()) %>% 
  as.matrix()
  

lmfit <- lm(outcome ~ predictors[,c(2:4)])
s_val <- lmfit$coefficients

logLikelihoodLogit <- function(vBeta, mX, vY) {
  return(-sum(vY*(mX %*% vBeta - log(1+exp(mX %*% vBeta)))
    + (1-vY)*(-log(1 + exp(mX %*% vBeta)))))  
}

my_optim <- optimx(s_val, logLikelihoodLogit, method = 'BFGS', mX = predictors_int, vY = outcome, hessian=TRUE)

my_optim

attr1 <- attr(my_optim, "details")[1, ]
hess_optx <- attr1$nhatend
OI <- solve(hess_optx)
se_optx<-sqrt(diag(OI))

est_optx <- my_optim %>%
  select(1:ncol(predictors_int)) %>% t()

est_optx

remx1 = function(x){
  return(gsub("as.matrix.predictors.","",x))
}
rownames(est_optx)<- remx1(rownames(est_optx)) 



