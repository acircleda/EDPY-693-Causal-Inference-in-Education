# Below is what I came up with. I also found the following online that take a
# slightly different approach (i.e., correlating u and t then creating
# equations) but I think this does the same thing. 
# https://rstudio-pubs-static.s3.amazonaws.com/195619_f2b6ccddabdc4f3f8a168e1a7f328829.html 

library(lavaan)

# function to simulate IV data 
IV.sim <- function(n, b.iv, b.ut, b.uy, b.t){
  
  u <- rnorm(n, 0, 1) #unmeasured confounder 
  iv <- rnorm(n, 0, 1) #instrument
  
  e_t <- rnorm(n, 0, 1) #random error for treatement
  e_y <- rnorm(n, 0, 1) #random error for outcome

  # compute treatment and outcome
  t <- b.iv*iv + b.ut*u + e_t
  y <- b.t*t + b.uy*u + e_y
  
  # save as data frame
  df <- data.frame(y, t, iv, u)
  
  # regression/lavaan models
  naivemod <- 'y~t'
  ivmod <-'y~t
          t~iv
          y~~t'
  fullmod <- 'y~t+u'
  
  # estimate models
  naive <- sem(naivemod, df) 
  iv <- sem(ivmod, df)
  full <- sem(fullmod, df)
  
  # save regression coefficient for treatment effect
  n.est <- parameterestimates(naive)[1,4]
  iv.est <- parameterestimates(iv)[1,4]
  full.est <- parameterestimates(full)[1,4]
  return(c(n.est,iv.est, full.est))
  
}

# replicate function 1000 times and save as data frame
res <- data.frame(t(replicate(1000,IV.sim(100, .5, .5, .5, 1))))

names(res) <- c("naive", "iv","full") #column names
summary(res) #summarize findings
hist(res$iv)
hist(res$naive)
hist(res$full)


# function to simulate IV data with an exogenous covariate
IV.sim.c <- function(n, b.iv, b.ut, b.uy, b.ct, b.cy, b.t){
  
u <- rnorm(n, 0, 1)
e_t <- rnorm(n, 0, 1)
e_y <- rnorm(n, 0, 1)
iv <- rnorm(n, 0, 1)
c <- rnorm(n, 0, 1)
t <- b.iv*iv + b.ut*u + b.ct*c + e_t
y <- b.t*t + b.uy*u + b.cy*c + e_y

df <- data.frame(y, t, iv, c, u)

naivemod <- 'y~t+c'
ivmod <- 'y~t+c
          t~iv+c
          y~~t'
fullmod <- 'y~t+u+c'

naive <- sem(naivemod, df) 
iv <- sem(ivmod, df)
full <- sem(fullmod, df)
n.est <- parameterestimates(naive)[1,4]
iv.est <- parameterestimates(iv)[1,4]
full.est <- parameterestimates(full)[1,4]
return(c(n.est,iv.est, full.est))

}
res.c <- data.frame(t(replicate(1000,IV.sim.c(100, .5, .5, .5, .5, .5, 1))))
names(res.c) <- c("naive", "iv","full")
summary(res.c)




