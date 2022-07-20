install.packages("sandwich")
install.packages("gmm")
install.packages("ivreg")

setwd("C:/Users/LG/Desktop/Drug issues_MLB/drug")

library(ivreg)
library(sandwich)
library(gmm)
library(plm)
library(readxl)
library(dplyr)
library(UsingR)

#Loading data
gamelevel <- read.csv("22total_07-19_rcode_man.csv")
summary(gamelevel)

colSums(is.na(gamelevel))
gamelevel[is.na(gamelevel) | gamelevel=="InF"] = NA

# Simple regression can be implemented
lmfit_sample <- gamelevel %>%
  lm(utility ~ drug_se + drug_tweet_daily + drug_news +
       star + win + compete + rank + stadiumage +
       pos_t1 + dv_t1 + lg_t1 + ws_t1 +
       tmdummy1+tmdummy2+tmdummy3+tmdummy4+tmdummy5+
       tmdummy6+tmdummy7+tmdummy8+tmdummy9+tmdummy10+
       tmdummy11+tmdummy12+tmdummy13+tmdummy14+tmdummy15+
       tmdummy16+tmdummy17+tmdummy18+tmdummy19+tmdummy20+
       tmdummy21+tmdummy22+tmdummy23+tmdummy24+tmdummy25+
       tmdummy26+tmdummy27+tmdummy28+tmdummy29+tmdummy30+
       yrdummy1+yrdummy2+yrdummy3+yrdummy4+yrdummy5+
       yrdummy6+yrdummy7+yrdummy8+yrdummy9+yrdummy10+
       yrdummy11+yrdummy12+yrdummy13+
       monthdummy1+monthdummy2+monthdummy3+monthdummy4+
       wddummy1+wddummy2+wddummy3+wddummy4+wddummy5+
       wddummy6+wddummy7+
       dndummy1+dndummy2,
       data= .)

summary(lmfit_sample)



# Simulate One column data
set.seed(123)

# Reproducible

# Generate the data from normal distribution
n <- 200
x <- rnorm(n, mean = 4, sd = 2)
# set up the moment conditions for comparison
# MM (just identified)
g0 <- function(tet, x) {
  m1 <- (tet[1] - x)
  m2 <- (tet[2]^2 - (x - tet[1])^2)
  f <- cbind(m1, m2)
  return(f)
}


# Test codes from Git-hub

gmmOpt2 <- gamelevel %>%
  gmm(utility ~ ticket + drug_se + drug_tweet_daily + drug_news,
      x = ~ ticket + ticket_lag + drug_se + drug_tweet_daily + drug_news,
      data = ., wmatrix = "optimal")

summary(gmmOpt2)



# main 2stage gmm data(10000 iteration)

gmmOpt1 <- gamelevel %>% 
  gmm(utility ~ ticket + drug_se + drug_tweet_daily + drug_news +
      star + win + compete + rank + stadiumage +
      pos_t1 + dv_t1 + lg_t1 + ws_t1 +
      tmdummy1+tmdummy2+tmdummy3+tmdummy4+tmdummy5+
      tmdummy6+tmdummy7+tmdummy8+tmdummy9+tmdummy10+
      tmdummy11+tmdummy12+tmdummy13+tmdummy14+tmdummy15+
      tmdummy16+tmdummy17+tmdummy18+tmdummy19+tmdummy20+
      tmdummy21+tmdummy22+tmdummy23+tmdummy24+tmdummy25+
      tmdummy26+tmdummy27+tmdummy28+tmdummy29+
      yrdummy1+yrdummy2+yrdummy3+yrdummy4+yrdummy5+
      yrdummy6+yrdummy7+yrdummy8+yrdummy9+yrdummy10+
      yrdummy11+yrdummy12+
      monthdummy1+monthdummy2+monthdummy3+
      wddummy1+wddummy2+wddummy3+wddummy4+wddummy5+wddummy6+
      dndummy1,
      x = ~ ticket+ ticket_lag + drug_se + drug_tweet_daily + drug_news +
      star + win + compete + rank + stadiumage +
      pos_t1 + dv_t1 + lg_t1 + ws_t1 +
      tmdummy1+tmdummy2+tmdummy3+tmdummy4+tmdummy5+
      tmdummy6+tmdummy7+tmdummy8+tmdummy9+tmdummy10+
      tmdummy11+tmdummy12+tmdummy13+tmdummy14+tmdummy15+
      tmdummy16+tmdummy17+tmdummy18+tmdummy19+tmdummy20+
      tmdummy21+tmdummy22+tmdummy23+tmdummy24+tmdummy25+
      tmdummy26+tmdummy27+tmdummy28+tmdummy29+
      yrdummy1+yrdummy2+yrdummy3+yrdummy4+yrdummy5+
      yrdummy6+yrdummy7+yrdummy8+yrdummy9+yrdummy10+
      yrdummy11+yrdummy12+
      monthdummy1+monthdummy2+monthdummy3+
      wddummy1+wddummy2+wddummy3+wddummy4+wddummy5+wddummy6+
      dndummy1,
      data = .,
      wmatrix = "optimal",
      vcov= "iid",
      optfct = "nlminb",
      control = list(eval.max = 10000)
      )

summary(gmmOpt1)


# Simulate One column data
set.seed(123)

# Reproducible

# Generate the data from normal distribution
n <- 200
x <- rnorm(n, mean = 4, sd = 2)
# set up the moment conditions for comparison
# MM (just identified)
g0 <- function(tet, x) {
  m1 <- (tet[1] - x)
  m2 <- (tet[2]^2 - (x - tet[1])^2)
  f <- cbind(m1, m2)
  return(f)
}

# GMM (over identified)
g1 <- function(tet, x) {
  m1 <- (tet[1] - x)
  m2 <- (tet[2]^2 - (x - tet[1])^2)
  m3 <- x^3 - tet[1] * (tet[1]^2 + 3 * tet[2]^2)
  f <- cbind(m1, m2, m3)
  return(f)
}

print(res0 <- gmm(g0, x, c(mu = 0, sig = 0)))
print(res1 <- gmm(g1, x, c(mu = 0, sig = 0)))

summary(res0)

summary(res1)

iv_res <- gamelevel %>%
  ivreg(utility ~ drug_se + drug_tweet_daily + drug_news + star + win + compete | ticket, data = .)

summary(iv_res)

