rm(list=ls())

nonlin <- read.csv("C:/Users/LG/Desktop/Drug issues_MLB/drug/22total_07-19.csv", header=T, na.strings=c("."))


# Non-linear

attach(nonlin)
rss <- function(beta) {
  beta1 <-  beta[1]
  beta2 <-  beta[2]
  fit <- beta1*(1-exp(-beta2*x))
  err <- y-fit
  returns(sum(err*err))
}

optim(fn=rss, par=c(1, 0, 5))

opt <- optim(fn=rss, par=c(1,0))
beta <- opt$par
beta1 <- beta[1]
beta2 <- beta[2]
fit <- beta1*
  
plot(x, y, pch=21)
par(new=T)
plot(x, fit, pch=19, cex=0.6, col="red", ylab="", ylim=c(0,1))



-----# Newton Raphson code 1#-----

f <- function(x){
  z = x*exp(x) - cos(x)
  return(z)
}

fdash <- function(x){
  z=exp(x)*(1 + x) + sin(x)
  return(z)
}

x0 = as.double(readline(prompt="Enter a point on function"))
n=1
while(n<100){
  n <- n+1
  y0 = f(x0)
  y1 = fdash(x0)
  if(y1 < 0.0000001){
    break(0)
  }
  x1 <- x0 - (y0/y1)
  if(abs((x0-x1)/x1) < 0.0000001){
    print("convergent")
    
    break(0)
  }
  x0 <- x1
  print(paste("interation no.",n,"solvalue of root",x0))
}



-----# optimization other2#-----

install.packages("Rglpk")
library(Rglpk)

obj <- c(2, 4, 3)
mat <- matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(60, 40, 80)
max <- TRUE
Rglpk_solve_LP(obj, mat, dir, rhs, max = max)


install.packages("rootoned")
require("rootoned")
alpha <- 1
efn <- function(x, alpha) {
  exp(-alpha * x) - 0.2
}
tint <- c(0, 100)
resr1 <- uniroot(efn, tint, tol = 1e-10, alpha = 1)



#10 - 3*x + x^3
z <- c(10, -3, 0, 1)
require(polynom)
allroots <- polyroot(z)
print(allroots)



ljfn <- function(r, ep = 0.6501696, sig = 0.3165555) {
  fn <- 4 * ep * ((sig/r)ˆ12 - (sig/r)ˆ6)
}
min <- optimize(ljfn, interval = c(0.001, 5))
print(min)





-----# optimization other3(newton)#-----

newton <- function(fun, tol = 1e-7, x0 = 1, N = 300){
  h <- 1e-7
  i <- 1
  x1 <- x0
  p <- numeric(N)
  while(i <= N){
    df.dx <- (fun(x0 + h) - fun(x0)) / h
    x1 <- (x0 - (fun(x0) / df.dx))
    p[i] <- x1
    i = i+1
    if(abs(x1 - x0) < tol) break
    x0 = x1
  }
  return(p[1:(i - 1)])
}

fun <- function(x) x^3+2
newton(fun, x0 = 2)



----------------------------------# newton main optimization
  
  install.packages("readxl")
install.packages("lmtest")

library(readxl)
library(dplyr)
library(ggplot2)

total <- read_excel("22total_07-19.xlsx")
tota22 <- read.csv("22total_07-19.csv")

tota22

str(tota22)
View(tota22)

x <- select(tota22, be_t)
y <- select(tota22, utility)


NonLinear = lm(y2 ~ x, data = total)
summary(NonLinear)



newton <- function(fun, tol = 1e-7, x0 = 1, N = 1000){
  h <- 1e-7
  i <- 1
  x1 <- x0
  p <- numeric(N)
  while(i <= N){
    df.dx <- (fun(x0 + h) - fun(x0)) / h
    x1 <- (x0 - (fun(x0) / df.dx))
    p[i] <- x1
    i = i+1
    if(abs(x1 - x0) < tol) break
    x0 = x1
  }
  return(p[1:(i - 1)])
}


fun <- function(x) x^3+2
newton(fun, x0 = 2)


p <- ggplot(data.frame(x = c(-2, 2)), aes(x = x)) + 
  stat_function(fun = fun, color = "blue") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

d <- data.frame(label = 1:length(newton(fun, x0 = 2)), 
                x = newton(fun, x0 = 2), 
                y = fun(newton(fun, x0 = 2)))

p2 <- p + geom_point(data = d, aes(x = x, y = y, frame = label)) + 
  geom_text(data = d, aes(x = x, y = y + 20, label = label, frame = label), size = 3)

p2





--------------------------------------------------------------------------
  
  
  
  newton <- function(fun, tol = 1e-7, x0 = 1, N = 1000){
    h <- 1e-7
    i <- 1
    x1 <- x0
    p <- numeric(N)
    while(i <= N){
      df.dx <- (fun(x0 + h) - fun(x0)) / h
      x1 <- (x0 - (fun(x0) / df.dx))
      p[i] <- x1
      i = i+1
      if(abs(x1 - x0) < tol) break
      x0 = x1
    }
    return(p[1:(i - 1)])
  }


fun <- function(x) x^3+2
newton(fun, x0 = 2)


p <- ggplot(data.frame(x = c(-2, 2)), aes(x = x)) + 
  stat_function(fun = fun, color = "blue") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

d <- data.frame(label = 1:length(newton(fun, x0 = 2)), 
                x = newton(fun, x0 = 2), 
                y = fun(newton(fun, x0 = 2)))

p2 <- p + geom_point(data = d, aes(x = x, y = y, frame = label)) + 
  geom_text(data = d, aes(x = x, y = y + 20, label = label, frame = label), size = 3)

p2





--------------------------------------------------------------------------------
  
  
  
  newton <- function(fun, tol = 1e-7, x0 = 1, N = 1000){
    h <- 1e-7
    i <- 1
    x1 <- x0
    p <- numeric(N)
    while(i <= N){
      df.dx <- (fun(x0 + h) - fun(x0)) / h
      x1 <- (x0 - (fun(x0) / df.dx))
      p[i] <- x1
      i = i+1
      if(abs(x1 - x0) < tol) break
      x0 = x1
    }
    return(p[1:(i - 1)])
  }


fun <- function(x) x^3+2
newton(fun, x0 = 2)


p <- ggplot(data.frame(x = c(-2, 2)), aes(x = x)) + 
  stat_function(fun = fun, color = "blue") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

d <- data.frame(label = 1:length(newton(fun, x0 = 2)), 
                x = newton(fun, x0 = 2), 
                y = fun(newton(fun, x0 = 2)))

p2 <- p + geom_point(data = d, aes(x = x, y = y, frame = label)) + 
  geom_text(data = d, aes(x = x, y = y + 20, label = label, frame = label), size = 3)

p2




---------------------------------------------------------------------------------
  
  
  
  newton <- function(fun, tol = 1e-7, x0 = 1, N = 1000){
    h <- 1e-7
    i <- 1
    x1 <- x0
    p <- numeric(N)
    while(i <= N){
      df.dx <- (fun(x0 + h) - fun(x0)) / h
      x1 <- (x0 - (fun(x0) / df.dx))
      p[i] <- x1
      i = i+1
      if(abs(x1 - x0) < tol) break
      x0 = x1
    }
    return(p[1:(i - 1)])
  }


fun <- function(x) x^3+2
newton(fun, x0 = 2)

rnorm(n=21107, mean=-0.02, sd=1)
obj_a_lowest <- 0.9009813
obj_a_lowest


p <- ggplot(data.frame(x = c(-2, 2)), aes(x = x)) + 
  stat_function(fun = fun, color = "blue") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

d <- data.frame(label = 1:length(newton(fun, x0 = 2)), 
                x = newton(fun, x0 = 2), 
                y = fun(newton(fun, x0 = 2)))

p2 <- p + geom_point(data = d, aes(x = x, y = y, frame = label)) + 
  geom_text(data = d, aes(x = x, y = y + 20, label = label, frame = label), size = 3)

p2


p <- ggplot(data.frame(x = c(-2, 2)), aes(x = x)) + 
  stat_function(fun = fun, color = "blue") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

d <- data.frame(label = 1:length(newton(fun, x0 = 2)), 
                x = newton(fun, x0 = 2), 
                y = fun(newton(fun, x0 = 2)))

p2 <- p + geom_point(data = d, aes(x = x, y = y, frame = label)) + 
  geom_text(data = d, aes(x = x, y = y + 20, label = label, frame = label), size = 3)

p2


----------------------------------------------------------------------------------
  
  
  
  
  
  newton <- function(fun, tol = 1e-7, x0 = 1, N = 1000){
    h <- 1e-7
    i <- 1
    x1 <- x0
    p <- numeric(N)
    while(i <= N){
      df.dx <- (fun(x0 + h) - fun(x0)) / h
      x1 <- (x0 - (fun(x0) / df.dx))
      p[i] <- x1
      i = i+1
      if(abs(x1 - x0) < tol) break
      x0 = x1
    }
    return(p[1:(i - 1)])
  }


fun <- function(x) x^2
newton(fun, x0 = 2)


p <- ggplot(data.frame(x = c(-2, 2)), aes(x = x)) + 
  stat_function(fun = fun, color = "blue") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

d <- data.frame(label = 1:length(newton(fun, x0 = 2)), 
                x = newton(fun, x0 = 2), 
                y = fun(newton(fun, x0 = 2)))

p2 <- p + geom_point(data = d, aes(x = x, y = y, frame = label)) + 
  geom_text(data = d, aes(x = x, y = y + 20, label = label, frame = label), size = 3)

p2
