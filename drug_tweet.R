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





rnorm(n=21107, mean=-0.1, sd=0.5)
obj_a1_lowest <- 0.9009813
obj_a1_lowest







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







----------------------------------------------------
  
  
  func1 <- function(x) {
    x^2 - 10
  }

curve(func1, col = 'blue', lwd = 2, from = 0, n = 100, xlim=c(0, 5), ylab='f(x)')
abline(a=0, b=0, lty = 5)

func2 <- function(x) {
  x^3 - 2* x - 5
}

uniroot(func2, c(2,3))

newton.raphson <- function(f, a, b, tol = 1e-5, n = 1000) {
  require(numDeriv) # Package for computing f'(x)
  
  x0 <- a # Set start value to supplied lower bound
  k <- n # Initialize for iteration results
  
  fa <- f(a)
  if (fa == 0.0) {
    return(a)
  }
  
  fb <- f(b)
  if (fb == 0.0) {
    return(b)
  }
  
  for (i in 1:n) {
    dx <- genD(func = f, x = x0)$D[1] # First-order derivative f'(x0)
    x1 <- x0 - (f(x0) / dx) # Calculate next value x1
    k[i] <- x1 # Store x1
    # Once the difference between x0 and x1 becomes sufficiently small, output the results.
    if (abs(x1 - x0) < tol) {
      root.approx <- tail(k, n=1)
      res <- list('root approximation' = root.approx, 'iterations' = k)
      return(res)
    }
    # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
    x0 <- x1
  }
  print('Too many iterations in method')
}


newton.raphson(func2, 2, 3)




square.root<-function(x,tol=1e-6,r=x/2)  #function to calculate the square roots
{
  n.iter=0  #number of iterations
  while(abs(r^2-x)>=tol)  #condition to check for a defined level of tolerance
  {
    r=(r+x/r)/2  #
    n.iter=n.iter+1  #number of iterations is incremented
  }
  output<-list(r,n.iter)
  names(output)<-c("x.sqrt","n.iter")
  return(output)
}

square.root(10)

square.root(99)

square.root(100)

square.root(c(10,99,100))


square.root1<-function(x,tol=1e-6,r=x/2)#function to calculate the square roots
{
  output<-data.frame(x.sqrt=double(),n.iter=double())  #create a blank data frame
  n.iter=rep(0,times=length(x))  #a vector of zeroes
  i=1  #iteration counter
  while(i<=length(x))  #iterate through all the values of the vector
  {
    while(abs(r[i]^2-x[i])>=tol)  #the checking condition for the tolerance
    {
      r[i]=(r[i]+x[i]/r[i])/2
      n.iter[i]=n.iter[i]+1  #the number of iterations is incremented
    }
    result<-data.frame(r[i],n.iter)  #store the results in another data frame
    names(result)<-c("x.sqrt","n.iter")  #name the columns
    rbind(output,result)  #append the result data frame to the output data frame
    i=i+1  #iterate the looping counter
  }
  return(output)  #return the output data frame
}


square.root1(5)

square.root<-function(x, tol=1e-6, r=x/2)  #function to calculate the square roots
{
  n.iter=0  #number of iterations
  while(any(abs(r^2-x)>=tol))  #"any" added here - loops while any values greater than tol
  {
    n.iter=n.iter+(abs(r^2-x)>=tol)  #only increases n.iter for errors greater than tol
    r=(r+x/r)/2        #swapped with line above to get right value in n.iter calculation       
  }
  output<-list(r,n.iter)
  names(output)<-c("x.sqrt","n.iter")
  return(output)
}

square.root(c(10,99,100))




---------------------------------------------------------------------------------
  
  
  newtonRaphson.basic(f, fprime, a, b, 
                      tol = 1e-8, n.Seq = 20,
                      nmax = 15, ...)





obj_a_lowest