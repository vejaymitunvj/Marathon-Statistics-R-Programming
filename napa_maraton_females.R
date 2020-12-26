## Read in Napa Valley Marathon data

napaf2015 <- read.csv(
"http://www.stat.ufl.edu/~winner/data/napa_marathon_fm2015.csv",
header=T)
attach(napaf2015)    ## Can refer to vars in napa2015f directly
names(napaf2015)     ## Prints variable names in napa2015f
levels(Gender)       ## Find out levels and labels for variable Gender

## Create a variable named f.mph which contains the mph's of females
f.mph <- mph[Gender == "F"]

## Obtain the mean and variance of f.mph
(mean.f <- mean(f.mph))
(var.f <- var(f.mph))

## Use the method of moments to obtain parameters of Gamma(alpha,beta) dist
##    with same mean and variance as the data
## mean = alpha / beta   var = alpha / beta^2
## =>  beta = mean / var    alpha = beta * mean = mean^2 / var
(beta.f <- mean.f / var.f)
(alpha.f <- mean.f^2 / var.f)




## Set up plot for histogram of data and superimposed gamma density
breaks.lo <- floor(min(f.mph))   ## Assures lower bound below all data
breaks.hi <- ceiling(max(f.mph)) ##    "    upper   "   above  "    "
breaks.width <- 0.20             ## Sets binwidth at 0.20 (data specific)
breaks.data <- seq(breaks.lo, breaks.hi, breaks.width)
breaks.dens <- seq(breaks.lo, breaks.hi, 0.01)

## Generate Relative Frequency Histogram and superimposed gamma density
win.graph(height=5.5, width=7.0)
hist(f.mph, breaks=breaks.data, xlab="mph", main="Females", freq=F)
lines(breaks.dens, dgamma(breaks.dens,alpha.f,beta.f))


## Compare quantiles of data (empirical) and gamma (theoretical) distributions
q.list <- c(.10,.25,.50,.75,.90)
q.data <- quantile(f.mph, q.list)
q.gamma <- qgamma(q.list, alpha.f, beta.f)

## Combine columns of quantiles, data and gamma for "nicer printing"
##   include column names and print with 4 decimal places
q.out <- cbind(q.list, q.data, q.gamma)
colnames(q.out) <- c("Quantile", "Data", "Gamma")
round(q.out, 4)

## Compare probabilities of ranges for data and gamma distributions
##  Range 1 =(0,5], Range 2 = (5,8) Range 3 = [8,INF) (Complement of (0,8))
p.data1 <- sum(f.mph <= 5) / length(f.mph)
p.data2 <- sum((f.mph > 5) & (f.mph < 8)) / length(f.mph)
p.data3 <- sum(f.mph >= 8) / length(f.mph)
p.gamma1 <- pgamma(5, alpha.f, beta.f)
p.gamma2 <- pgamma(8, alpha.f, beta.f) - pgamma(5, alpha.f, beta.f)
p.gamma3 <- 1 - pgamma(8, alpha.f, beta.f)

## Stack results from ranges within data and gamma, then combine columns
##  include row names (ranges) and column names
p.data <- rbind(p.data1, p.data2, p.data3)
p.gamma <- rbind(p.gamma1, p.gamma2, p.gamma3)
p.out <- cbind(p.data, p.gamma)
rownames(p.out) <- c("<=5","5-8",">=8")
colnames(p.out) <- c("Data", "Gamma")
round(p.out, 4)


  