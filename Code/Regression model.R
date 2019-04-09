##### Read data

setwd('Dropbox/Work/Courses/Berkeley/STAT 222/STAT-222-Capstone-Project')
data <- read.csv('Data/deathrate.csv')

##### Clean data

# Drop redundant index column
data$X <- NULL

# Drop counties with missing poverty data
data <- data[data$County != 'Kalawao County, HI',]
data <- data[data$County != 'Mc Kean County, PA',]

# Drop duplicate rows
data <- data[-c(788, 3924, 7060, 10196),]

# TO DO: Merge data with cities.csv to get latitudes and longitudes for mapping

##### Model

## Linear regression

# Single county

county_name = 'Alameda County, CA'
alameda <- data[data$County == county_name, ]
alameda_y <- exp(alameda[alameda$Year %in% 2000:2014, ]$Deathrate)
alameda_x <- exp(alameda[alameda$Year %in% 1999:2013, ]$Deathrate)
alameda_X <- diag(alameda_x)
beta <- coef(lm(alameda_y ~ alameda_X + 0))
# Note: We should probably look into changing the linear model
# to a generalized linear model to account for the count data.
# The poisson distribution may be a more accurate model for our data.
plot(beta)

# Multiple counties

unique_counties <- c('Alameda County, CA', 'Sonoma County, CA', 'Marin County, CA')
# unique_counties <- unique(data$County) 
# Change code for all counties
# Note: This throws an error when calling as.matrix below. 
# We need to figure out how to solve this memory issue
n <- length(unique_counties)
y_list <- vector("list", n)
X_list <- vector('list', n)

i <- 1
for (county_name in unique_counties) {
  county <- data[data$County == county_name,]
  county_y <- exp(county[county$Year %in% 2000:2014, ]$Deathrate)
  county_x <- exp(county[county$Year %in% 1999:2013, ]$Deathrate)
  county_X <- diag(county_x)
  y_list[[i]] <- county_y
  X_list[[i]] <- county_X
  i <- i+1
}

library(Matrix)

y <- do.call(c, y_list)
X <- do.call(bdiag, X_list)
X <- as.matrix(X)
beta <- coef(lm(y ~ X + 0))
plot(log(beta))

## Regularized regression

# We would like to regularize our answer towards the all ones vector
# To do this we adopt the approach described here:
# https://stats.stackexchange.com/questions/16604/computing-ridge-regression-with-prior-different-from-0

ridge <- function(y, X, lambda, prior_mean) {
  n <- dim(X)[1]
  p <- dim(X)[2]
  return(solve((t(X) %*% X + lambda * diag(p)), t(X) %*% y + lambda * prior_mean))
}

# To do: Figure out how to incorporate covariance of prior

# Single county

# Should recover original linear regression 
beta <- ridge(alameda_y, alameda_X, lambda = 1000000, rep(1,15))
delta <- log(beta)
plot(delta)

# Should get closer to all ones vector as lambda increases
beta <- ridge(alameda_y, alameda_X, lambda = 10000, rep(1,15))
delta <- log(beta)
plot(delta)

# Multiple counties

beta <- ridge(y, X, lambda = 0, rep(1,length(y)))
delta <- log(beta)
plot(delta)

