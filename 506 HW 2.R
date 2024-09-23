##################
### Question 1 ###
##################
### (a) ###
# Version 1
game.loop = function(value)
{
  # First we need to check whether the input is valid.(remember to add)
  # Main Game Part
  roll = sample(1:6, size = value, replace = TRUE)
  winning = 0
  for(i in 1:value)
  {
    if(roll[i] == 3 || roll[i] == 5)
    {
      winning = winning + 2
    }else
    {
      winning = winning - 2
    }
  }
  return(winning)
}


# Version 2
game.vectorized = function(value)
{
  # First we need to check whether the input is valid.(remember to add)
  # Main Game Part
  roll = sample(1:6, size = value, replace = TRUE)
  winning = ifelse(roll == 3 | roll == 5, 2, -2)
  return(sum(winning))
}


# Version 3
game.table = function(value)
{
  # First we need to check whether the input is valid.(remember to add)
  # Main Game Part
  roll = sample(1:6, size = value, replace = TRUE)
  new.roll = as.data.frame(table(roll))
  new.roll$roll = as.numeric(as.character(new.roll$roll))
  new.roll$Freq = as.numeric(as.character(new.roll$Freq))
  win3 = sum(new.roll[which(new.roll$roll==3),2])
  win5 = sum(new.roll[which(new.roll$roll==5),2])
  fail = sum(new.roll$Freq)-win3-win5
  winning = 2*win3+2*win5-2*fail
  return(winning)
}


# Version 4
game.vapply = function(value)
{
  # First we need to check whether the input is valid.(remember to add)
  # Main Game Part
  roll = sample(1:6, size=value, replace = TRUE)

  winning = sum(vapply(roll, function(x){
    if (x == 3 || x == 5)
    {
      return(2)
    } else {
      return(-2)
    }
  },numeric(1)
  ))
  return(winning)
}





### (b) ###
value1  = 3
game.loop(value1)
game.vectorized(value1)
game.table(value1)
game.vapply(value1)

value2 = 3000
game.loop(value2)
game.vectorized(value2)
game.table(value2)
game.vapply(value2)



### (c) ###
set.seed(1506)
game.loop(3000)

set.seed(1506)
game.vectorized(3000)

set.seed(1506)
game.table(3000)

set.seed(1506)
game.vapply(3000)


### (d) ###
library(microbenchmark)
#set.seed(1506)
microbenchmark(game.loop(1000),
               game.vectorized(1000),
               game.table(1000),
               game.vapply(1000))


#set.seed(506)
microbenchmark(game.loop(1000000),
               game.vectorized(1000000),
               game.table(1000000),
               game.vapply(1000000))



### (e) ### 
value = 10000000
MC.result = game.vectorized(value)/value
MC.result







##################
### Question 2 ###
##################
### (a) ###
data = read.csv("C:/Users/z1883/Desktop/cars.csv",header = TRUE)
colnames(data) = c("Height","Length","Width","Driveline",
                   "Engine.Type","Hybrid","Number.of.Forward.Gears",
                   "Transmission","City.mpg","Fuel.Type","Highway.mpg",
                   "Classification","ID","Make","Model.Year","Year",
                   "Horsepower","Torque")
head(data)

### (b) ###
gasoline.data = data[which(data$Fuel.Type == "Gasoline"),]
head(gasoline.data)

### (c) ###
library(e1071)
# Box-Cox Transformation
library(MASS)
sum(abs(gasoline.data$Highway.mpg) != gasoline.data$Highway.mpg)
# the result meaning that all the Highway.mpg are non-negative\
# We can use Box-Cox transformation
highwayMPG = 0
model = lm(Highway.mpg~Torque+Horsepower+Height+Length+Width
           +as.factor(Year), data=gasoline.data)

b = boxcox(model)
# Exact lambda
lambda <- b$x[which.max(b$y)] 
lambda

if(lambda != 0)
{
  highwayMPG=(gasoline.data$Highway.mpg^lambda - 1)/lambda
  
}else{
  highwayMPG=log(gasoline.data$Highway.mpg)
}

skewness(gasoline.data$Highway.mpg)
skewness(highwayMPG)
hist(gasoline.data$Highway.mpg,xlim = c(0,50))
hist(highwayMPG)

qqnorm(gasoline.data$Highway.mpg, pch = 1, frame = FALSE)
qqline(gasoline.data$Highway.mpg, col = "steelblue", lwd = 2)

qqnorm(highwayMPG, pch = 1, frame = FALSE)
qqline(highwayMPG, col = "steelblue", lwd = 2)

new.gasoline = cbind(gasoline.data, highwayMPG)
head(new.gasoline)


### (d) ###
model1 = lm(highwayMPG~Torque+Horsepower+Height+Length+Width
           +as.factor(Year), data=new.gasoline)
summary(model1)


### (e) ###












### (f) ###
new.gasoline$Year = as.factor(new.gasoline$Year)
X = model.matrix(highwayMPG~Torque+Horsepower+Height+Length+Width
                 +Year, data = new.gasoline)
head(X)
beta.hat = solve(t(X)%*%X)%*%(t(X)%*%new.gasoline$highwayMPG)
beta.hat





























































































































