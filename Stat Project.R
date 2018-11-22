library(dplyr)
library(zoo)
beijing <- read.csv("C:/Users/alan/Desktop/all my stuff/Soph/Stat 410/Beijing.csv", header = TRUE)
# for model
head(beijing)
tail(beijing)

# dealing with NAs (approximations)
beijing <- beijing[,-1]
beijing$pm2.5 <- na.approx(beijing$pm2.5, na.rm = FALSE, rule = 2)

# indicator variable columns formation
#beijing <- beijing %>% mutate(Spr = as.numeric(month %in% c(3,4,5)))
#beijing <- beijing %>% mutate(Sum = as.numeric(month %in% c(6,7,8)))
#beijing <- beijing %>% mutate(Fal = as.numeric(month %in% c(9,10,11)))
#beijing <- beijing %>% mutate(Win = as.numeric(month %in% c(12,1,2)))


beijing <- beijing %>% mutate(Jan = as.numeric(month == 1))
beijing <- beijing %>% mutate(Feb = as.numeric(month == 2))
beijing <- beijing %>% mutate(Mar = as.numeric(month == 3))
beijing <- beijing %>% mutate(Apr = as.numeric(month == 4))
beijing <- beijing %>% mutate(May = as.numeric(month == 5))
beijing <- beijing %>% mutate(Jun = as.numeric(month == 6))
beijing <- beijing %>% mutate(Jul = as.numeric(month == 7))
beijing <- beijing %>% mutate(Aug = as.numeric(month == 8))
beijing <- beijing %>% mutate(Sep = as.numeric(month == 9))
beijing <- beijing %>% mutate(Oct = as.numeric(month == 10))
beijing <- beijing %>% mutate(Nov = as.numeric(month == 11))
beijing <- beijing %>% mutate(Dec = as.numeric(month == 12))
beijing <- beijing %>% mutate(Year10 = as.numeric(year == 2010))
beijing <- beijing %>% mutate(Year11 = as.numeric(year == 2011))
beijing <- beijing %>% mutate(Year12 = as.numeric(year == 2012))
beijing <- beijing %>% mutate(Year13 = as.numeric(year == 2013))
beijing <- beijing %>% mutate(Year14 = as.numeric(year == 2014))
beijing <- beijing %>% mutate(SW = as.numeric(cbwd == 'cv'))
beijing <- beijing %>% mutate(NE = as.numeric(cbwd == 'NE'))
beijing <- beijing %>% mutate(NW = as.numeric(cbwd == 'NW'))
beijing <- beijing %>% mutate(SE = as.numeric(cbwd == 'SE'))

# getting rid of categorical columns
#beijing <- subset(beijing, select = -c(cbwd, month))

# again for report
head(beijing)

# ihs transformation
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}

# bad models for report
# categorizing months
ml1 <- lm(pm2.5~ DEWP+ TEMP + PRES + DEWP:TEMP + DEWP:PRES + TEMP:PRES + DEWP:TEMP:PRES + day + hour + 
            ihs(Is)+ihs(Iws)+ ihs(Ir) + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug+ Sep + Oct+ Nov + Dec - 1, data = beijing)
summary(ml1)

# categorizing years
ml2 <- lm(ihs(pm2.5)~ DEWP+ TEMP + PRES + DEWP:TEMP + DEWP:PRES + TEMP:PRES + DEWP:TEMP:PRES + day + hour + month + 
            ihs(Is)+ihs(Iws)+ ihs(Ir) + Year10 + Year11 + Year12 + Year13 + Year14 - 1, data = beijing)
summary(ml2)

# categorizing cbwd
ml3 <- lm(ihs(pm2.5)~ DEWP+ TEMP + PRES + DEWP:TEMP + DEWP:PRES + TEMP:PRES + DEWP:TEMP:PRES + day + hour + month + year + 
            ihs(Is)+ihs(Iws)+ ihs(Ir) + SW + NE + NW + SE - 1, data = beijing)
summary(ml3)

# trial models
#ml1 <- lm(pm2.5~ DEWP + TEMP + PRES + log(Iws+0.00001) + log(Is+0.00001)+ log(Ir+0.00001) + day + hour, data = beijing)
#ml3 <- lm(pm2.5~ DEWP + TEMP + PRES + DEWP:TEMP + DEWP:PRES + year + TEMP:PRES + DEWP:TEMP:PRES + ihs(Iws) + ihs(Iws) + ihs(Iws) + day + hour -1, data = beijing)
#ml4 <- lm(pm2.5~ DEWP + TEMP + PRES + ihs(Iws) + ihs(Iws) + ihs(Iws) + day + hour + (Year10-1)+(Year11-1)+(Year12-1)+(Year13-1)+(Year14-1), data = beijing)
#ml <- lm(pm2.5~ DEWP + TEMP + PRES + log(Iws+0.00001) + Is^2 + Ir^2 + cv + NE + NW + SE + Jan + (Feb-1) + (Mar-1) + (Apr-1)+ (May-1)+(Jun-1)+(Jul-1)+(Aug-1)+(Sep-1)+(Oct-1)+(Nov-1)+(Dec-1)+Year10+Year12+Year13+Year14, data = beijing)
#ml2 <- lm(ihs(pm2.5)~ DEWP + TEMP +PRES + ihs(Iws)+ ihs(Is) + ihs(Ir) + Spr + Sum + Fal + Win - 1, data = beijing)
#ml1 <- lm(pm2.5~ DEWP  + TEMP + PRES + ihs(Iws)+ ihs(Ir) + Feb + Mar + Apr + May +Jul+Aug+ Sep + Nov + Dec - 1, data = beijing)

# optimal model
mlo <- lm(ihs(pm2.5)~ DEWP  + TEMP + ihs(Iws)+ ihs(Ir) + Jan + Feb + Mar + Apr + May + Jun +Aug+ Sep + Oct+ Nov + Dec - 1, data = beijing)
anova(mlo)
summary(mlo)

# AIC BIC- backwards
backAIC <- step(mlo,direction="backward", data=beijing)
backBIC <- step(mlo,direction="backward", data=beijing, k=log(43824))

# forward selection
#mint <- lm(pm2.5~1,data=beijing)
#forwardAIC <- step(mint,scope=list(lower=~1, 
#                                   upper=~ DEWP + TEMP + PRES*TEMP +DEWP*PRES + DEWP*PRES*TEMP + TEMP*DEWP+ ihs(Iws) + ihs(Is) + ihs(Ir) + day + hour +
#                                     (Jan-1) + (Feb-1) + (Mar-1) + (Apr-1)+ (May-1)+(Jun-1)+(Jul-1)+(Aug-1)+(Sep-1)+(Oct-1)+(Nov-1)+(Dec-1)),
#                   direction="forward", data=beijing)

# VIF
library(car)
vif(mlo)

# Checking Model Assumptions
par(mfrow=c(2,2))
plot(ml2)
abline(v=2*6/45,lty=2)

#  LASSO
x = model.matrix(mlo)
y = beijing$pm2.5
# setting parameters
library(glmnet)
set.seed(888)
train = sample(1:nrow(beijing), .7 * nrow(beijing))
test = (-train)
ytest = y[test]
lambda <- 10^seq(10, -2, length = 100)
# choosing best lambda
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
# lasso predictions
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = lambda)
plot(lasso.mod)
lasso.pred <-predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - ytest)^2)
# coefficient analysis
out = glmnet(x, y, alpha = 1, lambda = lambda)
lasso_coef = predict(out, type = "coefficients", s= bestlam)[1:18,]
lasso_coef

# predictions for missing values
beijing2 <- read.csv("C:/Users/alan/Desktop/all my stuff/Soph/Stat 410/Beijing.csv", header = TRUE)
beijing2 <- beijing2[,-1]
beijing2 <- beijing2 %>% mutate(Jan = as.numeric(month == 1))
beijing2 <- beijing2 %>% mutate(Feb = as.numeric(month == 2))
beijing2 <- beijing2 %>% mutate(Mar = as.numeric(month == 3))
beijing2 <- beijing2 %>% mutate(Apr = as.numeric(month == 4))
beijing2 <- beijing2 %>% mutate(May = as.numeric(month == 5))
beijing2 <- beijing2 %>% mutate(Jun = as.numeric(month == 6))
beijing2 <- beijing2 %>% mutate(Jul = as.numeric(month == 7))
beijing2 <- beijing2 %>% mutate(Aug = as.numeric(month == 8))
beijing2 <- beijing2 %>% mutate(Sep = as.numeric(month == 9))
beijing2 <- beijing2 %>% mutate(Oct = as.numeric(month == 10))
beijing2 <- beijing2 %>% mutate(Nov = as.numeric(month == 11))
beijing2 <- beijing2 %>% mutate(Dec = as.numeric(month == 12))
testt <- subset(beijing2, is.na(beijing2$pm2.5))
head(testt)
inv.ihs <- function(x) {
  y <- sqrt((exp(x) - x)^2 - 1)
  return(y)
}
pm2.5_predicted <- inv.ihs(predict(mlo, newdata = testt))

head(cbind(testt, pm2.5_predicted))
