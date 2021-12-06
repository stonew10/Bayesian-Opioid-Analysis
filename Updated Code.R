library(dplyr)
library(data.table)
library(ggplot2)
library(readr)

Opioid_Data <- read_csv("Opioid Data.csv")
df.1 <- Opioid_Data
## Population Data
#df.1 <- read.csv()
df.1$Age <- factor(df.1$Age, levels = c("1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64"))

## Plots
plot(Deaths~Year, df.1)

####################
# Total Death Data #
###################
par(mfrow=c(1,1))
## Group by year and Race
s.year.race <- df.1 %>% 
  group_by(Year,Race) %>%
  summarize(tot_Deaths=sum(Deaths))

ggplot(s.year.race, aes(x=Year, y=tot_Deaths, shape=Race, color=Race)) +
  geom_line() +
  #geom_text(aes(label=tot_Deaths), size=3, hjust=.5, vjust=-.5)+
  theme(
    plot.title = element_text(hjust = .5),
    legend.position = "bottom")+
  ggtitle("Deaths by Race")+
  ylab("Deaths")

## Group by year and Gender
s.year.gender <- df.1 %>% 
  group_by(Year,Gender) %>%
  summarize(tot_Deaths=sum(Deaths))

ggplot(s.year.gender, aes(x=Year, y=tot_Deaths, shape=Gender, color=Gender)) +
  geom_line() +
  #geom_text(aes(label=tot_Deaths), size=3, hjust=.5, vjust=-.5)+
  theme(plot.title = element_text(hjust = .5),
        legend.position = "bottom")+
  ggtitle("Deaths by Gender")+
  ylab("Deaths")

## Group by year and age
s.year.age <- df.1 %>% 
  group_by(Year,Age) %>%
  summarize(tot_Deaths=sum(Deaths))

ggplot(s.year.age, aes(x=Year, y=tot_Deaths, color=Age)) +
  #geom_point() +
  geom_line() +
  theme(plot.title = element_text(hjust = .5))+
  ggtitle("Deaths by Age")+
  ylab("Deaths")

# Ages between 20 to 69
s.year.age_f <- filter(s.year.age, as.integer(Age) %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
#c(5,6,7,8,9,10,12,13,14)

ggplot(s.year.age_f, aes(x=Year, y=tot_Deaths, color=Age)) +
  #geom_point() +
  geom_line() +
  #geom_text(aes(label=tot_Deaths), size=3, hjust=.5, vjust=-.5)+
  theme(
    plot.title = element_text(hjust = .5),
    legend.position = "bottom")+
  ggtitle("Deaths by Age \n Ages 20 to 59")+
  ylab("Deaths")

#Group by Year, Gender, Race, and Age
s.all <- df.1 %>%
  group_by(Year,Gender,Race,Age)%>%
  summarize(tot_Deaths=sum(Deaths))

#white Black Males of Age between 20 to 69
s.all.m <- filter(s.all, as.integer(Gender) %in% c(2))
s.all.m.age <- filter(s.all.m, as.integer(Age) %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
s.all.m.age.r <- filter(s.all.m.age, as.integer(Race) %in% c(3,4))

ggplot(s.all.m.age.r, aes(x=Year, y=tot_Deaths, shape=Race, color=Age)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=tot_Deaths), size=3, hjust=.5, vjust=-.5)+
  theme(plot.title = element_text(hjust = .5),
        legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=7))+
  ggtitle("Deaths by White and Black Males \n Ages 20 to 69")+
  ylab("Deaths")

#############################
# Crude Rate by 100,000 pop #
############################

## Group by year and Race
c.year.race <- df.1 %>% 
  group_by(Year,Race) %>%
  summarize(CR=sum(Deaths)/sum(Population)*100000)

ggplot(c.year.race, aes(x=Year, y=CR, shape=Race, color=Race)) +
  #geom_point() +
  geom_line() +
  #geom_text(aes(label=round(CR,2)), size=3, hjust=.5, vjust=-.5)+
  theme(plot.title = element_text(hjust = .5),
        legend.position = "bottom")+
  ggtitle("Crude Rate by Race")+
  ylab("Crude Rate (per 100,000)")

## Group by year and Gender
c.year.gender <- df.1 %>% 
  group_by(Year,Gender) %>%
  summarize(CR=sum(Deaths)/sum(Population)*100000)

ggplot(c.year.gender, aes(x=Year, y=CR, shape=Gender, color=Gender)) +
  #geom_point() +
  geom_line() +
  #geom_text(aes(label=round(CR,2)), size=3, hjust=.5, vjust=-.5)+
  theme(plot.title = element_text(hjust = .5),
        legend.position = "bottom")+
  ggtitle("Crude Rate by Gender")+
  ylab("Crude Rate (per 100,000)")

## Group by year and age
c.year.age <- df.1 %>% 
  group_by(Year,Age) %>%
  summarize(CR=sum(Deaths)/sum(Population)*100000)

ggplot(c.year.age, aes(x=Year, y=CR, color=Age)) +
  #geom_point() +
  geom_line() +
  theme(plot.title = element_text(hjust = .5))+
  ggtitle("Crude Rate by Age")+
  ylab("Crude Rate (per 100,000)")

# Ages between 20 to 69
c.year.age_f <- filter(c.year.age, as.integer(Age) %in% c(5,6,7,8,9,10,12,13,14))

ggplot(c.year.age_f, aes(x=Year, y=CR, color=Age)) +
  #geom_point() +
  geom_line() +
  #geom_text(aes(label=round(CR,2)), size=3, hjust=.5, vjust=-.5)+
  theme(plot.title = element_text(hjust = .5),
        legend.position = "bottom")+
  ggtitle("Crude Rate by Gender")+
  ylab("Crude Rate (per 100,000)")

#Group by Year, Gender, Race, and Age
c.all <- df.1 %>%
  group_by(Year,Gender,Race,Age)%>%
  summarize(CR=sum(Deaths)/sum(Population)*100000)

#white Black Males of Age between 20 to 49
c.all.m <- filter(c.all, as.integer(Gender) %in% c(2))
c.all.m.age <- filter(c.all.m, as.integer(Age) %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
c.all.m.age.r <- filter(c.all.m.age, as.integer(Race) %in% c(3,4))

ggplot(c.all.m.age.r, aes(x=Year, y=CR, shape=Race, color=Age)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=round(CR,2)), size=3, hjust=.5, vjust=-.5)+
  theme(plot.title = element_text(hjust = .5),
        legend.position = "bottom",
        legend.title = element_text(size=10),
        legend.text = element_text(size=7))+
  ggtitle("Crude Rate by White and Black Males \n Ages 20 to 69")+
  ylab("Crude Rate")


############
## Modeling
############

library(fastDummies)
require(devtools)
require(ggplot2)
require(gridExtra)
require(ProbBayes)
require(tidyverse)
require(runjags)
require(coda)

dat2 <- read_csv("Opioid DataT.csv")
#dat2 <- read.csv()
str(dat2)

#prepare our JAGS script
## create indictor variable for Sex. Female is Reference
dat2$Sex_F = fastDummies::dummy_cols(dat2$Sex)[,names(fastDummies::dummy_cols(dat2$Sex)) == ".data_0"]
dat2$Sex_M = fastDummies::dummy_cols(dat2$Sex)[,names(fastDummies::dummy_cols(dat2$Sex)) == ".data_1"]

## create indicator variables for Race.  Native American (1), Asian/Pacific(2), Black(3), White(4)
dat2$Race_AI = fastDummies::dummy_cols(dat2$Race1)[,names(fastDummies::dummy_cols(dat2$Race1)) == ".data_1"]
dat2$Race_API = fastDummies::dummy_cols(dat2$Race1)[,names(fastDummies::dummy_cols(dat2$Race1)) == ".data_2"]
dat2$Race_Black = fastDummies::dummy_cols(dat2$Race1)[,names(fastDummies::dummy_cols(dat2$Race1)) == ".data_3"]
dat2$Race_White = fastDummies::dummy_cols(dat2$Race1)[,names(fastDummies::dummy_cols(dat2$Race1)) == ".data_4"]

##create indicator variables for age. Age 20-24 is reference
dat2$Age1_20 = fastDummies::dummy_cols(dat2$Age1)[,names(fastDummies::dummy_cols(dat2$Age1)) == ".data_1"]
dat2$Age1_25 = fastDummies::dummy_cols(dat2$Age1)[,names(fastDummies::dummy_cols(dat2$Age1)) == ".data_2"]
dat2$Age1_30 = fastDummies::dummy_cols(dat2$Age1)[,names(fastDummies::dummy_cols(dat2$Age1)) == ".data_3"]
dat2$Age1_35 = fastDummies::dummy_cols(dat2$Age1)[,names(fastDummies::dummy_cols(dat2$Age1)) == ".data_4"]
dat2$Age1_40 = fastDummies::dummy_cols(dat2$Age1)[,names(fastDummies::dummy_cols(dat2$Age1)) == ".data_5"]
dat2$Age1_45 = fastDummies::dummy_cols(dat2$Age1)[,names(fastDummies::dummy_cols(dat2$Age1)) == ".data_6"]
dat2$Age1_50 = fastDummies::dummy_cols(dat2$Age1)[,names(fastDummies::dummy_cols(dat2$Age1)) == ".data_7"]
dat2$Age1_55 = fastDummies::dummy_cols(dat2$Age1)[,names(fastDummies::dummy_cols(dat2$Age1)) == ".data_8"]
dat2$Age1_60 = fastDummies::dummy_cols(dat2$Age1)[,names(fastDummies::dummy_cols(dat2$Age1)) == ".data_9"]

#JAGS code for the model
modelString <-"
model {
## sampling
for (i in 1:N){
y[i] ~ dnorm(Intercept + 
Female*x_female[i] + Male*x_male[i] + 
American.Indian*x_race_AI[i] + Asian.Pacific.Islander*x_race_API[i] + African.American*x_race_B[i] + White*x_race_W[i] +
Age.20.24*x_age_20[i] + Age.25.29*x_age_25[i] + Age.30.34*x_age_30[i] + Age.35.39*x_age_35[i] + 
Age.40.44*x_age_40[i] + Age.45.49*x_age_45[i] + Age.50.54*x_age_50[i] + Age.55.59*x_age_55[i] + 
Age.60.64*x_age_60[i] + Time*Year[i], invsigma2)
}
## priors
Intercept ~ dnorm(mu0, g0)
Female ~ dnorm(mu1, g1)
Male ~ dnorm(mu2, g2)
American.Indian ~ dnorm(mu3, g3)
Asian.Pacific.Islander ~ dnorm(mu4, g4)
African.American ~ dnorm(mu5, g5)
White ~ dnorm(mu6, g6)
Age.20.24 ~ dnorm(mu7, g7)
Age.25.29 ~ dnorm(mu8, g8)
Age.30.34 ~ dnorm(mu9, g9)
Age.35.39 ~ dnorm(mu10, g10)
Age.40.44 ~ dnorm(mu11, g11)
Age.45.49 ~ dnorm(mu12, g12)
Age.50.54 ~ dnorm(mu13, g13)
Age.55.59 ~ dnorm(mu14, g14)
Age.60.64 ~ dnorm(mu15, g15)
Time ~ dnorm(mu16, g16)
invsigma2 ~ dgamma(a, b)
sigma <- sqrt(pow(invsigma2, -1))
}
"    
#Pass the data and hyperparameter values to JAGS:
y = as.vector(dat2$CR.100)
x_female = as.vector(dat2$Sex_F)
x_male = as.vector(dat2$Sex_M)
x_race_AI = as.vector(dat2$Race_AI)
x_race_B = as.vector(dat2$Race_Black)
x_race_API = as.vector(dat2$Race_API)
x_race_W = as.vector(dat2$Race_White)
x_age_20 = as.vector(dat2$Age1_20)
x_age_25 = as.vector(dat2$Age1_25)
x_age_30 = as.vector(dat2$Age1_30)
x_age_35 = as.vector(dat2$Age1_35)
x_age_40 = as.vector(dat2$Age1_40)
x_age_45 = as.vector(dat2$Age1_45)
x_age_50 = as.vector(dat2$Age1_50)
x_age_55 = as.vector(dat2$Age1_55)
x_age_60 = as.vector(dat2$Age1_60)
Year = as.vector(dat2$Year)
N = length(y)  # Compute the number of observations

the_data <- list("y" = y,
                 "x_female" = x_female, "x_male" = x_male,
                 "x_race_AI" = x_race_AI, "x_race_API" = x_race_API, "x_race_B" = x_race_B, "x_race_W" = x_race_W,
                 "x_age_20" = x_age_20, "x_age_25" = x_age_25, "x_age_30" = x_age_30, "x_age_35" = x_age_35,
                 "x_age_40" = x_age_40, "x_age_45" = x_age_45, "x_age_50" = x_age_50, "x_age_55" = x_age_55,
                 "x_age_60" = x_age_60, "Year" = Year, "N" = N,
                 "mu0" = 0, "g0" = 1,
                 "mu1" = 0, "g1" = 1,
                 "mu2" = 0, "g2" = 1, 
                 "mu3" = 0, "g3" = 1,
                 "mu4" = 0, "g4" = 1, 
                 "mu5" = 0, "g5" = 1,
                 "mu6" = 0, "g6" = 1, 
                 "mu7" = 0, "g7" = 1,
                 "mu8" = 0, "g8" = 1, 
                 "mu9" = 0, "g9" = 1,
                 "mu10" = 0, "g10" = 1, 
                 "mu11" = 0, "g11" = 1,
                 "mu12" = 0, "g12" = 1,
                 "mu13" = 0, "g13" = 1, 
                 "mu14" = 0, "g14" = 1,
                 "mu15" = 0, "g15" = 1,
                 "mu16" = 0, "g16" = 1,
                 "a" = 1, "b" = 1)

initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}

#Run the JAGS code:
posterior_MLR <- run.jags(modelString,
                          n.chains = 2,
                          data = the_data,
                          monitor = c("Intercept", 
                                      "Female", "Male",
                                      "American.Indian", "Asian.Pacific.Islander", "African.American", "White",
                                      "Age.20.24", "Age.25.29", "Age.30.34", "Age.35.39",
                                      "Age.40.44", "Age.45.49", "Age.50.54", "Age.55.59",
                                      "Age.60.64","Time", "sigma"),
                          adapt = 1000,
                          burnin = 5000,
                          sample = 5000,
                          thin = 5,
                          inits = initsfunction)

#summary statistics for the MLR
summary(posterior_MLR)
plot(posterior_MLR, vars = "Intercept")
plot(posterior_MLR, vars = "Female")
plot(posterior_MLR, vars = "Male")
plot(posterior_MLR, vars = "American.Indian")
plot(posterior_MLR, vars = "Asian.Pacific.Islander")
plot(posterior_MLR, vars = "African.American")
plot(posterior_MLR, vars = "White")
plot(posterior_MLR, vars = "Age.20.24")
plot(posterior_MLR, vars = "Age.25.29")
plot(posterior_MLR, vars = "Age.30.34")
plot(posterior_MLR, vars = "Age.35.39")
plot(posterior_MLR, vars = "Age.40.44")
plot(posterior_MLR, vars = "Age.45.49")
plot(posterior_MLR, vars = "Age.50.54")
plot(posterior_MLR, vars = "Age.55.59")
plot(posterior_MLR, vars = "Age.60.64")
plot(posterior_MLR, vars = "Time")
plot(posterior_MLR, vars = "sigma")


post <- as.mcmc(posterior_MLR)
post %>% as.data.frame %>%
  gather(parameter, value) -> post2

ggplot(transform(post2,
                 parameter=factor(parameter,levels=c("Intercept", 
                                                     "Female", "Male",
                                                     "American.Indian", "Asian.Pacific.Islander", "African.American", "White",
                                                     "Age.20.24", "Age.25.29", "Age.30.34", "Age.35.39",
                                                     "Age.40.44", "Age.45.49", "Age.50.54", "Age.55.59",
                                                     "Age.60.64", "Time", "sigma"))), aes(value)) +
  geom_density() + facet_wrap(~ parameter, ncol = 3) +
  theme(strip.text.x = element_text(size=10))

#facet, free scale

library(BAS)
library(dplyr)
library(data.table)
library(ggplot2)
library(Rmisc)

#df <- read.csv()
df <- dat2
#df <- df[,c(1,2,3,4,8)]
test_year <- 18

dat_train <- filter(df, Year %in% c(test_year-3,test_year-2,test_year-1))
dat_train <- filter(df, Year %in% c(test_year-5,test_year-4,test_year-3,test_year-2,test_year-1))
dat_train <- filter(df, Year %in% c(test_year-10,test_year-9,test_year-8,test_year-7,test_year-6,
                                    test_year-5,test_year-4,test_year-3,test_year-2,test_year-1))

dat_train <- filter(df, Year %in% c(test_year-8,test_year-7,test_year-6,test_year-5,test_year-3))

training <- subset(dat2, Year > 13)
dat_test <- filter(df, Year %in% c(test_year))

#CR_scale <- scale(dat_train$CR.100)
model1 <- bas.lm(CR.100 ~ .,
                 data = dat2,
                 #dat_train
                 prior = "BIC", n.models=1,
                 modelprior = uniform(), initprobs = "eplogp",
                 force.heredity = FALSE, pivot = TRUE
)

plot(model1, which = 3, ask = FALSE, caption = "", sub.caption = "") #cycle 1-4 for which to see different plots

model1 <- bas.lm(CR.100 ~ . -Race1 -Age1 -Deaths -Population -Gender -Race -CR -Sex -Age,
                 data = training,
                 #dat_train
                 prior = "JZS",
                 modelprior = uniform())

model1
summary(model1)
cog.coef <- coef(model1)
cog.coef
par(mfrow = c(1,1))
#plot(cog.coef)

image(model1, rotate = F)
coef.model1 <- coef(model1)
#plot(coef.model1, subset =c(1:14), ask = F)
confint(coef.model1, level = 0.90)
plot(confint(coef.model1, parm = 1:8, level=0.95))
plot(confint(coef.model1, parm = 9:17, level=0.95))
plot(confint(coef(model1, level=0.95)))

#nr <- nrow(df)
#cog.step <- step(model1, k = log(nr))

# Highest probability model
HPM <- predict(model1, estimator = "HPM")
variable.names(HPM)

# Median probability model
MPM <- predict(model1, estimator = "MPM")
variable.names(MPM)

# Best predictive model
BPM <- predict(model1, estimator = "BPM")
variable.names(BPM)

#dat_test <- filter(dat2, Year %in% c(test_year))
new.pred <- predict(model1, newdata = dat_test, estimator = "BMA")

hist(new.pred[["fit"]])
hist(dat_test$CR.100, breaks = 15)

time2 <- dat2$Year^2
time3 <- dat2$Year^3
CYear <- dat2$Year
SexFI <- CYear*dat2$Sex_F; SexMI <- CYear*dat2$Sex_M
RaceAII <- CYear*dat2$Race_AI; RaceAPII <- CYear*dat2$Race_API
RaceBlackI <- CYear*dat2$Race_Black; RaceWhiteI <- CYear*dat2$Race_White

Age20I <- CYear*dat2$Age1_20; Age25I <- CYear*dat2$Age1_25
Age30I <- CYear*dat2$Age1_30; Age35I <- CYear*dat2$Age1_35
Age40I <- CYear*dat2$Age1_40; Age45I <- CYear*dat2$Age1_45
Age50I <- CYear*dat2$Age1_50; Age55I <- CYear*dat2$Age1_55
Age60I <- CYear*dat2$Age1_60; 

linear_reg <- lm(log(dat2$CR.100) ~ CYear + time2 + time3 
                 #+ dat2$Sex_F + SexFI
                  + dat2$Sex_M + SexMI
                 #+ dat2$Race_AI + RaceAII
                 + dat2$Race_API + RaceAPII
                 + dat2$Race_Black + RaceBlackI
                 + dat2$Race_White + RaceWhiteI
                 #+ dat2$Age1_20 
                 #+ Age20I 
                 + dat2$Age1_25 + Age25I
                 + dat2$Age1_30 + Age30I + dat2$Age1_35 + Age35I
                 + dat2$Age1_40 + Age40I + dat2$Age1_45 + Age45I
                + dat2$Age1_50 + Age50I + dat2$Age1_55 + Age55I
                  + dat2$Age1_60 + Age60I
                   , data = training)
#Interpretation: compare coefficient to baseline factor, eg. black to white, not black directly to crude rate
#Remove NA in model
summary(linear_reg)
#stepwiseM <- stepwise()
hist(resid(linear_reg))
qqnorm(resid(linear_reg))
abline()
hist(studres(linear_reg))

outlierlist <- studres(linear_reg) 
outliers <- subset(outlierlist, outlierlist > 4)
plot(fitted(linear_reg), studres(linear_reg))
library(glmnet)
dataMatrix <- cbind(CYear, time2, time3, SexFI, SexMI, RaceAII, RaceAPII,
                    RaceBlackI, RaceWhiteI, Age20I, Age25I, Age30I, Age35I,
                   Age40I, Age45I, Age50I, Age55I, Age60I)

LSex_F <- dat2$Sex_F; LSex_M <- dat2$Sex_M
LRace_AI <- dat2$Race_AI; LRace_API <- dat2$Race_API
LRace_Black <- dat2$Race_Black; LRace_White <- dat2$Race_White
LAge20 <-  dat2$Age1_20; LAge25 <- dat2$Age1_25
LAge30 <-  dat2$Age1_30; LAge35 <- dat2$Age1_35
LAge40 <-  dat2$Age1_40; LAge45 <- dat2$Age1_45
LAge50 <-  dat2$Age1_50; LAge55 <- dat2$Age1_55
LAge60 <-  dat2$Age1_60; 

dataMatrix <- cbind(CYear, time2, time3,
                    LSex_F, SexFI,
                    LSex_M, SexMI,
                    LRace_AI, RaceAII,
                    LRace_API, RaceAPII,
                    LRace_Black, RaceBlackI,
                    LRace_White, RaceWhiteI,
                    LAge20, Age20I, LAge25, Age25I,
                    dat2$Age1_30, Age30I, dat2$Age1_35, Age35I,
                    dat2$Age1_40, Age40I, dat2$Age1_45, Age45I,
                    dat2$Age1_50, Age50I, dat2$Age1_55, Age55I,
                    dat2$Age1_60, Age60I)

dataMatrix <- as.matrix(dataMatrix)
fit.lasso <- glmnet(dataMatrix, log(dat2$CR.100), family='gaussian')
fit_lasso_cv <- cv.glmnet(dataMatrix, dat2$CR.100, alpha = 1, nfolds = 100)

sum(coef(fit_lasso_cv, s = "lambda.1se")[-1] ^ 2)

log(fit_lasso_cv$lambda.min)
log(fit_lasso_cv$lambda.1se)

coefs_lasso <- as.matrix(coef(fit_lasso_cv, s = "lambda.1se"))
ix = which(abs(coefs_lasso[,1]) > 0)
### Number of model selected variables
length(ix)
coefs_lasso[ix,1,drop=FALSE]

par(mfrow=c(1,2))
###### Lasso plot
plot(fit.lasso, xvar="lambda")
###### Lasso CV plot
plot(fit_lasso_cv, main = 'CV Lasso')

###### Tidyverse's handy package that
###### let's you see lambda.min and lambda.1se
library(broom)
tidy(fit_lasso_cv)
glance(fit_lasso_cv)
#fit_lasso_cv$nzero
(fit_lasso_cv$lambda.min)
(fit_lasso_cv$lambda.1se)

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

lambda_best = fit_lasso_cv$lambda.min
predictions_train <- predict(fit_lasso_cv, s = lambda_best, newx = test_x)
eval_results(test_y, predictions_train, test)


time2 <- dat_train$Year^2
time3 <- dat_train$Year^3
linear_reg <- lm(dat_train$CR.100 ~ dat_train$Year + time2 + time3 
                 + dat_train$Sex_F + dat_train$Sex_M 
                 + dat_train$Race_AI + dat_train$Race_API 
                 + dat_train$Race_Black + dat_train$Race_White 
                 +dat_train$Age1_20 
                 +dat_train$Age1_25 
                 +dat_train$Age1_30 
                 +dat_train$Age1_35 
                 +dat_train$Age1_40 
                 +dat_train$Age1_45 
                 +dat_train$Age1_50 
                 +dat_train$Age1_55 
                 +dat_train$Age1_60 
                 , data = dat_train)
summary(linear_reg)
lm_pred <- predict(linear_reg, newdata = dat_test)

#nr <- nrow(dat2)
#cog.step <- step(linear_reg, k = log(nr))
#cog.step
library(bayestestR)
CI_real <- CI(dat_test$CR.100, ci = 0.95)
CI_real
CI_pred <- CI(new.pred[["fit"]], ci = 0.89)
CI_pred
CI_pred_reg <- CI(lm_pred, ci = 0.95)
CI_pred_reg

library(glmnet)
dataMatrix <- cbind()




