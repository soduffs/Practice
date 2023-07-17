# ECN 477 SPRING 2023
# 1/17/23

# coding Tips:
# comment!!!!!!
# run all lines: command+shift+enter
# don't forget to save often (esp at end of class)
# don't use spaces in file names (use _ instead)

# load packages
library(rio)
library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)

setwd("/Users/sineadoduffy/Library/CloudStorage/OneDrive-UNC-Wilmington/class master/ecn477")

#### 1.0 Summary Stats review ####

# check working directory
getwd()

# change working directory, put in one drive
setwd("/Users/sineadoduffy/Library/CloudStorage/OneDrive-UNC-Wilmington/class master/ecn477")

# import data
# option 1: use wizard, saves automatically

# option 2: use rio package
# download: tool, install packages, rio
# call package using library()
library(rio)
# import data using import("datafolder/filename.filetype")
import("data/cps.xlsx")
# save/name data
cps <- import("data/cps.xlsx")

## lecture day 1/19/23 ####
# get average of wage in cps data
mean(cps$wage) # average of nominal wages
# unique() tells ends of data?
unique(cps$year)

## filter to only 2015 cps data
library(tidyverse) #useful for data manipulation and plotting
# get cps from 2015 using a PIPE (function of tidyverse)
cps15 <- cps %>% filter(year==2015) # keep only 2015 data from cps

# unique() to see possible values that year can take in cps
unique(cps15$year)
# save average wage for workers is 2015 as wbar15
wbar15 <- mean(cps15$wage)
# use export() to save data frame to wd
export(cps15, "data/cps15.xlsx")

# histogram time of US wages in 2015
hist(cps15$wage, 
     main="Wage Distribution in 2015", 
     xlab="Average Hourly Earnings") # x axis label)
# add vertical line in wbar15 (average wage)
abline(v=wbar15, col="darkgreen", lwd=4)

# Histogram using ggplot
ggplot(data=cps15) +                                                             # sets up "base" plot
  geom_histogram(aes(x=wage), color="black", fill="white") +                     # create histogram for wages
  geom_vline(aes(xintercept=wbar15), color="red", size=2) +                      # adds a vertical line at the average
  geom_text(aes(x=wbar15+3), label="Mean AHE", y=1000, angle=90, color="red") +  # add label to vertical line
  ggtitle("US Wage Distribution in 2015") +                                      # adds title to plot
  xlab("Average Hourly Earnings") +                                              # add x axis label
  ylab("Count")                                                                  # y axis label
  theme_classic() +                                                              # non-crappy theme
  theme(plot.title=element_text(hjust=0.5))                                      # center title
ggsave("ecn477/wage_hisogram")

# Manually calculate 95% CI
sd15 <- sd(cps15$wage) # standard deviation of the wage
n <- length(cps15$wage) # sample size
se <- sd15/sqrt(n) # standard error of the mean, uncertainty of the average
lb <- wbar15-1.96*se # lower bound of CI
ub <- wbar15+1.96*se # upper bound of CI

# Simple computation of 95%
t.test(cps15$wage)

# Conditional averages by sex
cps15$sex <- ifelse(cps15$female==1, "F", "M")
cps15_stats <- cps15 %>% group_by(sex) %>% summarize(wbar=mean(wage))
View(cps15_stats)

# Overlapping histogram by sex
ggplot(cps15) + geom_histogram(aes(x=wage, color=sex, fill=sex), alpha=0.2) +
  geom_vline(data=cps15_stats, aes(xintercept=wbar, color=sex), size=2) +
  ggtitle("US Wage Distribution in 2015 By Sex") +                                      # adds title to plot
  xlab("Average Hourly Earnings") +                                              # add x axis label
  ylab("Count") +
  theme_classic()


# Difference of Means test
t.test(wage~sex, data=cps15)


## 1/31/23 ####
# conduct a difference of means test for the average wage across education
library(rio)
View(cps15)
t.test(wage~bachelor, data=cps15) # point estimate=mean group 1-mean group 0

# save conditional averages by education
cps15_stats_educ <- cps15 %>% group_by(bachelor) %>% summarize(wbar=mean(wage))
reg_same_as_t <- lm(wage~bachelor, cps15)
summary(reg_same_as_t) # the intercept 

# just bachelors (another way, can't do t-test)
cps15_bachelor <- cps15 %>% filter(bachelor==1)
mean(cps15_bachelor$wage)


# manual summary statistics
mean(cps15$wage)
median(cps15$wage)
var(cps15$wage)
sd(cps15$wage)
min(cps15$wage) # not a moment, tells extreme
# higher order summary statistics
install.packages(moments)
library(moments)
skewness(cps15$wage)
kurtosis(cps15$wage)
# summary statistics table & export
install.packages(stargazer)
library(stargazer)
stargazer(cps15,type="text")
stargazer(cps15[c("wage","bachelor","female","age")],
          title="CPS Descriptive Statistics",
          type="html",
          digits = 2,
          covariate.labels=c("Average Hourly Earnings", "Bachelor's Degree", "Female", "Age"),
          out="tables/cps_stats.doc"
          )
system("open tables/cps_stats.doc") # opens word doc from R

## Merging two data sets
# import data
state_pop <- import("data/statepop.xlsx")
state_bach <- import("data/statebach.xlsx")
# rename variables (columns) to match
state_bach <- state_bach %>% rename(state="Region Name", bach="2019")
# inner_join to merge into one data frame
state_all <- inner_join(state_pop, state_bach) %>% 
  select(state,pop,bach) # keep only variables of interest


#### 2.1 linear regression review ####
#### trick of the day
# preliminaries

# load packages
library(rio) #imports/exports
library(tidyverse) # cleans data

#import data #
class_data <- import("data/caschool.xlsx")
View(class_data)

# scatter plot
plot1 <- ggplot(class_data) +
  geom_point(aes(x=str, y=testscr), color='pink') +
  ggtitle("Scatterplot of Test Scores & Student-Teacher Ratio") +
  xlab("Student-Teacher Ratio") +
  ylab("Average Test Score") +
  theme_classic()
plot(plot1) # plot() to view

# correlation coefficient
cor(class_data$str, class_data$testscr)
cor.test(class_data$str,class_data$testscr) # baby version of regression


#### HW1 2/3/23 ####
# 1 a standard normal distribution has a mean equal to 0 and a variance equal 1

# 2 Type 1 Error occurs when: rejects the null hypothesis when the null is true in reality

# 3 An investor most likely to prefer a portfolio with positive skewness and low kurtosis

# 4 Frequency of Type I errors will rise while the frequency of Type II errors fall

# 5 The Law of Large #s states that:
  # The sample average of some variable Y will get close to population mean as sample size N becomes large
# DATA
install.packages(moments)
library(rio) #imports/exports
library(tidyverse)
library(moments)
unrate <- import("data/ECN477_UR_Parties.xlsx") # load data
View(unrate) # view unemployment data
# HISTOGRAM
library(tidyverse)
urhisto <- ggplot(unrate) + geom_histogram(aes(x=UR), color="navy", fill="#4c00ff") + # fix `binwidth`?
  geom_vline(data=unrate, aes(xintercept=ur_bar), size=1) +
  ggtitle("Unemployment Rates Since 1953") +
  xlab("Unemployment Rate") +
  ylab("Frequency") +
  theme_classic()
plot(urhisto)

# normal distribution?
shapiro.test(unrate$UR) # ain't

# f-test to determine homogeneity of variance
var.test(UR~Dem, data=unrate) # FTR, no sig dif btwn variances?

# 6 1st-4th moments
ur_bar <- mean(unrate$UR) # 5.87
sd(unrate$UR) # 1.67
var(unrate$UR) # 2.78

library(moments)
skewness(unrate$UR) # 0.89
kurtosis(unrate$UR) # 4.19

# 7 
# Average UR under R President
ur_r <- unrate %>% filter(unrate$Dem==0)
mean(ur_r$UR) # 5.87

# Average UR under D President
ur_d <- unrate %>% filter(unrate$Dem==1)
mean(ur_d$UR) # 5.88

# 9 Sample t-test
t.test(UR~Dem, data=unrate)



#### 2.1 ####
library(rio)
class_data <- import("data/caschool.xlsx")
View(class_data)
reg1 <- lm(testscr~str, data=class_data)
summary(reg1)

# 2/9/23

# confidence intercals for coefficients
confint(reg1)
confint(reg1, level=0.99) # confint at 99% sig level

# prediction
class_fake <- data.frame(str=c(15:30)) # crate "fake" observations
class_fake$testscr <- predict(reg1, class_fake) # predicted value: predict(model, dataframe)
class_fake$predint <- predict(reg1,class_fake, interval="prediction") # 
# add regression line + confidence intervals (in GOLD!)
plot2 <- plot1+
  geom_smooth(aes(x=str,y=testscr), method=lm, color="gold")
plot(plot2)
# add prediction interval to original dataset
class_data <- cbind(class_data, predict(reg1,interval="prediction")) #?
# add prediction interval to plot
plot3 <- plot2+
  geom_line(data=class_data, aes(x=str,y=lwr), color="red", linetype="dashed")+
  geom_line(data=class_data, aes(x=str, y=upr),color="red", linetype="dashed")
plot(plot3)

# 2/14/23
#### 2.2 -- Multiple Linear Regression ####

# check hist
hist(class_data$el_pct)
mean(class_data$el_pct)
# test to see if STR and EL are correlated
cor.test(class_data$str, class_data$el_pct)
# see that variables are stat. sig. correlated
reg2 <- lm(testscr~str + el_pct, data=class_data)
summary(reg2)

# Interactions
# Define a dummy variables for EL
# Use 20 for threshold for el_pct (arbitrary)
class_data$high_el <- ifelse(class_data$el_pct >= 20, 1, 0)

reg_interact <- lm(testscr ~ str + high_el + (str*high_el), class_data)
summary(reg_interact)

#### 2.3 Model Comparison ####
summary(reg1)
summary(reg2)
reg3 <- lm(testscr ~ str + el_pct + meal_pct, class_data)
summary(reg3)

reg4 <- lm(testscr ~ str + el_pct + calw_pct , class_data)
summary(reg4)

reg5 <- lm(testscr ~ str + el_pct + meal_pct + calw_pct, class_data)
summary(reg5)

# 2/21/23
library(stargazer) # makes pretty tables
stargazer(reg1,reg2,reg3,reg4,reg5,
          title="Regressions Using CA School Data",
          type="html",
          digits= 2,
          out="tables/caschool_regs.doc")
system("open tables/caschool_regs.doc") # opens word doc from R


#### 2.4 Issues with OLS ####
class_data$low_el <- ifelse(class_data$el_pct < 20,1,0)
# or <- 1-class_data$high_el

# regression dummies
reg_dums <- lm(testscr ~ high_el + low_el, data=class_data)
summary(reg_dums) # note: R drops low_el due to perfect multicolinearity

# alternative: drop the intercept
reg_dums_no_int <- lm(testscr~high_el+low_el - 1, data=class_data) # -1 takes out the intercept (common 1 term)
summary(reg_dums_no_int) # this gives conditional average, but you have to look at the confint?

# Heteroskedasticity
library(sandwich) # provides robust standard errors (vcovHC)=heteroskedastic consistency
library(lmtest) # allows us to use coeftest
coeftest(reg1, vcov=vcovHC(reg1, type="HC1")) # SE increases
# reject the null less often when you use robust SE






#### HW2 ####

## section 1: education and wages

# wd & data
setwd("/Users/sineadoduffy/Library/CloudStorage/OneDrive-UNC-Wilmington/class master/ecn477")
library(rio)
cps_data <- import("ECN477_cps.xlsx")
View(cps_data)

# 1 estimate lm
reg_1 <- lm(wage~educ, data=cps_data)
# get estimates
summary(reg_1)
confint(reg_1)

# 2 city of heteroskedasti
library(sandwich)
library(lmtest)
coeftest(reg_1, vcov=vcovHC(reg_1,type="HC1")) # controlling for heterosked
# heterosked = underestimate sample uncertainty because SE(beta) is going to be too small if hetero
# reject null more often with sample uncertainty?

# 3 multilinear regression
reg_2 <- lm(wage ~ educ + black + female, data=cps_data)
summary(reg_2) # substantial change?

# 4 wage diff btwn black and nonblack
# yes?

# 5 MLR2
reg_3 <- lm(wage~educ+black+female+northeast+midwest+south+west,data=cps_data)
summary(reg_3) # west not defined?
reg_3$coefficients # get coefficients to predict hourly wage for specified

b0 <- reg_3$coefficients[1] # use only one dimension
b1 <- reg_3$coefficients[2]
b2 <- reg_3$coefficients[3]
b6 <- reg_3$coefficients[7]

# predict hourly wage for a black male worker with educ = 12
wage_predict <- b0+(b1*12)+(b2*1)+(b6*1) # include b0?
wage_predict # 16.21449

# soques notes
cps_q5_reg <- lm(wage~educ+black+female+northeast+midwest+south+west,data=cps_data)
cps_fake <- data.frame(educ=12, black=1, female=0, northeast=0, south=1, west=0, midwest=0)
cps_predict <- predict(cps_q5_reg, cps_fake)
cps_predict # 16.21, same as manual calc w betas

## section 2: capital asset pricing model + risk premium
capm_data <- import("ECN477_capm.xlsx")
View(capm_data) # variations in security return as function of portfolio return

# 6 
## create new variables for each stock and market risk premium
rf <- capm_data$riskfree # risk free return vector
rp_dis <- capm_data$dis - rf
rp_ge <- capm_data$ge - rf
rp_gm <- capm_data$gm - rf
rp_ibm <- capm_data$ibm - rf
rp_msft <- capm_data$msft - rf
rp_xom <- capm_data$xom - rf
rp_mkt <- capm_data$mkt - rf
## a) find highest average risk premium
# avg_rp vector
avg_rp <- c(
  mean(rp_dis), # -0.031
  mean(rp_ge), # -0.031
  mean(rp_gm), # -0.041
  mean(rp_ibm), # -0.023
  mean(rp_msft), # -0.024
  mean(rp_xom), # -0.022
  mean(rp_mkt)) # -0.03
# max of avg vector
max(avg_rp) # -0.022, xom

## b) find largest volatility (variance)
var_rp <- c(
  var(rp_dis), # 0.007
  var(rp_ge), # 0.005
  var(rp_gm), # 0.016
  var(rp_ibm), # 0.008
  var(rp_msft), # 0.012
  var(rp_xom), # 0.003
  var(rp_mkt)) # 0.002
# max of avg vector
max(var_rp) # 0.016, gm

# 7 & 8
## estimate CAPM model for each firm (6 regs)
# CAPM model: risk premium (return for stock j
# - return on the risk free rate) =
# alpha (aka incpt) + betaj (return on mkt - risk free rate) + error term
# how can I make this a loop?
# capm_regs <- c(
capm_dis <- lm(rp_dis~rp_mkt, data=capm_data)
capm_ge <- lm(rp_ge~rp_mkt, data=capm_data)
capm_gm <- lm(rp_gm~rp_mkt, data=capm_data)
capm_ibm <- lm(rp_ibm~rp_mkt, data=capm_data)
capm_msft <- lm(rp_msft~rp_mkt, data=capm_data)
capm_xom <- lm(rp_xom~rp_mkt, data=capm_data)

# loop
dependent_vars <- c("rp_dis", "rp_ge", "rp_gm", "rp_ibm", "rp_msft", "rp_xom")
capm_regs <- list()

for (i in dependent_vars) {
  capm_regs[[i]] <- lm(paste(i, "~ rp_mkt"), data = capm_data)
}
# summary of reg
summary(capm_regs[["rp_dis"]])


# not for mkt
## estimate beta j for sa
# for (i in capm_regs) {capm_summary <- capm_regs$coefficients[2], print(capm_summary)}
coefficients(capm_dis)[2]
coefficients(capm_ge)[2]
coefficients(capm_gm)[2]
coefficients(capm_ibm)[2]
coefficients(capm_msft)[2]
coefficients(capm_xom)[2]

# chat loop directions yaaaaaas
# create vector of "regressions" with " " !
capm_regs <- c("capm_dis", "capm_ge", "capm_gm", "capm_ibm", "capm_msft", "capm_xom")
# for (i in reg_vector) { new object <- function(get(i))[subset if necessary] print(object)

for (i in capm_regs) {
  capm_summary <- coefficients(get(i))[2]
  print(capm_summary)
}

# 9 & 10
confint(capm_dis)
confint(capm_ge)
confint(capm_gm)
confint(capm_ibm)
confint(capm_msft)
confint(capm_xom)

# same but loop using reg vector and new function
for(i in capm_regs) {
  capm_confint_summary <- confint(get(i))
  print(capm_confint_summary)
}



#### 3 ####
# 3/16/23
library(tidyverse)
library(rio)
library(lmtest)
library(sandwich)
library(stargazer)

# import traffic data
beer_data <- import("ECN477_fatality.xlsx")
view(beer_data)

# allnite = number of 

# create new variable fatal = traffic deaths per 10,000 people
beer_data$fatal <- beer_data$mrall * 10000
hist(beer_data$fatal)
# get only 1982
beer_data_1982 <- beer_data %>% filter(year==1982)
reg1982 <- lm(fatal ~ beertax,  data = beer_data_1982)
summary(reg1982)

# get only 1988
beer_data_1988 <- beer_data %>% filter(year==1988)
reg1988 <- lm(fatal ~ beertax,  data = beer_data_1988)
summary(reg1988)

#### 3.2 Panel Reg w/ Two Time Periods ####
library(plm)
# filter data to 82 and 88 using or | condition
beer_data_8288 <- beer_data %>% filter(year==1982 | year == 1988)
reg8288 <- lm(fatal ~ beertax,  data = beer_data_8288)
summary(reg8288)
view(beer_data_8288)
# dif of dif model using plm()
reg_did <- plm(fatal ~ beertax, data = beer_data_8288, model = 'fd')
summary(reg_did) # Oneway (individual) effect means 
# what does oneway mean? and individual refers to the single z variable?

#### 3.3 Fixed Effects Regression ####
# Entity-level Fixed Effects Only
reg_fe1 <- plm(fatal ~ beertax, data=beer_data, effect="individual")
summary(reg_fe1)

# Alternative way w/ lm() Add state fixed effects
# factor() tells R to treat it as an identifier, not a numeric value
# could also do this on continent : factor(continent)
summary(lm(fatal ~ beertax + factor(state), data=beer_data))

# Time Fixed Effects Only
# change last arguement to effect = "time"
reg_fe2 <- plm(fatal ~ beertax, data=beer_data, effect = "time")
summary(reg_fe2)

# Alternative way: Add time (?) fixed effects
summary(lm(fatal ~ beertax + factor(year), data=beer_data))

# Estimate TWFE: Two-Way Fixed Effects Model
reg_twfe <- plm(fatal ~ beertax, data=beer_data, effect = "twoways")
summary(reg_twfe)

#### 3.4 Clustered Standard Errors ####
library(lmtest)
library(sandwich)
# optimal weighting matrix with vcov() ; HC1 is bickered about to get the best?
coeftest(reg_twfe, vcovHC(reg_twfe, type="HC1"))
# SE went up as did P

#### Summarizing FE Models ###
# Pooled Pannel Regression (pooling all years together, not controlling for anything)
reg_pool <- lm(fatal ~ beertax, data=beer_data)
# Kitchen-Sink Regression (overfitted)
reg_twfe_all <- plm(fatal ~ beertax + mlda + jaild + comserd + vmiles + unrate, data=beer_data, 
                    effect = "twoways")
# Get Clustered SE for each model to put in table
# diag makes sure it takes out the right ones, not the covariances
cluster_se <- list(
  sqrt(diag(vcovHC(reg_pool, type="HC1"))),
  sqrt(diag(vcovHC(reg_fe1, type="HC1"))),
  sqrt(diag(vcovHC(reg_fe2, type="HC1"))),
  sqrt(diag(vcovHC(reg_twfe, type="HC1"))),
  sqrt(diag(vcovHC(reg_twfe_all, type="HC1")))
)


# Compile models into single table
# All have clustered SE
stargazer(reg_pool, reg_fe1, reg_fe2, reg_twfe, reg_twfe_all,
          se=cluster_se,
          type="html",
          title="Effect of Beer Tax on Traffic Fatality Rate",
          add.lines = list(
            c("State Fixed Effects", "No", "Yes", "No", "Yes", "Yes")),
          out= "tables/beer_regs.doc" )


#### HW3 ####
# 3/27/23
library(tidyverse)
library(rio)
library(sandwich)
library(stargazer)
library(moments)
library(lmtest)
library(plm)

# import
data_gun <- import("ECN477_guns.xlsx")
# 1: why use log instead of raw
# standardization basically?
# see moments
hist(data_gun$vio)
skewness(data_gun$vio)
kurtosis(data_gun$vio)

# 2 Estimate Regression using only '77 data
data_gun77 <- data_gun %>% filter(year==77)
reg77 <- lm(log(vio) ~ shall + incarc_rate + density + avginc + pop + 
              pm1029 + pb1064 + pw1064, data = data_gun77)
# summary of effect of shall-issue law on violet crime 
summary(reg77)

# 3 Estimate Regression using only '99 data
data_gun99 <- data_gun %>% filter(year==99)
reg99 <- lm(log(vio) ~ shall + incarc_rate + density + avginc + pop +
              pm1029 + pb1064 + pw1064, data = data_gun99)
# summary of effect of shall-issue law on violet crime
summary(reg99)

# 4 Estimate Regression using all time data
reg_pool <- lm(log(vio) ~ shall + incarc_rate + density + avginc + pop +
                   pm1029 + pb1064 + pw1064, data = data_gun)

summary(reg_pool)

# 5 Chad's pool ?

# 6 Fixed Effects Regression
data_gun$stateid <- factor(data_gun$stateid)
data_gun$year <- factor(data_gun$year)
reg_fe <- plm(log(vio) ~ shall + incarc_rate + density + avginc + pop +
                pm1029 + pb1064 + pw1064 + stateid + year , 
              data=data_gun, index=c("stateid","year"), effect="twoways")
summary(reg_fe)

# 9
coeftest(reg_fe, vcov = vcovHC(reg_fe, type="HC1"))

# clustered SE for all models
cse <- list(sqrt(diag(vcovHC(reg77, type = "HC1"))),
                sqrt(diag(vcovHC(reg99, type = "HC1"))),
                sqrt(diag(vcovHC(reg_pool, type = "HC1"))),
                sqrt(diag(vcovHC(reg_fe, type = "HC1"))))

# stargazer to get table of all models
stargazer(reg77,reg99,reg_pool,reg_fe,
          se = cse,
          type = "html",
          omit = c("stateid","year","Constant"),
          out = "/Users/sineadoduffy/Library/CloudStorage/OneDrive-UNC-Wilmington/class master/ecn477/guns.doc"
)



#### 4.0 ####
# import data
hmda_data <- import("ECN477_hmda.xlsx")
# Transform variables from 
hmda_data$deny <- ifelse(hmda_data$s7==3,1,0) # create deny variable
hmda_data$debt <- hmda_data$s46
hmda_data$black <- ifelse(hmda_data$s13==3,1,0)
# summary statistics
stargazer(hmda_data[c("deny", "debt", "black")],
          type="text")
# quick t-test
t.test(deny ~ black, hmda_data)

#### 4.1 Linear Probability Model ####
# Simple LPM w/ only debt
lpm1 <- lm(deny ~ debt, hmda_data)
summary(lpm1)

# adding black variable
lpm2 <- lm(deny ~ debt + black, hmda_data)
summary(lpm2)

# Prediction for debt/income = 30 and black = 0
predict(lpm2, data.frame(debt=30, black=0))
# Prediction for debt/income = 30 and black =1
predict(lpm2, data.frame(debt=30, black=1))

##### 4.2 Probit Model ####
# 4/11/23
# glm = "generalized linear model"

# Probit model w/ only debt
# binomial refers to distribution of SE, or x's relationship with y more generally?
probit1 <- glm(deny ~ debt, data=hmda_data,
               family = binomial(link="probit"))
summary(probit1)

# Interpreting the intercept
pnorm(-2.19)
# same
pnorm(probit1$coefficients[1])
# type = "response" gives probability from z score that is beta0?
predict(probit1, data.frame(debt=0), type="response")

pr_deny_30 <- predict(probit1, data.frame(debt=30), type="response")
pr_deny_40 <- predict(probit1, data.frame(debt=40), type="response")
# marginal difference
pr_deny_40 - pr_deny_30

pr_deny_150 <- predict(probit1, data.frame(debt=150), type="response")
pr_deny_160 <- predict(probit1, data.frame(debt=160), type="response")
pr_deny_160 - pr_deny_150
# put non linear modeling on resume

# FIX!!! Plot of all predicted value for probit model
ggplot(data=hmda_data, aes(x=debt, y=deny))+ 
  geom_point()+
  geom_smooth(method="glm", method.args=list(family="bionomial"))

# Probit model w/ debt and black
probit2 <- glm(deny ~ debt + black, data= hmda_data, family=binomial(link="probit"))
summary(probit2)

# Predicted prob for debt=30 for black and non-black individuals
predict(probit2, data.frame(debt=30, black=1), type="response")
predict(probit2, data.frame(debt=30, black=0), type="response")

# Prediction for all possible debt levels and all possible levels for black(0,1)
hmda_fake1 <- data.frame(debt=seq(min(hmda_data$debt), max(hmda_data$debt),1),black=1)
hmda_fake0 <- data.frame(debt=seq(min(hmda_data$debt), max(hmda_data$debt),1),black=0)
hmda_fake <- rbind(hmda_fake1,hmda_fake0) # row bind

hmda_fake$pr_deny_probit2 <- predict(probit2, hmda_fake, type="response")

# Plot predicted probabilities for each race category
ggplot(data=hmda_fake, aes(x=debt, y=pr_deny_probit2, color=factor(black))) +
  geom_point() +
  theme_classic()

#### 4.3 Logit Model ####
logit2 <- glm(deny ~ debt + black, data = hmda_data, 
              family=binomial(link = "logit"))
summary(logit2)

#### 4.4 Model Comparison w/ Binary Dependent Variables ####

library(pROC)

# pROC estimates ROC Curves and AUROC
# adding predicted probability from this model to dataframe
hmda_data$pr_lpm <- predict(lpm2, data=hmda_data)
# prob for probit
hmda_data$pr_probit <- predict(probit2, data=hmda_data, type="response")
# for logit
hmda_data$pr_logit <- predict(logit2, data=hmda_data, type="response")

# compare actual outcomes to model's prediction
roc_lpm <- roc(hmda_data$deny ~ hmda_data$pr_lpm, plot=TRUE)
# get integral for area under roc curve for lpm
auroc_lpm <- roc_lpm$auc
auroc_lpm

# get integral for probit
roc_probit <- roc(hmda_data$deny ~ hmda_data$pr_probit, plot=TRUE)
roc_probit # can also say roc_probit$auc
# for logit
roc_logit <- roc(hmda_data$deny ~ hmda_data$pr_logit, plot=TRUE)
roc_logit

# add more right-side control variables to minimize OVB
hmda_data$hir <- hmda_data$s45 # house payment to income ratio
hmda_data$ltv <- hmda_data$s6/hmda_data$s50 # loan to value
hmda_data$ccs <- hmda_data$s43 # consumer payment score
hmda_data$mcs <- hmda_data$s42 # mortgage payment score
hmda_data$pbr <- hmda_data$s44>0 # previous bankruptcy
hmda_data$dmi <- hmda_data$s53==1 # denied mortgage insurance
hmda_data$se <- hmda_data$s27a==1 # self-employed
hmda_data$hs <- hmda_data$school>=12 # high-school education or more

# lpm
lpm_all <- lm(deny ~ debt + black + hir + ltv + ccs + mcs + pbr + dmi + se + hs+ uria, hmda_data)
summary(lpm_all)

# probit
probit_all <- glm(deny ~ debt + black + hir + ltv + ccs + mcs + pbr + dmi + se + hs+ uria, hmda_data, family=binomial)
summary(probit_all)

# logit
logit_all <- glm(deny ~ debt + black + hir + ltv + ccs + mcs + pbr + dmi + se + hs+ uria, hmda_data,family=binomial(link="logit"))
summary(logit_all)

#### 5

#### HW4 ####
# Packages
library(rio)
library(moments)
# Import and save/name data
nels <- import("data/nels.xlsx")
# see moments
hist(nels$FAMINC)
skewness(nels$FAMINC)
kurtosis(nels$FAMINC)

# 1
# Create COLLEGE variable from two given variables (2 yr or 4 yr college)
# data$newvariable <-  
# ifelse(data$existingVARIABLE==#(value of x) | (or) data$VARIABLE==#, # (value you want the new variable to be if observation meets criteria), # (value if criteria not met))
nels$college <- ifelse(nels$PSECHOICE==2 | nels$PSECHOICE==3,1,0) # create COLLEGE variable
# Percent that attend college
attend_c <- sum(nels$college)
attend_c / nrow(nels) * 100

# 2
# Calculate mean GRADES, FAMINC, FAMSIZE, FEMALE, BLACK
avg_grades <- mean(nels$GRADES) # 6.53
avg_faminc <- mean(nels$FAMINC) # 51.39
avg_famsiz <- mean(nels$FAMSIZ) # 4.21
avg_female <- mean(nels$FEMALE) # 0.496
avg_black <- mean(nels$BLACK) # 0.056
# % of sample that is black
black <- sum(nels$BLACK) # 56
pct_black <- black / nrow(nels) * 100 # 5.6

# 3
# (a) What % of grads who attend college select a four-year college
# create new variable where attend 4 yr = 1, or else = 0
nels$attend_4yr <- ifelse(nels$PSECHOICE==3,1,0)
# sum 4 yr, get % of college attendees that go 4 yr
total_4yr <- sum(nels$attend_4yr) # 527
pct_4yr <- (total_4yr / attend_c) * 100 # 67.74

# (b) % of attend_4yr = black?
# new variable black_4yr to match attend_4yr with black
nels$black_4yr <- ifelse(nels$BLACK==1 & nels$PSECHOICE==3, 1, 0)
total_black4yr <- sum(nels$black_4yr) # 31
# % of 4yr attendees that = black
pct_black4yr <- (total_black4yr / total_4yr) * 100 # 5.88


# 4
# Estimate LPM explaining COLLEGE using GRADES, FAMINC, FAMSIZ, PARCOLL, FEMALE, BLACK
lpm1 <- lm(college ~ GRADES + FAMINC + FAMSIZ + PARCOLL + FEMALE + BLACK, data=nels)
summary(lpm1)
# betas are probabilities

# 5
# Predicted Probability of attending college for a black female with
# GRADES = 5, avg FAMINC, FAMSIZ = 5, PARCOLL = 1
predict(lpm1, 
        data.frame(GRADES=5, FAMINC=avg_faminc, FAMSIZ=5, PARCOLL=1, FEMALE=1, BLACK=1))
# predicted probability = 1.08

# 6
# Repeat caluclation for
# non-black male
predict(lpm1,
        data.frame(GRADES=5, FAMINC=avg_faminc, FAMSIZ=5, PARCOLL=1, FEMALE=0, BLACK=0))
# predicted probability = 0.92

# non-black female
predict(lpm1,
        data.frame(GRADES=5, FAMINC=avg_faminc, FAMSIZ=5, PARCOLL=1, FEMALE=1, BLACK=0))
# predicted probability = 0.94

# black male
predict(lpm1,
        data.frame(GRADES=5, FAMINC=avg_faminc, FAMSIZ=5, PARCOLL=1, FEMALE=0, BLACK=1))
# predicted probability = 1.07

# 7
# Reestiamte model, omit variables PARCOLL, BLACK, FEMALE
lpm2 <- lm(college ~ GRADES + FAMINC + FAMSIZ, data=nels)
summary(lpm2)

# 8
# Marginal effect on the probability of attending college for a student moving from
# GRADES =avg to =2.635 c.p.
# caluclate these probabilities and the difference btwn the two
# create avg_parcoll
avg_parcoll <- mean(nels$PARCOLL)
# GRADES = avg
pp_avg_grades <- predict(lpm1,
                         data.frame(GRADES=avg_grades, FAMINC=avg_faminc, FAMSIZ=avg_famsiz, PARCOLL=avg_parcoll, FEMALE=avg_female, BLACK=avg_black))
# GRADES = 2.635 (5th %)
pp_5_grades <- predict(lpm1,
                       data.frame(GRADES=2.635, FAMINC=avg_faminc, FAMSIZ=avg_famsiz, PARCOLL=avg_parcoll, FEMALE=avg_female, BLACK=avg_black))
# difference btwn the two
pp_5_grades - pp_avg_grades # 0.26

# 9
# call pROC for model comparison; estimates ROC Curves and AUROC
library(pROC)
# estimate logit model
logit1 <- glm(college ~ GRADES + FAMINC + FAMSIZ + PARCOLL + FEMALE + BLACK, data = nels, 
              family=binomial(link = "logit"))
summary(logit1)

# adding predicted probability from this model to dataframe
nels$pr_lpm <- predict(lpm1, data=nels)
# for logit
nels$pr_logit <- predict(logit1, data=nels, type="response")

# compare actual outcomes to model's prediction
roc_lpm <- roc(nels$college ~ nels$pr_lpm, plot=TRUE)

# get integral for area under roc curve for lpm
auroc_lpm <- roc_lpm$auc
auroc_lpm
# get integral for logit
roc_logit <- roc(nels$college ~ nels$pr_logit, plot=TRUE)
roc_logit
# same thing
auroc_logit <- roc_logit$auc
auroc_logit

# 10
# units of logit estimates are logodds = log(odds) = log(p / (1 - p)
# predict college when grades=avg
lgavg <- predict(logit1, 
                 data.frame(GRADES=avg_grades, FAMINC=52, FAMSIZ=4, PARCOLL=1, FEMALE=1, BLACK=0, type="response"))
lgavg # 2.36

# predict college when grades=top5%
lg5 <- predict(logit1,
               data.frame(GRADES=2.635, FAMINC=52, FAMSIZ=4, PARCOLL=1, FEMALE=1, BLACK=0, type="reponse"))
lg5 # 4.37

# marginal effect of avg->top 5%
lg5 - lgavg # 2.02 logodds





#### 5.0 & 5.1 Time Series Analysis ####
# clear consol
cat("\014")
# FRED API key: fac2e3262a353e069c8471d0114ca9d7
library(tidyverse) # organize data
library(fredr) # use the FRED API
library(reshape2) # transform data into long (melt function)

#### 5.1 Data Prep and Cleaning ###
# API = Application Programming Interface

# Pull Data from FRED API
fred_key <- 'fac2e3262a353e069c8471d0114ca9d7' # save as character
# set FRED key to personal FRED API key
fredr_set_key(fred_key)

# Set start date and end date for sample
start_date <- as.Date("1960-01-01")
end_date <- as.Date("2019-10-01")

# Pull nominal GDP from FRED
# see documentation
ngdp_raw <- fredr(
  series_id='GDP',
  observation_start= start_date,
  observation_end=end_date
)

ngdp <- rename(ngdp_raw, ngdp=value) # rename 'value' to 'ngdp'
ngdp <- ngdp %>% select(date, ngdp)

# Plot GDP in levels
ggplot(data=ngdp) +
  geom_line(aes(x=date,y=ngdp)) +
  labs(title="U.S. Nominal Gross Domestic Product",
        x = "Date",
        y = "Billions of Dollars") +
  theme_minimal()

# Pull Real GDP over the same period
rgdp_raw <- fredr(series_id='GDPC1',
              observation_start = start_date,
              observation_end = end_date)
# Does same as above, just different
rgdp <- rgdp_raw %>%
  transmute(date=date, rgdp = value)

# Merge nominal and real GDP into one (wide) data frame
macro_wide <- merge(ngdp, rgdp, by="date")
macro_long <- melt(macro_wide, id = "date") # ggplot likes long

# Plot both series together
ggplot(data=macro_long, aes(x=date,y=value, color=variable)) +
  geom_line() + 
  labs(title="U.S. Gross Domestic Product", x="Date", y="Billions of Dollars",
       color="",
       caption="Source: Bureau of Economic Analysis") +
  scale_color_manual(labels=c("Nominal", "Real"), values=c("darkgreen", "deeppink2")) +
  theme_minimal()


# Transformations
# he's gonna say that simple growth rate we know is inferior to something else?
# symmetry: symmetric because an equal change in 

# maybe unload package
macro_wide <- macro_wide %>%
  mutate(rgdp_tm1 = lag(rgdp), # First lag of RGDP
         rgdp_diff1 = rgdp - lag(rgdp), # First Difference of RGDP
         rgdp_diff4 = rgdp - lag(rgdp,4), # Fourth Difference of RGDP (Annual Change in BB Dollars)
         rgdp_pgrowth1 = 100 * ((rgdp - lag(rgdp))/lag(rgdp)), # Quarterly % Change
         # rgdp_pgrowth = 100* (rgdp/lag(rgdp) -1) # EXACTLY Same as above
         rgdp_pgrowth1_a = 100 * ((rgdp / lag(rgdp)) ^ 4 - 1), # quarterly % change, annualized
        rgdp_pgrowth4 = 100 * ((rgdp - lag(rgdp,4))/lag(rgdp,4)), # Annual % change (growth)
        rgdp_lgrowth1_a = 4 * 100 * (log(rgdp) - log(lag(rgdp)) ), # Log first difference (Annualized)
          )
# Pull log growth from FRED directly (specify units)
rgdp_lgrowth_a_raw <- fredr(series_id='GDPC1',
                  observation_start = start_date,
                  observation_end = end_date,
                  units = 'pca'
                  )
# Plot to compare % change quarterly growth to log-diff quarterly growth
ggplot(macro_wide) +
  geom_line(aes(x = date, y = rgdp_pgrowth1_a, color = '% Change')) +
  geom_line(aes(x=date, y=rgdp_lgrowth1_a, color = 'Log-Diff')) +
  scale_color_manual(name = "", values = c('% Change'='deeppink2', 'Log-Diff' = 'lightgreen')) + # overlays second line over first
  labs(title = "U.S. GDP Quarerly Annualized Growth",
       x= "Date",
       y = "Percent") +
  theme_classic()

#### 5.2 Autocorrelation Function (ACF) #### 
acf_rgdp <- acf(macro_wide$rgdp) # ACF of Real GDP in levels -- clearly nonstationary
# gdp growth rate
# pre-covid gives same level, but growth rate has two significant lags
act_rgdp_growth <- acf(macro_wide$rgdp_lgrowth1_a, na.action = na.pass) # ACF of real gdp growth (annualized)

#### 5.3 Testing for Stationarity #### 
# Augmented Dicky-Fuller Test
library(tseries)
# adf for rgdp in levels
adf.test(macro_wide$rgdp)
# adf for annualized growth of real gdp
adf.test(na.omit(macro_wide$rgdp_lgrowth1_a) )


#### 5.4 Autoregressions ####
library(rio)
library(tidyverse)
# import and save/name data
# filter out COVID dates
macro_data <- import("data/ECN477_macro.xlsx") %>%
  filter(Date < as.Date('2020-01-01'))

# Convert gdp (growth) to a time series variable
gdp <- ts(macro_data$gdp, start=c(1961,2), freq=4)
# WHAT IS THE 2?

# Estimate AR(1)
library(forecast)
ar1 <- arima(gdp, order=c(1,0,0)) # order=c(lags, 0, 0)
summary(ar1)

# is this thing useful?
library(lmtest)
coeftest(ar1)

ar_best <- auto.arima(gdp,
                      max.p=24, # max number of AR terms
                      max.q=0, # max number of MA terms (force to be 0)
                      stationary=TRUE, # know from adf test
                      seasonal=FALSE, # no seasonal adjustment terms?
                      ic="bic" # use bic for model selection
                      )
summary(ar_best) # see that p=2 is the optimal model (also see # of coefs)
summary(forecast(ar_best, h=4)) # forecast optimal model for the next 4 periods


#### 5.5 Autoregressive Distributed Lag Models ####
# convert ntfs to time series
ntfs <- ts(macro_data$ntfs,
           start=c(1961,2),
           freq=4)
library(dynlm)
# Estimate ARDL (2,1)
# distribute 
ardl_2_1 <- dynlm(gdp ~ L(gdp, 1:2) + L(ntfs, 1))
summary(ardl_2_1)

grangertest(gdp ~ ntfs, data=macro_data, order=4)
grangertest(ntfs ~ gdp, data=macro_data, order=4)



#### HW5 -- TIME SERIES ####
# 1
# Data Prep and Cleaning, plot
# API = Application Programming Interface

# load fred package
library(fredr)

# Pull Data from FRED API
fred_key <- 'fac2e3262a353e069c8471d0114ca9d7' # save as character
# set FRED key to personal FRED API key
fredr_set_key(fred_key)

# Set start date and end date for sample
start_date <- as.Date("1970-01-01")
end_date <- as.Date("2019-03-01")

# Pull CPI from FRED
cpi_raw <- fredr(
  series_id='CPIAUCSL',
  observation_start= start_date,
  observation_end=end_date
)

cpi <- rename(cpi_raw, index=value) # rename 'value' to 'index'
cpi <- cpi %>% select(date, index)

## 1
# Plot CPI in levels
ggplot(data=cpi) +
  geom_line(aes(x=date, y=index)) +
  labs(title="CPI",
       x = "Date",
       y= "Index") +
  theme_minimal() # that hoe LINEAR

# Pull Average Hourly Earnings over the same period
ahe_raw <- fredr(series_id='AHETPI',
                 observation_start = start_date,
                 observation_end = end_date)
# Does same as above, just different
ahe <- ahe_raw %>%
  transmute(date=date, dph = value)

# Merge nominal and real GDP into one (wide) data frame
pi <- merge(cpi, ahe, by="date")
pi_long <- melt(pi, id = "date") # ggplot likes long

# Plot both series together
ggplot(data=pi_long, aes(x=date, y=value, color=variable)) +
  geom_line()+
  labs(title="CPI and AHE", x="Date", y="Unit Value",
       color="")+
  scale_color_manual(labels=c("CPI", "AHE"), values=c("deeppink2", "darkgreen")) +
  theme_minimal()


## 2
# Run an ADF on each time series (CPI and Wage)
library(tseries)
# adf for cpi 
adf.test(pi$index) # ftj null that data is nonstationary
adf.test(pi$dph)

## 3
# Calculate annual CPI inflation from the index values
pi <- pi %>%
  mutate(
    cpi_pi = 100 * ((index - lag(index,12))/lag(index,12))) # Annual % change (growth)

# Calculate annual Wage inflation from AHE
pi <- pi %>%
  mutate(
    wage_pi = 100 * ((dph - lag(dph,12))/lag(dph,12)))

# check it with a plot
ggplot(pi) +
  geom_line(aes(x=date,y=cpi_pi, color="CPI Inflation")) +
  geom_line(aes(x=date,y=wage_pi, color="Wage Inflation")) +
  labs(title = "CPI Price Inflation and Wage Inflation",
       x = "Date",
       y = "% Change From 1 Year Ago")+
  scale_color_manual(name="",values = c("CPI Inflation" = "darkblue", "Wage Inflation" = "red")) +
  theme_minimal()

# See if this variable is now stationary that the data is transformed w/ ADF

adf.test(na.omit(pi$cpi_pi)) # passes test
adf.test(na.omit(pi$wage_pi)) # seems to fail given large p


## 5
# Estimate ARDL(0,1) with dependent variable as CPI inflation and the first lag of WAGE 
# Autoregressive Distributed Lag Models
library(dynlm)

# convert variables to time series
cpi_pi <- ts(pi$cpi_pi,
             start=c(1970,1),
             freq=12)
wage_pi <- ts(pi$wage_pi,
              start=c(1970,1),
              freq=12)


## 6 
# NOW estimate ARDL(0,1)
ardl_01 <- dynlm(cpi_pi ~ 
                   L(wage_pi, 1))

summary(ardl_01)

# estimate ARDL(1,1)
ardl_11 <- dynlm(cpi_pi ~
                   L(cpi_pi,1) + 
                   L(wage_pi, 1))

summary(ardl_11)
library(stargazer)
stargazer(ardl_01, ardl_11, type="text")
# Effect of wage decreased substantially
# The stat sig effect we estimated in the other model can be attributed to OVB!


## 7
# See if single lag of each variable is the optimal number to use in ARDL(p,q)
# Find optimal # of lags to include in an AR(p) for CPI inflation
# first, look at ACF of cpi_pi
acf_cpi_pi <- acf(cpi_pi, na.action = na.pass) # obvs nonstationary?
# partial ACF to assess each period's marginal correlation (when tf did we do that?!)
pacf(cpi_pi, na.action = na.pass) 

# use arima to get optimal lag for cpi_pi (p*)
ar_best <- auto.arima(cpi_pi,
                      max.p=4, # max # of AR terms (any?)
                      max.q=0,
                      stationary=TRUE,
                      seasonal=FALSE,
                      ic="bic")
pstar <- ar_best$arma[1]
print(pstar) # p* = 2

# find optimal lags of wage inflation (r*) using p*
# r* = 1 but don't worry about how to do that
rstar <-  1

## 8
# forecast cpi inflation for april 2019 using optimal ARDL(p,r) model
# create the optimal ARDL
ardl_best <- dynlm(cpi_pi ~ L(cpi_pi, 1:pstar) + L(wage_pi, 1:rstar))

# create forecast using ardl_best
fcast <- coef(ardl_best) %*% 
  c(1,cpi_pi[length(cpi_pi)-1], cpi_pi[length(cpi_pi)-2], wage_pi[length(wage_pi)-1])
print(fcast)



# doesn't work for dynlm or lm!
summary(forecast(ardl_best, h=1)) # forecast optimal model for the next 4 periods


# extra shit 
# Estimate AR(1) for cpi_pi
library(forecast)
ar1_cpi_pi <- arima(cpi_pi, order=c(1,0,0))
summary(ar1_cpi_pi)


# Estimate AR(1)
library(forecast)
ar1 <- arima(cpi, order=c(1,0,0)) # order=c(lags, 0, 0)
summary(ar1)


grangertest(gdp ~ ntfs, data=macro_data, order=4)
grangertest(ntfs ~ gdp, data=macro_data, order=4)



