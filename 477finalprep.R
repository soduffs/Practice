# final practice

#### HW1 PRACTICE ####

# 6
data_ur <- import("data/ECN477_UR_Parties.xlsx")
library(moments)
library(tidyverse)
# histogram of ur variable
hist(data_ur$UR)
# moments
mean(data_ur$UR) #5.87
var(data_ur$UR) # 2.78 > 1 (not SND)
sd(data_ur$UR) # 1.67 > 1 (not SND)
skewness(data_ur$UR) # 0.89 (+ skew)
kurtosis(data_ur$UR) # 4.19 (leptokurtic, heavy tails)

# normal distribution?
shapiro.test(data_ur$UR) # ain't

# f-test to determine homogeneity of variance
var.test(UR~Dem, data=data_ur) # FTR, no sig dif btwn variances?


# 7
# UR under different parties
ur_r <- data_ur %>% filter(data_ur$Dem==0)
mean(ur_r$UR) # 5.87

ur_d <- data_ur %>% filter(data_ur$Dem==1)
mean(ur_d$UR) # 5.88

# 8
# explain why a t test is necessary if we want to make a difinitive statement
# about the difference between these conditional means
t.test(UR~Dem, data=data_ur)




# hw 1 again
library(moments)

# 6
data_unrate <- import("data/ECN477_UR_Parties.xlsx")
hist(data_unrate$UR)
mean(data_unrate$UR)
var(data_unrate$UR)
sd(data_unrate$UR)
skewness(data_unrate$UR)
kurtosis(data_unrate$UR)
shapiro.test(data_unrate$UR)


# 7
unrate_r <- data_unrate %>% filter(data_unrate$Dem==0)
mean(unrate_r$UR)

unrate_d <- data_unrate %>% filter(data_unrate$Dem==1)
mean(unrate_d$UR)

# 9 
t.test(UR ~ Dem, data=data_unrate)



#### HW2 ####

data_cps <- import("ECN477_cps.xlsx")
view(data_cps)

# estimate lm
reg1 <- lm(wage ~ educ, data=data_cps)
summary(reg1)
confint(reg1)

# 2 : estimate the same regression as before,
# but with robust (or heteroskedastic-consistent) standard errors
library(sandwich)
coeftest(reg1, vcov=vcovHC(reg1, type="HC1"))

# 3 : estimate mlr
reg2 <- lm(wage ~ educ + black + female, data = data_cps)
summary(reg2)

# 4 stat sig wage differential between black and nonblack workers?
data_cps_nb <- data_cps %>% filter(data_cps$black==0)
data_cps_b <- data_cps %>% filter(data_cps$black==1)
mean(data_cps_nb$wage)
mean(data_cps_b$wage)
t.test(wage ~ black, data=data_cps)

# mlr2
reg3 <- lm(wage ~ 
             educ + black + female + northeast + midwest + south + west,
           data=data_cps)
summary(reg3)
predict(reg3, 
        data.frame(educ=12, black=1, female=0, northeast=0, midwest=0, south=1, west=0))


# Section 2: capital asset pricing model
data_capm <- import("ECN477_capm.xlsx")

# create new variables for each stock's risk premium
# first create risk free vector
rf <- data_capm$riskfree
# then make risk premium for each stock
rp_dis <- (data_capm$dis - rf)
rp_ge <- (data_capm$ge - rf)
rp_gm <- (data_capm$gm - rf)
rp_ibm <- data_capm$ibm - rf
rp_msft <- data_capm$msft - rf
rp_xom <- data_capm$xom - rf
rp_mkt <- data_capm$mkt - rf

# find highest average rp
avg_rp <- c(
    mean(rp_dis),
    mean(rp_ge),
    mean(rp_gm),
    mean(rp_ibm),
    mean(rp_msft),
    mean(rp_xom),
    mean(rp_mkt)
  )
max(avg_rp) # xom     
      

# find largest volatility (i.e. variance)
vars <- c(
    var(rp_dis),
    var(rp_ge),
    var(rp_gm),
    var(rp_ibm),
    var(rp_msft),
    var(rp_xom),
    var(rp_mkt)
  )
max(vars)  # msft     
      

# 7 : estimate capm for each firm
m_dis <- lm(rp_dis ~ rp_mkt, data = data_capm)
m_ge <- lm(rp_ge ~ rp_mkt, data=data_capm)
m_gm <- lm(rp_gm ~ rp_mkt, data=data_capm)
m_ibm <- lm(rp_ibm ~ rp_mkt, data=data_capm)
m_msft <- lm(rp_msft ~ rp_mkt, data=data_capm)
m_xom <- lm(rp_xom ~ rp_mkt, data=data_capm)

coefficients(m_dis)[2]
coefficients(m_ge)[2]
coefficients(m_gm)[2]
coefficients(m_ibm)[2]
coefficients(m_msft)[2]
coefficients(m_xom)[2]      


confint(m_dis)



#### HW3 ####

data_gun <- import("ECN477_guns.xlsx")

# create new dataframe filtering for year=1977
data_gun77 <- data_gun %>% filter(data_gun$year==77)
reg77 <- lm(log(vio) ~ 
              shall + incarc_rate + density + avginc + pop + pm1029 + pb1064 + pw1064,
            data=data_gun77)
summary(reg77)

data_gun99 <- data_gun %>% filter(data_gun$year==99)
reg99 <- lm(log(vio) ~ 
              shall + shall + incarc_rate + density + avginc + pop + pm1029 + pb1064 + pw1064,
            data=data_gun99)
summary(reg99)

reg_pool <- lm(log(vio) ~ shall + incarc_rate + density + avginc + pop + pm1029 + pb1064 + pw1064,
               data=data_gun)
summary(reg_pool)

# fixed effects regression
library(plm)
# Fixed Effects
data_gun$stateid <- factor(data_gun$stateid) # convert stateid to factor variable
data_gun$year <- factor(data_gun$year) # convert year to  factor variable
regfe <- plm(log(vio) ~ 
               shall + incarc_rate + density + avginc + pop + pm1029 + pb1064 + pw1064 + stateid + year ,
             data=data_gun, index=c("stateid","year"), effect="twoways")
summary(regfe)

# clustered standard errors
coeftest(regfe, vcov = vcovHC(regfe, type="HC1"))


#### HW4 ####
data_nels <- import("data/nels.xlsx")
view(data_nels)
data_nels$college <- data_nels

# 1
# Create COLLEGE variable from two given variables (2 yr or 4 yr college)
# data$newvariable <-  
# ifelse(data$existingVARIABLE==#(value of x) | (or) data$VARIABLE==#, # (value you want the new variable to be if observation meets criteria), # (value if criteria not met))
data_nels$college <- ifelse(data_nels$PSECHOICE==2 | data_nels$PSECHOICE==3,1,0) # create COLLEGE variable
# Percent that attend college
attend_c <- sum(data_nels$college)
attend_c / nrow(data_nels) * 100

# mean for variables
avg_grades <- mean(data_nels$GRADES)
avg_faminc <- mean(data_nels$FAMINC)
avg_famsiz <- mean(data_nels$FAMSIZ)
avg_female <- mean(data_nels$FEMALE)
avg_black <- mean(data_nels$BLACK)

# % of sample that is black
sum(data_nels$BLACK) / nrow(data_nels) * 100


# 3 : conditional averages
sum(data_nels$PSECHOICE==3) / sum(data_nels$college) * 100

sum(ifelse(data_nels$BLACK==1 & data_nels$PSECHOICE==3, 1,0)) / 
  sum(data_nels$PSECHOICE==3) * 100

# lpm
lpm <- lm(college ~ GRADES + FAMINC + FAMSIZ + PARCOLL + FEMALE + BLACK, data= data_nels)
summary(lpm)

predict(lpm, 
        data.frame(GRADES=5, FAMINC=avg_faminc, FAMSIZ=5, PARCOLL=1, FEMALE=1, BLACK=1))

# 6 : repeat for nonblack male...
predict(lpm,
        data.frame(GRADES=5, FAMINC=avg_faminc, FAMSIZ=5, PARCOLL=1, FEMALE=0, BLACK=0))
predict(lpm,
        data.frame(GRADES=5, FAMINC=avg_faminc, FAMSIZ=5, PARCOLL=1, FEMALE=1, BLACK=0))
predict(lpm, 
        data.frame(GRADES=5, FAMINC=avg_faminc, FAMSIZ=5, PARCOLL=1, FEMALE=0, BLACK=1))
# 8
predict(lpm, 
        data.frame(
          GRADES=avg_grades, FAMINC=avg_faminc, FAMSIZ=avg_famsiz, PARCOLL=avg_parcoll, FEMALE=avg_female, BLACK=avg_black))
predict(lpm,
        data.frame(
          GRADES=2.635, FAMINC=avg_faminc,FAMSIZ=avg_famsiz, PARCOLL=avg_parcoll, FEMALE=avg_female, BLACK=avg_black
        ))
# logit model
logit <- glm(college ~ GRADES + FAMINC + FAMSIZ + PARCOLL + FEMALE + BLACK, data=data_nels,
             family=binomial(link="logit"))
summary(logit)


#### 5 -- TIME SERIES ####
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
















