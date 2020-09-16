###Final Project
library("tidyr")
library("fOptions")
library("dplyr")
library("fGarch")
library("rmgarch")

##assupmtion:
##initial date: 06/27/2019
##one-month libor rate(annualized) on initial date was 2.40238%(Sources: Fred Economics Data)
r_f = 2.40238/100
##the sources of historical stock price data come from yahoofinance.com. I hope it works
##Here are the ticker list of the Dow Jones Index and the other 29 component stocks in the Dow Jones index:
ticker <- c("^DJI","AAPL","AXP","BA","CAT","CSCO","CVX","DIS","GS","HD","IBM","INTC","JNJ","JPM",
               "KO","MCD","MMM","MRK","MSFT","NKE","PFE","PG","TRV","UNH","UTX","V","VZ","WBA","WMT","XOM")
folder <- "C:/Users/z/Desktop/FIN 567/project/stock price and log return data/"
g <- ".csv"
datafiles <- paste0(rep(folder,30),ticker,rep(g,30)) 

start <- "2017-06-27"
end <- "2019-06-27"
 
##There are 503 rows of historical data for (log) stock returns(two years of historical stock data)
stock_return <- matrix(rep(0,503*30),nrow=503,ncol=30)
colnames(stock_return) <- ticker

stock_price_time0 <- rep(0,30)
for (i in 1:30) {
  data1 <- read.csv(datafiles[i],stringsAsFactors = FALSE)
  data1$Date <- as.Date(data1$Date)
  data2 <- drop_na(data1[which(data1>start & data1<=end),])
  stock_return[,i] <- data2[,3]
  stock_price_time0[i] <- data2[nrow(data2),2]
}

## now we need to handle some dividend rate for all the stocks inside our portfolio:
## you can see we have already done some preliminary calculation for dividend yieldsin another R script,
## which saves us a lot of energy and time
folder1 <- "C:/Users/z/Desktop/FIN 567/project/dividend rate/"
g1 <- "_div_yield.csv"
datafiles1 <- paste0(rep(folder1,30),ticker,rep(g1,30)) 
div <- rep(0,30)

for (i in 1:30) {
  data3 <- read.csv(datafiles1[i],stringsAsFactors = FALSE)[,2]
  if (length(data3)!=21){
    print("error")
  }
  div[i] <- data3[21]
}
cost_of_carry <- r_f-div

##now we want to calculate the K here to determine the composition of the two portfolios for both methods(k1: vega_neutral, k2: theta_neutral)
time0 <- read.csv("C:/Users/z/Desktop/FIN 567/project/calculate k/all the options on 20190627.csv",)
ticker_new <- data.frame(c("DJX",ticker[-1]))
names(ticker_new) <- "ticker"
time0_call <- time0[which(time0$days == 30 & time0$delta ==50),]
time0_put <- time0[which(time0$days == 30 & time0$delta ==-50),]

##just changing the order of the historical data by the order of ticker_new and remove some unnecessary columns
time0_call <- left_join(ticker_new, time0_call, by = "ticker")[,c("ticker","impl_volatility","impl_strike")]
time0_put <- left_join(ticker_new, time0_put, by = "ticker")[,c("ticker","impl_volatility","impl_strike")]

vega_time0_call <- rep(0,30)
vega_time0_put <- rep(0,30)
theta_time0_call <- rep(0,30)
theta_time0_put <- rep(0,30)
delta_time0_call <- rep(0,30)
delta_time0_put <- rep(0,30)
gamma_time0_call <- rep(0,30)
gamma_time0_put <- rep(0,30)
price_time0_call <- rep(0,30)
price_time0_put <- rep(0,30)
tau_time0 <- 21/252
vol_time0_call <-  time0_call[,2]
vol_time0_put <- time0_put[,2]
X_time0_call <- time0_call[,3]
X_time0_put <- time0_put[,3]

for (i in 1:30) {
   if(i==1){
     s_time0 <- stock_price_time0[i]/100
     vega_time0_call[i] <- GBSGreeks(Selection = "vega", TypeFlag = "c", S = s_time0, X = X_time0_call[i], Time = tau_time0, r = r_f, 
                                      b = cost_of_carry[i], sigma = vol_time0_call[i])
     vega_time0_put[i] <- GBSGreeks(Selection = "vega", TypeFlag = "p", S = s_time0, X = X_time0_put[i], Time = tau_time0, r = r_f, 
                                    b = cost_of_carry[i], sigma = vol_time0_put[i])
     theta_time0_call[i] <- GBSGreeks(Selection = "theta", TypeFlag = "c", S = s_time0, X = X_time0_call[i], Time = tau_time0, r = r_f, 
                                     b = cost_of_carry[i], sigma = vol_time0_call[i])
     theta_time0_put[i] <- GBSGreeks(Selection = "theta", TypeFlag = "p", S = s_time0, X = X_time0_put[i], Time = tau_time0, r = r_f, 
                                      b = cost_of_carry[i], sigma = vol_time0_put[i])
     delta_time0_call[i] <- GBSGreeks(Selection = "delta", TypeFlag = "c", S = s_time0, X = X_time0_call[i], Time = tau_time0, r = r_f, 
                                      b = cost_of_carry[i], sigma = vol_time0_call[i])
     delta_time0_put[i] <- GBSGreeks(Selection = "delta", TypeFlag = "p", S = s_time0, X = X_time0_put[i], Time = tau_time0, r = r_f, 
                                     b = cost_of_carry[i], sigma = vol_time0_put[i])
     gamma_time0_call[i] <- GBSGreeks(Selection = "gamma", TypeFlag = "c", S = s_time0, X = X_time0_call[i], Time = tau_time0, r = r_f, 
                                      b = cost_of_carry[i], sigma = vol_time0_call[i])
     gamma_time0_put[i] <- GBSGreeks(Selection = "gamma", TypeFlag = "p", S = s_time0, X = X_time0_put[i], Time = tau_time0, r = r_f, 
                                     b = cost_of_carry[i], sigma = vol_time0_put[i])
     price_time0_call[i] <- GBSOption(TypeFlag = "c", S = s_time0, X = X_time0_call[i], Time = tau_time0, r = r_f, 
                                      b = cost_of_carry[i], sigma = vol_time0_call[i])@price
     price_time0_put[i] <- GBSOption(TypeFlag = "p", S = s_time0, X = X_time0_put[i], Time = tau_time0, r = r_f, 
                                      b = cost_of_carry[i], sigma = vol_time0_put[i])@price
}else{
    vega_time0_call[i] <- GBSGreeks(Selection = "vega", TypeFlag = "c", S = stock_price_time0[i], X = X_time0_call[i], Time = tau_time0, r = r_f, 
                                  b = cost_of_carry[i], sigma = vol_time0_call[i])
    vega_time0_put[i] <- GBSGreeks(Selection = "vega", TypeFlag = "p", S = stock_price_time0[i], X = X_time0_put[i], Time = tau_time0, r = r_f, 
                                 b = cost_of_carry[i], sigma = vol_time0_put[i])
    theta_time0_call[i] <- GBSGreeks(Selection = "theta", TypeFlag = "c", S = stock_price_time0[i], X = X_time0_call[i], Time = tau_time0, r = r_f, 
                                   b = cost_of_carry[i], sigma = vol_time0_call[i])
    theta_time0_put[i] <- GBSGreeks(Selection = "theta", TypeFlag = "p", S = stock_price_time0[i], X = X_time0_put[i], Time = tau_time0, r = r_f, 
                                  b = cost_of_carry[i], sigma = vol_time0_put[i])
    delta_time0_call[i] <- GBSGreeks(Selection = "delta", TypeFlag = "c", S = stock_price_time0[i], X = X_time0_call[i], Time = tau_time0, r = r_f, 
                                     b = cost_of_carry[i], sigma = vol_time0_call[i])
    delta_time0_put[i] <- GBSGreeks(Selection = "delta", TypeFlag = "p", S = stock_price_time0[i], X = X_time0_put[i], Time = tau_time0, r = r_f, 
                                    b = cost_of_carry[i], sigma = vol_time0_put[i])
    gamma_time0_call[i] <- GBSGreeks(Selection = "gamma", TypeFlag = "c", S = stock_price_time0[i], X = X_time0_call[i], Time = tau_time0, r = r_f, 
                                     b = cost_of_carry[i], sigma = vol_time0_call[i])
    gamma_time0_put[i] <- GBSGreeks(Selection = "gamma", TypeFlag = "p", S = stock_price_time0[i], X = X_time0_put[i], Time = tau_time0, r = r_f, 
                                    b = cost_of_carry[i], sigma = vol_time0_put[i])
    price_time0_call[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=stock_price_time0[i], X=X_time0_call[i], Time = tau_time0,
                                                r=r_f, b=cost_of_carry[i], sigma=vol_time0_call[i], n=tau_time0*252, title = NULL, description = NULL)@price
    price_time0_put[i] <- CRRBinomialTreeOption(TypeFlag = "pa", S=stock_price_time0[i], X=X_time0_put[i], Time = tau_time0,
                                               r=r_f, b=cost_of_carry[i], sigma=vol_time0_put[i], n=tau_time0*252, title = NULL, description = NULL)@price
  }
}
k_vega_neutral <- (vega_time0_call[1]+vega_time0_put[1])/sum(vega_time0_call[2:30],vega_time0_put[2:30])
k_vega_neutral
k_theta_neutral <- (theta_time0_call[1]+theta_time0_put[1])/sum(theta_time0_call[2:30],theta_time0_put[2:30])
k_theta_neutral


##if we assume we sell one index straddle and buy kwi components stocks straddles(wi is equal among all
## the component stocks, which is 1/29), we generate the value of new k here for two formulas
k_vega_actual <- k_vega_neutral * 29
k_vega_actual 
k_theta_actual <- k_theta_neutral * 29
k_theta_actual

## calculating greek letter risks of both portfolios constructed on time 0 after getting the value of k
## vega_neutral
vega_port_vega <- -(vega_time0_call[1] + vega_time0_put[1])+ k_vega_neutral * sum(vega_time0_call[2:30],vega_time0_put[2:30])
vega_port_vega 
theta_port_vega <- -(theta_time0_call[1] + theta_time0_put[1])+ k_vega_neutral * sum(theta_time0_call[2:30],theta_time0_put[2:30])
theta_port_vega
gamma_port_vega <- -(gamma_time0_call[1] + gamma_time0_put[1])+ k_vega_neutral * sum(gamma_time0_call[2:30],gamma_time0_put[2:30])
gamma_port_vega
delta_port_vega <- -(delta_time0_call[1] + delta_time0_put[1])+ k_vega_neutral * sum(delta_time0_call[2:30],delta_time0_put[2:30])
delta_port_vega 

## theta_neutral(Greek Letter Risks)
vega_port_theta <- -(vega_time0_call[1] + vega_time0_put[1])+ k_theta_neutral * sum(vega_time0_call[2:30],vega_time0_put[2:30])
vega_port_theta 
theta_port_theta <- -(theta_time0_call[1] + theta_time0_put[1])+ k_theta_neutral * sum(theta_time0_call[2:30],theta_time0_put[2:30])
theta_port_theta
gamma_port_theta <- -(gamma_time0_call[1] + gamma_time0_put[1])+ k_theta_neutral * sum(gamma_time0_call[2:30],gamma_time0_put[2:30])
gamma_port_theta
delta_port_theta <- -(delta_time0_call[1] + delta_time0_put[1])+ k_theta_neutral * sum(delta_time0_call[2:30],delta_time0_put[2:30])
delta_port_theta

##calculate the initial portfolio value at time 0 for both methods:
port_time0_vega_neutral <- -(price_time0_call[1]+price_time0_put[1])+k_vega_neutral*sum(price_time0_call[2:30],price_time0_put[2:30])
port_time0_vega_neutral
port_time0_theta_neutral <- -(price_time0_call[1]+price_time0_put[1])+k_theta_neutral*sum(price_time0_call[2:30],price_time0_put[2:30])
port_time0_theta_neutral

##Next we do some data pre-processing of the historical implied volatilities of all the component options inside the Dow Jones index
his_vol <- read.csv("C:/Users/z/Desktop/FIN 567/project/historical option data of all the options/historical option data.csv",)
his_vol_important <- his_vol[,c(2,3,4,5,7,8)]
his_call <- his_vol_important[which(his_vol_important$days == 30 & his_vol_important$delta ==50),]
his_put  <- his_vol_important[which(his_vol_important$days == 30 & his_vol_important$delta ==-50),]

vol_call_log <- matrix(rep(0,30*503),nrow = 503, ncol = 30)
vol_put_log <- matrix(rep(0,30*503),nrow = 503, ncol = 30)
colnames(vol_call_log) <- paste0(ticker_new[,1],rep("_vol_call",30))
colnames(vol_put_log) <- paste0(ticker_new[,1],rep("_vol_put",30))
vol_call_time0 <- rep(0,30)
vol_put_time0 <- rep(0,30)

for (i in 1:30) {
  call_one <- his_call[which(his_call$ticker==ticker_new[i,1]),"impl_volatility"]
  put_one <- his_put[which(his_put$ticker==ticker_new[i,1]),"impl_volatility"]
  vol_call_time0[i] <- call_one[length(call_one)]
  vol_put_time0[i] <- put_one[length(put_one)]
  vol_call_log[,i]=log(call_one[2:length(call_one)]/call_one[1:length(call_one)-1])
  vol_put_log[,i]=log(put_one[2:length(put_one)]/put_one[1:length(put_one)-1])
}

colnames(stock_return) <- ticker_new[,1]
market_factor <- cbind(stock_return,vol_call_log,vol_put_log) ##important very important

## As an initial step (not required), estimate a univariate GARCH model for each return process(there are 93 factors)
## Specify univariate GARCH(1,1) model and set mean return = 0
uspec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                    distribution.model = "norm") 

## Check the univariate specification for the 90 series, actually we don't really care about the 
## parameters for each garch model, what we really want is the daily shock and the estimated sigma 
## on last day for each market factor to simulate sigma

sigma_tplus1 <- rep(0,90)
daily_shock <- matrix(rep(0, 90*503),nrow=503,ncol=90)
colnames(daily_shock) <- colnames(market_factor)

## we only record the parameters of the garch(1,1) model for the log_return of stocks for future use
garch_stock_coef <- matrix(rep(0, 30*3), nrow = 3, ncol=30)
colnames(garch_stock_coef) <-  ticker_new[,1] 

for (j in 1:90) {
  log_return <- market_factor[,j]
  fit.marg <- ugarchfit(spec = uspec, data = log_return)
  if (j <= 30) {
    garch_stock_coef[,j] <- coef(fit.marg)
  }
  daily_shock[,j] <- fit.marg@fit[["z"]]
  sigma_time0 <- fit.marg@fit[["sigma"]][length(fit.marg@fit[["sigma"]])]
  log_return_time0 <- log_return[length(log_return)]
  ## use the garch model to estimate the estimated variance for each market factor on time 1
  sigma_tplus1[j] <- sqrt(sum(coef(fit.marg)*c(1,log_return_time0^2,sigma_time0^2)))
}

##Let's right now create a function of FHS process assuming constant correlations in order to simulate
## the log_return of all the market factors on time 1
multivariate_FHS_1day <- function(FH, shock_database, sigma_tplus1){
  set.seed(123)
  sim_ret_tplus1 <- matrix(rep(0, ncol(shock_database)*FH),nrow = ncol(shock_database), ncol = FH)
  colnames(sim_ret_tplus1) <- 1:FH
  for (k in 1:FH) {
     m <- sample(1:nrow(shock_database),1,replace = T)
     z_tplus1_sim <- shock_database[m,]
     sim_ret_tplus1[,k] <- z_tplus1_sim * sigma_tplus1
  }
  return(sim_ret_tplus1)
}
sim_ret_tplus1_1d <- multivariate_FHS_1day(FH = 10000, shock_database = daily_shock, sigma_tplus1 = sigma_tplus1)

## sort out all the market factors at time 0
variable_time0 <- cbind(stock_price_time0, vol_call_time0, vol_put_time0)

port_value_sim1_vega <- rep(0,10000)
port_value_sim1_theta <- rep(0,10000)
tau_sim1 <- 20/252 ## one day passed

for (l in 1:10000) {
  stock_sim1 <- variable_time0[,1] * exp(sim_ret_tplus1_1d[,l][1:30])
  vol_call_sim1 <- variable_time0[,2] * exp(sim_ret_tplus1_1d[,l][31:60])
  vol_put_sim1 <- variable_time0[,3] * exp(sim_ret_tplus1_1d[,l][61:90])
  price_call_sim1 <- rep(0,30)
  price_put_sim1 <- rep(0,30)
  for (i in 1:30) {
    if(i==1){
      s_sim1_DJX <- stock_sim1[1]/100
      price_call_sim1[i] <- GBSOption(TypeFlag = "c", S = s_sim1_DJX , X = X_time0_call[i], Time = tau_sim1, r = r_f, 
                                       b = cost_of_carry[i], sigma = vol_call_sim1[i])@price
      price_put_sim1[i] <- GBSOption(TypeFlag = "p", S = s_sim1_DJX , X = X_time0_put[i], Time = tau_sim1, r = r_f, 
                                      b = cost_of_carry[i], sigma = vol_put_sim1[i])@price
    }else{
      price_call_sim1[i] <- CRRBinomialTreeOption(TypeFlag = "ca", S=stock_sim1[i], X=X_time0_call[i], Time = tau_sim1,
                                                   r=r_f, b=cost_of_carry[i], sigma=vol_call_sim1[i], n=tau_sim1*252, title = NULL, description = NULL)@price
      price_put_sim1[i] <- CRRBinomialTreeOption(TypeFlag = "pa", S=stock_sim1[i], X=X_time0_put[i], Time = tau_sim1,
                                                  r=r_f, b=cost_of_carry[i], sigma=vol_put_sim1[i], n=tau_sim1*252, title = NULL, description = NULL)@price
    }
  }
  port_value_sim1_vega[l] <- -(price_call_sim1[1]+price_put_sim1[1])+k_vega_neutral*sum(price_call_sim1[2:30],price_put_sim1[2:30])
  port_value_sim1_theta[l] <- -(price_call_sim1[1]+price_put_sim1[1])+k_theta_neutral*sum(price_call_sim1[2:30],price_put_sim1[2:30])
}

PL_vega_sim1 <- port_value_sim1_vega-port_time0_vega_neutral
PL_theta_sim1 <-  port_value_sim1_theta-port_time0_theta_neutral

VaR_vega_1perc <- abs(quantile(PL_vega_sim1, 0.01))
VaR_vega_1perc
VaR_vega_5perc <- abs(quantile(PL_vega_sim1, 0.05))
VaR_vega_5perc
VaR_theta_1perc <- abs(quantile(PL_theta_sim1, 0.01))
VaR_theta_1perc
VaR_theta_5perc <- abs(quantile(PL_theta_sim1, 0.05))
VaR_theta_5perc

hist(PL_vega_sim1, main = "P/L Distribution of the vega-neutral portfolio(1-day horizon)", xlab = "P/L",breaks=100,col = "green",
     border = "blue")
hist(PL_theta_sim1, main = "P/L Distribution of the theta-neutral portfolio(1-day horizon)", xlab = "P/L", breaks=100,col = "green",
     border = "blue")

##calculate the expected shortfall based on the VaR we have generated above
ES_1perc_vega_1 = -mean(PL_vega_sim1[PL_vega_sim1<=(-VaR_vega_1perc)])
ES_1perc_vega_1
ES_5perc_vega_1 = -mean(PL_vega_sim1[PL_vega_sim1<=(-VaR_vega_5perc)])
ES_5perc_vega_1
ES_1perc_theta_1 = -mean(PL_theta_sim1[PL_theta_sim1<=(-VaR_theta_1perc)])
ES_1perc_theta_1
ES_5perc_theta_1 = -mean(PL_theta_sim1[PL_theta_sim1<=(-VaR_theta_5perc)])
ES_5perc_theta_1


##now we want to calculate the VaR of both portfolios using a horizon of one-month(21 trading days)
##since after 21 trading days, all the options are expired simultaneously, we only need to simulate 
##30 market factors(log_return of the 30 stocks' prices)and use the option payoff the calculate
## the profits/losses for the portfolios in 21 trading days.

sigma_initial <- sigma_tplus1[1:30]
garch_stock_coef
daily_shock_stock <- daily_shock[,1:30]
multivariate_FHS_1month <- function(FH, shock_database, sigma_initial, garch_coef){
  set.seed(123)
  horizon <- 21
  sim_ret_cul <- matrix(rep(0, ncol(shock_database)*FH),nrow = ncol(shock_database), ncol = FH)
  for (day in 1:horizon) {
    if (day==1) {
      sim_ret_new <- matrix(rep(0, ncol(shock_database)*FH),nrow = ncol(shock_database), ncol = FH)
      sim_std_new <- matrix(rep(0, ncol(shock_database)*FH),nrow = ncol(shock_database), ncol = FH)
      for (i in 1:FH) {
        m <- sample(1:nrow(shock_database),1,replace = T)
        z_tplus1_sim <- shock_database[m,]
        log_return_tplus1_sim <- z_tplus1_sim * sigma_initial
        sim_ret_new[,i] <- log_return_tplus1_sim
        for (j in 1:ncol(shock_database)) {
          sigma_initial[j] <- sqrt(sum(garch_coef[,j]*c(1,log_return_tplus1_sim[j]^2,sigma_initial[j]^2)))
          }
        sim_std_new[,i] <- sigma_initial}
      sim_ret_cul <- sim_ret_new
      }else{
        for (i in 1:FH) {
          m <- sample(1:nrow(shock_database),1,replace = T)
          z_tplus1_sim <- shock_database[m,]
          log_return_tplus1_sim <- z_tplus1_sim * sim_std_new[,i]
          sim_ret_new[,i] <- log_return_tplus1_sim
          for (j in 1:ncol(shock_database)) {
            sigma_initial[j] <- sqrt(sum(garch_coef[,j]*c(1,log_return_tplus1_sim[j]^2,sim_std_new[j,i]^2)))
          }
          sim_std_new[,i] <- sigma_initial}
      sim_ret_cul <- sim_ret_new + sim_ret_cul
      }
  }
  return(sim_ret_cul)
}
#cbind(sim_ret_cul, sim_ret_new)
FHS_ret_21_sim <- multivariate_FHS_1month(FH=10000, shock_database=daily_shock_stock, sigma_initial=sigma_initial, garch_coef=garch_stock_coef)

port_value_sim21_vega <- rep(0,10000)
port_value_sim21_theta <- rep(0,10000)   
for(i in 1:10000){
  price_call_sim21 <- rep(0,30)
  price_put_sim21 <- rep(0,30)
  stock_sim_expiration <- exp(FHS_ret_21_sim[,i])*stock_price_time0
  for (j in 1:30) {
    if (j==1) {
      stock_sim21_DJX <- stock_sim_expiration[1]/100
      price_call_sim21[j] <- max(stock_sim21_DJX - X_time0_call[j],0)
      price_put_sim21[j] <- max(X_time0_put[j] - stock_sim21_DJX,0)
    }else{
      price_call_sim21[j] <- max(stock_sim_expiration[j] - X_time0_call[j],0)
      price_put_sim21[j] <- max(X_time0_put[j] - stock_sim_expiration[j],0)
    }
  }
  port_value_sim21_vega[i] <- -(price_call_sim21[1]+price_put_sim21[1])+k_vega_neutral*sum(price_call_sim21[2:30],price_put_sim21[2:30])
  port_value_sim21_theta[i] <- -(price_call_sim21[1]+price_put_sim21[1])+k_theta_neutral*sum(price_call_sim21[2:30],price_put_sim21[2:30])
}

PL_vega_sim21 <- port_value_sim21_vega-port_time0_vega_neutral
PL_theta_sim21 <-  port_value_sim21_theta-port_time0_theta_neutral

VaR_vega_1perc_21 <- abs(quantile(PL_vega_sim21, 0.01))
VaR_vega_1perc_21
VaR_vega_5perc_21 <- abs(quantile(PL_vega_sim21, 0.05))
VaR_vega_5perc_21
VaR_theta_1perc_21 <- abs(quantile(PL_theta_sim21, 0.01))
VaR_theta_1perc_21
VaR_theta_5perc_21 <- abs(quantile(PL_theta_sim21, 0.05))
VaR_theta_5perc_21

hist(PL_vega_sim21, main = "P/L Distribution of the vega-neutral portfolio(1-month horizon)", xlab = "P/L",breaks=1000,col = "green",
     border = "blue", xlim=c(-25,25))
hist(PL_theta_sim21, main = "P/L Distribution of the theta-neutral portfolio(1-month horizon)", xlab = "P/L",breaks=1000,col = "green",
     border = "blue", xlim=c(-20,20))

ES_1perc_vega_21 = -mean(PL_vega_sim21[PL_vega_sim21<=(-VaR_vega_1perc_21 )])
ES_1perc_vega_21
ES_5perc_vega_21 = -mean(PL_vega_sim21[PL_vega_sim21<=(-VaR_vega_5perc_21)])
ES_5perc_vega_21
ES_1perc_theta_21 = -mean(PL_theta_sim21[PL_theta_sim21<=(-VaR_theta_1perc_21)])
ES_1perc_theta_21
ES_5perc_theta_21 = -mean(PL_theta_sim21[PL_theta_sim21<=(-VaR_theta_5perc_21)])
ES_5perc_theta_21
