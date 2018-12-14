# Portfolio return  
#Transform Daily Prices to Monthly Log Returns
# http://www.reproduciblefinance.com/code/
#http://www.reproduciblefinance.com/2017/09/25/asset-prices-to-log-returns/
# http://www.reproduciblefinance.com/2017/10/12/introduction-to-portfolio-returns/
#install.packages("timetk")
#install.packages("tidyquant")
#install.packages("tibbletime")
library(tidyverse)
library(tidyquant)
library(timetk)
library(tibbletime)
# We are building toward analyzing the returns of a 5-asset portfolio consisting of the following.
# 
# + SPY (S&P500 fund) weighted 25%
# + EFA (a non-US equities fund) weighted 25%
# + IJS (a small-cap value fund) weighted 20%
# + EEM (an emerging-mkts fund) weighted 20%
# + AGG (a bond fund) weighted 10%
# I chose those 5 assets because they seem to offer a balanced blend of large, small, international, emerging and bond exposure. We will eventually build a Shiny app to let users choose different assets and see different results. Today, we are just going to work with the individual prices/returns of those 5 assets.

# The symbols vector holds our tickers. 
symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

prices <- getSymbols(symbols, 
                     src = 'yahoo', 
                     from = "2012-12-31",
                     to = "2017-12-31",
                     auto.assign = TRUE, 
                     warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>%
  `colnames<-`(symbols)
# We now have an xts object of the adjusted prices for our 5 assets. Have a quick peek.
prices %>% head
#Our first reading is from January 3, 2005 (the first trading day of that year) and we have daily prices. Let’s stay in thextsworld and convert to monthly prices using a call to to.monthly(prices, indexAt = "last", OHLC = FALSE) from quantmod. The argument index = "last" tells the function whether we want to index to the first day of the month or the last day.
prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)

head(prices_monthly)

#Note this will give us log returns by the method = "log" argument. We could have used method = "discrete" to get simple returns.
asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))

head(asset_returns_xts)

asset_returns_dplyr_byhand <- prices %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>%
  spread(asset, returns) %>% 
  select(date, symbols)

# method 2
asset_returns_tq_builtin <- prices %>%
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, prices, -date) %>% 
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn, period = "monthly", type = "log") %>% 
  spread(asset, monthly.returns) %>% 
  select(date, symbols)

asset_returns_tbltime <- 
  prices %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  tbl_time(index = date) %>% 
  as_period("monthly", side = "end") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>% 
  tq_transmute(mutate_fun = periodReturn, type = "log") %>% 
  spread(asset, monthly.returns) %>% 
  select(date, symbols)


#Let’s take a peek at our 4 monthly log return objects.

head(asset_returns_xts)
head(asset_returns_dplyr_byhand)
head(asset_returns_tq_builtin)
head(asset_returns_tbltime)

asset_returns_long_format <- 
  asset_returns_dplyr_byhand %>% 
  gather(asset, returns, -date)

head(asset_returns_long_format)
#We now have 3 columns and if we want to run an operation like mutate on each asset, we just need to call goup_by(asset) in the piped flow.
