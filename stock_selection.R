library(quantmod)
library(tidyverse)
library(rvest)
library(lubridate)
library(tidyquant)
library(dygraphs)

startDate = "2008-01-01"
endDate = "2018-10-15"

getSymbols("AVGO", from = startDate, to = endDate)
MA %>% Ad() %>% chartSeries()

## web scraping: get the list of S&P500 stocks
# Web-scrape SP500 stock list
sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
  html_node("table.wikitable") %>%
  html_table() %>%
  select(`Ticker symbol`, Security, `GICS Sector`, `GICS Sub Industry`) %>% 
  as_tibble()
# Format names
names(sp_500) <- sp_500 %>% 
  names() %>% 
  str_to_lower() %>% 
  make.names()
# Show results
sp_500 
sp_500$ticker.symbol <- gsub('[.]', '-', sp_500$ticker.symbol)

# count of distinct values for each column
sp_500 %>% 
  lapply(function(x) x %>% unique() %>% length()) %>%
  unlist() # show in condensed format

# distribution of securities by sector
sp_500 %>%
  # Summarise data by frequency
  group_by(gics.sector) %>%
  summarise(count = n()) %>%
  # Visualize 
  ggplot(aes(x = gics.sector %>% fct_reorder(count),
             y = count
            )) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), size = 3, nudge_y = 4, nudge_x = .1) + 
  scale_y_continuous(limits = c(0,80)) +
  ggtitle(label = "Sector Frequency Among S&P500 Stocks") +
  xlab(label = "GICS Sector") +
  theme(plot.title = element_text(size = 16)) + 
  coord_flip() 
### selecting from different industries and sectors helps to reduce correlation and diversify the portfolio



############ creating functions to map
get_stock_prices <- function(ticker, return_format = "tibble", ...) {
  # Get stock prices
  stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...)
  # Rename
  names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    stock_prices <- stock_prices_xts %>%
      as_tibble() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  } else {
    stock_prices <- stock_prices_xts
  }
  stock_prices
}


get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
  # Convert tibble to xts
  if (!is.xts(x)) {
    x <- xts(x[,-1], order.by = x$Date)
  }
  # Get log returns
  log_returns_xts <- periodReturn(x = x$Adjusted, type = 'log', period = period, ...)
  # Rename
  names(log_returns_xts) <- "Log.Returns"
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    log_returns <- log_returns_xts %>%
      as_tibble() %>%
      rownames_to_column(var = "Date") %>%
      mutate(Date = ymd(Date))
  } else {
    log_returns <- log_returns_xts
  }
  log_returns
}


sp_500_detail <- sp_500 %>% filter(!ticker.symbol %in% c('BHF', 'DXC', 'JEF', 'UA')) %>%
  mutate(
    stock.prices = map(ticker.symbol, 
                       function(.x) get_stock_prices(.x, 
                                                     return_format = "tibble",
                                                     from = startDate,
                                                     to = endDate)
    ),
    log.returns  = map(stock.prices, 
                       function(.x) get_log_returns(.x, return_format = "tibble")),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, ~ sum(complete.cases(.)))
  ) 

sp_500_detail %>% 
  select(ticker.symbol, mean.log.returns:n.trade.days) 
### want stocks with a large number of observations because this gives more 
### samples (and a longer time span) to trend the stock, thus increasing our confidence in the statistics.

sp_500_short <- sp_500_detail %>% 
  select(ticker.symbol, mean.log.returns:n.trade.days)

sp_500_short %>% 
  select(ticker.symbol, mean.log.returns:n.trade.days) %>%
  ggplot(aes(sd.log.returns, mean.log.returns, color = n.trade.days)) + 
    geom_point(shape=16, aes(size = n.trade.days), alpha = .4) + 
    theme_minimal() + 
    scale_color_gradient(low = "#0091ff", high = "#f0650e") + 
    scale_alpha(range=c(.25, .6)) + 
  labs(x = "Risk (StDev Log Returns)", y = "Growth (Mean Log Returns)", title = "S&P 500 Stock Risk vs Growth") 

# zoom in
sp_500_short %>% 
  select(ticker.symbol, mean.log.returns:n.trade.days) %>%
  ggplot(aes(sd.log.returns, mean.log.returns, color = n.trade.days)) + 
  geom_point(shape=16, aes(size = n.trade.days), alpha = .4) + 
  theme_minimal() + 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") + 
  scale_alpha(range=c(.25, .6)) + 
  coord_cartesian(xlim = c(0.01, 0.033), ylim = c(0.001, 0.00132)) + 
  geom_text(aes(label=ticker.symbol), hjust = 0, vjust = 0) + 
  geom_rect(mapping = aes(xmin = 0.016, xmax = 0.0328, ymin = 0.00109, ymax = 0.00125, fill = factor(1)), alpha = 0) + 
  guides(fill=FALSE) + 
  labs(x = "Risk (StDev Log Returns)", y = "Growth (Mean Log Returns)") 

# output 4 selected stock
sp_500_short %>%
  filter(sd.log.returns < 0.0315,
         mean.log.returns >= 0.001) %>%
  select(ticker.symbol, mean.log.returns:n.trade.days) %>%
  arrange(mean.log.returns %>% desc()) %>% 
  left_join(sp_500, by = 'ticker.symbol') %>% filter(ticker.symbol %in% c('AVGO', 'ABMD', 'ROST', 'ALGN'))

### combined: straw broom charts
tickers <- c('AVGO_xts', 'ABMD_xts', 'ROST_xts', 'ALGN_xts')
closePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
names(closePrices) <- c('AVGO.Close', 'ABMD.Close', 'ROST.Close', 'ALGN.Close')
dygraph(closePrices, main = "AVGO, ABMD, ROST and ALGN Close Prices", group = "stock") %>%
  dyRangeSelector()


### AVGO
AVGO <- tq_get("AVGO", get = "stock.prices", from = startDate, to = endDate)
AVGO_xts <- xts(AVGO[,-1], order.by = as.Date(AVGO$date))
# candlestick chart
dygraph(AVGO_xts[,1:4], main = "AVGO: Broadcom Candlestick") %>% dyCandlestick()
# candlestick + Bollinger Bands
week_num = 25      # input
end <- ymd(Sys.Date())
start <- end - weeks(week_num)
AVGO %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, close = close, high = high, low = low)) + 
  geom_bbands(aes(high = high, low = low, close = close),
              ma_fun = SMA, n = 20, sd = 2, size = 1) + 
  labs(title = str_c("AVGO: Broadcom Candlestick with Bollinger Bands for the past ", week_num, " weeks"),
       x = "", y = "Closing Price") +
  coord_x_date(xlim = c(start, end),
               ylim = c(180, 300))
# daily return distribution
AVGO_log_returns <- AVGO_xts %>%
  Ad() %>%
  dailyReturn(type = "log")
names(AVGO_log_returns) <- "AVGO.Log.Returns"
AVGO_log_returns %>%    
  ggplot(aes(x = AVGO.Log.Returns)) + 
  geom_histogram(bins = 100) + 
  geom_density() +
  geom_rug(alpha = 0.5) + 
  labs(title = "AVGO: Daily log returns", y = "Count")
# Monte-Carlo
N <- 252 # Number of Stock Price Simulations
M <- 250  # Number of Monte Carlo Simulations   
mu <- mean(AVGO_log_returns, na.rm = TRUE)
sigma <- sd(AVGO_log_returns, na.rm = TRUE)
day <- 1:N
price_init <- AVGO_xts$adjusted[[nrow(AVGO_xts$adjusted)]]
# Simulate prices
set.seed(123)
monte_carlo_mat <- matrix(nrow = N, ncol = M)
for (j in 1:M) {
  monte_carlo_mat[[1, j]] <- price_init
  for(i in 2:N) {
    monte_carlo_mat[[i, j]] <- monte_carlo_mat[[i - 1, j]] * exp(rnorm(1, mu, sigma))
  }
}
# Format and organize data frame
price_sim <- cbind(day, monte_carlo_mat) %>% as_tibble() 
nm <- str_c("Sim.", seq(1, M))
nm <- c("Day", nm)
names(price_sim) <- nm
price_sim <- price_sim %>%
  gather(key = "Simulation", value = "Stock.Price", -(Day))
# Visualize simulation
price_sim %>%
  ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) + 
  geom_line(alpha = 0.1) +
  ggtitle(str_c("AVGO: ", M, 
                " Monte Carlo Simulations for Prices Over ", N, 
                " Trading Days"))

end_stock_prices <- price_sim %>% filter(Day == max(Day))
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_end_stock_prices <- quantile(end_stock_prices$Stock.Price, probs = probs)
dist_end_stock_prices %>% round(2)   # put it in a table

# Inputs
N_hist          <- nrow(AVGO_xts) / 252
p_start_hist    <- AVGO_xts$adjusted[[1]]
p_end_hist      <- AVGO_xts$adjusted[[nrow(AVGO_xts)]]
N_sim           <- N / 252
p_start_sim     <- p_end_hist
p_end_sim       <- dist_end_stock_prices[[4]]
# Compound annual growth rate (CAGR) calculations
CAGR_historical <- (p_end_hist / p_start_hist) ^ (1 / N_hist) - 1   # 35.72%
CAGR_sim        <- (p_end_sim / p_start_sim) ^ (1 / N_sim) - 1    # 34.19%


### ABMD
ABMD <- tq_get("ABMD", get = "stock.prices", from = startDate, to = endDate)
ABMD_xts <- xts(ABMD[,-1], order.by = as.Date(ABMD$date))
# candlestick chart
dygraph(ABMD_xts[,1:4], main = "ABMD: ABIOMED Inc Candlestick") %>% dyCandlestick()
# candlestick + Bollinger Bands
ABMD %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, close = close, high = high, low = low)) + 
  geom_bbands(aes(high = high, low = low, close = close),
              ma_fun = SMA, n = 20, sd = 2, size = 1) + 
  labs(title = str_c("ABMD: ABIOMED Inc Candlestick with Bollinger Bands for the past ", week_num, " weeks"),
       x = "", y = "Closing Price") +
  coord_x_date(xlim = c(start, end),
               ylim = c(270, 500))
# daily return distribution
ABMD_log_returns <- ABMD_xts %>%
  Ad() %>%
  dailyReturn(type = "log")
names(ABMD_log_returns) <- "ABMD.Log.Returns"
ABMD_log_returns %>%    
  ggplot(aes(x = ABMD.Log.Returns)) + 
  geom_histogram(bins = 100) + 
  geom_density() +
  geom_rug(alpha = 0.5) + 
  labs(title = "ABMD: Daily log returns", y = "Count")
# Monte-Carlo
N <- 252 # Number of Stock Price Simulations
M <- 250  # Number of Monte Carlo Simulations   
mu <- mean(ABMD_log_returns, na.rm = TRUE)
sigma <- sd(ABMD_log_returns, na.rm = TRUE)
day <- 1:N
price_init <- ABMD_xts$adjusted[[nrow(ABMD_xts$adjusted)]]
# Simulate prices
set.seed(123)
monte_carlo_mat <- matrix(nrow = N, ncol = M)
for (j in 1:M) {
  monte_carlo_mat[[1, j]] <- price_init
  for(i in 2:N) {
    monte_carlo_mat[[i, j]] <- monte_carlo_mat[[i - 1, j]] * exp(rnorm(1, mu, sigma))
  }
}
# Format and organize data frame
price_sim <- cbind(day, monte_carlo_mat) %>% as_tibble() 
nm <- str_c("Sim.", seq(1, M))
nm <- c("Day", nm)
names(price_sim) <- nm
price_sim <- price_sim %>%
  gather(key = "Simulation", value = "Stock.Price", -(Day))
# Visualize simulation
price_sim %>%
  ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) + 
  geom_line(alpha = 0.1) +
  ggtitle(str_c("ABMD: ", M, 
                " Monte Carlo Simulations for Prices Over ", N, 
                " Trading Days"))

end_stock_prices <- price_sim %>% filter(Day == max(Day))
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_end_stock_prices <- quantile(end_stock_prices$Stock.Price, probs = probs)
dist_end_stock_prices %>% round(2)   # put it in a table

# Inputs
N_hist          <- nrow(ABMD_xts) / 252
p_start_hist    <- ABMD_xts$adjusted[[1]]
p_end_hist      <- ABMD_xts$adjusted[[nrow(ABMD_xts)]]
N_sim           <- N / 252
p_start_sim     <- p_end_hist
p_end_sim       <- dist_end_stock_prices[[4]]
# Compound annual growth rate (CAGR) calculations
CAGR_historical <- (p_end_hist / p_start_hist) ^ (1 / N_hist) - 1   # 34.92%
CAGR_sim        <- (p_end_sim / p_start_sim) ^ (1 / N_sim) - 1    # 32.79%

### ROST: Ross Stores
ROST <- tq_get("ROST", get = "stock.prices", from = startDate, to = endDate)
ROST_xts <- xts(ROST[,-1], order.by = as.Date(ROST$date))
# candlestick chart
dygraph(ROST_xts[,1:4], main = "ROST: Ross Stores Candlestick") %>% dyCandlestick()
# candlestick + Bollinger Bands
ROST %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, close = close, high = high, low = low)) + 
  geom_bbands(aes(high = high, low = low, close = close),
              ma_fun = SMA, n = 20, sd = 2, size = 1) + 
  labs(title = str_c("ROST: Ross Stores Candlestick with Bollinger Bands for the past ", week_num, " weeks"),
       x = "", y = "Closing Price") +
  coord_x_date(xlim = c(start, end),
               ylim = c(70, 105))
# daily return distribution
ROST_log_returns <- ROST_xts %>%
  Ad() %>%
  dailyReturn(type = "log")
names(ROST_log_returns) <- "ROST.Log.Returns"
ROST_log_returns %>%    
  ggplot(aes(x = ROST.Log.Returns)) + 
  geom_histogram(bins = 100) + 
  geom_density() +
  geom_rug(alpha = 0.5) + 
  labs(title = "ROST: Daily log returns", y = "Count")
# Monte-Carlo
N <- 252 # Number of Stock Price Simulations
M <- 250  # Number of Monte Carlo Simulations   
mu <- mean(ROST_log_returns, na.rm = TRUE)
sigma <- sd(ROST_log_returns, na.rm = TRUE)
day <- 1:N
price_init <- ROST_xts$adjusted[[nrow(ROST_xts$adjusted)]]
# Simulate prices
set.seed(123)
monte_carlo_mat <- matrix(nrow = N, ncol = M)
for (j in 1:M) {
  monte_carlo_mat[[1, j]] <- price_init
  for(i in 2:N) {
    monte_carlo_mat[[i, j]] <- monte_carlo_mat[[i - 1, j]] * exp(rnorm(1, mu, sigma))
  }
}
# Format and organize data frame
price_sim <- cbind(day, monte_carlo_mat) %>% as_tibble() 
nm <- str_c("Sim.", seq(1, M))
nm <- c("Day", nm)
names(price_sim) <- nm
price_sim <- price_sim %>%
  gather(key = "Simulation", value = "Stock.Price", -(Day))
# Visualize simulation
price_sim %>%
  ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) + 
  geom_line(alpha = 0.1) +
  ggtitle(str_c("ROST: ", M, 
                " Monte Carlo Simulations for Prices Over ", N, 
                " Trading Days"))

end_stock_prices <- price_sim %>% filter(Day == max(Day))
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_end_stock_prices <- quantile(end_stock_prices$Stock.Price, probs = probs)
dist_end_stock_prices %>% round(2)   # put it in a table

# Inputs
N_hist          <- nrow(ROST_xts) / 252
p_start_hist    <- ROST_xts$adjusted[[1]]
p_end_hist      <- ROST_xts$adjusted[[nrow(ROST_xts)]]
N_sim           <- N / 252
p_start_sim     <- p_end_hist
p_end_sim       <- dist_end_stock_prices[[4]]
# Compound annual growth rate (CAGR) calculations
CAGR_historical <- (p_end_hist / p_start_hist) ^ (1 / N_hist) - 1   # 32.31%
CAGR_sim        <- (p_end_sim / p_start_sim) ^ (1 / N_sim) - 1    # 31.06%

### ALGN: Align Technology
ALGN <- tq_get("ALGN", get = "stock.prices", from = startDate, to = endDate)
ALGN_xts <- xts(ALGN[,-1], order.by = as.Date(ALGN$date))
# candlestick chart
dygraph(ALGN_xts[,1:4], main = "ALGN: Align Technology Candlestick") %>% dyCandlestick()
# candlestick + Bollinger Bands
ALGN %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, close = close, high = high, low = low)) + 
  geom_bbands(aes(high = high, low = low, close = close),
              ma_fun = SMA, n = 20, sd = 2, size = 1) + 
  labs(title = str_c("ALGN: Align Technology Candlestick with Bollinger Bands for the past ", week_num, " weeks"),
       x = "", y = "Closing Price") +
  coord_x_date(xlim = c(start, end),
               ylim = c(200, 450))
# daily return distribution
ALGN_log_returns <- ALGN_xts %>%
  Ad() %>%
  dailyReturn(type = "log")
names(ALGN_log_returns) <- "ALGN.Log.Returns"
ALGN_log_returns %>%    
  ggplot(aes(x = ALGN.Log.Returns)) + 
  geom_histogram(bins = 100) + 
  geom_density() +
  geom_rug(alpha = 0.5) + 
  labs(title = "ALGN: Daily log returns", y = "Count")
# Monte-Carlo
N <- 252 # Number of Stock Price Simulations
M <- 250  # Number of Monte Carlo Simulations   
mu <- mean(ALGN_log_returns, na.rm = TRUE)
sigma <- sd(ALGN_log_returns, na.rm = TRUE)
day <- 1:N
price_init <- ALGN_xts$adjusted[[nrow(ALGN_xts$adjusted)]]
# Simulate prices
set.seed(123)
monte_carlo_mat <- matrix(nrow = N, ncol = M)
for (j in 1:M) {
  monte_carlo_mat[[1, j]] <- price_init
  for(i in 2:N) {
    monte_carlo_mat[[i, j]] <- monte_carlo_mat[[i - 1, j]] * exp(rnorm(1, mu, sigma))
  }
}
# Format and organize data frame
price_sim <- cbind(day, monte_carlo_mat) %>% as_tibble() 
nm <- str_c("Sim.", seq(1, M))
nm <- c("Day", nm)
names(price_sim) <- nm
price_sim <- price_sim %>%
  gather(key = "Simulation", value = "Stock.Price", -(Day))
# Visualize simulation
price_sim %>%
  ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) + 
  geom_line(alpha = 0.1) +
  ggtitle(str_c("ALGN: ", M, 
                " Monte Carlo Simulations for Prices Over ", N, 
                " Trading Days"))

end_stock_prices <- price_sim %>% filter(Day == max(Day))
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_end_stock_prices <- quantile(end_stock_prices$Stock.Price, probs = probs)
dist_end_stock_prices %>% round(2)   # put it in a table

# Inputs
N_hist          <- nrow(ALGN_xts) / 252
p_start_hist    <- ALGN_xts$adjusted[[1]]
p_end_hist      <- ALGN_xts$adjusted[[nrow(ALGN_xts)]]
N_sim           <- N / 252
p_start_sim     <- p_end_hist
p_end_sim       <- dist_end_stock_prices[[4]]
# Compound annual growth rate (CAGR) calculations
CAGR_historical <- (p_end_hist / p_start_hist) ^ (1 / N_hist) - 1   # 31.97%
CAGR_sim        <- (p_end_sim / p_start_sim) ^ (1 / N_sim) - 1    # 30.06%


