library(tidyverse)
library(tidyquant)
library(plotly)
train = read.csv("hackaton_train_2cols.csv")
test <- read.csv("hackaton_test_2cols.csv")
colnames(test)  =  c("Index", "Station", "Anomaly")
colnames(train) = c("Index", "Station", "Anomaly")
# visualizing time series
# a =test %>% 
#   ggplot(aes(x = Index, y = Station, col = factor(Anomaly))) + 
#   geom_line() + 
#   labs(title = "Time Series Data - Station 4.2",
#        y = 'Electric Consumption',
#        color = "Anomaly") + 
#   theme_tq()
# ggplotly(a)

# Rolling stats
window_sizes <- c(2,3,4,5,25,35,50,100,200,400,800)
max_window = max(window_sizes)
test = rbind(tail(train,max_window -1),test)
train = train %>% mutate(Lag1 = lag(Station,1),
                         Lag2 = lag(Station,2),
                         Lag5 = lag(Station,5),
                         Lag10 = lag(Station,10),
                         Lag30 = lag(Station,30),
                         Lag50 = lag(Station,50),
                         diff1 = Station - Lag1,
                         diff2 = Station - Lag2,
                         diff5 = Station - Lag5,
                         diff30 = Station - Lag30,
                         diff50 = Station - Lag50)
test = test %>% mutate(Lag1 = lag(Station,1),
                       Lag2 = lag(Station,2),
                       Lag5 = lag(Station,5),
                       Lag10 = lag(Station,10),
                       Lag30 = lag(Station,30),
                       Lag50 = lag(Station,50),
                       diff1 = Station - Lag1,
                       diff2 = Station - Lag2,
                       diff5 = Station - Lag5,
                       diff30 = Station - Lag30,
                       diff50 = Station - Lag50)

for (i in window_sizes){
  train[[paste0("RollingMean_", i)]] = rollmean(train$Station, k = i, align = 'right', fill = NA)
  train[[paste0('RollingSD_', i)]] = rollapply(train$Station, width = i, FUN = sd, align = 'right', fill = NA)
  
}
for (i in window_sizes){
  test[[paste0("RollingMean_", i)]] = rollmean(test$Station, k = i, align = 'right', fill = NA)
  test[[paste0('RollingSD_', i)]] = rollapply(test$Station, width = i, FUN = sd, align = 'right', fill = NA)
}
names(train)
train = train %>% 
  mutate(meanaddsd = RollingMean_50 + RollingSD_50,
         meandiffsd = RollingMean_50 - RollingSD_50)
test = test %>% 
  mutate(meanaddsd = RollingMean_50 + RollingSD_50,
         meandiffsd = RollingMean_50 - RollingSD_50)

# train %>% 
#   ggplot() + 
#   geom_line(aes(Index,Station,color = 'Original')) + 
#   geom_line(aes(Index,RollingMean_5, color = 'Rolling Mean-5')) + 
#   geom_line(aes(Index,RollingMean_10, color = 'Rolling Mean-10')) + 
#   geom_line(aes(Index,RollingMean_25, color = 'Rolling Mean-25')) + 
#   geom_line(aes(Index,RollingMean_50, color = 'Rolling Mean-50')) + 
#   # geom_line(aes(Index,RollingMean_100, color = 'Rolling Mean-100')) + 
#   # geom_line(aes(Index,RollingMean_200, color = 'Rolling Mean-200')) + 
#   # geom_line(aes(Index,RollingMean_400, color = 'Rolling Mean-400')) + 
#   labs(title = "Time Series Data - Station4.2 with Rolling Mean",
#        x = "Index",
#        y = "Value",
#        color = "Legend") +
#   theme_tq()

# train %>% 
#   ggplot() + 
#   geom_line(aes(Index,Station,color = 'Original')) + 
#   geom_line(aes(Index,RollingSD_5, color = 'Rolling Mean-5')) + 
#   geom_line(aes(Index,RollingSD_10, color = 'Rolling Mean-10')) + 
#   geom_line(aes(Index,RollingSD_25, color = 'Rolling Mean-25')) + 
#   geom_line(aes(Index,RollingSD_50, color = 'Rolling Mean-50')) + 
#   # geom_line(aes(Index,RollingSD_100, color = 'Rolling Mean-100')) + 
#   # geom_line(aes(Index,RollingSD_200, color = 'Rolling Mean-200')) + 
#   # geom_line(aes(Index,RollingSD_400, color = 'Rolling Mean-400')) + 
#   labs(title = "Time Series Data - Station4.2 with Rolling SD",
#        x = "Index",
#        y = "Value",
#        color = "Legend") +
#   theme_tq()
write_csv(train,'train_v1.csv')
write_csv(test,'test_v1.csv')

