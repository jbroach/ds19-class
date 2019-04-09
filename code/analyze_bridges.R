# simplified for 2019

library(tidyverse)
library(lubridate)
library(ggplot2)
library(modelr)
library(purrr)
library(broom)

# load bike counts & weather data (ignore the wx warnings)
source("code/load_data2.R") # note the two new variables "bikecounts" & "weather"

# join wx & bridge counts
bikecount_wx <- bikecounts %>% 
  left_join(weather, by = c("date" = "DATE")) %>% # note single equals here
  select(date, bridge, total, TMIN, TMAX, PRCP)
  
ggplot(bikecount_wx, aes(x = date, y = total, group = bridge,
                         color = bridge)) +
    geom_line() + facet_wrap(bikecount_wx$bridge)

# average daily bike counts by bridge
bikecounts %>% 
  group_by(bridge) %>% 
  summarize(avg_daily_counts=mean(total)) %>%
  View()

# average monthly bike counts by bridge by year
bikecounts %>% 
  group_by(bridge, month(date), year(date)) %>%
  summarize(avg_monthly_counts=mean(total)) %>% 
  View()

# average monthly all time
avg_monthly <- bikecounts %>%
  group_by(bridge, month(date, label = T)) %>%
  summarize(avg_monthly_counts=mean(total))

ggplot(avg_monthly, aes(x = `month(date, label = T)`, 
                        y = avg_monthly_counts,
                         color = bridge)) +
  geom_point()

# models are objects in R
lm(total ~ TMIN + TMAX + PRCP + bridge, data = bikecount_wx) %>% 
  summary()

# can modify variables without storing
lm(total ~ I(40 - TMIN) + I(TMAX - 68) + I((TMAX - 68)^2) + 
     PRCP + bridge, data = bikecount_wx) %>% summary()
m1_poly <- lm(total ~ I(40 - TMIN) + I(TMAX - 68) + I((TMAX - 68)^2) + 
                PRCP + bridge, data = bikecount_wx)
coef(m1_poly)[]
p <- ggplot(data = data.frame(x = 0), 
            mapping = aes(x = x))
f <- function(x) {6.352e+01 *x + -9.857e-1*x^2}
p + stat_function(fun = f, color="lightblue", size=2) + xlim(-32,32) 

bikecount_wx <- bikecount_wx %>% add_predictions(m1_poly) %>%
  mutate(pred = pred)

ggplot() + 
  geom_line(data = bikecount_wx, aes(x = date, y = total), color = "red") +
  geom_line(data = bikecount_wx, aes(x = date, y = pred), color = "blue") +
  facet_wrap(bikecount_wx$bridge)

plot

lm(total ~ I(TMAX - 60) + I(40 - TMIN) +
     PRCP + bridge, data = bikecount_wx) %>% summary()

lm(total ~ PRCP + I(TMAX - 68) + factor(month(date)), data = bikecount_wx) %>% summary()

bw_nested <- bikecount_wx %>% 
  group_by(bridge) %>% 
  nest()

(fit <- lm(total ~ TMIN + TMAX + PRCP, data = bw_nested[[1, "data"]]))

mod1_func <- function(df) {
  lm(total ~ TMIN + TMAX + PRCP, data = df)
}
mod1_func(bw_nested[[1, "data"]])


fits <- purrr::map(bw_nested$data[1:2], mod1_func)
fits

(bw_nested <- bw_nested %>% 
    mutate(fit = purrr::map(data, mod1_func),
           tidy = purrr::map(fit, tidy),
           glance = purrr::map(fit, glance)))


broom::tidy(bw_nested$fit[[1]])

(bw_nested %>% 
    select(bridge, tidy) %>% 
    unnest(tidy) %>%
    arrange(term, estimate, bridge))

(bw_nested %>% 
  unnest(glance) %>% 
  arrange(desc(r.squared)))
