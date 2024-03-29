# Preliminaries

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest, knitr, here, AER, modelsummary)

# Load Data

full.data <- read_rds("data/output/TaxBurden_Data.rds")

# Create 1970-1985 Specific Data

data.1985 <- full.data %>% 
  filter(Year >= 1970 & Year <= 1985)

data.1985 <- data.1985 %>%
  group_by(state) %>%
  arrange(state, Year) %>%
  mutate(tax_change = ifelse(tax_state - lag(tax_state) > 0, 1, 0))

# Calculate Proportions

proportions <- data.1985 %>%
  group_by(Year) %>%
  summarise(Proportion = mean(tax_change, na.rm = TRUE), .groups = 'drop')

# Create Graph

graph.1970.1985 <- ggplot(proportions, aes(x = as.factor(Year), y = Proportion)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", 
       y = "Proportion",
       title = "States from 1970 to 1985 with Change in Tax") +
  theme_classic()

print(graph.1970.1985)

# Change Filter to 1970 to 2018

data.2018 <- full.data %>% 
  filter(Year >= 1970 & Year <= 2018)

# Calculate AVGs

avgs <- data.2018 %>%
  group_by(Year) %>%
  summarise(Avg_Tax = mean(tax_dollar, na.rm = TRUE),
            Avg_Price = mean(cost_per_pack, na.rm = TRUE))


# Create Line Plot

graph.1970.2018 <- ggplot(avgs, aes(x = as.factor(Year)) +
  geom_line(aes(y = Avg_Tax), color = "darkred", label = "Average Tax") + 
  geom_line(aes(y = Avg_Price), color="steelblue", label = "Average Price") +
  labs(x = "Year", 
       y = "Values",
       title = "Avg. Tax & Price of Cigarettes from 1970 to 2018")) +
  theme_classic()

# If not using color = for legend need to figure out how to add labels

print(graph.1970.2018)

# Calculate Price Increase

price.increase <- full.data %>%
  group_by(state) %>%
  summarise(increase = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE))

# Identify States

top.states <- price.increase %>%
  top_n(5, increase) %>%
  pull(state)

# Filter Year

data.top <- full.data %>%
  filter(state %in% top.states & Year >= 1970 & Year <= 2018)

# Calculate AVGs

state.avgs <- data.top %>%
  group_by(state, Year) %>%
  summarise(Avg_Packs = mean(sales_per_capita, na.rm = TRUE))

# Plot Graph

graph.top <- ggplot(state.avgs, aes(x = Year, y = Avg_Packs, color = state)) +
  geom_line() +
  labs(x = "Year", 
       y = "Packs Per Capita",
       title = "Avg. Number of Packs Sold per Capita from 1970 to 2018", 
       color = "State") +
  theme_classic()

print(graph.top)

# Do Same to Find Lowest States

price.increase <- full.data %>%
  group_by(state) %>%
  summarise(increase = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE))

bottom.states <- price.increase %>%
  top_n(-5, increase) %>%
  pull(state)

# Filter Years

data.bottom <- full.data %>%
  filter(state %in% bottom.states & Year >= 1970 & Year <= 2018)

new.state.avgs <- data.bottom %>%
  group_by(state, Year) %>%
  summarise(Avg_Packs = mean(sales_per_capita, na.rm = TRUE))

# Plot Graph

graph.bottom <- ggplot(new.state.avgs, aes(x = Year, y = Avg_Packs, color = state)) +
  geom_line() +
  labs(x = "Year", 
       y = "Packs per Capita", 
       title = "Avg. Number of Packs Sold per Capita from 1970 to 2018",
       color = "State") +
  theme_classic()

print(graph.bottom)

# Estimate ATEs

cig.data <- read_rds(here("data/output/TaxBurden_Data.rds"))

cig.data <- cig.data %>% 
    mutate(log_sales=log(sales_per_capita),
                log_price_cpi=log(price_cpi),
                log_price=log(cost_per_pack),
                tax_cpi=tax_state*(218/index),                            
                total_tax_cpi=tax_dollar*(218/index),                    
                log_total_tax=log(total_tax_cpi),                             
                log_state_tax=log(tax_cpi))

# First ATE

ates<- cig.data %>%
    filter(Year <= 1990)

ate1 <- (feols(log_sales ~ log_price, data = ates))
summary(ate1)

# Second ATE

ate2 <- (feols(log_sales ~ 1 | log_price ~ log_total_tax, 
             data=ates))
summary(ate2)

# Third ATE (First Stage then Reduced Form)

first.stage <- (feols(log_price ~ log_total_tax, data = ates))
summary(first.stage)

price.hat <- predict(first.stage)

second.stage <- (feols(log_sales ~ price.hat, data = ates))
summary(second.stage)

# Repeat of Above under Different Timeframe

new.ates <- cig.data %>% ungroup() %>%
    filter(Year >= 1991 & Year <= 2015)

# First ATE

second.ate1 <- (feols(log_sales ~ log_price, data = new.ates))
summary(second.ate1)

# Second ATE

second.ate2 <- (feols(log_sales ~ 1 | log_price ~ log_total_tax, 
             data= new.ates))
summary(second.ate2)

# Third ATE (First Stage then Reduced Form)

first.stage2 <- (feols(log_price ~ log_total_tax, data = new.ates))
summary(first.stage2)

price.hat2 <- predict(first.stage2)

second.stage2 <- (feols(log_sales ~ price.hat2, data = new.ates))
summary(second.stage2)

save.image("submission1/hwk3_workspace.Rdata")
