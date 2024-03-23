# Preliminaries

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, fixest, knitr, here, wesanderson, AER, modelsummary)

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
  geom_bar(stat = "identity", fill = "#12aeb6") +
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

fixed.avgs <- gather(avgs, key = "taxprice", value = "value", -Year)

# Create Line Plot

graph.1970.2018 <- ggplot(fixed.avgs, aes(x = Year, y = value, color = taxprice )) +
  geom_line() +
  labs(x = "Year", 
       y = "Avg. Tax and Price",
       title = "Avg. Tax & Price of Cigarettes from 1970 to 2018",
       color = "Tax & Price") +
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
                price_cpi_2012=cost_per_pack*(cpi_2012/index),
                log_price=log(price_cpi_2012),
                total_tax_cpi=tax_dollar*(cpi_2012/index),
                log_total_tax=log(total_tax_cpi))

# First ATE

ates<- cig.data %>%
    filter(Year <= 1990)

ate1 <- (feols(log_sales ~ log_price, data = ates))
summary(ate1)

table.one <- ate1$coefficients
print(table.one)

# Second ATE

ate2 <- (feols(log_sales ~ 1 | log_price ~ log_total_tax, 
             data=ates))
summary(ate2)

table.two <- ate2$coefficients
print(table.two)

# Third ATE (First Stage then Reduced Form)

first.stage <- (feols(log_price ~ log_total_tax, data = ates))
summary(first.stage)

table.three <- first.stage$coefficients
print(table.three)

price.hat <- predict(first.stage)

second.stage <- (feols(log_sales ~ log_total_tax, data = ates))
summary(second.stage)

table.four <- second.stage$coefficients
print(table.four)

# Repeat of Above under Different Timeframe

new.ates <- cig.data %>% ungroup() %>%
    filter(Year >= 1991 & Year <= 2015)

# First ATE

second.ate1 <- (feols(log_sales ~ log_price, data = new.ates))
summary(second.ate1)

table.five <- second.ate1$coefficients
print(table.five)

# Second ATE

second.ate2 <- (feols(log_sales ~ 1 | log_price ~ log_total_tax, 
             data= new.ates))
summary(second.ate2)

table.six <- second.ate2$coefficients
print(table.six)


# Third ATE (First Stage then Reduced Form)

first.stage2 <- (feols(log_price ~ log_total_tax, data = new.ates))
summary(first.stage2)

table.seven <- first.stage2$coefficients
print(table.seven)

price.hat2 <- predict(first.stage2)

second.stage2 <- (feols(log_sales ~ log_total_tax, data = new.ates))
summary(second.stage2)

table.eight <- second.stage2$coefficients
print(table.eight)

save.image("submission3/hwk3_workspace.Rdata")


