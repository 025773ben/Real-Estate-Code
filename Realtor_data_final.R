## Libraries-------------------------------------------------------------------

library(ggplot2)
library(knitr)
library(clipr)
library(scales)
library(maps)
library(plotly)
library(sf)
library(gridExtra)
library(patchwork)
library(ggthemes)
library(tidyverse)
library(cowplot)
library(kableExtra)
library(RColorBrewer)
library(rsample)
library(MASS)
library(caret)
library(doParallel)
library(randomForest)
library(tidymodels)
library(mice)
library(xgboost)
library(yardstick)
library(parsnip)
library(kernlab)
library(ranger)
library(modeltime)
library(vip)
library(dplyr)

final_df <- read.csv("C:\\Users\\Ben_1\\Downloads\\realtor_final_dataset.csv")

#variables for analysis
final_df2 <- select(final_df, local.flood.flood_factor_score, local.wildfire.fire_factor_score, photos.0.href,                   
                          photos.2.href, photos.3.href, photos.4.href, photos.5.href, photos.6.href, photos.8.href, photos.11.href,
                          photos.12.href, photos.13.href, photos.14.href, photos.15.href, photos.16.href, 
                          photos.19.href, photos.20.href, photos.21.href, photos.22.href, photos.25.href, 
                          photos.26.href, photos.27.href, photos.28.href, photos.29.href, photos.31.href, 
                          photos.32.href, photos.34.href, photos.35.href, photos.37.href, photos.38.href, 
                          photos.39.href, photos.40.href, photos.41.href, photos.42.href, photos.43.href, 
                          photos.44.href, photos.45.href, photos.46.href, photos.48.href, photos.49.href, 
                          photos.50.href, photos.51.href, photos.52.href, photos.53.href, photos.54.href, 
                          photos.56.href, photos.57.href, photos.58.href, text, lot_sqft, address.locality, address.postalCode,
                          address.region, address.street, sqft, beds, stories, type, listPrice, baths, baths_full, lastSoldPrice, garage,
                          soldOn, coordinates.latitude, coordinates.longitude, year_built, id)

## Cleaning the data & adding new columns--------------------------------------

#Eliminate missing response variable and nonsense cases
final_df3 <- final_df2[complete.cases(final_df2$lastSoldPrice, final_df2$sqft) & final_df2$sqft != 0, ]

# Remove 1% outliers
final_df4 <- final_df3 %>% filter(sqft >= 699.2 & sqft <= 5545.2
                       & lastSoldPrice <= 2700000 & lastSoldPrice >= 50000
                       & beds <= 6 & beds >= 1
                       & baths > 0 & baths < 7)

final_df4 <- final_df4 %>% 
  mutate(listPrice=ifelse(listPrice==1, NA, listPrice)) %>% 
  mutate(garage = ifelse(garage == 66, 2, garage)) %>% 
  mutate(garage = ifelse(garage == 48, NA, garage)) %>% 
  mutate(garage = ifelse(garage == 52, 1, garage)) %>% 
  mutate(garage = ifelse(garage == 39, 1, garage)) %>% 
  mutate(garage = ifelse(garage == 22, 2, garage)) %>% 
  mutate(beds = ifelse(beds == 44, 4, beds)) %>% 
  mutate(beds = ifelse(beds == 15, 9, beds)) %>% 
  mutate(garage = ifelse(garage == 15, NA, garage)) %>% 
  mutate(lot_sqft = ifelse(lot_sqft == 999999999, NA, lot_sqft)) %>% 
  mutate(lot_sqft = ifelse(lot_sqft == 404149680, 9300, lot_sqft)) %>% 
  mutate(lot_sqft = ifelse(lot_sqft == 162609480, 42687, lot_sqft)) %>% 
  mutate(lot_sqft = ifelse(lot_sqft == 76447800, 4356, lot_sqft)) %>% 
  mutate(lot_sqft = ifelse(lot_sqft == 1, 9147, lot_sqft)) %>% 
  mutate(lot_sqft = ifelse(lot_sqft == 17, NA, lot_sqft)) %>% 
  mutate(lot_sqft = ifelse(lot_sqft < 1498, NA, lot_sqft)) %>% 
  mutate(lot_sqft = ifelse(lot_sqft > 121968, NA, lot_sqft))

# Add US region
final_df4 <- final_df4 %>%
  mutate(country.region = case_when(
    address.region %in% c("WA", "OR", "CA", "AZ", "HI") ~ "West",
    address.region %in% c("ME", "NH", "VT", "NY", "MA", "CT", "RI", "NJ", "PA", "DC") ~ "Northeast",
    address.region %in% c("OH", "MI", "IN", "IL", "WI", "IA", "NE") ~ "Midwest",
    address.region %in% c("FL", "GA", "AL", "SC", "NC", "VA", "WV", "TN", "KY", "AR", "DE", "MD", "OK") ~ "South",
    TRUE ~ "Unknown"
  ))

median_price_per_state <- aggregate(lastSoldPrice ~ address.region, data = final_df4, FUN = median)

# Create a function to map state abbreviations to their respective capitals, b/c
# we want to display city, state and some states have multiple cities
get_state_capital <- function(state) {
  state <- toupper(state) 
  capital <- switch(state,
                    WA = "Olympia",
                    OR = "Salem",
                    CA = "Sacramento",
                    AZ = "Phoenix",
                    HI = "Honolulu",
                    ME = "Augusta",
                    NH = "Concord",
                    VT = "Montpelier",
                    NY = "Albany",
                    MA = "Boston",
                    CT = "Hartford",
                    RI = "Providence",
                    NJ = "Trenton",
                    PA = "Harrisburg",
                    DE = "Dover",
                    DC = "Washington, D.C.",
                    MD = "Annapolis",
                    OH = "Columbus",
                    MI = "Lansing",
                    IN = "Indianapolis",
                    IL = "Springfield",
                    WI = "Madison",
                    IA = "Des Moines",
                    NE = "Lincoln",
                    FL = "Tallahassee",
                    GA = "Atlanta",
                    AL = "Montgomery",
                    SC = "Columbia",
                    NC = "Raleigh",
                    VA = "Richmond",
                    WV = "Charleston",
                    TN = "Nashville",
                    KY = "Frankfort",
                    AR = "Little Rock",
                    OK = "Oklahoma City",
                    "Unknown")
  return(capital)
}

# Add the 'state.capital'
final_df4$state.capital <- sapply(final_df4$address.region, get_state_capital)

final_df4 <- final_df4 %>% 
  unite(col = "city_and_state", state.capital, address.region, 
                                 sep = ", ", remove = F) %>% 
  mutate(city_and_state = ifelse(city_and_state == "Washington, D.C., DC", "Washington, DC",
                                 city_and_state))
  
# Add Age
final_df4 <- final_df4 %>% 
  mutate(age = 2023 - year_built)

# Add Property Tax Rate
final_df4 <- final_df4 %>%
  mutate(property_tax_rate = case_when(
    state.capital == "Columbus" ~ 2.06,
    state.capital == "Raleigh" ~ 0.96,
    state.capital == "Phoenix" ~ 0.64,
    state.capital == "Oklahoma City" ~ 1.2,
    state.capital == "Richmond" ~ 1.12,
    state.capital == "Atlanta" ~ 1.09,
    state.capital == "Sacramento" ~ 0.93,
    state.capital == "Nashville" ~ 0.88,
    state.capital == "Des Moines" ~ 2.1,
    state.capital == "Columbia" ~ 0.87,
    state.capital == "Lincoln" ~ 1.99,
    state.capital == "Madison" ~ 2.11,
    state.capital == "Olympia" ~ 1.14,
    state.capital == "Harrisburg" ~ 1.58,
    state.capital == "Indianapolis" ~ 1.19,
    state.capital == "Washington, D.C." ~ 0.6,
    state.capital == "Salem" ~ 1.15,
    state.capital == "Tallahassee" ~ 0.93,
    state.capital == "Trenton" ~ 2.78,
    state.capital == "Albany" ~ 2.17,
    state.capital == "Boston" ~ 0.78,
    state.capital == "Annapolis" ~ 0.97,
    state.capital == "Dover" ~ 0.56,
    state.capital == "Montgomery" ~ 0.43,
    state.capital == "Providence" ~ 1.72,
    state.capital == "Frankfort" ~ 0.94,
    state.capital == "Lansing" ~ 2.26,
    state.capital == "Little Rock" ~ 0.86,
    state.capital == "Honolulu" ~ 0.32,
    state.capital == "Hartford" ~ 2.4,
    state.capital == "Concord" ~ 2.52,
    state.capital == "Augusta" ~ 1.4,
    state.capital == "Montpelier" ~ 2.05,
    state.capital == "Charleston" ~ 0.77,
    state.capital == "Springfield" ~ 2.18,
    TRUE ~ NA_real_
  ))

# Add Mortgage Interest Rate
final_df4$soldOn <- as.Date(final_df4$soldOn, format = "%m/%d/%Y")
final_df4$mortgage_interest_rate <- NA
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2022-12-16") & final_df4$soldOn <= as.Date("2022-12-22")] <- 6.27
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2022-12-23") & final_df4$soldOn <= as.Date("2022-12-29")] <- 6.42
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2022-12-30") & final_df4$soldOn <= as.Date("2023-01-05")] <- 6.48
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-01-06") & final_df4$soldOn <= as.Date("2023-01-12")] <- 6.33
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-01-13") & final_df4$soldOn <= as.Date("2023-01-19")] <- 6.15
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-01-20") & final_df4$soldOn <= as.Date("2023-01-26")] <- 6.13
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-01-27") & final_df4$soldOn <= as.Date("2023-02-02")] <- 6.09
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-02-03") & final_df4$soldOn <= as.Date("2023-02-09")] <- 6.12
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-02-10") & final_df4$soldOn <= as.Date("2023-02-16")] <- 6.32
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-02-17") & final_df4$soldOn <= as.Date("2023-02-23")] <- 6.5
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-02-24") & final_df4$soldOn <= as.Date("2023-03-02")] <- 6.65
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-03-03") & final_df4$soldOn <= as.Date("2023-03-09")] <- 6.73
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-03-10") & final_df4$soldOn <= as.Date("2023-03-16")] <- 6.6
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-03-17") & final_df4$soldOn <= as.Date("2023-03-23")] <- 6.42
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-03-24") & final_df4$soldOn <= as.Date("2023-03-30")] <- 6.32
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-03-31") & final_df4$soldOn <= as.Date("2023-04-06")] <- 6.28
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-04-07") & final_df4$soldOn <= as.Date("2023-04-13")] <- 6.27
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-04-14") & final_df4$soldOn <= as.Date("2023-04-20")] <- 6.39
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-04-21") & final_df4$soldOn <= as.Date("2023-04-27")] <- 6.43
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-04-28") & final_df4$soldOn <= as.Date("2023-05-04")] <- 6.39
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-05-05") & final_df4$soldOn <= as.Date("2023-05-11")] <- 6.35
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-05-12") & final_df4$soldOn <= as.Date("2023-05-18")] <- 6.39
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-05-19") & final_df4$soldOn <= as.Date("2023-05-25")] <- 6.57
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-05-26") & final_df4$soldOn <= as.Date("2023-06-01")] <- 6.79
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-06-02") & final_df4$soldOn <= as.Date("2023-06-08")] <- 6.71
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-06-09") & final_df4$soldOn <= as.Date("2023-06-15")] <- 6.69
final_df4$mortgage_interest_rate[final_df4$soldOn >= as.Date("2023-06-16") & final_df4$soldOn <= as.Date("2023-06-22")] <- 6.67

# Add log last sold price for linear regression model
final_df4 = final_df4 %>% 
  mutate(lastSoldPricelog = log10(lastSoldPrice))

# Save the data frame as a CSV file
write.csv(final_df4, "C:\\Users\\Ben_1\\REU_2023\\final_data4.csv", row.names = FALSE)

#summary stats-----------------------------------------------------------------
summary(final_df4)
price_sold <- summary(final_df4$lastSoldPrice)
price_sold <-data.frame(x = c(50000, 360000, 2700000),
                        label = c(paste0("Min: 50k"),
                                  paste0("Median:360k"),
                                  paste0("Max: 2.7mil")))

quantile(final_df4$garage, prob = 0.99, na.rm = T)
quantile(final_df4$year_built, prob = 0.01, na.rm = T)

View(final_df4 %>% select(mortgage_interest_rate, soldOn,
                          address.locality, address.postalCode, city_and_state, 
                          address.street))

View(final_df4 %>% select(summary(lastSoldPrice)))

## Data Visualization---------------------------------------------------------

# Number of homes per state capital
final_df4 %>%
  count(city_and_state) %>%
  ggplot(aes(x = reorder(city_and_state, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightblue", color ="black") +
  labs(title = "Number of Single-Family Homes per State Capital", x = NULL, y = "Count") +
  coord_flip()

# Number of homes per region
final_df4 %>% 
  count(country.region) %>% 
  ggplot(aes(x = reorder(country.region, -n), y = n, fill = country.region)) +
  geom_bar(stat = "identity", show.legend = F) +
  geom_text(aes(label = comma(n)), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Number of single-family homes per region", x = NULL, y = "Count")

##Fire and Flood Data-----------------------------------------------------------
state_avg_scores <- final_df4 %>%
  group_by(city_and_state) %>%
  summarise(avg_fire_score = mean(local.wildfire.fire_factor_score, na.rm = TRUE))
sorted_state_avg_scores <- state_avg_scores %>%
  arrange(desc(avg_fire_score))

bar_plot_fire <- ggplot(sorted_state_avg_scores, aes(x = reorder(city_and_state, -avg_fire_score), y = avg_fire_score)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = NULL, y = "Fire Risk Score", title = "Average Fire Risk Score by State Capital") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(3.2))) +
  theme(axis.text.x = element_text(angle = 45, size = 10, vjust = 1, hjust = 1))
bar_plot_fire

state_avg_scores <- final_df4 %>%
  group_by(city_and_state) %>%
  summarise(avg_flood_score = mean(local.flood.flood_factor_score, na.rm = TRUE)) %>%
  arrange(desc(avg_flood_score))

bar_plot_flood <- ggplot(state_avg_scores, aes(x = reorder(city_and_state, -avg_flood_score), y = avg_flood_score)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(x = NULL, y = "Flood Risk Score") +
  ggtitle("Average Flood Risk Score by State") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(4.3))) +
  theme(axis.text.x = element_text(angle = 45, size = 7, vjust = 1, hjust = 1))
bar_plot_flood

grid.arrange(bar_plot_flood, bar_plot_fire, ncol = 2)

## Scatter plot to compare all variables vs sold price---------------------------
scatter_plots <- list(
  # ggplot(final_df4, aes(x = local.flood.flood_factor_score, y = lastSoldPrice)) +
  #   geom_point() +
  #   labs(x = "flood risk score", y = "Price Sold (log10 scale)",
  #        title = "Scatter plots comparing variables to Price Sold") +
  #   scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  #   scale_x_continuous(breaks = 1:10),
  
  # ggplot(final_df4, aes(x = local.wildfire.fire_factor_score, y = lastSoldPrice)) +
  #   geom_point(alpha = .25) +
  #   labs(x = "fire risk score", y = "Price Sold (log form)") +
  #   scale_y_log10(labels = comma) +
  #   scale_x_continuous(breaks = 1:10),
  
  ggplot(final_df4, aes(x = lot_sqft, y = lastSoldPrice)) +
    geom_point(alpha = .25) +
    labs(x = "lot sqft (log10 scale)", y = "price sold (log10 scale)",
         title = "Price Sold vs Lot Sqft (log10 scale)") +
    scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
    scale_x_log10(labels = comma),
  
  ggplot(final_df4, aes(x = sqft, y = lastSoldPrice)) +
    geom_point(alpha = .25) +
    geom_smooth(method = "lm") +
    labs(x = "sqft (log10 scale)", y = "price sold (log10 scale)",
         title = "Price Sold vs Sqft (log10 scale)") +
    scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
    scale_x_log10(labels = comma),
  
  # ggplot(final_df4, aes(x = beds, y = lastSoldPrice)) +
  #   geom_point() +
  #   labs(x = "beds", y = "Price Sold") +
  #   scale_y_log10(labels = comma),
  
  # ggplot(final_df4, aes(x = stories, y = lastSoldPrice)) +
  #   geom_point() +
  #   labs(x = "stories", y = "Price Sold") +
  #   scale_y_log10(labels = comma),
  
  ggplot(final_df4, aes(x = listPrice, y = lastSoldPrice)) +
    geom_point(alpha = .5, aes(color = country.region, shape = country.region)) +
    labs(x = "list price (log10 scale)", y = "price sold (log10 scale)",
         title = "Price Sold vs List Price (log10 scale)") +
    geom_smooth(method = "lm", color = "black") +
    scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
    scale_x_log10(labels = scales::dollar_format(prefix = "$")),
  
  # ggplot(final_df4, aes(x = baths, y = lastSoldPrice)) +
  #   geom_point() +
  #   labs(x = "baths", y = "Price Sold (log10 scale)") +
  #   scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  #   scale_x_continuous(breaks = 0:10),
  
  # ggplot(final_df4, aes(x = garage, y = lastSoldPrice)) +
  #   geom_point() +
  #   labs(x = "garage", y = "Price Sold (log10 scale)") +
  #   scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  #   scale_x_continuous(breaks = 0:15),
  
  ggplot(final_df4, aes(x = age, y = lastSoldPrice)) +
    geom_point(alpha = .3) +
    #geom_smooth(method = "lm") +
    labs(x = "year built", y = "price sold (log10 scale)",
         title = "Price Sold vs Year Built") +
    scale_y_log10(labels = scales::dollar_format(prefix = "$"))
)
combined_plot <- wrap_plots(scatter_plots, ncol = 2)
combined_plot

title <- ggdraw() +
  draw_label("Combined Scatter Plots", size = 14)
final_plot <- plot_grid(title, combined_plot, nrow = 2, rel_heights = c(0.1, 0.9))
final_plot


final_df4 %>% 
ggplot(aes(x = noun_freq, y = lastSoldPrice)) +
  geom_point(alpha = .3) +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_x_sqrt()

final_df4 %>% 
  dplyr::select(noun_freq.x, adj_freq.x, text, total_words.x, total_words.y) %>% 
  view()

nondesc = final_df4 %>% 
  filter(total_words <= 45) %>% 
  dplyr::select(text) %>% 
  unique()

nondesc_list <- read_csv("nondesc_list.csv")

final_df4 <- final_df4 %>% 
  mutate(text=ifelse(text %in% nondesc_list$text, NA, text))

final_df2$beds %>% 
  summary()
# Create histograms for each variable------------------------------------------
histograms <- list(
  ggplot(final_df4, aes(x = lastSoldPrice)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    labs(title = "Histogram: Price Sold (log10 scale)",
         x = "Price Sold USD (log10 scale)", y = "Frequency") +
    scale_x_log10(labels = scales::dollar_format(prefix = "$")) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 10, vjust = 0.5, hjust = .5),
          plot.title = element_text(hjust = 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 4000)),
  
  # ggplot(final_df4, aes(x = listPrice)) +
  #   geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  #   labs(x = "list price (log10 scale)", y = "Frequency", title = "Histogram: List Price") +
  #   scale_x_log10(labels = scales::dollar_format(prefix = "$")) +
  #   theme(panel.grid.major.x = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         axis.text.x = element_text(size = 10, vjust = 0.5, hjust = .5),
  #         plot.title = element_text(hjust = 0)) +
  #   scale_y_continuous(expand = c(0, 0), limits = c(0, 4000)),
  
  # ggplot(final_df4, aes(x = local.flood.flood_factor_score)) +
  #   geom_histogram(bins = 10, fill = "dodgerblue", color = "black") +
  #   labs(x = "flood factor score", y = "Frequency") +
  #   ggtitle("Histogram: flood factor score") +
  #   scale_x_continuous(breaks = 1:10),
  # 
  # ggplot(final_df4, aes(x = local.wildfire.fire_factor_score)) +
  #   geom_histogram(bins = 10, fill = "red", color = "black") +
  #   labs(x = "fire factor score", y = "Frequency") +
  #   ggtitle("Histogram: fire factor score") +
  #   scale_x_continuous(breaks = 1:10),
  
  ggplot(final_df2, aes(x = lot_sqft)) +
    geom_histogram(bins = 50, fill = "lightblue", color = "black") +
    labs(x = "lot sqft (log10 scale)", y = "Frequency", title = "Histogram: Lot Sqft") +
    scale_x_log10(labels = comma) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 10, vjust = 0.5, hjust = .5),
          plot.title = element_text(hjust = 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 4000)),
  
  ggplot(final_df2, aes(x = sqft)) +
    geom_histogram(bins = 20, fill = "lightblue", color = "black") +
    labs(x = "sqft (log10 scale)", y = "Frequency", title = "Histogram: Sqft") +
    scale_x_log10(labels = comma) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 10, vjust = 0.5, hjust = .5),
          plot.title = element_text(hjust = 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 4000)),
  
  # ggplot(final_df4, aes(x = beds)) +
  #   geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  #   labs(x = "beds", y = "Frequency") +
  #   ggtitle("Histogram: Beds") +
  #   scale_x_continuous(breaks = 0:15) +
  #   theme(panel.grid.major.x = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         axis.text.x = element_text(size = 10, vjust = 0.5, hjust = .5, margin = margin(t = 5, r = 0)),
  #         plot.title = element_text(hjust = 0, margin = margin(b = 10))),
  # 
  # ggplot(final_df4, aes(x = stories)) +
  #   geom_histogram(bins = 4, fill = "lightblue", color = "black" ) +
  #   labs(x = "stories", y = "Frequency") +
  #   ggtitle("Histogram: stories") +
  #   scale_x_continuous(breaks = 1:4),

  # ggplot(final_df4, aes(x = baths)) +
  #   geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  #   labs(x = "baths", y = "Frequency") +
  #   ggtitle("Histogram: baths") +
  #   theme(panel.grid.major.x = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         axis.text.x = element_text(size = 10, vjust = 0.5, hjust = .5, margin = margin(t = 5, r = 0)),
  #         plot.title = element_text(hjust = 0, margin = margin(b = 10))),
  # 
  # ggplot(final_df4, aes(x = garage)) +
  #   geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  #   labs(x = "garage", y = "Frequency") +
  #   ggtitle("Histogram: garage") +
  #   scale_x_log10(labels = comma),

  ggplot(final_df2, aes(x = year_built)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    labs(x = "age", y = "Frequency", title = "Histogram: Age" ) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 10, vjust = 0.5, hjust = .5),
          plot.title = element_text(hjust = 0)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 150))
)
combined_plot2 <- wrap_plots(histograms, ncol = 2)
combined_plot2

#noun_freq histogram
final_df4 %>% 
  filter(noun_freq != 0) %>% 
  ggplot(aes(x = noun_freq)) +
  geom_histogram(fill = "lightblue", color = "black")

final_df4 %>% 
  filter(noun_freq != 0) %>% 
  ggplot(aes(noun_freq)) +
  geom_boxplot()

final_df4 %>%
  filter(noun_freq != 0 & noun_freq < .2) %>% 
  ggplot(aes(x = noun_freq, y = lastSoldPrice)) +
  geom_point(alpha = .3) +
  scale_y_log10() +
  scale_x_log10()

quantile(final_df4$noun_freq, prob = 0.99, na.rm = T)
sum(auto$noun_freq == 0)
29470-12783


# Create boxplots for each variable--------------------------------------------
boxplots <- list(
  ggplot(final_df4, aes(x = lastSoldPrice/10000, y = "")) +
    geom_boxplot(fill = "lightblue", color = "black") +
    geom_text(data = price_sold, aes(x = x/10000, label = label),
              y = -Inf, vjust = c(-19.5, -6.2, -19.5),
              color = "black", size = 3) +
    labs(x = "Price Sold (log10 scale in 10,000)", y = NULL) +
    ggtitle("Boxplot: Price Sold") +
    scale_x_log10(labels = scales::dollar_format(prefix = "$")),
  
  # ggplot(final_df4, aes(x = local.flood.flood_factor_score, y = "")) +
  #   geom_boxplot(fill = "lightblue", color = "black") +
  #   labs(x = "flood factor score", y = "") +
  #   ggtitle("Boxplot: flood factor score") +
  #   scale_x_continuous(breaks = 1:10),
  # 
  # ggplot(final_df4, aes(x = local.wildfire.fire_factor_score, y = "")) +
  #   geom_boxplot(fill = "lightblue", color = "black") +
  #   labs(x = "fire factor score", y = "") +
  #   ggtitle("Boxplot: fire factor score") +
  #   scale_x_continuous(breaks = 1:10),
  
  ggplot(final_df4, aes(x = lot_sqft, y = "")) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(x = "lot sqft (log10 scale)", y = NULL, title = "Boxplot: lot sqft") +
    scale_x_log10(labels = comma),
  
  ggplot(final_df4, aes(x = sqft, y = "")) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(x = "sqft (log10 scale)", y = NULL, title = "Boxplot: sqft") +
    scale_x_log10(labels = comma),
  
  ggplot(final_df2, aes(x = beds, y = "")) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(x = "beds", y = NULL, title = "Boxplot: beds"),
  
  ggplot(final_df4, aes(x = stories, y = "")) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(x = "stories", y = NULL, title = "Boxplot: stories"),
  
  ggplot(final_df4, aes(x = listPrice, y = "")) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(x = "list price (log10 scale)", y = NULL,
         title = "Boxplot: list price") +
    scale_x_log10(labels = scales::dollar_format(prefix = "$")),
  
  ggplot(final_df4, aes(x = baths, y = "")) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(x = "baths", y = NULL, title = "Boxplot: baths"),
  
  ggplot(final_df4, aes(x = garage, y = "")) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(x = "garage", y = NULL, title = "Boxplot: garage"),
  
  ggplot(final_df4, aes(x = year_built, y = "")) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(x = "year built", y = "", title = "Boxplot: year built") +
    coord_cartesian(xlim = c(1895, max(final_df4$year_built)))
)
combined_plot3 <- wrap_plots(boxplots, ncol = 3)
combined_plot3

## Multiple Box Plots----------------------------------------------------------

# Price Sold by region
ggplot(final_df4, aes(x = country.region, y = lastSoldPrice)) +
  geom_boxplot(aes(fill = country.region)) +
  geom_jitter(width = .1, alpha = .25) +
  geom_violin(aes(fill = country.region), show.legend = T) +
  labs(x = "", y = "Price Sold (log10 scale)", title = "Price Sold by Region",
       fill = "Region") +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_x_discrete(labels = NULL)

#Price Sold by Beds
plot_beds <- ggplot(final_df4 %>% mutate(beds = as.factor(beds)) %>% filter(!is.na(beds)),
       aes(x = beds, y = lastSoldPrice)) +
  geom_boxplot(aes(fill = beds), show.legend = F) +
  #geom_jitter(width = .1, alpha = .25) +
  #geom_violin(aes(fill = beds), show.legend = T) +
  labs(x = "Number of Bedrooms", y = "Price Sold (log10 scale)",
       title = "Price Sold by Number of Bedrooms") +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_x_discrete() +
  scale_fill_brewer(palette = "Dark2")

final_df4 %>% mutate(beds = as.factor(beds)) %>% filter(!is.na(beds)) %>% 
  count(beds) %>% mutate(percent = round(n/sum(n)*100, 2))

# Price Sold by Bathrooms
plot_baths <- ggplot(final_df4 %>% mutate(baths = as.factor(baths)) %>% filter(!is.na(baths)),
       aes(x = baths, y = lastSoldPrice)) +
  geom_boxplot(aes(fill = baths), show.legend = F) +
  #geom_jitter(width = .1, alpha = .25) +
  #geom_violin(aes(fill = beds), show.legend = T) +
  labs(x = "Number of Bathrooms", y = "Price Sold (log10 scale)",
       title = "Price Sold by Number of Bathrooms") +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_x_discrete() +
  scale_fill_brewer(palette = "Dark2")

final_df4 %>% mutate(baths = as.factor(baths)) %>% filter(!is.na(baths)) %>% 
  count(baths) %>% mutate(percent = round(n/sum(n)*100, 2))

combined_plot9 <- plot_beds + plot_baths + plot_layout(ncol = 2)
combined_plot9

# Price Sold by Garages
final_df4 %>% 
  filter(!is.na(garage) & garage > 0 & garage <= 6) %>% 
  mutate(garage = as.factor(garage)) %>%  
  ggplot(aes(x = garage, y = lastSoldPrice)) +
  geom_boxplot() +
  #geom_jitter(width = .1, alpha = .05) +
  #geom_violin() +
  labs(x = "Number of garage spaces", y = "Price Sold (log10 scale)",
       title = "Price Sold by Number of Garage Spaces") +
  scale_y_log10(labels = scales::dollar_format(prefix = "$"))

# Price Sold by Fire Score
plot_fire <- final_df4 %>% 
  mutate(local.wildfire.fire_factor_score = as.numeric(as.character(local.wildfire.fire_factor_score))) %>% 
  filter(!is.na(local.wildfire.fire_factor_score)) %>% 
  ggplot(aes(x = local.wildfire.fire_factor_score, y = lastSoldPrice, fill = local.wildfire.fire_factor_score)) +
  geom_boxplot(aes(group = local.wildfire.fire_factor_score, fill = local.wildfire.fire_factor_score)) +
  geom_jitter(width = .1, alpha = .25) +
  geom_violin(aes(group = local.wildfire.fire_factor_score, 
                  fill = local.wildfire.fire_factor_score), show.legend = F) +
  labs(x = "Fire Risk Score", y = "Price Sold (log10 scale)",
       title = "Price Sold by Fire Risk Score") +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_fill_gradient(low = "#FFCCCC", high = "red") +
  scale_x_continuous(breaks = 1:10) +
  guides(fill = "none")

# Price Sold by Flood Score
plot_flood <- final_df4 %>% 
  mutate(local.flood.flood_factor_score = as.numeric(as.character(local.flood.flood_factor_score))) %>% 
  filter(!is.na(local.flood.flood_factor_score)) %>% 
  ggplot(aes(x = local.flood.flood_factor_score, y = lastSoldPrice, fill = local.flood.flood_factor_score)) +
  geom_boxplot(aes(group = local.flood.flood_factor_score, fill = local.flood.flood_factor_score)) +
  geom_jitter(width = .1, alpha = .25) +
  # geom_violin(aes(group = local.flood.flood_factor_score, 
  #                 fill = local.flood.flood_factor_score), show.legend = F) +
  labs(x = "Flood Risk Score", y = "Price Sold (log10 scale)",
       title = "Price Sold by Flood Risk Score") +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  scale_fill_gradient(low = "powderblue", high = "blue") +
  scale_x_continuous(breaks = 1:10) +
  guides(fill = "none")

combined_plot8 <- plot_flood + plot_fire + plot_layout(ncol = 2)
combined_plot8

# Mortgage Interest Rate
final_df4 %>% 
  filter(!is.na(mortgage_interest_rate)) %>% 
  mutate(mortgage_interest_rate = as.factor(mortgage_interest_rate)) %>%  
  ggplot(aes(x = mortgage_interest_rate, y = lastSoldPrice)) +
  geom_boxplot() +
  #geom_jitter(width = .1, alpha = .05) +
  #geom_violin() +
  labs(x = "Mortgage Interest Rate (%)", y = "Price Sold (log10 scale)",
       title = "Price Sold by Mortgage Interest Rate") +
  scale_y_log10(labels = scales::dollar_format(prefix = "$"))


final_df4 %>% 
  filter(!is.na(noun_freq)) %>% 
  mutate(noun_freq = as.factor(noun_freq)) %>%  
  ggplot(aes(x = noun_freq, y = lastSoldPrice)) +
  geom_boxplot() +
  #geom_jitter(width = .1, alpha = .05) +
  #geom_violin() +
  labs(x = "Mortgage Interest Rate (%)", y = "Price Sold (log10 scale)",
       title = "Price Sold by Mortgage Interest Rate") +
  scale_y_log10(labels = scales::dollar_format(prefix = "$"))
## Property Tax Rate----------------------------------------------------------

final_df4 %>%
  distinct(property_tax_rate, city_and_state) %>%
  arrange(property_tax_rate) %>%
  ggplot() +
  geom_segment(aes(x = reorder(city_and_state, property_tax_rate), xend = reorder(city_and_state, property_tax_rate),
                   y = 0, yend = property_tax_rate), color = "steelblue", size = 2) +
  scale_y_continuous(labels = function(y) paste0(y, "%")) +
  coord_flip() +
  labs(x = NULL, y = "Property Tax Rate", 
       title = "Property Tax Rate by State Capital") +
  theme_minimal() +
  geom_text(aes(x = reorder(city_and_state, property_tax_rate),
                y = property_tax_rate, label = paste0(property_tax_rate, "%")),
            hjust = -.1, color = "black", size = 3) 

final_df4 %>% 
  ggplot(aes(x = property_tax_rate, y = lastSoldPrice)) +
  geom_point(alpha = .3) +
  #geom_smooth(method = "lm") +
  labs(x = "year built", y = "price sold (log10 scale)",
       title = "Price Sold vs Year Built") +
  scale_y_log10(labels = scales::dollar_format(prefix = "$"))

## LM-------------------------------------

#auto make a data frame including all the necessary variables that way when you need to drop NA
#it dosnt apply this to the entire function

auto <- final_df5 %>% 
  dplyr::select(lastSoldPricelog, local.flood.flood_factor_score, local.wildfire.fire_factor_score, sqft,beds,
                baths, coordinates.latitude, coordinates.longitude, age, mortgage_interest_rate, property_tax_rate,
                lot_sqft, address.locality, address.postalCode, noun_freq, adj_freq, country.region, image_counts, photos_degree)

set.seed(123)
splits <- initial_split(auto %>% drop_na())
train <- training(splits)
test <- testing(splits)

lm <- lm(lastSoldPricelog~
           local.flood.flood_factor_score +
           local.wildfire.fire_factor_score +
           sqft +
           beds +
           baths +
           coordinates.latitude +
           coordinates.longitude +
           age +
           mortgage_interest_rate +
           property_tax_rate +
           noun_freq +
           adj_freq +
           photos_degree +
           image_counts,
         data = train)

lm_preds <- predict(lm, test)

lm_train <- predict(lm, train)

#training results
lm_rmse_train <- sqrt(mean((train$lastSoldPricelog - lm_train)^2, na.rm = T))
lm_rmse_train
lm_r2_train <- R2(pred = lm_train, obs = train$lastSoldPricelog, na.rm = T)
lm_r2_train

lm_rmse_train_text <- sqrt(mean((train$lastSoldPricelog - lm_train)^2, na.rm = T))
lm_rmse_train_text
lm_r2_train_text <- R2(pred = lm_train, obs = train$lastSoldPricelog, na.rm = T)
lm_r2_train_text

#bhc results
lm_rmse_basic <- sqrt(mean((test$lastSoldPricelog - lm_preds)^2, na.rm = T))
lm_rmse_basic
lm_r2_basic <- R2(pred = lm_preds, obs = test$lastSoldPricelog, na.rm = T)
lm_r2_basic

#text results
lm_rmse_text <- sqrt(mean((test$lastSoldPricelog - lm_preds)^2, na.rm = T))
lm_rmse_text
lm_r2_text <- R2(pred = lm_preds, obs = test$lastSoldPricelog, na.rm = T)
lm_r2_text

#full results
lm_rmse_full <- sqrt(mean((test$lastSoldPricelog - lm_preds)^2, na.rm = T))
lm_rmse_full
lm_r2_full <- R2(pred = lm_preds, obs = test$lastSoldPricelog, na.rm = T)
lm_r2_full

summary(lm)

kableExtra::kable(x = broom::tidy(lm) %>% mutate_if(is.numeric, round, digits = 4),
                  format = "pipe")

test %>% 
  ggplot(aes(x=lastSoldPricelog))+
  geom_abline(slope = 1, aes(linetype= 1))+
  geom_point(aes(y=lm_preds)) +
  labs(x = "Price Sold Log", y = "lm prediction", title = "Linear Model")

# ALt method
lm.aic <- stepAIC(lm)
summary(lm.aic)


cor(final_df4 %>% select(lastSoldPricelog, baths, sqft, beds, property_tax_rate)
    %>% na.omit()) %>% 
  round(2)

## Random Forest----------------------------------------------------------------

rf <- rand_forest(mode = "regression", engine = "randomForest", trees = 500, min_n = 8, mtry = 4) 

rf_wf <- workflow() %>% 
  add_model(rf) %>% 
  add_formula(lastSoldPricelog~
                local.flood.flood_factor_score +
                local.wildfire.fire_factor_score +
                sqft +
                beds +
                baths +
                coordinates.latitude +
                coordinates.longitude +
                age +
                mortgage_interest_rate +
                property_tax_rate +
                image_counts +
                photos_degree)

folds <- vfold_cv(train)

# K-fold cross validation; Tuning the Model to determine optimal number of trees, min_n, and mtry
# Will take awhile
rf_tune <- tuneRF(x = train %>% dplyr::select(local.flood.flood_factor_score, 
                                       local.wildfire.fire_factor_score,
                                       sqft,
                                       beds,
                                       baths, 
                                       coordinates.latitude,
                                       coordinates.longitude,
                                       age,
                                       mortgage_interest_rate,
                                       property_tax_rate,
                                       noun_freq,
                                       adj_freq,
                                       photos_degree,
                                       image_counts),
                  y = train$lastSoldPricelog, mtryStart = 1, ntreeTry = 500, stepFactor = 2,
                  improve = .00001, doBest = T, plot = T, trace = T)
rf_tune

# train
rf_wf_train <- rf_wf %>% fit(train)

rf_preds_train_text <- predict(rf_wf_train, train) %>% 
  unlist()

rf_preds_train_basic <- predict(rf_wf_train, train) %>% 
  unlist()

rf_rmse_text_train <- RMSE(pred = rf_preds_train_text, obs = train$lastSoldPricelog)
rf_rmse_text_train
rf_r2_text_train <- R2(pred = rf_preds_train_text, obs = train$lastSoldPricelog)
rf_r2_text_train
rf_rmse_basic_train <- RMSE(pred = rf_preds_train_basic, obs = train$lastSoldPricelog)
rf_rmse_basic_train
rf_r2_basic_train <- R2(pred = rf_preds_train_basic, obs = train$lastSoldPricelog)
rf_r2_basic_train

rf_wf <- rf_wf %>% fit(train)

rf_preds <- predict(rf_wf, test) %>% 
  unlist()

rf_rmse_basic <- RMSE(pred = rf_preds, obs = test$lastSoldPricelog)
rf_rmse_basic
rf_r2_basic <- R2(pred = rf_preds, obs = test$lastSoldPricelog)
rf_r2_basic

rf_rmse_text <- RMSE(pred = rf_preds, obs = test$lastSoldPricelog)
rf_rmse_text
rf_r2_text <- R2(pred = rf_preds, obs = test$lastSoldPricelog)
rf_r2_text

rf_rmse_full <- RMSE(pred = rf_preds, obs = test$lastSoldPricelog)
rf_rmse_full
rf_r2_full <- R2(pred = rf_preds, obs = test$lastSoldPricelog)
rf_r2_full

test %>% 
  ggplot(aes(x = lastSoldPricelog))+
  geom_abline(slope = 1, aes(linetype= 1))+
  geom_point(aes(y = rf_preds)) +
  labs(x = "Price Sold Log", y = "prediction", title = "Random Forest")

# Create the variable importance plot
rf_model <- extract_fit_parsnip(rf_wf)
vip(rf_model, num_features = 14 ,geom = "point") +
  labs(title = "Variable Importance Plot")

## XGBoost---------------------------------------------------------------------

xgb <- boost_tree(mode = "regression", engine = "xgboost", mtry = tune(),
                  trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
                  loss_reduction = tune(), sample_size = tune(), stop_iter = tune())

xgb_wf <- workflow() %>% 
  add_model(xgb) %>% 
  add_formula(lastSoldPricelog~
                local.flood.flood_factor_score + 
                local.wildfire.fire_factor_score +
                sqft + 
                beds + 
                baths +
                coordinates.latitude +
                coordinates.longitude +
                age +
                mortgage_interest_rate +
                property_tax_rate +
                noun_freq +
                adj_freq +
                photos_degree +
                image_counts)

xgb_grid <- grid_latin_hypercube(trees(),
                                 min_n(),
                                 tree_depth(),
                                 learn_rate(),
                                 loss_reduction(),
                                 sample_size = sample_prop(),
                                 stop_iter(),
                                 finalize(mtry(), train),
                                 size = 5)

# This will take awhile; Run when we have all the image and text variables so we can tune once
xgb_results <- tune_grid(object = xgb_wf, resamples = folds, grid = xgb_grid,
                         control = control_grid(save_pred = T))

# Will select the best RMSE
best_xgb_params <- select_best(xgb_results)

best_xgb_params

xgb_wf <- finalize_workflow(xgb_wf, best_xgb_params) %>% 
  fit(train)

xgb_preds <- predict(xgb_wf, test) %>% 
  unlist()

#test results
xgb_rmse_test <- RMSE(pred = xgb_preds, obs = test$lastSoldPricelog)
xgb_rmse_test
xgb_r2_test <- R2(pred = xgb_preds, obs = test$lastSoldPricelog)
xgb_r2_test

#basic results
xgb_rmse_basic <- RMSE(pred = xgb_preds, obs = test$lastSoldPricelog)
xgb_rmse_basic
xgb_r2_basic <- R2(pred = xgb_preds, obs = test$lastSoldPricelog)
xgb_r2_basic

#text results
xgb_rmse_text <- RMSE(pred = xgb_preds, obs = test$lastSoldPricelog)
xgb_rmse_text
xgb_r2_text <- R2(pred = xgb_preds, obs = test$lastSoldPricelog)
xgb_r2_text

#full results
xgb_rmse_full <- RMSE(pred = xgb_preds, obs = test$lastSoldPricelog)
xgb_rmse_full
xgb_r2_full <- R2(pred = xgb_preds, obs = test$lastSoldPricelog)
xgb_r2_full



xgb_rmse_text2 <- RMSE(pred = xgb_preds, obs = test$lastSoldPricelog)
xgb_rmse_text2
xgb_r2_text2 <- R2(pred = xgb_preds, obs = test$lastSoldPricelog)
xgb_r2_text2

test %>% 
  ggplot(aes(x = lastSoldPricelog))+
  geom_abline(slope = 1, aes(linetype = 1))+
  geom_point(aes(y = xgb_preds)) +
  labs(x = "Price Sold Log", y = "prediction", title = "XGBoost")

# RF vs XGBoost
test %>% 
  ggplot(aes(x = rf_preds, y = xgb_preds)) +
  geom_point() +
  geom_abline(slope = 1, aes(linetype= 1)) +
  labs(title = "RF vs XBG")

## SVM-------------------------------------------------------------------------

svr <- svm_rbf(mode = "regression", 
               engine = "kernlab", 
               cost = tune(),
               rbf_sigma = tune(),
               margin = tune())

svr_wf <- workflow() %>% 
  add_model(svr) %>% 
  add_formula(lastSoldPricelog~
                local.flood.flood_factor_score + 
                local.wildfire.fire_factor_score +
                sqft + 
                beds + 
                baths +
                coordinates.latitude +
                coordinates.longitude +
                age +
                mortgage_interest_rate +
                property_tax_rate +
                noun_freq +
                adj_freq)

svr_grid <- grid_regular(cost(), rbf_sigma(), svm_margin(), levels = 5)

# This will take awhile
svr_results <- tune_grid(object = svr_wf, resamples = folds, grid = svr_grid)

# Will select the best RMSE
best_svr_params <- select_best(svr_results)

svr_wf <- finalize_workflow(svr_wf, best_svr_params) %>% 
  fit(train)

svr_preds <- predict(svr_wf, test) %>% 
  unlist()

#basic
svr_rmse_basic <- RMSE(pred = svr_preds, obs = test$lastSoldPricelog)
svr_rmse_basic
svr_r2_basic <- R2(pred = svr_preds, obs = test$lastSoldPricelog)
svr_r2_basic

#text
svr_rmse_text <- RMSE(pred = svr_preds, obs = test$lastSoldPricelog)
svr_rmse_text
svr_r2_text <- R2(pred = svr_preds, obs = test$lastSoldPricelog)
svr_r2_text

#full
svr_rmse_full <- RMSE(pred = svr_preds, obs = test$lastSoldPricelog)
svr_rmse_full
svr_r2_full <- R2(pred = svr_preds, obs = test$lastSoldPricelog)
svr_r2_full

test %>% 
  ggplot(aes(x=lastSoldPricelog))+
  geom_abline(slope = 1, aes(linetype= 1))+
  geom_point(aes(y = svr_preds)) +
  labs(x = "Price Sold Log", y = "svr prediction", title = "SVR")

## Result Table----------------------------------------------------------------

results_table <- tibble(model = c("lm", "rf", "xgb", "svr"),
                        rmse_basic = c(lm_rmse_basic, rf_rmse_basic, xgb_rmse_basic, svr_rmse_basic),
                        R2_basic = c(lm_r2_basic, rf_r2_basic, xgb_r2_basic, svr_r2_basic),
                        rmse_text = c(lm_rmse_text, rf_rmse_text, xgb_rmse_text, svr_rmse_text),
                        R2_text = c(lm_r2_text, rf_r2_text, xgb_r2_text, svr_r2_text),
                        rmse_full = c(lm_rmse_full, rf_rmse_full, xgb_rmse_full, svr_rmse_full),
                        R2_full = c(lm_r2_full, rf_r2_full, xgb_r2_full, svr_r2_full))

results_table <- tibble(model = c("lm", "rf", "xgb", "svr"),
                        rmse_basic = c(lm_rmse_basic, rf_rmse_basic, xgb_rmse_basic, svr_rmse_basic),
                        R2_basic = c(lm_r2_basic, rf_r2_basic, xgb_r2_basic, svr_r2_basic),
                        rmse_text = c(lm_rmse_text, rf_rmse_text, xgb_rmse_test, svr_rmse_text),
                        R2_text = c(lm_r2_text, rf_r2_text, xgb_r2_test, svr_r2_text))
results_table

results_table2 <- tibble(
  model = c("lm", "rf", "xgb", "svr"),
  rmse_basic = round(c(lm_rmse_basic, rf_rmse_basic, xgb_rmse_basic, svr_rmse_basic), 4),
  R2_basic = round(c(lm_r2_basic, rf_r2_basic, xgb_r2_basic, svr_r2_basic), 4),
  rmse_text = round(c(lm_rmse_text, rf_rmse_text, xgb_rmse_text2, svr_rmse_text), 4),
  R2_text = round(c(lm_r2_text, rf_r2_text, xgb_r2_text2, svr_r2_text), 4))
results_table2

results_table3 <- tibble(
  model = c("lm", "rf", "xgb", "svr"),
  rmse_basic = round(c(lm_rmse_basic, rf_rmse_basic, xgb_rmse_basic, svr_rmse_basic), 4),
  R2_basic = round(c(lm_r2_basic, rf_r2_basic, xgb_r2_basic, svr_r2_basic), 4),
  rmse_text = round(c(lm_rmse_text, rf_rmse_text, xgb_rmse_text2, svr_rmse_text), 4),
  R2_text = round(c(lm_r2_text, rf_r2_text, xgb_r2_text2, svr_r2_text), 4),
  rmse_full = round(c(lm_rmse_full, rf_rmse_full, xgb_rmse_full, NA), 4),
  R2_full = round(c(lm_r2_full, rf_r2_full, xgb_r2_full, NA), 4))
results_table3

summary(final_df4)

## Text-------------------------------------------------------------------------

nouns <- read.csv("C:\\Users\\Ben_1\\Downloads\\nouns.csv")
adjectives <- read.csv("C:\\Users\\Ben_1\\Downloads\\adjectives.csv")

text35 <- final_df4 %>% 
  dplyr::select(text, id) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

word_count_per_id <- text35 %>%
  group_by(id) %>%
  summarize(total_words = sum(nchar(word)))

merged_data <- merge(word_count_per_id, nouns, by = "id")

merged_data <- merged_data %>% 
  mutate(noun_freq = nouns/total_words)

merged_data <- merge(merged_data, adjectives, by = "id")

merged_data <- merged_data %>% 
  mutate(adj_freq = adjectives/total_words)

final_df4 <- merge(final_df4, merged_data, by = "id", all.x = TRUE)

final_df4 %>% 
  dplyr::select(text,total_words,noun_freq, adj_freq) %>% 
  view()

nondesc = final_df4 %>% 
  filter(total_words <= 45) %>% 
  dplyr::select(text) %>% 
  unique()

nondesc_list <- read_csv("nondesc_list.csv")

final_df4 <- final_df4 %>% 
  mutate(text=ifelse(text %in% nondesc_list$text, NA, text))

final_df4 <- final_df4 %>% 
  mutate(noun_freq = ifelse(is.na(noun_freq), 0, noun_freq)) %>% 
  mutate(adj_freq = ifelse(is.na(adj_freq), 0, adj_freq))

final_df4 %>% 
  filter(adj_freq > 0 & adj_freq <= 0.08) %>%
  ggplot(aes(x = adj_freq, lastSoldPrice)) +
  geom_point() +
  scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
  labs(y = "Price Sold (log10 scale)", title = "Price sold vs adjective frequency")
##-------------------------------------------------------------
final_df5 <- read.csv("C:\\Users\\Ben_1\\Downloads\\final_data_complete.csv")

final_df5 <- final_df5 %>% 
  mutate(image_counts = ifelse(is.na(image_counts), 0, image_counts)) %>% 
  mutate(photos_degree = ifelse(is.na(photos_degree), 0, photos_degree))

xgb_rmse_full <- 0.1162
xgb_r2_full <- 0.8454

final_df5 %>% 
  filter(!is.na(photos_degree)) %>% 
  mutate(photos_degree = as.factor(photos_degree)) %>%  
  ggplot(aes(x = photos_degree, y = lastSoldPrice, fill = photos_degree), show.legend = F) +
  geom_boxplot(show.legend = F) +
  geom_jitter(width = .1, alpha = .25, show.legend = F) +
  geom_violin(show.legend = F) +
  labs(x = "Degree based on Visual Features", y = "Price Sold (log10 scale)",
       title = "Price Sold by Property Degree") +
  scale_y_log10(labels = scales::dollar_format(prefix = "$"))

