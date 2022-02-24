library(dplyr)
library(dbplyr)
library(ggplot2)
library(tidyverse)
library(maps)

# process data
data <- data.frame(read.csv("../data/incarceration_trends.csv"))
data <- data %>%
  filter(year >= 1990 & year <= 2016) %>%
  # remove all rows with no desired data
  filter(!is.na(aapi_pop_15to64) | !is.na(aapi_prison_pop) | !is.na(aapi_jail_pop)
         | !is.na(white_pop_15to64) | !is.na(white_prison_pop) | !is.na(white_jail_pop)) %>%
  # select desired variables
  select(year, fips, county_name,
         aapi_pop_15to64, aapi_prison_pop, aapi_jail_pop,
         white_pop_15to64, white_prison_pop, white_jail_pop
  )

# total infos
total_data_by_year <- data %>%
  group_by(year) %>%
  summarise(
    aapi_sum = sum(aapi_pop_15to64, na.rm = TRUE),
    aapi_prison_sum = sum(aapi_prison_pop, na.rm = TRUE),
    aapi_prison_rate = sum(aapi_prison_pop, na.rm = TRUE)/sum(aapi_pop_15to64, na.rm = TRUE),
    aapi_jail_sum = sum(aapi_jail_pop, na.rm = TRUE),
    aapi_jail_rate = sum(aapi_jail_pop, na.rm = TRUE)/sum(aapi_pop_15to64, na.rm = TRUE),
    white_sum = sum(white_pop_15to64, na.rm = TRUE),
    white_prison_sum = sum(white_prison_pop, na.rm = TRUE),
    white_prison_rate = sum(white_prison_pop, na.rm = TRUE)/sum(white_pop_15to64, na.rm = TRUE),
    white_jail_sum = sum(white_jail_pop, na.rm = TRUE),
    white_jail_rate = sum(white_jail_pop, na.rm = TRUE)/sum(white_pop_15to64, na.rm = TRUE)
  )

# join county geo info
us_counties <- map_data("county")
us_counties$county_name <- paste(us_counties$region, us_counties$subregion, sep = ",")
county.fips <- rename(county.fips, county_name = polyname)
us_counties <- left_join(us_counties, county.fips)

data_couty <- data %>%
  group_by(fips) %>%
  summarise(
    aapi_sum = sum(aapi_pop_15to64),
    white_sum = sum(white_pop_15to64),
    aapi_prison_rate = sum(aapi_prison_pop, na.rm = TRUE)/sum(aapi_pop_15to64, na.rm = TRUE),
    white_prison_rate = sum(white_prison_pop, na.rm = TRUE)/sum(white_pop_15to64, na.rm = TRUE),
  )

us_counties <- inner_join(us_counties, data_couty, by="fips")


# Summary
summary <- list()
# index 0 for aapi value, 1 for white value

summary$average_prison_pop <- c(
  mean(total_data_by_year$aapi_prison_sum),
  mean(total_data_by_year$white_prison_sum)
)

summary$highest_p_rate_in_a_year <- c(
  max(total_data_by_year$aapi_prison_sum/total_data_by_year$aapi_sum),
  max(total_data_by_year$white_prison_sum/total_data_by_year$white_sum)
)

summary$lowest_p_rate_in_a_year <- c(
  min(total_data_by_year$aapi_prison_sum/total_data_by_year$aapi_sum),
  min(total_data_by_year$white_prison_sum/total_data_by_year$white_sum)
)

summary$highest_j_rate_in_a_year <- c(
  max(total_data_by_year$aapi_jail_sum/total_data_by_year$aapi_sum),
  max(total_data_by_year$white_jail_sum/total_data_by_year$white_sum)
)

summary$lowest_j_rate_in_a_year <- c(
  min(total_data_by_year$aapi_jail_sum/total_data_by_year$aapi_sum),
  min(total_data_by_year$white_jail_sum/total_data_by_year$white_sum)
)

summary$p_increasement <- c(
  (total_data_by_year$aapi_prison_sum[27] - total_data_by_year$aapi_prison_sum[1])/total_data_by_year$aapi_prison_sum[1],
  (total_data_by_year$white_prison_sum[27] - total_data_by_year$white_prison_sum[1])/total_data_by_year$white_prison_sum[1]
)

summary$j_increasement <- c(
  (total_data_by_year$aapi_jail_sum[27] - total_data_by_year$aapi_jail_sum[1])/total_data_by_year$aapi_jail_sum[1],
  (total_data_by_year$white_jail_sum[27] - total_data_by_year$white_jail_sum[1])/total_data_by_year$white_jail_sum[1]
)

# make summary table
summary_table <- t(data.frame(
  rename(
    data.frame(summary),
    Average_Prison_Popluation_Per_Year = average_prison_pop,
    Highest_Prison_Population_Rate_in_a_year = highest_p_rate_in_a_year,
    Lowest_Prison_Population_Rate_in_a_year = lowest_p_rate_in_a_year,
    Highest_Jail_Population_Rate_in_a_year = highest_j_rate_in_a_year,
    Lowest_Jail_Population_Rate_in_a_year = lowest_j_rate_in_a_year,
    Prison_percentage_increasement_since_1990 = p_increasement,
    Jail_percentage_increasement_since_1990 = j_increasement,
  )
))

summary_table[1,1] <- round(summary_table[1,1], digits = 2)
summary_table[1,2] <- round(summary_table[1,2], digits = 2)
summary_table[2:7,1] <- round(summary_table[2:7,1], digits = 5)
summary_table[2:7,2] <- round(summary_table[2:7,2], digits = 5)
summary_table[2:7,1] <- summary_table[2:7,1] * 100
summary_table[2:7,2] <- summary_table[2:7,2] * 100
summary_table[2:7,1] <- paste(as.character(summary_table[2:7,1]), "%")
summary_table[2:7,2] <- paste(as.character(summary_table[2:7,2]), "%")
summary_table <- rbind(c("AAPI", "White"), summary_table)

# time series chart
# disposed chart
# tsc <- ggplot(total_data_by_year, aes(total_data_by_year$year)) +
#   geom_line(aes(y = (aapi_prison_sum - aapi_prison_sum[6])/aapi_prison_sum[6], color = "AAPI Prison sum")) +
#   geom_line(aes(y = (aapi_jail_sum - aapi_jail_sum[6])/aapi_jail_sum[6], color = "AAPI Jail sum")) +
#   geom_line(aes(y = (white_prison_sum - white_prison_sum[6])/white_prison_sum[6], color = "White Prison sum")) +
#   geom_line(aes(y = (white_jail_sum - white_jail_sum[6])/white_jail_sum[6], color = "White Jail sum")) +
#   scale_color_manual(values=color_p) +
#   scale_y_continuous(breaks = seq(0.0, 100, by=5), labels = scales::percent) +
#   scale_x_continuous(breaks = seq(1990, 2016, by=5)) +
#   ggtitle("Prison and Jail Rate of AAPI and White from 1990 to 2016") +
#   xlab("year") +
#   ylab("Percentage %")

color_p <- c("#E69F00", "#D55E00", "#0072B2", "#56B4E9")
tsc <- ggplot(total_data_by_year, aes(total_data_by_year$year)) +
  geom_line(aes(y = aapi_prison_rate, color = "AAPI Prison Rate")) +
  geom_line(aes(y = white_prison_rate, color = "White Prison Rate")) +
  geom_line(aes(y = aapi_jail_rate, color = "AAPI Jail Rate")) +
  geom_line(aes(y = white_jail_rate, color = "White Jail Rate")) +
  scale_color_manual(values=color_p) +
  scale_y_continuous(breaks = seq(0.0, .005, by=.0005), labels = scales::percent) +
  scale_x_continuous(breaks = seq(1990, 2016, by=5)) +
  ggtitle("Prison and Jail Population Rate from 1990 to 2016") +
  xlab("Year") +
  ylab("Rate of Population %")

# Variable comparison chart
chart_prison_jail_rate_increasement <- ggplot(total_data_by_year, aes(total_data_by_year$year)) +
  geom_line(aes(y = (aapi_prison_sum - aapi_prison_sum[1])/aapi_prison_sum[1], color = "AAPI Prison Population")) +
  geom_line(aes(y = (aapi_jail_sum - aapi_jail_sum[1])/aapi_jail_sum[1], color = "AAPI Jail Population")) +
  geom_line(aes(y = (white_prison_sum - white_prison_sum[1])/white_prison_sum[1], color = "White Prison Population")) +
  geom_line(aes(y = (white_jail_sum - white_jail_sum[1])/white_jail_sum[1], color = "White Jail Population")) +
  scale_color_manual(values=color_p) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1990, 2016, by=5)) +
  ggtitle("Prison Population Increasement from 1990 to 2016") +
  xlab("Year") +
  ylab("Increasement in Prison Population %")

chart_population_increasement <- ggplot(total_data_by_year, aes(total_data_by_year$year)) +
  geom_line(aes(y = aapi_sum/aapi_sum[1], color = "AAPI Population")) +
  geom_line(aes(y = white_sum/white_sum[1], color = "White Population")) +
  scale_color_manual(values=c(color_p[2], color_p[4])) +
  scale_x_continuous(breaks = seq(1990, 2016, by=5)) +
  ggtitle("Total Population Increasement from 1990 to 2016") +
  xlab("Year") +
  ylab("Increasement of Population %")


# Map

map <- ggplot(
    data = us_counties,
    mapping = aes(x = long, y = lat, group = group, fill = (aapi_prison_rate - white_prison_rate))
  ) +
  geom_polygon() +
  coord_map() +
  scale_fill_continuous(low = "#FFFFFF", high = "Red", label = scales::percent) +
  labs(fill = "Prison Rate Difference %") +
  ggtitle("AAPI and White Prison Population Rate Difference in the U.S. by County") +
  theme_minimal()


