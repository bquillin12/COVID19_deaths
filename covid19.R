##########################################################################################
########################## COVID-19 deaths per 1 million population ######################
##########################################################################################


# Installations -----------------------------------------------------------

# install.packages(c("tidyverse", "knitr"))
library(tidyverse)
library(knitr)

# Load data and summarize -------------------------------------------------

# read latest covid deaths data from JHU CSSE
covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-23-2020.csv")

# summarize infection deaths by country

covid_summary <- covid %>% 
  select(Province_State, Country_Region, Deaths) %>% 
  group_by(Country_Region) %>% 
  summarize(total = sum(Deaths, na.rm = TRUE))

# read world population data from World Bank Development Data Group (http://data.worldbank.org)
population <- read_csv("population_2018.csv")
population$pop_2018 <- as.numeric(population$pop_2018)

# Join dataframes
covid_joined <- covid_summary %>% 
  left_join(population,
            by = "Country_Region")

# calculate deaths per 1 million population 
covid_joined$deaths_per_million <- covid_joined$total/(covid_joined$pop_2018/1000000)


# Create table for selected countries -------------------------------------

covid_table <- covid_joined %>% 
  select(Country_Region, deaths_per_million) %>%
  filter(Country_Region %in% c("US", "China", "Italy", "Korea", "Japan", "United Kingdom",
                               "Mexico", "Canada", "India", "Sweden", "Norway", "France", "Spain",
                               "Brazil", "South Korea", "Argentina", "South Africa", "Egypt",
                               "Russia", "Finland", "Denmark"))

covid_table %>% 
  spread(Country_Region, deaths_per_million) %>% 
  kable()


# Plot --------------------------------------------------------------------

covid.plot <- ggplot(covid_table, aes(x = reorder(Country_Region, deaths_per_million),
                        y = deaths_per_million)) +
         geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "COVID-19 deaths per 1 million population",
       subtitle = "Selected countries as of April 23, 2020",
       caption = "Source: Johns Hopkins CSSE.",
       x = "",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.0))

covid.plot
  
                        
                        
                        



