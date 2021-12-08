pacman::p_load(data.table, qs, stringr, tidyverse, googlesheets4, ggplot2, 
               lubridate, reshape2, openxlsx, scales, googledrive, zoo,
               forecast, rlist)

disney_raw <- read.csv("disney_plus_titles.csv")

View(head(disney_raw))

disney_raw %>% 
  distinct(title) %>% 
  tally()

disney <- disney_raw %>% 
  mutate(date_added = ymd(as.Date(date_added, format = "%B %d, %Y")),
         month_added = format(date_added, "%Y-%m")) 


# Does Disney+ has more focus on TV Shows than movies in recent years?
# visual of occurrences by type
# movies seem to have the edge throughout the pandemic months though TV show
# titles are getting added more
# filtered out when it first got released in 2019 due to large added titles
# rolling sum of total titles in tandem recommended
# forecast expected movies/tv shows added for next 6 months???

# occurrences by type and month/day 
type_tally <- disney %>% 
  group_by(type, month_added) %>%
  filter(month_added != "NA") %>% 
  summarise(count = n()) %>% 
  arrange(month_added, desc(count))

type_tally %>% 
  filter(month_added != "NA" & month_added >= "2020-01") %>% 
  ggplot(aes(x = month_added, y=count, color=type, group = type)) + geom_smooth() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Year-Month", y = "Count of Titles", title = "Number of TV Shows and Movies Added on Disney+")

# Understanding what content is available in different countries

country_availability <- disney %>% 
  filter(grepl("Germany", country) | !grepl("", country))

countries <- c("United States", "United Kingdom", "Australia", "Germany", "France",
               "South Korea", "Japan", "Canada", "Mexico", "Spain", "Hungary", "India",
               "Singapore", "Ireland", "Brazil")


# mapping of each country and how many titles are offered on disney
# currently displays indicies, will need to change to labels


# get a unique country list
country_list <- unlist(str_split(disney$country, ","))
country_list <- unique(trimws(country_list))
countries <- country_list
print(country_list)

# get total titles
total_titles <- sum(nrow(disney))
# get mapping for country counts
disney_countries <- map_df(countries, ~ disney %>% 
         filter(grepl(.x, country, ignore.case=TRUE)) %>% 
         count(), .id="Country") %>% 
  mutate(Country_Name = countries[as.numeric(Country)],
         Number_of_titles = n,
         Percentage_of_titles = Number_of_titles/total_titles)

disney_countries <- disney_countries %>% 
  select(Country_Name, Number_of_titles, Percentage_of_titles) %>% 
  filter(Country_Name != "") %>% 
  arrange(desc(Number_of_titles))

# column plot of titles by country
disney_countries %>% 
  arrange(Number_of_titles) %>% 
  top_n(10, Number_of_titles) %>% 
  ggplot(aes(x = reorder(Country_Name, -Percentage_of_titles), y = Percentage_of_titles), color = Country_Name) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Country", y = "% Share of Disney+ Titles") +
  annotate("text", x = 1.3, y = 0.9, label = "1147 titles!")
  
# Network analysis of Actors / Directors and find interesting insights
# unpack list similar to titles
director_list <- unlist(str_split(disney$director, ","))
director_list <- unique(trimws(director_list))
print(director_list)

disney_director <- map_df(director_list, ~ disney %>% 
                            filter(grepl(.x, director, ignore.case=TRUE)) %>% 
                            count(), .id="director") %>% 
  mutate(Director_Name = director_list[as.numeric(director)],
         Number_of_titles = n,
         Percentage_of_titles = Number_of_titles/total_titles) %>% 
  arrange(desc(Number_of_titles))

actor_list <- unlist(str_split(disney$cast, ","))
actor_list <- unique(trimws(actor_list))
print(actor_list)

# will need to correct due to NA and possible misattribution
disney_actor <- map_df(actor_list, ~ disney %>% 
                            filter(grepl(.x, cast, ignore.case=TRUE)) %>% 
                            count(), .id="actor") %>% 
  mutate(Actor_Name = director_list[as.numeric(actor)],
         Number_of_titles = n,
         Percentage_of_titles = Number_of_titles/total_titles) %>% 
  arrange(desc(Number_of_titles))


# Identifying similar content by matching text-based features

