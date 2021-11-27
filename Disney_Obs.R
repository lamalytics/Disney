pacman::p_load(data.table, qs, stringr, tidyverse, googlesheets4, ggplot2, 
               lubridate, reshape2, openxlsx, scales, googledrive, zoo,
               forecast)

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

countries <- c("United States", "United Kingdom", "Austrailia", "Germany", "France",
               "South Korea", "Japan", "Canada", "Mexico")

# mapping of each country and how many titles are offered on disney
# currently displays indicies, will need to change to labels
map_df(countries, 
       ~ disney %>% 
         filter(grepl(.x, country, ignore.case=TRUE)) %>% 
         count(), 
       .id="Country")

  
# Network analysis of Actors / Directors and find interesting insights

# Identifying similar content by matching text-based features

