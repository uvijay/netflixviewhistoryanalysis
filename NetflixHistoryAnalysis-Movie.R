install.packages('dash')
library(forcats)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(tidyverse)
library(stringr)
library(data.table)
library(fuzzyjoin)

setwd("F:/Datasets")                        
viewing_data <- read.csv("AritraTVNetflixViewingHistory.csv",stringsAsFactors=FALSE)
str(viewing_data)
summary(viewing_data)
viewing_data$Date <- ymd(viewing_data$Date)


viewing_data_split <- viewing_data %>%
  separate(col = Title, into = c("title", "season", "episode"), sep = ': ')
viewing_data_split <- viewing_data_split %>% mutate(category = ifelse(is.na(viewing_data_split$season),"Movies","Series/Episodes"))
viewing_data_split$title <- str_to_upper(str_trim(viewing_data_split$title,"both"))

viewing_data_movies <- viewing_data_split[is.na(viewing_data_split$season),]
viewing_data_movies <- viewing_data_split[is.na(viewing_data_split$episode),]
viewing_data_movies <- select(viewing_data_movies, 1,4,5)
viewing_data_movies <- viewing_data_movies %>%
  separate(col = title, into = c("title","alt_title"), sep = '[(]',extra = "drop",fill = "right")
viewing_data_movies$title <- str_to_upper(str_trim(viewing_data_movies$title,"both"))


movies_data <- read.csv("IMDB-Dataset/movies.csv",stringsAsFactors=FALSE)
movies_data <- movies_data %>% 
  extract(title, c("title","year"), regex = '(^.+)\\((\\d+)')
bmovies_data <- read.csv("IMDB-Dataset/bollywoodmovies.csv",stringsAsFactors=FALSE)
bmovies_data <- select(bmovies_data, 1,2,3,5)
colnames(bmovies_data) <- c('movieId','title','year','genres')
movies_data$type='Hollywood'
bmovies_data$type='Bollywood'
movies_data_splt <- rbind(movies_data, bmovies_data)

movies_data_all <- movies_data_splt %>%
  separate(col = genres, into = c("Genre1", "Genre2", "Genre3", "Genre4", "Genre5"), sep = '[|]',extra = "drop",fill = "right")

movies_data_all <- movies_data_all %>%
  separate(col = title, into = c("title","alt_title"), sep = '[(]',extra = "drop",fill = "right")
movies_data_all <- movies_data_all %>%
  separate(col = title, into = c("title","alt_title"), sep = ':',extra = "drop",fill = "right")
movies_data_all <- movies_data_all %>%
  separate(col = title, into = c("title","alt_title"), sep = '[..]',extra = "drop",fill = "right")

movies_data_all$title <- sub("(.*)\\,(.*)","\\2\\1",movies_data_all$title) 
movies_data_all$title <- str_to_upper(str_trim(movies_data_all$title,"both"))
movies_data_all$Genre1 <- str_trim(movies_data_all$Genre1,"both")

movies_viewed <- merge(viewing_data_movies, movies_data_all,
                    all.x = TRUE, all.y = FALSE)

movies_viewed <- movies_viewed[!is.na(movies_viewed$movieId),]
movies_viewed <- subset(movies_viewed, Genre1!='(no genres listed)')
movies_viewed$monyr <- format(movies_viewed$Date,"%b-%y")
movies_viewed$yr <- format(movies_viewed$Date,"%Y")
movies_viewed <- movies_viewed %>%
  group_by(title, Date) %>%
  arrange(desc(year)) %>% # in each group, arrange in ascending order by distance
  filter(row_number() == 1)
#movies_viewed_na <- movies_viewed[is.na(movies_viewed$Genre1),]
#sapply(movies_viewed, function(x) sum(is.na(x)))



netflix_movies <- movies_viewed %>%
  count(Date,title) %>%
  arrange(desc(n))


# PLOTTING MOVIES PER DAY
netflix_movies_plot <- ggplot(aes(x = Date, y = n, color = n), data = netflix_movies) +
  geom_col(color = c("#db4242")) +
  theme_minimal() +
  ggtitle("Movies Watched on Netflix Daily") +
  scale_x_date(breaks = "5 months",date_labels = "%b %y") +
  labs(x = "Year", y = "No of Movies") 
netflix_movies_plot


netflix_movies_day <- netflix_movies[order(netflix_movies$Date),]
netflix_movies_day$weekday <- wday(netflix_movies_day$Date)
netflix_movies_day$weekdayStr <- weekdays(netflix_movies_day$Date, abbreviate = T)
netflix_movies_day$months <- months(netflix_movies_day$Date, abbreviate = T)

netflix_movies_day$weekdayStr <- 
  factor(netflix_movies_day$weekday, levels = rev(1:7), 
         labels = rev(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")),ordered = TRUE)
netflix_movies_day$months <- factor(month(netflix_movies_day$Date)
                                      ,levels = as.character(1:12), 
                                      labels = c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec"),ordered = TRUE)


netflix_movies_day$yearmon <- factor(as.yearmon(netflix_movies_day$Date)) 
netflix_movies_day$week <- as.numeric(format(netflix_movies_day$Date,"%W"))
netflix_movies_day$weekbyseven <- ceiling(day(netflix_movies_day$Date) / 7)

netflix_movies_day_calendar <- 
  ggplot(netflix_movies_day, aes(weekbyseven, weekdayStr, fill = n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(netflix_movies_day$Date) ~ months) + 
  scale_fill_gradient(low = "#FFD000", high = "#FF1919") + 
  ggtitle("Netflix Movie Watch history") +
  labs(x = "Days", y = "Month") 
netflix_movies_day_calendar


total_genre <- movies_viewed %>%
               count(Genre1)

# PLOTTING FAVORITE GENRES - OVERALL
genre_viewing_plot <- total_genre %>% 
  ggplot(aes(Genre1, n)) +
  geom_col(fill = "#5b59d6") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Movies watched by Genre in Netflix - Overall")
genre_viewing_plot

total_genre_yr <- movies_viewed %>%
  count(yr,Genre1) %>% 
  group_by(yr) %>% 
  mutate(Genre1, Percentage=n/sum(n)*100)

# PLOTTING FAVORITE GENRES - OVER THE YEARS
genre_viewing_plot <- total_genre_yr %>% 
  ggplot(aes(Genre1, Percentage)) +
  geom_col(fill = "#5b59d6") +
  facet_wrap( ~ yr) +
  coord_polar()  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.y.right = element_blank(),                 # hide right axis labels
        axis.ticks.y = element_blank(),                      # hide left/right axis ticks
        axis.text.x = element_text(size = 8, face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) +
  ggtitle("Movies watched by Genre in Netflix - Change over the years")
genre_viewing_plot


netflix_movies_year <- movies_viewed %>%
  ungroup %>%
  count(yr,type) 


# PLOTTING MOVIES by TYPE - YEARLY
netflix_movies_plot <- netflix_movies_year %>% 
  ggplot(aes(x = yr, y = n, fill=type,label = n)) +
  geom_bar(position="stack",stat="identity" ) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  ggtitle("Movies watched by year H'wood vs B'Wood") +
  labs(x = "Year", y = "No of Movies") 
netflix_movies_plot


viewing_data_split$yr <- format(viewing_data_split$Date,"%Y")

netflix_movies_series_year <- viewing_data_split %>%
  ungroup %>%
  count(yr,category) 


# PLOTTING MOVIES by TYPE - YEARLY
netflix_movies_series_plot <- netflix_movies_series_year %>% 
  ggplot(aes(x = yr, y = n, fill=category,label = n)) +
  geom_bar(position="stack",stat="identity" ) +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  ggtitle("Movies watched by year Movies vs Series") +
  scale_fill_manual(values=c("#999999",  "#56B4E9"))
  labs(x = "Year", y = "No of Movies/Episodes") 
netflix_movies_series_plot




total_release_yr <- movies_viewed %>%
  ungroup %>%
  count(year)


# PLOTTING MOVIES by RELEASE YEAR
netflix_genre_plot <- subset(total_release_yr, year>1950) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "#59a6d6") +
  ggtitle("Movies watched by Release Year") +
  labs(x = "Year", y = "No of Movies") 
netflix_genre_plot
