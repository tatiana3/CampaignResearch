library(spotifyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidytext)
library(formattable)
#quanteda

Sys.setenv(SPOTIFY_CLIENT_ID = '7a3050ce36344121941e46c21c18b104')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a534f2c3270d4611baf799c0f06fad8c')
access_token <- get_spotify_access_token()

buttigieg <- get_playlist_audio_features('nicoleklein07@aol.com', '60MlbwzROxKGHRWVjY2oUU')
sanders <- get_playlist_audio_features('nicoleklein07@aol.com', '4USt0LOJt4uGDBM2jjgfKI')
obama <- get_playlist_audio_features('nicoleklein07@aol.com', '7o63mjuaEMILFDd0HGNOR7')
kamala <- get_playlist_audio_features('nicoleklein07@aol.com', '7BVgmVbt0fshvbtSOzDplZ')
trump <- get_playlist_audio_features('nicoleklein07@aol.com', '1gOWVQHFUlwmI6aNWXJOXR')
romney <- get_playlist_audio_features('nicoleklein07@aol.com', '1XtLPm0jEa3a8QH0P0fYOg')


#for pre-searched lyrics
list = str_replace_all(list, "[\r\n]" , " ")

#EXAMPLE
obama$lyrics[1:41] = str_replace_all(obama$lyrics[1:41], "[\r\n]" , " ")

#get artist
kamala$artist <- NA
for(i in 1:length(kamala$playlist_id)) {
  temp <- as.data.frame(kamala$track.artists[i])
  names(kamala$artist) <- c(names(kamala$artist), temp[,3])
  kamala$artist[i] <- temp[,3]
}

buttigieg$artist <- NA
for(i in 1:length(buttigieg$playlist_id)) {
  temp <- as.data.frame(buttigieg$track.artists[i])
  names(buttigieg$artist) <- c(names(buttigieg$artist), temp[,3])
  buttigieg$artist[i] <- temp[,3]
}

sanders$artist <- NA
for(i in 1:length(sanders$playlist_id)) {
  temp <- as.data.frame(sanders$track.artists[i])
  names(sanders$artist) <- c(names(sanders$artist), temp[,3])
  sanders$artist[i] <- temp[,3]
}

obama$artist <- NA
for(i in 1:length(obama$playlist_id)) {
  temp <- as.data.frame(obama$track.artists[i])
  names(obama$artist) <- c(names(obama$artist), temp[,3])
  obama$artist[i] <- temp[,3]
}

trump$artist <- NA
for(i in 1:length(trump$playlist_id)) {
  temp <- as.data.frame(trump$track.artists[i])
  names(trump$artist) <- c(names(trump$artist), temp[,3])
  trump$artist[i] <- temp[,3]
}

romney$artist <- NA
for(i in 1:length(romney$playlist_id)) {
  temp <- as.data.frame(romney$track.artists[i])
  names(romney$artist) <- c(names(romney$artist), temp[,3])
  romney$artist[i] <- temp[,3]
}

#GET LYRICS MANUALLY
kamala$lyrics <- NA
buttigieg$lyrics <- NA
sanders$lyrics <- NA
obama$lyrics <- NA
trump$lyrics <- NA
romney$lyrics <- NA

#GET GENDER MANUALLY
kamala$gender <- NA
buttigieg$gender <- NA
sanders$gender <- NA
obama$gender <- NA
trump$gender <- NA
romney$gender <- NA

fix.contractions <- function(doc) {
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

kamala$lyrics <- sapply(kamala$lyrics, fix.contractions)

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
kamala$lyrics <- sapply(kamala$lyrics, removeSpecialChars)
kamala$lyrics <- sapply(kamala$lyrics, tolower)

kamala$track.album.release_date <- as.Date(kamala$track.album., "%d%b%Y")

kamala$year <- NA
kamala$decade <- NA
kamala$year <- as.numeric(substr(kamala$track.album.release_date, 1, 4))

kamala <- kamala %>%
  mutate(decade = 
           ifelse(kamala$year %in% 1970:1979, "1970s",
                  ifelse(kamala$year %in% 1980:1989, "1980s", 
                         ifelse(kamala$year %in% 1990:1999, "1990s", 
                                ifelse(kamala$year %in% 2000:2009, "2000s", 
                                       ifelse(kamala$year %in% 2010:2019, "2010s",
                                              ifelse(kamala$year %in% 1960:1969, "1960s",
                                                     ifelse(kamala$year %in% 1950:1959, "1950s",
                                              "NA"))))))))

kamala <- kamala %>%
  mutate(charted = 
           ifelse(kamala$track.popularity %in% 50:100, "Popular", "Unpopular"))

my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

#chart on released songs per decade
kamala %>%
  filter(decade != "NA") %>%
  group_by(decade, charted) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = charted), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Released Songs") +
  labs(x = NULL, y = "Song Count")

undesirable_words <- c("chorus", "repeat",
                       "theres", "yeah", "baby", 
                       "alright", "wanna", "gonna", 
                       "whoa", "gotta", "make", "ooh", "uurh", 
                       " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "repeat")

kamala_words_filtered <- kamala %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

kamala_words_filtered %>% 
  filter(word == "race") %>%
  select(word, track.name, year, decade, track.popularity, charted) %>%
  arrange() %>%
  top_n(10,track.name) %>%
  mutate(song = color_tile("lightblue","lightblue")(track.name)) %>%
  mutate(word = color_tile("lightgreen","lightgreen")(word)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Tokenized Format Example") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)
