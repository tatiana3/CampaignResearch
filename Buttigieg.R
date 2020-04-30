buttigieg$lyrics <- sapply(buttigieg$lyrics, fix.contractions)
buttigieg$lyrics <- sapply(buttigieg$lyrics, removeSpecialChars)
buttigieg$lyrics <- sapply(buttigieg$lyrics, tolower)

buttigieg$year <- NA
buttigieg$decade <- NA
buttigieg$year <- as.numeric(substr(buttigieg$track.album.release_date, 1, 4))

buttigieg <- buttigieg %>%
  mutate(decade = 
           ifelse(buttigieg$year %in% 1970:1979, "1970s",
                  ifelse(buttigieg$year %in% 1980:1989, "1980s", 
                         ifelse(buttigieg$year %in% 1990:1999, "1990s", 
                                ifelse(buttigieg$year %in% 2000:2009, "2000s", 
                                       ifelse(buttigieg$year %in% 2010:2019, "2010s",
                                              ifelse(buttigieg$year %in% 1960:1969, "1960s",
                                                     ifelse(buttigieg$year %in% 1950:1959, "1950s",
                                                            "NA"))))))))

buttigieg <- buttigieg %>%
  mutate(charted = 
           ifelse(buttigieg$track.popularity %in% 50:100, "Popular", "Unpopular"))

#chart on released songs per decade
buttigieg %>%
  filter(decade != "NA") %>%
  group_by(decade) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs), stat = "identity", fill = my_colors[2])  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Release Dates of Pete Buttigieg's Campaign Music") +
  labs(x = "Decade", y = "Song Count")

buttigieg %>%
  filter(decade != "NA") %>%
  group_by(decade) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = 'blue'), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Release Dates of Pete Buttigieg's Campaign Music") +
  labs(x = "Decade", y = "Song Count")

buttigieg_words_filtered <- buttigieg %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

buttigieg_words_filtered %>% 
  filter(word == "love") %>%
  select(word, track.name, year, decade, track.popularity, charted) %>%
  arrange() %>%
  top_n(10,track.name) %>%
  mutate(song = color_tile("lightblue","lightblue")(track.name)) %>%
  mutate(word = color_tile("lightgreen","lightgreen")(word)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Tokenized Format Example") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

buttigieg_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[1]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Pete Buttigieg's Lyrics") +
  coord_flip()

buttigieg_words_counts <- buttigieg_words_filtered %>%
  count(word, sort = TRUE) 

wordcloud2(buttigieg_words_counts[1:300, ], size = .5)
