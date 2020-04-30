trump$lyrics <- sapply(trump$lyrics, fix.contractions)
trump$lyrics <- sapply(trump$lyrics, removeSpecialChars)
trump$lyrics <- sapply(trump$lyrics, tolower)

trump$year <- NA
trump$decade <- NA
trump$year <- as.numeric(substr(trump$track.album.release_date, 1, 4))

trump <- trump %>%
  mutate(decade = 
           ifelse(trump$year %in% 1970:1979, "1970s",
                  ifelse(trump$year %in% 1980:1989, "1980s", 
                         ifelse(trump$year %in% 1990:1999, "1990s", 
                                ifelse(trump$year %in% 2000:2009, "2000s", 
                                       ifelse(trump$year %in% 2010:2019, "2010s",
                                              ifelse(trump$year %in% 1960:1969, "1960s",
                                                     ifelse(trump$year %in% 1950:1959, "1950s",
                                                            "NA"))))))))

trump <- trump %>%
  mutate(charted = 
           ifelse(trump$track.popularity %in% 50:100, "Popular", "Unpopular"))

#chart on released songs per decade
trump %>%
  filter(decade != "NA") %>%
  group_by(decade, genre) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = genre), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Release Dates of Donald Trump's Campaign Music") +
  labs(x = "Decade", y = "Song Count")

trump %>%
  filter(decade != "NA") %>%
  group_by(decade) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs), stat = "identity", fill = my_colors[5])  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Release Dates of Donald Trump's Campaign Music") +
  labs(x = "Decade", y = "Song Count")

trump_words_filtered <- trump %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

trump_words_filtered %>% 
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

trump_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = 'red') +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Trump Lyrics") +
  coord_flip()

trump_words_counts <- trump_words_filtered %>%
  count(word, sort = TRUE) 

wordcloud2(trump_words_counts[1:300, ], size = .5)
