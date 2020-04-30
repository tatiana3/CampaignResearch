romney$lyrics <- sapply(romney$lyrics, fix.contractions)
romney$lyrics <- sapply(romney$lyrics, removeSpecialChars)
romney$lyrics <- sapply(romney$lyrics, tolower)

romney$year <- NA
romney$decade <- NA
romney$year <- as.numeric(substr(romney$track.album.release_date, 1, 4))

romney <- romney %>%
  mutate(decade = 
           ifelse(romney$year %in% 1970:1979, "1970s",
                  ifelse(romney$year %in% 1980:1989, "1980s", 
                         ifelse(romney$year %in% 1990:1999, "1990s", 
                                ifelse(romney$year %in% 2000:2009, "2000s", 
                                       ifelse(romney$year %in% 2010:2019, "2010s",
                                              ifelse(romney$year %in% 1960:1969, "1960s",
                                                     ifelse(romney$year %in% 1950:1959, "1950s",
                                                            "NA"))))))))

romney <- romney %>%
  mutate(charted = 
           ifelse(romney$track.popularity %in% 50:100, "Popular", "Unpopular"))

#chart on released songs per decade
romney %>%
  filter(decade != "NA") %>%
  group_by(decade, genre) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = genre), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Release Dates of Mitt Romney's Campaign Music") +
  labs(x = "Decade", y = "Song Count")

romney %>%
  filter(decade != "NA") %>%
  group_by(decade) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs), stat = "identity", fill = my_colors[1])  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Release Dates of Mitt Romney's Campaign Music") +
  labs(x = "Decade", y = "Song Count")

romney_words_filtered <- romney %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

romney_words_filtered %>% 
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

romney_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[5]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Mitt Romney's Lyrics") +
  coord_flip()

romney_words_counts <- romney_words_filtered %>%
  count(word, sort = TRUE) 

wordcloud2(romney_words_counts[1:300, ], size = .5)
