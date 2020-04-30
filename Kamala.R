kamala$lyrics <- sapply(kamala$lyrics, fix.contractions)
kamala$lyrics <- sapply(kamala$lyrics, removeSpecialChars)
kamala$lyrics <- sapply(kamala$lyrics, tolower)

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

#chart on released songs per decade
kamala %>%
  filter(decade != "NA") %>%
  group_by(decade, race) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = race), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Release Dates of Kamala Harris' Campaign Music") +
  labs(x = "Decade", y = "Song Count")

kamala %>%
  filter(decade != "NA") %>%
  group_by(decade) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(position = 'fill', aes(x = decade, y = number_of_songs), stat = "identity", fill = 'salmon')  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Release Dates of Kamala Harris' Campaign Music") +
  labs(x = "Decade", y = "Song Count")

kamala_words_filtered <- kamala %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

kamala_words_filtered %>% 
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

kamala_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Kamala Lyrics") +
  coord_flip()

kamala_words_counts <- kamala_words_filtered %>%
  count(word, sort = TRUE) 

wordcloud2(kamala_words_counts[1:300, ], size = .5)
