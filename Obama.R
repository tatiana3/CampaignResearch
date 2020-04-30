obama$lyrics <- sapply(obama$lyrics, fix.contractions)
obama$lyrics <- sapply(obama$lyrics, removeSpecialChars)
obama$lyrics <- sapply(obama$lyrics, tolower)

obama$year <- NA
obama$decade <- NA
obama$year <- as.numeric(substr(obama$track.album.release_date, 1, 4))

obama <- obama %>%
  mutate(decade = 
           ifelse(obama$year %in% 1970:1979, "1970s",
                  ifelse(obama$year %in% 1980:1989, "1980s", 
                         ifelse(obama$year %in% 1990:1999, "1990s", 
                                ifelse(obama$year %in% 2000:2009, "2000s", 
                                       ifelse(obama$year %in% 2010:2019, "2010s",
                                              ifelse(obama$year %in% 1960:1969, "1960s",
                                                     ifelse(obama$year %in% 1950:1959, "1950s",
                                                            "NA"))))))))

obama <- obama %>%
  mutate(charted = 
           ifelse(obama$track.popularity %in% 50:100, "Popular", "Unpopular"))

#chart on released songs per decade
obama %>%
  filter(decade != "NA") %>%
  group_by(decade, genre) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = genre), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Release Dates of Barack Obama's Campaign Music") +
  labs(x = "Decade", y = "Song Count")

obama %>%
  filter(decade != "NA") %>%
  group_by(decade) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs), stat = "identity", fill = my_colors[4])  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Release Dates of Barack Obama's Campaign Music") +
  labs(x = "Decade", y = "Song Count")

obama_words_filtered <- obama %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

obama_words_filtered %>% 
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

obama_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[3]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Barack Obama's Lyrics") +
  coord_flip()

obama_words_counts <- obama_words_filtered %>%
  count(word, sort = TRUE) 

wordcloud2(obama_words_counts[1:300, ], size = .5)
