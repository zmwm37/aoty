## Parse album rank scrape into separate csvs for graph


# LOAD LIBRARIES & DATA ---------------------------------------------------

library(tidyverse)
load("./album_ranks_2015_2018.RData")

colnames(lp.ranks) <- gsub('\\.','_',names(lp.ranks))
lp.ranks <- lp.ranks %>% mutate(album_id = 1:n())

# artists
art <- lp.ranks %>% 
  select(artist) %>% 
  distinct() %>%
  mutate(artist_id = 1:n())

# albums 
album <- lp.ranks %>% 
  select(album_id,
         album,
         date,
         critic_rank,
         user_rank,
         critic_score,
         user_score,
         n_critic_reviews = n_reviews,
         n_user_reviews) %>%
  mutate(album_id = 1:n())

# genres
genre <- lp.ranks %>%
  select(genre) %>%
  distinct() %>%
  mutate(genre_id = 1:n())

# artist-album mapping
artAlbumMap <- lp.ranks %>% 
  select(artist, album_id) %>%
  left_join(art, by = 'artist') %>%
  select(album_id, artist_id)


# album-genre mapping
genreAlbumMap <- lp.ranks %>% 
  select(genre, album_id) %>%
  left_join(genre, by = 'genre') %>%
  select(album_id, genre_id)



# WRITE OUT DATA FOR GRAPH ------------------------------------------------
write.csv(art, './graphData/artists.csv')
write.csv(album, './graphData/albums.csv')
write.csv(genre, './graphData/genre.csv')

write.csv(artAlbumMap,'./graphData/artAlbumMap.csv')
write.csv(genreAlbumMap,'./graphData/genreAlbumMap.csv')

