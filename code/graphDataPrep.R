## Parse album rank scrape into separate csvs for graph


# LOAD LIBRARIES & DATA ---------------------------------------------------

library(tidyverse)
load("./album_ranks_2015_2018.RData")

colnames(lp.ranks) <- gsub('\\.','_',names(lp.ranks))
lp.ranks <- lp.ranks %>% mutate(album_id = 1:n())


# CREATE NODES ------------------------------------------------------------


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
         n_user_reviews)

# genres - ZM:CLEAN - this should be based on genre tag scrape, not lp.ranks
genre <- lp.ranks %>%
  select(genre) %>%
  distinct() %>%
  mutate(genre_id = 1:n())


# CREATE RELATIONSHIPS ----------------------------------------------------

# artist-album mapping
artAlbumMap <- lp.ranks %>% 
  select(artist, album_id) %>%
  left_join(art, by = 'artist') %>%
  select(album_id, artist_id)



# join on album ID ZM:CLEAN: final is output of genreScrape.R
tagsPrep <- final %>%
  select(artist,album, tag1, tag2, tag3, tag4, tag5, tag6, tag7) %>%
  left_join(lp.ranks %>% 
              select(artist,album,album_id), by = c('artist','album')) %>%
  mutate(tag1 = ifelse(tag1 == ' Add Tag', "None", tag1))

tagsPrep <- data.table(tagsPrep)

# convert wide to long
tagsComboLong <- melt(tagsPrep, id.vars = c('album_id'),
                      measure.vars = c('tag1', 'tag2', 'tag3', 'tag4', 'tag5', 'tag6', 'tag7')) %>%
  select(-variable, album_id, genre = value)


# filter out genres that aren't genres
tagCombosClean <- tagCombosRaw %>% filter(complete.cases(.),
                                          genre != ' Add Tag',
                                          genre != ' More' )

# filter out record labels that are genres
recordIndex <- grep("record", tagCombosClean1$genre)


genreAlbumMap <- tagCombosClean1[-recordIndex,]

# replace genre with genre ID 
placeholder code


# WRITE OUT DATA FOR GRAPH ------------------------------------------------
write.csv(art, './graphData/artists.csv')
write.csv(album, './graphData/albums.csv')
write.csv(genre, './graphData/genre.csv')

write.csv(artAlbumMap,'./graphData/artAlbumMap.csv')
write.csv(genreAlbumMap,'./graphData/genreAlbumMap.csv')

