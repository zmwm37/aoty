## Function to take scraped album data and parse into fields


cleanCriticScore <- function(df){
    df<- albumDfRaw
lp.ranks <- df %>% 
    separate(col = 1, 
             into = c("c.rank.artist.album",
                      "blank",
                      "date.org",
                      "genre",
                      "critic.score.org",
                      "n.reviews.org",
                      "stores"),
             sep = "\\\n")

#adjust data for albums with no genre
index <- is.na(lp.ranks$stores) == T
lp.ranks[index,]$stores <-lp.ranks[index,]$n.reviews.org
lp.ranks[index,]$n.reviews.org<-lp.ranks[index,]$critic.score.org
lp.ranks[index,]$critic.score.org<-lp.ranks[index,]$genre
if(sum(index) >0 ){
lp.ranks[index,]$genre <-"None"
}

#parse out rank, artist and album
lp.ranks$critic.rank <- as.numeric(word(lp.ranks$c.rank.artist.album,sep = "\\."))
lp.ranks$artist <- sub("[0-9]*.\\. *(.*?) *\\-.*", "\\1", lp.ranks$c.rank.artist.album)
lp.ranks$album <- sub(".*\\-","\\1", lp.ranks$c.rank.artist.album)

#clean date
lp.ranks$date<-as.Date(lp.ranks$date.org,"%B %d,%Y")

#extract critc score and number of reviews
lp.ranks$critic.score <- as.numeric(gsub(".*(.[0-9]+)","\\1",lp.ranks$critic.score.org))
lp.ranks$n.reviews <- as.numeric(gsub("([0-9]+).*$","\\1",lp.ranks$n.reviews.org))

#flags for stores
lp.ranks$amazon <- ifelse(grepl("Amazon",lp.ranks$stores) == 1,1,0)
lp.ranks$iTunesMusic <- ifelse(grepl("iTunesMusic",lp.ranks$stores) == 1,1,0)
lp.ranks$spotify <- ifelse(grepl("Spotify",lp.ranks$stores) == 1,1,0)

lp.ranks.c <- lp.ranks %>%
    select(-c.rank.artist.album,
           -blank,
           -date.org,
           -n.reviews.org,
           -critic.score.org,-stores)
}