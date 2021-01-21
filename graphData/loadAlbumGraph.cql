// ---- NODES ----
    
// Albums
LOAD CSV WITH HEADERS FROM "file:///albums.csv" as row
MERGE (n:Album {albumID: row.album_id })
SET n.albumName = row.album,
    n.releaseDate = row.date,
    n.criticRank = row.critic_rank,
    n.userRank = row.user_rank,
    n.criticScore = row.critic_score,
    n.userScore = row.user_score,
    n.numCriticReviews= row.n_critic_reviews,
    n.numUserReviews = row.n_user_reviews
RETURN n;

// Genres
LOAD CSV WITH HEADERS FROM "file:///genre.csv" as row
MERGE(n:Genre {genreID: row.genre_id})
SET n.genre = row.genre
RETURN n;

// Artists
LOAD CSV WITH HEADERS FROM "file:///artists.csv" as row
MERGE(n:Artist {artistID: row.artist_id})
SET n.artist = row.artist
RETURN n;

// ---- RELATIONSHIPS ----
    
// (Album) -[TAGGED_IN]-> (Genre)
LOAD CSV WITH HEADERS FROM "file:///genreAlbumMap.csv" as row
MERGE (a:Album {albumID: row.album_id})
MERGE (g:Genre {genreID: row.genre_id})
MERGE (a)-[:TAGGED_AS]->(g)
RETURN a, g;

// (Artist)-[CREATED]-> (Album)
LOAD CSV WITH HEADERS FROM "file:///artAlbumMap.csv" as row
MERGE (a:Album {albumID: row.album_id})
MERGE (r:Artist {artistID: row.artist_id})
MERGE (r)-[:CREATED]->(a)
RETURN r,a;