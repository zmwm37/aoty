## Overview
# This script is designed to scrape all the genre tags for each album. 
# The output is then fed into graphDataPrep.R


## Libraries
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(igraph)
library(visNetwork)

## Functions
source('./functions/cleanCriticScore.R')

# SCRAPE GENRES -----------------------------------------------------------
# enter page range and year range
pages <- 1
years <- c(2018)


# loop for each year
for (y in years){
    tagsList <- list()
    albumList<- list()
    
    # loop of urls for each page of 25 albums 
    for(i in pages){
        #make vector of album page urls
        url <- paste0("https://www.albumoftheyear.org/ratings/6-highest-rated/",y,"/",i)
        
        html <- read_html(url)
        
        # store artist, album, and critic data 
        albumList[[i]] <- html %>% html_nodes('.albumListRow')%>% 
            html_text () %>%
            data.frame()
        
        # store urls for tags loops
        album.urls <- html %>% 
            html_nodes(".albumListTitle a") %>% 
            html_attr('href')
        
        # store album titles/artist for linking
        album.titles <- html %>%
            html_nodes(".albumListTitle a") %>%
            html_text()
        
        #scrape genre from each url
        list <- list()
        z<-1
        for(u in album.urls){
            url.a <- paste("https://www.albumoftheyear.org",u,sep = "")
            html <- read_html(url.a)
            
            # genre tags
            genreTags <- html %>% 
                html_nodes(".tag") %>% 
                html_text()
            
            # album name 
            albumArtist <- album.titles[z]
            
            list[[z]] <- c(albumArtist, genreTags)
            z<- z+1
        }
        
        # combine vectors into a dataframe
        tags<-as.data.frame(
            t(
                as.matrix(sapply(list, '[', seq(max(sapply(list, length))))
                )
            )
        )
        
        # set parameters for id, used to sort tags after union
        a <- ((i-1)*25)+1
        b <- a + dim(tags)[1]-1
        
        tags$id <- a:b
        
        tagsList[[i]] <- tags 
    }
    
    
    # set what names should be
    full.names <- c('album.info','V1','V2','V3','V4','V5','V6','V7', 'id')
    
    # separate into lists of proper length and too short
    tagsListGood <- tagsList[sapply(tagsList,function(x) ncol(x) ==length(full.names))]
    
    tagsListBad <- tagsList[sapply(tagsList,function(x) ncol(x) != length(full.names))]
    
    
    
    # add missing columns to bad tag lists
    redeemed.list <- list()
    if(length(tagsListBad >0)){
        
        for(b in length(tagsListBad)){
            
            df <- tagsListBad[b][[1]]
            
            # identify missing columns
            missing.names <- setdiff(full.names,names(df))
            
            # create missing variables
            missingVars <-data.frame(
                matrix(
                    rep(NA,
                        dim(df)[1]*length(missing.names)
                    ),
                    dim(df)[1]
                )
            )
            
            colnames(missingVars) <- missing.names
            
            dfFull <- cbind(df,missingVars)
            
            redeemed.list[[b]] <-dfFull
        }
        
    }
    
    # make into dataframes
    tagsDfGood <- do.call(rbind,tagsListGood)
    tagsDfRedeem <- do.call(rbind, redeemed.list)
    
    # combine tag dataframes
    tagsDfFull <- rbind(tagsDfGood,tagsDfRedeem) %>%
        arrange(id)
    
    # combine album dataframes
    albumDfRaw <- do.call(rbind, albumList)
    
    # clean album dataframes
    albumDfClean <- cleanCriticScore(albumDfRaw)
    
    # rename columns
    colnames(tagsDfFull) = c('album.info', "tag1","tag2","tag3","tag4",
                             "tag5","tag6","tag7", 'id')
    
    # combine album info with genre tags
    final <- cbind(albumDfClean,tagsDfFull) %>% select (-id)
    # saveout dataframe
    # saveRDS(tagsDfFull,paste0('aotyGenreTags',y,'.Rds'))
}


