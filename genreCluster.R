# LOAD LIBRARIES ----
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(visNetwork)
library(wesanderson)

# LOAD DATA ----
tags15 <- readRDS('./aotyGenreTags2015.Rds')
tags16 <- readRDS('./aotyGenreTags2016.Rds')
tags17 <- readRDS('./aotyGenreTags2017.Rds')
tags18 <- readRDS('./aotyGenreTags2018.Rds')
tags19 <- readRDS('./aotyGenreTags2019.Rds')

# TRANFORM DATA ----
outlist <- list() 

for(i in 1:nrow(tagsDfFull)){
    
    x <- as.character(unlist(tagsDfFull[i,])) 
    xCombo <- data.frame(t(combn(x,2)))
    outlist[[i]] <- xCombo
}

tagCombosRaw <- do.call(rbind,outlist)

colnames(tagCombosRaw) <- c('tag1','tag2')
tagCombosClean1 <- tagCombosRaw %>% filter(complete.cases(.),
                                           tag1 != ' Add Tag' & tag2 != ' Add Tag',
                                           tag1 != ' More' & tag2 != ' More')

recordIndex <- c(grep("record", tagCombosClean1$tag1),
                 grep("record", tagCombosClean1$tag2))


tagCombosClean <- tagCombosClean1[-recordIndex,]

# CLUSTER ----
# create clusters - Louvain
g <- graph_from_data_frame(tagCombosClean, directed=F)

clusterList <- cluster_louvain(g)

# turn cluster into DF
cl.list <- list()
for (i in 1:length(clusterList)) {
    df <- data.frame(clusterList[[i]])
    df$cluster <- paste0('c',i)
    cl.list[[i]] <- df
}

clusterDf <- do.call(rbind,cl.list)
colnames(clusterDf) <- c('label','cluster')

# add colors for labels
cluster <- unique(clusterDf$cluster)
c.group <- c('Pop','Rock','Electronic','Other','Jazz','Metal','Drop7','Drop8','Drop9','Rap/Hip-Hop')
c.color <- wes_palette('Zissou1',7, type = 'continuous')
# c.color <- rainbow(7)
# c.color <- rainbow(10)[c(1:6,10)]
clusterColors <- data.frame(cluster = cluster,group = c.group, color = c.color)
clusterColorsTemp <- data.frame(cluster = cluster[c(1:6,10)], 
                                group = c.group[c(1:6,10)], 
                                color = c.color[7:1])

# VISUALIZE ----
# visNetwork
nodes <- data.frame(
    id = unique(
        c(unique(as.character(tagCombosClean$tag1)),
          unique(as.character(tagCombosClean$tag2)))
    ),
    label = unique(
        c(unique(as.character(tagCombosClean$tag1)),
          unique(as.character(tagCombosClean$tag2)))
    )
)

# add clusters and colors onto nodes
nodes2 <- nodes %>% 
    left_join(clusterDf, by = 'label') %>%
    filter(!cluster %in% c('c7','c8','c9')) %>%
    left_join(clusterColorsTemp, by = 'cluster')
    

edges <- tagCombosClean
colnames(edges) <- c('from','to')

# plot the graph
visNetwork(nodes2,edges) %>%
    visPhysics(maxVelocity = 1) %>%
    visGroups(groupname = c.group[1], color =c.color[7]) %>%
    visGroups(groupname = c.group[2], color =c.color[6]) %>%
    visGroups(groupname = c.group[3], color =c.color[5]) %>%
    visGroups(groupname = c.group[4], color =c.color[4]) %>%
    visGroups(groupname = c.group[5], color =c.color[3]) %>%
    visGroups(groupname = c.group[6], color =c.color[2]) %>%
    visGroups(groupname = c.group[10], color =c.color[1]) %>%
    visLegend()
