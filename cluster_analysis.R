#sample code from: https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/

rm(list=ls())

library(tidyverse)
library(tidycensus)
library(factoextra)
library(ggthemes)
library(RColorBrewer)
library(tmap)
library(tigris)

#### Chattanooga 

Chatt_census <- read.csv("Chatt_census12.csv")


#Scaling the data (code from here: https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r)
Chatt_scaled <- Chatt_census %>% 
  mutate(PubSNAP_rate = PubSNAP/TotalPop, 
         RenterOcc_rate = RenterOcc/TotalPop, 
         WhitePop_rate = WhitePop / TotalPop, 
         Grad_deg_rate = Grad_deg/TotalPop, 
         PubTrans_commute_ratio = PubTrans_commute/Drive_commute, 
         Citizen_Ratio = NotUS_cit/US_cit) %>% 
  select(c(GEOID,MedHHInc, MedHomeVal, MedHUBuilt, PubSNAP_rate, RenterOcc_rate,
           WhitePop_rate, Grad_deg_rate, PubTrans_commute_ratio, Citizen_Ratio)) %>% 
  mutate_at(c(2:10), ~(scale(.) %>% 
                         as.vector)) %>% 
  na.omit()
  

#Identifying number of clusters

set.seed(123)

km.out <- kmeans(Chatt_scaled, centers = 20, nstart = 20)
km.out

#### Scree Plot, from MNR / PBN ####

# First, identify the number of clusters (k)
# "Elbow" method
mydata <- Chatt_scaled
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within cluster sum of squares",
     main="Assessing the Optimal Number of Clusters",
     pch=20, cex=2)
abline(v=4, lty="dashed", lwd=2, col="red")

### 4 clusters are best
k <- 4
set.seed(123)
# Build model with k clusters: km.out
km.out <- kmeans(Chatt_scaled, centers = k, nstart = 20)

##Assigning Cluster IDs
Chatt_scaled$cluster_id <- factor(km.out$cluster)

ggplot(Chatt_scaled, 
       aes(x= MedHHInc, y= WhitePop_rate, color = cluster_id))+ 
  geom_point()

chatt_join <- Chatt_scaled %>% 
  select(c(GEOID, cluster_id)) %>% 
  write.csv("chatt_cluster_ids.csv")


Chatt_census12 <- 
  get_acs(geography = "tract", 
          variables = c("B25026_001E"),   year=2012, state=47, county=065, 
          geometry=TRUE, output="wide"
          ) %>% 
  mutate(GEOID = as.numeric(GEOID))

Chatt_census_clusters <- Chatt_census12 %>% 
  left_join(chatt_join) 

tm_shape(Chatt_census12)+ 
  tm_polygons()

#### Nashville 

Nash_census <- read.csv("Nash_census12.csv")


#Scaling the data (code from here: https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r)
Nash_scaled <- Nash_census %>% 
  mutate(PubSNAP_rate = PubSNAP/TotalPop, 
         RenterOcc_rate = RenterOcc/TotalPop, 
         WhitePop_rate = WhitePop / TotalPop, 
         Grad_deg_rate = Grad_deg/TotalPop, 
         PubTrans_commute_ratio = PubTrans_commute/Drive_commute, 
         Citizen_Ratio = NotUS_cit/US_cit) %>% 
  select(c(GEOID,MedHHInc, MedHomeVal, MedHUBuilt, PubSNAP_rate, RenterOcc_rate,
           WhitePop_rate, Grad_deg_rate, PubTrans_commute_ratio, Citizen_Ratio)) %>% 
  na.omit() %>% 
  mutate_at(c(2:10), ~(scale(.) %>% 
                         as.vector))


#Identifying number of clusters

set.seed(123)

km.out <- kmeans(Nash_scaled, centers = 20, nstart = 20)
km.out

#### Scree Plot, from MNR / PBN ####

# First, identify the number of clusters (k)
# "Elbow" method
mydata <- Nash_scaled
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within cluster sum of squares",
     main="Assessing the Optimal Number of Clusters",
     pch=20, cex=2)
abline(v=4, lty="dashed", lwd=2, col="red")

### 4 clusters are best
k <- 4
set.seed(123)
# Build model with k clusters: km.out
km.out <- kmeans(Nash_scaled, centers = k, nstart = 20)

##Assigning Cluster IDs
Nash_scaled$cluster_id <- factor(km.out$cluster)

ggplot(Nash_scaled, 
       aes(x= MedHHInc, y= WhitePop_rate, color = cluster_id))+ 
  geom_point()

nash_join <- Nash_scaled %>% 
  select(c(GEOID, cluster_id)) 
  write.csv("nash_cluster_ids.csv")


Nash_census19 <- 
  get_acs(geography = "tract", 
          variables = c("B25026_001E"), 
          year=2019, state=47, county=037, 
          geometry=TRUE, output="wide") %>% 
  mutate(GEOID = as.numeric(GEOID))

Nash_census_clusters <- Nash_census19 %>% 
  left_join(nash_join) 

tm_shape(Nash_census_clusters)+ 
  tm_polygons(fill = "cluster_id")
