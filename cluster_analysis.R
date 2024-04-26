#sample code from: https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/

rm(list=ls())

library(dplyr)
library(tidyverse)
library(factoextra)
library(ggthemes)
library(RColorBrewer)


#Scaling the data (code from here: https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r)
NTD_scaled <- NTD_cluster %>% 
  mutate_at(c("UPT", "VRH", "OPEXP_TOTAL"), ~(scale(.) %>% 
                                                as.vector))

#Identifying number of clusters

set.seed(123)

km.out <- kmeans(NTD_scaled, centers = 20, nstart = 20)
km.out

#### Scree Plot, from MNR / PBN ####

# First, identify the number of clusters (k)
# "Elbow" method
mydata <- NTD_scaled
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within cluster sum of squares",
     main="Assessing the Optimal Number of Clusters",
     pch=20, cex=2)
abline(v=4, lty="dashed", lwd=2, col="red")

### 4 clusters are best
k <- 20
set.seed(123)
# Build model with k clusters: km.out
km.out <- kmeans(NTD_scaled, centers = k, nstart = 20)

##Assigning Cluster IDs
NTD_scaled$cluster_id <- factor(km.out$cluster)

#Rebuilding NTD scaled to join identifiers back in

NTD_cluster2 <- NTD.ts %>% 
  filter(Year == 2019 & Agency.Status == "Active" &
           Reporter.Type == "Full Reporter"& Service != "PO"&
           Mode == "MB"| 
           Mode == "RB" | 
           Mode =="CB"| 
           Mode == "TB"&
           Service != "PO") %>% 
  group_by(Agency.Name) %>% 
  summarize(across(c(UPT, VRH, OPEXP_TOTAL),sum)) %>% 
  filter(UPT & VRH & OPEXP_TOTAL != 0)

NTD_scaled2 <- NTD_cluster2 %>% 
  mutate_at(c("UPT", "VRH", "OPEXP_TOTAL"), ~(scale(.) %>% 
                                                as.vector))

NTD_scaled <- NTD_scaled %>% 
  left_join(NTD_scaled2)

library(viridis)

ggplot(NTD_scaled,
       aes(VRH, UPT, color = cluster_id))+ 
  geom_point()+ 
  labs(y = "Scaled unliked passenger trips", 
       x = "Scaled vehicle revenue miles", 
       title = "Cluster IDs by UPT and VRM")+
  scale_x_log10(limits = c(0.08, 11))+ 
  scale_y_log10()+ 
  scale_color_viridis(discrete = TRUE, option = "viridis", guide = guide_legend(title = "Cluster ID", label = list(size = 1)))


ggsave("cluster_plot.png", height = 6, width = 7)



#Making an actually useful file of the clusters

NTD_agency <- NTD.ts %>% 
  select(c(NTD.ID, Agency.Name))
NTD_agency <- NTD_agency[!duplicated(NTD_agency$NTD.ID), ]



clusters_no_scale <- NTD_clusters %>% 
  select(Agency.Name, cluster_id) %>% 
  left_join(NTD_cluster2) %>% 
  mutate(UPT = UPT/1000, 
         VRH = VRH / 1000, 
         cluster_id = as.numeric(cluster_id)) 

options(scipen=10000)


ggplot(clusters_no_scale, 
       aes( x= VRH, y= UPT, col = cluster_id))+ 
         geom_point(size = .75)+ 
  scale_x_log10()+ 
  scale_y_log10()+ 
  labs( x= "1000s of Vehicle Revenue Hours", 
        y= "1000s of Unlinked Passenger Trips")+ 
  theme_clean()+ 
  scale_color_viridis_b()



table(NTD_clusters$cluster_id)

write.csv(NTD_clusters, "C:\\Users\\Jonathan\\OneDrive - PennO365\\F23\\Thesis\\R\\NTD_9_2023\\Data\\NTD_clusters.csv")

