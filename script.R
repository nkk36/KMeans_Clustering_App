# Setup ################################

# Load packages
library(clValid)
library(corrplot)
library(data.table)
library(DBI)
library(dbplyr)
library(dplyr)
library(fossil)
library(ggplot2)
library(knitr)
library(magrittr)
library(Matrix)
library(networkD3)
library(plotly)
library(RColorBrewer)
library(RMySQL)

Load functions
source("R/Transform_to_Percentage.R")
source("R/Transform_by_Capping.R")
source("R/Min_Max_Scaling.R")
source("R/Transform_Negative_Values.R")

# Load data
con = dbConnect(drv = MySQL(),
                db = "kmeans_2D_naics",
                host = Sys.getenv("Host"),
                user = Sys.getenv("username"),
                password = Sys.getenv("password"))
# df_original = con %>% 
#   tbl("indices_values_dobl_2D_naics") %>%
#   collect()
companies  = con %>% 
  tbl("indices_values_duns_vendornames_dobl_2D_naics") %>%
  collect()
naics_2D  = con %>% 
  tbl("indices_values_naics_2D_Desc_Code") %>%
  collect()
load("cluster_kmeans.RData")


# # Pre-Process ################################
# 
# # Create copy of original data set
# df = df_original
# 
# # Step 1: Make all values positive
# df = Transform_Negative_Values(df, 
#                                type = "absolute value")
# # Step 2: Divide each value by the sum of numbers in its row
# df = Transform_to_Percentage(df)
# # Step 3: Set values below 0.1 to zero
# df = Transform_by_Capping(df = df, 
#                           lower_bound = 0.1, 
#                           upper_bound = 0.9, 
#                           type = "Below")
# 
# # Step 4: Convert data to a data frame
# matrix = sparseMatrix(i = df$row, j = df$col, x = df$values)
# matrix = as.matrix(matrix)
# df_for_cp = as.data.frame(matrix)
# 
# remove(matrix, df)
# 
# # Clustering ################################
# 
# Clusters = 2:50
# Cluster_KMeans = list()
# 
# for (i in Clusters) {
#   # Set seed
#   set.seed(123)
#   
#   # Perform K-Means clustering with 13 groups
#   temp_kmeans = kmeans(x = df_for_cp, centers = i, nstart=50, iter.max = 15)
#   Cluster_KMeans[[i - 1]] = temp_kmeans
# }
# 
# remove(temp_kmeans)
# 
# # Write CSVs ################################
# i = 2
# for (element in Cluster_KMeans){
#   temp = data.frame(cluster = element$cluster)
#   write.csv(temp, paste("data/clusters/cluster", as.character(i), ".csv",sep = ""), row.names = FALSE)
#   i = i + 1
# }
# 
# i = 2
# for (element in Cluster_KMeans){
#   temp = data.frame(cluster = element$centers)
#   write.csv(temp, paste("data/clusters/centers", as.character(i), ".csv",sep = ""), row.names = FALSE)
#   i = i + 1
# }
# 
# for (i in 2:50){
#   temp = read.csv(paste("data/clusters/centers", as.character(i), ".csv", sep = ""))
#   colnames(temp) = c("V1","V2", "V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15", 
#                      "V16","V17","V18","V19","V20","V21","V22","V23","V24")
#   write.csv(temp, paste("data/clusters/centers", as.character(i), ".csv",sep = ""), row.names = FALSE)
# }
# # Write to database ################################
# for (i in 2:50){
#   c = read.csv(paste("data/clusters/cluster", as.character(i), ".csv", sep = ""))
#   center = read.csv(paste("data/clusters/centers", as.character(i), ".csv", sep = ""))
#   dbWriteTable(conn = con, name = paste("cluster", as.character(i), sep = ""), value = c)
#   dbWriteTable(conn = con, name = paste("center", as.character(i), sep = ""), value = center)
#   }

# Visualize ################################

# Between WSS/Total WSS
t = data.frame(wss = sapply(X = Cluster_KMeans, 
           FUN = function(x){
             
             sum(x$betweenss)/x$totss*100
             
           }))
t$clusters = 2:50

g_wss = ggplot(data = t) +
  geom_point(mapping = aes(x = clusters, y = wss)) + 
  geom_line(mapping = aes(x = clusters, y = wss)) + 
  labs(title = "Variance Explained using K-Means",
       subtitle = "Number of clusters from 2 - 50",
       x = "Number of Clusters",
       y = "Percentage of Variance Explained")

p_wss = plot_ly(t, 
        x = ~clusters, 
        y = ~wss, 
        type = "scatter", 
        mode = "lines+markers+text",
        hoverinfo = "x+y") %>%
  layout(title = "Variance Explained using K-Means",
         xaxis = list(title = "Number of Clusters"),
         yaxis = list (title = "Percentage of Variance Explained"),
         hovermode = "x")

# Centers Heatmap

t2 = sapply(X = Cluster_KMeans, 
            FUN = function(x){
              
              x$centers
              
              })

vals <- unique(scales::rescale(c(t2[[1]])))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Reds", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)



plot_ly(x = 1:2,
        text = ~matrix(rep(paste("Major NAICS Category: ", naics_2D$Sector, sep = ""), 49), ncol = 49, nrow = 24),
        z = t(t2[[1]]), 
        colorscale = colz,
        type = "heatmap",
        hoverinfo = "x+text+z") %>%
  layout(title = "Centers of Clusters",
         xaxis = list(title = "Clusters"),
         yaxis = list (title = "Major NAICS Code", showticklabels = FALSE, ticks = ""),
         margin = list(
           r = 10, 
           t = 25, 
           b = 40, 
           l = 50
         ))