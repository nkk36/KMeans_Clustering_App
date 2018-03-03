library(dbplyr)
library(dplyr)
library(DT)
library(Matrix)
library(networkD3)
library(plotly)
library(reshape2)
library(shiny)
source("helpers.R")
load("cluster_kmeans.RData")

# # Connect with database
# con = Establish_Connection()

# Define server logic 
shinyServer(function(input, output) {
  
  output$clustering_data = renderDataTable({
    
    df = Get_Data_for_Clustering(connection = con)
    
    # Step 1: Make all values positive
    df = Transform_Negative_Values(df,
                                   type = "absolute value")
    # Step 2: Divide each value by the sum of numbers in its row
    df = Transform_to_Percentage(df)
    # Step 3: Set values below 0.1 to zero
    df = Transform_by_Capping(df = df,
                              lower_bound = 0.1,
                              upper_bound = 0.9,
                              type = "Below")
    
    # Step 4: Convert data to a data frame
    matrix = sparseMatrix(i = df$row, j = df$col, x = df$values)
    matrix = as.matrix(matrix)
    df = as.data.frame(matrix)
    
    companies = Get_Company_Crosswalk(connection = con)
    
    df = cbind.data.frame(`Vendor Name` = companies$vendorname, df)
    
    df = datatable(data = df, 
                   caption = "Data used for clustering. Scroll horizontally along the table to see all the columns. 
                   Each column corresponds to one major NAICS category.",
                   filter = "top",
                   options = list(pageLength = 10,
                                  scrollX = TRUE)
    )
    
    remove(matrix)
    return(df)
    
  })
  
  output$naics_description = renderDataTable({
  
    naics = Get_NAICS_Data(connection = con)
    colnames(naics)[1:3] = c("Major NAICS Category", "Column Code")
    naics = datatable(data = naics[,c(1,3)],
                      caption = "Use this table to match columns with NAICS categories.",
                      options = list(pageLength = 24))
    
    return(naics)
    
    })
  
  output$data_description = renderPrint({
    
    df = Get_Data_for_Clustering(connection = con)
    
    # Step 1: Make all values positive
    df = Transform_Negative_Values(df,
                                   type = "absolute value")
    # Step 2: Divide each value by the sum of numbers in its row
    df = Transform_to_Percentage(df)
    # Step 3: Set values below 0.1 to zero
    df = Transform_by_Capping(df = df,
                              lower_bound = 0.1,
                              upper_bound = 0.9,
                              type = "Below")
    
    # Step 4: Convert data to a data frame
    matrix = sparseMatrix(i = df$row, j = df$col, x = df$values)
    matrix = as.matrix(matrix)
    df = as.data.frame(matrix)
    
    return(describe(df))
    
  })
  
  output$fit = renderPlotly({
    
    temp = data.frame(wss = sapply(X = Cluster_KMeans, 
                                   FUN = function(x){
                                     sum(x$betweenss)/x$totss*100
                                     
                                     }))
    temp$clusters = 2:50
    
    p = plot_ly(temp, 
                x = ~clusters, 
                y = ~wss, 
                type = "scatter", 
                mode = "lines+markers+text",
                hoverinfo = "x+y") %>%
        layout(title = "Variance Explained using K-Means",
               xaxis = list(title = "Number of Clusters"),
               yaxis = list (title = "Percentage of Variance Explained"),
               hovermode = "x",
               margin = list(r = 10, 
                             t = 50, 
                             b = 40, 
                             l = 50))
    
    return(p)
    
  })
  
  output$heatmap = renderPlotly({
    
    heatmap = sapply(X = Cluster_KMeans, 
                     FUN = function(x){
                       
                       x$centers
                       
                       })
    
    num_cluster = as.numeric(input$num_cluster)
    
    vals = unique(scales::rescale(c(heatmap[[num_cluster - 1]])))
    o = order(vals, decreasing = FALSE)
    cols = scales::col_numeric("Reds", domain = NULL)(vals)
    colz = setNames(data.frame(vals[o], cols[o]), NULL)
    
    
    
    p = plot_ly(x = 1:num_cluster,
                text = ~matrix(rep(paste("Major NAICS Category: ", 
                                         naics_2D$Sector, sep = ""), 
                                   num_cluster), 
                               ncol = num_cluster, 
                               nrow = 24),
                z = t(heatmap[[num_cluster - 1]]), 
                colorscale = colz,
                type = "heatmap",
                hoverinfo = "x+text+z") %>%
      layout(title = "Cluster Centers",
             xaxis = list(title = "Clusters", dtick = 1),
             yaxis = list (title = "Major NAICS Code", 
                           showticklabels = FALSE, 
                           ticks = ""),
             margin = list(r = 10, 
                           t = 50, 
                           b = 40, 
                           l = 50)
             )
    
    return(p)
  })
  
  output$pie = renderPlotly({
    
    num_cluster = as.numeric(input$num_cluster)
    naics = Get_NAICS_Data(connection = con)
    
    cluster_companies = data.frame(cluster = Cluster_KMeans[[num_cluster - 1]]$cluster)
    
    temp = cluster_companies %>%
      group_by(cluster) %>%
      summarise(n = n())
    
    cluster_categories = Get_Major_Cluster_Categories(Cluster_KMeans[[num_cluster - 1]]$centers, naics)
    temp$cluster = as.character(temp$cluster)
    cluster_categories$cluster = as.character(cluster_categories$cluster)
    
    temp = left_join(x = temp, y = cluster_categories, by = "cluster")
    
    p <- plot_ly(data = temp, 
                 labels = ~cluster, 
                 values = ~n,
                 text = ~Sector,
                 textposition = "outside",
                 type = 'pie') %>%
      layout(title = "Relative Size of each Cluster",
             xaxis = list(showgrid = FALSE, 
                          zeroline = FALSE, 
                          showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, 
                          zeroline = FALSE, 
                          showticklabels = FALSE),
             margin = list(r = 50, 
                           t = 50, 
                           b = 50, 
                           l = 50))
    
    return(p)
    
  })
  
  output$companies_cluster_table = renderDataTable({
    
    
    num_cluster = as.numeric(input$num_cluster)
    
    companies = Get_Company_Crosswalk(connection = con)
    cluster_categories = Get_Major_Cluster_Categories(Cluster_KMeans[[num_cluster - 1]]$centers, naics)
    
    
    num_cluster = as.numeric(input$num_cluster)
    cluster_companies = data.frame(Company = companies$vendorname,
                                   Cluster = Cluster_KMeans[[num_cluster - 1]]$cluster)
    
    cluster_companies$Cluster = as.character(cluster_companies$Cluster)
    cluster_categories$cluster = as.character(cluster_categories$cluster)
    
    cluster_companies = left_join(x = cluster_companies, 
                                  y = cluster_categories,
                                  by = c("Cluster" = "cluster"))

    
    cluster_companies = datatable(data = cluster_companies,
                                  caption = "Companies and the cluster they belong to.",
                                  filter = "top",
                                  options = list(pageLength = 20))
    
  })
  
})
