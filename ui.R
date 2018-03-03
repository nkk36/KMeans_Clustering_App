library(magrittr)
library(shiny)
library(shinycssloaders)

shinyUI(navbarPage(
  title = "Clustering with Major NAICS Categories",
  windowTitle = "Clustering with Major NAICS Categories",
  #theme = "theme.css",
  
  tabPanel(title = "Home",
           box(
           fluidRow(
             column(width = 10,
                    offset = 6,
                    includeMarkdown("Home.md")
                    )
             )
           )
           ),
  tabPanel(title = "Data",
           fluidRow(
             column(width = 12, 
                    offset = 1,
                    tabBox(width = 10,
                           title = "Data",
                           tabPanel(
                             dataTableOutput(outputId = "naics_description"), 
                             title = "NAICS Categories"),
                           tabPanel(
                             dataTableOutput(outputId = "clustering_data") %>% 
                               withSpinner(type = 5),
                             title = "Clustering Data"),
                           tabPanel(
                             verbatimTextOutput(outputId = "data_description") %>% 
                               withSpinner(type = 5),
                             title = "Clustering Data Description")
                            )
                    )
             )
           ),
  tabPanel(title = "Clusters",
           sidebarLayout(
             sidebarPanel(width = 2,
               selectInput(inputId = "num_cluster", 
                           label = "Choose the number of clusters:", 
                           choices = c(2:50))
               ),
             mainPanel(
               box(width = 12,
                   title = "Clustering Fit",
                   plotlyOutput(outputId = "fit") %>% withSpinner(type = 5)),
                   helpText("This graph shows the ratio of the between-cluster sum of squares and the total sum of squares for different choices for the number of clusters (2-50). The choice for the number of clusters lie along the x-axis and the ratio indicating fit lies along the y-axis. The \"best\" fit is usually chosen to be the point along the x-axis where the marginal gain in variance explained begins to drop."), 
               tabBox(width = 12,
                      title = "Description of Clusters",
                 tabPanel(plotlyOutput(outputId = "heatmap") %>% withSpinner(type = 5), 
                          helpText("This heatmap shows the center point of each cluster. Clusters lie along the x-axis and the NAICS categories lie along the y-axis. Scroll over the heatmap to see the average percentage of revenue generated in each category for each cluster."), 
                          title = "Centers"),
                 tabPanel(plotlyOutput(outputId = "pie") %>% withSpinner(type = 5), 
                          helpText("This pie chart shows the relative size of each cluster and the most significant NAICS category of the cluster. The most significant category was determined by finding the maximum element of each of the cluster centers."),
                          title = "Cluster Sizes & Primary Category")
               ),
               box(width = 12,
                   collapsible = TRUE,
                   title = "Company-Cluster Table",
                   dataTableOutput(outputId = "companies_cluster_table") %>% withSpinner(type = 5))
             ) 
           )),
  tabPanel(title = "About",
           box(
             fluidRow(
               column(width = 10,
                      offset = 6,
                      includeMarkdown("About.md")
               )
             )
           )
  )
  
))