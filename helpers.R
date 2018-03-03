# Setup ==================================================
# Load packages
library(DBI)
library(dbplyr)
library(RMySQL)
#########################
# Helper Functions:  ####
#########################
# Establish_Connection ==================================================
Establish_Connection = function(){
  
  connection = dbConnect(drv = MySQL(),
                  db = "kmeans_2D_naics",
                  host = Sys.getenv("Host"),
                  user = Sys.getenv("username"),
                  password = Sys.getenv("password"))
  
  return(connection)
  
}
# Get_NAICS_Data ==================================================
Get_NAICS_Data = function(connection){
  
  temp  = connection %>% 
    tbl("indices_values_naics_2D_Desc_Code") %>%
    collect()
  
  return(temp)
  
}
# Get_Company_Crosswalk ==================================================
Get_Company_Crosswalk = function(connection){
  
  temp = con %>% 
    tbl("indices_values_duns_vendornames_dobl_2D_naics") %>%
    collect()
  
  return(temp)
  
}
# Get_Data_for_Clustering ==================================================
Get_Data_for_Clustering = function(connection){
  
  temp = con %>%
    tbl("indices_values_dobl_2D_naics") %>%
    collect()

  return(temp)
  
}
# Min_Max_Scaling ==================================================
Min_Max_Scaling = function(df){
  
  # Get the minimum of each column
  col_min = df %>% 
    group_by(col) %>%
    summarise(min = min(values)) %>%
    arrange(col)
  
  # Get the maximum of each column
  col_max = df %>% 
    group_by(col) %>%
    summarise(max = max(values)) %>%
    arrange(col)
  
  # Join columns min/max with data
  df = left_join(x = df, y = col_min, by = "col")
  df = left_join(x = df, y = col_max, by = "col")
  
  # Perform min-max scaling
  df = df %>%
    mutate(values_hat = (values-min)/(max - min)) %>%
    select(row, col, values_hat)
  
  # Rename column
  colnames(df)[3] = "values"
  
  # Return
  return(df)
  
}
# Transform_by_Capping ==================================================
Transform_by_Capping = function(df, lower_bound, upper_bound, type){
  
  # Cap values above a given percentile 
  if (type == "Above"){
    quantile_upper_bound = quantile(df$values, probs = upper_bound)
    df$values[df$values > quantile_upper_bound] = quantile_upper_bound
  }
  
  # Cap values below a given percent 
  else if (type == "Below"){
    df$values[df$values < lower_bound] = 0
    df = df[df$values != 0, ]
    rownames(df) = 1:nrow(df)
  }
  
  # Return
  return(df)
  
}
# Transform_Negative_Values ==================================================
Transform_Negative_Values = function(df, type){
  
  # Make all negative values 0
  if (type == "zero"){
    # Set all negative values to 0
    df = df[df$values > 0,]
  }
  # Make all negative values positive
  else if (type == "absolute value"){
    # Make all values positive
    df$values = abs(df$values)
  }
  
  # Return
  return(df)
  
}
# Transform_to_Percentage ==================================================
Transform_to_Percentage = function(df){
  
  # Load packages
  library(dplyr)
  library(magrittr)
  
  # Calculate the sum of each row
  row_sum = df %>%
    group_by(row) %>%
    summarise(total = sum(values)) %>%
    arrange(row)
  
  # Join row sums with data and convert to percentages by each row ordering the data by row
  df = left_join(x = df, y = row_sum, by = "row")
  df = df %>%
    mutate(percentage = round(values/total,2)) %>%
    arrange(row) %>%
    select(row, col, percentage)
  
  # Rename columns
  colnames(df)[3] = "values"
  
  # Return
  return(df)
  
}
# Get_Major_Cluster_Categories ==================================================
Get_Major_Cluster_Categories = function(centers, naics){
  
  test = data.frame(centers)
  test$cluster = rownames(test)
  test2 = melt(test, id.vars = "cluster")
  test3 = test2 %>%
    group_by(cluster) %>%
    mutate(Major = variable[which.max(value)]) %>%
    ungroup %>%
    left_join(y = naics[,c(1,3)], by = c("Major" = "Column_Code")) %>%
    select(cluster, Sector) %>%
    group_by(cluster) %>%
    summarise(Sector = paste(unique(Sector), collapse = ","))
  
  return(test3)
}