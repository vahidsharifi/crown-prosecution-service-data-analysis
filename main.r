# Creating a vector of libraries we need
# Install them if you haven't installed them yet
lib = c('readr', 'hash', 'tidyverse', 'data.table', 'zoo', 'dplyr', 'ggmap', 'skimr',
        'devtools', 'visdat', 'DataExplorer', 'inspectdf', 'ggplot2', 'ggcorrplot', 'corrplot', 'reshape2',
       'factoextra', 'e1071', 'forecast', 'mice', 'randomForest', 'reshape2', 'lubridate', 'ggmap', 'GGally',
        'viridis', 'hrbrthemes', 'gganimate', 'caTools', 'caret', 'Rtsne', 'cluster', 'nnet')

#Install All the libraries and dependencies we need using the for loop
for (item in lib){
    install.packages(item)
}

library(tidyverse)
library(cluster)
library(readr)
library(ggmap)
library(GGally)
library(dplyr)
library(visdat)
library(lubridate)
library(data.table)
library(zoo)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(reshape2)
library(factoextra)
library(e1071)
library(randomForest)
library(forecast)
library(mice)
library(skimr)
library(DataExplorer)
library(inspectdf)
library(devtools)
library(glue)
library(repr)
library(viridis)
library(hrbrthemes)
library(gganimate)
library(caTools)
library(caret)
library(Rtsne)
library(nnet)

# Set the output size of the plots
options(repr.plot.width = 20, repr.plot.height = 15)

# Getting current path
current_path <- getwd()
dataset_path <-  paste(current_path, '/Dataset - Assignment',sep = '')

# Get a list of all the subfolders in the main folder
subfolders <- list.dirs(dataset_path, recursive = FALSE)

# Initialize an empty list to store dataframes
df_list <- list()

# Loop through each subfolder
for (subfolder in subfolders) {
  # Get a list of all the csv files in the subfolder
  csv_files <- list.files(subfolder, pattern = "^principal_offence_category_.*\\.csv$", full.names = TRUE)
  
  # Loop through each csv file
  for (csv_file in csv_files) {
    # Read the csv file
    df <- read.csv(csv_file, stringsAsFactors = FALSE)
    
    # Extract the month from the filename
    splitted_path <- strsplit(csv_file,split = '/')
    file_name <- splitted_path[[1]][length(splitted_path[[1]])]
    month <- tolower(substr(file_name, 28, 30)) 
    
    # Add the "month" and "folder" columns to the dataframe
    df$month <- month
    df$year <- substr(subfolder, nchar(subfolder) - 3,nchar(subfolder))
    
    # Append the dataframe to the list
    df_list[[length(df_list) + 1]] <- df
  }
}

# Concatenate all the dataframes into a single dataframe
all_data <- do.call(rbind, df_list)
head(all_data)

vis_miss(all_data)

vis_dat(all_data)

glimpse(all_data)

drop_percentage_columns <- function(dataframe) {
  dataframe <- dataframe %>% select(-starts_with('Per'))
  return(dataframe)
}

all_data <- drop_percentage_columns(all_data)
head(all_data)

# Create a function for dropping extera characters
drop_extera_chars <- function(df, except_cols) {
  colnames(df) <- ifelse(colnames(df) %in% except_cols, 
                         colnames(df), 
                         substring(colnames(df), 11))
  return(df)
}


# Deploy it and update the dataset
all_data <- drop_extera_chars(all_data, c('year', 'month'))
head(all_data)

# Create a function to lower case column names
lower_columns <- function(dataframe) {
  colnames(dataframe) <- tolower(colnames(dataframe))
    return (dataframe)
}

# Deploy it
all_data <- lower_columns(all_data)

head(all_data)

# Create a function for it
convert_dots_to_underscores <- function(dataframe) {
  col_names <- colnames(dataframe)
  modified_col_names <- gsub("\\.", "_", col_names)
  colnames(dataframe) <- modified_col_names
  return(dataframe)
}

# Deploy it
all_data = convert_dots_to_underscores(all_data)

head(all_data)

# Create a function for it
drop_extera_chars2 <- function(all_data) {
  col_names <- colnames(all_data)
  updated_col_names <- col_names
  
  for (i in 1:length(col_names)) {
    if (grepl("_convictions", col_names[i])) {
      updated_col_names[i] <- gsub("_convictions", "", col_names[i])
    } else {
      updated_col_names[i] <- gsub("_unsuccessful", "_un", col_names[i])
    }
  }
  
  colnames(all_data) <- updated_col_names
  return(all_data)
}


# Deploy it
all_data <- drop_extera_chars2(all_data)

head(all_data)

# Create a function to rename column
rename_first_column_to_area <- function(all_data){
  names(all_data)[1] <- 'area'
  return(all_data)
}

# Deploy it
all_data <- rename_first_column_to_area(all_data)
head(all_data)

# Create a function to check string inconsistencies
string_inconsistency_checker <- function(all_data){
  if(length(unique(all_data$month)) == 12){
  print('There is no string inconsistencies in the month column')
  print('The unique values for months are :')
  print(unique(all_data$month))
  }
}

# Deploy it
string_inconsistency_checker(all_data)

# Defining our function for it
create_time_stamp <- function(all_data){
  time_stamp <- as.Date(paste(all_data$month, all_data$year, "01", sep = "-"), format = "%B-%Y-%d")
  all_data$date <- as.Date(time_stamp)
  return(all_data)
}

# Running our function
all_data = create_time_stamp(all_data)
head(all_data)

# Defining our function for it
null_finder <- function(all_data){
  null_location <- which(is.na(all_data), arr.ind=TRUE)
  return(null_location)
}

# Running our function
null_coordinates <- null_finder(all_data)
null_coordinates

# Defining our function for it
check_for_null <- function(all_data){
  if (sum(is.na(all_data)) != 0){
    null_coordinates <- null_finder(all_data)
    glue('We have -{sum(is.na(all_data))}- null value in our dataset. The row number of this value is -{null_coordinates[1]}-')
  }
  else{
    print('There is no null value')
  }
}


# Running our function
check_for_null(all_data)

head(all_data)

glimpse(all_data)

# Creating the function
order_by_date <- function(all_data){
all_data <- all_data[order(all_data$date), ]
}

# Running the function
order_by_date(all_data)
head(all_data)

#Creating the vectore of month in dataset
available_month <- sort(unique(all_data$date))

# Convert the dates to the year-month format
year_month <- format(available_month, "%Y-%m")

# Create a table to count the frequency of each year-month
month_counts <- table(year_month)

# Generate a sequence of all months in the desired period
all_months <- seq(as.Date("2014-01-01"), as.Date("2018-12-01"), by = "month")
all_year_month <- format(all_months, "%Y-%m")

# Create a data frame with the full sequence of months and their counts
full_month_counts <- data.frame(year_month = all_year_month, count = 0)
full_month_counts$count <- month_counts[full_month_counts$year_month]

# Identify missing months and set their count to 0
full_month_counts$count[is.na(full_month_counts$count)] <- 0

# Reshape the data for heatmap plot
heatmap_data <- dcast(full_month_counts, year_month ~., value.var = "count")

# Set the output size
options(repr.plot.width = 20, repr.plot.height = 15)

# Create a heatmap plot
ggplot(melt(heatmap_data, id.vars = "year_month"), aes(x = variable, y = year_month)) +
  geom_tile(aes(fill = value != 0), color = "white") +
  scale_fill_manual(values = c("white", "lightblue"), guide = 'none') +
  xlab(" ") +
  ylab("Year-Month") +
  ggtitle("Missing Months Visualization") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 25, face = "bold"),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 13)
  )

# Create our function
column_sorter <- function(all_data) {
  cols <- colnames(all_data)
  cols <- c(cols[1], cols[(length(cols)-2):length(cols)], cols[2:(length(cols)-3)])
  all_data[, cols]
}

# Deploy it
all_data <- column_sorter(all_data)

head(all_data)

raw_data <- all_data

# Create our function
char_to_int  <- function(dataframe) {
  for (col in names(dataframe[,5:ncol(dataframe)])) {
    if (is.numeric(dataframe[[col]])) {
      dataframe[[col]] <- gsub(",", "", dataframe[[col]])
      dataframe[[col]] <- as.integer(dataframe[[col]])
    } else if (is.character(dataframe[[col]])) {
      dataframe[[col]] <- as.integer(gsub(",", "", dataframe[[col]]))
    }
  }
  return(dataframe)
}


# Deploy it
all_data <- char_to_int(all_data)

head(all_data)

# Check for string consistency in area
glue("The number of unique area is {length(unique(all_data$area))}")
unique(all_data$area)


# devide all areas to three main region

region_list <- list(
  North = c("Cleveland", "Cumbria", "Durham", "Humberside", "Lancashire", "Merseyside", "Northumbria", "North Wales", "North Yorkshire", "South Yorkshire"),
  Center = c("Bedfordshire", "Cambridgeshire", "Derbyshire", "Leicestershire", "Lincolnshire", "Norfolk", "Northamptonshire", "Nottinghamshire", "Staffordshire", "Suffolk", "Warwickshire", "West Mercia"),
  South = c("Avon and Somerset", "Cheshire", "Devon and Cornwall", "Dorset", "Dyfed Powys", "Essex", "Gloucestershire", "GreaterManchester", "Gwent", "Hampshire", "Hertfordshire", "Kent", "Metropolitan and City", "South Wales", "Surrey", "Sussex", "Thames Valley", "West Midlands", "West Yorkshire", "Wiltshire"),
    All = c("National")
)


# Defining a function for mapping the areas to our region map
region_mapper <- function(all_data, region_list) {
    all_data$region <- ifelse(all_data$area %in% region_list$North, "North",
                        ifelse(all_data$area %in% region_list$Center, "Center",
                               ifelse(all_data$area %in% region_list$South, "South", "All")))
    return (all_data)
    }

all_data <- region_mapper(all_data, region_list)

head(all_data)

# Create our function
region_shifter <- function(all_data) {
  num_cols <- ncol(all_data)
  new_data <- all_data[, c(num_cols, 1:(num_cols - 1))]
  return(new_data)
}

# Deploy it
all_data <- region_shifter(all_data)

head(all_data)

glimpse(all_data)

vis_dat(all_data)

vis_miss(all_data)

inspect_types(all_data) %>% show_plot()

inspect_types(all_data, raw_data) %>% show_plot()

# Create a function to split data
splitter <- function(df) {
crime_columns <- !grepl("_un$", colnames(df))
  unsuccesful_columns <- grepl("_un$", colnames(df))
  unsuccesful_columns[1:5] <- TRUE
  
  return(list(df[, crime_columns], df[, unsuccesful_columns]))
}

# Deploy it, and order new dataframes by date
splitted_dataframes <- splitter(all_data)
crime <- data.frame(splitted_dataframes[1])
ucrime <- data.frame(splitted_dataframes[2])
crime <- crime[order(crime$date), ]
ucrime <- ucrime[order(ucrime$date), ]
all_data <- all_data[order(all_data$date), ]

head(crime)

head(ucrime)

# Create a function to add a new column
sum_and_add_column <- function(df) {
  # Get the column names from column 5 to the last column
  columns_to_sum <- names(df)[6:ncol(df)]
  
  # Sum the values in the selected columns
  df$ALL <- rowSums(df[columns_to_sum])
  
  # Return the modified dataframe
  return(df)
}

# Deploy it on all datasets
crime <- sum_and_add_column(crime)
ucrime <- sum_and_add_column(ucrime)
all_data <- sum_and_add_column(all_data)


head(crime)

head(ucrime)

# Define a function to group our data by the years
group_by_year <- function(dataframe){
  dataframe <- dataframe[,-c(1,2,3,5)]
  dataframe <- group_by(dataframe, year)
  summarise_all(dataframe, list(sum))
}

# Select the months with data available for all the years
splitted_df <- crime[!crime$month %in% c('feb', 'mar', 'apr', 'may','jun', 'nov'), ]
splitted_df <- group_by_year(splitted_df)
head(splitted_df)

# Visualize the stacked bar graph of crime dataset
df <- melt(splitted_df ,  id.vars = 'year', variable.name = 'crimes')
options(repr.plot.width = 20, repr.plot.height =10)
ggplot(df,aes(x = year, y = value)) + 
geom_bar(aes(fill = crimes),stat = "identity",position = "dodge", width = 0.8) + 
theme(text = element_text(size = 18), element_line(linewidth =1))
     

# Select the months with data available for all the years
u_splitted_df <- ucrime[!ucrime$month %in% c('feb', 'mar', 'apr', 'may','jun', 'nov'), ]
u_splitted_df <- group_by_year(u_splitted_df)
head(u_splitted_df)

# Visualize the stacked bar graph of ucrime dataset
df <- melt(u_splitted_df ,  id.vars = 'year', variable.name = 'unsuccessful_crimes')
options(repr.plot.width = 20, repr.plot.height =10)
ggplot(df,aes(x = year, y = value)) + 
geom_bar(aes(fill = unsuccessful_crimes),stat = "identity",position = "dodge", width = 0.8) + 
theme(text = element_text(size = 18), element_line(linewidth =1))
     

# Select the months with data available for all the years
all_splitted_df <- all_data[!all_data$month %in% c('feb', 'mar', 'apr', 'may','jun', 'nov'), ]
head(group_by_year(all_splitted_df))

# Visualize the stacked bar graph of all dataset
df <- melt(group_by_year(all_splitted_df) ,  id.vars = 'year', variable.name = 'unsuccessful_and_successful')
options(repr.plot.width = 20, repr.plot.height =10)
ggplot(df,aes(x = year, y = value)) + 
geom_bar(aes(fill = unsuccessful_and_successful),stat = "identity",position = "dodge", width = 0.8) + 
theme(text = element_text(size = 18), element_line(linewidth =1))
     

# Visualized stacked area chart
options(repr.plot.width = 25.2, repr.plot.height = 15)
ggplot(crime, aes(x = date, y = ALL, fill = area)) +
  geom_area(alpha = 0.6, linewidth = 0.5, colour = "white") +
  scale_x_date(date_labels = "%b %d, %Y", date_breaks = "1 month") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Stacked Area Chart for Successful Crimes with Respect to the Time and Area") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 1.5),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 18),
    axis.text = element_text(angle = 90, hjust = 1, size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    )


options(repr.plot.width = 25.2, repr.plot.height = 15)
ggplot(ucrime, aes(x = date, y = ALL, fill = area)) +
  geom_area(alpha = 0.6, linewidth = 0.5, colour = "white") +
 scale_x_date(date_labels = "%b %d, %Y", date_breaks = "1 month")+
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Stacked Area Chart for Unsuccessful Crimes with Respect to the Time and Area") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 1.5),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 18),
    axis.text = element_text(angle = 90, hjust = 1, size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
       )


# Create a dataframe to keep ratio of crime rates in different categories
ratio_df <- splitted_df[,-c(1)] / u_splitted_df[,-c(1,13)]

ratio_df

# Create a function to add the years column to the ratio_df
insert_column <- function(column, dataset) {
  result <- cbind(column, dataset)
    colnames(result)[1] <- 'year'
  return(result)
}

ratio_df <- insert_column(splitted_df$year, ratio_df)
head(ratio_df)

# Plotting the yearly ratio of successful and unsuccessful crime

plot_df <- data.frame(
  year = ratio_df$year,
  value = ratio_df$ALL
)

options(repr.plot.width = 25.2, repr.plot.height = 15)

# Create the bar plot with adjusted width
yearly_ratio <- ggplot(plot_df, aes(x = year, y = value)) +
  geom_bar(stat = "identity", width = 0.2, fill = "cyan") +
  labs(title = "Yearly Ratio of Successful and Unsuccessful Crime") +
  labs(x = "Year", y = "Ratio") +
  theme_minimal() +
  theme(axis.text = element_text(size = 15), axis.title = element_text(size = 18),
       plot.title = element_text(size = 30, hjust = 0.5, vjust = 1.5))  # Adjust the size as desired


# Plotting the monthly ratio of successful and unsuccessful crime

# Slicing dataframe and creating a dataframe of the ratio
u_grouped_by_date <- filter(ucrime, area == "National")[,-c(1,2,3,4)]
grouped_by_date <- filter(crime, area == "National")[,-c(1,2,3,4)]
monthly_ratio_df <- u_grouped_by_date[,c(1,2)]
monthly_ratio_df$ALL <- grouped_by_date[,14]/u_grouped_by_date[,15]

plot_df <- data.frame(
  month = monthly_ratio_df$date,
  value = monthly_ratio_df$ALL
)

options(repr.plot.width = 25.2, repr.plot.height = 15)

# Create the bar plot with adjusted width

monthly_ratio <- ggplot(plot_df, aes(x = month, y = value)) +
  geom_bar(stat = "identity", width = 25, fill = "#69b3a2") +
  labs(title = "Monthly Ratio of Successful and Unsuccessful Crime") +
  scale_x_date(date_labels = "%b %d, %Y", date_breaks = "1 month") +
  labs(x = "month", y = "Ratio") +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 30, hjust = 0.5, vjust = 1.5))


monthly_ratio

yearly_ratio

# Define a function to group our data by the months
group_by_month <- function(dataframe){
  dataframe <- dataframe[,-c(1,2,4,5)]
  dataframe <- group_by(dataframe, month)
  summarise_all(dataframe, list(sum))
}

# Deploy it
crime_grouped_by_months <- group_by_month(crime)
crime_grouped_by_months <- crime_grouped_by_months[order(crime_grouped_by_months$month), ]
crime_grouped_by_months

month_sorter <- function(dataframe){
    real_order_of_months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

    # Convert the "Month" column to a factor with the custom ordering
    dataframe$month <- factor(dataframe$month, levels = real_order_of_months)

    # Sort the dataframe based on the custom ordering of the "Month" column
    dataframe <- dataframe[order(dataframe$month), ]

    return (dataframe)
    }


crime_grouped_by_months <- month_sorter(crime_grouped_by_months)
crime_grouped_by_months

# This function gets the grouped_by dataframe, and devide all each month by the number of month
# available in the dataset. It helps to get an average crime number for each month in each segment
# two November, one February, one March, two April, two May, and June are missed.

divide_rows_by_weights <- function(dataframe) {
    
  # this is the number of month for each month. 
    num_months_in_5year <- c(5, 4, 4, 3, 3, 3, 5, 5, 5, 5, 3, 5)
    
    dataframe[, -1] <- sweep(dataframe[, -1], 1, num_months_in_5year, "/")
    
    return (dataframe)
}

average_crime_monthly <- divide_rows_by_weights(crime_grouped_by_months)
average_crime_monthly

doughnut_plotter <- function(s) {

    # Compute percentages
    s$fraction <- s$ALL / sum(s$ALL)

    # Compute the cumulative percentages (top of each rectangle)
    s$ymax <- cumsum(s$fraction)

    # Compute the bottom of each rectangle
    s$ymin <- c(0, head(s$ymax, n=-1))

    # Compute label position
    s$labelPosition <- (s$ymax + s$ymin) / 2

    # Creating the percentage
    s$percentage <- round(s$ALL/sum(s$ALL), digits = 4)

    # Compute a good label
    s$label <- paste0(s$month, "\n %", 100*s$percentage)


   # Define the custom palette with 12 dark colors
    custom_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                        "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                        "#1a1a1a", "#757575")
    
    options(repr.plot.width = 20, repr.plot.height = 10)
    
    # Make the plot
    ggplot(s, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = month)) +
      geom_rect() +
      geom_text(x = 2.3, aes(y = labelPosition, label = label, color = month), size = 6) +
      scale_fill_manual(values = custom_palette) +  # Use the custom fill palette
      scale_color_manual(values = custom_palette) +  # Use the custom color palette
      coord_polar(theta = "y") +
      xlim(c(-1, 4)) +
      theme_void() +
      theme(legend.position = "none")

    }

# Deploy it
df <- melt(average_crime_monthly ,  id.vars = 'month', variable.name = 'successful_crimes')
options(repr.plot.width = 20, repr.plot.height =10)
ggplot(df,aes(x = month, y = value)) + 
geom_bar(aes(fill = successful_crimes),stat = "identity",position = "dodge", width = 0.8) + 
theme(text = element_text(size = 18), element_line(linewidth =1))
     

# Plot doughnout plot
doughnut_plotter(average_crime_monthly)

# Group ucrime dataset by month
ucrime_grouped_by_months <- group_by_month(ucrime)
ucrime_grouped_by_months <- ucrime_grouped_by_months[order(ucrime_grouped_by_months$month), ]
ucrime_grouped_by_months

ucrime_grouped_by_months <- month_sorter(ucrime_grouped_by_months)
ucrime_grouped_by_months

# get average number of crimes
average_ucrime_monthly <- divide_rows_by_weights(ucrime_grouped_by_months)
average_ucrime_monthly

# Visualize it
df <- melt(average_ucrime_monthly ,  id.vars = 'month', variable.name = 'unsuccessful_crimes')
options(repr.plot.width = 17, repr.plot.height =10)
ggplot(df,aes(x = month, y = value)) + 
geom_bar(aes(fill = unsuccessful_crimes),stat = "identity",position = "dodge", width = 0.8) + 
theme(text = element_text(size = 18), element_line(linewidth =1))

doughnut_plotter(average_ucrime_monthly)

# Create a function to associate month by seasons
calculate_season_sums <- function(df, season_col) {
    
    season_months <- list(
  Winter = c("jan", "feb", "dec"),
  Spring = c("mar", "apr", "may"),
  Summer = c("jun", "jul", "aug"),
  Autumn = c("sep", "oct", "nov")
)

  seasons <- names(season_months)
  season_df <- data.frame(matrix(0, nrow = length(seasons), ncol = ncol(df)))
  
  for (i in 1:length(seasons)) {
    season <- seasons[i]
    season_cols <- which(df[[season_col]] %in% season_months[[season]])
    
    season_rows <- df[season_cols, ]
    season_sums <- colSums(season_rows[, -1], na.rm = TRUE)
    
    season_df[i, -1] <- season_sums
  }
  
  row.names(season_df) <- seasons
    colnames(season_df)[-1] <- colnames(df)[-1]
    colnames(season_df)[1] <- "Season"
    season_df$Season <- rownames(season_df)

    # Step 4: Remove the row names
    rownames(season_df) <- NULL

    
  return(season_df)
}

# Check the result
calculate_season_sums(average_crime_monthly, "month")

# Deploy it
s <- calculate_season_sums(average_crime_monthly, "month")

# Define the visualization function
doughnut_plotter2 <- function(s){
# Compute percentages
s$fraction <- s$ALL / sum(s$ALL)

# Compute the cumulative percentages (top of each rectangle)
s$ymax <- cumsum(s$fraction)

# Compute the bottom of each rectangle
s$ymin <- c(0, head(s$ymax, n=-1))

# Compute label position
s$labelPosition <- (s$ymax + s$ymin) / 2

# Creating the percentage
s$percentage <- round(s$ALL/sum(s$ALL), digits = 4)

# Compute a good label
s$label <- paste0(s$Season, "\n percentage: %", 100*s$percentage)


# Make the plot
ggplot(s, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Season)) +
  geom_rect() +
  geom_text( x=1.6, aes(y=labelPosition, label=label, color=Season), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")
    }

df <- melt(s ,  id.vars = 'Season', variable.name = 'successful_crimes')
options(repr.plot.width = 17, repr.plot.height =10)
ggplot(df,aes(x = Season, y = value)) + 
geom_bar(aes(fill = successful_crimes),stat = "identity",position = "dodge", width = 0.8) + 
theme(text = element_text(size = 18), element_line(linewidth =1))

doughnut_plotter2(s)

calculate_season_sums(average_ucrime_monthly, "month")

us <- calculate_season_sums(average_ucrime_monthly, "month")

df <- melt(us ,  id.vars = 'Season', variable.name = 'unsuccessful_crimes')
options(repr.plot.width = 20, repr.plot.height =10)
ggplot(df,aes(x = Season, y = value)) + 
geom_bar(aes(fill = unsuccessful_crimes),stat = "identity",position = "dodge", width = 0.8) + 
theme(text = element_text(size = 18), element_line(linewidth =1))

doughnut_plotter2(us)

# Create a dataframe grouped by date
grouped_by_date <- filter(crime, area == "National")[,-c(1,2,3,4,18)]
head(grouped_by_date)

# Create a function for visualizing the stacked linear graph
stacked_line_grapher <- function (data){
    df_long <- data %>% pivot_longer(cols = -date, names_to = "Category", values_to = "Value")

    options(repr.plot.width = 25.2, repr.plot.height = 18)

    # Plot the stacked line graph
    p <- ggplot(df_long, aes(x = date, y = Value, group = Category, color = Category)) +
      geom_line(linewidth=1.2) +
      scale_x_date(date_labels = "%b %d, %Y", date_breaks = "3 month") +
      labs(x = "Date", y = "Value", title = "Crime Trend for Different Categories 2014-2018") +
      theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15),
        plot.title = element_text(size = 30, hjust = 0.5, vjust = 1.5),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18))
    return (p)
    }

stacked_line_grapher(grouped_by_date)

u_grouped_by_date <- filter(ucrime, area == "National")[,-c(1,2,3,4,19)]
head(u_grouped_by_date)

stacked_line_grapher(u_grouped_by_date)

df_long <- grouped_by_date %>% pivot_longer(cols = -date, names_to = "Category", values_to = "Value")
df_long$Convictions <- as.character(df_long$Value)


options(repr.plot.width = 30, repr.plot.height = 21.4)

# Plot and Animate
a <- ggplot(df_long, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Date: {closest_state}") +
  geom_text(aes(label = Convictions, y = Value),
            position = position_dodge(0.9), vjust = -1 ) +
  theme_classic() +
  transition_states(states = date, transition_length = 1, state_length = 1) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# Add pauses between frames
a_slow <- animate(a, width = 940, height = 480, nframes = 2*length(unique(df_long$date)) * 2, fps = 5)

a_slow

# Define a function to group our data by the area
group_by_region <- function(dataframe){
  dataframe <- dataframe[,-c(1,3,4,5)]
  dataframe <- group_by(dataframe, area)
  summarise_all(dataframe, list(sum))
}

# Deploy it
grouped_by_region <- group_by_region(crime)
head(grouped_by_region)

# Removing the National row from the dataset
grouped_by_region = filter(grouped_by_region, area != "National")
head(grouped_by_region)


#Creating a dataframe contains lattitude and longitude of the different areas  
area_map <- data.frame(
  area = c("Avon and Somerset", "Bedfordshire", "Cambridgeshire", "Cheshire", "Cleveland", "Cumbria", "Derbyshire", "Devon and Cornwall", "Dorset", "Durham", "Dyfed Powys", "Essex", "Gloucestershire", "GreaterManchester", "Gwent", "Hampshire", "Hertfordshire", "Humberside", "Kent", "Lancashire", "Leicestershire", "Lincolnshire", "Merseyside", "Metropolitan and City", "Norfolk", "Northamptonshire", "Northumbria", "North Wales", "North Yorkshire", "Nottinghamshire", "South Wales", "South Yorkshire", "Staffordshire", "Suffolk", "Surrey", "Sussex", "Thames Valley", "Warwickshire", "West Mercia", "West Midlands", "West Yorkshire", "Wiltshire"),
  Longitude = c(-2.4724, -0.4713, 0.08831, -2.77264, -1.26479, -3.09273, -1.57126, -3.84036, -2.28556, -1.6176, -4.05117, 0.56873, -2.16179, -2.39688, -3.04425, -1.34453, -0.26407, -0.41604, 0.67834, -2.70649, -1.21497, -0.217, -2.93036, -0.10956, 1.00236, -0.98553, -1.66276, -3.84354, -1.08271, -1.07417, -3.50016, -1.35208, -1.91323, 1.00494, -0.47046, -0.44144, -1.12764, -1.58323, -2.62168, -1.89858, -1.64323, -1.99768),
  Latitude = c(51.38897, 52.06864, 52.38235, 53.20986, 54.5715, 54.65139, 53.12793, 50.6697, 50.75391, 54.77868, 52.06963, 51.77291, 51.86328, 53.47239, 51.74864, 51.05983, 51.80825, 53.68436, 51.23425, 53.82765, 52.67212, 53.18563, 53.41623, 51.51488, 52.63654, 52.24587, 55.14918, 53.13666, 54.22759, 53.12322, 51.59882, 53.47227, 52.87999, 52.20758, 51.29732, 50.92856, 51.61744, 52.30101, 52.32348, 52.48624, 53.78, 51.34982)
)
area_map <- area_map[order(area_map$area), ]
head(area_map)

# Adding the total crime number to the area_map dataframe
area_map$crime <- grouped_by_region$ALL
head(area_map)

# Define the bounding box coordinates for England and Wales
ew_bbox <- c(left = -5.797, bottom = 50.064, right = 1.799, top = 55.811)

# Get the map of England and Wales using ggmap
ew_map <- get_stamenmap(ew_bbox, zoom = 7, maptype = "watercolor")

options(repr.plot.width = 15, repr.plot.height =15)

# Create a plot using ggmap and ggplot2
plot <- ggmap(ew_map, extent = "device") +
  geom_point(data = area_map, aes(x = Longitude, y = Latitude, size = crime, color = area), alpha = 0.85) +
  scale_size_continuous(range = c(3, 20)) +
  scale_color_manual(values = rainbow(length(unique(area_map$area)))) +
  geom_text(data = area_map, aes(x = Longitude, y = Latitude, label = area), vjust = -1.5, size = 4) +
  theme_void()

# Display the plot
print(plot)

# Adding regions to the area_map
area_map <- region_mapper(area_map, region_list)
head(area_map)

# Removing Longitue and Latitude from area_map
area_map <- area_map[,-c(2,3)]
head(area_map)


# Rename columns and and
colnames(area_map) <- c('individual', 'value', 'group')
data <- area_map
data <- data[order(data$value), ]
head(data)

# Add a new column for labeling the bars
data$labeler <- paste(as.character(data$individual), '%', as.character(format(round(100 * data$value / sum(data$value), 2), nsmall = 2)), sep = '')
head(data)

# Create a function for plotting circular stacked bar graph
circular_stacked_bar_plotter <- function(data){
empty_bar <- 4
to_add <- data.frame(matrix(NA, empty_bar * nlevels(data$group), ncol(data)))
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each = empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

options(repr.plot.width = 18, repr.plot.height = 20)

# Make the plot
p <- ggplot(data, aes(x = as.factor(id), y = value, fill = group)) +
  geom_bar(stat = "identity", alpha = 0.5) +
  ylim(-100000, 390444) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 25)
  ) +
  coord_polar() +
  geom_text(
    data = label_data,
    aes(x = id, y = value + 10, label = labeler, hjust = hjust),
    color = "black",
    fontface = "bold",
    alpha = 0.6,
    size = 5,
    angle = label_data$angle,
    inherit.aes = FALSE
  ) +
  labs(title = "Crime Rate Comparison of Different Areas")

  return (p)
}

circular_stacked_bar_plotter(data)


# make sample dataframe

Category <- data[order(-data$value),]$labeler
Percent <- data[order(-data$value),]$value

internetImportance<-data.frame(Category,Percent)

# append number to category name
internetImportance$Category <-
     paste0(internetImportance$Category)

# set factor so it will plot in descending order 
internetImportance$Category <-
    factor(internetImportance$Category, 
    levels=rev(internetImportance$Category))


options(repr.plot.width = 15, repr.plot.height = 15)


# plot

ggplot(internetImportance, aes(x = Category, y = Percent,
    fill = Category)) + 
    geom_bar(width = 0.9, stat="identity") + 
    coord_polar(theta = "y") +
    xlab("") + ylab("") +
    ylim(c(0,350000)) +
    ggtitle("Average Crime Conviction Proportion of Different Areas") +
    geom_text(data = internetImportance, hjust = 1, size = 4,
              aes(x = Category, y = 0, label = Category)) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
         plot.title = element_text(hjust = 0.5, vjust = 1, size = 25))



ratio_plotter <- function(data1, data2){
    u_grouped_by_date <- filter(data2, area == "National")[,-c(1,2,3,4,5)]
    grouped_by_date <- filter(data1, area == "National")[,-c(1,2,3,4,5)]
    ratio_df <- grouped_by_date/u_grouped_by_date[,-c(13)]
    colnames(ratio_df)[13] <- 'Average'

    
    # Calculate column averages
    column_averages <- colMeans(ratio_df)
    
    # Convert the column_averages into a data frame
    df <- data.frame(Column = colnames(ratio_df), Ratio = column_averages)
    
    options(repr.plot.width = 25.2, repr.plot.height = 15)

    # Define the color palette
    num_shades <- nrow(df)
    green_palette <- colorRampPalette(c("lightgreen", "darkgreen"))
    green_colors <- green_palette(num_shades)

    # Create the barplot using ggplot2
    plot <- ggplot(df, aes(x = Column, y = Ratio, fill = Ratio)) +
      geom_bar(stat = "identity", width = 0.7) +
      labs(title = "Ratio of Convictioin to Unsuccessful Crimes",
           x = "Category",
           y = "Ratio") +
      scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 40, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
            axis.text.y = element_text( size = 15),
            axis.title = element_text(size = 30),
            legend.position = "none")
    
    return(plot)
}

ratio_plotter(crime, ucrime)

# Define a correlation visualiser function
correlation_maker <- function(data){
options(repr.plot.width = 25.2, repr.plot.height = 15)
plot <- ggpairs(data[,-c(1,2,3,4,5)] , title="correlogram with ggpairs()") 
return (plot)
}

# Correlation of crime categories for convictions
correlation_maker(crime)

# Correlation of crime categories for Unsuccessful
correlation_maker(ucrime)

# Correlation of crime categories for convictions
correlation_maker(filter(crime, area != "National", area !="Metropolitan and City"))

# Correlation of crime categories for Unsuccessful
correlation_maker(filter(ucrime, area != "National", area !="Metropolitan and City"))

# Correlation of crime categories for convictions
correlation_maker(filter(crime, area == "National"))

# Correlation of crime categories for Unsuccessful
correlation_maker(filter(ucrime, area == "National"))

# Ordering the datasets by date and area
crime <- crime[order(crime$date, crime$area), ]
ucrime <- ucrime[order(ucrime$date, ucrime$area), ]
all_data <- all_data[order(all_data$date, all_data$area), ]


# Create a function to deploy a simple linear model and interpret and visualize the result
crime_prediction_slm <- function(dataset){
    set.seed(42)
    # Split the dataset into training and test sets
    reg_df <- filter(dataset, area == "National")
    split <- sample.split(reg_df$ALL, SplitRatio = 0.7)  
    train_data <- subset(reg_df, split == TRUE)
    test_data <- subset(reg_df, split == FALSE)

    # Perform simple linear regression
    model <- lm(ALL ~ date, data = train_data)

    # Predict on the test set
    predictions <- predict(model, newdata = test_data)

    # Evaluate metrics and score
    RMSE <- caret::RMSE(predictions, test_data$ALL)
    normalized_RMSE <- RMSE / (max(test_data$ALL) - min(test_data$ALL))
    R2 <- summary(model)$r.squared
    
    
    
    
    
    
    # Summary of the regression model
    model_summary <- summary(model)

    # Plot comparison of predictions and true values
    plot_data <- data.frame(Predicted = predictions, True = test_data$ALL)
    plot <- ggplot(plot_data, aes(x = True, y = Predicted)) +
    geom_point(size = 2) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
    labs(x = "True Values", y = "Predicted Values", title = "Comparison of Predictions and True Values") +
    theme_minimal() +
    theme(
    axis.text = element_text(size = 21),
    axis.title = element_text(size = 21),
    plot.title = element_text(size = 30),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
    )

    # Return results as a list
    results <- list(
    RMSE = RMSE,
    normalized_RMSE = normalized_RMSE,
    R2 = R2,
    ModelSummary = model_summary,
    Plot = plot
    )
    
    return (results)
}

slm_model_1 = crime_prediction_slm(crime)
RMSE <- slm_model_1$RMSE
normalized_RMSE <- slm_model_1$normalized_RMSE
R2 <- slm_model_1$R2
model_summary <- slm_model_1$ModelSummary
model_1_plot <- slm_model_1$Plot

# Print the metrics and score
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("Normalized Root Mean Squared Error (RMSE):", normalized_RMSE, "\n")
cat("R-squared (R2):", R2, "\n")

print(model_summary)

model_1_plot

slm_model_2 = crime_prediction_slm(ucrime)
RMSE <- slm_model_2$RMSE
normalized_RMSE <- slm_model_2$normalized_RMSE
R2 <- slm_model_2$R2
model_summary <- slm_model_2$ModelSummary
model_2_plot <- slm_model_2$Plot

# Print the metrics and score
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("Normalized Root Mean Squared Error (RMSE):", normalized_RMSE, "\n")
cat("R-squared (R2):", R2, "\n")

print(model_summary)

model_2_plot

# Create a model for deploy, interpret, and visualise the multivariate regression model
perform_regression <- function(dataset, target_variable, independent_variables, split_ratio = 0.7) {
    set.seed(42)
    # Step 1: Split the dataset
    split_data <- caTools::sample.split(dataset[[target_variable]], SplitRatio = split_ratio)
    train_data <- subset(dataset, split_data == TRUE)
    test_data <- subset(dataset, split_data == FALSE)
  
    # Step 2: Perform multivariate regression
    formula <- as.formula(paste(target_variable, paste(independent_variables, collapse = " + "), sep = " ~ "))
    model <- lm(formula, data = train_data)

    # Step 3: Predict on the test set
    predictions <- predict(model, newdata = test_data)

    # Step 4: Evaluate metrics and score
    RMSE <- caret::RMSE(predictions, test_data[[target_variable]])
    normalized_RMSE <- RMSE / (max(test_data[[target_variable]]) - min(test_data[[target_variable]]))
    R2 <- summary(model)$r.squared



    # Plot comparison of predicted and actual values
    plot_data <- data.frame(True = test_data[[target_variable]], Predicted = predictions)
    plot_comparison <- ggplot(plot_data, aes(x = True, y = Predicted)) +
    geom_point(size = 2) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
    labs(x = "True Values", y = "Predicted Values", title = "Comparison of Predictions and True Values") +
    theme_minimal() +
    theme(
        axis.text = element_text(size = 21),
        axis.title = element_text(size = 21),
        plot.title = element_text(size = 30),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18)
    )


    # Plot residuals
    residuals <- test_data[[target_variable]] - predictions
    plot_residuals <- ggplot(data.frame(Residuals = residuals), aes(x = seq_along(Residuals), y = Residuals)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
    labs(x = "Observation", y = "Residuals", title = "Residual Plot") +
    theme_minimal() +
    theme(
        axis.text = element_text(size = 21),
        axis.title = element_text(size = 21),
        plot.title = element_text(size = 30),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18)
    )


    # Plot smooth histogram of residuals
    resuduals_histogram <- ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
    geom_density(fill = "lightblue", color = "black") +
    labs(x = "Residuals", y = "Density", title = "Smooth Histogram of Residuals") +
    theme_minimal() +
    theme(
        axis.text = element_text(size = 21),
        axis.title = element_text(size = 21),
        plot.title = element_text(size = 30),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18)
    )



    # Calculate standardized residuals
    standardized_residuals <- residuals / sd(residuals)

    # Plot Normality Q-Q plot
    plot_normality <- ggplot(data.frame(Standardized_Residuals = standardized_residuals), aes(sample = Standardized_Residuals)) +
    stat_qq() +
    stat_qq_line() +
    labs(x = "Theoretical Quantiles", y = "Standardized Residuals", title = "Normality Q-Q Plot") +
    theme_minimal() +
    theme(
        axis.text = element_text(size = 21),
        axis.title = element_text(size = 21),
        plot.title = element_text(size = 30),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18)
    )


    results <- list(RMSE=RMSE,
                    normalized_RMSE=normalized_RMSE,
                    R2=R2,
                    model_summary = summary(model),
                    comparison_plot = plot_comparison,
                    residuals_plot = plot_residuals,
                    resuduals_histogram=resuduals_histogram,
                    plot_normality=plot_normality)


    # Return the regression model, predictions, and plots
    return(results)
}


mlm_model_1 = perform_regression(crime, "ALL", c("month", "area", "year"))
RMSE <- mlm_model_1$RMSE
normalized_RMSE <- mlm_model_1$normalized_RMSE
R2 <- mlm_model_1$R2
model_summary <- mlm_model_1$model_summary
comparison_plot <- mlm_model_1$comparison_plot
residuals_plot <- mlm_model_1$residuals_plot
resuduals_histogram <- mlm_model_1$resuduals_histogram
plot_normality <- mlm_model_1$plot_normality

print(paste("RMSE:", RMSE))
print(paste("Normalized RMSE Score:", normalized_RMSE))
print(paste("R2 Score:", R2))

model_summary

comparison_plot

residuals_plot

resuduals_histogram

plot_normality

mlm_model_2 = perform_regression(ucrime, "ALL", c("month", "area", "year"))
RMSE <- mlm_model_2$RMSE
normalized_RMSE <- mlm_model_2$normalized_RMSE
R2 <- mlm_model_2$R2
model_summary <- mlm_model_2$model_summary
comparison_plot <- mlm_model_2$comparison_plot
residuals_plot <- mlm_model_2$residuals_plot
resuduals_histogram <- mlm_model_2$resuduals_histogram
plot_normality <- mlm_model_2$plot_normality

print(paste("RMSE:", RMSE))
print(paste("Normalized RMSE Score:", normalized_RMSE))
print(paste("R2 Score:", R2))

model_summary

comparison_plot

residuals_plot

resuduals_histogram

plot_normality

# Create a function for evaluating the WCSS
kmean_withinss <- function(k) {
    cluster <- kmeans(crime[,-c(1,2,3,4,5,18)], k)
    return (cluster$tot.withinss)
}

# Create a function to perform k-means clustering

perform_kmeans_clustering <- function(data, k) {
  numeric_data <- data[,-c(1,2,3,4,5,18)]
  kmeans_result <- kmeans(numeric_data, centers = k)  
  return(kmeans_result)
}


# Create a function to visualize clusters using PCA

plot_pca_clusters <- function(data, k) {
  pca_result <- prcomp(data, scale. = TRUE)
  reduced_data <- as.data.frame(pca_result$x[, 1:2])
  kmeans_result <- kmeans(reduced_data, centers = k)
  cluster_labels <- kmeans_result$cluster
  data_with_clusters <- cbind(reduced_data, Cluster = as.factor(cluster_labels))
  ggplot(data_with_clusters, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size=2) +
    labs(title = "K-means Clustering (PCA)", x = "Principal Component 1", y = "Principal Component 2") +
    theme_minimal()+
    theme(
        axis.text = element_text(size = 21),
        axis.title = element_text(size = 21),
        plot.title = element_text(size = 30),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 17)
    )

}

# Create a function to visualize clusters using t-SNE

plot_tsne_clusters <- function(data, k) {
  data <- data[!duplicated(data), ]
  kmeans_result <- kmeans(data, centers = k)
  cluster_labels <- kmeans_result$cluster
  data_with_clusters <- cbind(data, Cluster = as.factor(cluster_labels))
  tsne_result <- Rtsne(data, dims = 2, perplexity = 30, verbose = TRUE)
  tsne_data <- as.data.frame(tsne_result$Y)
  tsne_data$Cluster <- as.factor(cluster_labels)
  ggplot(tsne_data, aes(x = V1, y = V2, color = Cluster)) +
    geom_point(size=1.8) +
    labs(title = "K-means Clustering (t-SNE)", x = "Dimension 1", y = "Dimension 2") +
    theme_minimal()+
    theme(
        axis.text = element_text(size = 21),
        axis.title = element_text(size = 21),
        plot.title = element_text(size = 30),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 17)
    )

}


# Set maximum cluster 
max_k <-10 
wss <- sapply(2:max_k, kmean_withinss)
elbow <-data.frame(2:max_k, wss)
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(1, 20, by = 1))

model_k_3 <- perform_kmeans_clustering(crime, 3)

print("The Number of Instances belong to Each Cluster:")
print(table(model_k_3$cluster))
cat("\n\n\n\n")
print(model_k_3[-c(1)])

plot_pca_clusters(crime[,-c(1,2,3,4,5,18)], k = 3)

plot_tsne_clusters(crime[,-c(1,2,3,4,5,18)], k = 3)

model_k_4 <- perform_kmeans_clustering(crime, 4)

print("The Number of Instances belong to Each Cluster:")
print(table(model_k_4$cluster))
cat("\n\n\n\n")
print(model_k_4[-c(1)])

plot_pca_clusters(crime[,-c(1,2,3,4,5,18)], k = 4)

plot_tsne_clusters(crime[,-c(1,2,3,4,5,18)], k = 4)

model_k_5 <- perform_kmeans_clustering(crime, 5)

print("The Number of Instances belong to Each Cluster:")
print(table(model_k_5$cluster))
cat("\n\n\n\n")
print(model_k_5[-c(1)])

plot_pca_clusters(crime[,-c(1,2,3,4,5,18)], k = 5)

plot_tsne_clusters(crime[,-c(1,2,3,4,5,18)], k = 5)

# Creating a function to plot confusion matrix
confusion_matrix_plotter <- function(predicted_classes, actual_classes, model){
predicted_classes <- predict(model, newdata = test_data)
confusion_matrix <- table(predicted_classes, actual_classes)
confusion_df <- as.data.frame(confusion_matrix)
options(repr.plot.width = 25.2, repr.plot.height = 15)
ggplot(data = confusion_df, aes(x = predicted_classes, y = actual_classes, fill = Freq)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = Freq), size = 10, color = "brown") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Predicted Class", y = "Actual Class") +
  theme_minimal()+
    theme(
        axis.text = element_text(size = 21),
        axis.title = element_text(size = 21),
        plot.title = element_text(size = 30),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 17)
    )
}

# Creating a function to generate metrics of the model
metrics_summary <- function(predicted_classes, actual_classes){
    predicted_classes <- as.factor(predicted_classes)
    actual_classes <- as.factor(actual_classes)
    levels(predicted_classes) <- levels(actual_classes)
    confusionMatrix(predicted_classes, actual_classes)
}

data <- crime[,-c(2,3,4,5)]
set.seed(123) 
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
model <- multinom(region ~ ., data = train_data)
predicted_classes <- predict(model, newdata = test_data, type = "class")
actual_classes <- test_data$region
accuracy <- sum(predicted_classes == actual_classes) / length(actual_classes)
cat("Accuracy:", accuracy, "\n")


metrics_summary(predicted_classes, actual_classes)

confusion_matrix_plotter(predicted_classes, actual_classes, model)

data <- crime[,-c(2,3,4,5)]
set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
train_data$region <- as.factor(train_data$region)
test_data <- data[-train_indices, ]
model <- randomForest(region ~ ., data = train_data, ntree = 100)
predicted_classes <- predict(model, newdata = test_data)
actual_classes <- test_data$region
accuracy <- sum(predicted_classes == actual_classes) / length(actual_classes)
cat("Accuracy:", accuracy, "\n")


metrics_summary(predicted_classes, actual_classes)

confusion_matrix_plotter(predicted_classes, actual_classes, model)

plot(model)
legend("topright", legend = levels(train_data$region), col = 1:length(levels(train_data$region)), pch = 1, cex = 1.5)

varImpPlot(model, main = "Variable Importance Plot", cex.axis = 1.5, cex.lab = 1.5, cex.main = 2, pch = 15)

set.seed(123)
train_indices <- sample(1:nrow(crime[,-c(2,3,4,5)]), 0.7 * nrow(crime[,-c(2,3,4,5)]))  # 70% for training
train_data <- crime[train_indices, ]
test_data <- crime[-train_indices, ]
train_data$region <- as.factor(train_data$region)
svm_model <- svm(region ~ ., data = train_data)
predicted_classes <- predict(svm_model, newdata = test_data)
actual_classes <- test_data$region
accuracy <- sum(predicted_classes == actual_classes) / length(actual_classes)
cat("Accuracy:", accuracy, "\n")


metrics_summary(predicted_classes, actual_classes)

confusion_matrix_plotter(predicted_classes, actual_classes, svm_model)
