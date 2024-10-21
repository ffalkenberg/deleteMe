# #Install Library
# library(rvest)
# library(dplyr)
# library(httr)
# library(jsonlite)
# library(lubridate)
# library(ggplot2)


# #Load data into R
# orders <- read.csv("orders.csv")
# users <- read.csv("users.csv")

# ##Task 1
# #Merge two data sets:
# orders_full <- merge(orders, users, by = "user_id", all = TRUE)
# print(head(orders_full))

# ##Task 2
# #Structure of the data frame
# str(orders_full)

# #Convert the proper date format 
# orders_full$order_date <- ymd(orders_full$order_date)
# orders_full$delivery_date <- ymd(orders_full$delivery_date)
# orders_full$user_dob <- ymd(orders_full$user_dob)
# orders_full$user_reg_date <- ymd(orders_full$user_reg_date)

# #Convert value in variable return with new label Yes and No
# orders_full$return <- factor(orders_full$return, 
#                              levels = c(0, 1), 
#                              labels = c("No", "Yes"))

# # Check the structure to confirm the conversion
# str(orders_full)

# ##Task 3
# #Computing Summary for all variable
# summary_stats <- summary(orders_full)
# print(summary_stats)

# #Identify columns with missing values
# missing_values <- colSums(is.na(orders_full))

# # Display columns that have missing values
# missing_values[missing_values > 0]

# #Two methods to impute missing value (Check DSA class to read)

# #Compute delivery_time as the difference between delivery_date and order_date
# orders_full$delivery_time <- orders_full$delivery_date - orders_full$order_date
# head(orders_full$delivery_time)

# #A lot of wrong values
# mistake_delivery_time <- sum(orders_full$delivery_time <= 0, na.rm = TRUE)
# mistake_delivery_time

head(orders_full[orders_full$delivery_time <= 0, c("order_date", "delivery_date", "delivery_time")])

# The negative delivery_time rows have missing order_date or delivery_date values. 
# You can clean the data by removing these rows:

orders_full <- orders_full[!is.na(orders_full$order_date) & !is.na(orders_full$delivery_date), ]

# #Plot the DOB distribution
# orders_full <- orders_full %>%
#   mutate(year_of_birth = year(user_dob))

# ggplot(orders_full, aes(x = year_of_birth, y = ..density..)) +
#   geom_histogram(binwidth = 1, fill = "grey", color = "black", alpha = 0.7) +
#   geom_density(color = "red", size = 0.5) +
#   labs(title = "Distribution of User Date of Birth (Year)",
#        x = "Year of Birth",
#        y = "Density") +
#   theme_minimal()

# summary(orders_full$user_dob)

# Calculate the IQR
# orders_full <- orders_full %>%
#   mutate(
#   iqr = IQR(year_of_birth, na.rm = TRUE),
#   # Determine lower and upper bounds for outliers
#   lower_bound = quantile(year_of_birth, 0.25, na.rm = TRUE) - 1.5 * iqr,
#   upper_bound = quantile(year_of_birth, 0.75, na.rm = TRUE) + 1.5 * iqr
#   ) %>%
#   # Filter out outliers based on the bounds
#     filter(between(year_of_birth, lower_bound, upper_bound)) %>%
#     # Remove the temporary columns
#     select(-year_of_birth, -iqr, -lower_bound, -upper_bound)

# Calculate IQR and bounds once for the whole dataset
iqr_value <- IQR(orders_full$year_of_birth, na.rm = TRUE)
lower_bound <- quantile(orders_full$year_of_birth, 0.25, na.rm = TRUE) - 1.5 * iqr_value
upper_bound <- quantile(orders_full$year_of_birth, 0.75, na.rm = TRUE) + 1.5 * iqr_value

outliers <- orders_full %>%
  filter(!between(year_of_birth, lower_bound, upper_bound))

# Print the outliers
print(outliers)

# Now filter out the outliers 
orders_full <- orders_full %>%
  filter(between(year_of_birth, lower_bound, upper_bound))

##Task 4
# Extract unique values from the item_size column
# unique_item_sizes <- unique(orders_full$item_size)
# print(unique_item_sizes)

# Categorize products based on item_size
orders_full$product_category <- case_when(
  orders_full$item_size %in% c("xs", "s", "m", "l", "xl", "xxl", "xxxl") ~ "Clothing",
  grepl("^[0-9]{2}$", orders_full$item_size) ~ "Shoes",
  orders_full$item_size %in% c("104", "116", "128", "140", "152", "164", "176") ~ "Children's Clothing",
  grepl("\\+$", orders_full$item_size) ~ "Plus Size",
  orders_full$item_size == "unsized" ~ "Unsized Product",
  TRUE ~ "Other"
)

# Display the product categories
print(unique(orders_full$product_category))

# ##Exercise 2: Web Scraping
# #Install Library
# library(rvest)
# library(dplyr)
# library(ggplot2)

# #Create the vector season
# seasons <- c()
#   for (year in 1995:2024) {
#     season <- paste(year, year + 1, sep = "-")
#     season <- paste(year, substr(year + 1, 3, 4), sep = "-")
#     seasons <- c(seasons, season)
#   }

# #Create the vector matchday
#   matchdays <- 1:34
#   link_url <- paste0("https://www.kicker.de/bundesliga/tabelle/", season, "/", matchday, "/")
  
# #Setup the function:
#   point_difference <- function(season, matchday){
#     link_url <- paste0("https://www.kicker.de/bundesliga/tabelle/", season, "/", matchday, "/")
#     link_url
#     page <- read_html(link_url)
#     # Extract the first two <td> elements with the specified class
#     elements <- page %>% html_nodes("td.kick__table--ranking__master.kick__respt-m-o-5")
#     # Extract the text content
#     values <- html_text(elements) %>%
#       trimws()        # removing  white spaces
#     #Format the value
#     values <- as.numeric(values)
#     #Extract the value of first and second team
#     first_team <- values[1]
#     second_team <- values[2]
#     #Calculate the point difference:
#     c <- first_team - second_team 
#     return(c)
#   }

#   season <- "1996-97"
#   matchday <- 1
#   link_url <- paste0("https://www.kicker.de/bundesliga/tabelle/", season, "/", matchday, "/")
#   link_url
#   point_difference(1996-97,1)
  
  
  
  
  
# #Create the URL structure
# urls <- character(length(seasons) * length(matchday))  # Pre-allocate vector

# # Counter for the urls vector
# url_index <- 1

# for (season in seasons) {
#   for (matchday in matchdays) {
#     link_url <- paste0("https://www.kicker.de/bundesliga/tabelle/", season, "/", matchday, "/")
#     point_difference(season,matchday)
    
#     urls[url_index] <- link_url
#     url_index <- url_index + 1
#   }
# }


# # Print the first few URLs
# print(urls[1:5])

# warnings()

# #Get the link
# link_url <- "https://www.kicker.de/bundesliga/tabelle/1995-96/10/"
# # Read the HTML code from the website
# page <- read_html(link_url)

# # Extract the first two <td> elements with the specified class
# elements <- page %>% html_nodes("td.kick__table--ranking__master.kick__respt-m-o-5")

# # Extract the text content
# values <- html_text(elements) %>%
#   trimws()        # removing  white spaces
# # Print the values
# values <- as.numeric(values)
# print(values)

# first_score <- values[1]
# second_score <- values[2]

# c <- first_score - second_score
# c
# first_score
# second_score

# # Function to extract points from Bundesliga table webpage
# extract_points <- function(url) {
#   # Read webpage content (replace with a more robust scraping method if needed)
#   page <- readLines(url, warn = FALSE)
  
#   # Extract points for top 2 teams
#   points <- grep("Platz 1.*Punkte: (\\d+)", page, perl = TRUE)
#   points <- as.numeric(gsub("Platz 1.*Punkte: ", "", points))
  
#   # Check if data is available
#   if (length(points) < 2) {
#     return(c(NA, NA))
#   }
  
#   # Return points for first and second team
#   return(c(points[1], points[2]))
# }

# # Example usage
# url <- "https://www.kicker.de/bundesliga/tabelle/1996-97/30"
# points <- extract_points(url)

# # Print the points
# cat("Points of First Team:", points[1], "\n")
# cat("Points of Second Team:", points[2])


# # Function to calculate the point difference
# point_difference <- function(season, matchday) {
  
#   # Generate the URL for the desired season and matchday
#   base_url <- "https://www.kicker.de/bundesliga/tabelle"
#   season_url <- paste(base_url, season, matchday, sep = "/")
#   # Scrape the webpage
#   webpage <- read_html(season_url)
  
#   # Extract the table data
#   table <- webpage %>%
#     html_element("table") %>%
#     html_table()
  
#   # Clean the data and extract the points for the first and second teams
#   clean_table <- table %>%
#     filter(!is.na(Platz)) %>%
#     mutate(Punkte = as.numeric(gsub("[^0-9]", "", Punkte))) # Clean the points column
  
#   # Get the points for the first and second teams
#   first_team_points <- clean_table$Punkte[1]
#   second_team_points <- clean_table$Punkte[2]
  
#   # Compute and return the point difference
#   point_diff <- first_team_points - second_team_points
#   return(point_diff)
# }

# # Vector of seasons in format "1995-96", "1996-97", ..., "2023-24"
# seasons <- paste(1995:2023, (1995:2023) + 1, sep = "-")

# # Initialize an empty list to store point differences
# point_differences <- numeric(length(seasons))

# # Loop over each season to get the point difference on matchday 34
# for (i in seq_along(seasons)) {
#   point_differences[i] <- point_difference(season = seasons[i], matchday = 34)
# }