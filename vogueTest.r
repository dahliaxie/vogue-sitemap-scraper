library(httr)
library(rvest)
library(stringr)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)  # For plotting

# Define the URL for the base sitemap
base_url <- "https://www.vogue.com/sitemap.xml"

# Define a list of designer brands
designers <- c("Gucci", "Chanel", "Louis Vuitton", "Dior", "Prada")

# Function to fetch sitemap URLs and their lastmod dates
fetch_sitemap_urls <- function(base_sitemap_url) {
  response <- GET(base_sitemap_url)
  
  if (status_code(response) == 200) {
    page <- read_html(response)
    locs <- page %>% html_nodes("loc") %>% html_text()
    lastmod <- page %>% html_nodes("lastmod") %>% html_text()
    
    sitemap_info <- data.frame(
      url = locs,
      lastmod = as.POSIXct(lastmod, format="%Y-%m-%d", tz="UTC")
    )
    
    return(sitemap_info)
  } else {
    stop("Failed to fetch sitemap URLs.")
  }
}

process_articles <- function(sitemap_url) {
  all_content <- vector()
  article_dates <- vector()
  
  print(paste("Fetching weekly sitemap:", sitemap_url))
  response <- GET(sitemap_url)
  
  if (status_code(response) == 200) {
    page <- read_html(response)
    locs <- page %>% html_nodes("loc") %>% html_text()
    lastmods <- page %>% html_nodes("lastmod") %>% html_text()
    
    # Convert lastmod dates to POSIXct
    article_dates <- as.POSIXct(lastmods, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
    
    # Process each article
    for (article_url in locs) {
      article_response <- GET(article_url)
      
      if (status_code(article_response) == 200) {
        article_page <- read_html(article_response)
        
        # Extract article content from the specified CSS container
        content <- article_page %>% 
          html_nodes("div.body__inner-container") %>% 
          html_text() %>% 
          paste(collapse = " ")
        
        # Append to the contents vector
        all_content <- c(all_content, content)
      } else {
        print(paste("Failed to fetch article content from:", article_url))
      }
    }
  } else {
    print(paste("Failed to fetch weekly sitemap content from:", sitemap_url))
  }
  
  all_content <- paste(all_content, collapse = " ")
  
  # Count mentions of each designer
  designer_counts <- sapply(designers, function(designer) {
    count <- length(str_extract_all(tolower(all_content), tolower(designer))[[1]])
    return(count)
  })
  
  # Create a data frame with the counts
  counts_df <- data.frame(Designer = designers, Count = designer_counts)
  return(list(count_df = counts_df, article_dates = article_dates))
}

# Fetch the sitemaps
sitemap_info <- fetch_sitemap_urls(base_url)

# Get the two most recent weekly sitemap URLs
most_recent_sitemaps <- sitemap_info[order(sitemap_info$lastmod, decreasing = TRUE), ]
most_recent_sitemaps <- most_recent_sitemaps[1:3, ]

# Initialize an empty list to hold counts for each week
counts_list <- list()

# Process articles from each of the two most recent weekly sitemaps separately
for (i in 1:nrow(most_recent_sitemaps)) {
  sitemap_url <- most_recent_sitemaps$url[i]
  
  # Process the articles for this week and get the article dates
  results <- process_articles(sitemap_url)
  week_counts <- results$count_df
  article_dates <- results$article_dates
  
  # Calculate the week start and end dates based on article dates
  if (length(article_dates) > 0) {
    week_start_date <- min(article_dates)
    week_end_date <- most_recent_sitemaps$lastmod[i]
    
    # Store the counts with the week date range as a column
    week_counts$Week <- paste(format(week_start_date, "%Y-%m-%d"), "to", format(week_end_date, "%Y-%m-%d"))
    counts_list[[i]] <- week_counts
  }
}

# Combine all week counts into a single data frame
combined_counts <- do.call(rbind, counts_list)

# Ensure combined_counts is a data frame
combined_counts <- as.data.frame(combined_counts)

# Reshape the data frame for side-by-side comparison
combined_counts_wide <- combined_counts %>%
  pivot_wider(names_from = Week, values_from = Count, values_fill = list(Count = 0))

# Print the combined data frame for comparison
print("Designer counts comparison:")
print(combined_counts_wide)

# Calculate percentage change between the two weeks
# Identify the column names for the weeks
week_cols <- colnames(combined_counts_wide)[-1]  # Exclude the Designer column

# Ensure weeks are in chronological order
week_cols <- sort(week_cols)

# Calculate the percentage change
percentage_changes <- combined_counts_wide %>%
  gather(key = "Week", value = "Count", -Designer) %>%
  spread(key = "Week", value = "Count") %>%
  mutate(Percentage_Change = (.[[week_cols[length(week_cols)]]] - .[[week_cols[1]]]) / .[[week_cols[1]]] * 100)

# Print the percentage changes
print("Percentage Change in Designer Mentions:")
print(percentage_changes)
# Reshape the data frame for plotting
combined_counts_long <- combined_counts %>%
  pivot_longer(cols = Count, names_to = "variable", values_to = "value") %>%
  select(-variable)

# Print the long format data frame for verification
print("Combined Counts Long Data Frame:")
print(head(combined_counts_long))

# Plot the data
ggplot(combined_counts_long, aes(x = Week, y = value, color = Designer, group = Designer)) +
  geom_line() +
  geom_point() +
  labs(title = "Change in Designer Mentions Over Weeks",
       x = "Week",
       y = "Mentions",
       color = "Designer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
