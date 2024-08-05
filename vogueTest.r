library(httr)
library(rvest)

# Define the URL for the sitemap
url <- "https://www.vogue.com/sitemap.xml"

# Send a GET request to the URL
response <- GET(url)

# Check the HTTP status code
status_code <- status_code(response)
print(paste("Status Code:", status_code))

if (status_code == 200) {
  print("Base sitemap URL is accessible.")
  
  # Read the HTML content
  page <- read_html(response)
  
  # Extract <loc> tags from the base sitemap
  locs <- page %>% html_nodes("loc") %>% html_text()
  
  # Print out the total number of <loc> values extracted
  print(paste("Total <loc> values found:", length(locs)))
  
  # Get the last 10 <loc> values
  recent_10_locs <- head(locs, 10)
  
  # Print the last 10 <loc> values
  print("Recent 10 <loc> values:")
  print(recent_10_locs)
  
  # Process each <loc> URL
  for (loc_url in recent_10_locs) {
    # Send a GET request to each <loc> URL
    loc_response <- GET(loc_url)
    
    # Check the HTTP status code
    loc_status_code <- status_code(loc_response)
    print(paste("Status Code for", loc_url, ":", loc_status_code))
    
    if (loc_status_code == 200) {
      print(paste("URL is accessible:", loc_url))
      
      # Read the HTML content of the <loc> URL
      loc_page <- read_html(loc_response)
      
      # Extract article URLs from the sub-sitemap
      article_locs <- loc_page %>% html_nodes("url loc") %>% html_text()
      
      # Print out the article URLs
      #print(paste("Article URLs from", loc_url, ":"))
      #print(article_locs)
      
      # Process each article URL to get the <h1> headings
      for (article_url in article_locs) {
        # Send a GET request to each article URL
        article_response <- GET(article_url)
        
        # Check the HTTP status code
        article_status_code <- status_code(article_response)
        print(paste("Status Code for", article_url, ":", article_status_code))
        
        if (article_status_code == 200) {
          #print(paste("Article URL is accessible:", article_url))
          
          # Read the HTML content of the article URL
          article_page <- read_html(article_response)
          
          # Extract <h1> headings from the article page
          headings <- article_page %>% html_nodes("h1") %>% html_text()
          
          # Print the extracted <h1> headings
          #print(paste("Headings for article:", article_url))
          print(headings)
          
        } else {
          print(paste("Article URL is not accessible:", article_url))
        }
      }
      
    } else {
      print(paste("URL is not accessible:", loc_url))
    }
  }
  
} else {
  print("Base sitemap URL is not accessible.")
}
