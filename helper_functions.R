# Some basic helper functions for working with the Trove API.

library(tidyverse)
library(httr)
library(magrittr)

simpleSearch <- function(searchString, key = api_key, full_text = T, s = "*", zone = "newspaper") {
  ##
  # Queries the Trove API using the provided search string.
  #
  # Paramaters:
  #    - searchString (chr): A string with the search parameters.
  #    - key (chr): Your Trove API key
  #    - full_text (bool): If true, returns full text of results. If not, metadata only.
  #    - s (chr): The 'cursor' returned from a previous search. (see below)
  #    - zone (chr): The zone to search. Defaults to 'newspaper'
  #
  # Returns:
  #    - out (list): List of 3:
  #         - 'results' (tbl): A tibble with metadata and article text
  #         - 's' (chr): Cursor position for the next tranche of results.
  #         - 'total' (int): The total number of results found
  #
  # The cursor: The API only returns 20 results at a time. To retrieve
  # the next 20 results, you need to call the same query again, providing
  # the code for the start of the next tranche.
  ##
  
  # Constant: url of the API
  root <- "https://api.trove.nla.gov.au/v2/result"
  
  # Build query
  query <- list()
  query$q <- searchString
  query$key <- key
  query$zone <- zone
  if (full_text == T) {
    query$include = "articletext"
  }
  query$s = s
  query$bulkHarvest = "true"
  
  # Request results from server:
  resp <- GET(url = root, query = query)
  
  # Check success.
  if (status_code(resp) >= 300) {
    message_for_status(resp, task = "download records from Trove")
    return(resp)
  }
  
  # Helper function for processing results
  .drill_down <- function(x) {
    .out <- tibble(
      id = x$id,
      heading = x$heading,
      category = x$category,
      newspaper_id = x$title$id,
      newspaper_name = x$title$value,
      date = x$date
    )
    
    if (full_text == T) {
      .out$text <- x$articleText
    }
    
    return(.out)
  }
  
  # Shape results into tibble
  results <- content(resp, as = "parsed") %>%
    flatten() %>% # Drill down through response until you reach the list of articles
    .$zone %>%
    .[[1]] %>%
    .$records %>%
    .$article %>%
    map_dfr(.drill_down)
  
  # And save the cursor to access the next page of results
  s <- content(resp)$response$zone[[1]]$records$nextStart
  total <- content(resp)$response$zone[[1]]$records$total %>% as.integer()
  
  out = list(results = results, s = s, total = total)
  
  return(out)
}

countResults <- function(searchString, key = api_key, zone = "newspaper") {
  ##
  # Queries the Trove API using the provided search string.
  # Simply returns the number of results.
  #
  # Paramaters:
  #    - searchString (chr): A string with the search parameters.
  #    - key (chr): Your Trove API key
  #
  # Returns:
  #    - 'total' (int): the number of results returned
  #    - 'years': the results broken down by year
  ##
  # Constant: url of the API
  root <- "https://api.trove.nla.gov.au/v2/result"
  
  # Request results from server:
  resp <- GET(
    root,
    query = list(
      q = searchString,
      key = key,
      zone = zone
    )
  )
  
  # Check success.
  if (status_code(resp) != "200") {
    message_for_status(resp, task = "download results from Trove.")
    return(resp)
  }
  
  # Retrieve the total
  total <- content(resp)$response$zone[[1]]$records$total %>% as.integer()
  
  return(total)
}

getNewspaperMetadata <- function(key = api_key) {
  ##
  # Downloads up-to-date list of all newspapers in Trove.
  ##
  
  out <- GET(
    "https://api.trove.nla.gov.au/v2/newspaper/titles?",
    query = list(key = api_key)
  ) %>%
    content() %>%
    .$response %>%
    .$records %>%
    .$newspaper %>%
    map(function(x) {
      x$issn <- as.character(x$issn)
      return(x)
    }) %>%
    map_dfr(as_tibble)
  
  return(out)
}

harvest <- function(searchString, key = api_key, full_text = T, s = "*", zone = "newspaper") {
  ##
  # Retrieves all the results for a given query, managing the 100
  # calls per minute limit.
  #
  # Parameters:
  #    - As for simpleSearch above
  #
  # Returns:
  #    - out (tbl): a tibble with one row per article.
  ##
  
  # Get total number of results:
  total <- countResults(searchString, key, zone)
  if (total < 1) {
    stop("No results found.")
  }
  message(total, " records found. Downloading...")
  
  # Begin loop:
  i <- 0 # Set iteration number
  out_rows <- 0 # Set row number
  s <- "*" # Set cursor to initial position
  results_tbl <- tibble(
    id = character(),
    heading = character(),
    category = character(),
    newspaper_id = character(),
    newspaper_name = character(),
    date = character()
  )
  if (full_text == T) {
    results_tbl$text = character()
  }
  
  # Set up timer
  tick <- Sys.time()
  
  while (out_rows < total) {
    # Get next tranche
    next_tranche <- simpleSearch(searchString, key, full_text, s, zone)
    # Accumulate results:
    results_tbl <- bind_rows(results_tbl, next_tranche$results)
    
    # Update iteration variables
    i <- i + 1 # iteration
    s <- next_tranche$s # cursor
    out_rows <- nrow(results_tbl) # results accumulated
    
    # Every 99 iterations, print message and wait one minute
    if (i %% 99 == 0) {
      tock <- Sys.time()
      elapsed <- difftime(tock, tick, units = "mins") %>% as.numeric() %>% ceiling()
      message(out_rows, "/", total, " downloaded. ", elapsed, " minutes elapsed.")
      Sys.sleep(elapsed %% 60 + 1) # Sleep for the rest of the minute plus one second for safety
    }
  }
  
  tock <- Sys.time()
  elapsed <- difftime(tock, tick, units = "mins") %>% as.numeric %>% round(digits = 2)
  message(out_rows, "/", total, " downloaded. ", elapsed, " minutes elapsed.")
  
  # At the end, return the goods!
  out <- list(
    query = searchString,
    total = nrow(results_tbl),
    elapsed = elapsed,
    results = results_tbl
  )
  return(out)
  
}
