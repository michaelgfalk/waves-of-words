# Some basic helper functions for working with the Trove API.

library(fastmatch)
library(tidyverse)
library(httr)
library(magrittr)
library(DBI)
library(RPostgreSQL)

simpleSearch <- function(searchString, key = api_key, s = "*", zone = "newspaper") {
  ##
  # Queries the Trove API using the provided search string.
  #
  # Paramaters:
  #    - searchString (chr): A string with the search parameters.
  #    - key (chr): Your Trove API key
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
  query$include = "articletext"
  query$s = s
  query$bulkHarvest = "true"
  
  # Request results from server:
  resp <- GET(url = root, query = query)
  
  # Check success.
  if (status_code(resp) >= 300) {
    message_for_status(resp, task = "download records from Trove")
    return(list(resp = resp, flag = "downloadError"))
  }
  
  # Get information about overall search results
  info <- content(resp)$response$zone[[1]]$records
  if ('nextStart' %in% names(info)) {
    s <- info$nextStart # get cursor for next page
  } else {
    s <- NA # if there is no cursor, return NA
  }
  total <- info$total %>% as.integer()
  
  # Shape results into tibble
  # Get the list of articles from the response
  articles <- content(resp, as = "parsed") %>%
    flatten() %>%
    .$zone %>%
    .[[1]] %>%
    .$records %>%
    .$article
  
  # Check status of articles
  status <- articles %>%
    map_lgl(function(x) {!'status' %in% names(x)})
  
  # If there are no articles with text, silently return the status info
  if (sum(status) == 0) {
    return(list(searchString = searchString, results = tibble(var = as.character()), s = s, total = total, resp = resp, flag = "statusError"))
  }
  
  # Wrap in a try() so that the harvest won't break if there's an error
  drilled_down <- try({
    results <- articles %>%
      map_if(
        .p = status, # Only apply to articles without a status (i.e. with text)
        .f = .drill_down, # Drill in and reshape to tibble
        .else = function(x) return(NULL) # Else return NULL
        ) %>%
      bind_rows()
  })
  
  # If there is an error, return a message and the response
  if (class(drilled_down)[1] == "try-error") {
    message("Drill down failed at s = ", s)
    return(list(resp = resp, s = s, flag = "parseError"))
  }
  
  out = list(searchString = searchString, results = results, s = s, total = total, resp = resp, flag = "success")
  
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
  results_tbl <- tibble(
    art_id = character(),
    heading = character(),
    category = character(),
    newspaper_id = character(),
    date = character()
  )
  if (full_text == T) {
    results_tbl$text = character()
  }
  
  # Set up timer
  tick <- Sys.time()
  
  while (out_rows < total) {
    # Wait 390 milliseconds (API limit = 100 calls/min)
    Sys.sleep(0.39)
    
    # Get next tranche 
    next_tranche <- simpleSearch(searchString, key, full_text, s, zone)
    
    # Handle results of search
    if (class(next_tranche) == "response") {
      message("Download failed. Returning results so far.")
      return(list(
        searchString = searchString,
        results_tbl = results_tbl,
        s = s
      ))
    }
    
    # Accumulate results:
    results_tbl <- bind_rows(results_tbl, next_tranche$results)
    
    # Update iteration variables
    i <- i + 1 # iteration
    s <- next_tranche$s # cursor
    out_rows <- nrow(results_tbl) # results accumulated
    
    if (i %% 100 == 0) {
      tock <- Sys.time()
      elapsed <- difftime(tock, tick, units = "mins") %>% as.numeric() %>% round(digits = 2)
      message(out_rows, "/", total, " downloaded. ", elapsed, " minutes elapsed.")
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

clean_text <- function(trove_tbl, stopwords) {
  # Strips out html tags and stopwords from a tbl of Trove results
  
  out <- trove_tbl %>%
    mutate(
      # Strip out html markup
      text = str_remove_all(text, "<[^>]*>"),
      # Cut trailing and repeated whitespace
      text = str_squish(text),
      # Tokenise
      tokens = str_to_lower(text),
      tokens = str_remove_all(tokens, "[:punct:]"),
      # Tokenize
      tokens = str_split(tokens, " "),
      # Delete stopwords and truncate long strings
      tokens = map(tokens, function(x) x[! x %fin% stopwords]),
      tokens = map(tokens, str_trunc, width = 255),
      # Reformat
      tokens = map(tokens, unlist, use.names = F, recursive = F),
      tokens = map_chr(tokens, paste0, collapse = ","),
      tokens = paste0("{",tokens,"}"),
      # Now we need to clean the columns that have a maximum string length
      heading = str_trunc(heading, 255),
      category = str_trunc(category, 255)
      )
  
  return(out)
}

get_elapsed <- function(tock, tick) {
  elapsed <- difftime(tock, tick)
  out <- list()
  out$num <- elapsed %>% as.numeric() %>% round(digits = 2)
  out$units <- elapsed %>% attributes() %>% .$units
  return(out)
}

harvest_articles_into_db <- function(searchString, conn, key = api_key, s = "*", max_iter = Inf, stopwords, verbosity = 100) {
  ##
  # Harvests full-text newspaper results from Trove, preprocesses them, and
  # enters them into a Postgres database
  #
  # Parameters:
  #    - As for simpleSearch above, plus
  #    - conn (PostgreSQLConnection): connection to a properly-formed PostGres database
  #    - max_iter (arbitrary level to stop the iterations)
  #    - verbosity (numeric): how often to report on progress (defaults to every 100 iterations)
  #
  # Returns:
  #    - out (lst): a list of information about the harvest
  ##
  
  # Define database structure:
  trove_article_fields <- c(
    art_id = "integer",
    heading = "character varying (255)",
    category = "character varying (255)",
    newspaper_id = "integer",
    date = "date",
    text = "text",
    tokens = "character varying (255)[]"
  )
  
  # Create list to accumulate errors
  error_list <- list()
  err_idx <- 0
  
  # Create hash table of stopwords
  stpwrd_hash <- fmatch.hash("foo", stopwords)
  
  # Get total number of results:
  total <- countResults(searchString, key, zone = "newspaper")
  if (total < 1) {
    stop("No results found.")
  }
  message(total, " records found. Downloading...")
  global_tick <- Sys.time()
  Sys.sleep(0.6) # Wait to ensure we don't breach the download limit.
  
  # Define iteration variables
  it <- list()
  it$i <- 1 # iteration number
  it$go <- TRUE # flag to continue loop
  it$out_rows <- 0 # accumulator for downloaded records
  it$s <- s # cursor for next page of results
  
  # Define iterator function
  iterate <- function(it, next_tranche, tick, global_tick, verbosity, total) {
    
    # Stop timer
    tock <- Sys.time()
    
    # Print message as required
    if (it$i %% verbosity == 0) {
      elapsed <- get_elapsed(tock, global_tick)
      message(it$out_rows, "/", total, " downloaded. ", elapsed$num, " ", elapsed$units, " elapsed.")
    }
    
    # Update iterator variables
    it$i <- it$i + 1
    if (is.na(next_tranche$s)) {
      it$go <- FALSE # End the loop if there is no nextStart returned from the server
    }
    it$s <- next_tranche$s # cursor
    it$out_rows <- it$out_rows + nrow(next_tranche$results) # results accumulated
    
    # Pause to ensure API limit isn't breached.
    iter_duration <- difftime(tock, tick, units = "secs") %>% as.numeric()
    Sys.sleep(0.6 - min(0.6, iter_duration))
    
    # Return iteration variables
    return(it)
  }
  
  while (it$go & it$i < max_iter) {
    # Start timer
    tick <- Sys.time()
    
    # Remember last s
    last_s <- it$s
    
    # Get next tranche
    next_tranche <- simpleSearch(searchString, key,
                                   s = it$s, zone = "newspaper")
    
    # Handle the results of the simpleSearch
    if (next_tranche$flag == "downloadError") {
      # Stop execution and return the results so far.
      message("Download failed on iteration ", it$i, ". Returning error log.")
      return(list(error_list = error_list, problem_s = last_s, lastResp = next_tranche$resp, lastError = next_tranche$flag))
    } else if (next_tranche$flag == "parseError") {
      # Save error into log and continue the harvest
      message("Parse failed on iteration ", it$i, ". Response object saved, execution continuing.")
      # Add error to list
      err_idx <- err_idx + 1 # iterate error index
      next_tranche$problem_s <- last_s # add problematic cursor to next_tranche
      error_list[[err_idx]] <- next_tranche # store in error list
      
      # Iterate
      it <- iterate(it, next_tranche, tick, global_tick, verbosity, total)
      next
    } else if (next_tranche$flag == "statusError") {
      # Fail silently and move on
      err_idx <- err_idx + 1
      next_tranche$problem_s <- last_s # save problematic cursor
      error_list[[err_idx]] <- next_tranche # store in error list
      
      # Iterate
      it <- iterate(it, next_tranche, tick, global_tick, verbosity, total)
      next
    }
    
    # Accumulate results in the database:
    # try() will return TRUE if the insert worked, and an invisible
    # object of class "try-error" if it failed.
    write_to_db <- try(
      {next_tranche$results %>%
        clean_text(stpwrd_hash) %>%
        dbWriteTable(conn, "trove_article", .,
                     row.names = F,
                     overwrite = F,
                     append = T,
                     field.types = trove_article_fields)},
      silent = T
    )
    
    # If the write failed, break out of the function, returning the last
    # tranche of results and all the necessary info:
    if (class(write_to_db) == "try-error") {
      message(paste0(
        "There was an error writing to the database at iteration ", it$i, ":\n",
        write_to_db[1],
        "\n\nReturning data downloaded from Trove."
      ))
      return(list(
        next_tranche = next_tranche,
        searchString = searchString,
        articles_harvested = it$out_rows,
        problem_s = last_s,
        s = next_tranche$s,
        error_list = error_list,
        db_write_error = write_to_db[1]
        ))
    }
    
    # Iterate
    it <- iterate(it, next_tranche, tick, global_tick, verbosity, total)
  }
  
  tock <- Sys.time()
  elapsed <- get_elapsed(tock, global_tick)
  message("Finished! ", it$out_rows, "/", total, " downloaded. ", elapsed$num, " ", elapsed$units, " elapsed.")
  
  # At the end, return some information about the harvest.
  out <- list(
    searchString = searchString,
    articles_harvested = it$out_rows,
    elapsed = elapsed,
    s = it$s,
    error_list = error_list
  )
  return(out)
  
}

.drill_down <- function(x) {
  # Initialise list of columns
  # Sometimes Trove returns a NULL for one or other field.
  .null_check <- function(y) {
    if (is.null(y)) {
      return(NA)
    } else {
      return(as.character(y)) # coerce to character to avoid parsing problems
    }
  }
  
  .out <- list( # quicker to create list than tibble
    art_id = .null_check(x$id),
    heading = .null_check(x$heading),
    category = .null_check(x$category),
    newspaper_id = .null_check(x$title$id),
    date = .null_check(x$date),
    text = .null_check(x$articleText)
  )
  
  return(.out)
}

write_resp_to_db <- function(resp, conn, stopwords, full_text = T) {
  # Extracts data from response and writes it to the wow database
  
  # Column spec
  trove_article_fields <- c(
    art_id = "integer",
    heading = "character varying (255)",
    category = "character varying (255)",
    newspaper_id = "integer",
    date = "date",
    text = "text",
    tokens = "character varying (255)[]"
  )
  
  # Write to db
  resp %>%
    content() %>%
    flatten() %>% # Find the list of articles
    .$zone %>%
    .[[1]] %>%
    .$records %>%
    .$article %>%
    map_dfr(.drill_down) %>%
    clean_text(stopwords) %>%
    dbWriteTable(conn, "trove_article", .,
                 row.names = F,
                 overwrite = F,
                 append = T,
                 field.types = trove_article_fields)
    
  return("Tbl written to database!")
    
}
