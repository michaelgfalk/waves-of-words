####
#
#  WAVES OF WORDS: Mapping and Modelling Australia's Pacific Past
#
#  Project: Trove Project
#
#  This script: harvesting from Trove directly into a local Postgresql database
#
#  I have now set up a Postgresql database on my machine, and written the necessary
#  code to get Trove articles into it. The script preprocesses the text as it goes
#  in. It should hopefully not run into too many errors!
#
####

# Load helper functions, stopword list and API key
source('init.R')

# Connect to database
wow_db <- DBI::dbConnect(DBI::dbDriver("PostgreSQL"),
                                   user = "michaelgfalk",
                                   host = "127.0.0.1",
                                   password = rstudioapi::askForPassword(),
                                   dbname = "waves_of_words")

# And start to harvest...

# 2019-03-15: 'aboriginal' -- the first 6000 iterations should take a bit more than
# an hour
harvest_20190315 <- harvest_articles_into_db("aboriginal", wow_db, api_key, "*",
                                             max_iter = 6000, english_stopwords)
# Indeed, it took 1.3 hours (i.e. 1 hour 18 minutes) at home.
# harvest_20190315$s == "AoEpMTI4Mjg3MjEx"
harvest_20190315_1 <- harvest_articles_into_db("aboriginal", wow_db, api_key,
                                               harvest_20190315$s,
                                               Inf, english_stopwords)
# This one failed after approx 4,000 iterations, because simpleSearch was
# unable to parse the results:
#       Column `heading` can't be converted from character to integer
# Not sure what caused that... I've wrapped the call to simpleSearch in
# a try(), so we can debug this next week...

# I stupidly lost the nextStart from that last harvest....
# Argh!! Starting again from scratch.
# Remember to save the harvest info this time. Also.
harvest_20190320_0 <- harvest_articles_into_db("aboriginal", wow_db, api_key, "*",
                                               max_iter = 11000, english_stopwords)
saveRDS(harvest_20190320_0, "research/trove_results/harvest_20190320_0.rds")

# There were two responses that failed to parse. Let's check them out.
fail_1 <- content(harvest_20190320_0$error_list[[1]]$resp)
fail_2 <- content(harvest_20190320_0$error_list[[2]]$resp)

# The bug's been fixed, so we can write these to the db...
write_resp_to_db(
  resp = harvest_20190320_0$error_list[[1]]$resp,
  conn = wow_db,
  stopwords = english_stopwords
)
write_resp_to_db(
  resp = harvest_20190320_0$error_list[[2]]$resp,
  conn = wow_db,
  stopwords = english_stopwords
)

# Harvest the rest of the articles
harvest_20190320_1 <- harvest_articles_into_db("aboriginal", wow_db, api_key,
                                               harvest_20190320_0$s,
                                               max_iter = 20000,
                                               english_stopwords)
saveRDS(harvest_20190320_1, "research/trove_results/harvest_20190320_1.rds")
# This harvest ended with a database write error on iteration 19028, after 380000
# articles had been downloaded. Because of the way I'd done it, I forgot
# to include all the relevant information in the output from a db write error.
# However, we still have enough info to pick up the transaction where we
# left off, and luckily there were no errors accumulated in the error_list
# anyway.

# It turns out that the articles have the status 'coming soon'. I've now
# rewritten the harvest code, so it should silently skip articles like
# this that have no text.

# Time to finish it off! I've also rewritten the while loop condition,
# so hopefully the harvest should stop correctly when the last
# page of results is reached.
harvest_20190321_0 <- harvest_articles_into_db("aboriginal", wow_db, api_key,
                                               harvest_20190320_1$s,
                                               max_iter = 200,
                                               english_stopwords)
saveRDS(harvest_20190321_0, "research/trove_results/harvest_20190321_0.rds")

# That was a long boring process of debugging. Now hopefully the harvest
# can proceed without interruption:
harvest_20190321_1 <- harvest_articles_into_db("aboriginal", wow_db, api_key,
                                               harvest_20190321_0$s,
                                               max_iter = Inf,
                                               english_stopwords)
saveRDS(harvest_20190321_1, "research/trove_results/harvest_20190321_1.rds")

# Now to do another query...
harvest_20190322_0 <- harvest_articles_into_db("polynesian", wow_db, api_key,
                                               "*",
                                               max_iter = Inf,
                                               english_stopwords)
saveRDS(harvest_20190321_1, "research/trove_results/harvest_20190322_0.rds")


