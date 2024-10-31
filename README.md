This is the R / Shiny app code for the U2 Tour Map Shiny app that is deployed at: 

https://qjxo03-hans-van0leeuwen.shinyapps.io/u2_tour_map/

Note: the Shiny app needs a data file that has not been committed to Github as that data belongs to u2gigs.com. I had permission to use the data for my Google Data Analytics Capstone Project. The data file used by the Shiny app and the columns in the file are described below.

u2data_all_shows_clean_final.csv

$ head u2data_all_shows_clean_final.csv 
"showID","tour","leg","date","venue","city","state","country","song_position","snippet","encore","song_title","show_url","song_url","song_lyrics"

> u2data <- read_csv("u2data_all_shows_clean_final.csv")
Rows: 39653 Columns: 15                                                                          
── Column specification ─────────────────────────────────────────────────────────────────────────
Delimiter: ","
chr  (10): tour, leg, venue, city, state, country, song_title, show_url, song_url, song_lyrics
dbl   (2): showID, song_position
lgl   (2): snippet, encore
date  (1): date
