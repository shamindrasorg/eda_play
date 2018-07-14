#-------------------------------------------------------------------------------
# CLEAN UP
#-------------------------------------------------------------------------------
cat("\014") # Clean console

# TODO: Check whether to remove this for final version!
rm(list = ls()) # Clean all objects

#-------------------------------------------------------------------------------
# SETUP
# purpose: load all libraries
#          load the main directory names (TODO: use the 'here' package by
#                                               Jenny Bryan)
#-------------------------------------------------------------------------------

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
# Source: https://gist.github.com/stevenworthington/3178163#file-ipak-r

ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# Install and load the CRAN pacakages
packages <- c("tidyverse", "devtools", "xml2", "rvest")
ipak(packages)

# Install and load the required devtools packages
# TODO: develop ipak for devtools (combine with single ipak)
#       pass in devtools only packages in one go
devtools::install_github("krlmlr/here")
library(here)

# see here: https://github.com/jennybc/here_here
# Define the data directories
raw_data_dir <- here::here("data/raw_data/office_quotes")

#-------------------------------------------------------------------------------
# EXPLORE the website - make a plan of attack
#-------------------------------------------------------------------------------

# The main office episodes website containing the required data
# This has been downloaded manually and saved in the "raw_data_dir"
SRC_URL <- "http://www.officequotes.net/index.php"

# NOTES:
# - There are basically 4 panels on each page - nice!
# - One top horizontal header panel and three vertical panels below
# - The left side panel contains all of the episode list and the links to the
#   episode transcripts
# Let's start scraping!

# TODO: generalize this manual reference to the source html later
main_src_html <- file.path(raw_data_dir
                           , "office_quotes_main_20170805.html")
main_src_html

get_episode_links <- xml2::read_html(main_src_html) %>%
    rvest::html_nodes(".navEp a") %>%
    rvest::html_attr("href")

# Let's examine it
head(x = get_episode_links, n = 10)
str(get_episode_links)
length(get_episode_links)
tail(x = get_episode_links, n = 10)

eps_only       <- stringr::str_detect(string = get_episode_links
                                      , pattern = "no")
eps_only_links <- get_episode_links[eps_only]
eps_only_links

# TODO: remove this auxillary function and make it an
#       anonymous function in the purrr::map_chr
# Auxillary function to get the numeric episode reference
# e.g. "http://www.officequotes.net/no9-22.php" --> "no9-22"
# TODO: perhaps we can consolidate this into a tibble directly!
#       e.g. https://jennybc.github.io/purrr-tutorial/ls13_list-columns.html#call_the_api_of_ice_and_fire
#       might get a bit overwhelming for the user
f_get_ep_ref <- function(x){
    stringr::str_extract(string = x, pattern = "no\\d\\-\\d{2}")
}

# Apply the function as required
eps_ref <- purrr::map_chr(.x = eps_only_links, ~ f_get_ep_ref(.x))

# Extract the season
eps_season_num <- stringr::str_extract(string = eps_ref
                                       , pattern = "no\\d") %>%
    stringr::str_replace(string = ., pattern = "no", replacement = "")

eps_season_num

# Extract the episode
eps_episode_num <- stringr::str_replace(string = eps_ref
                                        , pattern = "no\\d\\-"
                                        , replacement = "") %>%
    as.numeric()

eps_season_num

eps_details <- tibble::tibble(eps_ref = eps_ref
                              , episode_src_url = eps_only_links
                              , season_season_num = eps_season_num
                              , episode_num = eps_episode_num)

# Check the dataframe
View(head(eps_details))

# TODO: make tibble names and the columns consistent
# e.g. 'eps_episode_num' --> 'episode_num'

#-------------------------------------------------------------------------------
# DOWNLOAD Data and scrape out the quotes in a tidy data frame
#-------------------------------------------------------------------------------

# TODO: change this to be generic for any ep later
first_ep <- eps_details %>%
    dplyr::slice(1) %>%
    dplyr::select(episode_src_url)

# TODO: This should come directly from the url, not saved data
first_ep_raw_path <- here::here("data/raw_data/n01-01.html")

first_ep_html <- xml2::read_html(first_ep_raw_path)

first_ep_html %>%
    rvest::html_nodes(".quote b") %>%
    rvest::html_attr("td")

first_ep_html %>%
    rvest::html_nodes("div.quote") %>%
    rvest::html_attr("td")


/html/body/table/tbody/tr[2]/td[2]/div[7]/text()[3]
/html/body/table/tbody/tr[2]/td[2]/div[7]/text()[5]

/html/body/table/tbody/tr[2]/td[2]/div[7]/text()[1]

first_ep_html %>%
    rvest::html_nodes(".quote") %>%
    head(1) %>%
    toString()

first_ep_html %>%
    rvest::html_nodes(".quote") %>%
    head(1) %>%
    toString()

first_ep_html %>%
    rvest::html_nodes(".quote") %>%
    rvest::html_text()