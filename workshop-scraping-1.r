### ----------------------------------------------------------
### Workshop: An introduction to web scraping with R, I
### Simon Munzert
### Duke University, October 2014
### ----------------------------------------------------------


### preparations ---------------------------------------------

# clear workspace
rm(list=ls(all=TRUE))

# install and load packages
pkgs <- c("RCurl", "XML", "stringr", "httr", "plyr", "ggplot2")
# install.packages(pkgs)
lapply(pkgs, library, character.only=T)



### package overview ------------------------------------------

library(RCurl) # R as HTTP(S) client
  # website_content <- getURL("url.here")
  # raw_vector <- getBinaryURL("path.to.binary.content")
  # handle <- getCurlHandle(curl.options)

library(httr) # light wrapper around RCurl

library(XML) # R as HTML/XML parser
   parsed_xml_content <- xmlParse("xml.document")
   parsed_html_content <- htmlParse("html.document")
   extracted_info <- xpathSApply(parsed.xml.content, xpath.expression, xmlValue)
   table_as_data_frame <- readHTMLTable("html.document.with.table")

library(stringr) # R for string manipulation / regex 
   extracted_string <- str_extract(string, regular.expression)
   manipulated_string <- str_replace(string, regular.expression, replacement)



### a gentle beginning: scraping an HTML table ---------------

url <- "http://en.wikipedia.org/wiki/List_of_hospitals_in_North_Carolina"
foo <- readHTMLTable(url, which = 1)
head(foo)
names(foo)

# how many hospital beds in total?
sum(as.numeric(foo$`Hospital beds`))

# where are the most hospitals?
sort(table(foo$City), decreasing = TRUE)[1:10]

# where are they geographically located?
  # snippet needed to gather geo coordinates from Google Maps API 
  # library(ggmap)
  # places <- str_c(foo$Name, foo$City, sep=", ")
  # coordinates <- ldply(places, geocode)

library(ggmap)
load("nc_hospital_coords.RData")
foo$lon <- coordinates$lon
foo$lat <- coordinates$lat
nc_map <- get_map(location=c(lon=-79, lat=35), zoom=7, maptype="hybrid")
p <- ggmap(nc_map) + geom_point(data=foo, aes(x=lon, y=lat), col="red", size=3)
p



### easy-going web scraping with XPath/SelectorGadget --------

# goal: extract all story headings from front page
# tool: http://selectorgadget.com/

url <- "http://www.nytimes.com"
html_parsed <- htmlParse(url, encoding = "UTF-8")

xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "story-heading", " " ))]/a' # use SelectorGadget to generate XPath expression
headings <- xpathSApply(html_parsed, xpath, xmlValue)
headings <- str_replace_all(headings, "\\n" , " ")
headings <- str_replace_all(headings, "(  )" , "")
headings <- str_replace_all(headings, "^( )" , "")
head(headings)

# what are the top terms?
# requires the following packages installed: tm, wordcloud, RColorBrewer
source("wordCloudMaker.r")
wordCloudMaker(headings, min.freq = 2, max.words = 25, rot.per = .20, return.data = TRUE)



### accessing multiple pages by URL manipulation --------------

# goal: download press releases from Transparency International
# source: http://www.transparency.org/news/pressreleases/year/2010
# challenge: releases are scattered over several pages
# a quick-and-dirty solution: exploit URL patterns


# getPageURLs() function
getPageURLs <- function(url) {
  baseurl <- htmlParse(url)
  total_pages <- as.numeric(xpathSApply(baseurl, "//div[@id='Page']/strong[2]", xmlValue))
  max_url <- (total_pages - 1)*10
  add_url <- str_c("/P", seq(10, max_url, 10))
  urls_list <- as.list(str_c(url, add_url))
  urls_list[length(urls_list) + 1] <- url
  return(urls_list)
}

# execute getPageURLs()
url <- "http://www.transparency.org/news/pressreleases/year/2010"
urls_list <- getPageURLs(url)

# dlPages() function
dlPages <- function(pageurl, folder ,handle) {
  dir.create(folder, showWarnings = FALSE)
  page_name <- str_c(basename(pageurl), ".html")
  if (!file.exists(str_c(folder, "/", page_name))) {
    content <- try(getURL(pageurl, curl = handle))
    write(content, str_c(folder, "/", page_name))
    Sys.sleep(1)
  }
}

# execute dlPages()
handle <- getCurlHandle(httpheader = list(
             from = "slm77@duke.edu",
            'user-agent' = str_c(R.version$version.string)
))
l_ply(urls_list, dlPages, folder = "tp_index_2010", handle = handle)
list.files("tp_index_2010")

# getPressURLs() function
getPressURLs <- function(folder) {
  pages_parsed <- lapply(str_c(folder, "/", dir(folder)), htmlParse)
  urls <- unlist(llply(pages_parsed, xpathSApply, "//a/@href"))
  press_urls <- urls[str_detect(urls, "http.+/pressrelease/")]
  press_urls_list <- as.list(press_urls)
  return(press_urls_list)
}

# execute getPressURLs()
press_urls_list <- getPressURLs(folder = "tp_index_2010")
head(press_urls_list)
length(press_urls_list)

# dlPress() function
dlPress <- function(press_url, folder, handle) {
  dir.create(folder, showWarnings = FALSE)
  press_filename <- str_c(basename(press_url), ".html")
  if (!file.exists(str_c(folder, "/", press_filename))) {
    content <- try(getURL(press_url, curl = handle))
    write(content, str_c(folder, "/", press_filename))
    Sys.sleep(1)
  }
}

# execute dlPress()
handle <- getCurlHandle()
l_ply(press_urls_list, dlPress, folder = "tp_press_2010", handle = handle)
list.files("tp_press_2010")[1:3]

# why functions are useful: scrape 2014 press releases
url <- "http://www.transparency.org/news/pressreleases/year/2014"
urls_list <- getPageURLs(url)
l_ply(urls_list, dlPages, folder = "tp_index_2014", handle = handle)
press_urls_list <- getPressURLs(folder = "tp_index_2014")
l_ply(press_urls_list, dlPress, folder = "tp_press_2014", handle = handle)



### Scraping with regular expressions --------------

# the power of regular expressions
simpsons <-  "555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson,Homer5553642Dr. Julius Hibbert"
unlist(str_extract_all(simpsons, "[[:digit:]]"))
unlist(str_extract_all(simpsons, "[[:alpha:]., ]{2,}"))
unlist(str_extract_all(simpsons, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}"))

# access Queen's Speeches at
# http://www.parliament.uk/about/how/occasions/stateopening/queensspeeches/

url <- "http://www.parliament.uk/about/how/occasions/stateopening/queensspeeches/"
url_parsed <- htmlParse(url)

# gather links to speeches
links_speeches <- xpathSApply(url_parsed, '//*[(@id = "ctl00_ctl00_FormContent_SiteSpecificPlaceholder_PageContent_ctlMainBody_wrapperDiv")]//li//a', xmlGetAttr, "href")
links_speeches <- str_replace_all(links_speeches, "\\#.+", "") # remove fragment identifiers

# download speeches
dir.create("queens_speeches")
years <- rev(seq(1994,2014,1))
for (i in 1:length(years)) {
 download.file(links_speeches[i], destfile=str_c("queens_speeches/",years[i], ".html"))
 Sys.sleep(1)
}

# import speeches as plain text
queens_speeches <- lapply(str_c("queens_speeches", "/", dir("queens_speeches")), readLines)

# a quick look into the texts
sapply(queens_speeches, function(x) {sum(str_count(x, "European Union"))})
sapply(queens_speeches, function(x) {sum(str_count(x, "Lords"))})
sapply(queens_speeches, function(x) {sum(str_count(x, "Scotland|Scottish"))})
sapply(queens_speeches, function(x) {sum(str_count(x, "[Ee]conom"))})



### Next session (October 17) ----------------------

# if you plan to work with Twitter data: register for a Twitter developer account at https://dev.twitter.com/
# how to access APIs
# how to deal with JSON data
# how to scrape data from dynamic (i.e., JavaScript-enriched) web sites
# web scraping etiquette



### Homework assignment ----------------------------

# 1. Visit http://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_United_Kingdom_general_election,_1992 and extract the table containing the elected MPs int the United Kingdom general election of 1992. Which party has most 'Sirs'?

# 2. The XML file potus.xml contains biographical information on US presidents. Parse the file into an object of the R session. This works with the xmlParse() function from the XML package.
  # (a) Extract the names of all presidents!
  # (b) Extract the value of the <occupation> node for all Republican presidents.
  # (c) Extract information from the <education> nodes.
  # (d) Convert the parsed XML data into a common data.frame. 

# 3. Describe the types of strings that conform to the following regular expressions and construct an example that is matched by the regular expression.
  # (a) [0-9]+\\$
  # (b) \\b[a-z]{1,4}\\b
  # (c) . * ?\\.txt$
  # (d) \\d{2}/\\d{2}/\\d{4}
  # (e) <(.+?)>.+?</\\1>