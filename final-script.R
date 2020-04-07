#########################################
########## Folketinget-Proejct ########## 
######################################### 

### October 2018
## Tim Runck

# This is the replication code for the 2018 folketinget project. 


rm(list = ls())

setwd("E:/R")

library(RCurl)
library(stringr)
library(XML)
library(xml2)
library(data.table)
library(rvest)
library(tidyr)
library(readr)
library(haven)
library(plyr)
library(dplyr)

#sys.sleeper
nytnyt <- function (periods =c(1,2)){
  tictoc <- runif(1, periods[1], periods[2])
  cat(paste0(Sys.time()), "- Sleeping for", round(tictoc, 2), "seconds\n")
  Sys.sleep(tictoc)
}

# base url for scraping
base.url <- "https://www.ft.dk/samling/20181/lovforslag/l129b/index.htm"

url1 <- "https://www.ft.dk/samling/20181/lovforslag/l"
url2 <- "/index.htm"

# create main archive
if(!dir.exists("ftl_archivenew")) dir.create("ftl_archivenew")

# create sub-archive for search results
archive.folder <- "./ftl_archivenew/search_archive/"
if(!dir.exists(archive.folder)) dir.create(archive.folder)


dfs <- list()

# number of pages to scrape
npages <- 200
# create progress bar
pb <- txtProgressBar(min = 1, max = npages, initial = 1, char = "-", width = 60, style = 3)

# main loop over 'npages' ft pages
for (i in 1:npages) {
  
  #update progress bar
  #setTxtProgressBar(pb, i)
  
  # build url to fetch
  final.url <- paste0(url1, i, url2)
  
  #check archive for file
  file.name <- paste0(archive.folder, "search_page_", "i", ".RData")
  if(file.exists(file.name)) {
    next
  } else {
    # print status update
    print(paste("Downloading:", i))
    
    to.save <- getURL(final.url) # download html source code
    save(to.save, file = file.name) # save downloaded source code
    nytnyt() # take a break
  }
}

ft_extract <- function(p) {
  results <- p %>% html_nodes(".tingdok-normal > span") %>% html_text(trim = TRUE)
  Regnr <- p %>% html_nodes("span.tingdok__breadcrumb-b__item") %>% html_text(trim = T)
  Heading <- p %>% html_nodes("div.tingdok-heading") %>% html_text(trim = TRUE)
  af <- which(results == "Af:")
  Stiller <- results[af + 1]
  controlling <-  p %>% html_nodes(".tingdok__caseinfotopspot-a__container a") %>% html_text(trim = TRUE)
  controlling <- str_extract_all(controlling, "\\(\\w*\\)")
  controlling <- str_remove_all(controlling, "\\(")
  controlling <- str_remove_all(controlling, "\\)")
  
  ud <- which(results == "Udvalg:")
  Udvalg <- results[ud + 1]
  
  sam <- which(results == "Samling:")
  Samling <- results[sam + 1]
  
  stat <- which(results == "Status:")
  Status <- results[stat +1]
  
  results2 <- p %>% html_nodes(" div.tingdok > div.tingdok__caseinfospot-a > div > div.tingdok-normal") %>% html_text(trim = TRUE)
  
  Reading <- if (length(grep("3. behandlet", results2, value = T)) == 0) print (NA) else (grep("3. behandlet", results2, value = T))
  Resume <- if (length(grep("lovforslaget", results2, value = T)) == 0) print (NA) else (grep("lovforslaget", results2, value = T))
  Poll <- if (length(grep("stemmer|For stemte|enstemmigt|vedtaget med|Vedtaget\n\nFor\t", results2, value = T)) == 0) print (NA) else (grep("stemmer|For stemte|enstemmigt|vedtaget med|Vedtaget\n\nFor\t", results2, value = T))
  Ministerområde <- if (length(grep("Ministerområde", results2, value = T)) == 0) print (NA) else (grep("Ministerområde", results2, value = T))
  tosplit <- (strsplit(Ministerområde, ": \r\n              \r\n              \r\n                  \r\n                      \r\n                        ")[[1]])
  Ministerområde <- tosplit[[2]]
  
  df <- list(
    Regnr = Regnr, 
    Heading = Heading, 
    Stiller = Stiller, 
    Udvalg = Udvalg,
    Samling = Samling,
    Status = Status, 
    Reading = Reading,
    Resume = Resume, 
    Poll = Poll,
    controlling = controlling, 
    Ministerområde = Ministerområde,
    stringsAsFactors = F
  )
  
}

# empty list for dataframes
dfs <- list()

# define archive folder
archive.folder <- "C:/Users/au615270/Documents/Tingdok_Project/archive/ftl_archive/"

# get list of downloaded search pages
file.list <- list.files (path = "C:/Users/au615270/Documents/Tingdok_Project/archive/ftl_archive", recursive = T)


## Main loop
for (file in file.list) {## LOOP OVER ARCHIVE
  
  # get date of respective file
  file.number <- str_extract(file, "[[:digit:]]")
  
  # print status to the screen
  print(paste("Working on:", file))
  
  # load html source code from archive
  load(paste0(archive.folder, file))
  
  # create 'xml_document' from html source code
  p <- read_html(to.save)
  # get current iteration number
  iteration <- (1 + length(dfs))
  
  # extract data from search page
  dfs[[iteration]] <- ft_extract(p)
}

workingon.data <- as.data.frame(rbindlist(dfs))

########################################################################
######################## RECODING ######################################
########################################################################


#########################STILLER#########################

workingon.data$Stiller[workingon.data$Stiller=="Privat"] <- "0"
workingon.data$Stiller[workingon.data$Stiller=="Statsminister"] <- "1"
workingon.data$Stiller[workingon.data$Stiller=="Udenrigsminister"] <- "2"
workingon.data$Stiller[workingon.data$Stiller=="Finansminister"] <- "3"
workingon.data$Stiller[workingon.data$Stiller=="?konomi- og Budgetminister"] <- "4"
workingon.data$Stiller[workingon.data$Stiller=="?konomiminister"] <- "5"
workingon.data$Stiller[workingon.data$Stiller=="Arbejdsminister"] <- "6"
workingon.data$Stiller[workingon.data$Stiller=="Justitsminister"] <- "7"
workingon.data$Stiller[workingon.data$Stiller=="Fiskeriminister"] <- "8"
workingon.data$Stiller[workingon.data$Stiller==" Min. Ud?k+EF+Norden"] <- "9"
workingon.data$Stiller[workingon.data$Stiller=="Min. Mark+Norden"] <- "10"
workingon.data$Stiller[workingon.data$Stiller=="Boligminister"] <- "11"
workingon.data$Stiller[workingon.data$Stiller=="Indenrigsminister"] <- "12"
workingon.data$Stiller[workingon.data$Stiller=="Kulturminister"] <- "13"
workingon.data$Stiller[workingon.data$Stiller=="Gr?nlandsminister"] <- "14"
workingon.data$Stiller[workingon.data$Stiller=="Forsvarsminister"] <- "15"
workingon.data$Stiller[workingon.data$Stiller=="Min. off. arb. og forurening"] <- "16"
workingon.data$Stiller[workingon.data$Stiller==" Min. offentlige arbejder"] <- "17"
workingon.data$Stiller[workingon.data$Stiller=="Socialminister"] <- "18"
workingon.data$Stiller[workingon.data$Stiller=="Undervisningsminister"] <- "19"
workingon.data$Stiller[workingon.data$Stiller=="Landbrugsminister"] <- "20"
workingon.data$Stiller[workingon.data$Stiller=="Kirkeminister"] <- "21"
workingon.data$Stiller[workingon.data$Stiller=="Handelsminister"] <- "22"
workingon.data$Stiller[workingon.data$Stiller=="Milj?minister"] <- "23"
workingon.data$Stiller[workingon.data$Stiller=="Skatte- og afgiftsminister"] <- "24"
workingon.data$Stiller[workingon.data$Stiller=="Budgetminister"] <- "25"
workingon.data$Stiller[workingon.data$Stiller=="Min. uden portef?lje"] <- "26"
workingon.data$Stiller[workingon.data$Stiller=="Industriminister"] <- "27"
workingon.data$Stiller[workingon.data$Stiller=="Energiminister"] <- "28"
workingon.data$Stiller[workingon.data$Stiller=="Min. nordiske anliggender"] <- "29"
workingon.data$Stiller[workingon.data$Stiller=="Sundhedsminister"] <- "30"
workingon.data$Stiller[workingon.data$Stiller=="Trafik- og Kommunikationsminister"] <- "31"
workingon.data$Stiller[workingon.data$Stiller=="Kommunikationsminister"] <- "32"
workingon.data$Stiller[workingon.data$Stiller=="Erhvervsminister"] <- "33"
workingon.data$Stiller[workingon.data$Stiller=="Milj?- og energiminister"] <- "34"
workingon.data$Stiller[workingon.data$Stiller=="Landbrugs- og fiskeriminister"] <- "35"
workingon.data$Stiller[workingon.data$Stiller=="F?devareminister"] <- "36"
workingon.data$Stiller[workingon.data$Stiller=="Forskningsminister"] <- "37"
workingon.data$Stiller[workingon.data$Stiller=="Trafikminister"] <- "38"
workingon.data$Stiller[workingon.data$Stiller=="Minister for ligestilling"] <- "40"
workingon.data$Stiller[workingon.data$Stiller=="IT- og forskningsminister "] <- "41"
workingon.data$Stiller[workingon.data$Stiller=="Indenrigs- og sundhedsminister"] <- "42"
workingon.data$Stiller[workingon.data$Stiller=="Videnskabsminister"] <- "43"
workingon.data$Stiller[workingon.data$Stiller=="?konomi- og erhvervsminister"] <- "44"
workingon.data$Stiller[workingon.data$Stiller=="Besk?ftigelsesminister"] <- "45"
workingon.data$Stiller[workingon.data$Stiller=="Integrationsminister"] <- "46"
workingon.data$Stiller[workingon.data$Stiller=="Skatteminister"] <- "47"
workingon.data$Stiller[workingon.data$Stiller=="Transport- og energiminister"] <- "48"
workingon.data$Stiller[workingon.data$Stiller=="Familie- og forbrugerminister"] <- "49"
workingon.data$Stiller[workingon.data$Stiller=="Minister for sundhed og forebyggelse"] <- "50"
workingon.data$Stiller[workingon.data$Stiller=="Klima- og energiminister"] <- "51"
workingon.data$Stiller[workingon.data$Stiller=="Velf?rdsminister"] <- "52"
workingon.data$Stiller[workingon.data$Stiller=="Transportminister"] <- "53"
workingon.data$Stiller[workingon.data$Stiller=="Indenrigs- og socialminister"] <- "54"
workingon.data$Stiller[workingon.data$Stiller=="Minister for udviklingsbistand"] <- "55"
workingon.data$Stiller[workingon.data$Stiller=="Erhvervs- og v?kstminister"] <- "56"
workingon.data$Stiller[workingon.data$Stiller=="Social- og integrationsminister"] <- "57"
workingon.data$Stiller[workingon.data$Stiller=="B?rne- og undervisningsminister"] <- "58"
workingon.data$Stiller[workingon.data$Stiller=="Minister for ligestilling og kirke"] <- "59"
workingon.data$Stiller[workingon.data$Stiller=="?konomi- og indenrigsminister"] <- "60"
workingon.data$Stiller[workingon.data$Stiller=="Minister for by, bolig og landdistrikter"] <- "61"
workingon.data$Stiller[workingon.data$Stiller=="Klima-, energi- og bygningsminister"] <- "62"
workingon.data$Stiller[workingon.data$Stiller=="Minister for forskning, innovation og videreg?ende uddannelser"] <- "63"
workingon.data$Stiller[workingon.data$Stiller=="Europaminister"] <- "64"
workingon.data$Stiller[workingon.data$Stiller=="Handels- og europaminister"] <- "65"
workingon.data$Stiller[workingon.data$Stiller=="Social-, b?rne- og integrationsminister"] <- "66"
workingon.data$Stiller[workingon.data$Stiller=="Social- og indenrigsminister"] <- "67"
workingon.data$Stiller[workingon.data$Stiller=="Uddannelses- og forskningsminister"] <- "68"
workingon.data$Stiller[workingon.data$Stiller=="Minister for b?rn, ligestilling, integration og sociale forhold"] <- "69"
workingon.data$Stiller[workingon.data$Stiller=="Sundheds- og ?ldreminister"] <- "70"
workingon.data$Stiller[workingon.data$Stiller=="Transport- og bygningsminister"] <- "71"
workingon.data$Stiller[workingon.data$Stiller=="Energi-, forsynings- og klimaminister"] <- "72"
workingon.data$Stiller[workingon.data$Stiller=="Udl?ndinge-, integrations- og boligminister"] <- "73"
workingon.data$Stiller[workingon.data$Stiller=="Milj?- og f?devareminister"] <- "74"
workingon.data$Stiller[workingon.data$Stiller=="Minister for b?rn, undervisning og ligestilling"] <- "75"
workingon.data$Stiller[workingon.data$Stiller=="B?rne- og socialminister"] <- "76"
workingon.data$Stiller[workingon.data$Stiller=="Udl?ndinge- og integrationsminister"] <- "77"
workingon.data$Stiller[workingon.data$Stiller=="Transport-, bygnings- og boligminister"] <- "78"
workingon.data$Stiller[workingon.data$Stiller=="?ldreminister"] <- "79"
workingon.data$Stiller[workingon.data$Stiller=="Minister for offentlig innovation"] <- "80"
workingon.data$Stiller[workingon.data$Stiller=="Minister for fiskeri og ligestilling"] <- "81"
workingon.data$Stiller[workingon.data$Stiller=="Folketinget"] <- "82"



#######################ERRORS#############################
workingon.data$Stiller[workingon.data$Stiller=="og"] <- "996"
workingon.data$Stiller[workingon.data$Stiller=="Udvalg:"] <- "997"
workingon.data$Stiller[workingon.data$Stiller=="fungerende"] <- "998"

workingon.data$Stiller[workingon.data$Stiller==""] <- "99"
workingon.data$Stiller[workingon.data$Stiller==""] <- "999"


#########################SAMLING#########################

workingon.data$Samling[workingon.data$Samling=="2007-08 (1. samling)"] <- "42"
workingon.data$Samling[workingon.data$Samling=="2007-08 (2. samling)"] <- "43"
workingon.data$Samling[workingon.data$Samling=="2008-09"] <- "44"
workingon.data$Samling[workingon.data$Samling=="2009-10"] <- "45"
workingon.data$Samling[workingon.data$Samling=="2010-11 (1. samling)"] <- "46"
workingon.data$Samling[workingon.data$Samling=="2011-12"] <- "47"
workingon.data$Samling[workingon.data$Samling=="2012-13"] <- "48"
workingon.data$Samling[workingon.data$Samling=="2013-14"] <- "49"
workingon.data$Samling[workingon.data$Samling=="2014-15 (1. samling)"] <- "50"
workingon.data$Samling[workingon.data$Samling=="2014-15 (2. samling)"] <- "51"
workingon.data$Samling[workingon.data$Samling=="2015-16"] <- "52"
workingon.data$Samling[workingon.data$Samling=="2016-17"] <- "53"
workingon.data$Samling[workingon.data$Samling=="2017-18"] <- "54"
workingon.data$Samling[workingon.data$Samling=="2018-19"] <- "55"




#########################STATUS#########################
workingon.data$Status[workingon.data$Status=="Stadf?stet"] <- "1"
workingon.data$Status[workingon.data$Status=="Fremsat"] <- "2"
workingon.data$Status[workingon.data$Status=="Vedtaget"] <- "3"
workingon.data$Status[workingon.data$Status=="Delt"] <- "4"
workingon.data$Status[workingon.data$Status=="Beretning afgivet"] <- "5"
workingon.data$Status[workingon.data$Status=="Bet?nkning afgivet"] <- "6"
workingon.data$Status[workingon.data$Status=="Forkastet"] <- "7"
workingon.data$Status[workingon.data$Status=="Tilbagetaget"] <- "8"
workingon.data$Status[workingon.data$Status=="3.beh./Vedtaget - 3.beh./"] <- "9"
workingon.data$Status[workingon.data$Status=="3.beh./Vedtaget"] <- "9"
workingon.data$Status[workingon.data$Status=="3. beh./Forkastet - 3."] <- "10"
workingon.data$Status[workingon.data$Status=="3. beh./Forkastet"] <- "10"
workingon.data$Status[workingon.data$Status=="2. beh./Direkte til 3. beh."] <- "11"
workingon.data$Status[workingon.data$Status=="1. beh./Henvist til udvalg"] <- "12"
workingon.data$Status[workingon.data$Status=="Bortfaldet"] <- "13"
workingon.data$Status[workingon.data$Status=="2. beh./Henvist til udvalg"] <- "11"


##########################UDVALG#########################

workingon.data$Udvalg[workingon.data$Udvalg=="Udv. for forretningsordenen"] <- "1"
workingon.data$Udvalg[workingon.data$Udvalg=="Udv. t. valgs pr?v. & Udv. t. pr?v. af valgene"] <- "2"
workingon.data$Udvalg[workingon.data$Udvalg=="Arbejdsmarkedsudvalget"] <- "3"
workingon.data$Udvalg[workingon.data$Udvalg=="Boligudvalget"] <- "4"
workingon.data$Udvalg[workingon.data$Udvalg=="Energipolitisk udvalg"] <- "5"
workingon.data$Udvalg[workingon.data$Udvalg=="Erhvervsudvalget"] <- "6"
workingon.data$Udvalg[workingon.data$Udvalg=="Finansudvalget"] <- "7"
workingon.data$Udvalg[workingon.data$Udvalg=="Forsvarsudvalget"] <- "8"
workingon.data$Udvalg[workingon.data$Udvalg=="Indf?dsretsudvalget"] <- "9"
workingon.data$Udvalg[workingon.data$Udvalg=="Kirkeudvalget"] <- "10"
workingon.data$Udvalg[workingon.data$Udvalg=="Kommunaludvalget"] <- "11"
workingon.data$Udvalg[workingon.data$Udvalg=="Kulturudvalget"] <- "12"
workingon.data$Udvalg[workingon.data$Udvalg=="Landbrugs- og fiskeriudvalget"] <- "13"
workingon.data$Udvalg[workingon.data$Udvalg=="Markedsudvalget/ Europaudvalget"] <- "14"
workingon.data$Udvalg[workingon.data$Udvalg=="Milj?- og planl?gningsudvalget/ Udv. For fysisk planl?gning"] <- "15"
workingon.data$Udvalg[workingon.data$Udvalg=="Udv. om off. arb./trafikudvalget"] <- "16"
workingon.data$Udvalg[workingon.data$Udvalg=="Politisk-?konomisk udvalg"] <- "17"
workingon.data$Udvalg[workingon.data$Udvalg=="Retsudvalget"] <- "18"
workingon.data$Udvalg[workingon.data$Udvalg=="Skatte- (og afgifts-) udvalget"] <- "19"
workingon.data$Udvalg[workingon.data$Udvalg=="Socialudvalget"] <- "20"
workingon.data$Udvalg[workingon.data$Udvalg=="Uddannelsesudvalget/ Undervisningsudvalget"] <- "21"
workingon.data$Udvalg[workingon.data$Udvalg=="Udenrigsudvalget"] <- "22"
workingon.data$Udvalg[workingon.data$Udvalg=="Udv. ang. vidensk. forskning/forskningsudvalget"] <- "23"
workingon.data$Udvalg[workingon.data$Udvalg=="Udv. ang?ende Gr?nlands love"] <- "24"
workingon.data$Udvalg[workingon.data$Udvalg=="Udv. vedr. dansk sikkerhedspolitik"] <- "25"
workingon.data$Udvalg[workingon.data$Udvalg=="Sundhedsudvalget"] <- "26"
workingon.data$Udvalg[workingon.data$Udvalg=="Christianiaudvalget"] <- "27"
workingon.data$Udvalg[workingon.data$Udvalg=="Udvalget for F?devarer, Landbrug og Fiskeri"] <- "28"
workingon.data$Udvalg[workingon.data$Udvalg=="Udv. for Udl?ndinge- og Integrationspolitik"] <- "29"
workingon.data$Udvalg[workingon.data$Udvalg=="Udv. for Videnskab og Teknologi"] <- "30"
workingon.data$Udvalg[workingon.data$Udvalg=="Undervisningsudvalget"] <- "31"
workingon.data$Udvalg[workingon.data$Udvalg=="Udvalget vedr?rende Gr?nlandske Forhold"] <- "32"
workingon.data$Udvalg[workingon.data$Udvalg=="Udvalget for Videnskab og Teknologi"] <- "33"
workingon.data$Udvalg[workingon.data$Udvalg=="Udvalget for Udl?ndinge- og Integrationspolitik"] <- "34"
workingon.data$Udvalg[workingon.data$Udvalg=="Udvalget for Landdistrikter og ?er"] <- "35"
workingon.data$Udvalg[workingon.data$Udvalg=="Udvalget for Forskning, Innovation og Videreg?ende Uddannelser"] <- "36"
workingon.data$Udvalg[workingon.data$Udvalg=="Udvalget for Forretningsordenen"] <- "37"
workingon.data$Udvalg[workingon.data$Udvalg=="Udl?ndinge-, Integrations- og Boligudvalget"] <- "38"
workingon.data$Udvalg[workingon.data$Udvalg=="Udl?ndinge- og Integrationsudvalget"] <- "39"
workingon.data$Udvalg[workingon.data$Udvalg=="Uddannelsesudvalget"] <- "40"
workingon.data$Udvalg[workingon.data$Udvalg=="Uddannelses- og Forskningsudvalget"] <- "41"
workingon.data$Udvalg[workingon.data$Udvalg=="Transportudvalget"] <- "42"
workingon.data$Udvalg[workingon.data$Udvalg=="Transport-, Bygnings- og Boligudvalget"] <- "43"
workingon.data$Udvalg[workingon.data$Udvalg=="Transport- og Bygningsudvalget"] <- "44"
workingon.data$Udvalg[workingon.data$Udvalg=="Trafikudvalget"] <- "45"
workingon.data$Udvalg[workingon.data$Udvalg=="Sundheds- og Forebyggelsesudvalget"] <- "46"
workingon.data$Udvalg[workingon.data$Udvalg=="Sundheds- og ?ldreudvalget"] <- "47"
workingon.data$Udvalg[workingon.data$Udvalg=="Social-, Indenrigs- og B?rneudvalget"] <- "48"
workingon.data$Udvalg[workingon.data$Udvalg=="Social- og Indenrigsudvalget"] <- "49"
workingon.data$Udvalg[workingon.data$Udvalg=="Skatteudvalget"] <- "50"
workingon.data$Udvalg[workingon.data$Udvalg=="Milj?udvalget"] <- "51"
workingon.data$Udvalg[workingon.data$Udvalg=="Milj?- og Planl?gningsudvalget"] <- "52"
workingon.data$Udvalg[workingon.data$Udvalg=="Milj?- og F?devareudvalget"] <- "53"
workingon.data$Udvalg[workingon.data$Udvalg=="Ligestillingsudvalget"] <- "54"
workingon.data$Udvalg[workingon.data$Udvalg=="Klima-, Energi- og Bygningsudvalget"] <- "55"
workingon.data$Udvalg[workingon.data$Udvalg=="Gr?nlandsudvalget"] <- "56"
workingon.data$Udvalg[workingon.data$Udvalg=="F?r?udvalget"] <- "57"
workingon.data$Udvalg[workingon.data$Udvalg=="Europaudvalget"] <- "58"
workingon.data$Udvalg[workingon.data$Udvalg=="Erhvervs-, V?kst- og Eksportudvalget"] <- "59"
workingon.data$Udvalg[workingon.data$Udvalg=="Det Politisk-?konomiske Udvalg"] <- "60"
workingon.data$Udvalg[workingon.data$Udvalg=="Det Energipolitiske Udvalg"] <- "61"
workingon.data$Udvalg[workingon.data$Udvalg=="By- og Boligudvalget"] <- "62"
workingon.data$Udvalg[workingon.data$Udvalg=="B?rne- og Undervisningsudvalget"] <- "63"
workingon.data$Udvalg[workingon.data$Udvalg=="Besk?ftigelsesudvalget"] <- "63"
workingon.data$Udvalg[workingon.data$Udvalg=="Energi- Forsynings- og Klimaudvalget"] <- "64"
workingon.data$Udvalg[workingon.data$Udvalg=="Energi-, Forsynings- og Klimaudvalget"] <- "65"
workingon.data$Udvalg[workingon.data$Udvalg=="Udvalget vedr?rende F?r?ske Forhold"] <- "66"

################################################ DAYS AND MONTHS DUMMY ################################################ 

workingon.data$tr_be_d <- "1"
workingon.data$tr_be_m <- "1"

################################################ ID ################################################ 

workingon.data$ID <- 1:2770

################################################ REGNR ################################################ 

regnr <- unlist(workingon.data$Regnr)
regnr <- str_extract(regnr, "[[:Digit:]]+")
workingon.data$Regnr <- regnr

################################################ DAYS AND MONTHS REPLACEMENT ################################################ 

reading <- unlist(workingon.data$Reading)
reading <- gsub("3. behandlet", "", reading)
reading <- gsub(", vedtaget ", "", reading)
tr_be_d <- str_extract(reading, "[[:Digit:]]+")
tr_be_d <- str_replace_na(tr_be_d)
workingon.data$tr_be_d <- tr_be_d


reading <- unlist(workingon.data$Reading)
reading <- gsub("3. behandlet", "", reading)
reading <- gsub(", vedtaget ", "", reading)
tr_be_m <- str_extract(reading, "-[[:Digit:]]+-")
tr_be_m <- str_extract(tr_be_m, "[[:Digit:]]+")
workingon.data$tr_be_m <- tr_be_m
workingon.data$Reading <- NULL

################################################ FOR ################################################ 

poll <- workingon.data$Poll
poll <- substring(poll, 6, nchar(poll))
infavor <- str_extract(poll, "[[:Digit:]]+")
workingon.data$For <- infavor

################################################ IMOD ################################################ 

imodstemte <- workingon.data$Poll
imodstemte <- str_replace_all(imodstemte, "hverken for eller imod stemte", "")
imodstemte <- str_replace_all(imodstemte, "[\r\n]" , " ")
imodstemte <- str_extract_all(imodstemte, "imod stemte [[:Digit:]]+ | mod [[:Digit:]]+ | [[:Digit:]]+ stemmer imod forslaget | [[:Digit:]]+ enstemmigt | imod [[:Digit:]]+")
imodstemte <- gsub("vedtaget | imod stemte | mod | stemmer imod forslaget", "", imodstemte)
imodstemte <- trimws(imodstemte)
workingon.data$Imod <- imodstemte
workingon.data$Imod[workingon.data$Imod=="character(0)"] <- "0"

################################################ UNDLADER ################################################ 

undlader <- workingon.data$Poll
undlader <- str_replace_all(undlader, "[\r\n]" , " ")
undlader <- str_extract_all(undlader, "eller imod stemte [[:Digit:]]+ | [[:Digit:]]+ stemmer hverken for eller imod forslaget")
undlader <- gsub("eller imod stemte | stemmer hverken for eller imod forslaget", "", undlader)
undlader <- trimws(undlader)
workingon.data$Undlader <- undlader
workingon.data$Undlader[workingon.data$Undlader=="character(0)"] <- "0"

#(\d+)(?!.*\d)([\s\S].+?(?=stemte hverken for eller imod))

################################################ DUMMYS ################################################ 

parties <- workingon.data$Poll
#parties <- regmatches(parties, gregexpr("(?<=\\().*?(?=\\))", parties, perl=T))
parties <- str_extract(parties,  "(?<=\\().+?(?=\\))")
workingon.data$PartiesInfavor <- parties


parties2 <- workingon.data$Poll
parties2 <- str_extract_all(parties2,  "(?<=\\().+?(?=\\))", simplify = T)

workingon.data$PartiesAgainst <- parties2[1:2770, 2]
workingon.data$PartiesAbsent <- parties2[1:2770, 3]

##################################### CODING PARTIES  #####################################################


SD1 <- regmatches(workingon.data$PartiesInfavor, gregexpr("SD", workingon.data$PartiesInfavor, perl=T))
SD1[SD1=="SD"] <- "1"
SD1[lengths(SD1) == 0] <- as.numeric(0)
SD1 <- unlist((as.numeric(SD1)))

SD2 <- regmatches(workingon.data$PartiesAgainst, gregexpr("SD", workingon.data$PartiesAgainst, perl=T))
SD2[SD2=="SD"] <- "3"
SD2[lengths(SD2) == 0] <- as.numeric(0)
SD2 <- unlist((as.numeric(SD2)))

SD3 <- regmatches(workingon.data$PartiesAbsent, gregexpr("SD", workingon.data$PartiesAbsent, perl=T))
SD3[SD3=="SD"] <- "5"
SD3[lengths(SD3) == 0] <- as.numeric(0)
SD3 <- unlist((as.numeric(SD3)))

df <- paste(SD1 + SD2 + SD3)
workingon.data$SD <- df

###########################################################################################################
###########################################################################################################

KF1 <- regmatches(workingon.data$PartiesInfavor, gregexpr("KF", workingon.data$PartiesInfavor, perl=T))
KF1[KF1=="KF"] <- "1"
KF1[lengths(KF1) == 0] <- as.numeric(0)
KF1 <- unlist((as.numeric(KF1)))

KF2 <- regmatches(workingon.data$PartiesAgainst, gregexpr("KF", workingon.data$PartiesAgainst, perl=T))
KF2[KF2=="KF"] <- "3"
KF2[lengths(KF2) == 0] <- as.numeric(0)
KF2 <- unlist((as.numeric(KF2)))

KF3 <- regmatches(workingon.data$PartiesAbsent, gregexpr("KF", workingon.data$PartiesAbsent, perl=T))
KF3[KF3=="KF"] <- "5"
KF3[lengths(KF3) == 0] <- as.numeric(0)
KF3 <- unlist((as.numeric(KF3)))

df <- paste(KF1 + KF2 + KF3)
workingon.data$KF <- df

# When calculating matrix results I had to make sure that Infavor = 1, Against = 3, Abstained = 5, are not results of adding up 1, 3 and 5
###########################################################################################################
###########################################################################################################


RV1 <- regmatches(workingon.data$PartiesInfavor, gregexpr("RV", workingon.data$PartiesInfavor, perl=T))
RV1[RV1=="RV"] <- "1"
RV1[lengths(RV1) == 0] <- as.numeric(0)
RV1 <- unlist((as.numeric(RV1)))

RV2 <- regmatches(workingon.data$PartiesAgainst, gregexpr("RV", workingon.data$PartiesAgainst, perl=T))
RV2[RV2=="RV"] <- "3"
RV2[lengths(RV2) == 0] <- as.numeric(0)
RV2 <- unlist((as.numeric(RV2)))

RV3 <- regmatches(workingon.data$PartiesAbsent, gregexpr("RV", workingon.data$PartiesAbsent, perl=T))
RV3[RV3=="RV"] <- "5"
RV3[lengths(RV3) == 0] <- as.numeric(0)
RV3 <- unlist((as.numeric(RV3)))

df <- paste(RV1 + RV2 + RV3)
workingon.data$RV <- df

###########################################################################################################
###########################################################################################################


SF1 <- regmatches(workingon.data$PartiesInfavor, gregexpr("SF", workingon.data$PartiesInfavor, perl=T))
SF1[SF1=="SF"] <- "1"
SF1[lengths(SF1) == 0] <- as.numeric(0)
SF1 <- unlist((as.numeric(SF1)))

SF2 <- regmatches(workingon.data$PartiesAgainst, gregexpr("SF", workingon.data$PartiesAgainst, perl=T))
SF2[SF2=="SF"] <- "3"
SF2[lengths(SF2) == 0] <- as.numeric(0)
SF2 <- unlist((as.numeric(SF2)))

SF3 <- regmatches(workingon.data$PartiesAbsent, gregexpr("SF", workingon.data$PartiesAbsent, perl=T))
SF3[SF3=="SF"] <- "5"
SF3[lengths(SF3) == 0] <- as.numeric(0)
SF3 <- unlist((as.numeric(SF3)))

df <- paste(SF1 + SF2 + SF3)
workingon.data$SF <- df

###########################################################################################################
###########################################################################################################


LA1 <- regmatches(workingon.data$PartiesInfavor, gregexpr("LA", workingon.data$PartiesInfavor, perl=T))
LA1[LA1=="LA"] <- "1"
LA1[lengths(LA1) == 0] <- as.numeric(0)
LA1 <- unlist((as.numeric(LA1)))

LA2 <- regmatches(workingon.data$PartiesAgainst, gregexpr("LA", workingon.data$PartiesAgainst, perl=T))
LA2[LA2=="LA"] <- "3"
LA2[lengths(LA2) == 0] <- as.numeric(0)
LA2 <- unlist((as.numeric(LA2)))

LA3 <- regmatches(workingon.data$PartiesAbsent, gregexpr("LA", workingon.data$PartiesAbsent, perl=T))
LA3[LA3=="LA"] <- "5"
LA3[lengths(LA3) == 0] <- as.numeric(0)
LA3 <- unlist((as.numeric(LA3)))

df <- paste(LA1 + LA2 + LA3)
workingon.data$LA <- df

###########################################################################################################
###########################################################################################################


ALT1 <- regmatches(workingon.data$PartiesInfavor, gregexpr("ALT", workingon.data$PartiesInfavor, perl=T))
ALT1[ALT1=="ALT"] <- "1"
ALT1[lengths(ALT1) == 0] <- as.numeric(0)
ALT1 <- unlist((as.numeric(ALT1)))

ALT2 <- regmatches(workingon.data$PartiesAgainst, gregexpr("ALT", workingon.data$PartiesAgainst, perl=T))
ALT2[ALT2=="ALT"] <- "3"
ALT2[lengths(ALT2) == 0] <- as.numeric(0)
ALT2 <- unlist((as.numeric(ALT2)))

ALT3 <- regmatches(workingon.data$PartiesAbsent, gregexpr("ALT", workingon.data$PartiesAbsent, perl=T))
ALT3[ALT3=="ALT"] <- "5"
ALT3[lengths(ALT3) == 0] <- as.numeric(0)
ALT3 <- unlist((as.numeric(ALT3)))

df <- paste(ALT1 + ALT2 + ALT3)
workingon.data$ALT <- df

###########################################################################################################
###########################################################################################################


EL1 <- regmatches(workingon.data$PartiesInfavor, gregexpr("EL", workingon.data$PartiesInfavor, perl=T))
EL1[EL1=="EL"] <- "1"
EL1[lengths(EL1) == 0] <- as.numeric(0)
EL1 <- unlist((as.numeric(EL1)))

EL2 <- regmatches(workingon.data$PartiesAgainst, gregexpr("EL", workingon.data$PartiesAgainst, perl=T))
EL2[EL2=="EL"] <- "3"
EL2[lengths(EL2) == 0] <- as.numeric(0)
EL2 <- unlist((as.numeric(EL2)))

EL3 <- regmatches(workingon.data$PartiesAbsent, gregexpr("EL", workingon.data$PartiesAbsent, perl=T))
EL3[EL3=="EL"] <- "5"
EL3[lengths(EL3) == 0] <- as.numeric(0)
EL3 <- unlist((as.numeric(EL3)))

df <- paste(EL1 + EL2 + EL3)
workingon.data$EL <- df

###########################################################################################################
###########################################################################################################


DF1 <- regmatches(workingon.data$PartiesInfavor, gregexpr("DF", workingon.data$PartiesInfavor, perl=T))
DF1[DF1=="DF"] <- "1"
DF1[lengths(DF1) == 0] <- as.numeric(0)
DF1 <- unlist((as.numeric(DF1)))

DF2 <- regmatches(workingon.data$PartiesAgainst, gregexpr("DF", workingon.data$PartiesAgainst, perl=T))
DF2[DF2=="DF"] <- "3"
DF2[lengths(DF2) == 0] <- as.numeric(0)
DF2 <- unlist((as.numeric(DF2)))

DF3 <- regmatches(workingon.data$PartiesAbsent, gregexpr("DF", workingon.data$PartiesAbsent, perl=T))
DF3[DF3=="DF"] <- "5"
DF3[lengths(DF3) == 0] <- as.numeric(0)
DF3 <- unlist((as.numeric(DF3)))

df <- paste(DF1 + DF2 + DF3)
workingon.data$DF <- df

###########################################################################################################
###########################################################################################################


NY1 <- regmatches(workingon.data$PartiesInfavor, gregexpr("NY", workingon.data$PartiesInfavor, perl=T))
NY1[NY1=="NY"] <- "1"
NY1[lengths(NY1) == 0] <- as.numeric(0)
NY1 <- unlist((as.numeric(NY1)))

NY2 <- regmatches(workingon.data$PartiesAgainst, gregexpr("NY", workingon.data$PartiesAgainst, perl=T))
NY2[NY2=="NY"] <- "3"
NY2[lengths(NY2) == 0] <- as.numeric(0)
NY2 <- unlist((as.numeric(NY2)))

NY3 <- regmatches(workingon.data$PartiesAbsent, gregexpr("NY", workingon.data$PartiesAbsent, perl=T))
NY3[NY3=="NY"] <- "5"
NY3[lengths(NY3) == 0] <- as.numeric(0)
NY3 <- unlist((as.numeric(NY3)))

df <- paste(NY1 + NY2 + NY3)
workingon.data$NY <- df

###########################################################################################################
###########################################################################################################

UFG1 <- regmatches(workingon.data$PartiesInfavor, gregexpr("UFG", workingon.data$PartiesInfavor, perl=T))
UFG1[UFG1=="UFG"] <- "1"
UFG1[lengths(UFG1) == 0] <- as.numeric(0)
UFG1 <- unlist((as.numeric(UFG1)))

UFG2 <- regmatches(workingon.data$PartiesAgainst, gregexpr("UFG", workingon.data$PartiesAgainst, perl=T))
UFG2[UFG2=="UFG"] <- "3"
UFG2[lengths(UFG2) == 0] <- as.numeric(0)
UFG2 <- unlist((as.numeric(UFG2)))

UFG3 <- regmatches(workingon.data$PartiesAbsent, gregexpr("UFG", workingon.data$PartiesAbsent, perl=T))
UFG3[UFG3=="UFG"] <- "5"
UFG3[lengths(UFG3) == 0] <- as.numeric(0)
UFG3 <- unlist((as.numeric(UFG3)))

df <- paste(UFG1 + UFG2 + UFG3)
workingon.data$UFG <- df

###########################################################################################################
###########################################################################################################

### SINGLE-LETTER-PARTIES ###

workingon.data$foruseonly <- workingon.data$PartiesInfavor
V1 <- regmatches(workingon.data$foruseonly, gregexpr("\\bV\\b", workingon.data$foruseonly, perl=T))
V1[V1=="V"] <- "1"
V1[lengths(V1) == 0] <- as.numeric(0)
V1 <- unlist((as.character(V1)))
V1 <- unlist((as.numeric(V1)))



workingon.data$foruseonly <- workingon.data$PartiesAgainst
#workingon.data[, 19:27] <- apply(workingon.data[, 19:27], 2, function(x) as.character(gsub("RV", "", x)))
V2 <- regmatches(workingon.data$foruseonly, gregexpr("\\bV\\b", workingon.data$foruseonly, perl=T))
V2[V2=="V"] <- "3"
V2[lengths(V2) == 0] <- as.numeric(0)
V2 <- unlist((as.character(V2)))
V2 <- unlist(as.numeric(V2))


workingon.data$foruseonly <- workingon.data$PartiesAbsent
V3 <- regmatches(workingon.data$foruseonly, gregexpr("\\bV\\b", workingon.data$foruseonly, perl=T))
V3[V3=="V"] <- "5"
V3[lengths(V3) == 0] <- as.numeric(0)
V3 <- unlist((as.numeric(V3)))

df <- paste(V1 + V2 + V3)
workingon.data$V <- df

###########################################################################################################
###########################################################################################################

workingon.data$foruseonly <- workingon.data$PartiesInfavor
S1 <- regmatches(workingon.data$foruseonly, gregexpr("\\bS\\b", workingon.data$foruseonly, perl=T))
S1[S1=="S"] <- "1"
S1[lengths(S1) == 0] <- as.numeric(0)
S1 <- unlist((as.character(S1)))
S1 <- unlist((as.numeric(S1)))


workingon.data$foruseonly <- workingon.data$PartiesAgainst
S2 <- regmatches(workingon.data$foruseonly, gregexpr("\\bS\\b", workingon.data$foruseonly, perl=T))
S2[S2=="S"] <- "3"
S2[lengths(S2) == 0] <- as.numeric(0)
S2 <- unlist((as.character(S2)))
S2 <- unlist(as.numeric(S2))

workingon.data$foruseonly <- workingon.data$PartiesAbsent
S3 <- regmatches(workingon.data$foruseonly, gregexpr("\\bS\\b", workingon.data$foruseonly, perl=T))
S3[S3=="S"] <- "5"
S3[lengths(S3) == 0] <- as.numeric(0)
S3 <- unlist((as.numeric(S3)))

df <- paste(S1 + S2 + S3)
workingon.data$S <- df

workingon.data$foruseonly <- NULL


########################## Ministerom #############################################
colnames(workingon.data)[9] <- "Ministeromr?de"

workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Besk?ftigelsesministeriet"] <- "1"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="B?rne- og Socialministeriet"] <- "2"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Energi-, Forsynings- og Klimaministeriet"] <- "3"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Erhvervs- og V?kstministeriet"] <- "4"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Erhvervsministeriet"] <- "5"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Finansministeriet"] <- "6"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Folketinget"] <- "7"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Forsvarsministeriet"] <- "8"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Handels- og europaministeriet"] <- "9"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Indenrigs- og Socialministeriet"] <- "10"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Indenrigs- og Sundhedsministeriet"] <- "11"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Justitsministeriet"] <- "12"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Klima- og Energiministeriet"] <- "13"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Klima-, Energi- og Bygningsministeriet"] <- "14"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Kulturministeriet"] <- "15"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Milj?- og F?devareministeriet"] <- "16"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Milj?ministeriet"] <- "17"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for B?rn og Undervisning"] <- "18"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for B?rn, Ligestilling, Integration og Sociale Forhold"] <- "19"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for B?rn, Undervisning og Ligestilling"] <- "20"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for By, Bolig og Landdistrikter"] <- "21"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for Familie- og Forbrugeranliggender"] <- "22"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for Flygtninge, Indvandrere og Integration"] <- "23"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for F?devarer, Landbrug og Fiskeri"] <- "24"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for Forskning, Innovation og Videreg?ende Uddannelser"] <- "25"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for Ligestilling"] <- "26"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for Ligestilling og Kirke"] <- "27"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for Sundhed og Forebyggelse"] <- "28"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for Udviklingsbistand"] <- "29"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Ministeriet for Videnskab, Teknologi og Udvikling"] <- "30"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="?konomi- og Erhvervsministeriet"] <- "31"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="?konomi- og Indenrigsministeriet"] <- "32"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Skatteministeriet"] <- "33"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Social- og Indenrigsministeriet"] <- "34"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Social- og Integrationsministerie"] <- "35"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Social-, b?rne- og integrationsministeriet"] <- "36"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Socialministeriet"] <- "37"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Sundheds- og ?ldreministeriet"] <- "38"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Transport- og Bygningsministeriet"] <- "39"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Transport- og Energiministeriet"] <- "40"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Transport-, Bygnings- og Boligministeriet"] <- "41"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Transportministeriet"] <- "42"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Uddannelses- og Forskningsministeriet"] <- "43"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Udenrigsministeriet"] <- "44"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Udl?ndinge- og Integrationsministeriet"] <- "45"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Udl?ndinge-, Integrations- og Boligministeriet"] <- "46"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Undervisningsministeriet"] <- "47"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Velf?rdsministeriet"] <- "48"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Europaministeriet"] <- "49"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Kirkeministeriet"] <- "50"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Social- og Integrationsministeriet"] <- "51"
workingon.data$Ministeromr?de[workingon.data$Ministeromr?de=="Statsministeriet"] <- "52"

########################## STREAMLINING  #############################################


Un1 <- unique(workingon.data$Poll)
stringlength <- data.frame(Group=Un1, x=nchar(Un1))
which(stringlength$x > 520)
#[1]  321  561  769  859 1012 1228 1258 1281 1616 1680 1991 2349

testdata$Imod <- lapply(testdata$Imod, gsub, pattern='imod stemte ', replacement='')
testdata$Imod <- lapply(testdata$Imod, gsub, pattern=' enstemmigt', replacement='')
testdata$Imod <- lapply(testdata$Imod, gsub, pattern='imod ', replacement='')

workingon.data$Heading <- NULL
workingon.data$Resume <- NULL
workingon.data$SD <- NULL
workingon.data$Poll <- NULL
workingon.data$PartiesInfavor <- NULL
workingon.data$PartiesAgainst <- NULL
workingon.data$PartiesAbsent <- NULL
workingon.data$foruseonly <- NULL

myvec <- sapply(dfsnew, NROW)
which(myvec == 2)

sapply(testdata, class)
error <- regmatches(workingon.data$Poll, gregexpr("fejl", workingon.data$Poll, perl=T))
myvec <- sapply(error, NROW)
which(myvec >= 1)
numerics <- as.numeric(which(myvec >=1))
errornew <- workingon.data$Poll[c(numerics)]
errorframe <- data.frame(errornew)
write.table(errorframe, file="errorframe.txt")
imod <- regmatches(workingon.data$Poll, gregexpr("imod", workingon.data$Poll))
myvec <- sapply(imod, NROW)
length(which(myvec >=1))
length(which(workingon.data$Imod >=1))
newnumerics <- regmatches(workingon.data$Poll, gregexpr("\\d+", workingon.data$Poll))

write_sav(testdata, "folketinget11042019.sav")
testdata <- read_sav("C:/Users/au615270/Downloads/folketinget09042019.sav")
testdataframe <- plyr::ldply(newnumerics, rbind)

newest <- cbind.data.frame(exceldataframe, testdataframe)
exceldataframe <- newest
save(exceldataframe, file="exceldataframe.Rdata")
write.csv(exceldataframe, file="exceldataframe.csv", col.names = T)
