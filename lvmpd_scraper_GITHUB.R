#Libraries

##Basic data cleaning
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)

##Needed to run RSelenium
library(RSelenium)
library(wdman)
library(netstat)

#Render the summary HTML Document
library(rmarkdown)
library(knitr)

##Upload results to Google Drive
library(googledrive)
options(googledrive_quiet = TRUE)

##Send results as Gmail
library(mailR)

####
####
####
####

remDr <- remoteDriver()

#remDr$open(silent = TRUE)
remDr$open()

#For Later: Create the CSV name based on run date/time
##Doing it up front, because it could take a while for webpage to load, scraper to run
### Accounting for UTC time difference
currentDateTime <- format((Sys.time() - (6*60*60)), 
                          "%Y-%m-%d_%I%p")


csv_path_pr <- paste0("data/lvmpd_pr_list_",
                      currentDateTime,
                      ".csv")

#For later: Get export time for email title
### Accounting for UTC time difference
ExportDateTime <- format((Sys.time() - (6*60*60)), 
                         "%Y-%m-%d_%I%p")

#Navigate to the LVMPD record request website
remDr$navigate("https://lvmpd-portal.dynamics365portals.us/latest-requests/")

#Create the DataFrame that I'll put loop results in
lvmpd_pr_master <- NULL

####
####
####
####

#SCRAPE THE LIST OF RECORD REQUESTS

#Use nested For Loops to get everything

#The i value targets the navigation buttons on the record request webpage
##The xpath is based on the button's position on the page.
##The first page actually has the value of "2". Second page is in "3" position
##Repeat "9" after a certain point, because the next page is always in the 9th position

#This gets the first 10 pages of most recent records
x <- c(3,4,5,6,7,8,9,9,9,9)

for (i in x) {
  
  #Pause the process for 5 seconds 
  Sys.sleep(5) 
  
  ####################################
  #################################### 
  #RECORD REQUEST ID
  
  #Target all the links that are showing
  request_id <- remDr$findElements(using = 'class',
                                   "details-link")
  
  #Extract the "text" value of attribute
  ##Turn list into a data frame
  request_id <- lapply(request_id, 
                       function (x) x$getElementAttribute('text')) %>% 
    unlist() %>% 
    as.data.frame() 
  
  #Tidy DataFrame
  request_id <- request_id %>% 
    #Rename column name 
    select("request_id" = 1) %>% 
    #Remove any rows with the string "NPR"
    filter(str_detect(request_id, "NPR")) %>% 
    #Keep only distinct rows
    distinct()
  
  ####################################
  ####################################
  #REQUEST LINKS
  
  #Target all the links that are showing
  request_link <- remDr$findElements(using = 'class',
                                     "details-link")
  
  #Turn them into a data frame
  request_link <- lapply(request_link, 
                         function (x) x$getElementAttribute('href')) %>% 
    unlist() %>% 
    as.data.frame() 
  
  #Get only links to pages with request info
  ##Tidy up DataFrame
  request_link_page <- request_link %>% 
    #Rename column name 
    select("request_link" = 1) %>% 
    #Remove any rows with the string "public-download"
    filter(!str_detect(request_link, "public-download")) %>% 
    #Keep only distinct rows
    distinct() %>% 
    #Create column out of end of hyperlink (for join later)
    ##Get rid of everything in hyperlink before "srid="
    mutate(join_data = str_replace(request_link, '.*srid=', ""),
           ##Get rid of everything in hyperlink after first remaining hyphen
           join_data = str_replace(join_data, '-.*', ""))
  
  #Get only links to downloads for completed requests
  ##Tidy up DataFrame
  request_link_download <- request_link %>% 
    #Rename column name 
    select("download_link" = 1) %>% 
    #Keep only rows with the string "public-download"
    filter(str_detect(download_link, "public-download")) %>% 
    #Keep only distinct rows
    distinct() %>% 
    #Create column out of end of hyperlink (for join later)
    ##Get rid of everything in hyperlink before "srid="
    mutate(join_data = str_replace(download_link, '.*srid=', ""),
           ##Get rid of everything in hyperlink after first remaining hyphen
           join_data = str_replace(join_data, '-.*', ""))
  
  #Left join
  ##Why: Every request has a request_link value. Not all have a download_link value
  request_link_all <- request_link_page %>% 
    left_join(request_link_download, by = "join_data") %>% 
    select(request_link, download_link, join_data)
  
  #####################################
  ##################################### 
  #REQUEST TIME (nested For Loop)
  
  request_time_df <- NULL 
  
  for (j in 1:10) {
    
    #Targets specific xpath with Request Date
    ##The "j" value is looping it through all 10 requests on the page
    date_path <- paste0("/html/body/div[2]/div[5]/div[2]/div/div[2]/div/div[2]/table/tbody/tr[",
                        j,
                        "]/td[5]")
    
    request_time <- remDr$findElement(using = 'xpath',
                                      date_path)
    
    #Pull the Request Date from the aria-label attribute
    
    request_time <- request_time$getElementAttribute('aria-label') %>% 
      unlist() %>% 
      as.data.frame()
    
    #Bind to my DataFrame
    request_time_df <- rbind(request_time_df, 
                             request_time)
    
  }
  
  #Tidy up DataFrame
  request_time_df <- request_time_df %>% 
    select(request_time = 1)
  
  #####################################
  ##################################### 
  #REQUEST Status (nested For Loop)
  request_status_df <- NULL 
  
  for (k in 1:10) {
    
    #Targets specific xpath with Request Status
    ##The "k" value is looping it through all 10 requests on the page
    status_path <- paste0("/html/body/div[2]/div[5]/div[2]/div/div[2]/div/div[2]/table/tbody/tr[",
                          k,
                          "]/td[4]")
    
    request_status <- remDr$findElement(using = 'xpath',
                                        status_path)
    
    #Pull the Request Datew from the aria-label attribute
    
    request_status <- request_status$getElementAttribute('aria-label') %>% 
      unlist() %>% 
      as.data.frame()
    
    #Bind to my DataFrame
    request_status_df <- rbind(request_status_df, 
                               request_status)
    
  }
  
  #Tidy up DataFrame
  request_status_df <- request_status_df %>% 
    select(request_status = 1)
  
  #####################################
  ##################################### 
  
  #Combine all DataFrames
  lvmpd_df <- cbind(request_id,
                    request_time_df,
                    request_status_df,
                    request_link_all)
  
  #Make the date and time their own columns
  lvmpd_df <- lvmpd_df %>% 
    mutate(request_date = mdy(word(request_time, 1)),
           request_time_true = word(request_time, -2, -1))
  
  #Final order
  lvmpd_df <- lvmpd_df %>% 
    select(request_id,
           request_status,
           request_date,
           request_time = request_time_true,
           request_link,
           download_link)
  
  #Bind the loop results into the master DataFrame
  lvmpd_pr_master <- rbind(lvmpd_pr_master,
                           lvmpd_df)
  
  #####################################
  ##################################### 
  
  #Click button to next page
  
  #Target the navigation buttons using xpath and "i" value in our initial loop
  btn_path <- paste0("/html/body/div[2]/div[5]/div[2]/div/div[2]/div/div[8]/div/ul/li[",
                     i,
                     "]/a")
  #Select the button
  btn <- remDr$findElement(using = 'xpath',   
                           btn_path)
  #Click the button
  btn$clickElement()
  
}

#Kill the current Selenium session
remDr$closeWindow()

rs_driver_object <- rs_driver_object$server$stop()

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
system("taskkill /F /IM ChromeDriver.exe", intern=FALSE, ignore.stdout=FALSE) 

#Get rid of all objects except necessities
rm(list=setdiff(ls(), 
                c("lvmpd_pr_master",
                  "currentDateTime",
                  "csv_path_pr",
                  "start.time",
                  "ExportDateTime")))

####
####
####
####

#SCRAPE THE INDIVIDUAL RECORD REQUEST PAGES

#Headless Selenium browser speeds this part up tremendously
port <- as.integer(4444L + rpois(lambda = 1000, 1))
pJS <- wdman::phantomjs(port = port)
remDr <- remoteDriver(browserName = "phantomjs", port = port)
remDr$open()


#Create a list of URLs from my most recent scrape
url_list <- lvmpd_pr_master %>% 
  select(request_link) %>% 
  select(request_link) %>% 
  as.list() %>% 
  unlist() %>% 
  as.list()

#Create an empty DataFrame to put loop results in
lvmpd_page_master <- NULL


#Loop through the individual record pages

for (i in 1:length(url_list)) {
  
  #Navigates to each page in my URL column
  remDr$navigate(url_list[[i]])
  
  ####
  ####
  
  ##
  #Request number
  request_id <- remDr$findElements(using = 'xpath',
                                   "//*[@id='adoxio_caseidnumber']")
  
  request_id <- lapply(request_id, 
                       function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(request_id = 1)
  
  ##
  #Event number
  event_no <- remDr$findElements(using = 'xpath',
                                 "//*[@id='adoxio_reportnumber']")
  
  event_no <- lapply(event_no, 
                     function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(event_no = 1)
  
  ##
  #Start Date
  start_date <- remDr$findElements(using = 'xpath',
                                   "//*[@id='adoxio_startdate']")
  
  start_date <- lapply(start_date, 
                       function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(start_date = 1) %>% 
    mutate(start_date = ymd(str_sub(start_date, 1,  10)))
  
  ##
  #End Date
  end_date <- remDr$findElements(using = 'xpath',
                                 "//*[@id='adoxio_enddate']")
  
  end_date <- lapply(end_date, 
                     function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(end_date = 1) %>% 
    mutate(end_date = ymd(str_sub(end_date, 1,  10)))
  
  ##
  #First Name Of Involved Party
  first_name <- remDr$findElements(using = 'xpath',
                                   "//*[@id='adoxio_firstnameofinvolvedparty']")
  
  first_name <- lapply(first_name, 
                       function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(first_name = 1)
  
  ##
  #Last Name Of Involved Party
  last_name <- remDr$findElements(using = 'xpath',
                                  "//*[@id='adoxio_lastnameofinvolvedparty']")
  
  last_name <- lapply(last_name, 
                      function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(last_name = 1)
  
  ##
  #Request Text
  request_text <- remDr$findElements(using = 'xpath',
                                     "//*[@id='adoxio_natureofincidenttext']")
  
  request_text <- lapply(request_text, 
                         function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(request_text = 1)
  
  ##
  #Street Address 1
  street_add1 <- remDr$findElements(using = 'xpath',
                                    "//*[@id='adoxio_involvedpartyaddressstreet1']")
  
  street_add1 <- lapply(street_add1, 
                        function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(street_add1 = 1)
  
  ##
  #Street Address 2
  street_add2 <- remDr$findElements(using = 'xpath',
                                    "//*[@id='adoxio_involvedpartyaddressstreet2']")
  
  street_add2 <- lapply(street_add2, 
                        function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(street_add2 = 1)
  
  ##
  #Street Address 3
  street_add3 <- remDr$findElements(using = 'xpath',
                                    "//*[@id='adoxio_involvedpartyaddressstreet3']")
  
  street_add3 <- lapply(street_add3, 
                        function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(street_add3 = 1)
  
  ##
  #City
  city <- remDr$findElements(using = 'xpath',
                             "//*[@id='adoxio_involvedpartycity_name']")
  
  city <- lapply(city, 
                 function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(city = 1)
  
  ##
  #State
  state <- remDr$findElements(using = 'xpath',
                              "//*[@id='adoxio_involvedpartystate_name']")
  
  state <- lapply(state, 
                  function (x) x$getElementAttribute('value')) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    rename(state = 1)
  
  ###
  ###
  
  #Create master DataFrame
  master_data <- cbind(request_id, event_no,
                       start_date, end_date, first_name,
                       last_name, street_add1,
                       street_add2, street_add3, city, state, 
                       request_text)
  
  ###
  ###
  #Media Request
  
  #Looks at the "Yes" box
  media_yes <- remDr$findElements(using = 'xpath',
                                  "//*[@id='adoxio_mediarequest_1']")
  
  
  #This returns "true" if the attribute exists (if YES box is checked)
  ##Returns NULL if the attribute doesn't exist (if YES box is NOT checked)
  media_yes <- lapply(media_yes, 
                      function (x) x$getElementAttribute('checked')) %>% 
    unlist()
  
  #This checks to see whether the media "yes" box is checked
  ##If its not checked (NULL), returns "not media"
  ##If it is checked (not NULL), returns "media"
  #### Add result to my master DataFrame
  master_data <- master_data %>% 
    mutate(media_request = ifelse(!is.null(media_yes),
                                  "yes", "no"))
  
  ###
  ###
  
  #Bind results to master DataFrame
  lvmpd_page_master <- rbind(lvmpd_page_master,
                             master_data)
  
}


####

#Join the two DataFrames
lvmpd_join_master <- lvmpd_pr_master %>% 
  left_join(lvmpd_page_master, by = "request_id") 

##Create the CSV name based on original run date/time
csv_path_join <- paste0("data/lvmpd_pr_all_",
                        currentDateTime,
                        ".csv")

#Save output DataFrame
write.csv(lvmpd_join_master, 
          csv_path_join, 
          row.names=FALSE)

####

#Filter it to just media requests
lvmpd_join_master_media <- lvmpd_join_master %>%
  filter(media_request == "yes")

##Create the CSV name based on original run date/time
csv_path_media <- paste0("data/lvmpd_pr_media_",
                         currentDateTime,
                         ".csv")

#Save output DataFrame
write.csv(lvmpd_join_master_media, 
          csv_path_media, 
          row.names=FALSE)

####

#Get rid of all objects and DF except necessities
rm(list=setdiff(ls(), 
                c("lvmpd_pr_master",
                  "lvmpd_page_master",
                  "lvmpd_join_master",
                  "lvmpd_join_master_media",
                  "csv_path_media",
                  "csv_path_join",
                  "currentDateTime",
                  "start.time",
                  "ExportDateTime")))
