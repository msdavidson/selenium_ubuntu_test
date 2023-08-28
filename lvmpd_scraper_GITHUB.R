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

remDr$open(silent = TRUE)

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


  #Pause the process for 5 seconds 
  Sys.sleep(5) 

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

#Kill the current Selenium session
remDr$closeWindow()

####
####
####
####


##Create the CSV name based on original run date/time
csv_path_join <- paste0("data/lvmpd_pr_all_",
                        currentDateTime,
                        ".csv")

#Save output DataFrame
write.csv(request_time_df, 
          csv_path_join, 
          row.names=FALSE)
