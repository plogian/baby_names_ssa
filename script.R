#first get the 1000 most popular baby names, number of births, 
# and % of births for every year 1880-2016

library(RSelenium)
library(rvest)

#for Docker
#docker pull selenium/standalone-firefox:2.53.0
#docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0

#setwd("~/R/babynames")
rem_dr <- remoteDriver(remoteServerAddr = "192.168.99.100",port = 4445L)
rem_dr$open(silent = TRUE)
rem_dr$setImplicitWaitTimeout(10)

male_names_list <- list()
female_names_list <- list()
years_completed <- list()
years_all <- c(1880:2016)


year_search_pt_1<- function (year){
  rem_dr$navigate("https://www.ssa.gov/oact/babynames/")
  
  #input year
  year_input<- rem_dr$findElement(using="css selector", "#year")
  year_input$sendKeysToElement(list(year)) 
  
  top_1000 <- rem_dr$findElement(using="css selector", "#rank > option:nth-child(5)")
  top_1000$clickElement()
}  

percent_total_births <- function(year) {
  percent_radio_box <- rem_dr$findElement(using="css selector", "#percent")
  percent_radio_box$clickElement()
  
  go_button <- rem_dr$findElement()
  go_button$clickElement()
} 
  
number_total_births <- function(year) {
  number_radio_box <- rem_dr$findElement(using="css selector", "#number")
  number_radio_box$clickElement()
  
  go_button <- rem_dr$findElement()
  go_button$clickElement()
}  

scrape_table <- function() {
  table <- htmlParse(rem_dr$getPageSource()[[1]])
  read
}
  

