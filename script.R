#first get the 1000 most popular baby names, number of births, 
# and % of births for every year 1880-2016

library(RSelenium)
library(rvest)
library(XML)

#for Docker
#docker pull selenium/standalone-firefox:2.53.0
#docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0

# to use with vnc
# docker pull selenium/standalone-firefox:2.53.0
# docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox-debug:2.53.0

#setwd("~/R/babynames")
rem_dr <- remoteDriver(remoteServerAddr = "192.168.99.100",port = 4445L)
rem_dr$open(silent = TRUE)
rem_dr$setImplicitWaitTimeout(10)

years_all <- as.character(c(1882:2016))

year_search_pt_1<- function (year){
  rem_dr$navigate("https://www.ssa.gov/oact/babynames/")
  
  #input year
  year_input<- rem_dr$findElement(using="css selector", "#year")
  year_input$clearElement()
  year_input$sendKeysToElement(list(year)) 
  
  top_1000 <- rem_dr$findElement(using="css selector", "#rank > option:nth-child(5)")
  top_1000$clickElement()
}  

percent_total_births <- function() {
  percent_radio_box <- rem_dr$findElement(using="css selector", "#percent")
  percent_radio_box$clickElement()
  

  go_button <- rem_dr$findElement(using="css selector", "#content > section:nth-child(4) > div > div > div:nth-child(1) > form > p:nth-child(4) > input[type='submit']:nth-child(1)")
  go_button$clickElement()
} 

number_total_births <- function(year) {
  number_radio_box <- rem_dr$findElement(using="css selector", "#number")
  number_radio_box$clickElement()
  
  go_button <- rem_dr$findElement(using="css selector", "#content > section:nth-child(4) > div > div > div:nth-child(1) > form > p:nth-child(4) > input[type='submit']:nth-child(1)")
  go_button$clickElement()
} 

scrape_table <- function() {
  table <- rem_dr$findElement(using="css selector", "body > table:nth-child(3) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(2) > p:nth-child(1) > table:nth-child(1) > tbody:nth-child(2)")
  table_html <- table$getPageSource()[[1]]
  table_r <- readHTMLTable(table_html)[[3]]
  return(table_r)
}

merge_and_split_tables_male <- function(table1, table2, year) {
  male_names <- table1[,c(1:3)]
  male_names_2 <- table2[,c(1:3)]
  male_names_merged <- cbind(male_names, male_names_2[,3])
  names(male_names_merged) <- c(paste0("Rank ", year) , "Male name", paste0("Percent of total males ", year), paste0("Number of males ", year))
  male_names_merged <- male_names_merged[-c(1, 1002), ]
  return(male_names_merged)
}

merge_and_split_tables_female <- function(table1, table2, year) {
  female_names <- table1[,c(1, 4:5)]
  female_names_2 <- table2[,c(1, 4:5)]
  female_names_merged <- cbind(female_names, female_names_2[,3])
  names(female_names_merged) <- c(paste0("Rank ", year) , "Female name", paste0("Percent of total females ", year), paste0("Number of females ", year))
  female_names_merged <- female_names_merged[-1, ]
  female_names_merged <- subset(female_names_merged, female_names_merged[,4] != 0)
  
  return(female_names_merged)
}

popular_names_by_year_male <- data.frame()
popular_names_by_year_female <- data.frame()

for(i in years_all) {
  n <- as.integer(i)-1879
  year_search_pt_1(i)
  percent_total_births()
  table_percent <- scrape_table()
  
  year_search_pt_1(i)
  number_total_births()
  table_number <- scrape_table()
  
  male_names_year <- merge_and_split_tables_male(table_percent, table_number, i)
  if(nrow(popular_names_by_year_male) > 0){
    popular_names_by_year_male <-merge(popular_names_by_year_male, male_names_year, all.x= TRUE, all.y=TRUE)
  } else {
    popular_names_by_year_male <- male_names_year
  } 

  female_names_year <- merge_and_split_tables_female(table_percent, table_number, i)
  if(nrow(popular_names_by_year_female) > 0){
    popular_names_by_year_female <-merge(popular_names_by_year_female, female_names_year, all.x= TRUE, all.y=TRUE)
  } else {
    popular_names_by_year_female <- female_names_year
  } 
}

write.csv(popular_names_by_year_female, "popular_names_by_year_female.csv")
write.csv(popular_names_by_year_male, "popular_names_by_year_male.csv")

popular_names_by_year_female <- read.csv("popular_names_by_year_female.csv", header=TRUE)
rank <- seq(3, ncol(popular_names_by_year_female), 3)
id <- c(1, rank) 
popular_names_by_year_female[,id] <- as.numeric(as.character(unlist(popular_names_by_year_female[,id])))

number <- seq(5, ncol(popular_names_by_year_female), 3)
popular_names_by_year_female[,number] <- lapply(popular_names_by_year_female[,number],function(x){as.numeric(gsub(",", "", x))})


#should only divide by number of years actually available
popular_names_by_year_female$noughts_sum_annual_frequency <- rowSums(popular_names_by_year_female[,c('Number.of.females.2000',
                                                                    'Number.of.females.2001', 'Number.of.females.2002', 
                                                                    'Number.of.females.2003','Number.of.females.2004',
                                                                    'Number.of.females.2005', 'Number.of.females.2006', 
                                                                    'Number.of.females.2007', 'Number.of.females.2008',
                                                                    'Number.of.females.2009')], na.rm=TRUE)

popular_names_by_year_female$noughts_available_years <- rowSums(!is.na(popular_names_by_year_female[,c('Number.of.females.2000',
                                                                                                'Number.of.females.2001', 'Number.of.females.2002', 
                                                                                                'Number.of.females.2003','Number.of.females.2004',
                                                                                                'Number.of.females.2005', 'Number.of.females.2006', 
                                                                                                'Number.of.females.2007', 'Number.of.females.2008',
                                                                                                'Number.of.females.2009')]))

popular_names_by_year_female$noughts_average_annual_frequency <- ifelse(popular_names_by_year_female$noughts_sum_annual_frequency==0, 0, popular_names_by_year_female$noughts_sum_annual_frequency/popular_names_by_year_female$noughts_available_years)

popular_names_by_year_female$tens_sum_annual_frequency <- rowSums(popular_names_by_year_female[,c('Number.of.females.2010',
                                                                                                      'Number.of.females.2011', 'Number.of.females.2012', 
                                                                                                      'Number.of.females.2013','Number.of.females.2014',
                                                                                                      'Number.of.females.2015', 'Number.of.females.2016')], na.rm=TRUE)


popular_names_by_year_female$tens_available_years <- rowSums(!is.na(popular_names_by_year_female[,c('Number.of.females.2010',
                                                                                                    'Number.of.females.2011', 'Number.of.females.2012', 
                                                                                                    'Number.of.females.2013','Number.of.females.2014',
                                                                                                    'Number.of.females.2015', 'Number.of.females.2016')]))

popular_names_by_year_female$tens_average_annual_frequency <- ifelse(popular_names_by_year_female$tens_sum_annual_frequency==0, 0, popular_names_by_year_female$tens_sum_annual_frequency/popular_names_by_year_female$tens_available_years)



popular_names_by_year_female$percent_change_in_average_annual_frequency <- (popular_names_by_year_female$tens_average_annual_frequency - popular_names_by_year_female$noughts_average_annual_frequency)/popular_names_by_year_female$noughts_average_annual_frequency

popular_names_by_year_female$total_tens_noughts_years_available <- popular_names_by_year_female$tens_available_years + popular_names_by_year_female$noughts_available_years

names_going_extinct_female <- popular_names_by_year_female[order(popular_names_by_year_female$percent_change_in_average_annual_frequency),]

#exclude names that are available for less than 5 years
names_going_extinct_female <- subset(names_going_extinct_female, names_going_extinct$total_tens_noughts_years_available>5)
names_going_extinct_female <- popular_names_by_year_female[order(popular_names_by_year_female$percent_change_in_average_annual_frequency, -popular_names_by_year_female$noughts_average_annual_frequency),]
names_going_extinct_female_narrow <- names_going_extinct_female[, c(2, 363:421)]

write.csv(names_going_extinct_female, "names_going_extinct_female_full.csv")
write.csv(names_going_extinct_female_narrow, "names_going_extinct_female_narrow.csv")

popular_names_by_year_male <- read.csv("popular_names_by_year_male.csv", header=TRUE)
rank <- seq(3, ncol(popular_names_by_year_male), 3)
id <- c(1, rank) 
popular_names_by_year_male[,id] <- as.numeric(as.character(unlist(popular_names_by_year_male[,id])))

number <- seq(5, ncol(popular_names_by_year_male), 3)
popular_names_by_year_male[,number] <- lapply(popular_names_by_year_male[,number],function(x){as.numeric(gsub(",", "", x))})


#should only divide by number of years actually available
popular_names_by_year_male$noughts_sum_annual_frequency <- rowSums(popular_names_by_year_male[,c('Number.of.males.2000',
                                                                                                 'Number.of.males.2001', 'Number.of.males.2002', 
                                                                                                 'Number.of.males.2003','Number.of.males.2004',
                                                                                                 'Number.of.males.2005', 'Number.of.males.2006', 
                                                                                                 'Number.of.males.2007', 'Number.of.males.2008',
                                                                                                 'Number.of.males.2009')], na.rm=TRUE)

popular_names_by_year_male$noughts_available_years <- rowSums(!is.na(popular_names_by_year_male[,c('Number.of.males.2000',
                                                                                                   'Number.of.males.2001', 'Number.of.males.2002', 
                                                                                                   'Number.of.males.2003','Number.of.males.2004',
                                                                                                   'Number.of.males.2005', 'Number.of.males.2006', 
                                                                                                   'Number.of.males.2007', 'Number.of.males.2008',
                                                                                                   'Number.of.males.2009')]))

popular_names_by_year_male$noughts_average_annual_frequency <- ifelse(popular_names_by_year_male$noughts_sum_annual_frequency==0, 0, popular_names_by_year_male$noughts_sum_annual_frequency/popular_names_by_year_male$noughts_available_years)

popular_names_by_year_male$tens_sum_annual_frequency <- rowSums(popular_names_by_year_male[,c('Number.of.males.2010',
                                                                                              'Number.of.males.2011', 'Number.of.males.2012', 
                                                                                              'Number.of.males.2013','Number.of.males.2014',
                                                                                              'Number.of.males.2015', 'Number.of.males.2016')], na.rm=TRUE)


popular_names_by_year_male$tens_available_years <- rowSums(!is.na(popular_names_by_year_male[,c('Number.of.males.2010',
                                                                                                'Number.of.males.2011', 'Number.of.males.2012', 
                                                                                                'Number.of.males.2013','Number.of.males.2014',
                                                                                                'Number.of.males.2015', 'Number.of.males.2016')]))

popular_names_by_year_male$tens_average_annual_frequency <- ifelse(popular_names_by_year_male$tens_sum_annual_frequency==0, 0, popular_names_by_year_male$tens_sum_annual_frequency/popular_names_by_year_male$tens_available_years)



popular_names_by_year_male$percent_change_in_average_annual_frequency <- (popular_names_by_year_male$tens_average_annual_frequency - popular_names_by_year_male$noughts_average_annual_frequency)/popular_names_by_year_male$noughts_average_annual_frequency

popular_names_by_year_male$total_tens_noughts_years_available <- popular_names_by_year_male$tens_available_years + popular_names_by_year_male$noughts_available_years

names_going_extinct_male <- popular_names_by_year_male[order(popular_names_by_year_male$percent_change_in_average_annual_frequency),]

#exclude names that are available for less than 5 years
names_going_extinct_male <- subset(names_going_extinct_male, names_going_extinct$total_tens_noughts_years_available>5)
names_going_extinct_male <- popular_names_by_year_male[order(popular_names_by_year_male$percent_change_in_average_annual_frequency, -popular_names_by_year_male$noughts_average_annual_frequency),]
names_going_extinct_male_narrow <- names_going_extinct_male[, c(2, 363:421)]

write.csv(names_going_extinct_male, "names_going_extinct_male_full.csv")
write.csv(names_going_extinct_male_narrow, "names_going_extinct_male_narrow.csv")
