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

rem_dr$navigate("https://www.ssa.gov/oact/babynames/state/")

state_df <- data.frame()

#create the state selectors
for(i in 1:51) {
    state_selector <- (paste0("#state > option:nth-child(", i, ')'))
    state <- rem_dr$findElement(using="css selector", state_selector)
    state$clickElement()
    #pick a year 
    for(k in 1:11) {
      year_selector <- paste0("#year > option:nth-child(", k, ")")
      year <- rem_dr$findElement(using="css selector", year_selector)
      year$clickElement()
      
      go_button <- rem_dr$findElement(using="css selector", "#content > div > table > tbody > tr > td:nth-child(2) > form:nth-child(3) > p > input[type='submit']")
      go_button$clickElement()
      
      caption_text <- rem_dr$findElement(using="css selector", ".greyruled-td > p:nth-child(2) > table:nth-child(1) > caption:nth-child(1)")$getElementText()
      state_year <- gsub("Popularity for top 100 names in |for births in ", "",  caption_text)
      
      table <- rem_dr$findElement(using="css selector", ".greyruled-td > p:nth-child(2) > table:nth-child(1) > tbody:nth-child(2)")
      table_html <- table$getPageSource()[[1]]
      table_r <- readHTMLTable(table_html)[[4]]
      table_r$state_year <- state_year
      names(table_r) <- c("Rank", "Male name", "Number of males", "Female name", "Number of females", "state_year")
      table_r <- table_r[-1, ]
      state_df <- rbind(state_df, table_r)
      rem_dr$navigate("https://www.ssa.gov/oact/babynames/state/")
      tryCatch({
        survey_pop_up <- rem_dr$findElement(using="css selector", "#acsMainInvite > a:nth-child(2)")
        survey_pop_up$clickElement()
        
      }, error = function(e) {}
      )
      state <- rem_dr$findElement(using="css selector", state_selector)
      state$clickElement()
    }
}


write.csv(state_df, "popular_names_by_state.csv")

