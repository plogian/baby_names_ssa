library(XML)

territories_df <- data.frame()
  
for (i in 1:19) {
  year <- i+1997
  puerto_rico <- paste0("https://www.ssa.gov/oact/babynames/territory/puertorico", year, ".html")
  html <- readLines(puerto_rico)
  table_html <- readHTMLTable(html)[[2]]
  table_html <- table_html[-1, ]
  table_html$year <- year
  table_html$territory <- "Puerto Rico"
  territories_df <- rbind(territories_df, table_html)
}

for (i in 1:19) {
  year <- i+1997
  other_territories <- paste0("https://www.ssa.gov/oact/babynames/territory/allother", year, ".html")
  html <- readLines(other_territories)
  table_html <- readHTMLTable(html)[[2]]
  table_html <- table_html[-1, ]
  table_html$year <- year
  table_html$territory <- "All Other Territories"
  territories_df <- rbind(territories_df, table_html)
}

names(territories_df) <- c("Rank", "Male name", "Number of males", "Female name", "Number of females", "Year", "Territory")
write.csv(territories_df, "popular_names_by_territories.csv")
