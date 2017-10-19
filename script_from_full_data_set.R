zip_url <- "https://www.ssa.gov/oact/babynames/names.zip"
download.file(zip_url, "names.zip")

female_names_all <- data.frame()
male_names_all <- data.frame()

for (i in 1880:2016) {
  assign(paste0("names_", i), read.table(unz("names.zip", paste0("yob", i, ".txt")), sep=","))
  df <- eval(parse(text=paste0("names_", i)))
  #order by number of births 
  df <- df[order(-df$V3),]
  
  #split the dataset into male and female
  male_names <- subset(df, df$V2=="M")
  female_names <- subset(df, df$V2=="F")
  
  male_names <- male_names[, -2]
  female_names <- female_names[, -2]
  
  male_names$rank <- 0
  female_names$rank <- 0
  
  names(male_names) <- c("Name", paste0("Number of Births in ", i), paste0("Rank in ", i)) 
  names(female_names) <- c("Name", paste0("Number of Births in ", i), paste0("Rank in ", i)) 
  
  #give each name a rank for that year
  for (k in 1:nrow(male_names)) {
    male_names[k, 3] <- k
  }
  
  for (k in 1:nrow(female_names)) {
    female_names[k, 3] <- k
  }
  
  if(nrow(female_names_all)> 0){
    female_names_all <-merge(female_names_all, female_names, all.x= TRUE, all.y=TRUE)
  } else {
    female_names_all <- female_names
  } 
  
  if(nrow(male_names_all)> 0){
    male_names_all <-merge(male_names_all, male_names, all.x= TRUE, all.y=TRUE)
  } else {
    male_names_all <- male_names
  } 
}

write.csv(female_names_all, "female_names_all.csv")
write.csv(male_names_all, "male_names_all.csv")

#now, find A.A.R. 1880-2009, 2010-2016, create csvs female_names_1880; male_names_1880
female_names_1880 <- female_names_all
male_names_1880 <- male_names_all

ids <- seq(3, 261, 2)
ids_2 <- seq(263, 275, 2)
ids_mid <- seq(243,261, 2)

female_names_1880$aar_1880_to_2009 <- round(rowMeans(subset(female_names_1880, select= ids), na.rm=T), digits=0)
male_names_1880$aar_1880_to_2009 <- round(rowMeans(subset(male_names_1880, select= ids), na.rm=T), digits=0)

female_names_1880$aar_2000_to_2009 <- round(rowMeans(subset(female_names_1880, select= ids_mid), na.rm=T), digits=0)
male_names_1880$aar_2000_to_2009 <- round(rowMeans(subset(male_names_1880, select= ids_mid), na.rm=T), digits=0)

female_names_1880$aar_2010_to_2016 <- round(rowMeans(subset(female_names_1880, select= ids_2), na.rm=T), digits=0)
male_names_1880$aar_2010_to_2016 <- round(rowMeans(subset(male_names_1880, select= ids_2), na.rm=T), digits=0)

female_names_1880$aar_percent_change <- (female_names_1880$aar_1880_to_2009-female_names_1880$aar_2010_to_2016)/female_names_1880$aar_1880_to_2009
female_names_1880 <- female_names_1880[order(female_names_1880$aar_percent_change),]
female_names_1880_prep <- female_names_1880[,c(1, 276:278)]
write.csv(female_names_1880_prep, "female_names_1880.csv")

male_names_1880$aar_percent_change <- (male_names_1880$aar_1880_to_2009-male_names_1880$aar_2010_to_2016)/male_names_1880$aar_1880_to_2009
male_names_1880 <- male_names_1880[order(male_names_1880$aar_percent_change),]
male_names_1880_prep <- male_names_1880[,c(1, 276:278)]
write.csv(male_names_1880_prep, "male_names_1880.csv")

#now do same calculation for 2000 to 2009
female_names_1880$aar_percent_change_mid <- (female_names_1880$aar_2000_to_2009-female_names_1880$aar_2010_to_2016)/female_names_1880$aar_2000_to_2009
female_names_2000 <- female_names_1880[order(female_names_1880$aar_percent_change_mid),]
female_names_2000_prep <- female_names_2000[c(1, 277, 279:280)]
write.csv(female_names_2000_prep, "female_names_2000.csv")

male_names_1880$aar_percent_change_mid <- (male_names_1880$aar_2000_to_2009-male_names_1880$aar_2010_to_2016)/male_names_1880$aar_2000_to_2009
male_names_2000 <- male_names_1880[order(male_names_1880$aar_percent_change_mid),]
male_names_2000_prep <- male_names_2000[c(1, 277, 279:280)]
write.csv(male_names_2000_prep, "male_names_2000.csv")