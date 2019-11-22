#Web Scraping Code

#This project used several different sources of information on protests in China. 
#This dataset comes from a group of Chinese activists.  Information about protest events from 2013 to 2016 was posted on their website.
#The website contains information on over 40,000 protest events, but the data is not compiled into a spreadsheet.
#The only practical solution to obtaining the data in a useable format is to scrape the data from the website and compile it into a table myself. 
#The website is https://newsworthknowingcn.blogspot.com (all information on the website is in Chinese)



library(rvest)
library(purrr)
library(stringi)
library(tidyverse)
library(stringr)
library(xml2)
library(pingr)
library(qdapRegex)

#Creating a list of all the URLs
#All webpages follow the form: https://newsworthknowingcn.blogspot.com/year/month/year-month-day
#example: https://newsworthknowingcn.blogspot.com/2016/06/2016-06-13

seqB <- seq(as.Date('2013/06/01'), as.Date('2016/06/01'), by ='month' )
seqB <- gsub('-',"/"  ,seqB)
seqB <- gsub('.{2}$',""  ,seqB)
month_urlsA <-  paste0('https://newsworthknowingcn.blogspot.com/' ,seqB)  #a list of urls for each month
#month_urls_16 <- subset(month_urlsA, grepl( "/2016",  month_urlsA)==TRUE)

###################################
#get the links for every day
length(month_urlsA) #37
datalist <- list()
for (i in 1:37) {
  Wickedonna_html <- read_html(month_urlsA[i])
  Wickedonna_nodes <- html_nodes(Wickedonna_html, ".posts a") %>%
    html_attr("href") 
  dat <- data.frame(Wickedonna_nodes)
  dat$i <- i
  datalist[[i]] <- dat
}
all_the_links = do.call(rbind, datalist)
all_the_links_ch <- as.character(all_the_links$Wickedonna_nodes  )
setwd("/Users/phillipcecil/Documents/Masters Econ/thesis/china/wickedonna")
write.csv(all_the_links_ch, "all_the_links.csv")
links_cleaned <- read.csv(  "sites_cleaned.csv")#I removed all non day links in excel
links_cleaned_ch <- as.character(links_cleaned$sites) 

###################################################
#Web scraping all of the text for every day of 2016

#get the links for 2016
links_cleaned_ch_16 <- subset( links_cleaned_ch, grepl( "/2016",links_cleaned_ch  )==TRUE)
head(links_cleaned_ch_16)

#extract the text
datalist_headlines_16 <- list()
length(links_cleaned_ch_16)#167
for (i in 1:167) {
  Wickedonna_lines_html_16 <- read_html(links_cleaned_ch_16[i])
  Wickedonna_lines_nodes_16 <- html_nodes(Wickedonna_lines_html_16, "b")
  Wickedonna_lines_text_16 <- html_text(Wickedonna_lines_nodes_16)
  dat_lines_16 <- data.frame(Wickedonna_lines_text_16)
  dat_lines_16$i <- i
  datalist_headlines_16[[i]] <- dat_lines_16
}
all_the_lines_16 = do.call(rbind, datalist_headlines_16)
#remove irrelevent rows from the data set
all_the_lines_16B <- subset(  all_the_lines_16, stri_length(all_the_lines_16$Wickedonna_lines_text_16) > 3)

#save the data as a table
all_the_lines_16B$Wickedonna_lines_text_16 <- as.character(all_the_lines_16B$Wickedonna_lines_text_16 )
write.table(all_the_lines_16B, file = "all_the_lines_16B.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

######################################
#the text for every day of 2015

#get the links for 2015
links_cleaned_ch_15 <- subset( links_cleaned_ch, grepl( "/2015",links_cleaned_ch  )==TRUE)

#Extract the text
datalist_headlines_15 <- list()
length(links_cleaned_ch_15)#368
for (i in 1:368) {
  Wickedonna_lines_html_15 <- read_html(links_cleaned_ch_15[i])
  Wickedonna_lines_nodes_15 <- html_nodes(Wickedonna_lines_html_15, "b")
  Wickedonna_lines_text_15 <- html_text(Wickedonna_lines_nodes_15)
  dat_lines_15 <- data.frame(Wickedonna_lines_text_15)
  dat_lines_15$i <- i
  datalist_headlines_15[[i]] <- dat_lines_15
}
all_the_lines_15 = do.call(rbind, datalist_headlines_15)

#Format the data abbd export as a CSV file
all_the_lines_15B <- subset(  all_the_lines_15, stri_length(all_the_lines_15$Wickedonna_lines_text_15) > 3)
all_the_lines_15B$Wickedonna_lines_text_15 <- as.character(all_the_lines_15B$Wickedonna_lines_text_15 )
write.table(all_the_lines_15B, file = "all_the_lines_15B_new.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

######################################
#the text for every day of 2014

#get the links for 2014
links_cleaned_ch_14 <- subset( links_cleaned_ch, grepl( "/2014",links_cleaned_ch  )==TRUE)

#Extract the text
datalist_headlines_14 <- list()
length(links_cleaned_ch_14)#368
for (i in 1:368) {
  Wickedonna_lines_html_14 <- read_html(links_cleaned_ch_14[i])
  Wickedonna_lines_nodes_14 <- html_nodes(Wickedonna_lines_html_14, "b")
  Wickedonna_lines_text_14 <- html_text(Wickedonna_lines_nodes_14)
  dat_lines_14 <- data.frame(Wickedonna_lines_text_14)
  dat_lines_14$i <- i
  datalist_headlines_14[[i]] <- dat_lines_14
}
all_the_lines_14 = do.call(rbind, datalist_headlines_14)

#Format the data and export as a CSV file
all_the_lines_14B <- subset(  all_the_lines_14, stri_length(all_the_lines_14$Wickedonna_lines_text_14) > 3)
all_the_lines_14B$Wickedonna_lines_text_14 <- as.character(all_the_lines_14B$Wickedonna_lines_text_14 )
write.table(all_the_lines_14B, file = "all_the_lines_14B.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

######################################
#the text for every day of 2013

#get the links for 2014
links_cleaned_ch_13 <- subset( links_cleaned_ch, grepl( "/2013",links_cleaned_ch  )==TRUE)

#Extract the data
datalist_headlines_13 <- list()
length(links_cleaned_ch_13)#210
for (i in 1:210) {
  Wickedonna_lines_html_13 <- read_html(links_cleaned_ch_13[i])
  Wickedonna_lines_nodes_13 <- html_nodes(Wickedonna_lines_html_13, ".entry-content")
  Wickedonna_lines_text_13 <- html_text(Wickedonna_lines_nodes_13)
  dat_lines_13 <- data.frame(Wickedonna_lines_text_13)
  dat_lines_13$i <- i
  datalist_headlines_13[[i]] <- dat_lines_13
}
all_the_lines_13 = do.call(rbind, datalist_headlines_13)

#Format the data and export as a CSV file
all_the_lines_13B <- subset(  all_the_lines_13, stri_length(all_the_lines_13$Wickedonna_lines_text_13) > 3)
all_the_lines_13B$Wickedonna_lines_text_13 <- as.character(all_the_lines_13B$Wickedonna_lines_text_13 )

df_13_split <- data.frame()
for( i in 1:210 ) {
  all_the_lines_13B_split <- strsplit(all_the_lines_13B$Wickedonna_lines_text_13[i], split = "\n")
  all_the_lines_13B_split_df <- data.frame(all_the_lines_13B_split)
  names(all_the_lines_13B_split_df) <- "x"
  df_13_split <- rbind(df_13_split, all_the_lines_13B_split_df  )
}
df_13_split$date <- NA
write.table(df_13_split, file = "df_13_split.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

