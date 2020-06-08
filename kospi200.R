library(rvest)
library(httr)
library(tidyverse)
library(xts)
library(dplyr)
library(data.table)

kospi200_code_list <- list()

for (i in 1:20) {
  
  url <- paste0("https://finance.naver.com/sise/entryJongmok.nhn?&page=", i)
  html <- GET(url)
  html_code_list <-
    read_html(html, encoding = "EUC-KR") %>% 
    html_nodes(., 'table') %>%
    html_nodes(., 'td') %>%
    html_nodes(., 'a') %>%
    html_attr(., 'href')
  
  html_code_list <- html_code_list[1:10] 
  kospi200_code_list <- append(kospi200_code_list, unlist(lapply(html_code_list, str_sub, start = 21)))
}

kospi200_code_list <- unlist(kospi200_code_list)

GetDataFrame <- function(code) {
  code <- paste0(code, ".KS")
  
  df <- getSymbols(Symbols=code, 
                   src = "yahoo", 
                   from = "2020-01-01", 
                   to = "2020-03-31", auto.assign = F)
  df <- as.data.table(df)
  df$Code <- code
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted", "Code")
  
  df
}

df <- GetDataFrame(kospi200_code_list[1])
for (code in kospi200_code_list[2:200]) {
  print(code)
  df1 <- GetDataFrame(code)
  df <- rbind(df, df1)  
}
saveRDS(df, 'd:/temp/kospi200.rds')
kospi200
