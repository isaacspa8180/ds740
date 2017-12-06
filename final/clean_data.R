df_all <- read.csv('winemag-data-130k-v2.csv', na.strings='')

#remove duplicates
df_all_no_dups <- df_all[which(!duplicated(df_all$description)), ]

#only Alsace province
df <- df_all_no_dups[which(df_all_no_dups$province == 'Alsace'), ]

#drop levels
df <- droplevels(df)

#drop region_2
df$region_2 <- NULL

#drop taster_twitter_handle
df$taster_twitter_handle <- NULL

#description is unique so am dropping factor
df$description <- as.character(df$description)

#drop country, province as these are redundant
df$country <- NULL
df$province <- NULL

#drop X
df$X <- NULL

#peel off title, description
title_desc <- df[, c('title', 'description')]

#drop title, description
df$title <- NULL
df$description <- NULL

#NULLs designation is 16% NULL and price is 12%. What to do?
