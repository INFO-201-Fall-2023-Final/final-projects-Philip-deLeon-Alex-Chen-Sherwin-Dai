library("stringr")
library(dplyr)

unemp_df <- read.csv("october2023unemploymentrates.csv")
education_df <- read.csv("State_education_final.csv")

merge_df <- merge(x = unemp_df, y = education_df, by.x = "State", by.y = "States",
            all.x = TRUE)
write.csv(merge_df, "C:\\Users\\alexc\\Downloads\\merged_df.csv", row.names=FALSE)