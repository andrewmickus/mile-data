library(rvest)
library(lubridate)
library(stringr)

url <- 'http://www.trackandfieldnews.com/index.php/category-stats/1932-splits-in-world-record-miles'
webpage <- read_html(url)
mile_table <- html_nodes(webpage, 'table')
mile <- html_table(mile_table)[[2]]

# not sure why "" isn't capturing the blank space...
blank_space <- mile[2,1]
names(mile) <- c('Time','Name', 'Year')
splits <- mile[mile$Time == blank_space,2]

#merges in mile splits
mile <- cbind(mile[mile$Time != blank_space,], splits)
mile$splits <- as.character(mile$splits)

#cleans up some messy rows
mile$splits[mile$Name == "John Walker (New Zealand)"] <- str_replace(mile$splits[mile$Name == "John Walker (New Zealand)"], "55.8.", "55.8, ")
mile$splits[mile$Name == "Norm Taber (US)"] <- c("(58.0, 67.0 [2:05.0], 68.0 [3:13.0], 59.6) ")
mile$Time[mile$Name == "Herb Elliott (Australia)"] <- "3:54.5"

#gets time in seconds for each lap
mile$lap1 <- as.numeric(substr(mile$splits, 2, 5))
mile$lap2 <- as.numeric(substr(mile$splits, 8, 11))
mile$lap3 <- as.numeric(substr(mile$splits, 23, 26))
mile$lap4 <- as.numeric(substr(mile$splits, 38, 41))

#clears up ambiguity around which century the record was set, Walter George was in the 19th century, everyone else was 20th
mile$Year2 <- sapply(1:nrow(mile), function(x) if(mile$Name[x] == "Walter George (Great Britain)") {sub("86","1886",mile$Year[x])}
                                              else{paste0(substr(mile$Year[x], 1, nchar(mile$Year[x])-2), "19", substr(mile$Year[x], nchar(mile$Year[x])-1, nchar(mile$Year[x]))  )})

mile$Year2 <- year(as.Date(mile$Year2, "%m/%d/%Y"))

#drop unused columns
mile$splits <- NULL
mile$Year   <- NULL



#get de-dupped list of record holders for look-up table
allRecordHolders <- data.frame(mile$Name[substr(mile$Name,1,1) != "—"])
names(allRecordHolders) <- "Name"

#country is everything between the ( )
allRecordHolders$Country <- sapply(1:nrow(allRecordHolders), 
                            function(x) substr(allRecordHolders$Name[x], regexpr("(",allRecordHolders$Name[x], fixed = TRUE)[1]+1, regexpr(")",allRecordHolders$Name[x], fixed = TRUE)[1]-1))
  
#delete country from name
allRecordHolders$Name <- sapply(1:nrow(allRecordHolders), 
                            function(x) substr(allRecordHolders$Name[x], 1, regexpr("(",allRecordHolders$Name[x], fixed = TRUE)[1]-1))


mile$Name <- sapply(1:nrow(mile),
                     function(x)  if(substr(mile$Name[x],1,1) == "—") 
                                      {allRecordHolders$Name[grep(substr(mile$Name[x],2, nchar(mile$Name[x])),allRecordHolders$Name)]}
                                  else 
                                      {allRecordHolders$Name[grep(word(mile$Name[x],1,2),allRecordHolders$Name)]}   )


mile$Country <- sapply(1:nrow(mile),
                    function(x)  if(substr(mile$Name[x],1,1) == "—") 
                    {allRecordHolders$Country[grep(substr(mile$Name[x],2, nchar(mile$Name[x])),allRecordHolders$Name)]}
                    else 
                    {allRecordHolders$Country[grep(word(mile$Name[x],1,2),allRecordHolders$Name)]}   )

#adds rows for my mile runs
mile <- rbind(mile, c("5:19.00", "Fat Andrew", 74, 76, 79, 90, "2016", ""),
                    c("4:37.00", "Fast Andrew", 68.75, 68.75, 68.75, 68.75, "2016", ""))

rownames(mile) <- c(1:nrow(mile))

write.csv(mile, file = "miledata.csv")

