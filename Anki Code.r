# Anki database schema: https://gist.github.com/sartak/3921255

library("DBI")
library("RSQLite")
library("reshape2")

trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

# connect to the sqlite file
setwd("C:/Users/KMCBETH/Documents/Anki/Kevin")
dbPath <- "C:/Users/KMCBETH/Documents/Anki/Kevin/collection.anki2"
con = dbConnect(RSQLite::SQLite(), dbname="collection.anki2")

alltables = dbListTables(con)

# Why are there lots of nulls?
# Note the division by 1000.0 to convert from milliseconds to seconds AND cast as float
# otherwise the numbers turn negative!
Reviews <- dbGetQuery(con, 'select a.id / 1000.0 as id,
                      a.cid / 1000.0 as cid,
                      a.usn,
                      a.ease,
                      a.ivl,
                      a.lastIvl,
                      a.factor,
                      a.time,
                      a.type,
                      b.tags 
                      from revlog as a
                      left join 
                        (select a.id, a.nid, b.tags 
                          from cards as a 
                          left join notes as b 
                          on a.nid = b.id) 
                      as b 
                      on a.cid = b.id')
Cards <- dbGetQuery(con, 'select a.id / 1000.0 as id, 
                          a.nid / 1000.0 as nid,
                          a.queue,
                          a.due * 1.0 as due,
                          a.ivl,
                          b.tags 
                          from cards as a 
                          left join notes as b 
                          on a.nid = b.id')
dbDisconnect(con)

### Cleaning Cards Table
Cards$id <- as.POSIXct(Cards$id, origin="1970-01-01") # Convert epoch time to R time
Cards$nid <- as.POSIXct(Cards$nid, origin="1970-01-01") # Convert epoch time to R time
Cards$tags <- trim(Cards$tags)
SplitTags <- strsplit(Cards$tags, split = " ")

Cards <- data.frame(id = rep(Cards$id, sapply(SplitTags, length)), 
                   nid = rep(Cards$nid, sapply(SplitTags, length)), 
                   queue = rep(Cards$queue, sapply(SplitTags, length)), 
                   due = rep(Cards$due, sapply(SplitTags, length)),
                   ivl = rep(Cards$ivl, sapply(SplitTags, length)),
                   tags = unlist(SplitTags))
Cards$One <- 1
DeDupedCards <- Cards[!(Cards$tags %in% c("2000", "2001", "2002", "2003", "2004", 
                                          "2005", "2006", "2007", "2008", "2009",
                                          "2010", "2011", "2012", "2013", "2014", 
                                          "2015", 
                                          "Exam6", "Exam7", "Exam8",
                                          "CAS", "CasualFellow", "Notecard",
                                          "marked")),]
blah <- xtabs(~tags + queue, data = DeDupedCards)



# Cleaning Reviews Table
Reviews$id <- as.POSIXct(Reviews$id, origin="1970-01-01") # Convert epoch time to R time
Reviews$cid <- as.POSIXct(Reviews$cid, origin="1970-01-01") # Convert epoch time to R time
for (i in 1:length(Reviews$ease)){
  Reviews$correct[i] <- min(Reviews$ease[i] - 1, 1)
}
  
        # Converting 1, 2, 3, 4 to 0 (fail) and 1 (success)
Reviews$tags <- trim(Reviews$tags)
SplitTags <- strsplit(Reviews$tags, split = " ")

test <- data.frame(id = rep(Reviews$id, sapply(SplitTags, length)), 
                   cid = rep(Reviews$cid, sapply(SplitTags, length)), 
                   usn = rep(Reviews$usn, sapply(SplitTags, length)), 
                   ease = rep(Reviews$ease, sapply(SplitTags, length)),
                   correct = rep(Reviews$correct, sapply(SplitTags, length)),
                   ivl = rep(Reviews$ivl, sapply(SplitTags, length)),
                   lastIvl = rep(Reviews$lastIvl, sapply(SplitTags, length)),
                   factor = rep(Reviews$factor, sapply(SplitTags, length)),
                   time = rep(Reviews$time, sapply(SplitTags, length)),
                   type = rep(Reviews$type, sapply(SplitTags, length)),
           tags = unlist(SplitTags))

library(dplyr)
test %>%
  group_by(tags) %>%
  summarise(Hit = mean(correct), Reviews = n()) %>%
  filter(!(grepl("^20|CAS|^Exam|CasualFellow|Notecard", tags)))