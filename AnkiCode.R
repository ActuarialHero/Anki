# Anki database schema: https://gist.github.com/sartak/3921255
# Have to set folder on line 22
options(dplyr.print_max = 1e9)

DBFolder <- "C:/BlahBlahBlah" # Set database folder here
args <- commandArgs(trailingOnly = TRUE) # I have Excel pass in the number of days via command line

library("DBI")
library("RSQLite")
library("reshape2")
library("plyr")
library("dplyr")
library("scales")

TagFilters <- "^20|CAS|^Exam|CasualFellow|Notecard|marked|^Ch|leech"
NumberOfDays <- as.numeric(args[1])

trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)
}

# connect to the sqlite file
setwd(DBFolder) # Set to folder containing the file below
con = dbConnect(RSQLite::SQLite(), dbname="collection.anki2")

# Note the division by 1000.0 to convert from milliseconds to seconds AND cast as float
# otherwise the numbers turn negative!
Reviews <- dbGetQuery(con, paste("select a.id / 1000.0 as id,
                      a.cid / 1000.0 as cid,
                      a.usn as usn,
                      a.ease as ease,
                      a.ivl as ivl,
                      a.lastIvl as lastIvl,
                      a.factor as factor,
                      a.time as time,
                      a.type as type,
                      b.tags as tags 
                      from revlog as a
                      inner join 
                        (select a.id, a.nid, b.tags 
                          from cards as a 
                          left join notes as b 
                          on a.nid = b.id) 
                      as b 
                      on a.cid = b.id
                      where a.id / 1000.0 >", as.integer(Sys.time()) - NumberOfDays * 86400))
Cards <- dbGetQuery(con, 'select a.id / 1000.0 as id, 
                          a.nid / 1000.0 as nid,
                          a.did / 1000.0 as did,
                          a.queue,
                          a.due * 1.0 as due,
                          a.ivl,
                          b.tags 
                          from cards as a 
                          left join notes as b 
                          on a.nid = b.id
                          where a.did <> 1433462613923') # Filters out CPCU cards
dbDisconnect(con)

############################################
### This section summarizes up the cards ###
############################################
Cards$id <- as.POSIXct(Cards$id, origin="1970-01-01") # Convert epoch time to R time
Cards$nid <- as.POSIXct(Cards$nid, origin="1970-01-01") # Convert epoch time to R time

# Split out the different kinds of cards
AllCards <- Cards
NotecardCards <- Cards[grepl("Notecard", Cards$tags),]
CASCards <- Cards[grepl("Exam", Cards$tags),]
CFCards <- Cards[grepl("CasualFellow", Cards$tags),]

# Total Counts
TotalRow <- data.frame(Paper = "Total", 
                       Cards = nrow(AllCards),
                       Notecards = nrow(NotecardCards),
                       CASProblems = nrow(CASCards),
                       CFProblems = nrow(CFCards),
                       Suspended = nrow(AllCards[AllCards$queue == -1,]),
                       New = nrow(AllCards[AllCards$queue == 0,]),
                       Learn = nrow(AllCards[AllCards$queue == 1,]),
                       Review = nrow(AllCards[AllCards$queue == 2,]),
                       Relearn = nrow(AllCards[AllCards$queue == 3,]))

# Split tags into separate records
CardTagSplitter <- function(Input) {
  Input$tags <- trim(Input$tags)
  SplitTags <- strsplit(Input$tags, split = " ")
  
  Output <- data.frame(id = rep(Input$id, sapply(SplitTags, length)), 
                       nid = rep(Input$nid, sapply(SplitTags, length)), 
                       queue = rep(Input$queue, sapply(SplitTags, length)), 
                       due = rep(Input$due, sapply(SplitTags, length)),
                       ivl = rep(Input$ivl, sapply(SplitTags, length)),
                       tags = unlist(SplitTags))
  
  return(Output)
}

# Run splitting function
AllCards <- CardTagSplitter(AllCards)
NotecardCards <- CardTagSplitter(NotecardCards)
CASCards <- CardTagSplitter(CASCards)
CFCards <- CardTagSplitter(CFCards)

# Summarization function
SummarizeCards <- function(Input){
  Output <- Input %>%
    group_by(tags) %>%
    summarise(Count = n()) %>%
    filter(!(grepl(TagFilters, tags)))
  
  return(Output)
}

# Run summarization function
AllCardSummary <- SummarizeCards(AllCards)
AllCardSummary <- rename(AllCardSummary, Paper = tags, Cards = Count)

NotecardCardSummary <- SummarizeCards(NotecardCards)
NotecardCardSummary <- rename(NotecardCardSummary, Paper = tags, Notecards = Count)

CASCardSummary <- SummarizeCards(CASCards)
CASCardSummary <- rename(CASCardSummary, Paper = tags, CASProblems = Count)

CFCardSummary <- SummarizeCards(CFCards)
CFCardSummary <- rename(CFCardSummary, Paper = tags, CFProblems = Count)

SuspendedCardSummary <- SummarizeCards(AllCards[which(AllCards$queue == -1),])
SuspendedCardSummary <- rename(SuspendedCardSummary, Paper = tags, Suspended = Count)

NewCardSummary <- SummarizeCards(AllCards[which(AllCards$queue == 0),])
NewCardSummary <- rename(NewCardSummary, Paper = tags, New = Count)

LearnCardSummary <- SummarizeCards(AllCards[which(AllCards$queue == 1),])
LearnCardSummary <- rename(LearnCardSummary, Paper = tags, Learn = Count)

ReviewCardSummary <- SummarizeCards(AllCards[which(AllCards$queue == 2),])
ReviewCardSummary <- rename(ReviewCardSummary, Paper = tags, Review = Count)

RelearnCardSummary <- SummarizeCards(AllCards[which(AllCards$queue == 3),])
RelearnCardSummary <- rename(RelearnCardSummary, Paper = tags, Relearn = Count)

SummaryList <- list(NotecardCardSummary, 
                    CASCardSummary, 
                    CFCardSummary, 
                    SuspendedCardSummary, 
                    NewCardSummary, 
                    LearnCardSummary, 
                    ReviewCardSummary, 
                    RelearnCardSummary)

CardSummaries <- AllCardSummary
for ( .df in SummaryList) {
  CardSummaries <-merge(CardSummaries, .df, by="Paper", all=TRUE)
}

##############################################
### This section summarizes up the reviews ###
##############################################

# Cleaning Reviews Table
Reviews$id <- as.POSIXct(Reviews$id, origin="1970-01-01") # Convert epoch time to R time
Reviews$cid <- as.POSIXct(Reviews$cid, origin="1970-01-01") # Convert epoch time to R time
# Converting 1, 2, 3, 4 to 0 (fail) and 1 (success)
for (i in 1:length(Reviews$ease)){
  Reviews$correct[i] <- min(Reviews$ease[i] - 1, 1)
}

# Splitting into parts
AllReviews <- Reviews
NotecardReviews <- Reviews[grepl("Notecard", Reviews$tags),]
CASReviews <- Reviews[grepl("Exam", Reviews$tags),]
CFReviews <- Reviews[grepl("CasualFellow", Reviews$tags),]

TotalRow <- cbind(TotalRow,
                  OverallReviews = nrow(AllReviews),
                  OverallHit = mean(AllReviews$correct),
                  OverallTime = mean(AllReviews$time / 86400000.0),
                  NotecardReviews = nrow(NotecardReviews),
                  NotecardHit = mean(NotecardReviews$correct),
                  NotecardTime = mean(NotecardReviews$time / 86400000.0),
                  CASReviews = nrow(CASReviews),
                  CASHit = mean(CASReviews$correct),
                  CASTime = mean(CASReviews$time / 86400000.0),
                  CFReviews = nrow(CFReviews),
                  CFHit = mean(CFReviews$correct),
                  CFTime = mean(CFReviews$time / 86400000.0))

ReviewTagSplitter <- function(Input) {
  Input$tags <- trim(Input$tags)
  SplitTags <- strsplit(Input$tags, split = " ")
  
  Output <- data.frame(id = rep(Input$id, sapply(SplitTags, length)), 
                     cid = rep(Input$cid, sapply(SplitTags, length)), 
                     usn = rep(Input$usn, sapply(SplitTags, length)), 
                     ease = rep(Input$ease, sapply(SplitTags, length)),
                     correct = rep(Input$correct, sapply(SplitTags, length)),
                     ivl = rep(Input$ivl, sapply(SplitTags, length)),
                     lastIvl = rep(Input$lastIvl, sapply(SplitTags, length)),
                     factor = rep(Input$factor, sapply(SplitTags, length)),
                     time = rep(Input$time, sapply(SplitTags, length)),
                     type = rep(Input$type, sapply(SplitTags, length)),
                     tags = unlist(SplitTags))
  
  return(Output)
}

AllReviews <- ReviewTagSplitter(Reviews)
NotecardReviews <- ReviewTagSplitter(NotecardReviews)
CASReviews <- ReviewTagSplitter(CASReviews)
CFReviews <- ReviewTagSplitter(CFReviews)

SummarizeReviews <- function(Input){
  Output <- Input %>%
    group_by(tags) %>%
    summarise(Reviews = n(), Hit = percent(mean(correct)), Time = mean(time / 86400000.0)) %>%
    filter(!(grepl(TagFilters, tags)))
  
  return(Output)
}

AllReviewSummary <- SummarizeReviews(AllReviews)
AllReviewSummary <- rename(AllReviewSummary, Paper = tags, OverallReviews = Reviews, OverallHit = Hit, OverallTime = Time)

NotecardReviewSummary <- SummarizeReviews(NotecardReviews)
NotecardReviewSummary <- rename(NotecardReviewSummary, Paper = tags, NotecardReviews = Reviews, NotecardHit = Hit, NotecardTime = Time)

CASReviewSummary <- SummarizeReviews(CASReviews)
CASReviewSummary <- rename(CASReviewSummary, Paper = tags, CASReviews = Reviews, CASHit = Hit, CASTime = Time)

CFReviewSummary <- SummarizeReviews(CFReviews)
CFReviewSummary <- rename(CFReviewSummary, Paper = tags, CFReviews = Reviews, CFHit = Hit, CFTime = Time)

SummaryList <- list(AllReviewSummary, NotecardReviewSummary, CASReviewSummary, CFReviewSummary)

FullSummary <- CardSummaries
for ( .df in SummaryList) {
  FullSummary <- merge(FullSummary, .df, by="Paper", all=TRUE)
}

FullSummary <- rbind(FullSummary, TotalRow)

write.csv(FullSummary, "FullSummary.csv")
