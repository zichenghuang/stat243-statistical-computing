## Zicheng Huang
## PS6


# 2
library(RSQLite)
drv <- dbDriver("SQLite")
dir <- 'C:/HZC/Berkeley/STAT243/ps6' # relative or absolute path to where the .db file is
dbFilename <- 'stackoverflow-2016.db'
db <- dbConnect(drv, dbname = file.path(dir, dbFilename))

result <- dbGetQuery(db, "select distinct userid, displayname from 
                     users U, questions Q, questions_tags T where
                     U.userid = Q.ownerid and
                     Q.questionid = T.questionid and
                     tag = 'r' and U.userid not in 
                     (select distinct userid from 
                     users U, questions Q, questions_tags T where 
                     U.userid = Q.ownerid and 
                     Q.questionid = T.questionid and 
                     tag = 'python')")

length(result$userid)


# 3
library(dplyr)
setwd('C:/HZC/Berkeley/STAT243/ps6')
# read in the result processed in Spark
BO <- read.csv("Obama", header = F, na.strings = "", stringsAsFactors = F)
# name each columns
names(BO) <- c("Date", "Time", "Language", "NumOfHits")
# change the Date column to date format
BO$Date <- as.Date(as.character(BO$Date), "%Y%m%d")
# obtain a table containing date and hits per day
Obama <- BO %>% group_by(Date) %>% summarize(HitsPerDay = sum(NumOfHits))
# read in the result processed in Spark
JM <- read.csv("McCain", header = F, na.strings = "", stringsAsFactors = F)
# name each columns
names(JM) <- c("Date", "Time", "Language", "NumOfHits")
# change the Date column to date format
JM$Date <- as.Date(as.character(JM$Date), "%Y%m%d")
# obtain a table containing date and hits per day
McCain <- JM %>% group_by(Date) %>% summarize(HitsPerDay = sum(NumOfHits))
# make a plot
plot(Obama$Date, Obama$HitsPerDay,
     type = "l", xlab = "Date", ylab = "Hits per Day",
     ylim = c(0, 4000000))
lines(McCain$Date, McCain$HitsPerDay, lty = 2)


# 4
library(parallel)
library(doParallel)
library(foreach)
library(stringr)

ncores <- as.integer(Sys.getenv("SLURM_CPUS_ON_NODE"))

registerDoParallel(ncores)

nSub <- 959

obama <- foreach(i=0:nSub,
                 .packages = c("stringr"),  # libraries to load onto each worker
                 .combine= c,               # how to combine results
                 .verbose=TRUE) %dopar%     # print statuses of each job
                 {
                   file <- paste("/global/scratch/paciorek/wikistats_full/dated_for_R/part-",
                                 str_pad(i, width=5, side="left", pad="0"), sep="")
                   table <- readLines(file)
                   output <- grep("Barack_Obama", table, value = TRUE)
                   output
                 }

df <- data.frame(do.call(rbind, strsplit(obama, " ", fixed = TRUE)))

dimension <- dim(df)[1]
sample <- head(df)
write.table(dimension, file='/global/home/users/zicheng_huang/dimension.txt')
write.table(dimension, file='/global/home/users/zicheng_huang/dimension')
write.table(sample, file='/global/home/users/zicheng_huang/sample.txt')
write.table(sample, file='/global/home/users/zicheng_huang/sample')


read.csv("dimension.txt")
