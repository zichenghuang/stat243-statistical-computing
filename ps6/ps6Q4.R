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
write.table(sample, file='/global/home/users/zicheng_huang/sample.txt')


