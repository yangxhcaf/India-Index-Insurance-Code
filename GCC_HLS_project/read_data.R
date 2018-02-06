# read in data shared by eli and koen

library(tidyr)
library(dplyr)

# read all the files from HLS to ingest
files = list.files("~/Documents/India-Index-Insurance-Code/GCC_HLS_project/Data/evi2_ts_sort/","*", full.names = TRUE)

# loop over them using lapply and bind the results
HLS_evi = do.call("rbind", lapply(files, function(file){
  load(file)
  gather(index, site, evi, names(index)[6:ncol(index)], factor_key = FALSE)
}))

HLS_evi$uniqueuserid = gsub( "-.*$", "", HLS_evi$site )

write.csv(HLS_evi,file='~/Documents/India-Index-Insurance-Code/GCC_HLS_project/Data/HLS_EVI.csv')


# read GCC and Yield data
gcc_yield <- readRDS("./Documents/India-Index-Insurance-Code/GCC_HLS_project/Data/cropmonitor_merged.rds")
names(gcc_yield)[names(gcc_yield)=='CCEield'] = "yield"
gcc_yield$site = paste(gcc_yield$uniqueuserid,gcc_yield$uniquecropsiteid,sep='-')
write.csv(gcc_yield,file = './Documents/India-Index-Insurance-Code/GCC_HLS_project/Data/cropmonitor_gcc.csv')


head(gcc_yield)
head(HLS_evi)
