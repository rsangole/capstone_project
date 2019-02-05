


################################################################################
## Split train/validate/test into separate files
################################################################################

all <- wnv.trap.date.rev2

train <- all[all$train,]
validate <- all[all$validate,]
test <- all[all$test,]





################################################################################
## Save trap/date pairs with ancillary data sources merged to date
################################################################################

base.path <- "D:\\ajc188\\github\\capstone_project"

my.path <- paste(base.path,"data","processed",sep="\\")


getwd()
setwd(my.path)
getwd()
list.files()

save(train, file="train.RData", compress = FALSE)
save(validate, file="validate.RData", compress = FALSE)
save(test, file="test.RData", compress = FALSE)

write.csv(train, paste(my.path,'train.csv',sep='\\'),row.names = FALSE)
write.csv(validate, paste(my.path,'validate.csv',sep='\\'),row.names = FALSE)
write.csv(test, paste(my.path,'test.csv',sep='\\'),row.names = FALSE)





