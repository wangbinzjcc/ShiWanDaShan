######
setwd('F:\\DataW\\shiwan-data')
dir()
ShiWdata <- read.csv("shiwanData2013-5-15.csv")
summary(ShiWdata)
uniq.dat <- with(ShiWdata, tapply(鉴定名,样方号,unique))

uniq.dat00 <- unique(unlist(uniq.dat))
c00 <- unlist(uniq.dat)
c01 <- uniq.dat00
names(c01) <- 1:length(c01)
c02 <- c(as.character(c00), as.character(c01))
names(c02) <- c(names(c00), 1:length(c01))
write.csv(c02, 'uniq.dat00.csv')
##############