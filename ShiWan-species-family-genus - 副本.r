########################################################
#  自动查找 科属~~
#######
setwd('F:\\DataW\\shiwan-data')
spedat <- readLines('ShiWanDaShan-AllSpecies.txt')

dat00 <- read.csv('SpeDat00.csv')
dat00$种名 <- gsub(' ','',dat00$种名)
###

la00 <- sapply(dat00$种名, 
       function(xx){grep(paste('^',xx,' ',sep=''), spedat)}
               )
la01 <- as.numeric(la00)

###
dat00$species <- gsub('\\w+ +([A-Z][a-z]+ +[a-z]+) +.*',
                             "\\1", spedat[la01])

### 
gen00 <- grep('属 +[A-Z]', spedat)
cu00 <- cut(la01, gen00)
gs00 <- gsub('\\(([0-9]+),[0-9]+]', '\\1', cu00)
dat00$属名 <- gsub('(\\w+) +[A-Z].*',
                           '\\1', spedat[as.numeric(gs00)])
dat00$genus <- gsub('\\w+ +([A-Z][a-z]+) .*',
                 '\\1', spedat[as.numeric(gs00)])

### 
fam00 <- grep('科 +[A-Z]',spedat)
cu01 <- cut(la01, fam00)
gs01 <- gsub('\\(([0-9]+),[0-9]+]', '\\1', cu01)
fam01 <- gsub('.* (\\w+ +[A-Z][a-z]+) ','\\1',
              spedat[as.numeric(gs01)])
dat00$科名 <- gsub('(\\w+) +[A-Z].+','\\1', fam01)
dat00$family <- gsub('\\w+ +([A-Z].+)','\\1', fam01)
#
write.csv(dat00, 'ShiWanSpeciesData00.csv')
#
##########################################################