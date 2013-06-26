##################################################
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
######
tabl.dat <- with(ShiWdata, tapply(鉴定名,样方号,table))
lapply(tabl.dat,function(xx){sort(xx,decreasing=T)[1:10]})
##############

head(ShiWdata)
AG00 <- aggregate(序号 ~ 鉴定名 + 样方号 , FUN=length, data=ShiWdata)
head(AG00)
RE00 <- reshape(data=AG00, v.names='序号', timevar='鉴定名', 
                idvar='样方号', direction='wide' )
RE00[is.na(RE00)] <- 0
RE00 
RE01 <- RE00[,-1]
#################################
## Raw data and plotting
require(vegan)
m <- betadiver(RE01)
plot(m)

## The indices
betadiver(help=TRUE)
## The basic Whittaker index

d4 <- betadiver(RE01, "r")
d5 <- betadiver(RE01, "I")
d6 <- betadiver(RE01, "e")
d7 <- betadiver(RE01, "t") 
mode(d4)
unclass(d4)
as.matrix(d4)
#
#
write.csv(as.matrix(d4),'d4.csv')
write.csv(as.matrix(d5),'d5.csv')
write.csv(as.matrix(d6),'d6.csv')
write.csv(as.matrix(d7),'d7.csv')
#
##################################
#
plot(R00 <- rowSums(RE01), ylim=c(0,max(R00)*1.2))
plot(A00 <- rowSums(RE01>0), ylim=c(0,max(A00)*1.2))
require(vegan)
H <- diversity(RE01)
simp <- diversity(RE01, "simpson")
S <- A00 ## rowSums(BCI > 0) does the same...
J <- H/log(S)
#
c("lightblue", "mistyrose", "lightcyan")

Alt00 <- c(283,390,500,700,900,1000)
#
tiff('ShiwandashanDiversity.tiff',
     width = 3200, height = 2800,res=600,compression = "lzw")
#
op00 <- par(mar=c(5.5,5.5,3,8), mex=0.43)
plot(H,type='o', pch=21, bg='lightblue' 
     , cex=1.5, ylim=c(0,max(H)*1.2), axes=F, xlab='海拔（m）',
     ylab='Shannon多样性指数')
box()
legend(3.5, 2,
       c('Shannon多样性指数', 'Simpson多样性指数', 'Pielou均匀度指数'),
       bty='n', pch = c(21, 24, 15), col=1,
       pt.bg=c('lightblue', 'mistyrose', 'lightcyan'),
       ncol=1, pt.cex =c(1.2, 1.2, 1.2), cex=0.8)
axis(1, at=1:6, labels=Alt00 , tck=0.02)
axis(2,tck=0.02)
par(op00)
####
op1 <- par(new=T, mex=0.43, mar=c(5.5,5.5,4,8))
plot(simp,type='o',pch=24, bg='mistyrose'  
     ,  cex=1.3,  ylim=c(0,max(simp)*1.2), axes=F, xlab='', ylab='')
axis(4,tck=0.02)
mtext('Simpson多样性指数', side=4, line=2.8, cex.lab=1)
par(op1)
###
op2 <- par(new=T, mex=0.43, mar=c(5.5,5.5,4,8))
points(J,type='o', pch=15, bg="lightcyan" 
       ,  cex=1.5, xlab='', ylab='')
mtext('Pielou均匀度指数', side=4, line=5, cex.lab=1)
par(op2)
#
dev.off()
#
#
################################################
dir()
R00 <- read.csv('uniq.dat.csv')
S00 <- read.csv('ShiWanSpeciesData00.csv')
head(R00)
head(S00)
i00 <- c(2,4,6,8,10,12)

#
L00 <- sapply(i00, function(i){
  M00 <- match(R00[,i], S00$种名)
  length(unique(S00$科名[na.omit(M00)]))
}  )
#
L01 <- sapply(i00, function(i){
  M00 <- match(R00[,i], S00$种名)
  length(unique(S00$属名[na.omit(M00)]))
                    }  )

#
L02 <- sapply(i00, function(i){sum(unique(na.omit(R00[,i])) != '')})
#
dat00 <- data.frame(fam=L00, gen=L01, spe=L02)
dat00 <- t(dat00)
dat00
Altitude00 <- c(283,390,500,700,900,1000)
#############
 
tiff('ShiwanshanFamGenSpe.tiff', width = 3100, height = 2900,res=600,compression = "lzw")
par(mex=0.45,mar=c(6,6,3,0.5))
barplot(dat00, beside = TRUE,
        col = # 1#
        c("lightblue", "mistyrose", "lightcyan")
        , #angle = 00+45*0:2, density = 20,        
       
        legend = c('科数','属数','种数'), ylim = c(0, 100))
text(c(1,5,9,13,17,21)+0.5, L00+3,L00, col = 1,cex=0.8)
text(c(1,5,9,13,17,21)+1.5, L01+3,L01,  col = 1,cex=0.8)
text(c(1,5,9,13,17,21)+2.5, L02+3,L02,  col = 1,cex=0.8)
box()
#####
title(xlab='海拔（m）', ylab='数量（个）')
axis(side=1,at=c(1,5,9,13,17,21)+1.5,tick=F,line =-0.5,tck=0,Altitude00)
dev.off()
#

#######################################################

#
setwd('F:\\DataW\\shiwan-data')
dir()
ShiWdata <- read.csv("shiwanData2013-5-15.csv")
ShiWdata <- subset(ShiWdata,胸径.cm.>=2 & 
                     鉴定名 != '00枯立木' & 鉴定名 != '??' &
                   鉴定名 != '00藤本')
summary(ShiWdata)
###
tabl.dat <- with(ShiWdata, tapply(鉴定名,样方号,table))
lapply(tabl.dat,function(xx){sort(xx,decreasing=T)[1:20]})
###### 
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre7")
require(rJava)
require(XLConnect)
#
setwd('F:\\其它的\\北部湾样方调查资料')
d00 <- dir(pattern = '.xlsx')
d00[1]
d00 <- d00[-1]
df.one <-lapply(d00, function(xx){readWorksheetFromFile(xx,sheet=1)})
summary(df.one)
str(df.one)
df.one[[1]]
########
lapply(df.one,function(xx){sort(table(xx$sp),decreasing=T)[1:20]})

###
??betadiver

plot(m)
## The indices
betadiver(help=TRUE)
## The basic Whittaker index
d <- betadiver(sipoo, "w")
## This should be equal to Sorensen index (binary Bray-Curtis in
## vegan)
range(d - vegdist(sipoo, binary=TRUE))

###########






#












