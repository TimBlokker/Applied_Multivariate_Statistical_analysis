#Load libraries
library(readxl)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(dendextend)
library(cluster)
library(rpart)
library(rpart.plot)

#load data
erasmus<-read.csv(file="Assignment/SM_2012_13_20141103_01.csv", sep=";")
globe <- read_excel("Assignment/GLOBE-Phase-2-Aggregated-Societal-Culture-Data.xls", 
                    sheet = "GLOBE Phase 2 Society Level Soc")

#Pre-processing
names(erasmus)
erasmus2<-erasmus[,c(2,4,13)]
names(erasmus2)

#Remove missing entries from Erasmus dataset and assign completers to erasmus3
erasmus2[which(erasmus2[,1]=='???'),1]<-NA 
erasmus2[which(erasmus2[,2]=='???'),2]<-NA 
erasmus2[which(erasmus2[,3]=='???'),3]<-NA 
erasmus3=na.omit(erasmus2)
head(erasmus3)

#Filter for data on european countries
globe2<-globe%>%
  filter(globe$`Country Cluster`%in%c("Anglo","Eastern Europe", "Nordic Europe", "Germanic Europe", "Latin Europe" ))

#make the variable a factor
globe2$`Country Name`<-(as.factor(globe2$`Country Name`))

#Combine East and West Germany into Germany
levels(globe2$`Country Name`)[11:12]<-"Germany"
lev_g<-sort(levels(as.factor(globe2$`Country Name`)))

#Remove countries from Erasmus data set that were not present in the globe data set
lev_e<-levels(erasmus3$HOME_INSTITUTION_CTRY_CDE)[c(-2,-3,-4, -5,-7,-8, -11,-16,-19,-21,-22,-23,-24,-25,-27,-30,-33,-34)]

#Sort the levels according to the levels in erasmus dataset to be able to map the country names
lev_g2<-lev_g[c(3,28,11,5,26,7,8,12,13,14,16,18,20,21,27,23,6)]

#rename country names in globe to the short hand notation from the erasmus data set
globe2$`Country Name` <- mapvalues(globe2$`Country Name`, from=lev_g2, to=lev_e)
globe2=data.frame(globe2)

#drop levels that are empty
droplevels(erasmus3$HOST_INSTITUTION_COUNTRY_CDE)
droplevels(erasmus3$HOME_INSTITUTION_CTRY_CDE)

#filter erasmus dataset for countries that also exist in globe dataset
erasmus3<-erasmus3%>%
  filter(HOME_INSTITUTION_CTRY_CDE%in%lev_e)%>%
  filter(HOST_INSTITUTION_COUNTRY_CDE%in%lev_e)

#west and east germany are same students, remove country 94-> east germany
globe3<-globe2%>%
  filter(!(Country==94))
#merge the globe data set and the erasmus dataset by matching Home and Host country
x<-merge(globe3, erasmus3, by.x="Country.Name", by.y="HOME_INSTITUTION_CTRY_CDE")
z<-merge(globe3, erasmus3, by.x="Country.Name", by.y="HOST_INSTITUTION_COUNTRY_CDE")

#sort dataframes for student id to calculate cultural differnce between host and home country
x<-x%>%
  arrange(ID_MOBILITY_CDE)
z<-z%>%
  arrange(ID_MOBILITY_CDE)

#Cultural Differences in Globe data for Home and Host Country (Home - Host)
y<-x[,3:20]-z[,3:20]
y<-cbind(x$HOST_INSTITUTION_COUNTRY_CDE, z$HOME_INSTITUTION_CTRY_CDE,y)
names(y)[c(1,2)]<-c("Host_Country", "Home_country")

#calculate the distance between home and host country
y$FromTo<-paste(y$Home_country, "_", y$Host_Country )
y2<-y[,c(21,3:20)]
y3<-aggregate(y2[,2:19], y2[1], mean)  #mean is the actual value since all entries with same from_to are the same for all values

#The aggregation reduced the dataset to 270 rows so 270 different FromTo values, to be able to weigh the importance of 
#the different FromTo values we count the number of rows that were aggregated into one FromTo row
obs_weights<-y2%>%
  group_by(FromTo)%>%
  count()%>%
  arrange(FromTo)
frequencies=obs_weights[,20]

#change data type to numeric
y4<-y3%<>% mutate_if(is.factor,as.numeric)
xm<-apply(y4[,2:19],2,mean)
y5<-sweep(y4[2:19],2,xm)#center the data, I do not normalize for var=1 because the values used are on the same scale 

# Generate Andrews plot for the cultural difference data
par(mfrow=c(1,1))
t <-seq(-pi,pi,length=500)
plot(c(-pi,pi),c(-6,6),type="n",xlab=var,ylab="Value",main="Andrews Plot")
for (k in (1:270)){
  crseq <- (y5[k,1]/sqrt(2))+y5[k,2]*sin(t) + y5[k,3]*cos(t)+y5[k,4]*cos(2*t)
  points(t,crseq,type="l")
}
title("Andrews Plot")

#start with a cluster analysis since we suspect two clusters, we include the frequencies of the FromTo values as weights
#this gives information on which cultural values are attractive for certain countries to go to on erasmus
par(mfrow=c(1,1))
d=dist(y5, method='euclidean')
culDifclust <- hclust(d, method="average", members=frequencies)  #AVARAGE LINK and using the frequencies of the observations as weights

# Rectangle dendrogram using ggplot2
dend=as.dendrogram(culDifclust)
dend %>% set("labels_col", value = c("green", "blue", "red"), k=3) %>% 
  plot(main = "Weighted Cluster Dendrogram", ylab="Average Link Distance", xlab="Cultural Differences", )
abline(h = 3.2, lty = 2)

#we select 3 clusters and observe 2 outliers
culDif.gp <- cutree(culDifclust,k=3)
table(culDif.gp)#outliers? in group 3

#Assign cluster to FromTo cultural difference values
clusterd<-(cbind(culDif.gp,y5))

#Use rpart function with built in cross-validation and selection of best cp value
rpart=rpart(as.factor(clusterd$culDif.gp)~., data=clusterd)
print(rpart, digits=2, minlength=10, spaces=4)
printcp(rpart)
par(mfrow=c(1,2))
prp(rpart, type = 4, extra = 101, fallen.leaves = T,round = 2, space = -1, varlen = 5)
plotcp(rpart)

##Create correlation matrix from centered data matrix
xc=as.matrix(y5)
ss=t(xc)%*%xc
d=diag(ss)^(-1/2)
e=diag(d)
xn=xc%*%e
var(xn*sqrt(nrow(xc)-1))#xn unit variance if multiplied by sqrt of n-1
sqrt(diag(t(xn)%*%xn))#unit length
R=t(xn)%*%xn

#Factor analysis
pca.corr2=svd(R)
cerB=pca.corr2$v%*%diag((sqrt(pca.corr2$d)))#Factor loadings
cerB_red<-cerB#Reduced factor loadings
cer.psi=R-cerB_red%*%t(cerB_red)

#RMS calculations
cer.res <- (cer.psi - diag(cer.psi))^2 #making the diagonal 0 which are the specific variances
cer.RMS.overall <- sqrt(sum(cer.res)/length(cer.res))
cer.RMS.overall

#BiPlot
svd.xn<-svd(xn)
R.B<-as.matrix(xn)%*%svd.xn$v%*%(diag((svd.xn$d)^(-1))) #scores matrix #U is the  matrix of standardized PC scores! 
C.B<-svd.xn$v%*%diag(svd.xn$d)

#Create actual plot
par(mar=c(0.5,0.5,0.5,0.5),pty='s',oma=c(3.5,0,0,0),font=2)
par(mfrow=c(1,1))
plot(R.B[ ,1],R.B[ ,2], col=clusterd$culDif.gp ,axes=F,xlim=c(-1,1),ylim=c(-1,1),xlab=' ',ylab=' ',cex=1.2)#observations=factor scores=scaled principal component scores
legend("bottomright",bty='n',legend=levels(as.factor(clusterd$culDif.gp)), pch=1,horiz=T, xpd=T,cex=1.2,col=unique(clusterd$culDif.gp))

mtext('First component',side=1,line=3,cex=1)
mtext('Second component',side=2,line=3,cex=1)
axis(1,at=c(-1,-.8,-.6,-.4,-.2,0,.2,.4,.6,.8,1),cex=1.2)
axis(2,at=c(-1,-.8,-.6,-.4,-.2,0,.2,.4,.6,.8,1),cex=1.2)
box( )

points(C.B[,1],C.B[,2],pch=".")#plotting the variables = factor loadings
x<-abbreviate(colnames(y5))
text(C.B[,1]-.05,C.B[,2]+.05,x,cex=1.0)
for (i in seq(1,nrow(C.B),by=1)){
  arrows(0,0,C.B[i,1],C.B[i,2])
}
#Draw circle unit
library(plotrix)
draw.circle(0,0,1,border='black')

#Split the FromTO values again into Host and Home
clusterd2<-cbind(culDif.gp,y4[,1],y5, frequencies)
xxxx=strsplit(as.character(clusterd2[,2]),'_')
x=seq(1,540,2)
clusterd2$Home=unlist(xxxx)[x]
clusterd2$Host=unlist(xxxx)[x+1]
names(clusterd2)
names(clusterd2)[21]<-"Frequency"
clusterd3<-aggregate(clusterd2[,21], clusterd2[,c(22,1)], sum)

#Calculate how many exchanges from each home country fall into cluster 1,2 or 3 in % 
for(i in 1:length(clusterd3$Home)){
  indexes=which(clusterd3$Home==clusterd3$Home[i])
  if(length(indexes)>1){
    clusterd3$percentage[indexes[1]]=clusterd3$x[indexes[1]]/sum(clusterd3$x[indexes[1]],clusterd3$x[indexes[2]])
    clusterd3$percentage[indexes[2]]=clusterd3$x[indexes[2]]/sum(clusterd3$x[indexes[1]],clusterd3$x[indexes[2]])
  }else
    clusterd3$percentage[indexes[1]]=1
}

#sort by country
clusterd4=data.frame(clusterd3[order(clusterd3$Home, decreasing = T),])
head(clusterd4)

#calculate prior probabilities
clusterd5=clusterd4[,c(2,3)]
detach(package:plyr)#plyr interferes with dplyr functionality elsewise
clusterd6=clusterd5%>%
  group_by(culDif.gp)%>%
  summarise(sum=sum(x))%>%
  mutate(percentage=sum/sum(sum))

#plot bar diagram
a<-ggplot(clusterd4, aes(x=Home,y=percentage,fill=factor(culDif.gp)))
b=a+theme_classic()+   scale_fill_hue(l=20)
b+geom_col()+ geom_hline(yintercept=clusterd6$percentage[2], linetype="dashed", 
                         color = "steelblue", size=1)

library(MASS)
x=lda(clusterd$culDif.gp~., data =clusterd)
plot(x)
