erasmus<-read.csv(file="Assignment/SM_2012_13_20141103_01.csv", sep=";")
library(readxl)
library(dplyr)
globe <- read_excel("Assignment/GLOBE-Phase-2-Aggregated-Societal-Culture-Data.xls", 
                    sheet = "GLOBE Phase 2 Society Level Soc")
names(erasmus)
erasmus2<-erasmus[,c(2,4,13)]
erasmus2[which(erasmus2[,1]=='???'),1]<-NA #these are mainly enterprise placements
erasmus2[which(erasmus2[,2]=='???'),2]<-NA #these are mainly enterprise placements
erasmus2[which(erasmus2[,3]=='???'),3]<-NA #these are mainly enterprise placements

erasmus3=na.omit(erasmus2)
head(erasmus3)
globe2<-globe%>%
  filter(globe$`Country Cluster`%in%c("Anglo","Eastern Europe", "Nordic Europe", "Germanic Europe", "Latin Europe" ))

globe2$`Country Name`<-(as.factor(globe2$`Country Name`))
levels(globe2$`Country Name`)[11:12]<-"Germany"
lev_g<-sort(levels(as.factor(globe2$`Country Name`)))
lev_e<-levels(erasmus3$HOME_INSTITUTION_CTRY_CDE)[c(-2,-3,-4, -5,-7,-8, -11,-16,-19,-21,-22,-23,-24,-25,-27,-30,-33,-34)]
lev_g2<-lev_g[c(3,28,11,5,26,7,8,12,13,14,16,18,20,21,27,23,6)]
library(plyr)
globe2$`Country Name` <- mapvalues(globe2$`Country Name`, from=lev_g2, to=lev_e)
globe2=data.frame(globe2)
droplevels(erasmus3$HOST_INSTITUTION_COUNTRY_CDE)
droplevels(erasmus3$HOME_INSTITUTION_CTRY_CDE)
erasmus3<-erasmus3%>%
  filter(HOME_INSTITUTION_CTRY_CDE%in%lev_e)%>%
  filter(HOST_INSTITUTION_COUNTRY_CDE%in%lev_e)
#west and east gremany are same students, remove country 94-> east germany
globe3<-globe2%>%
  filter(!(Country==94))
x<-merge(globe3, erasmus3, by.x="Country.Name", by.y="HOME_INSTITUTION_CTRY_CDE")
z<-merge(globe3, erasmus3, by.x="Country.Name", by.y="HOST_INSTITUTION_COUNTRY_CDE")

#sort dataframes for student id
x<-x%>%
  arrange(ID_MOBILITY_CDE)
z<-z%>%
  arrange(ID_MOBILITY_CDE)
#Differences in Globe data for Home and Host Country (Home - Host)
y<-x[,3:20]-z[,3:20]
y<-cbind(x$HOST_INSTITUTION_COUNTRY_CDE, z$HOME_INSTITUTION_CTRY_CDE,y)
names(y)[c(1,2)]<-c("Host_Country", "Home_country")

#calculate the mean distance between home and host country
y$FromTo<-paste(y$Home_country, "_", y$Host_Country )
y2<-y[,c(21,3:20)]
y3<-aggregate(y2[,2:19], y2[1], mean)  #mean is the actual value since all entries with same from_to are the same for all values
obs_weights<-y2%>%
  group_by(FromTo)%>%
  count()%>%
  arrange(FromTo)
obs_weights

library(dplyr)
library(magrittr)
y4<-y3%<>% mutate_if(is.factor,as.numeric)
xm<-apply(y4[,2:19],2,mean)
y5<-sweep(y4[2:19],2,xm)#center the data, I do not normalize for var=1 because the values used are already normalized


stars(y5)

# Because the variables are normalized we know their ranges.
# Set up the plot axes and lables.
plot(c(.5,18),c(-2,2),type="n",xlab=var,ylab="Value",main="Profile Plot - Rock Data")
# Use a loop to generate profiles for each observation.
for (k in (1:270)){
  points(1:18,y5[k,],type="l")
}

# Generate Andrews plot for the rock data.
t <-seq(-pi,pi,length=500)
plot(c(-pi,pi),c(-6,6),type="n",xlab=var,ylab="Value",main="Andrews Plot - Rock Data")
for (k in (1:270)){
  crseq <- (y5[k,1]/sqrt(2))+y5[k,2]*sin(t) + y5[k,3]*cos(t)+y5[k,4]*cos(2*t)
  points(t,crseq,type="l")
}

#start with a cluster analysis
par(mfrow=c(1,1))
crab.clus <- hclust(dist(y5), method="average", members=obs_weights$n)  # AVARAGE LINK and using the frequencies of the observations as weights
plot(crab.clus,xlab="Crab Data",ylab="Average Link Distance", sub="")

#we select 3 clusters and observe 2 outliers
crab.gp <- cutree(crab.clus,k=3)
table(crab.gp)#outliers in group 3
library(cluster)
clusplot(y5,crab.gp,stand=TRUE,labels=3,main="Rock-single link")

clusterd<-(cbind(crab.gp,y5))
clusterd2<-(cbind(crab.gp,y4[,1],y5, obs_weights$freq))#interprete clusters as profile of cultural differences that different groups of students prefer, lets look what this groups could be
names(clusterd)
library(tree)
tree<-tree(as.factor(clusterd$crab.gp)~., data=clusterd)
plot(tree)                  # plot tree in variable scale
text(tree,srt=0,adj=1)              # puts names on tree plot
summary(tree)
#plot Deviance versus size by cross-validation - Tree g1.ind
tree.cv<-cv.tree(tree, FUN=prune.tree)
?cv.tree
plot(tree.cv,type="b")

# K means cluster method.
# Compute the kmeans cluster object for two clusters and 20 iterations
rock.kcl <- kmeans(y5, 3, 20) #THE CLUSTERING IS NOT VERY GOOD BSS/TOTALSS BUT WE HAVE ONLY 3 CLUSTERS
names(rock.kcl)
# The cluster id information is directly in the rock.kcl cluster
# object so there is no need for cutree function.
# Perform the pairwise plots with color coded cluster id included.


##Try PCA to reduce the dimensions and then du cluster analysis
xc=as.matrix(y5)
ss=t(xc)%*%xc
d=diag(ss)^(-1/2)
e=diag(d)
xn=xc%*%e
var(xn)*sqrt(nrow(xc)-1)
R=t(xn)%*%xn

#we need cov matrix for PCA
s=ss/(dim(y5)[1]-1)
pca.y5=svd(s)
pca.scores<-xc%*%pca.y5$u
plot(pca.scores)#pca scores
pca.y5$u#loadings


rock.pkcl <- kmeans(pca.scores,3,20)
par(mfrow=c(1,1))
plot(pca.scores[,1:3], col = rock.pkcl$cluster)
points(rock.pkcl$centers[,c(1,2)], col = 1:3, pch ="+", cex=2)
title ("K Means Cluster on first two PCs - Rock Data")
clusplot(y5,rock.pkcl$cluster,stand=TRUE,labels=2,main="Rock-kmeans, 2 clusters")

