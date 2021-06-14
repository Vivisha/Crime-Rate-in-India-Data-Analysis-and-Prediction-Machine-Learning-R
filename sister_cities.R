#These are the libraries used.
library(cluster)
library(RColorBrewer)

#Reading the dataset
crimes=read.csv('~/R/01_District_wise_crimes_committed_IPC_2001_2012.csv')

#Cleaning the data
crimes <- na.omit(crimes) 

#Preprocessing the data
	#Selection of relevant attributes for the model	
crimes<-crimes[,c(2,3,4,5,7,10,13,15,16,17,20,24,26,27,29)]
	#Elimination of the rows with totals for every district
crimes<-subset(crimes,(crimes$DISTRICT!="TOTAL" & crimes$DISTRICT!="DELHI UT TOTAL"))

#Getting the names of all the districts
districts<-crimes[1]
districts<-unique(districts)

#Creating a dataset by compressing the data for different years for a particular district by averaging it. 
x<-data.frame()
for (i in districts[[1]])
{
	l<-c()
	newdata <- crimes[ which(crimes[1]==i), ]
	l<-append(l,as.character(newdata[1,1]))
	l<-append(l,newdata[1,2])
	for (j in c(3:15))
	{
		mean<-mean(newdata[,j])
		l<-append(l,mean)
	}
	x<-rbind(x,l,stringsAsFactors=FALSE)	
}
	#Naming the columns of the dataset using the column names of the original dataset.
colnames(x)<-colnames(crimes)
x<-x[,-c(2)]

#Using a clustering model CLARA to cluster the district to obatin the sister cities based on crime rates along different crime parameters.
	#CLARA is a clustering algorithm for large datasets.
	#Removing the district column while fitting the model.
	#The number of clusters was decided by taking into account the number of districts and the variance in the dissimilarity.
fit<-clara(x[,-c(1)],k=10)   

#Getting the clustered points
cluster_points<-fit$cluster

#Generating a list of which each element is a cluster(only names of the district)
#Creating a vector of cluster means corresponding to that of the list mentioned above.
a<-c(1:10)
sister_cities<-list()
cluster_means<-c()
for(i in a)
{
	means<-c()
	cluster<-x[cluster_points==i,]
	for (j in c(2:10))
	{
		means<-append(means,mean(as.numeric(cluster[,j])))
	}
	cluster_means<-append(cluster_means,mean(means))

	temp<-cluster[[1]]
	temp<-list(temp)
	sister_cities<-append(sister_cities,temp)
}

#Bubble sort function is used for the purpose ordering the values for visualization. 
example <- function(x,sister_cities)
{
  n <- length(x) 
  for (k in n:2) 
  {
    i <- 1       
    while (i < k)
	{
      if (x[i] > x[i+1]) 
      {
	temp1<-list()
        temp <- x[i+1]
        x[i+1] <- x[i]
        x[i] <- temp
	temp1<-sister_cities[i+1]
	sister_cities[i+1]<-sister_cities[i]
	sister_cities[i]<-temp1
      }
      i <- i+1           
   }
 }
	return(list(x,sister_cities))

}

#Call to the sort function.
sorted<-example(cluster_means,sister_cities)
cluster_means<-sorted[[1]]
sister_cities<-sorted[[2]]

#Visualization
	#colorRampPalette is a function that returns a palette generating function.
rbPal <- colorRampPalette(c('blue','red'))
	#Instance of colorRampPalette i.ie, rbPal takes an integer as a parameter to return a palette of those many different colors.
color <- rbPal(10)[as.numeric(cut(cluster_points,breaks = 10))]
	#Plotting the clustered points based on the different clusters they belong to on the y-axis.
	#The X-axis indicates the distance of each point in a particular cluster from its cluster mean.
	#The different colors indicate the increase in crime rate when color changes from blue to red.
set.seed(1)
plot(cluster_points, nmax.lab = 40, max.strlen = 5,
     main = NULL, sub = NULL, xlab = expression("Distance from the cluster means"),ylab=expression("Clusters"),
     col = color,  do.col.sort = length(col) > 1, border = 0,cex=0.8,
     cex.names = par("cex.axis"), do.n.k = TRUE, do.clus.stat = TRUE)
title("Sister Cities vs crime rate vs variance of clustering")
print(sister_cities)

