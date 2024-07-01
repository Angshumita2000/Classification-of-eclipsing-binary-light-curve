# Reading the file called table2.DAT
data<-read.table("C:\\Users\\ANGSHUMITA\\Desktop\\project work\\J_A+A_519_A12.tar\\J_A+A_519_A12\\table2.dat")
dim(data)
head(data,5)#renaming the column in file 1
# naming the variable
ID<-data$V1
chip<-data$V2
RAh<-data$V3
RAm<-data$V4
RAs<-data$V5
DEd<-data$V6
DEm<-data$V7
DEs<-data$V8
Rcmag<-data$V9
B_RC<-data$V10
RC_IC<-data$V11
Period<-data$V12
type<-data$V13
Filename<-data$V14
# making a dataframe
df2<-data.frame(ID,chip,RAh,RAm,RAs,DEd,DEm,DEs,Rcmag,B_RC,RC_IC,Period,type,Filename)
View(df2)#making a data frame with the working variable


####
add <- paste0("C:\\Users\\ANGSHUMITA\\Desktop\\phot\\", df2$Filename) 
#pasting the file info into the last column

length(add)
View(df2)
####
# Data processing

DAT <- list(NULL)
for(i in 1:length(add)){
	DAT[[i]] <- read.table(add[i])
	print(paste("Successful for", i))   #seeing the missing values and filling tit with null values
}
# replacing the missing info in a particular column with "0000" to read the data
#vizualizing the phot folder

 for(i in 1:length(add)){

HJD<-DAT[[i]][,1]
Rel_flux_var_R_band<-DAT[[i]][,2]
Uncertainity_relfluxvar<-DAT[[i]][,3]
df3<-data.frame(HJD,Rel_flux_var_R_band,Uncertainity_relfluxvar)
print(df3)
}

#step (i)
LOL <- list(NULL)
for(i in 1:length(add)){
	LOL[[i]] <- ((DAT[[i]][,1]-floor(DAT[[i]][, 1]))/df2$Period[i])%%1# t-t_0/p_i
}


#step (ii)
LOL1 <- list(NULL)
for(i in 1:length(add)){
	LOL1[[i]] <- (LOL[[i]]+1)# extending p=[0,1]->p=[0,2)
}


#step (iii)

gol <- function(p,i)
	{
	
	
	p_order=c(LOL[[i]],LOL1[[i]])
	j <- max(unlist(lapply(p_order, function(x) which(x < p))))

	pj= p_order[j]
	pj1=p_order[j+1]
	aj <- (pj1 - p) / (pj1 - pj)
	bj <- 1-aj
	y=c(DAT[[i]]$V2,DAT[[i]]$V2)
	yj=y[j]
	yj1=y[j+1]

	return(aj*yj + bj*yj1)
	
	}
p_new1<-NULL

for(i in 1:1318){
	p_new1[i]=min(LOL[[i]])

	}

 max(p_new1)+0.0001

p_dash=seq(max(p_new1)+0.0001,1,length=272)
p_new <- NULL
for(i in 1:1318)
{
p_new[[i]]<-gol(p_dash,i)
}


p_data=data.frame(p_new)


distances <- dist(p_data)
hclust(distances, method = "complete")
plot(hclust(distances, method = "complete"))


k <- 3  # Number of clusters
kmedoids_result <- pam(distances,  k=k)
# Output of k-means clustering
print(kmedoids_result)

library(cluster)
sil_width <- silhouette(kmedoids_result$cluster, distances)
# Create silhouette plot
plot(sil_width, col = rainbow(3), main="Silhouette Plot for K=3 ",border = NA)

k <- 2  # Number of clusters
kmedoids_result2 <- pam(distances,  k=2)
# Output of k-means clustering
print(kmedoids_result2)


sil_width2 <- silhouette(kmedoids_result2$cluster, distances)
# Create silhouette plot
plot(sil_width2, col = rainbow(2),main="Silhouette Plot for K=2 ", border = NA)


k <- 4  # Number of clusters
kmeans_result4 <- kmeans(distances, centers = k)
print(kmeans_result4)
sil_width4 <- silhouette(kmeans_result4$cluster, distances)
# Create silhouette plot
plot(sil_width4, col = rainbow(4), main="Silhouette Plot for K=4 ",border = NA)



k_values <- 2:15  # Example range from 2 to 15 clusters


avg_sil_widths <- numeric(length(k_values))


for (k in k_values) {
  # Cluster the data using K-means algorithm
  km <- pam(distances, k=k)
  
  
  sil_width <- silhouette(km$cluster, distances)
  
  # Compute the average silhouette width for this K
  avg_sil_widths[k - min(k_values) + 1] <- mean(sil_width[, "sil_width"])
}

plot(k_values, avg_sil_widths, type = "b", xlab = "Number of Clusters (K)", ylab = "Average Silhouette Width")

points(k_values, avg_sil_widths, col = "red", pch = 19)

