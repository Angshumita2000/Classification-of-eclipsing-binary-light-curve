my_coeff<-read.csv(file="my.matrix.csv")

#.csv file for the least square estimates

results_matrix<-as.matrix(my_coeff)
dim(results_matrix)
a_i=results_matrix[i=1:1318,2]
b_i=results_matrix[i=1:1318,3]
g_i=results_matrix[i=1:1318,4]

# D matrix construction
d=matrix(0,nrow=1318,ncol=1318)
for(i in  1:1317)
{	
	 for (j in (i+1):1318)
		{ 
			
	
			phi=function(x)
				{
				((a_i[i]-a_i[j])+b_i[i]*sin(g_i[i]*x)-b_i[j]*sin(g_i[j]*x))^2
				}
		d[i,j]=integrate(phi,0,2, subdivisions = 1000)$val
		}


}
for(i in  2:1318)
{	
	 for (j in 1:i-1)
		{ 
			
	d[i,j]=d[j,i]
			
		}


}



library(dbscan)
dbscan(d,2000)
dbclust<-dbscan(d,5700000,minPts=50)
d[1,2]-d[1,6]
plot(dbclust$cluster)
res <- optics(d, eps = 5700000,minPts=50)
plot(res)
plot(d, col = "grey")
 polygon(d[res$order,], )
res2 <- extractDBSCAN(res, eps_cl =237774244674)
plot(res2)
library(stats)

 dend <- as.dendrogram(res2)
dend