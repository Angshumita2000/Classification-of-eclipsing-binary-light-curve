Iterative_fun=function(initial,i)
{
	##G_0=NULL
	
l=length(DAT[[i]]$V2)
e_i=rnorm(l,0,0.001)
x=c(LOL[[i]],LOL1[[i]])
y=c(DAT[[i]]$V2,DAT[[i]]$V2+e_i)
z=initial[1]+initial[2]*sin(x*initial[3])

	
	d_0=matrix(rep(0,2*1),2*l,3)

		for (j in 1:length(x))
		 {
			c_1=sum(y[j]-initial[1]-initial[2]*sin(initial[3]*x[j]))
			c_2=sum(sin(initial[3]*x[j])*(y[j]-initial[1]-G_0[2]*sin(initial[3]*x[j])))
			c_3=sum(initial[2]*x[j]*cos(initial[3]*x[j])*(y[j]-initial[1]-initial[2]*sin(initial[3]*x[j])))
  			d_0[j,1] <-c_1
			d_0[j,2]<-c_2
   			 d_0[j,3]<-c_3
   # Assigning values incrementally
y_0=y[j]-z[j]
			}
Y_0=matrix(y_0,2*l,1)


model=lm(Y_0~d_0-1)
B_0=coef(model)

return(B_0)
	
}

results_matrix <- matrix(NA, ncol = 5, nrow = 1318)
G_0=c(1,2,3)

for( i in 1:659)
{
  	tolerance <- 1e-6
	max_iterations <- 100
	g_current <- G_0
	converged <- FALSE
 	iterations <- 0
		while (!converged && iterations < max_iterations) 
			{
    				# Calculate b for current g
    				b <- Iterative_fun(g_current,i)
	 			g_next <- g_current + b
					if (max(abs(g_next - g_current) < tolerance))
						 {
      						converged <- TRUE
    							} 
						else {
      						# Update current g for next iteration
      							g_current <- g_next
    							}
    
  				  # Increment iteration counter
    					iterations <- iterations + 1
  			}
		results_matrix[i, ] <- c(i, g_current, iterations)

}

for( i in 660:1318)
{
  	tolerance <- 1e-6
	max_iterations <- 100
	g_current <- G_0
	converged <- FALSE
 	iterations <- 0
		while (!converged && iterations < max_iterations) 
			{
    				# Calculate b for current g
    				b <- Iterative_fun(g_current,i)
	 			g_next <- g_current + b
					if (max(abs(g_next - g_current) < tolerance))
						 {
      						converged <- TRUE
    							} 
						else {
      						# Update current g for next iteration
      							g_current <- g_next
    							}
    
  				  # Increment iteration counter
    					iterations <- iterations + 1
  			}
		results_matrix[i, ] <- c(i, g_current, iterations)

}
#Distance Matrix
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

