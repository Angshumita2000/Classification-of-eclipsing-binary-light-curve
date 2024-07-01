
#For any random lightcurve we onserve the fitting
e_i=rnorm(261,mean=0,sd=0.01)
y=c(DAT[[613]]$V2,DAT[[613]]$V2+e_i)
ord=order(c(LOL[[613]],LOL1[[613]]))
v=sort(c(LOL[[613]],LOL1[[613]]))

u=u[ord]

y=y[ord]

plot(v,y,type="b")
plot(v,u,type="b")
lines(v,y,type="b",col="red")

z=36.645583350+2.538164e-03*sin(v*2.141347e+00)+e_i
model<-lm(y~z)

summary(model)
plot(v,u,main="regression curve",xlab="phase",ylab="light curve",type="b")
lines(v,residuals(model),type="b",col="blue")
legend("topright", legend = "Fitted Curve", col = "blue", lty = 1)


#K-medoids clustering. Repeat it atleast for k=21

library(cluster)
k <- 3  # Number of clusters
Kmed_result3 <- pam(d, centers = k)
print(Kmed_result3)
plot(Kmed_result3)


k <- 4  # Number of clusters
Kmed_result4 <- pam(d, centers = k)
print(Kmed_result4)
plot(Kmed_result4)


k <- 21  # Number of clusters
Kmed_result21 <- pam(d, centers = k)
print(Kmed_result21)
plot(Kmed_result21)


# You will see a good pattern after k=15. It will show the present of outlier