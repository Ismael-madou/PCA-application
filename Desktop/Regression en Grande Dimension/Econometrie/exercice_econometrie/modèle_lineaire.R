A<-read.table("C:/Users/MGI/Desktop/Regression en Grande Dimension/Memoire M1/exercice_econometrie/sportcourse.csv", header=T, row.names=1, sep=";")
str(A) ; edit(A)

A[1:4,1:4]

A[1:4,5:8]

variable.names(A)

X11()

plot(A$x400m, A$x100m)

text(A$x400m,A$x100m, labels=rownames(A),
     pos=4, cex=0.8,
     col="blue") 

dev.off()

reg<-lm(X100m~X200m+X400m, data=A) 