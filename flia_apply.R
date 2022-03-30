rm("A", "B", "C", "my.list")
A<- sample(1:9,9, replace = T)
B<- sample(1:12,12, replace = T)
C<- c("a", "b", "c", "d", "f", "g", "h", "i", "j")
my.lst<-list(A,B,C)
vapply(my.lst, nrow, numeric(0))

my.matrx <- matrix(1:30,nrow = 10,ncol = 3)

tdata <- as.data.frame(cbind(c(1,1,1,1,1,2,2,2,2,2), my.matrx))
mean(tdata$V2)
tapply(tdata$V2, tdata$V1, mean)

tapply(tdata$V2, tdata$V1, sd)
sd(tdata$V2)
