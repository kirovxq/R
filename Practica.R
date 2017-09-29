#pregunta 1
#parte b
library(binom)
print("x<-4,k<-6,p<-0.9")
x<-4
k<- 6
p<-0.9
#utiza recursivida cambias pfdks{dadv}
ProbCombination<-function(x,k,p){
  if(x==0)return((1-p)^k)
  else{
    return ((((k-x+1)*p)/(x*(1-p)))*ProbCombination(x-1,k,p))
  }
}
Presult<-ProbCombination(x,k,p)
print(Presult)
#esta parte comprueba el Preslt trata de cambiar asnañm{ADQWD  23}
print(dbinom(x,k, p))
print(choose(k,x)* (p^x)*((1-p)^(k-x)))

#parte C
print("para n<-20 en un rango de 0 a 2")
n<-20
x<-1+runif(n)^2
y<- punif(x, min=0, max=2, lower.tail = FALSE)
plot(x,y, xlab="Y", ylab="pdf(Y)")
print("tamaño de la muestra 20")

#Pregunta 2
M<-2
W<-3
C<-3
m<-M/(M+W)
w<-W/(M+W)
Ccant<-0
for(j in 1:1000){
  CC<-0
  for(i in 1:C){
    op<-sample(0:1,1,prob =c(m,w))
    if(op==0){
      CC<-CC+1
    }
  }
  if(CC%%2==0) Ccant<-Ccant+1
}
prob<-Ccant/1000
print(prob)

#pregunta 3
#parte a
dataf1<-read.table("edad.txt",header = T)
dataf2<-read.table("dientes.txt",header = T)
dataf3<-read.table("edad_dientes.txt",header = T)
data<- cbind(dataf1,dataf2) 
data<-cbind(dataf3,data)
print(data)
#parte b
#ordena por edad
data$Edad
y <- order(data$Edad)
data$Edad<-data$Edad[y]
data$Num_dientes<-data$Num_dientes[y]
print(data)

#Pregunta 5
print("pregunta 5")
data("iris")
iris
df<-data.frame(
  Sepal.Length = c(1:3),
  Sepal.Width  = c(1:3),
  Petal.Length = c(1:3),
  Petal.Width  = c(1:3),
  row.names    = c("setosa","versicolor","virginica")
)

df$Sepal.Length[1]=mean(iris$Sepal.Length[1:50])
df$Sepal.Length[2]=mean(iris$Sepal.Length[51:100])
df$Sepal.Length[3]=mean(iris$Sepal.Length[100:150])

df$Sepal.Width[1]=mean(iris$Sepal.Width[1:50])
df$Sepal.Width[2]=mean(iris$Sepal.Width[50:100])
df$Sepal.Width[3]=mean(iris$Sepal.Width[100:150])

df$Petal.Length[1]=mean(iris$Petal.Length[1:50])
df$Petal.Length[2]=mean(iris$Petal.Length[50:100])
df$Petal.Length[3]=mean(iris$Petal.Length[100:150])

df$Petal.Width[1]=mean(iris$Petal.Width[1:50])
df$Petal.Width[2]=mean(iris$Petal.Width[50:100])
df$Petal.Width[3]=mean(iris$Petal.Width[100:150])
print(df)



#parte b
pidigits =
  read.table("http://www.itl.nist.gov/div898/strd/univ/data/PiDigits.dat",
             skip=60)
levels(pidigits)<-c(0:9)
table1<-table(pidigits)
df2<-data.frame(table1)
names(df2)<-c("números","frecuencias")
cat("Existen 5000 digitos \n")
print(df2)

#5c)
tinting 
c<-dim(tinting)
print("fila tinting")
print(c[1])
print("columna tinting")
print(c[2])
possum
c<-dim(possum)
print("fila possum")
print(c[1])
print("columna possum")
print(c[2])
c<-dim(possumsites)
print("fila possumsites")
print(c[1])
print("columna possumsites")
print(c[2])

#Tienes que cambiar esta dataframe======#QSDASDFFVWAER"# wfe sd 
df4<-data.frame(
  x=0.4,
  y=c(dbeta(0.4, 2, 6),dbeta(0.4, 4, 6),dbeta(0.4,6, 2)),
  etiq=c("dbdelta(x,2,6)","bdeta(x,4,6)","bdeta(x,6,2)")
)
#COLOCA OTROS COLORES COL=safasdgcadvW LWL+´r3
curve(dbeta(x, 2, 6), from=0, to=1,type ="p",col = "gray")
text(df4$x , df4$y[1] , labels = df4$etiq[1], pos = 4, col = "gray")
par(new=TRUE)
curve(dbeta(x, 4, 6), from=0, to=1,type ="l",col = "blue")
text(df4$x , df4$y[2] , labels = df4$etiq[1], pos = 4, col = "blue")
par(new=TRUE)
curve(dbeta(x, 6, 2), from=0, to=1,type ="s",col = "red")
text(df4$x , df4$y[3] , labels = df4$etiq[1], pos = 4, col = "red")
title(expression(f(y)==frac(1,B(a,b))*y^{a-1}*(1-y)^{b-1}))

#aqui nose que hace e{psfj ´cf}
bc <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}
result<-bc(10)
print(result)



library(moments)
m1 <- moment(1)
m1
m2 <- moment(2)
m2
x <- runif(100)
x
stopifnot(all.equal(m1(x), 0))
stopifnot(all.equal(m2(x), var(x) * 99 / 100))




