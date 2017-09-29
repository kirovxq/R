#Pregunta 1a
#Colocamos la los elementos en un arreglo
f1<- c(13563,-14156,-14319,16981,12921,11979,9568,8833,-12968, 8133)
#Elevamos al cuadrado
f2<-f1^75
#Verificamos cuales NO son "Inf" para luego imprimirlos
for(element in f2){
  if(!is.infinite(element))
    print(element)
}
#Verificamos cuales son "Inf" negativos para excluirlos y luego imprimir los demas elementos
for(element in f2){
  if(is.infinite(element)){
    if(element > 0)
      print(element)
  }
  else{
    print(element)
  }
}

#Pregunta 1b
#Llenamos los datos de la matriz varMatriz
varMatriz = matrix( c(77875.40, 35466.25, 39803.81, 27551.45, -73333.85, 55976.34, 23764.30, 36599.69, 76694.82, -36478.88, -70585.69, 47032.00), nrow=3, ncol=4)
#Elevamos los elementos de la matriz varMatriz por el numero 65
c<-varMatriz^65
#Dividimos cada elemento entre "Inf"
c<-c/Inf
#Verificamos cuales son "NaN" para luego imprimir sus index
which(is.nan(c), arr.ind = TRUE)
#Imprimimos los valores de varMatriz elevados a la 67 que no sean "NaN" y se añade infinito
b<-varMatriz^67
b<-b+Inf
for(element in b){
  if(is.infinite(element)){
    if(element > 0)
      print(element)
  }
}
#Verificamos que es idéntico a identificar aquellos valores en varMatriz que, cuando aumentan a una potencia de 67, no son iguales al infinito negativo
b<-varMatriz^67
for(element in b){
  if(is.infinite(element)){
    if(element > 0)
      print(element)
  }
}
#Valores en varMatriz que sean infinito negativo o finito cuando eleva varMatriz a una potencia de 67.
for(element in varMatriz){
  if(is.infinite(element^67)){
    print(element)
  }
}

#Pregunta 1c
f2 <- c(4.3,2.2,NULL,2.4,NaN,3.3,3.1,NULL,3.4,NA)
length(f2) #Verdadero
which(x=is.na(x=f2)) #Falso
is.null(x=f2) #Falso

#Pregunta 2a
#Agregamos un signo de exclamación a cada elemento de cada vector de la lista
f3<- list("a",c("b","c","d","e"),"f",c("g","h","i"))
func<-function(element){ paste0(element, "!")}
f3<-lapply(f3,func)
f3

#Pregunta 2b
f4 <- list(1:3,matrix(c(3.3,3.2,2.8,2.1,4.6,4.5,3.1,9.4),4,2),
           matrix(c(3.3,3.2,2.8,2.1,4.6,4.5,3.1,9.4),2,4))
#Función que haya la media geométrica de los elementos de un arreglo
geolista<-function(element,numb){for(i in element){numb <- numb*i}; numb <- numb^(1/length(element)); print(numb) }
#Hallamos la media geometrica de los arreglos de una lista
for(element in f4){
  num<-1
  geolista(element,1)
}
#Inspeccionamos que los elementos de cada vector o matriz sea un valor numerico
#Creamos una funcion "inspeccionadora"
inspect <- function(element,numb){for(i in element){if(!is.numeric(i)){print("tiene un elemento no numerico"); break()}}}
num<-1
for(element in f4){
  inspect(element,num)
}

al <- matrix(c(3.3,3.2,2.8,2.1,4.6,4.5,3.1,9.4),4,2)
al

#Pregunta 5a
ggplot(data = mpg)
print("No se ve nada")

#Pregunta 5b
names(mpg)
help(mpg)
print("Segun 'help' tiene 234 filas y 11 columnas.")

#Pregunta 5c
help(mpg)
print("Nos especifica como están distribuidas las ruedas en el vehículo.")

#Pregunta 5d
ggplot(data = mpg) + geom_point(mapping = aes(x = hwy, y = cyl))

#Pregunta 5e
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))
print("Porque ambas variables son del tipo categórico, por tal motivo los puntos se superponen uno sobre otro")

#Pregunta 5f
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
print("Porque el argumento de color no esta en geom_point, sino en aes.")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

#Pregunta 5g
help(mpg)
print("Los categóricos son: manufacturer, model, trans, drv, fl y class.")

#Pregunta 5h
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = cty))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = cty))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = cty))
print("En este último se genera error porque las variables continuas no pueden ser mapeadas a formas, estas variables son visualizadas en espectros y las categóricas estan divididas en categorías discretas.")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))

#Pregunta 5i
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = cty, size = cty))
print("Ambas estéticas son implementadas y diferentes leyendas son generadas.")

#Pregunta 5j
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), stroke = 3, shape = 21)
print("stroke ajusta el espesor de los bordes de las formas que pueden tomar diferentes colores, en ambos, tanto dentro como por fuera.")

#Pregunta 5k
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))
print("R al ejecutar el código crea variables temporables que contienen los resultados de la operación. Aqui la nueva variable toma un valor de verdadero si el desplazamiento del motor es menor a 5 o falso  is el desplazamiento es mayor igual a 5")

#Pregunta 5l
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ cyl)
ggplot(data = mpg) + geom_point(mapping = aes(x = drv, y = cyl)) + facet_wrap(~ displ)
print("La gráfica no tendra mucho sentido. Se dibujará un facet separado por cada valor unico de la variable continua. Si se tiene muchos valores unicos puede ocurrir un error con R por sobrecarga.")

ggplot(data = mpg) + geom_point(mapping = aes(x = drv, y = cyl))
ggplot(data = mpg) + geom_point(mapping = aes(x = drv, y = cyl)) + facet_grid(drv ~ cyl)
print("Las celdas vacias significan que no hay por observar datos que tengan una combinación única de valores. De tal forma, en este plot se puede determinar que no hay vehículos con 5 cilindros que son conducidos con 4 ruedas.")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ .)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(. ~ cyl)
print("El punto actua como marcador de posición para ninguna variable. En facet_grid(), los resultados en el plot aparecen representados en una sola dimensión (1xN o Nx1) en vez de una grilla de NxN.")

#Pregunta 5m
print("nrow selecciona cuantas filas el plot facetado tendrá. ncol selecciona cuantas columnas el plot facetado tendrá. 'as.table' detemrina la faceta inicial para empezar a llenar el plot, y 'dir' determina la dirección inicial para rellenar in el plot, ya sea horizontal o vertical. ")

#Pregunta 5n
print("Extenderá el plot vertical, en donde normalmente se tiene mas espacion de visión. Si se extiende horizontalmente, el plot va a comprimirse y será más difícil de verlo.")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(trans ~ drv)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ trans)
