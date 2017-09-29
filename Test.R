cambio <- function(x, y.vec = c(),ListaDeCambios) {
  
# encuentra las posibles formas posibles de tener x usando monedas Australianas
# x es dado en centavos y asumimos que es divisible por 5
# y.vec son monedas ya utilizada (asi la cantidad total es x + sum(y.vec))
  if (x == 0) {
    cat(y.vec, " Ves\n")
    #print(y.vec)
    a <-y.vec
    ListaDeCambios<-list(ListaDeCambios,a)
    ListaDeCambios
   # print(listc)
  } else {
    monedas <- c(200, 100, 50, 20, 10, 5)
    new.x <- x -monedas
    new.x <- new.x[new.x >= 0]
    for (z in new.x) {
      y.tmp <- c(y.vec, x - z)
      if (identical(y.tmp, sort(y.tmp))) {
        #print(y.tmp)
        #listc <- y
        cambio(z, y.tmp,ListaDeCambios)
      }
    }
  }
   # return(invisible(NULL))
}
A<-c()
ListaDeCambios <- list(c())
cambio(145,A,ListaDeCambios)
ListaDeCambios
#ListaDeCambios
#l <- list(c(2,3,4),5)
#l <- list(l,20)
#l
help("identical")
