library("tidyverse")

#metodo da secante

parabola <- function(x)
{
    x**2 + x - 6
}

ximais1 <- function(func, xl1, xl2) # funcao do proximo Xi na iteraçao
{
    f2 <- func(xl2)
    f1 <- func(xl1)
    return(
      xl2 - ((xl2 - xl1) * f2)/(f2- f1)
    )
}

metsec <- function(funcao, x1, x2)
{
  n <- 2
  #n começa no 2 pois x1, x2 já é definido
  #como os 2 chutes iniciais
  
  xi <- c(x1, x2) #lista com todas as iteraçoes
  
  while( abs(funcao(xi[n])) > 0.001 )  #precisao de o.oo1
  {
    
    xi <- append( xi[n], ximais1(funcao, xi[n - 1], xi[n]) )
    
    print(xi[n+1])
    n = n+1
  }
  
  cat("\n num de iteracoes: ", n , "\n ")
  cat("raiz: \n")
  return(xi[n])
  
}

metsec(parabola, 0, 4)

#gráfico da precisao (secante)
grf <- c(4,1.2,1.741935,2.052373,1.997181,1.999971)
grf2 <- (grf - 2) %>% abs()
plot(grf2)



