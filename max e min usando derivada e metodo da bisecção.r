f <- function(x, y)#funcao qualquer
{
  return( (4 + (x-1)**2 + (y-3)**2))
} 

h <- 10**-5 #precisao

#derivada de f
der <- function(p, variavel)
{
  if(variavel == "y")
  { #derivada parcial em y
    return(
      (f(0, p+h) - f(0, p))/h  )
    
  }else{
    #derivada parcial em x
    return( 
      (f(p+h, 0) - f(p, 0))/h  )
  }
}

#intervalo da raiz
a <- (-4)
b <- (4)

#num de iterações para o erro h
ndeiteracao <- ceiling(log2(b-a) - log2(h))

#met da bisecção
raiz <- function(vr)
{
  i <- 0
  while(i < ndeiteracao)
  {
    if( der(a, vr)*der((a+b)/2, vr) < 0)
    {
      b <- ((a+b)/2)
      
    }else{
        
      a <- ((a+b)/2)
    }
    
    i <- i+1
  }
  
  return((a+b)/2)
}
raiz("x")
raiz("y")
f(1,2)
# (1,3) é de fato o ponto minimo
