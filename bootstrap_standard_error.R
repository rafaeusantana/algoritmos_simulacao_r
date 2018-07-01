# Método Bootstrap para calcular o erro padrao do desvio padrão amostral

amostra <- c(3,5,6,2,5,7,3,5,3,1,2,3,5,8,6)

# funcao empírica
# alternativamente, pode ser usada a função sample para
# gerar a as amostras do bootstrap
Fe <- function(xi){
  a <- table(amostra)
  conta <- sum(a[names(a)<=xi])
  return(conta/length(amostra))
}

n_amostras <- 1000
reamostragens <- matrix(rep(0,n_amostras*length(amostra)), nrow=n_amostras, ncol=length(amostra))
desvio_padrao <- rep(0,n_amostras)

valores_unicos <- sort(unique(amostra))
for(i in 1:n_amostras){
  # simula as amostras
  for(j in 1:length(amostra)){
    u <- runif(1)
    for(k in 1:length(valores_unicos)){
      if(u <= Fe(valores_unicos[k])){
        reamostragens[i,j] <- valores_unicos[k]
        break
      }
    }
  }
}

# calcula o desvio padrao para cada amostra
for(i in 1:n_amostras){
  desvio_padrao[i] <- sd(reamostragens[i,])
}

media_desvios <- mean(desvio_padrao)
print(paste("Média dos desvios:",media_desvios))
erro_padrao <- (sum((desvio_padrao-media_desvios)**2)/(n_amostras-1))^0.5
print(paste("Erro padrão:",erro_padrao))
