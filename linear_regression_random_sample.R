# Simula uma amostra de números aleatórios e faz a regressão linear
x <- runif(100) * 100
y <- rep(0,length(x))

# gera valores para y com erro
for(i in 1:length(x)){
  y[i] <- 3*x[i]+rnorm(1)*20 # 3x + erro
}

# regressao linear
reg <- lm(y ~ x)
s <- summary(reg)
# não rejeita a hipótese nula de que o intercepto é zero, no caso do P-valor ser maior do que 5%
if(s$coefficients[1,4] > 0.05){
  reg <- lm(y ~ x - 1)
}

plot(x, y, main="Modelo linear")
abline(reg, col="red")