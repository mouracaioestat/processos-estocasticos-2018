### Códigos úteis

# Questão 1. Exercicio 7 da Lista 1
# Encontrando a prob. p3(D,D)
M <- matrix(c(2/3,1/3 ,1/4,3/4), byrow = T, ncol = 2)

M %*% M %*% M

potM <- function(M, n){
  if (n == 1)  return (M)
  if (n == 2)  return (M %*% M)
  if (n > 2) return ( M %*% potM(M, n-1))
}
potM(M, 3)


###########################################################
# Questão 2. Exercicio 9 da lista 1
# Modelo de Ehrenfest com n+1 espaços

# Construindo a matrix de transição
matriz <- function(greatest.estate){
  M <- matrix(0, nrow = greatest.estate+1, ncol = greatest.estate+1)
  M[1, 2] <- 1
  M[greatest.estate+1, greatest.estate] <- 1
  for(j in 2:greatest.estate){
    M[j, j-1] <- (j-1)/greatest.estate
    M[j, j+1] <- (greatest.estate - (j-1))/greatest.estate
    
  }
  return(M)
}

greatest.estate <- 4
M <- matriz(greatest.estate)


##########################################################
# Nos próximos exercicios eh interessante variar o 'p' e notar o efeito no grafico

# Questão Exercicio 12
#(a)
tempo.total <- 61
est.inicial <- 0
estados <- numeric()
p <- 0.15
estados[1] <- est.inicial
for(i in 2:tempo.total){
  u <- runif(n = 1, min = 0, max = 1)
  estados[i] <- ifelse(test = u <= p, yes = estados[i-1] + 1, no = estados[i-1] - 1)  
}
estados

# O codigo abaixo cria um gráfico para 
plot(c(0, tempo.total), c(-tempo.total*(abs(p-.5)+0.4), tempo.total*(abs(p-.5)+0.4)), type = "n", main = "", xlab = "Tempo", ylab = "Estado")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
lines(0:(tempo.total-1), estados, col="indianred")


#(c)
M <- matrix(c(.1,.1,.8, .2,.2,.6, .3,.3,.4), byrow = T, ncol = 3)
# Calcular a distribuicao estacionaria nos da uma intuicao
# do que se espera da simulacao
potM(M,2000)

tempo.total <- 61
est.inicial <- 0
estados <- numeric()
estados[1] <- est.inicial

for(i in 2:tempo.total){
  u <- runif(n = 1, min = 0, max = 1)
  if(u <= M[estados[i-1] + 1, 1]){
    estados[i] <- 0
  }
  if(M[estados[i-1] + 1, 1] < u && u<= M[estados[i-1] + 1, 1] + M[estados[i-1] + 1, 2]){
    estados[i] <- 1
  }
  if(M[estados[i-1] + 1, 1] + M[estados[i-1] + 1, 2] < u && u <= 
     M[estados[i-1] + 1, 1] + M[estados[i-1] + 1, 2]+ M[estados[i-1] + 1, 3]){
    estados[i] <- 2
  }
}
estados
table(estados)

# O codigo abaixo cria uma representacao grafica para o processo
plot(c(0, tempo.total), c(0,2), type = "n", main = "", xlab = "Tempo", ylab = "Estado")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
lines(0:(tempo.total-1), estados, col="indianred")



#########################################################

### Paradoxo da partida, adaptado de Dobrow (2016, p.256)

## Suponha que Catarina sempre chega no ponto de ônibus às 5h45 da manhã
## A empresa de ônibus libera seus ônibus de acordo com um processo de Poisson
## com uma taxa de 5,5 ônibus por hora. O processo se inicia diariamente às 5h.
## Qual é o tempo médio de espera de Catarina durante 1000 dias?

## Resolução:
## Geramos os tempos de chegada de um processo de Poisson até às 8h.
## Por simplicidade 5h equivale ao tempo 0 e 12h equivale ao tempo 7.
## Catarina chega, portanto, no tempo 0,75

#Espera <- function(){
# Inicio um vetor de tamanho variável
tempos <- numeric(0)
tempo_da_catarina <- 0.75


i <- 1
# Tempo do início do processo é zero
t <- 0
while(t < 7){
  tempos[i] <- t
  # A função rexp gera o tempo de chega de um ônibus
  t <- t + rexp(1, rate = 5.5)
  i <- i + 1
}
# Obtenho todos tempos de chegada
tempos

total_onibus <- length(tempos) - 1

# plot(tempos[-1], rep(0, total_onibus), pch = 4, ylab = "", xlab = "", ylim = c(-0.5,0.5), xlim = c(0.01,2.99))
# lines(c(0,3), c(0,0))

# Para saber qual o tempo do ônibus que Catarina pegou,
# pergunto quais os indices do vetor tempo cujo valor é acima de 0.75
# e escolho o menor índice, pois o vetor está ordenado.
which(tempos > tempo_da_catarina)
indice_onibus_catarina <- min(which(tempos > tempo_da_catarina))

espera <- tempos[indice_onibus_catarina] - tempo_da_catarina
espera
#return(espera) }

# registro <- replicate(1000, Espera())
# registro
# mean(registro)
# 1/5.5
# hist(registro)
# max(registro)

##########################################################
### Poisson bivariada

lambda <- 7 # Taxa do processo
area <- 36  # trata-se de um quadrado de lado 6
N <- rpois(1, lambda * area) # fixa o numero de pontos total
xcoord <- runif(N, 0, 6)
ycoord <- runif(N, 0, 6)
plot(xcoord, ycoord, cex=0.7, ylab = "", xlab = "")  


# setwd("~/Dropbox/Capacitação_Docente")
x <- as.matrix(unname( read.csv("base01coord_x.csv", header = TRUE) ) )
y <- as.matrix(unname( read.csv("base01coord_y.csv", header = TRUE) ) )

plot(x, y, cex=0.7, ylab = "", xlab = "")  


