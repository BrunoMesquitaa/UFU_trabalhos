
########  Função read_excel
if (!require('qcc'))install.packages("qcc");library(qcc)
require(readr)


X <- read_csv("github/UFU_trabalhos/Controle Estatístico de Qualidade/dados2.csv")
#calc amplitude by row
X$R <- apply(X, 1, function(x) diff(range(x)))
#calc media by row
X$Xbar <- rowMeans(X[,1:5])


####Exemplo completo###
## B e C
gráfico.R = qcc(X[,1:5], type="R")#m = 20 obs
# LCI 0 e LCS 771 sigma0 = 364.6

# removendo os pontos dentro de controle
X = X[X$R > 0 & X$R < 770.9356,]

gráfico.R1 = qcc(X[,1:5], type="R")#m = 17 obs
# LCI 0 e LCS 501.5237 LM = 237

# removendo os pontos dentro de controle
X = X[X$R > 0 & X$R < 501.5237,]

gráfico.R1 = qcc(X[,1:5], type="R")#m = 14 obs
# LCI 0 e LCS 316.8684 LM = 149.85

X = X[X$R > 0 & X$R < 316.8684,]

gráfico.R1 = qcc(X[,1:5], type="R")#m = 13 obs
# LCI 0 e LCS 269.3509 LM = 127.38

X = X[X$R > 0 & X$R < 269.3509,]

gráfico.R1 = qcc(X[,1:5], type="R")#m = 12obs
# LCI 0 e LCS 241.9306 LM = 114.41


grafico.xbar = qcc(X[,1:5], type="xbar",std.dev = 49.19)# m=12 obs
# LCI 5597.688 LCS 5729.679

# sigma0 49.19 mu 5663.683

# calcularndo NMA0
alfax=2*pnorm(-3)
NMA0 = 1/alfax
NMA0
# [1] 370.7708

k=3
n=5  #tamanho da amostra
delta=1.5 # ou delta =(mi1-mi0)/sigma0
lambda=1.5 #ou lambda = sigma1/sigma0

Pd= pnorm((-k-(delta*sqrt(n)))/lambda)+pnorm((-k+(delta*sqrt(n)))/lambda)
Pd

NMA1 = 1/Pd

mu1 = delta * 49.19 + 5663.683
sigma1 = 49.19*lambda
sigma1

# -------------------------------------------------------------------------

k=3
n=5  #tamanho da amostra
delta=4 # ou delta =(mi1-mi0)/sigma0
lambda=3 #ou lambda = sigma1/sigma0

Pd= pnorm((-k-(delta*sqrt(n)))/lambda)+pnorm((-k+(delta*sqrt(n)))/lambda)
Pd

NMA1 = 1/Pd

mu1 = delta * 49.19 + 5663.683
sigma1 = 49.19*lambda
sigma1

# -------------------------------------------------------------------------

k=3
n=5  #tamanho da amostra
delta=2 # ou delta =(mi1-mi0)/sigma0
lambda=2 #ou lambda = sigma1/sigma0

Pd= pnorm((-k-(delta*sqrt(n)))/lambda)+pnorm((-k+(delta*sqrt(n)))/lambda)
Pd

NMA1 = 1/Pd

mu1 = delta * 49.19 + 5663.683
sigma1 = 49.19*lambda
sigma1

# -------------------------------------------------------------------------

k=3
n=5  #tamanho da amostra
delta=1 # ou delta =(mi1-mi0)/sigma0
lambda=2 #ou lambda = sigma1/sigma0

Pd= pnorm((-k-(delta*sqrt(n)))/lambda)+pnorm((-k+(delta*sqrt(n)))/lambda)
Pd

NMA1 = 1/Pd

mu1 = delta * 49.19 + 5663.683
sigma1 = 49.19*lambda
sigma1

# -------------------------------------------------------------------------

# simulado dados
set.seed(123)
X1 = rnorm(5,5663.683,80)
X2 = rnorm(5,5663.683,80)
X3 = rnorm(5,5663.683,08)
X4 = rnorm(5,5663.683,80)
X5 = rnorm(5,5663.683,80)
data = data.frame(X1,X2,X3,X4,X5)

##FASE 2 , EM TEMPO REAL, opção newdata
fase1e2xbar = qcc(X[1:5], type = "xbar", std.dev=49.19, newdata = data)

##gráfico de R
fase1e2R = qcc(X[1:5], type = "R", newdata =data)






