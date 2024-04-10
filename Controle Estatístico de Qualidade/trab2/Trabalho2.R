library(readxl)
library(qcc)


# problema 2 --------------------------------------------------------------


dados_problema2 <- read_excel("ceq_trab2.xlsx", sheet = 1)

# a) Descrição e apresentação dos dados.
# Os dados representam os defeitos da superfície de placas de aço foram observados em 24 placas retangulares. Obterndo o numero de defeitos na placa.

# b) No contexto dos dados, qual é a característica de interesse a ser monitorada no processo?
# A caracteristica a ser monitorada é o número de defeitos na placa.

# c) Qual o gráfico de controle apropriado para monitorar o processo?
# O gráfico de controle apropriado para monitorar o processo é o gráfico de c. Número de não-conformidades (defeitos) na amostra nesse caso por placa e de tamanho unico.

# d) O processo está em controle (fase 1) ? Apresente o gráfico gerado no R e os comandos para gerá-lo.
n=1
grafico_c <- qcc(dados_problema2$num_def_placa, size = n, type = "c", nsigmas=3)
# No gráfico gerado, podemos observar que o processo está fora de controle, pois há pontos fora dos limites de controle.

# e) Qual é o parâmetro do processo em controle ( p0 ou u0, de acordo com o tipo de gráfico) ?
u0 = grafico_c$center/n
u0
# O parâmetro do processo em controle é u0 = 2.833333

# f) Quais são os limites do gráfico para monitorar futuras observações ( fase 2)?

dados_problema2 = dados_problema2[dados_problema2$num_def_placa < grafico_c$limits[2],]

grafico_c <- qcc(dados_problema2$num_def_placa, size = n, type = "c", nsigmas=3)

grafico_c$limits
grafico_c$center

# LIC=0   LM=2.272727  LSC=6.795397

# g) Qual é probabilidade de ocorrer alarme falso? Apresentar os comandos.

alfa = 1 - ppois(6,u0)
alfa

# A probabilidade de ocorrer alarme falso é de 0.02579325

# h) Qual é o poder do gráfico ao monitorar diferentes aumentos nos parâmetros do processo? Fixe 3 
# valores para o parâmetro fora de controle (p1 ou u1) e calcule o poder desse gráfico. Apresentar os
# comandos. *Valores diferentes entre os grupos.
# para aumento de 40%, 100%, 200%
u1_1 = u0*0.4+u0
u1_2 = u0*1+u0
u1_3 = u0*2+u0

# u1 = 4.25 (para 40%), 5.666666 (para 100%), 8.5 (para 200%)

beta1 = 1 - ppois(6,u1_1)
beta2 = 1 - ppois(6,u1_2)
beta3 = 1 - ppois(6,u1_3)

# poder = 0.1072298 (para 40%), 0.3403257 (para 100%), 0.7438221 (para 200%)

nma1_1 = 1/beta1
nma1_2 = 1/beta2
nma1_3 = 1/beta3

# nma = 9.325765 (para 40%), 2.94 (para 100%), 1.34 (para 200%)


# i) Proponha 3 futuras observações para exemplificar o monitoramento na fase 2 ( livre escolha) e apresente o gráfico gerado pelo R. Apresentar os comandos.

# simulando dados
set.seed(27)
dados_problema2_new = rpois(3, 3)

grafico_c <- qcc(dados_problema2$num_def_placa, size = n, newdata = dados_problema2_new, newsizes =n, type = "c", nsigmas=3)


# problema 4 --------------------------------------------------------------

dados_problema4 <- read_excel("ceq_trab2.xlsx", sheet = 2)

# a) Descrição e apresentação dos dados.
# Os dados a seguir apresentam aos resultados da inspeção de todos os notebooks produzidos nos últimos 10 dias. porem apresenta um numero variado de notebooks inspecionados por dia. e um a fracção de notebooks defeituosos.

# b) No contexto dos dados, qual é a característica de interesse a ser monitorada no processo?
# A caracteristica a ser monitorada é a proporção de notebooks defeituosos.

# c) Qual o gráfico de controle apropriado para monitorar o processo?
# O gráfico de controle apropriado para monitorar o processo é o gráfico de p. Proporção de não-conformidades (defeitos) na amostra.

# d) O processo está em controle (fase 1) ? Apresente o gráfico gerado no R e os comandos para gerá-lo.

grafico_p <- qcc(dados_problema4$notebook_def, size = dados_problema4$notebook_inspec, type = "p", nsigmas=3)
# No gráfico gerado, podemos observar que o processo está sob controle, pois não há pontos fora dos limites de controle.

# e) Qual é o parâmetro do processo em controle ( p0 ou u0, de acordo com o tipo de gráfico) ?

p0 <- sum(dados_problema4$notebook_def)/sum(dados_problema4$notebook_inspec)
p0
# O parâmetro do processo em controle é p0 = 0.6

# f) Quais são os limites do gráfico para monitorar futuras observações ( fase 2)?
cbind(grafico_p$limits, grafico_p$sizes)
grafico_p$center

# n = 80   LIC = 0 LSC = 0.1396555 
# n = 110   LIC = 0 LSC = 0.1396555 
# n = 90  LIC =  0 LSC = 0.1350999
# n = 75  LIC =  0 LSC = 0.1422679
# n = 130   LIC = 0 LSC = 0.1224869
# n = 120   LIC = 0 LSC = 0.1250385
# n = 70  LIC =  0 LSC = 0.1451553
# n = 125   LIC = 0 LSC = 0.1237244
# n = 105   LIC = 0 LSC = 0.1295290
# n = 95  LIC =  0 LSC = 0.1330969

# LM = 0.06

# g) Qual é probabilidade de ocorrer alarme falso? Apresentar os comandos.
n = 80 
alfa = 1 - pbinom( (floor(grafico_p$limits[1,2] * n) ), n, p0)
alfa
# A probabilidade de ocorrer alarme falso é de 0.002877441 para n = 80

n = 110
alfa = 1 - pbinom( (floor(grafico_p$limits[2,2] * n) ), n, p0)
alfa
# A probabilidade de ocorrer alarme falso é de 0.002439295 para n = 110

n = 90
alfa = 1 - pbinom( (floor(grafico_p$limits[3,2] * n) ), n, p0)
alfa
# A probabilidade de ocorrer alarme falso é de 0.00276615 para n = 90

n = 75
alfa = 1 - pbinom( (floor(grafico_p$limits[4,2] * n) ), n, p0)
alfa
# A probabilidade de ocorrer alarme falso é de 0.005032492 para n = 75

n = 130
alfa = 1 - pbinom( (floor(grafico_p$limits[5,2] * n) ), n, p0)
alfa
# A probabilidade de ocorrer alarme falso é de 0.00503001 para n = 130

n = 120
alfa = 1 - pbinom( (floor(grafico_p$limits[6,2] * n) ), n, p0)
alfa
# A probabilidade de ocorrer alarme falso é de 0.002254287 para n = 120

n = 70
alfa = 1 - pbinom( (floor(grafico_p$limits[7,2] * n) ), n, p0)
alfa
# A probabilidade de ocorrer alarme falso é de 0.002922969 para n = 70

n = 125
alfa = 1 - pbinom( (floor(grafico_p$limits[8,2] * n) ), n, p0)
alfa
# A probabilidade de ocorrer alarme falso é de 0.003415708 para n = 125

n = 105
alfa = 1 - pbinom( (floor(grafico_p$limits[9,2] * n) ), n, p0)
alfa
# A probabilidade de ocorrer alarme falso é de 0.004107462 para n = 105

n = 95
alfa = 1 - pbinom( (floor(grafico_p$limits[10,2] * n) ), n, p0)
alfa
# A probabilidade de ocorrer alarme falso é de 0.004451695 para n = 95


# h) Qual é o poder do gráfico ao monitorar diferentes aumentos nos parâmetros do processo? Fixe 3
# valores para o parâmetro fora de controle (p1 ou u1) e calcule o poder desse gráfico. Apresentar os
# comandos. *Valores diferentes entre os grupos.

# para aumento de 40%, 100%, 200%

#aumento de 40%

p1 <- p0*0.4+p0

n = 80
poder = 1 - pbinom( (floor(grafico_p$limits[1,2] * n) ), n, p1)
poder # 0.03474402
nma1 <- 1/poder
nma1 # 28.78193

n = 110
poder = 1 - pbinom( (floor(grafico_p$limits[2,2] * n) ), n, p1)
poder # 0.04227034
nma1 <- 1/poder
nma1 # 23.65725

n = 90
poder = 1 - pbinom( (floor(grafico_p$limits[3,2] * n) ), n, p1)
poder # 0.0377209
nma1 <- 1/poder
nma1 # 26.5105

n = 75
poder = 1 - pbinom( (floor(grafico_p$limits[4,2] * n) ), n, p1)
poder # 0.04832621
nma1 <- 1/poder
nma1 # 20.6927

n = 130
poder = 1 - pbinom( (floor(grafico_p$limits[5,2] * n) ), n, p1)
poder # 0.07917314
nma1 <- 1/poder
nma1 # 12.63055

n = 120
poder = 1 - pbinom( (floor(grafico_p$limits[6,2] * n) ), n, p1)
poder # 0.04396828
nma1 <- 1/poder
nma1 # 22.74367

n = 70
poder = 1 - pbinom( (floor(grafico_p$limits[7,2] * n) ), n, p1)
poder # 0.03120489
nma1 <- 1/poder
nma1 # 32.04626

n = 125
poder = 1 - pbinom( (floor(grafico_p$limits[8,2] * n) ), n, p1)
poder # 0.05981439
nma1 <- 1/poder
nma1 # 16.71839

n = 105
poder = 1 - pbinom( (floor(grafico_p$limits[9,2] * n) ), n, p1)
poder # 0.05676171
nma1 <- 1/poder
nma1 # 17.61751

n = 95
poder = 1 - pbinom( (floor(grafico_p$limits[10,2] * n) ), n, p1)
poder # 0.05456227
nma1 <- 1/poder
nma1 # 18.32768


#aumento de 100%

p1 <- p0*1+p0

n = 80
poder = 1 - pbinom( (floor(grafico_p$limits[1,2] * n) ), n, p1)
poder # 0.2486356
nma1 <- 1/poder
nma1 # 4.02195

n = 110
poder = 1 - pbinom( (floor(grafico_p$limits[2,2] * n) ), n, p1)
poder # 0.3398227
nma1 <- 1/poder
nma1 # 2.942711

n = 90
poder = 1 - pbinom( (floor(grafico_p$limits[3,2] * n) ), n, p1)
poder # 0.2810005
nma1 <- 1/poder
nma1 # 3.558712

n = 75
poder = 1 - pbinom( (floor(grafico_p$limits[4,2] * n) ), n, p1)
poder # 0.2860246
nma1 <- 1/poder
nma1 # 3.496202

n = 130
poder = 1 - pbinom( (floor(grafico_p$limits[5,2] * n) ), n, p1)
poder # 0.4970043
nma1 <- 1/poder
nma1 # 2.012055

n = 120
poder = 1 - pbinom( (floor(grafico_p$limits[6,2] * n) ), n, p1)
poder # 0.3665496
nma1 <- 1/poder
nma1 # 2.728144

n = 70
poder = 1 - pbinom( (floor(grafico_p$limits[7,2] * n) ), n, p1)
poder # 0.2141704
nma1 <- 1/poder
nma1 # 4.669179

n = 125
poder = 1 - pbinom( (floor(grafico_p$limits[8,2] * n) ), n, p1)
poder # 0.4317709
nma1 <- 1/poder
nma1 # 2.316043

n = 105
poder = 1 - pbinom( (floor(grafico_p$limits[9,2] * n) ), n, p1)
poder # 0.380033
nma1 <- 1/poder
nma1 # 2.631351

n = 95
poder = 1 - pbinom( (floor(grafico_p$limits[10,2] * n) ), n, p1)
poder # 0.3511452
nma1 <- 1/poder
nma1 # 2.847825

#aumento de 200%

p1 <- p0*2+p0

n = 80
poder = 1 - pbinom( (floor(grafico_p$limits[1,2] * n) ), n, p1)
poder # 0.7981669
nma1 <- 1/poder
nma1 # 1.252871

n = 110
poder = 1 - pbinom( (floor(grafico_p$limits[2,2] * n) ), n, p1)
poder # 0.9097059
nma1 <- 1/poder
nma1 # 1.099256

n = 90
poder = 1 - pbinom( (floor(grafico_p$limits[3,2] * n) ), n, p1)
poder # 0.8455156
nma1 <- 1/poder
nma1 # 1.18271

n = 75
poder = 1 - pbinom( (floor(grafico_p$limits[4,2] * n) ), n, p1)
poder # 0.8149196
nma1 <- 1/poder
nma1 # 1.227115

n = 130
poder = 1 - pbinom( (floor(grafico_p$limits[5,2] * n) ), n, p1)
poder # 0.9693203
nma1 <- 1/poder
nma1 # 1.031651

n = 120
poder = 1 - pbinom( (floor(grafico_p$limits[6,2] * n) ), n, p1)
poder # 0.9309903
nma1 <- 1/poder
nma1 # 1.074125

n = 70
poder = 1 - pbinom( (floor(grafico_p$limits[7,2] * n) ), n, p1)
poder # 0.736777
nma1 <- 1/poder
nma1 # 1.357263

n = 125
poder = 1 - pbinom( (floor(grafico_p$limits[8,2] * n) ), n, p1)
poder # 0.9534952
nma1 <- 1/poder
nma1 # 1.048773

n = 105
poder = 1 - pbinom( (floor(grafico_p$limits[9,2] * n) ), n, p1)
poder # 0.9193888
nma1 <- 1/poder
nma1 # 1.087679

n = 95
poder = 1 - pbinom( (floor(grafico_p$limits[10,2] * n) ), n, p1)
poder # 0.8937276
nma1 <- 1/poder
nma1 # 1.118909

# i) Proponha 3 futuras observações para exemplificar o monitoramento na fase 2 ( livre escolha) e apresente o gráfico gerado pelo R. Apresentar os comandos.

# simulando dados
dados_problema4_new = data.frame(notebook_def = c(1,12,6), notebook_inspec = c(80, 110, 90))

grafico_p <- qcc(dados_problema4$notebook_def, size = dados_problema4$notebook_inspec, 
                 newdata = dados_problema4_new$notebook_def, newsizes = dados_problema4_new$notebook_inspec, type = "p", nsigmas=3)

