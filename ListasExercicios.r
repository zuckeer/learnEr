# Solução Lista 1 de Exercícios em R

# Imports
install.packages("tidyverse")
library(tidyverse)

# Diretório de trabalho
setwd("E:/DSA_archives/Form_Estatistica/Lista1")
getwd()

# Carregando o dataset
data <- read_csv("hour.csv")

# Como calcular média, mediana e moda

# Média
lapply(data, mean)

# Mediana
lapply(data, median)

# Moda
md <- lapply(data, table)
lapply(md, max)

# Calculo da temperatura (coluna temp) média por dia da semana (coluna weekday).
data %>% 
  group_by(weekday) %>% 
  summarise(media_temp = mean(temp))

# Cria um histograma na coluna temp.
data %>% 
  ggplot(aes(x = temp)) + 
  geom_histogram(bins = 30)

#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
# Solução Lista 2 de Exercícios em R

# Diretório de trabalho
setwd("E:/DSA_archives/Form_Estatistica/Lista2")
getwd()

# Carregando o dataset
data <- read.csv("dataset.csv")
View(data)
str(data)

# 1-Quais as 3 cores dos veículos mais vendidos?
tabcor <- table(data$cor)
tabcor

# Outro jeito de responder o item 1
library(tidyverse)

data %>% 
  count(cor) %>% 
  top_n(3,n) %>% 
  arrange(desc(n))

# 2-De qual ano são os veículos mais vendidos?
data$ano <- as.factor(data$ano)
tabano <- table(data$ano)
tabano

# Melhorando a resposta do item 2
data %>% 
  count(ano) %>% 
  top_n(3,n) %>% 
  arrange(desc(n))

# 3-Crie um barplot para apresentar sua resposta no item 2.
barplot(tabcor, main="Counts")

# Outro jeito de responder o item 3
data %>% 
  count(ano) %>% 
  top_n(3,n) %>% 
  ggplot(aes(x = ano, y = n)) + 
  geom_bar(stat = "identity") +
  labs(title = "Ano dos veículos mais vendidos") +
  xlab("Ano") +
  ylab("Total de Carros")

# 4-Qual o percentual de vendas de veículos com transmissão automática?
tabcambio <- table(data$transmissao)
tabcambio
relfreq <- prop.table(tabcambio) * 100
relfreq

# Outro jeito de responder o item 4
data %>% 
  group_by(transmissao) %>% 
  summarise(perc_vendas_trans = n()) %>% 
  mutate("%" = round((perc_vendas_trans / sum(perc_vendas_trans)*100),3))

# 5-Crie um Pie Chart para representar sua resposta no item 4.
lbls <- c("Auto", "Manual")
lbls <- paste(lbls,"%",sep="")
pie(relfreq, labels = lbls, col = rainbow(length(lbls)), main="Pie Chart de Veículos Vendidos Por Tipo de Transmissão")


#Outro jeito de responder o  item 5
data %>% 
  group_by(transmissao) %>% 
  summarise(perc_vendas_trans = n()) %>% 
  mutate("%" = round((perc_vendas_trans / sum(perc_vendas_trans)*100),2)) %>% 
  ggplot(aes(x = "", y = transmissao, fill = transmissao)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = `%`), hjust = .5, vjust = -1.5, color = "white", fontface = "bold") +
  labs(title = "Percentual de vendas de veículos com transmissão automática") +
  xlab("") +
  ylab("Transmissão")

# 6- Qual o percentual de venda de veículos por modelo?

# Alternativa 1
install.packages("janitor")
library(janitor)
tabyl(data$modelo, sort = TRUE)

# Alternativa 2
data %>% 
  group_by(modelo) %>% 
  summarise(perc_vendas_mod = n()) %>% 
  mutate("%" = round((perc_vendas_mod / sum(perc_vendas_mod)*100),2))

# Alternativa 3
install.packages("epiDisplay")
library(epiDisplay)
tab1(data$modelo, sort.group = "decreasing", cum.percent = TRUE)
?tab1

# 7- Calcule o percentual de vendas por preço de veículo e o percentual acumulado.
tabpreco <- table(data$preco)
tabpreco
relfreq <- prop.table(tabpreco) * 100
relfreq
cumsum(relfreq)

# Alternativa
data %>% 
  group_by(preco) %>% 
  summarise(perc_vendas_preco = n()) %>% 
  mutate("freq_Relativa" = round((perc_vendas_preco / sum(perc_vendas_preco)*100),2),
         "freq_Acumulada" = cumsum(freq_Relativa))

# 8 - Liste o total de veículos vendidos por ano e por tipo de transmissão 
# (Dica: uma tabela de contingência faz isso para você com um comando)
tab <- table(data$ano, data$transmissao)
tab
?table

# 9- Imprima um resumo estatístico com o teste do qui-quadrado, graus de liberdade e valor p do resultado do item anterior.
summary(tab)
chisq.test(tab)

# 10 - Crie um barplot a partir do resultado do item 8.
barplot(tab, beside = TRUE, legend.text = rownames(tab), ylab = "Frequência Absoluta")

#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Solução Lista 3 de Exercícios em R

# Diretório de trabalho
setwd("E:/DSA_archives/Form_Estatistica/Lista3")
getwd()

# Carrega o dataset
data <- read.csv("dataset.csv")
View(data)
str(data)

# 1-Qual o tempo médio de atendimento?
duration = data$tempo_telefone
mean(duration)


# 2-Qual a mediana do tempo de atendimento?
duration = data$tempo_telefone
median(duration)

# 3-Qual o desvio padrão e variância do tempo de atendimento?
duration = data$tempo_telefone
sd(duration)
var(duration)

# 4-Quais os quartis e percentis 32, 57 e 98 do tempo de atendimento?
duration = data$tempo_telefone
quantile(duration)  
quantile(duration, c(.32, .57, .98)) 

# 5-Qual o intervalo interquartil do tempo de atendimento?
IQR(duration)    

# 6-Crie um boxplot que represente as medidas anteriores.
boxplot(duration, horizontal=TRUE) 

# 7-Calcule a covariância entre o tempo de atendimento e o total de clientes.
duration = data$tempo_telefone    
clientes = data$clientes       
cov(duration, clientes) # Indica uma relação linear positiva entre as duas variáveis.

# 8-Calcule a Frequência Absoluta da variável tempo de atendimento.
duration = data$tempo_telefone
range(duration) 

# Frequência Absoluta
breaks = seq(1.5, 5.5, by=0.5)    
breaks 
duration.cut = cut(duration, breaks, right=FALSE)
duration.freq = table(duration.cut)
duration.freq 
cbind(duration.freq) 

# 9-Crie um histograma que represente sua resposta no item anterior.
duration = data$tempo_telefone
hist(duration, right=FALSE)

colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan") 
hist(duration, right=FALSE, col=colors, main="Tempo de Atendimento x Total de Clientes", xlab="Duração em Minutos")   

# 10-Calcule a Frequência Relativa e Acumulada da variável tempo de atendimento.
duration = data$tempo_telefone
breaks = seq(1.5, 5.5, by=0.5) 

# Relativa
duration.cut = cut(duration, breaks, right=FALSE) 
duration.freq = table(duration.cut)
duration.freq
duration.relfreq = duration.freq / nrow(data)
duration.relfreq 
cbind(duration.relfreq)
options(digits=1) 
duration.relfreq 
cbind(duration.freq, duration.relfreq) 

# Acumulada
duration.cumfreq = cumsum(duration.freq)
duration.cumfreq 
cbind(duration.freq, duration.relfreq, duration.cumfreq) 

# 11-Crie um gráfico de linha para a Frequência Acumulada.
cumfreq0 = c(0, cumsum(duration.freq)) 
plot(breaks, cumfreq0, main="Tempo de Atendimento x Total de Clientes", xlab="Duração em Minutos",  ylab="Total Clientes") 
lines(breaks, cumfreq0)  

# 12-Crie um gráfico de dispersão para representar a relação entre as duas variáveis no dataset.
duration = data$tempo_telefone    
clientes = data$clientes       
plot(duration, clientes, xlab="Tempo de Atendimento", ylab="Total Clientes")

#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

# Solução Lista 4 de Exercícios em R

# Diretório de trabalho
setwd("E:/DSA_archives/Form_Estatistica/Lista4")
getwd()

# Carrega o dataset
data <- read.csv2("machebaya.csv",fileEncoding = "UTF-8")
data <- read.csv("machebaya.csv", sep = ';',fileEncoding = "UTF-8")
View(data)
attach(data)


# 1-Qual o total de pessoas com ensino médio na capital?
tabela1 <- table(reg_procedencia,grau_instrucao)
tabela1

# Formatando a saída
Total_linha <- margin.table(tabela1, 2)  # O argumento 2 define a marginal da linha
Total_coluna <- margin.table(tabela1, 1) # O argumento 1 define a marginal da coluna
tabela1_final <- rbind(cbind(tabela1,Total_coluna), c(Total_linha, sum(Total_coluna)))
dimnames(tabela1_final)[[1]][4] <- "Total_linha" 
tabela1_final

# 2-Qual o percentual de pessoas do interior com ensino superior?
tabela2 <- prop.table(tabela1)
tabela2

Total_linha <- margin.table(tabela2,2) 
Total_coluna <- margin.table(tabela2,1)
tabela2_final <- rbind(cbind(tabela2,Total_coluna),c(Total_linha, sum(Total_coluna)))
dimnames(tabela2_final)[[1]][4] <- "Total_linha" 
tabela2_final

# Formatando o resultado final
library(tidyverse)
tabela2_final %>% `*`(100) %>% round(2)

# 3-Crie um Stacked Bar Chart para representar a relação entre o grau de instrução e a região de procedência.
library(ggplot2)
library(scales)
library(reshape2)

ggplot(melt(tabela1_final[1:3,],value.name = "contagem",
            varnames = c("reg_procedencia","grau_instrucao") ))+        ## `melt` empilha os dados no formato necessário para o ggplot
  aes(x=grau_instrucao ,y=contagem, fill=reg_procedencia) +             ## Variáveis a serem plotadas. 
  geom_bar(stat="identity", position = "fill") +                        ## Define o gráfico de barras percentual empilhado 
  scale_fill_brewer(name="Região de\n Procedência")+                    ## Opções do preenchimento do gráfico (label e paleta de cores)
  scale_y_continuous(labels = percent_format()) +                       ## Formato do eixo Y em porcentagem
  theme_bw()+                                                           ## Define a cor do fundo do gráfico: neste caso, branco
  theme(legend.position="bottom") +                                     ## Define a posição da legenda abaixo do gráfico
  ggtitle("Stacked Bar: Distribuição da região de procedência por grau de instrução")+
  xlab("Grau de Instrução") + ylab("")                                  ## Define os `labels` dos eixos


# 4-Qual a relação entre grau_instrucao e estado_civil?
install.packages("gmodels")
library(gmodels)

CrossTable(grau_instrucao,estado_civil, 
           prop.r=TRUE,       # Se TRUE, entao retorna as proporções nas linhas
           prop.c=FALSE,      # Se TRUE, entao retorna as proporções nas colunas
           prop.t=FALSE,      # Se TRUE, entao retorna as proporções em relação ao total
           prop.chisq=FALSE,  # Se TRUE, entao retorna a contribuição de cada combinação para a estatística de Qui-quadrado
           digits=2)

# Teste do Qui-Quadrado
tabela3 <- table(grau_instrucao,estado_civil)
testequi <- chisq.test(tabela3) 
testequi

# 5-Como interpretar o resultado do Teste do Qui-quadrado no item anterior? As variáveis são dependentes ou independentes?

# Duas variáveis aleatórias x e y são chamadas independentes se a distribuição de probabilidade de uma variável não for afetada pela presença de outra.

# Para estabelecer que 2 variáveis categóricas são dependentes, a estatística qui-quadrado deve estar acima de um certo ponto de corte. 
# Este corte aumenta à medida que o número de classes dentro da variável aumenta.

# Alternativamente, você pode simplesmente executar um teste qui-quadrado e verificar os valores-p.

# Como todos os testes estatísticos, o teste qui-quadrado assume uma hipótese nula e uma hipótese alternativa. 
# A prática geral é, se o valor de p que sai no resultado é menor que um nível de significância pré-determinado, que é 0,05 normalmente, 
# então rejeitamos a hipótese nula.

# H0: As duas variáveis são independentes
# H1: As duas variáveis estão relacionadas.

# A hipótese nula do teste qui-quadrado é que as duas variáveis são independentes e a hipótese alternativa é que elas estão relacionadas.

# O valor do qui-quadrado será maior se a diferença entre os valores real e esperado aumentar.
# Além disso, quanto mais as categorias nas variáveis, maior deve ser a estatística qui-quadrado.

# Como o valor de p 0.3843 é maior que o nível de significância de 0,05, 
# não rejeitamos a hipótese nula de que o grau de instrução seja independente do estado civil.


# 6-Quais são as técnicas usadas para analisar a relação entre:

# 2 Variáveis Categóricas         
# ==> Teste do Qui-Quadrado

# 2 Variáveis Numéricas                                       
# ==> Regressão Linear 

# Variável Categórica (Dependente) e Numérica (Independente)  
# ==> Regressão Logística

# Variável Categórica (Independente) e Numérica (Dependente)  
# ==> ANOVA (Análise de Variância)


# 7-Explique o que é Correlação e o que é Causalidade.

# Correlação é um conceito que se refere à medida da relação entre duas variáveis. 
# Por exemplo, pensemos nas variáveis quantidade de sorvetes vendidos e temperatura. Em dias de calor acentuado, 
# verifica-se que a quantidade de sorvetes vendidos tende a ser mais elevada. Isto é, diz-se que há uma correlação positiva 
# entre temperaturas e quantidade de sorvetes vendidos. 
# A correlação ainda pode ser nula (ou quase nula), por exemplo, quando tentamos relacionar o consumo de arroz com a 
# temperatura. Esse consumo tende a não variar expressivamente em decorrência da mudança de temperatura, 
# diz-se então que a correlação entre o consumo de arroz e a temperatura é (praticamente) nula.

# Causalidade é um conceito em que há relação entre uma variável X e uma variável Y e a variável Y é consequência da variável X, 
# ou dito de outra maneira, a variável X é causa da variável Y. 
# Ainda no exemplo da temperatura e da quantidade vendida de sorvete, podemos identificar que dias de temperaturas mais 
# altas implicam em uma maior quantidade média de sorvetes vendidos. 
# Isto é, dadas as preferências das pessoas de consumirem mais sorvetes em dias de calor, quando a temperatura aumenta, em média, 
# a quantidade de sorvete vendida também aumenta. Com o devido embasamento teórico a respeito das preferências das pessoas, 
# pode-se concluir que o aumento de temperatura é a causa da elevação na venda de sorvetes.


# 8-Calcule as medidas de posição da variável salário.
str(data$salario)
salario <- as.double(salario)
summary(data$salario)


# 9-Calcule as medidas de dispersão da variável salário.
sd(data$salario)
var(data$salario)


# 10 - Apresente o relatório em PowerPoint ou usando RMarkdown






