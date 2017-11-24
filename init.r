
library(corrplot) #para matriz de correlacao
library(psych)
library(GPArotation)

surveyOriginal = read.table("responses.csv",sep=",",header=TRUE)

# preparacao dos dados
survey <- surveyOriginal[complete.cases(surveyOriginal),] #limpa missing data
#colnames(survey)
#transformando coluna Slow.songs.or.fast.songs em duas
survey["Slow music"] <- NULL
survey["Fast music"] <- NULL
survey["SmokingNumber"] <- NULL
survey["AlcoholNumber"] <- NULL
survey["Atrasado"] <- NULL
survey["QuantidadeMentiras"] <- NULL
survey["NivelEducacao"] <- NULL
survey['TempoGastoInternet'] <- NULL

#categorias
musicas <- c("Dance","Folk","Country","Classical.music","Musical","Pop","Rock","Metal.or.Hardrock","Punk","Hiphop..Rap","Reggae..Ska","Swing..Jazz","Rock.n.roll","Alternative","Latino","Techno..Trance","Opera")
filmes <- c("Movies","Horror","Thriller","Comedy","Romantic","Sci.fi","War","Fantasy.Fairy.tales","Animated","Documentary","Western","Action")
hobbysEInteresses <- c("History","Psychology","Politics","Mathematics","Physics","Internet","PC","Economy.Management","Biology","Chemistry","Reading","Geography","Foreign.languages","Medicine","Law","Cars","Art.exhibitions","Religion","Countryside..outdoors","Dancing","Musical.instruments","Writing","Passive.sport","Active.sport","Gardening","Celebrities","Shopping","Science.and.technology","Theatre","Fun.with.friends","Adrenaline.sports","Pets")
fobias <- c("Flying","Storm","Darkness","Heights","Spiders","Snakes","Rats","Ageing","Dangerous.dogs","Fear.of.public.speaking")
habitosAlimentares <- c("SmokingNumber", "AlcoholNumber", "Healthy.eating")
habitosDeCompra <- c("Finances","Shopping.centres","Branded.clothing","Entertainment.spending","Spending.on.looks","Spending.on.gadgets","Spending.on.healthy.eating")
personalidadeQuantitativo <- c("Daily.events","Prioritising.workload","Writing.notes","Workaholism","Thinking.ahead","Final.judgement","Reliability","Keeping.promises","Loss.of.interest","Friends.versus.money","Funniness","Fake","Criminal.damage","Decision.making","Elections","Self.criticism","Judgment.calls","Hypochondria","Empathy","Eating.to.survive","Giving","Compassion.to.animals","Borrowed.stuff","Loneliness","Cheating.in.school","Health","Changing.the.past","God","Dreams","Charity", "QuantidadeMentiras", "Atrasado","Number.of.friends","Waiting","New.environment","Mood.swings","Appearence.and.gestures","Socializing","Achievements","Responding.to.a.serious.letter","Children","Assertiveness","Getting.angry","Knowing.the.right.people","Public.speaking","Unpopularity","Life.struggles","Happiness.in.life","Energy.levels","Small...big.dogs","Personality","Finding.lost.valuables","Getting.up","Interests.or.hobbies","Parents..advice","Questionnaires.or.polls", "TempoGastoInternet")
personalidadeQualitativo  <- c("Punctuality","Lying","Internet.usage")
demograficosQuantitativos <- c("Age","Height","Weight","Number.of.siblings", "NivelEducacao")
demograficosQualitativos  <- c("Gender","Left...right.handed","Education","Only.child","Village...town","House...block.of.flats")

variaveisQuantitativas <- c(musicas, filmes, hobbysEInteresses, fobias, habitosAlimentares, habitosDeCompra, habitosDeCompra, demograficosQuantitativos, personalidadeQuantitativo)
variaveisQualitativas <-c(personalidadeQualitativo, demograficosQualitativos)

print(nrow(survey))
for (row in 1:nrow(survey)) {
  survey[row, "Slow.music"] <- abs(survey[row, "Slow.songs.or.fast.songs"]-6)
  survey[row, "Fast.music"] <- survey[row, "Slow.songs.or.fast.songs"]
  
  switch(as.character(survey[row, "Smoking"]),
         "never smoked"={survey[row, "SmokingNumber"] <- as.numeric(1)},
         "tried smoking"={survey[row, "SmokingNumber"] <- as.numeric(2)},
         "former smoker"={survey[row, "SmokingNumber"] <- as.numeric(3)},
         "current smoker"={survey[row, "SmokingNumber"] <- as.numeric(4)},
         {survey[row, "SmokingNumber"] <- NA}
  )
  
  switch(as.character(survey[row, "Alcohol"]),
         "never"={survey[row, "AlcoholNumber"] <- as.numeric(1)},
         "social drinker"={survey[row, "AlcoholNumber"] <- as.numeric(2)},
         "drink a lot"={survey[row, "AlcoholNumber"] <- as.numeric(3)},
         {survey[row, "SmokingNumber"] <- NA}
  )
  
  switch(as.character(survey[row, "Punctuality"]),
         "i am often early"={survey[row, "Atrasado"] <- as.numeric(1)},
         "i am always on time"={survey[row, "Atrasado"] <- as.numeric(2)},
         "i am often running late"={survey[row, "Atrasado"] <- as.numeric(3)},
         {survey[row, "Atrasado"] <- NA}
  )
  
  switch(as.character(survey[row, "Lying"]),
         "never"={survey[row, "QuantidadeMentiras"] <- as.numeric(1)},
         "only to avoid hurting someone"={survey[row, "QuantidadeMentiras"] <- as.numeric(2)},
         "sometimes"={survey[row, "QuantidadeMentiras"] <- as.numeric(3)},
         "everytime it suits me"={survey[row, "QuantidadeMentiras"] <- as.numeric(4)},
         {survey[row, "QuantidadeMentiras"] <- NA}
  )
  
  switch(as.character(survey[row, "Internet.usage"]),
         "no time at all"={survey[row, "TempoGastoInternet"] <- as.numeric(1)},
         "less than an hour a day"={survey[row, "TempoGastoInternet"] <- as.numeric(2)},
         "few hours a day"={survey[row, "TempoGastoInternet"] <- as.numeric(3)},
         "most of the day"={survey[row, "TempoGastoInternet"] <- as.numeric(4)},
         {survey[row, "TempoGastoInternet"] <- NA}
  )
  
  switch(as.character(survey[row, "Education"]),
         "primary school"={survey[row, "NivelEducacao"] <- as.numeric(1)},
         "secondary school"={survey[row, "NivelEducacao"] <- as.numeric(2)},
         "college/bachelor degree"={survey[row, "NivelEducacao"] <- as.numeric(3)},
         "masters degree"={survey[row, "NivelEducacao"] <- as.numeric(4)},
         {survey[row, "NivelEducacao"] <- NA}
  )
}
survey <- survey[complete.cases(survey),] #limpa missing data
print(nrow(survey))

surveyQuantitativo <- survey[,variaveisQuantitativas] #DataSet contendo somente colunas quantitativas

#Matrizes de correlação
corrplot(cor(survey[musicas]), type = "lower", tl.srt = 45, diag = FALSE)
corrplot(cor(survey[filmes]), type = c("lower"), tl.srt = 45, diag = FALSE)
corrplot(cor(survey[hobbysEInteresses]), type = c("lower"), tl.srt = 45, diag = FALSE)
corrplot(cor(survey[habitosAlimentares]), type = c("lower"), tl.srt = 45, diag = FALSE)
corrplot(cor(survey[fobias]), type = c("lower"), tl.srt = 45, diag = FALSE)
corrplot(cor(survey[habitosDeCompra]), type = c("lower"), tl.srt = 45, diag = FALSE)
corrplot(cor(survey[personalidadeQuantitativo]), type = c("lower"), tl.srt = 45, diag = FALSE)
corrplot(cor(survey[demograficosQuantitativos]), type = c("lower"), tl.srt = 45, diag = FALSE)


#INÍCIO - Análise Fatorial
#Etapas
#1 - Análise de Matriz de correlação e Adequação da utilização da AF
#2 - Extração dos fatores iniciais e Escolha dos numeros dos fatores
#3 - Rotação dos Fatores
#4 - Interpretação dos Fatores

#1 - Etapa - Adequação da utilização da AF
#Analisar Matriz de correlação (>0.30)
#aplicar estatistica KMO
#*compara correlações simples com parcial [0:1] (KMO próx. a 1)
#-Teste de esfericidade de Barlett
#*H0: Matriz correlação = Matriz identidade (H0 rejeitada)
#Análise de Matriz anti-imagem
#*Valores negativo das correlações parciais. (Maior mais aceitável)

#2 - Etapa - Extração dos fatores iniciais
#- Método de Extração
#*ACP - análise de componentes principais
#Variância comum: comunalidade

#2 - Etapa - Escolha do numero de fatores
#Possíveis critérios:
#-Critério Raiz latente (Critério de Kaiser)
#* valores > 1
#-Critério a priori ( é determinado o numero de fatores pelo pesquisador)
#-Critério da percentagem da variância (Nível significativo)
#-Critério do gráfico Scree
#* plotagem das raizes latentes(Y) e fatores(X).
#* parte mais horizontal limita a quantidade de fatores

#3 - Etapa - Rotação dos fatores
#Transformar os componentes principais
#-Rotação Ortogonal e obliquo(Varimax)

#4 - Etapa - Interpretação do dados
#- Cargas maiores de 0.30

#Analise Descritiva
summary(surveyQuantitativo)
sd(surveyQuantitativo)

#Análise dos componentes principais
fit <- princomp(surveyQuantitativo,cor=TRUE)
#cor = TRUE, os componentes principais serão gerados a partir da matriz de correlação
#caso cor = FALSE, os componentes principais serão gerados a partir da matriz de covariância

#Apresentação das variâncias explicadas por componentes e variância explicada acumulada
summary(fit)
#É recomendado que o percentual de variância explicada seja de pelo menos 60%
#Elevando-os ao quadrado, teremosa variância dos valores dos fatores
#Deve-se escolher os fatores com os valores maiores que 1

#Apresentação das Cargas Fatoriais considerando todos os vetores
#Na função loadings podemos acrescentar a opção cutoff = 0.5, para que os loadings < 0.5 fiquem ocultos
loadings(fit, cutoff = 0.5)

#Para analisar a quantidade de fatores suficientes à análise fatorial, deve-se realizar o scree plot
plot(fit, type = "line")
#onde deve-se observar o ponto na qual é atingido o padrão mais horizontal

#Cálculo dos scores das observações nos componentes principais
fit$scores

#Com o comando "biplot", consegue-se ver a angulação entre cada variável;
#a correlação entre as variáveis é dada pelo cosseno do ângulo que elas formam.
#Quanto maior a correlação entre duas variáveis, maior a probabilidade de existirem fatores comuns a elas.
biplot(fit) #consegue-se ver a angulação entre cada variável

##Análise fatorial a partir do pacote Psych
#Análise de adequação
#Teste de esferacidade de Barlett
#Hipotese nula(H0): matriz de correlação entre variáveis é a matriz identidade.
#ou seja, variáveis não são correlacionadas
cortest.bartlett(surveyQuantitativo)

#Se H0 é rejeitada, a análise fatorial é valida
# p-value pequeno - rejeita-se H0, logo pode-se aplicar a análise fatorial


#Extração de dados com rotação pelo método VARIMAX
#Com a rotação, passamos a trabalhar com uma matriz mais simples, facilitando a interpretação dos valores
#O método de rotação mais utilizado é processo VARIMAX,
#que se trata de um método ortogonal de rotação que minimiza o número de variáveis com altas cargas sobre
#um fator, reforçando assim a interpretabilidade dos fatores
fit <- principal(surveyQuantitativo, nfactors=6, rotate="varimax", scores=TRUE)
fit

#plot factor 1 by factor 2
load <- fit$loadings
plot(load)
text(load, labels=names(survey[]),cex = .4) #adiciona nome das variáveis
load

#Os resultados mostram os valores dos fatores para cada variavel, os autovalores
#(SS loadings) e a proporção de variância acumulada
#Outras opções para o parametro rotate(), qunatimax, promax, oblimin, simplin e cluster.

### Calculo KMO
#O calculo de KMO testa se as correlações simples são iguais as correlações parciais observada entre as variáveis.
#A correlação parcial entre duas variáveisé a correlação que existe entre duas variáveis que também
#se apresentam correlacionadas com as primeiras.
#Os coeficientes de correlação parcial entre pares de variáveis devem ser pequenos se estas partilharem fatores comuns

##Um KMO maior que 0.5 justifique a utilização da Análise Fatorial
##Logo nessa análise o KMO é igual a o.6812106, sendo justificado a análise fatorial.

#FIM - Análise Fatorial


#tests: https://www.statmethods.net/advstats/factor.html
surveyMusic=surveyQuantitativo[,musicas]
surveyMusicDem=surveyQuantitativo[,c(musicas,demograficosQuantitativos)]
surveyInteresses = surveyQuantitativo[,hobbysEInteresses]
surveyInteressesDem = surveyQuantitativo[, c(hobbysEInteresses, demograficosQuantitativos)]
surveyFinal = surveyQuantitativo[, personalidadeQuantitativo]
# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(surveyFinal)) # get eigenvalues
ap <- parallel(subject=nrow(surveyFinal),var=ncol(surveyFinal),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# hothorn cap.5 -- determine number of factors by p-value
sapply(1:10, function(f) +
      factanal(surveyFinal, factors = f, method ="mle", rotation = "varimax")$PVAL)

factanal(surveyFinal, factors = 3, method ="mle", rotation = "varimax")

#função hothorn cap.5
pfun <- function(nf, matrix) {
  matrixCovariance <- cor(matrix)
  fa <- factanal(covmat = matrixCovariance, factors = nf, method = "mle", n.obs = nrow(matrix))
  est <- tcrossprod(fa$loadings) + diag(fa$uniquenesses)
  ret <- round((matrixCovariance - est), 2)
  colnames(ret) <- rownames(ret) <- abbreviate(rownames(ret), 3)
  ret
}
pfun(3, surveyFinal)

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit <- factanal(surveyFinal, 9, rotation="varimax", method="mle")
print(fit, digits=2, cutoff=.1, sort=TRUE)
# plot factor 1 by factor 2 

test <- fit$loadings[, 7:9]
print(sort(test))

load <- fit$loadings[,4:5]
plot(load,type="n", col="black") # set up plot 
text(load,labels=names(surveyFinal),cex=.9, col = "black") # add variable names

# Principal Axis Factor Analysis
library(psych)
fit <- factor.pa(surveyFinal, nfactors=9)
fit # print results

# PCA Variable Factor Map 
install.packages("FactoMineR")
library(FactoMinerR)
result <- PCA(surveyFinal) # graphs generated automatically

library(sem)
surveyFinal.cov <- cov(surveyFinal)
model.surveyFinal <- specify.model()

# funcao que determina os fatores de uma amostra, dado as cargas
determinaFator <- function(variaveis, loads) {
  fatores <- array(dim=ncol(loads))
  for(i in 1:ncol(loads)) {
    fatores[i] <- sum(variaveis * loads[ ,i])
  }
  
  return (fatores)
}

# analise fatorial de n fatores
fa <- factanal(surveyFinal, factors = 2, rotation="varimax", method = "mle")
loads <- (fa$loadings)

# para cada amostra, determina seus fatores
fatores <- list()
for(i in 1:nrow(surveyFinal)) {
  fatores[[i]] <- determinaFator(surveyFinal[i,], loads)
}

# converte a lista de listas em uma matriz
# adaptado de: https://stackoverflow.com/questions/43425300/list-of-lists-to-matrix
max_length <- max(unlist(lapply (fatores, FUN = length)))
fatores <- sapply (fatores, function (x) {length (x) <- max_length; return (x)})
fatores <- t(fatores)

# http://motioninsocial.com/tufte/#dot-dash-plot
# plotta os fatores
library(lattice)
x <- fatores[,1]
y <- fatores[,2]
xyplot(y ~ x, xlab="Fator 1", ylab="Fator 2",
       par.settings = list(axis.line = list(col="transparent")),
       panel = function(x, y,...) { 
         panel.xyplot(x, y, col=1, pch=1, cex=.7)
         panel.rug(x, y, col=1, x.units = rep("snpc", 2), y.units = rep("snpc", 2), ...)})
