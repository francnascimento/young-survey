
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

#categorias
musicas <- c("Music","Slow.music", "Fast.music", "Dance","Folk","Country","Classical.music","Musical","Pop","Rock","Metal.or.Hardrock","Punk","Hiphop..Rap","Reggae..Ska","Swing..Jazz","Rock.n.roll","Alternative","Latino","Techno..Trance","Opera")
filmes <- c("Movies","Horror","Thriller","Comedy","Romantic","Sci.fi","War","Fantasy.Fairy.tales","Animated","Documentary","Western","Action")
hobbysEInteresses <- c("History","Psychology","Politics","Mathematics","Physics","Internet","PC","Economy.Management","Biology","Chemistry","Reading","Geography","Foreign.languages","Medicine","Law","Cars","Art.exhibitions","Religion","Countryside..outdoors","Dancing","Musical.instruments","Writing","Passive.sport","Active.sport","Gardening","Celebrities","Shopping","Science.and.technology","Theatre","Fun.with.friends","Adrenaline.sports","Pets")
fobias <- c("Flying","Storm","Darkness","Heights","Spiders","Snakes","Rats","Ageing","Dangerous.dogs","Fear.of.public.speaking")
habitosAlimentares <- c("SmokingNumber", "AlcoholNumber", "Healthy.eating")
habitosDeCompra <- c("Finances","Shopping.centres","Branded.clothing","Entertainment.spending","Spending.on.looks","Spending.on.gadgets","Spending.on.healthy.eating")
personalidadeQuantitativo <- c("Daily.events","Prioritising.workload","Writing.notes","Workaholism","Thinking.ahead","Final.judgement","Reliability","Keeping.promises","Loss.of.interest","Friends.versus.money","Funniness","Fake","Criminal.damage","Decision.making","Elections","Self.criticism","Judgment.calls","Hypochondria","Empathy","Eating.to.survive","Giving","Compassion.to.animals","Borrowed.stuff","Loneliness","Cheating.in.school","Health","Changing.the.past","God","Dreams","Charity","Number.of.friends","Waiting","New.environment","Mood.swings","Appearence.and.gestures","Socializing","Achievements","Responding.to.a.serious.letter","Children","Assertiveness","Getting.angry","Knowing.the.right.people","Public.speaking","Unpopularity","Life.struggles","Happiness.in.life","Energy.levels","Small...big.dogs","Personality","Finding.lost.valuables","Getting.up","Interests.or.hobbies","Parents..advice","Questionnaires.or.polls")
personalidadeQualitativo  <- c("Punctuality","Lying","Internet.usage")
demograficosQuantitativos <- c("Age","Height","Weight","Number.of.siblings")
demograficosQualitativos  <- c("Gender","Left...right.handed","Education","Only.child","Village...town","House...block.of.flats")

print(nrow(survey))
for (row in 1:nrow(survey)) {
  survey[row, "Slow.music"] <- abs(survey[row, "Slow.songs.or.fast.songs"]-6)
  survey[row, "Fast.music"] <- survey[row, "Slow.songs.or.fast.songs"]
  
  switch(as.character(survey[row, "Smoking"]),"never smoked"={survey[row, "SmokingNumber"] <- 1},
         "tried smoking"={survey[row, "SmokingNumber"] <- 2},
         "former smoker"={survey[row, "SmokingNumber"] <- 4},
         "current smoker"={survey[row, "SmokingNumber"] <- 5},
         {survey[row, "SmokingNumber"] <- NaN}
  )
  
  switch(as.character(survey[row, "Alcohol"]),"never"={survey[row, "AlcoholNumber"] <- 1},
         "social drinker"={survey[row, "AlcoholNumber"] <- 3},
         "drink a lot"={survey[row, "AlcoholNumber"] <- 5},
         {survey[row, "SmokingNumber"] <- NaN}
  )
}
survey <- survey[complete.cases(survey),] #limpa missing data

#Matrizes de correlação
corrplot(cor(survey[musicas]),diag=FALSE)
corrplot(cor(survey[filmes]))
corrplot(cor(survey[hobbysEInteresses]))
corrplot(cor(survey[habitosAlimentares]))
corrplot(cor(survey[fobias]))
corrplot(cor(survey[habitosDeCompra]))
corrplot(cor(survey[personalidadeQuantitativo]))
corrplot(cor(survey[demograficosQuantitativos]))

#INÍCIO - Análise Fatorial
#Etapas
#1 - Análise de Matriz de correlação e Adequação da utilizaçãoda AF
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
summary(survey)
sd(survey)

#Análise dos componentes principais
fit <- princomp(survey,cor=TRUE)
#cor = TRUE, os componentes principais serão gerados a partir da matriz de correlação
#caso cor = FALSE, os componentes principais serão gerados a partir da matriz de covariância

#Apresentação das variâncias explicadas por componentes e variância explicada acumulada
summary(fit)
#É recomendado que o percentual de variância explicada seja de pelo menos 60%
#Elevando-os ao quadrado, teremosa variância dos valores dos fatores
#Deve-se escolher os fatores com os valores maiores que 1

#Apresentação das Cargas Fatoriais considerando todos os vetores
#Na função loadings podemos acrescentar a opção cutoff = 0.5, para que os loadings < 0.5 fiquem ocultos
loadings(fit)

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
#Teste de efericidade de Barlett
#Hipotese nula(H0): matriz de correlação entre variáveis é a matriz identidade.
#ou seja, variáveis não são correlacionadas
cortest.bartlett(survey[])

#Se H0 é rejeitada, a análse fatorial é valida
# p-value pequeno - rejeita-se H0, logo pode-se aplicar a análise fatorial


#Extração de dados com rotação pelo método VARIMAX
#Com a rotação, passamos a trabalhar com uma matriz mais simples, facilitando a interpretação dos valores
#O método de rotação mais utilizado é processo VARIMAX,
#que se trata de um método ortogonal de rotação que minimiza o número de variáveis com altas cargas sobre
#um fator, reforçando assim a interpretabilidade dos fatores
fit <- principal(survey[], nfactors=3, rotate="varimax", scores=TRUE)
fit

#plot factor 1 by factor 2
load <- fit$loadings
plot(load, type="n")
text(load, labels=names(survey[]),cex = 7) #adiciona nome das variáveis
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



#cluster
kmeans(survey[filmes], 3)
distancias = dist(survey[filmes])
h = hclust(distancias)
plot(h)
rect.hclust(h, 4)
