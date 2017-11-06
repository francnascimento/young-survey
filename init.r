
library(corrplot) #para matriz de correlacao

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
corrplot(cor(survey[musicas]))
corrplot(cor(survey[filmes]))
corrplot(cor(survey[hobbysEInteresses]))
corrplot(cor(survey[habitosAlimentares]))
corrplot(cor(survey[fobias]))
corrplot(cor(survey[habitosDeCompra]))
corrplot(cor(survey[personalidadeQuantitativo]))
corrplot(cor(survey[demograficosQuantitativos]))

