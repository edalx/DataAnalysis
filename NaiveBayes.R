install.packages("twitteR")
install.packages("base64enc")
install.packages("NLP")
install.packages("tm")
install.packages("SnowballC")
install.packages("stringr")
install.packages("dplyr")
install.packages("e1071")
library(twitteR)
library(base64enc)
library(NLP)
library(tm)
library(SnowballC)
library(stringr)
library(dplyr)
library(e1071)
library(dismo)
library(maps)
library(ggplot2)

consumer_key="O5rc4ys9mt5sA5tf8AxGvT1Xp"
consumer_secret="0NZyAEBZx8ipK3jhMeCeeqTM7YCTwD3eQke3ceFebBjVWc4ncA"
access_token="564881222-rywwhfbaFNDZGkLBrqkojgkiZeByWNGCJ6GUPTxe"
access_secret="5Y2XamHUK0pFECccYpjU5OebZNzoTGhCbsun2kZ76Q7ep"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#Carga de twitters
eventos.e3 <- searchTwitter("#c",n=1000,since="2017-05-01",until='2017-07-17', lang = "es")
eventos.xbox<- searchTwitter("#XboxE3", n=1000,since="2017-05-01",until='2017-07-12', lang = "es")
eventos.nintendo<-searchTwitter("#NintendoE3", n=1000,since="2017-05-01",until='2017-07-12', lang = "es")
eventos.playstation<-searchTwitter("#SonyE3", n=1000,since="2017-05-01",until='2017-07-12', lang = "es")
marcas.xbox <- searchTwitter('Xbox_Spain',n=1000,since="2017-05-01",until='2017-07-12', lang = "es")
marcas.nintendo <- searchTwitter('NintendoES',n=1000,since="2017-05-01",until='2017-07-12', lang = "es")
marcas.playstation <-searchTwitter("PlayStationES",n=1000,since="2017-05-01",until='2017-07-12',lang = "es")
marcas.ubisoft <-searchTwitter("Ubisoft_Spain",n=1000,since="2017-05-01",until='2017-07-12', lang = "es")
marcas.ElectronicArts <-searchTwitter("EA_Espana",n=1000,since="2017-05-01",until='2017-07-12', lang = "es")
marcas.Betsheda <-searchTwitter("bethesda_ESP",n=1000,since="2017-05-01",until='2017-07-12', lang = "es")
marcas.IGN <-searchTwitter("IGN_es",n=1000,since="2017-05-01",until='2017-07-12', lang = "es")
marcas.3DJuegos <-searchTwitter("3djuegos",n=1000,since="2017-05-01",until='2017-07-12', lang = "es")
marcas.MundoGamers <-searchTwitter("Mundogamers",n=1000,since="2017-05-01",until='2017-07-12', lang = "es")
marcas.VandalOnline <-searchTwitter("VandalOnline",n=1000,since="2017-05-01",until='2017-07-12', lang = "es")

#Poner los twitters extraidos en un solo vector
allGames<-c(eventos.e3, eventos.xbox,eventos.nintendo,eventos.playstation ,marcas.xbox,marcas.nintendo,marcas.playstation,marcas.ubisoft,marcas.ElectronicArts, marcas.Betsheda, marcas.IGN,marcas.3DJuegos,
            marcas.MundoGamers,marcas.VandalOnline )
df = twListToDF(allGames)
#Deteminar el tamaño de la muestra
N<-nrow(df)
N
P<-0.5
Q<-0.5
alpha<-0.95
error<-0.05
zm<-1.96
n=((zm)^2*N*P*Q)/((N-1)*(error)^2+(zm)^2*P*Q)
n

sentimiento=vector(mode = 'character', length = n)

twitterTest <- sample(N, n)

#Extracci�n de la muestra para entrenamiento nbayes
dataTest <- df$text[twitterTest]
dataPredict<-df$text[-twitterTest]
length(dataTest)
length(dataPredict)

#Lectura y calificaci�n del twitter
sentimiento[1]<-"1"
sentimiento[2]<-"1"
sentimiento[3]<-"1"
sentimiento[4]<-"1"
sentimiento[5]<-"1"
sentimiento[6]<-"0"
sentimiento[7]<-"0"
sentimiento[8]<-"0"
sentimiento[9]<-"-1"
sentimiento[10]<-"1"
sentimiento[11]<-"1"
sentimiento[12]<-"1"
sentimiento[13]<-"0"
sentimiento[14]<-"0"
sentimiento[15]<-"1"
sentimiento[16]<-"0"
sentimiento[17]<-"0"
sentimiento[18]<-"0"
sentimiento[19]<-"1"
sentimiento[20]<-"1"

dataBind<-rbind(dataTest,sentimiento)
mDatos<-t(dataBind)

head(txtclean)

txtclean=str_replace_all(dataTest,"[^[:graph:]]", " ")
# remueve retweets
txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txtclean, ignore.case = T)
# remove @otragente
txtclean = gsub("@\\w+", "", txtclean)
# remueve simbolos de puntuaci�n
txtclean = gsub("[[:punct:]]", "", txtclean)
# remueve links
txtclean = gsub("http:\\w+", "", txtclean)
txtclean = gsub("https:\\w+", "", txtclean)


corpus<-Corpus(VectorSource(txtclean),readerControl = list(language = "es"))
length(corpus)
content(corpus[[15]])


corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus, removePunctuation)
corpus<-tm_map(corpus, removeWords, c(stopwords("spanish")))
# remove espacios en blanco extras
corpus = tm_map(corpus, stripWhitespace)

#Convierte los tweets en matrices 1 0 con las palabras que aparecen en cada uno de ellos
frequencies<-DocumentTermMatrix(corpus)

inspect(frequencies[1:20,])

#Encontramos las palabras que se mencionan al menos 2 veces en los tweets
findFreqTerms(frequencies, lowfreq = 2)

#Quitamos las palabras que menos se mencionan en los tweets
sparse<-removeSparseTerms(frequencies, 0.95)
sparse

tweetsSparse<-as.data.frame(as.matrix(sparse))

colnames(tweetsSparse)=make.names(colnames(tweetsSparse))

#Unimos la variable con el sentimiento previamente clasificado
tweetsSparse$sentimiento<-sentimiento

table(tweetsSparse$sentimiento)


model <- naiveBayes(as.factor(sentimiento) ~ ., data = tweetsSparse)

summary(model)

pDatos<-limpiar(dataPredict)

predictBayes<-predict(model, pDatos)

summary(predictBayes)
