## SMS Text Mining
#Installing Packages
install.packages("DBI")
install.packages("RMySQL")
install.packages("NLP")
install.packages("tm") 
install.packages("qdap")
install.packages("reshape")
install.packages("stringr")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("syuzhet")
install.packages("lubridate")
install.packages("qdapRegex")
install.packages( "bnlearn")

ptm <- proc.time()
#Loading Packages
library(DBI)
library(RMySQL)
library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(qdap)
library(ggplot2)
library(syuzhet)
library(openNLPdata)
library(rJava)
library(openNLP)
library(dtt)
getwd()
.jinit()

#Reading NUS data set from a SQL DB
DBD = dbDriver("MySQL");
setcon2db = dbConnect(DBD,user='root',password='MAVE88rick!(88',host='localhost',dbname='sms');
impodf = dbSendQuery(setcon2db, "select * from new_sms_download")
nussms = fetch(impodf, n = -1)
View(nussms)

## SMS content column as data frame from the 
smscontent = as.data.frame(nussms$content)
colnames(smscontent) = "SMS"
View(smscontent)
class(smscontent)
class(smscontent$SMS)
summary(smscontent$SMS)

#Removing duplicated sms using unique
smscontent = unique(smscontent)
summary(smscontent$SMS)

#Preprocessing 1

#Check for NUS Annoymization Codes
summary(grepl("<EMAIL>",smscontent$SMS))
summary(grepl("<URL>",smscontent$SMS))
summary(grepl("<IP>",smscontent$SMS))
summary(grepl("<TIME>",smscontent$SMS))
summary(grepl("<DATE>",smscontent$SMS))
summary(grepl("<DECIMAL>",smscontent$SMS))
summary(grepl("<#>",smscontent$SMS))

#Removing NUS Annoymization Cods
smscontent$SMS = gsub("<EMAIL>", "", smscontent$SMS)
smscontent$SMS = gsub("<URL>", "", smscontent$SMS)
smscontent$SMS = gsub("<TIME>", "", smscontent$SMS)
smscontent$SMS = gsub("<DATE>", "", smscontent$SMS)
smscontent$SMS = gsub("<DECIMAL>", "", smscontent$SMS)
smscontent$SMS = gsub("<#>", "", smscontent$SMS)
                

###!1 Handling smiles punctuations or 2 ==>
##########################################
#summary(grepl(":c",smscontent$SMS))
#library(qdapRegex)
#rm_emoticon(x)
#emo = ex_emoticon(smscontent$SMS)
#class(emo)
#e = do.call(rbind, lapply(emo, data.frame, stringsAsFactors=FALSE))
#colnames(e)[1] = "Code"
#dim(e)
#View(e)
#class(e$Code)
#e=na.omit(e)
#e = unique(e)
#write.csv(e,"e.csv")
#e$Code=as.factor(e$Code)
#summary(e$Code)
#write.csv(e,"emo.csv")
#e1=read.csv("emo.csv")
#smid = read.table("smid.txt",sep = ",")
#smid
#e1
#smid=read.csv("smid.txt", header = T, sep = ",")
#######
#d = read.csv("PE.csv")
##########################################

#2 Otherwise / removing punctuations
#summary(grepl("[:punct:]",smscontent$SMS))
#smscontent$SMS = gsub("[:punct:]", "", smscontent$SMS)
#summary(grepl("[:punct:]",smscontent$SMS))

#Removing numbers
#summary(grepl("\\d",smscontent$SMS))
#smscontent$SMS = gsub("\\d", "", smscontent$SMS)
#summary(grepl("\\d",smscontent$SMS))

#summary(grepl("[^a-zA-Z\\s]",smscontent$SMS))
#smscontent$SMS = gsub("[^a-zA-Z\\s]", "", smscontent$SMS)

##Preprocessing 2 
# Remove everything that is not a number or letter
#smscontent$SMS = stringr::str_replace_all(smscontent$SMS,"[^a-zA-Z\\s]", " ")


#Convert SMS Content to vector source then create corpus
sms.vec = VectorSource(smscontent)     
sms.corpus = Corpus(sms.vec)
summary(sms.corpus) 
inspect(sms.corpus)

#Preprocessing 3
#Change to lower case
sms.corpus= tm_map(sms.corpus, tolower)
writeLines(as.character(sms.corpus[1]))

#Remove all numbers 
sms.corpus = tm_map(sms.corpus, removeNumbers)

#Remove whitespace
sms.corpus = tm_map(sms.corpus, stripWhitespace)

#Stemming process
sms.corpus = tm_map(sms.corpus, stemDocument)

# Removing stop words
sms.corpus = tm_map(sms.corpus, removeWords, stopwords("english"))
# Plain Text Document
sms.corpus = tm_map(sms.corpus, PlainTextDocument)

#English Stop Words (other resourse)
stopWords = read.csv("esw.txt")
sms.corpus = tm_map(sms.corpus, removeWords, unlist(stopWords))

#Remove special stop words 
special_stop_words = read.csv("ssw.txt")
sms.corpus = tm_map(sms.corpus, removeWords, unlist(special_stop_words))

#Steam Document
sms.corpus = tm_map(sms.corpus, stemDocument)

# Strip is from qdap function to remove punctuation, can use char.keep="???"
sms.corpus = tm_map(sms.corpus, content_transformer(strip))





#Visualizing using Wordcloud
wordcloud(sms.corpus, max.words = 500, random.order = FALSE,rot.per=0.35, use.r.layout=FALSE,colors=brewer.pal(8,"Dark2"))


# Document Term Matrix
dtm = DocumentTermMatrix(sms.corpus)
#Finding the high frequency word
#findFreqTerms(dtm, 6592)
findFreqTerms(dtm, highfreq = Inf)

# Convert Document Term Matrix to Matrix for easy manipulation
dtm.matrix = as.matrix(dtm)
#Finding top 100 words with high frequency.
freq = colSums(dtm.matrix)
freq = sort(freq, decreasing = TRUE)
write.csv(head(freq,100),file = "wordfreq.txt")
wordfreq = read.csv("wordfreq.txt")
colnames(wordfreq) = c("Word","Frequency")
View(wordfreq)


############################################
wordfreq.vec = VectorSource(wordfreq$Word)     
wordfreq.corpus = Corpus(wordfreq.vec)
summary(wordfreq.corpus) 
inspect(wordfreq.corpus)

#### ploting top 50 for the whole dataset
library(ggplot2)   
top300 = ggplot(subset(wordfreq, freq>300), aes(Word, Frequency))    
top300 = top300 + geom_bar(stat="identity")   
top300 = top300 + theme(axis.text.x=element_text(angle=45, hjust=1))   
top300

######
set.seed(142)   
wordcloud(names(freq), freq, min.freq=100, scale=c(8, .1), random.order = FALSE,rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))  



########################################################################
oldsmscontent = smscontent                                             #
View(oldsmscontent)                                                    #
########################################################################
#install.packages("RSentiment")                                        #
#library(RSentiment)                                                   #
                                                                      #
#calculate_score(wordfreq)                                             #
                                                                      #
# classify emotion                                                    #
#class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)  #
# get emotion best fit                                                #
#emotion = class_emo[,7]                                               #
# substitute NA's by "unknown"                                        #
#emotion[is.na(emotion)] = "unknown"                                   #
                                                                      #
# classify polarity                                                   #
#class_pol = classify_polarity(some_txt, algorithm="bayes")            #
# get polarity best fit                                               #
#polarity = class_pol[,4]                                              #
#######################################################################


#####################################################################SMSCONTENTCHANGED>>>>>


# Connvert sms.corpus to data frame
mc = data.frame(text = sapply(sms.corpus, as.character), stringsAsFactors = FALSE)
colnames(mc) = "SMS"
View(mc)
class(mc$SMS)

# Removing blank rows
write.csv(mc,"mc.csv")
mc1 = read.csv("mc.csv", na.strings = c("", "NA"))
View(mc1)
mc1$X = NULL
mc1 = na.omit(mc1)
View(mc1)
smscontent = mc1
View(smscontent)
class(smscontent$SMS)
smscontent$SMS = as.character(smscontent$SMS)

#Sentiment

   
# Apply sentiment analysis (this is based on NRC Word-Emotion Association Lexicon by Saif Mohammed and Peter Turney.
sentiment = get_nrc_sentiment(smscontent$SMS)
View(sentiment)
head(sentiment)
dim(sentiment)

#Removing Negative, Neutral and Positive classes
sentiment$anticipation = NULL
sentiment$negative = NULL
sentiment$positive = NULL
sentiment$surprise = NULL

head(sentiment)

colnames(sentiment) = c("Anger", "Disgust" , "Fear", "Joy" , "Sadness" , "Trust")
head(sentiment)

smssentimentcontent = cbind(smscontent, sentiment)
View(smssentimentcontent)

allsentiment = data.frame(colSums(smssentimentcontent[, c(2:7)]))
View(allsentiment)
names(allsentiment) = "count"
ALLSentiment = allsentiment  
ALLSentiment = cbind("sentiment" = rownames(allsentiment), allsentiment)
View(ALLSentiment)
rownames(ALLSentiment) = NULL
View(ALLSentiment)


## Ploting Sentiment
ggplot(data = ALLSentiment,  aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All SMS")

head(smssentimentcontent)


#Assigning Sentiment for each SMS
smssentimentcontent$Sentiment = ifelse(smssentimentcontent$Anger >= 1, "Anger", "Other")
Anger = as.data.frame(smssentimentcontent[which(smssentimentcontent$Sentiment == "Anger"), ])
Anger = Anger[, -c(2:7)]
View(Anger)
dim(Anger)

smssentimentcontent$Sentiment = ifelse(smssentimentcontent$Disgust >= 1, "Disgust", "Other")
Disgust = as.data.frame(smssentimentcontent[which(smssentimentcontent$Sentiment == "Disgust"), ])
Disgust = Disgust[, -c(2:7)]
View(Disgust)
dim(Disgust)

smssentimentcontent$Sentiment = ifelse(smssentimentcontent$Fear >= 1, "Fear", "Other")
Fear = as.data.frame(smssentimentcontent[which(smssentimentcontent$Sentiment == "Fear"), ])
Fear = Fear[, -c(2:7)]
View(Fear)
dim(Fear)

smssentimentcontent$Sentiment = ifelse(smssentimentcontent$Joy >= 1, "Joy", "Other")
Joy = as.data.frame(smssentimentcontent[which(smssentimentcontent$Sentiment == "Joy"), ])
Joy = Joy[, -c(2:7)]
View(Joy)
dim(Joy)

smssentimentcontent$Sentiment = ifelse(smssentimentcontent$Sadness >= 1, "Sadness", "Other")
Sadness = as.data.frame(smssentimentcontent[which(smssentimentcontent$Sentiment == "Sadness"), ])
Sadness = Sadness[, -c(2:7)]
View(Sadness)
dim(Sadness)

smssentimentcontent$Sentiment = ifelse(smssentimentcontent$Trust >= 1, "Trust", "Other")
Trust = as.data.frame(smssentimentcontent[which(smssentimentcontent$Sentiment == "Trust"), ])
Trust = Trust[, -c(2:7)]
View(Trust)
dim(Trust)


#smssentimentcontent$Sum = (smssentimentcontent$Anger+smssentimentcontent$Disgust+smssentimentcontent$Fear+smssentimentcontent$Joy+smssentimentcontent$Sadness+smssentimentcontent$Trust)

#smssentimentcontent$Sentiment = ifelse(smssentimentcontent$Sum == 0, "Neutral", "Unknown")
#Neutral = as.data.frame(smssentimentcontent[which(smssentimentcontent$Sentiment == "Neutral"), ])
#Neutral = Neutral[, -c(2:7)]
#View(Neutral)
#dim(Neutral)
#Neutral = NULL
#smssentimentcontent$Sum = NULL



#Row bind for all sentiment
scsms = rbind(Anger , Disgust , Fear , Joy , Sadness , Trust)
View(scsms)
dim(scsms)
###################################################################Done with Obs. ==> 1. 21268



#Sentiment
scsmss = scsms
View(scsmss)
rownames(scsmss) = paste("Doc", 1:nrow(scsmss))

#get freq count for each word 
with(scsmss,wfm(SMS,Sentiment))
with(scsmss,wfm(SMS))

#Create corpus from "scsmss" data frame
scsmss_tm = Corpus(DataframeSource(scsmss[,1,drop=FALSE]))

#Transformation
scsmss_tm = tm_map(scsmss_tm, stripWhitespace)
scsmss_tm = tm_map(scsmss_tm, removeWords, stopwords("english"))

#Stop Words other resourse
stopWords = read.csv("esw.txt")
scsmss_tm = tm_map(scsmss_tm, removeWords,  unlist(stopWords))
# Plain Text Document
scsmss_tm = tm_map(scsmss_tm, PlainTextDocument)

#Remove special Stop Words 
special_stop_words = read.csv("ssw.txt")
scsmss_tm = tm_map(scsmss_tm, removeWords, unlist(special_stop_words))

#Steam Document
scsmss_tm = tm_map(scsmss_tm, stemDocument)

# Strip is from qdap function to remove punctuation, can use char.keep="???"
scsmss_tm = tm_map(scsmss_tm, content_transformer(strip))

#Visualizing using Wordcloud
wordcloud(scsmss_tm, max.words = 100, random.order = FALSE,rot.per=0.35, use.r.layout=FALSE,colors=brewer.pal(8,"Dark2"))

#set.seed(142)   
#wordcloud(names(freq), freq, min.freq=50, scale=c(8, .1), random.order = FALSE,rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))  

class(scsmss_tm)


#create document term matrix
dtm2 = DocumentTermMatrix(scsmss_tm)
inspect(dtm2)

######!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
#Remove Sparse Terms
#dtm2 = removeSparseTerms(dtm, 0.9999)
#dtm2
#inspect(dtm2)
#dtm = dtm2
######!#!#!#!#!#!#!#!#!#!#!#!#!#!#!


#find frequent terms - change the frequency
dic_words = findFreqTerms(dtm2, 50)
#dic_words = findFreqTerms(dtm2, lowfreq = , highfreq = Inf)

#create dtm with specific set of words
dtm_dic = DocumentTermMatrix(scsmss_tm, list(dictionary=dic_words))
inspect(dtm_dic)

#Convert DTM to scsmss data frame
df.dtm_dic = as.data.frame.matrix(dtm_dic)
dim(df.dtm_dic)
View(df.dtm_dic)

#Set all columns to numeric class
final.df = df.dtm_dic
for(i in 1:ncol(final.df)){final.df[,i] = as.numeric(final.df[,i])}

#Set i,j>0 to "Yes" else to "No" .
for(i in 1:ncol(final.df)){
  for(j in 1:nrow(final.df)){
    ifelse(final.df[j,i]>0,final.df[j,i]<-"Yes",final.df[j,i]<-"No")
  }
}
View(final.df)

#######################!
#merge with specific column from other dataset
final.dfs = cbind(final.df, Sentiment = scsmss$Sentiment)  
final.dfsmss = cbind(SMS = scsmss$SMS, final.dfs )

View(final.dfsmss)
dim(final.dfsmss)
summary(final.dfsmss)



# Class conversion (character to factor) ==> to build Bayesian Network
final.df.factor = final.dfsmss

for(i in 1:ncol(final.df.factor)){final.df.factor[,i] = as.factor(final.df.factor[,i])}

class(final.df.factor$love)

#### Building BN "Bayesian Network"
library(bnlearn)
final.df.bn = final.df.factor
summary(final.df.bn)
View(final.df.bn)
dim(final.df.bn)

sn = hc(final.df.bn)
plot(sn)

##################################################
BN = final.df.bn
dim(BN)
View(BN)
BN$SMS = NULL
write.csv(BN,"BN.csv", row.names = FALSE)


#Total Time
total_time <- proc.time() - ptm
total_time
#############################################################################
R.home()

## GeNIe
library(rJava)
.jinit("C:/Users/Ahmed/Google Drive/backup/SQL")
.jinit()
net = .jnew("java/lang/String")
.jcall(obj = net, returnSig = "V", method = "readFile", "BN.xdsl")

help(".jcall")




#######################################################################################################################
check = final.df.bn
check$sentiment = NULL
check
##

res$arcs = res$arcs[-which((res$arcs[,'from'] == "M..Work" & res$arcs[,'to'] == "Family")),]

plot(res)

fittedbn = bn.fit(res, data = bn_df)
print(fittedbn$Proteins)

cpquery(fittedbn, event = (Proteins=="<3"), evidence = ( Smoking=="no") )
cpquery(fittedbn, event = (Proteins=="<3"), evidence = ( Smoking=="no" & Pressure==">140" ) )
cpquery(fittedbn, event = (Pressure==">140"), evidence = ( Proteins=="<3" ) )
###############################################################################################################################


#Double Check word sentiment
###############################################################################################################################
sss = read.csv("sss.txt")
head(sss)
class(sss$Words)
View(sss)
dim(sss)
sss = unique(sss)
dim(sss)
sss$Words = as.character(sss$Words)

# Apply sentiment analysis (this is based on NRC Word-Emotion Association Lexicon by Saif Mohammed and Peter Turney.
sentimentsss = get_nrc_sentiment(sss$Words)
View(sentimentsss)
head(sentimentsss)
dim(sentimentsss)

#Removing Negative, Neutral and Positive classes
sentimentsss$anticipation = NULL
sentimentsss$negative = NULL
sentimentsss$positive = NULL
sentimentsss$surprise = NULL

head(sentimentsss)

colnames(sentimentsss) = c("Anger", "Disgust" , "Fear", "Joy" , "Sadness" , "Trust")
head(sentimentsss)

smssentimentcontentsss = cbind(sss, sentimentsss)
View(smssentimentcontentsss)

allsentimentsss = data.frame(colSums(smssentimentcontentsss[, c(2:7)]))
View(allsentimentsss)
names(allsentimentsss) = "count"
ALLSentimentsss = allsentimentsss  
ALLSentimentsss = cbind("sentiment" = rownames(allsentimentsss), allsentimentsss)
View(ALLSentimentsss)
rownames(ALLSentimentsss) = NULL
View(ALLSentimentsss)


## Ploting Sentiment
library(ggplot2)
ggplot(data = ALLSentimentsss,  aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All SMS")

head(smssentimentcontent)
sentimentsss$Trust = as.factor(sentimentsss$Trust)
summary(sentimentsss$Trust)
class(sentimentsss$Anger)



#########################################################################

df = smssentimentcontentsss
head(df)
df$Sum <- df$Anger+df$Disgust+df$Fear+df$Joy+df$Sadness+df$Trust
df1 = as.data.frame(df[which(df$Sum >= 1), ])
View(df1)
df1 = unique(df1)
df1 = df1[order(df1$Words),]

df0 = as.data.frame(df[which(df$Sum == 0), ])
View(df0)
#df0 = unique(df0)
#df0 = df0[order(df0$Words),]
write.table(df0$Words,"ssw1.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)





#########################################################################

##Generate some data
dd = data.frame(a = 1:4, b= 1:0, c=0:3)
dd

##Go through each row and determine if a value is zero
row_sub = apply(dd, 1, function(row) all(row !=0 ))
##Subset as usual
dd[row_sub,]


CreateNetwork = function(){
  # Creating a new network object 
  net = .jnew("smile/Network")
  
  # Creating node "Success" and setting/adding outcomes
  .jcall(obj = net, returnSig = "I", method = "addNode", as.integer(18), "Success")
  .jcall(obj = net, returnSig = "V", method = "setOutcomeId", "Success", as.integer(0), "Success")
  .jcall(obj = net, returnSig = "V", method = "setOutcomeId", "Success", as.integer(1), "Failure")
  
  # Creating node "Forecast" and setting/adding outcomes:
  .jcall(obj = net, returnSig = "I", method = "addNode", as.integer(18), "Forecast")
  .jcall(obj = net, returnSig = "V", method = "addOutcome", "Forecast", "Good")
  .jcall(obj = net, returnSig = "V", method = "addOutcome", "Forecast", "Moderate")
  .jcall(obj = net, returnSig = "V", method = "addOutcome", "Forecast", "Poor")
  .jcall(obj = net, returnSig = "V", method = "deleteOutcome", "Forecast", as.integer(0))
  .jcall(obj = net, returnSig = "V", method = "deleteOutcome", "Forecast", as.integer(0))
  
  # Adding an arc from "Success" to "Forecast":
  .jcall(obj = net, returnSig = "V", method = "addArc", "Success", "Forecast")
  
  # Filling in the conditional distribution for node "Success". The 
  # probabilities are:
  # P("Success" = Success) = 0.2
  # P("Success" = Failure) = 0.8
  aSuccessDef = .jarray(c(0.2, 0.8), "java/lang/Double")
  .jcall(obj = net, returnSig = "V", method = "setNodeDefinition", "Success", aSuccessDef)
  
  # Filling in the conditional distribution for node "Forecast". The 
  # probabilities are:
  # P("Forecast" = Good | "Success" = Success) = 0.4
  # P("Forecast" = Moderate | "Success" = Success) = 0.4
  # P("Forecast" = Poor | "Success" = Success) = 0.2
  # P("Forecast" = Good | "Success" = Failure) = 0.1
  # P("Forecast" = Moderate | "Success" = Failure) = 0.3
  # P("Forecast" = Poor | "Success" = Failure) = 0.6
  aForecastDef = .jarray(c(0.4, 0.4, 0.2, 0.1, 0.3, 0.6), "java/lang/Double")
  .jcall(obj = net, returnSig = "V", method = "setNodeDefinition", "Forecast", aForecastDef)
  
  # Changing the nodes' spacial and visual attributes
  .jcall(obj = net, returnSig = "V", method = "setNodePosition", "Success", 
         as.integer(20), as.integer(20), as.integer(80), as.integer(30))
  
  .jcall(obj = net, returnSig = "V", method = "setNodeBorderWidth", "Success", as.integer(2))
  .jcall(obj = net, returnSig = "V", method = "setNodePosition", "Forecast", 
         as.integer(30), as.integer(100), as.integer(60), as.integer(30))
  
  # Writting the network to a file
  .jcall(obj = net, returnSig = "V", method = "writeFile", "tutorial_a.xdsl")
}

##########################
InferenceWithBayesianNetwork = function(){
  # Load the file stored in Tutorial 1
  net = .jnew("smile/Network")
  .jcall(obj = net, returnSig = "V", method = "readFile", "tutorial_a.xdsl")
  
  # ---- We want to compute P("Forecast" = Moderate) ----
  # Updating the network
  .jcall(obj = net, returnSig = "V", method = "updateBeliefs")
  
  # Getting the handle of the node "Forecast"
  .jcall(obj = net, returnSig = "I", method = "getNode", "Forecast")
  
  # Getting the index of the "Moderate" outcome
  aForecastOutcomeIds = .jcall(obj = net, returnSig = "[S", method = "getOutcomeIds", "Forecast")
  for (outcomeIndex in 1 : length(aForecastOutcomeIds)){
    if ("Moderate" == aForecastOutcomeIds[outcomeIndex]){
      break;
    }
  }
  
  # Getting the value of the probability
  aValues = .jcall(obj = net, returnSig = "[D", method = "getNodeValue", "Forecast")
  P_ForecastIsModerate = aValues[outcomeIndex]
  cat("P(\"Forecast\" = Moderate) = ", P_ForecastIsModerate, "\n")
  
  # ---- We want to compute P("Success" = Failure | "Forecast" = Good) ----
  # Introducing the evidence in node "Forecast"
  .jcall(obj = net, returnSig = "V", method = "setEvidence", "Forecast", "Good")
  
  # Updating the network
  .jcall(obj = net, returnSig = "V", method = "updateBeliefs")
  
  # Getting the handle of the node "Success"
  .jcall(obj = net, returnSig = "I", method = "getNode", "Success")
  
  # Getting the index of the "Failure" outcome
  aSuccessOutcomeIds = .jcall(obj = net, returnSig = "[S", method = "getOutcomeIds", "Success")
  for (outcomeIndex in 1 : length(aSuccessOutcomeIds)){
    if ("Failure" == aSuccessOutcomeIds[outcomeIndex]){
      break;
    }
  }
  
  # Getting the value of the probability
  aValues = .jcall(obj = net, returnSig = "[D", method = "getNodeValue", "Success")
  P_SuccIsFailGivenForeIsGood = aValues[outcomeIndex]
  cat("P(\"Success\" = Failure | \"Forecast\" = Good) = ", P_SuccIsFailGivenForeIsGood, "\n")
  
  # ---- We want to compute P("Success" = Success | "Forecast" = Poor) ----
  # Clearing the evidence in node "Forecast"
  .jcall(obj = net, returnSig = "V", method = "clearEvidence", "Forecast")
  
  # Introducing the evidence in node "Forecast"
  .jcall(obj = net, returnSig = "V", method = "setEvidence", "Forecast", "Good")
  
  # Updating the network
  .jcall(obj = net, returnSig = "V", method = "updateBeliefs")
  
  # Getting the index of the "Failure" outcome
  aSuccessOutcomeIds = .jcall(obj = net, returnSig = "[S", method = "getOutcomeIds", "Success")
  for (outcomeIndex in 1 : length(aSuccessOutcomeIds)){
    if ("Failure" == aSuccessOutcomeIds[outcomeIndex]){
      break;
    }
  }
  
  # Getting the value of the probability
  aValues = .jcall(obj = net, returnSig = "[D", method = "getNodeValue", "Success")
  P_SuccIsSuccGivenForeIsPoor = aValues[outcomeIndex]
  cat("P(\"Success\" = Success | \"Forecast\" = Poor) = ", P_SuccIsSuccGivenForeIsPoor, "\n")
}


