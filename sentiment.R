##############################################################
##ANALYSING TWEET SENTIMENT ON OBAMACARE
#### THE SENTIMENT FUNCTION IN THIS THIS PROGRAM
### MADE PUBLICLY AVAILABLE BY https://github.com/jeffreybreen
#############################################################



#######################
## INSTALL PACKAGES
#######################

install.packages("reshape2")
install.packages("twitteR")
install.packages("plyr")
install.packages("ROAuth")
install.packages("stringr")
install.packages("ggplot2")

library("reshape2")
library("twitteR")
library("plyr")
library("ROAuth")
library("stringr")
library("ggplot2")


download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "#########################"
consumerSecret <- "###########################"
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL, 
                         authURL=authURL)
Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")) 
registerTwitterOAuth(Cred)
filecred = paste(addr, "twitter authentication.Rdata", sep = "/") 

save(Cred, file=filecred)
load(filecred)


########################################################################
#### Place Lat long 
#######################################################################



  ###### LA      :: 34.0500,-118.2500 
  ###### Chicago :: 41.8819,87.6278 
  ###### Phoenix :: 33.4483800,-112.0740400
  ###### Denver ::: 39.7391500,-104.9847000
  ###### Louisville :: 38.2542400,-85.7594100
  ###### Columbus :::: 39.9611800,-82.9987900
  ###### Houston ::::::::: 29.7632800,-95.3632700
  ###### Memphis ::::::::: 35.1495300,-90.0489800  
  ###### Milwaukee ::::::: 43.0389000,-87.9064700 
  ###### Miami ::::::::::: 25.7877,-80.2241 )




###################################################################
#### generating variables
##################################################################


addr <- "/Users/krp/dropbox/research/sentiment"
addrtt <- "/Users/krp/dropbox/research/sentiment/Tweets/"
dttoday <- "11_24"
dateSt <- "2014-11-17"
dateEd <- "2014-11-24"

#####################################################################
# DATES TWEETS HAVE BEEN COLLECTED :: 6_25; 7_03;  8_23; 10_12; 10_19; 
### 10_26; 11_02; 11_11; 11_24
#####################################################################
                     
##################################################
########################      TWITTER API CALL  
## THE API CALL GETS 1000 TWEETS ON OBAMACARE 
## AND ACA FROM ALL OVER AND 200 TWEETS FROM 
## A 50 MILE RADIUS OF ANY GIVEN CITY
#################################################

## 000 ALL USA

USObc = searchTwitter("Obamacare", n=1000, 
                          since = dateSt, until = dateEd,
                          cainfo="cacert.pem")

USACA = searchTwitter("#ACA", n=1000, 
                      since = dateSt, until = dateEd,                            
                      cainfo="cacert.pem")

#######################################
#### BLUE states that expanded MEDICAID
#######################################



## 1111 Chicago 50 mile 



ChicagObc = searchTwitter("Obamacare", n=200, 
                          since = dateSt, until = dateEd,
                          geocode="41.8819,-87.6278,50mi",
                          cainfo="cacert.pem")

ChicagACA = searchTwitter("#ACA", n=200, 
                          since = dateSt, until = dateEd,  
                          geocode="41.8819,-87.6278,50mi",
                          cainfo="cacert.pem")

## 2222 LA 50 mile 

LosAngObc = searchTwitter("Obamacare", n=200, 
                          since = dateSt, until = dateEd,                         
                          geocode="34.0500,-118.2500,50mi",
                          cainfo="cacert.pem")

LosAngACA = searchTwitter("#ACA", n=200, 
                          since = dateSt, until = dateEd,   
                          geocode="34.0500,-118.2500,50mi",
                          cainfo="cacert.pem")

#### RED states that expanded MEDICAID
######################################

## 3333 Columbus 50 mile 

ColumbObc = searchTwitter("Obamacare", n=200, 
                          since = dateSt, until = dateEd,  
                          geocode="39.9611,-82.9987,50mi",
                          cainfo="cacert.pem")

ColumbACA = searchTwitter("#ACA", n=200, 
                          since = dateSt, until = dateEd,   
                          geocode="39.9611,-82.9987,50mi",
                            cainfo="cacert.pem")

## 4444 Denver 50 mile 

DenverObc = searchTwitter("Obamacare", n=200, 
                          since = dateSt, until = dateEd,    
                          geocode="41.8500,-87.6500,50mi",
                          cainfo="cacert.pem")

DenverACA = searchTwitter("#ACA", n=200, 
                          since = dateSt, until = dateEd,  
                          geocode="41.8500,-87.6500,50mi",
                          cainfo="cacert.pem")


## 5555 Louisville KY 50 mile

LouisvObc = searchTwitter("Obamacare", n=200, 
                          since = dateSt, until = dateEd,  
                          geocode="38.2542,-85.7594,50mi",
                          cainfo="cacert.pem")

LouisvACA = searchTwitter("#ACA", n=200, 
                          since = dateSt, until = dateEd,     
                          geocode="38.2542,-85.7594,50mi",
                          cainfo="cacert.pem")

## 6666 Phoenix AR 50 mile
PhoeniObc = searchTwitter("Obamacare", n=200, 
                          since = dateSt, until = dateEd,  
                          geocode="33.4483,-112.0740,50mi",
                          cainfo="cacert.pem")

PhoeniACA = searchTwitter("#ACA", n=200, 
                          since = dateSt, until = dateEd,   
                          geocode="33.4483,-112.0740,50mi",
                          cainfo="cacert.pem")




##########################################################
#### RED STATES THAT DID NOT EXPAND MEDICAID

## 7777 Houston AR 50 mile

HoustoObc = searchTwitter("Obamacare", n=200, 
                          since = dateSt, until = dateEd,  
                          geocode="29.7632,-95.3632,50mi",
                          cainfo="cacert.pem")

HoustoACA = searchTwitter("#ACA", n=200, 
                          since = dateSt, until = dateEd,   
                          geocode="29.7632,-95.3632,50mi",
                          cainfo="cacert.pem")

## 8888 Milwaukee AR 50 mile

MilwauObc = searchTwitter("Obamacare", n=200, 
                          since = dateSt, until = dateEd,  
                          geocode="43.0389,-87.9064,50mi",
                          cainfo="cacert.pem")

MilwauACA = searchTwitter("#ACA", n=200, 
                          since = dateSt, until = dateEd,   
                          geocode="43.0389,-87.9064,50mi",
                          cainfo="cacert.pem")

## 9999 Memphis AR 50 mile

MemphiObc = searchTwitter("Obamacare", n=200, 
                          since = dateSt, until = dateEd,  
                          geocode="35.1495,-90.0489,50mi",
                          cainfo="cacert.pem")

MemphiACA = searchTwitter("#ACA", n=200, 
                          since = dateSt, until = dateEd,  
                          geocode="35.1495,-90.0489,50mi",
                          cainfo="cacert.pem")

## 10 Miami AR 50 mile

MiamiObc = searchTwitter("Obamacare", n=200, 
                         since = dateSt, until = dateEd,  
                         geocode="25.7877,-80.2241,50mi",
                         cainfo="cacert.pem")

MiamiACA = searchTwitter("#ACA", n=200, 
                         since = dateSt, until = dateEd,  
                         geocode="25.7877,-80.2241,50mi",
                         cainfo="cacert.pem")

###### LA      :: 34.0500, -118.2500
###### Chicago :: 41.8819, -87.6278 
###### Phoenix :: 33.4483800, -112.0740400
###### Denver ::: 39.7391500, -104.9847000
###### Louisville :: 38.2542400, -85.7594100
###### Columbus :::: 39.9611800, -82.9987900
###### Albuquerque ::::: 35.1107, -106.6100
###### Houston ::::::::: 29.7632800, -95.3632700
###### Memphis ::::::::: 35.1495300, -90.0489800  
###### Milwaukee ::::::: 43.0389000, -87.9064700 
###### Miami ::::::::::: 25.7877, -80.2241 


###########################################################

aaa <- paste(addrtt, "USObc", dttoday, ".csv", sep = '')

####################
### changing to a DF
####################

#####
##USA
#####

USObcDF <- twListToDF(USObc)
write.csv(USObcDF, file = paste(addrtt, "USObc", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

USACADF <- twListToDF(USACA)
write.csv(USACADF, file = paste(addrtt, "USACA", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

#########################
### Los Angeles CSV files
#########################

LosAngObcDF <- twListToDF(LosAngObc)
write.csv(LosAngObcDF, file = paste(addrtt, "LosAngObc", dttoday, ".csv", sep=''), 
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

LosAngACADF <- twListToDF(LosAngACA)
write.csv(LosAngACADF, file = paste(addrtt, "LosAngACA", dttoday, ".csv", sep=''), 
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

#####################
### Chicago CSV files
#####################

ChicagObcDF <- twListToDF(ChicagObc)
write.csv(ChicagObcDF, file = paste(addrtt, "ChicagObc", dttoday, ".csv", sep=''), 
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

ChicagACADF <- twListToDF(ChicagACA)
write.csv(ChicagACADF, file = paste(addrtt, "ChicagACA", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

######################
### Columbus CSV files
######################

ColumbObcDF <- twListToDF(ColumbObc)
write.csv(ColumbObcDF, file = paste(addrtt, "ColumbObc", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

ColumbACADF <- twListToDF(ColumbACA)
write.csv(ColumbACADF, file = paste(addrtt, "ColumbACA", dttoday, ".csv", sep=''), 
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')


###########################################################
### Columbus CSV files
######################

DenverObcDF <- twListToDF(DenverObc)
write.csv(DenverObcDF, file = paste(addrtt, "DenverObc", dttoday, ".csv", sep=''), 
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

DenverACADF <- twListToDF(DenverACA)
write.csv(DenverACADF, file = paste(addrtt, "DenverACA", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')




########################
### Louisville CSV files
########################

LouisvObcDF <- twListToDF(LouisvObc)
write.csv(LouisvObcDF, file = paste(addrtt, "LouisvObc", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

LouisvACADF <- twListToDF(LouisvACA)
write.csv(LouisvACADF, file = paste(addrtt, "LouisvACA", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

#####################
### Phoenix CSV files
#####################

PhoeniObcDF <- twListToDF(PhoeniObc)
write.csv(PhoeniObcDF, file = paste(addrtt, "PhoeniObc", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

PhoeniACADF <- twListToDF(PhoeniACA)
write.csv(PhoeniACADF, file = paste(addrtt, "PhoeniACA", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

#############################################################
########################################### Houston CSV files
#############################################################

HoustoObcDF <- twListToDF(HoustoObc)
write.csv(HoustoObcDF, file = paste(addrtt, "HoustoObc", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

HoustoACADF <- twListToDF(HoustoACA)
write.csv(HoustoACADF, file = paste(addrtt, "HoustoACA", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

###############################################################
############################################# Memphis CSV files
###############################################################

MemphiObcDF <- twListToDF(MemphiObc)
write.csv(MemphiObcDF, file = paste(addrtt, "MemphiObc", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

MemphiACADF <- twListToDF(MemphiACA)
write.csv(MemphiACADF, file = paste(addrtt, "MemphiACA", dttoday, ".csv", sep=''), 
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

#################################################################
############################################# Milwaukee CSV files
#################################################################

MilwauObcDF <- twListToDF(MilwauObc)
write.csv(MilwauObcDF, file = paste(addrtt, "MilwauObc", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

MilwauACADF <- twListToDF(MilwauACA)
write.csv(MilwauACADF, file = paste(addrtt, "MilwauACA", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

#################################################################
################################################# Miami CSV files
#################################################################

MiamiObcDF <- twListToDF(MiamiObc)
write.csv(MiamiObcDF, file = paste(addrtt, "MiamiObc", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')

MiamiACADF <- twListToDF(MiamiACA)
write.csv(MiamiACADF, file = paste(addrtt, "MiamiACA", dttoday, ".csv", sep=''),
          row.names = FALSE, fileEncoding = 'UTF-8-MAC')



######################
### Lisiting All files
######################



USfiles <- list.files("/Users/krp/Dropbox/Research/Sentiment/tweets", full.names = T, 
                      pattern = "USObc")
#################################################
## dropping the second file, which is a test file
#################################################

USObcfiles2 <- USfiles[-2]

TtUSObc <- lapply(USObcfiles2, function(x) read.csv(x))



##("6_06", "6_17", "6_25", "7_03", "8_01", "8_16",8_23; 10_12; 10_19; 10_26; 11_02; 11_11)

TtUStweets6_08 <- TtUSObc[[7]]$text
TtUStweets6_17 <- TtUSObc[[8]]$text
TtUStweets6_25 <- TtUSObc[[9]]$text
TtUStweets7_03 <- TtUSObc[[10]]$text
TtUStweets8_01 <- TtUSObc[[11]]$text
TtUStweets8_16 <- TtUSObc[[12]]$text
TtUStweets8_23 <- TtUSObc[[1]]$text
TtUStweets8_30 <- TtUSObc[[13]]$text
TtUStweets10_12 <- TtUSObc[[2]]$text
TtUStweets10_19 <- TtUSObc[[3]]$text
TtUStweets10_26 <- TtUSObc[[4]]$text
TtUStweets11_02 <- TtUSObc[[5]]$text
TtUStweets11_11 <- TtUSObc[[6]]$text


####################################
## removing unprocessable characters
####################################

TtUStweets6_08 <- str_replace_all(TtUStweets6_08,"[^[:graph:]]", " ")
TtUStweets6_17 <- str_replace_all(TtUStweets6_17,"[^[:graph:]]", " ")
TtUStweets6_25 <- str_replace_all(TtUStweets6_25,"[^[:graph:]]", " ")
TtUStweets7_03 <- str_replace_all(TtUStweets7_03,"[^[:graph:]]", " ")
TtUStweets8_01 <- str_replace_all(TtUStweets8_01,"[^[:graph:]]", " ")
TtUStweets8_16 <- str_replace_all(TtUStweets8_16,"[^[:graph:]]", " ")
TtUStweets8_23 <- str_replace_all(TtUStweets8_23,"[^[:graph:]]", " ")
TtUStweets8_30 <- str_replace_all(TtUStweets8_30,"[^[:graph:]]", " ")
TtUStweets10_12 <- str_replace_all(TtUStweets10_12,"[^[:graph:]]", " ")
TtUStweets10_19 <- str_replace_all(TtUStweets10_19,"[^[:graph:]]", " ")
TtUStweets10_26 <- str_replace_all(TtUStweets10_26,"[^[:graph:]]", " ")
TtUStweets11_02 <- str_replace_all(TtUStweets11_02,"[^[:graph:]]", " ")
TtUStweets11_11 <- str_replace_all(TtUStweets11_11,"[^[:graph:]]", " ")



#############################################################################
### sentiment function
#############################################################################
require(plyr)
require(stringr)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')  
{  
  require(plyr)  
          require(stringr)       
          
          # we got a vector of sentences. plyr will handle a list  
          # or a vector as an "l" for us  
          # we want a simple array ("a") of scores back, so we use   
          # "l" + "a" + "ply" = "laply":  
          
          scores = laply(sentences, function(sentence, pos.words, neg.words) {  
          
          # clean up sentences with R's regex-driven global substitute, gsub():  
          
          sentence = gsub('[[:punct:]]', '', sentence)  
          
          sentence = gsub('[[:cntrl:]]', '', sentence)  
          
          sentence = gsub('\\d+', '', sentence)  
          
          # and convert to lower case:  
          
          sentence = tolower(sentence)  
          
          # split into words. str_split is in the stringr package  
          
          word.list = str_split(sentence, '\\s+')  
          
          # sometimes a list() is one level of hierarchy too much  
          
          words = unlist(word.list)  
          
          # compare our words to the dictionaries of positive & negative terms  
          
          pos.matches = match(words, pos.words)  
          neg.matches = match(words, neg.words)  
          
          # match() returns the position of the matched term or NA  
          # we just want a TRUE/FALSE:  
          
          pos.matches = !is.na(pos.matches)  
          
          neg.matches = !is.na(neg.matches)  
          
          # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():  
          
          score = sum(pos.matches) - sum(neg.matches)  
          
          return(score)  
          
          }, pos.words, neg.words, .progress=.progress )  
          scores.df = data.frame(score=scores, text=sentences)  
          return(scores.df)  
          } 



##################################################################################
### SCORING TWEETS:
##################################################################################

#Load sentiment word lists
pos.words = scan('/Users/krp/Dropbox/Research/Sentiment/positive-words.txt', what='character', comment.char=';')
neg.words = scan('/Users/krp/Dropbox/Research/Sentiment/negative-words.txt', what='character', comment.char=';')


#####################################################################
#### Scoring all Obamacare tweets for the weeks the data is available
#####################################################################



USObc6_08 <- as.factor(TtUStweets6_08)
USObc6_08Score = score.sentiment(USObc6_08,pos.words,neg.words, .progress='text')
USObc6_08Score$Place <- "US"
USObc6_08Score$Week <- round((5*30)/7+(08/7)+1)


USObc6_17 <- as.factor(TtUStweets6_17)
USObc6_17Score = score.sentiment(USObc6_17,pos.words,neg.words, .progress='text')
USObc6_17Score$Place <- "US"
USObc6_17Score$Week <- round((5*30)/7+(17/7)+1)


USObc6_25 <- as.factor(TtUStweets6_25)
USObc6_25Score = score.sentiment(USObc6_25,pos.words,neg.words, .progress='text')
USObc6_25Score$Place <- "US"
USObc6_25Score$Week <- round((5*30)/7+(25/7)+1)


 
USObc7_03 <- as.factor(TtUStweets7_03)
USObc7_03Score = score.sentiment(USObc7_03,pos.words,neg.words, .progress='text')
USObc7_03Score$Place <- "US"
USObc7_03Score$Week <- round((6*30)/7+(03/7)+1)



USObc8_01 <- as.factor(TtUStweets8_01)
USObc8_01Score = score.sentiment(USObc8_01,pos.words,neg.words, .progress='text')
USObc8_01Score$Place <- "US"
USObc8_01Score$Week <- round((7*30)/7+(01/7)+1)


USObc8_16 <- as.factor(TtUStweets8_16)
USObc8_16Score = score.sentiment(USObc6_08,pos.words,neg.words, .progress='text')
USObc8_16Score$Place <- "US"
USObc8_16Score$Week <- round((7*30)/7+(16/7)+1)


USObc8_23 <- as.factor(TtUStweets8_23)
USObc8_23Score = score.sentiment(USObc8_23,pos.words,neg.words, .progress='text')
USObc8_23Score$Place <- "US"
USObc8_23Score$Week <- round((7*30)/7+(23/7)+1)



USObc8_30 <- as.factor(TtUStweets8_30)
USObc8_30Score = score.sentiment(USObc8_30,pos.words,neg.words, .progress='text')
USObc8_30Score$Place <- "US"
USObc8_30Score$Week <- round((7*30)/7+(30/7)+1)


USObc10_12 <- as.factor(TtUStweets10_12)
USObc10_12Score = score.sentiment(USObc10_12,pos.words,neg.words, .progress='text')
USObc10_12Score$Place <- "US"
USObc10_12Score$Week <- round((9*30)/7+(12/7)+1)


USObc10_19 <- as.factor(TtUStweets10_19)
USObc10_19Score = score.sentiment(USObc10_19,pos.words,neg.words, .progress='text')
USObc10_19Score$Place <- "US"
USObc10_19Score$Week <- round((9*30)/7+(19/7)+1)



USObc10_26 <- as.factor(TtUStweets10_26)
USObc10_26Score = score.sentiment(USObc10_26,pos.words,neg.words, .progress='text')
USObc10_26Score$Place <- "US"
USObc10_26Score$Week <- round((9*30)/7+(26/7)+1)



USObc11_02 <- as.factor(TtUStweets11_02)
USObc11_02Score = score.sentiment(USObc11_02,pos.words,neg.words, .progress='text')
USObc11_02Score$Place <- "US"
USObc11_02Score$Week <- round((10*30)/7+(02/7)+1)


USObc11_11 <- as.factor(TtUStweets11_11)
USObc11_11Score = score.sentiment(USObc11_11,pos.words,neg.words, .progress='text')
USObc11_11Score$Place <- "US"
USObc11_11Score$Week <- round((10*30)/7+(11/7)+1)



########################################################
####"6_06", "6_17", "6_25", "7_03", "8_01", "8_16",8_23; 
###10_12; 10_19; 10_26; 11_02; 11_11
### building a list for these scores
########################################################

ObcTweetScores <- rbind(USObc6_08Score, USObc6_17Score, USObc6_25Score, USObc7_03Score,
                             USObc8_01Score, USObc8_16Score, USObc8_23Score, USObc10_12Score,
                             USObc10_19Score, USObc10_26Score, USObc11_02Score, USObc11_11Score)

ObcTweetScores$threegrp <- ifelse (ObcTweetScores$score > 0, 1, 
                               ifelse (ObcTweetScores$score < 0, -1, 0))

##############################################
### Building a frequency table of these scores
##############################################

ObcPNN <- table(ObcTweetScores$threegrp)

###########################
### Turing it into a matrix
###########################

Testvar1 <- unlist(ObcPNN)
Testvar2 <- matrix(Testvar1, ncol = 3, byrow = T)
colnames(Testvar2) <- c("Neg", "Neut", "Pos")
rownames(Testvar2) <- c(24,25,26,27,31,33,34,41,42,43,44,45)
Total <- rowSums(Testvar2)

####################################
##Calculating Percentage of Opinion
###################################


NegPct <- (Testvar3[,1]/Total)*100
NeutPct <- (Testvar3[,2]/Total)*100
PosPct <- (Testvar3[,3]/Total)*100

PercentMatrix <- cbind(NegPct, NeutPct, PosPct)
PercentMatrix <- round(PercentMatrix, digits = 2)

PerMatMelt <- melt(PercentMatrix)
colnames(PerMatMelt) <- c("Week", "Sentiment", "Percentage")

####################################
#### Plotting weekly results
####################################

dplot <- ggplot(PerMatMelt, aes(x=Week, y=Percentage, group=Sentiment, colour=Sentiment)) + 
  geom_line(linetype = 3) +           
  theme(axis.text.x = element_text(size = 18, face = "bold", color = "black"),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 28)) +
  labs(title = "Opinion on Obamacare among Twitter users (June to Nov 2014)") +
  geom_smooth(method = lm, se = FALSE, linetype = 1) +
  scale_y_continuous(limits = c(0,100)) + 
  scale_color_manual(values = c("#FF0000", "#0000CC", "#009933"),
                                labels = c("Negative", "Neutral", "Positive")) 

print(dplot)




################################################
###Scoring tweets 1111. FOR THE WEEK OF 11/11
################################################


TtUS<- read.csv(file = paste(addrtt,"USObc",dttoday,".csv", sep=''), encoding='UTF-8-MAC')
TtUS$text<-as.factor(TtUS$text)
USScore = score.sentiment(TtUS$text,pos.words,neg.words, .progress='text')
USScore$Place <- "US"


TtChicag <- read.csv(file = paste(addrtt, "ChicagObc", dttoday, ".csv", sep=''), fileEncoding='UTF-8-MAC')
TtChicag$text<-as.factor(TtChicag$text)
ChicagScore = score.sentiment(TtChicag$text, pos.words,neg.words, .progress='text')
ChicagScore$Place <- "Chicago"

TtLosAng <- read.csv(file = paste(addrtt, "LosAngObc", dttoday, ".csv", sep=''), fileEncoding='UTF-8')
TtLosAng$text<-as.factor(TtLosAng$text)
LosAngScore = score.sentiment(TtLosAng$text, pos.words,neg.words, .progress='text')
LosAngScore$Place <- "LosAngeles"

TtColumb <- read.csv(file = paste(addrtt, "columbObc", dttoday, ".csv", sep=''), fileEncoding='UTF-8')
TtColumb$text<-as.factor(TtColumb$text)
ColumbScore = score.sentiment(TtColumb$text, pos.words,neg.words, .progress='text')
ColumbScore$Place <- "Columbus"

TtDenver <- read.csv(file = paste(addrtt, "DenverObc", dttoday, ".csv", sep=''), fileEncoding='UTF-8')
TtDenver$text<-as.factor(TtDenver$text)
DenverScore = score.sentiment(TtDenver$text, pos.words,neg.words, .progress='text')
DenverScore$Place <- "Denver"

TtLouisv <- read.csv(file = paste(addrtt, "LouisvObc", dttoday, ".csv", sep=''), fileEncoding='UTF-8')
TtLouisv$text<-as.factor(TtLouisv$text)
LouisvScore = score.sentiment(TtLouisv$text, pos.words,neg.words, .progress='text')
LouisvScore$Place <- "Louisville"

TtPhoeni <- read.csv(file = paste(addrtt, "PhoeniObc", dttoday, ".csv", sep=''), fileEncoding='UTF-8')
TtPhoeni$text<-as.factor(TtPhoeni$text)
PhoeniScore = score.sentiment(TtPhoeni$text, pos.words,neg.words, .progress='text')
PhoeniScore$Place <- "Phoenix"

TtHousto <- read.csv(file = paste(addrtt, "HoustoObc", dttoday, ".csv", sep=''), fileEncoding='UTF-8')
TtHousto$text<-as.factor(TtHousto$text)
HoustoScore = score.sentiment(TtHousto$text, pos.words,neg.words, .progress='text')
HoustoScore$Place <- "Houston"

TtMemphi <- read.csv(file = paste(addrtt, "MemphiObc", dttoday, ".csv", sep=''), fileEncoding='UTF-8')
TtMemphi$text<-as.factor(TtMemphi$text)
MemphiScore = score.sentiment(TtMemphi$text, pos.words,neg.words, .progress='text')
MemphiScore$Place <- "Memphis"

TtMilwau <- read.csv(file = paste(addrtt, "MilwauObc", dttoday, ".csv", sep=''), fileEncoding='UTF-8')
TtMilwau$text<-as.factor(TtMilwau$text)
MilwauScore = score.sentiment(TtMilwau$text, pos.words,neg.words, .progress='text')
MilwauScore$Place <- "Miwaukee"

TtMiami <- read.csv(file = paste(addrtt, "MiamiObc", dttoday, ".csv", sep=''), fileEncoding='UTF-8')
TtMiami$text<-as.factor(TtMiami$text)
MiamiScore = score.sentiment(TtMiami$text, pos.words,neg.words, .progress='text')
MiamiScore$Place <- "Miami"




#################################################################
### BUILDING A LIST FOR THE WEEK
#################################################################

Obc_Scores <- rbind(USScore,ChicagScore,LosAngScore,ColumbScore,
                       DenverScore,PhoeniScore,HoustoScore,MemphiScore,
                       MiamiScore,MilwauScore) 




#########################################
#######Generating the three group variable
##########################################

Obc_Scores$threegrp <- ifelse (Obc_Scores$score > 0, 1, 
                                  ifelse (Obc_Scores$score < 0, -1, 0))

##############################
#writing into a csv file below
##############################

write.csv(Obc_Scores, file = paste(addr, "/Scores/Scores", dttoday, ".csv", sep = ''),
          row.names=FALSE, fileEncoding = 'UTF-8-MAC')



#######################################################
######CALCULATING % NEG AND POS, method diff from above
#######################################################


USNeg <- subset(USScore, score<0) 
USPos <- subset(USScore, score>0)
USNegSc <- (length(USNeg$score)/length(USScore$score))*100
USPosSc <- (length(USPos$score)/length(USScore$score))*100 
USNeuSc <- 100 - (USPosSc + USNegSc)

ChicagNeg <- subset(ChicagScore, score<0) 
ChicagPos <- subset(ChicagScore, score>0)
ChicagNegSc <- (length(ChicagNeg$score)/length(ChicagScore$score))*100
ChicagPosSc <- (length(ChicagPos$score)/length(ChicagScore$score))*100 
ChicagNeuSc <- 100 - (ChicagPosSc + ChicagNegSc)

LosAngNeg <- subset(LosAngScore, score<0) 
LosAngPos <- subset(LosAngScore, score>0)
LosAngNegSc <- (length(LosAngNeg$score)/length(LosAngScore$score))*100
LosAngPosSc <- (length(LosAngPos$score)/length(LosAngScore$score))*100 
LosAngNeuSc <- 100 - (LosAngPosSc + LosAngNegSc)

ColumbNeg <- subset(ColumbScore, score<0) 
ColumbPos <- subset(ColumbScore, score>0)
ColumbNegSc <- (length(ColumbNeg$score)/length(ColumbScore$score))*100
ColumbPosSc <- (length(ColumbPos$score)/length(ColumbScore$score))*100 
ColumbNeuSc <- 100 - (ColumbPosSc + ColumbNegSc)

DenverNeg <- subset(DenverScore, score<0) 
DenverPos <- subset(DenverScore, score>0)
DenverNegSc <- (length(DenverNeg$score)/length(DenverScore$score))*100
DenverPosSc <- (length(DenverPos$score)/length(DenverScore$score))*100 
DenverNeuSc <- 100 - (DenverPosSc + DenverNegSc)

LouisvNeg <- subset(LouisvScore, score<0) 
LouisvPos <- subset(LouisvScore, score>0)
LouisvNegSc <- (length(LouisvNeg$score)/length(LouisvScore$score))*100
LouisvPosSc <- (length(LouisvPos$score)/length(LouisvScore$score))*100 
LouisvNeuSc <- 100 - (LouisvPosSc + LouisvNegSc)

PhoeniNeg <- subset(PhoeniScore, score<0) 
PhoeniPos <- subset(PhoeniScore, score>0)
PhoeniNegSc <- (length(PhoeniNeg$score)/length(PhoeniScore$score))*100
PhoeniPosSc <- (length(PhoeniPos$score)/length(PhoeniScore$score))*100 
PhoeniNeuSc <- 100 - (PhoeniPosSc + PhoeniNegSc)

HoustoNeg <- subset(HoustoScore, score<0) 
HoustoPos <- subset(HoustoScore, score>0)
HoustoNegSc <- (length(HoustoNeg$score)/length(HoustoScore$score))*100
HoustoPosSc <- (length(HoustoPos$score)/length(HoustoScore$score))*100 
HoustoNeuSc <- 100 - (HoustoPosSc + HoustoNegSc)

MemphiNeg <- subset(MemphiScore, score<0) 
MemphiPos <- subset(MemphiScore, score>0)
MemphiNegSc <- (length(MemphiNeg$score)/length(MemphiScore$score))*100
MemphiPosSc <- (length(MemphiPos$score)/length(MemphiScore$score))*100 
MemphiNeuSc <- 100 - (MemphiPosSc + MemphiNegSc)

MilwauNeg <- subset(MilwauScore, score<0) 
MilwauPos <- subset(MilwauScore, score>0)
MilwauNegSc <- (length(MilwauNeg$score)/length(MilwauScore$score))*100
MilwauPosSc <- (length(MilwauPos$score)/length(MilwauScore$score))*100 
MilwauNeuSc <- 100 - (MilwauPosSc + MilwauNegSc)

MiamiNeg <- subset(MiamiScore, score<0) 
MiamiPos <- subset(MiamiScore, score>0)
MiamiNegSc <- (length(MiamiNeg$score)/length(MiamiScore$score))*100
MiamiPosSc <- (length(MiamiPos$score)/length(MiamiScore$score))*100 
MiamiNeuSc <- 100 - (MiamiPosSc + MiamiNegSc)


Obc_Percent <- matrix(c("US",USPosSc,USNeuSc,USNegSc,
                             "Chicago",ChicagPosSc,ChicagNeuSc,ChicagNegSc,
                             "LosAngeles",LosAngPosSc,LosAngNeuSc,LosAngNegSc,
                             "Columbus",ColumbPosSc,ColumbNeuSc,ColumbNegSc,
                             "Denver",DenverPosSc,DenverNeuSc,DenverNegSc,
                             "Louisville",LouisvPosSc,LouisvNeuSc,LouisvNegSc,
                             "Phoenix",PhoeniPosSc,PhoeniNeuSc,PhoeniNegSc,
                             "Houston",HoustoPosSc,HoustoNeuSc,HoustoNegSc,
                             "Memphis",MemphiPosSc,MemphiNeuSc,MemphiNegSc,
                             "Milwaukee",MilwauPosSc,MilwauNeuSc,MilwauNegSc,
                             "Miami",MiamiPosSc,MiamiNeuSc,MiamiNegSc),  
                           byrow = T, ncol = 4,
                           dimnames = list(c(),
                                      c("Place", "Positive", "Neutral", "Negative")))

Obc_Percent <- as.data.frame(Obc_Percent)

write.csv(Obc_Percent, file = paste(addr, "/Scores/ScoresPNN", dttoday, ".csv", sep = ''),
          row.names=TRUE, fileEncoding = 'UTF-8-MAC')

##############################################
## Reshaping the dataframe for use with ggplot
##############################################

ObMelt <- melt(Obc_Percent, id.vars = "Place")
colnames(ObMelt) <- c("Place", "Sentiment", "Percentage")
ObMelt$Percentage <- as.numeric(ObMelt$Percentage)
ObMelt$Percentage <- round(ObMelt$Percentage, digit = 2)


#####################################################################################################
##  VISUALIZING ggplot functions
#####################################################################################################


##########################################
####visuaising in a barplot
##########################################

b <- ggplot(data = ObMelt, aes(x = Place, y = Percentage, fill = Sentiment)) 
  b1 <-  b + geom_bar(stat = "identity")  +
  theme(axis.text.x = element_text(size = 18, face = "bold", color = "black"),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 28)) +
  labs(title = "Opinion on Obamacare across America (Nov 4 - Nov 11)") +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_brewer(palette = 12)

print(b1)

