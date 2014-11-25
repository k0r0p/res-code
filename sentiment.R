## twitter API call:

library("twitteR")
library("plyr")
library("ROAuth")
library("stringr")
library("ggplot2")

addr <- "F:/RES DOCS/Sentiment"

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "Jk8xode8AQPKUDXbMWSTA8iqo"
consumerSecret <- "pAaUzTbJnUPtijQonOWCNJJKgBP5nxWsXgXyIIx2nBU2X7R940"
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL, 
                         authURL=authURL)
Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl") )

registerTwitterOAuth(Cred)

save(Cred, file="F:/RES DOCS/Sentiment/twitter authentication.Rdata")
load("F://RES DOCS/Sentiment/twitter authentication.Rdata")

##############################################################################
## getting tweets TO BE DONE EVERY FRIDAY MORNING
##############################################################################

## 000 ALL USA

USObc = searchTwitter("Obamacare", n=1000, 
                          since= "2014-08-02", until = "2014-08-09", 
                          cainfo="cacert.pem")

USACA = searchTwitter("#ACA", n=1000, 
                      since= "2014-08-02", until = "2014-08-09",                            cainfo="cacert.pem")


#### BLUE states that expanded MEDICAID
#######################################



## 1111 Chicago 50 mile 

ChicagObc = searchTwitter("Obamacare", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                          geocode="41.8819,-87.6278,50mi",
                          cainfo="cacert.pem")

ChicagACA = searchTwitter("#ACA", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                          geocode="41.8819,-87.6278,50mi",
                          cainfo="cacert.pem")

## 2222 LA 50 mile 

LosAngObc = searchTwitter("Obamacare", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                          geocode="34.0500,-118.2500,50mi",
                          cainfo="cacert.pem")

LosAngACA = searchTwitter("#ACA", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                          geocode="34.0500,-118.2500,50mi",
                          cainfo="cacert.pem")

#### RED states that expanded MEDICAID
######################################

## 3333 Columbus 50 mile 

ColumbObc = searchTwitter("Obamacare", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                          geocode="39.9611,-82.9987,50mi",
                          cainfo="cacert.pem")

ColumbACA = searchTwitter("#ACA", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                            geocode="39.9611,-82.9987,50mi",
                            cainfo="cacert.pem")

## 4444 Denver 50 mile 

DenverObc = searchTwitter("Obamacare", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                       geocode="41.8500,-87.6500,50mi",
                          cainfo="cacert.pem")

DenverACA = searchTwitter("#ACA", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                          geocode="41.8500,-87.6500,50mi",
                          cainfo="cacert.pem")


## 5555 Louisville KY 50 mile

LouisvObc = searchTwitter("Obamacare", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                       geocode="38.2542,-85.7594,50mi",
                       cainfo="cacert.pem")

LouisvACA = searchTwitter("#ACA", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                              geocode="38.2542,-85.7594,50mi",
                              cainfo="cacert.pem")

## 6666 Phoenix AR 50 mile
PhoeniObc = searchTwitter("Obamacare", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                       geocode="33.4483,-112.0740,50mi",
                       cainfo="cacert.pem")

PhoeniACA = searchTwitter("#ACA", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                           geocode="33.4483,-112.0740,50mi",
                           cainfo="cacert.pem")




##########################################################
#### RED STATES THAT DID NOT EXPAND MEDICAID

## 7777 Houston AR 50 mile

HoustoObc = searchTwitter("Obamacare", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                       geocode="29.7632,-95.3632,50mi",
                       cainfo="cacert.pem")

HoustoACA = searchTwitter("#ACA", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                           geocode="29.7632,-95.3632,50mi",
                           cainfo="cacert.pem")

## 8888 Milwaukee AR 50 mile

MilwauObc = searchTwitter("Obamacare", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                       geocode="43.0389,-87.9064,50mi",
                       cainfo="cacert.pem")

MilwauACA = searchTwitter("#ACA", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                             geocode="43.0389,-87.9064,50mi",
                             cainfo="cacert.pem")

## 9999 Memphis AR 50 mile

MemphiObc = searchTwitter("Obamacare", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                       geocode="35.1495,-90.0489,50mi",
                       cainfo="cacert.pem")

MemphiACA = searchTwitter("#ACA", n=200, 
                          since= "2014-08-02", until = "2014-08-09",                       geocode="35.1495,-90.0489,50mi",
                       cainfo="cacert.pem")

## 10 Miami AR 50 mile
MiamiObc = searchTwitter("Obamacare", n=200, 
                         since= "2014-08-02", until = "2014-08-09",                         geocode="25.7877,-80.2241,50mi",
                         cainfo="cacert.pem")

MiamiACA = searchTwitter("#ACA", n=200, 
                         since= "2014-08-02", until = "2014-08-09",                         geocode="25.7877,-80.2241,50mi",
                         cainfo="cacert.pem")

###### LA      :: 34.0500, 118.2500
###### Chicago :: 41.8819, 87.6278 
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
### changing to a DF

##USA

USObcDF <- twListToDF(USObc)
write.csv(USObcDF, file = "F:/RES DOCS/Sentiment/Tweets/USObc8_09.csv", 
          row.names = FALSE)

USACADF <- twListToDF(USACA)
write.csv(USACADF, file = "F:/RES DOCS/Sentiment/Tweets/USACA8_09.csv", 
          row.names = FALSE)

###########################################################
### Los Angeles CSV files

LosAngObcDF <- twListToDF(LosAngObc)
write.csv(LosAngObcDF, file = "F:/RES DOCS/Sentiment/Tweets/LosAngObc8_09.csv", 
          row.names = FALSE)

LosAngACADF <- twListToDF(LosAngACA)
write.csv(LosAngACADF, file = "F:/RES DOCS/Sentiment/Tweets/LosAngACA8_09.csv", 
          row.names = FALSE)

###########################################################
### Chicago CSV files

ChicagObcDF <- twListToDF(ChicagObc)
write.csv(ChicagObcDF, file = "F:/RES DOCS/Sentiment/Tweets/ChicagObc8_09.csv", 
          row.names = FALSE)

ChicagACADF <- twListToDF(ChicagACA)
write.csv(ChicagACADF, file = "F:/RES DOCS/Sentiment/Tweets/ChicagACA8_09.csv", 
          row.names = FALSE)

###########################################################
### Columbus CSV files

ColumbObcDF <- twListToDF(ColumbObc)
write.csv(ColumbObcDF, file = "F:/RES DOCS/Sentiment/Tweets/ColumbObc8_09.csv", 
          row.names = FALSE)

ColumbACADF <- twListToDF(ColumbACA)
write.csv(ColumbACADF, file = "F:/RES DOCS/Sentiment/Tweets/ColumbACA8_09.csv", 
          row.names = FALSE)


###########################################################
### Columbus CSV files

DenverObcDF <- twListToDF(DenverObc)
write.csv(DenverObcDF, file = "F:/RES DOCS/Sentiment/Tweets/DenverObc8_09.csv", 
          row.names = FALSE)

DenverACADF <- twListToDF(DenverACA)
write.csv(DenverACADF, file = "F:/RES DOCS/Sentiment/Tweets/DenverACA8_09.csv", 
          row.names = FALSE)

##########################################################
### Little Rock CSV files

AlbuqObcDF <- twListToDF(AlbuqObc)
write.csv(AlbuqObcDF, file = "F:/RES DOCS/Sentiment/Tweets/AlbuqObc8_09.csv", 
          row.names = FALSE)

AlbuqACADF <- twListToDF(LittleACA)
write.csv(AlbuqACADF, file = "F:/RES DOCS/Sentiment/Tweets/AlbuqACA8_09.csv", 
          row.names = FALSE)

############################################################
### Louisville CSV files

LouisvObcDF <- twListToDF(LouisvObc)
write.csv(LouisvObcDF, file = "F:/RES DOCS/Sentiment/Tweets/LouisvObc8_09.csv", 
          row.names = FALSE)

LouisvACADF <- twListToDF(LouisvACA)
write.csv(LouisvACADF, file = "F:/RES DOCS/Sentiment/Tweets/LouisvACA8_09.csv", 
          row.names = FALSE)

############################################################
### Phoenix CSV files

PhoeniObcDF <- twListToDF(PhoeniObc)
write.csv(PhoeniObcDF, file = "F:/RES DOCS/Sentiment/Tweets/PhoeniObc8_09.csv", 
          row.names = FALSE)

PhoeniACADF <- twListToDF(PhoeniACA)
write.csv(PhoeniACADF, file = "F:/RES DOCS/Sentiment/Tweets/PhoeniACA8_09.csv", 
          row.names = FALSE)

#############################################################
### Houston CSV files

HoustoObcDF <- twListToDF(HoustoObc)
write.csv(HoustoObcDF, file = "F:/RES DOCS/Sentiment/Tweets/HoustoObc8_09.csv", 
          row.names = FALSE)

HoustoACADF <- twListToDF(HoustoACA)
write.csv(HoustoACADF, file = "F:/RES DOCS/Sentiment/Tweets/HoustoACA8_09.csv", 
          row.names = FALSE)

###############################################################
### Memphis CSV files

MemphiObcDF <- twListToDF(MemphiObc)
write.csv(MemphiObcDF, file = "F:/RES DOCS/Sentiment/Tweets/MemphiObc8_09.csv", 
          row.names = FALSE)

MemphiACADF <- twListToDF(MemphiACA)
write.csv(MemphiACADF, file = "F:/RES DOCS/Sentiment/Tweets/MemphiACA8_09.csv", 
          row.names = FALSE)

#################################################################
### Milwaukee CSV files

MilwauObcDF <- twListToDF(MilwauObc)
write.csv(MilwauObcDF, file = "F:/RES DOCS/Sentiment/Tweets/MilwauObc8_09.csv", 
          row.names = FALSE)

MilwauACADF <- twListToDF(MilwauACA)
write.csv(MilwauACADF, file = "F:/RES DOCS/Sentiment/Tweets/MilwauACA8_09.csv", 
          row.names = FALSE)

#################################################################
### Miami CSV files

MiamiObcDF <- twListToDF(MiamiObc)
write.csv(MiamiObcDF, file = "F:/RES DOCS/Sentiment/Tweets/MiamiObc8_09.csv", 
          row.names = FALSE)

MiamiACADF <- twListToDF(MiamiACA)
write.csv(MiamiACADF, file = "F:/RES DOCS/Sentiment/Tweets/MiamiACA8_09.csv", 
          row.names = FALSE)


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
pos.words = scan('F:/RES DOCS/Sentiment/positive-words.txt', what='character', comment.char=';')
neg.words = scan('F:/RES DOCS/Sentiment/negative-words.txt', what='character', comment.char=';')


###############################################################################
###Data frame for empty tweet scores
################################################


#Import  csv for Obamacare


Obc8_01_US<- read.csv("F:/RES DOCS/Sentiment/tweets/USObc8_01.csv")
Obc8_01_US$text<-as.factor(Obc8_01_US$text)
USScore = score.sentiment(Obc8_01_US$text, pos.words,neg.words, .progress='text')

################################################
###creating a dataframe
################################################




Obc8_01_Chicag <- read.csv("F:/RES DOCS/Sentiment/tweets/ChicagObc8_01.csv")
Obc8_01_Chicag$text<-as.factor(Obc8_01_Chicag$text)
ChicagScore = score.sentiment(Obc8_01_Chicag$text, pos.words,neg.words, .progress='text')


Obc8_01_LosAng <- read.csv("F:/RES DOCS/Sentiment/tweets/LosAngObc8_01.csv")
Obc8_01_LosAng$text<-as.factor(Obc8_01_LosAng$text)
Obc8_01.Scores$LosAng = score.sentiment(Obc8_01_LosAng$text, pos.words,neg.words, .progress='text')

Obc8_01_Columb <- read.csv("F:/RES DOCS/Sentiment/tweets/ColumbObc8_01.csv")
Obc8_01_Columb$text<-as.factor(Obc8_01_Columb$text)
Obc8_01.Scores$Columb = score.sentiment(Obc8_01_Columb$text, pos.words,neg.words, .progress='text')
 

Obc8_01_Denver <- read.csv("F:/RES DOCS/Sentiment/tweets/DenverObc8_01.csv")
Obc8_01_Denver$text<-as.factor(Obc8_01_Denver$text)
Obc8_01.Scores$Denver = score.sentiment(Obc8_01_Denver$text, pos.words,neg.words, .progress='text')
 
Obc8_01_Louisv <- read.csv("F:/RES DOCS/Sentiment/tweets/LouisvObc8_01.csv")
Obc8_01_Louisv$text<-as.factor(Obc8_01_Louisv$text)
Obc8_01.Scores$Louisv = score.sentiment(Obc8_01_Louisv$text, pos.words,neg.words, .progress='text')

Obc8_01_Phoeni <- read.csv("F:/RES DOCS/Sentiment/tweets/PhoeniObc8_01.csv")
Obc8_01_Phoeni$text<-as.factor(Obc8_01_Phoeni$text)
Obc8_01.Scores$Phoeni = score.sentiment(Obc8_01_Phoeni$text, pos.words,neg.words, .progress='text')

Obc8_01_Housto <- read.csv("F:/RES DOCS/Sentiment/tweets/HoustoObc8_01.csv")
Obc8_01_Housto$text<-as.factor(Obc8_01_Housto$text)
Obc8_01.Scores$Housto = score.sentiment(Obc8_01_Housto$text, pos.words,neg.words, .progress='text')

Obc8_01_Memphi <- read.csv("F:/RES DOCS/Sentiment/tweets/MemphiObc8_01.csv")
Obc8_01_Memphi$text<-as.factor(Obc8_01_Memphi$text)
Obc8_01.Scores$Memphi = score.sentiment(Obc8_01_Memphi$text, pos.words,neg.words, .progress='text')

Obc8_01_Milwau <- read.csv("F:/RES DOCS/Sentiment/tweets/MilwauObc8_01.csv")
Obc8_01_Milwau$text<-as.factor(Obc8_01_Milwau$text)
Obc8_01.Scores$Milwau = score.sentiment(Obc8_01_Milwau$text, pos.words,neg.words, .progress='text')

Obc8_01_Miami <- read.csv("F:/RES DOCS/Sentiment/tweets/MiamiObc8_01.csv")
Obc8_01_Miami$text<-as.factor(Obc8_01_Miami$text)
Obc8_01.Scores$Miami = score.sentiment(Obc8_01_Miami$text, pos.words,neg.words, .progress='text')



write.csv(Obc8_01.Scores,file=paste(addr,"Obc8_01_Scores.csv",sep=""), row.names=TRUE)


?plot
#######################
##VISUALIZING
######################


########################################=======
####MULTIPLOT FUNCTION
##################################################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
###==========================================
##############################################

OScore$US <- OScore[,"Obc8_01.Scores$US$score"] 
OScore <- OScore[,"US"]

OScore$Chicag <- Obc8_01.Scores$Chicag$score

p1 <- qplot(Obc8_01.Scores$US$score,
            geom = "histogram",
            main = "US",
            xlab = "Sentiment score",
            ylab = "Frequency",
            #colour = "score", 
            aes = 3:1)

p2 <- qplot(Obc8_01.Scores$Chicag$score,
            geom = "histogram",
            main = "Chicago",
            xlab = "Sentiment score",
            ylab = "Frequency",
            #colour = "score",
            aes = 3:1)

p3 <- qplot(Obc8_01.Scores$LosAng$score,
            geom = "histogram",
            main = "Los Angeles",
            xlab = "Sentiment score",
            ylab = "Frequency",
            #colour = "score",
            aes = 3:1)

p4 <- qplot(Obc8_01.Scores$Columb$score,
            geom = "histogram",
            main = "Columbus",
            xlab = "Sentiment score",
            ylab = "Frequency",
            #colour = "score",
            aes = 3:1)



p5 <- qplot(Obc8_01.Scores$Denver$score,
            geom = "histogram",
            main = "Denver",
            xlab = "Sentiment score",
            ylab = "Frequency",
            #colour = "score",
            aes = 3:1)

p6 <- qplot(Obc8_01.Scores$Louisv$score,
            geom = "histogram",
            main = "Louisville",
            xlab = "Sentiment score",
            ylab = "Frequency",
            #colour = "score",
            aes = 3:1)

p7 <- qplot(Obc8_01.Scores$Phoeni$score,
            geom = "histogram",
            main = "Phoenix" ,
            xlab = "Sentiment score",
            ylab = "Frequency",
            #colour = "score",
            aes = 3:1)


p8 <- qplot(Obc8_01.Scores$Memphi$score,
            geom = "histogram",
            main = "Memphis",
            xlab = "Sentiment score",
            ylab = "Frequency",
            #colour = "score",
            aes = 3:1)

p9 <- qplot(Obc8_01.Scores$Milwau$score,
            geom = "histogram",
            main = "Milwaukee",
            xlab = "Sentiment score",
            ylab = "Frequency",
            #colour = "score",
            aes = 3:1)

p10 <- qplot(Obc8_01.Scores$Miami$score,
             geom = "histogram",
             main = "Miami",
             xlab = "Sentiment score",
             ylab = "Frequency",
             #colour = "score", 
             aes = 3:1)

p11 <- qplot(Obc8_01.Scores$Housto$score,
             geom = "histogram",
             main = "Houston",
             xlab = "Sentiment score",
             ylab = "Frequency",
             #colour = "score",
             aes = 3:1)




multiplot(p1, p2, p3, p4, p5, p6,p7, p8, p9, p10, p11, cols = 3)
