setwd("C:/Users/Sophia/Dropbox/projects/Fish_GCB")
library(caret)
library(ggplot2)

brGCB<-read.csv("BR_GCB.csv")
brGCB$size<-as.numeric(brGCB$size)
brGCB$habitatType<-gsub("[p][l][0-9]","pool",brGCB$habitat)
brGCB$habitatType<-gsub("[r][f][0-9]","riffle",brGCB$habitatType)
brGCB$habitatType<-gsub("[r][u][n][0-9]","run",brGCB$habitatType)
cysp<-brGCB[(brGCB$species == "cysp"),]
cysp$GCB_norm<-(cysp$GCB-mean(cysp$GCB))/sd(cysp$GCB)
etsp<-brGCB[(brGCB$species == "etsp"),]
etsp$GCB_norm<-(etsp$GCB-mean(etsp$GCB))/sd(etsp$GCB)
etca<-brGCB[(brGCB$species == "etca"),]
etca$GCB_norm<-(etca$GCB-mean(etca$GCB))/sd(etca$GCB)
etbl<-brGCB[(brGCB$species == "etbl"),]
etbl$GCB_norm<-(etbl$GCB-mean(etbl$GCB))/sd(etbl$GCB)
nobo<-brGCB[(brGCB$species == "nobo"),]
nobo$GCB_norm<-(nobo$GCB-mean(nobo$GCB))/sd(nobo$GCB)
g<-ggplot(data = etbl, aes(y = GCB, x = habitatType))
g<-g+geom_point(aes(color = season, alpha = 0.4), size = 4)
g
tapply(brGCB$GCB, brGCB$species, length)


hzGCB<-read.csv("HZ_GCB.csv")
hzGCB$size<-as.numeric(hzGCB$size)
hzGCB$habitatType<-gsub("[p][l][0-9]","pool",hzGCB$habitat)
hzGCB$habitatType<-gsub("[r][f][0-9]","riffle", hzGCB$habitatType)
hzGCB$habitatType<-gsub("[r][u][n][0-9]","run", hzGCB$habitatType)
hzGCB$habitatType<-gsub("riffle ","riffle", hzGCB$habitatType)
hzGCB$habitatType<-gsub("run ","run", hzGCB$habitatType)
tapply(hzGCB$GCB, hzGCB$habitatType, length)
HZcoba<-hzGCB[(hzGCB$species == "coba"),]
HZetca<-hzGCB[(hzGCB$species == "etca"),]
HZette<-hzGCB[(hzGCB$species == "ette"),]
HZetfl<-hzGCB[(hzGCB$species == "etfl"),]
HZluzo<-hzGCB[(hzGCB$species == "luzo"),]

g<-ggplot(data = HZcoba, aes(y = GCB, x = habitatType))
g<-g+geom_point(aes(color = season, alpha = 0.4), size = 4)
g
g<-ggplot(data = HZetca, aes(y = GCB, x = habitatType))
g<-g+geom_point(aes(color = season, alpha = 0.4), size = 4)
g

trCtr<-trainControl(method = "CV", number = 5)
fit<-train(GCB~habitatType+season+size, 
            data = HZluzo, 
            preProcess = (c("center", "scale")),
            method = "glm",
            trControl = trCtr
                        )
fit

#just etbl
 
