library(readxl)
GSR075dif_C <- read_excel("GSR075dif_C.xlsm") #adatok beolvasása
GSR075dif_C [is.na(GSR075dif_C)] <- 0 #előkészítés a PCA-hoz
GSR075dif <- GSR075dif_C[,-1] #előkészítés a PCA-hoz


tp_GSR075dif <-t(GSR075dif) #előkészítés a PCA-hoz
GSR075dif <- tp_GSR075dif [1:44,] #előkészítés a PCA-hoz

library (ggfortify)
pcaListGSR075dif <- prcomp(GSR075dif, center = TRUE, scale = FALSE) #prcomp - PCA komponens legnagyobb variancia alapján  
pcaScoresGSR075dif <- predict(pcaListGSR075dif) #score-ok kinyerése
pcaLoadingsGSR075dif <- pcaListGSR075dif$rotation #loading-ok kinyerése

pcaInfoGSR075dif <- summary(pcaListGSR075dif) #PCA összesítés
autoplot(pcaListGSR075dif) #ábrázolása a PC1-PC2 függvényében a pontokat

clusterInpGSR075dif <- data.frame (pcaListGSR075dif$x [,1:4]) #előkészítés az euklidészi távolságok kiszámításához

distGSR075dif<-dist(clusterInpGSR075dif,method = "euclidean") # euklidészi távolságok meghatározása

ahcGSR075dif<-hclust(distGSR075dif, "ward.D") #Agglomerációs hierarchikus klaszterezés az euklidészi távolságok alapján, Ward módszerével 
plot(ahcGSR075dif, main="Résztvevők csoportosítása az Euklidészi távolságok alapján,\n Ward módszerrel (GSR075dif)",xlab="Résztvevők", ylab="Magasság", sub=" ") #(dendogram)
identify(pcaListGSR075dif)

library(cluster)

mean_silhouettes<-c() # Slihouette indexek kiszámítása
for (i in 2:5){
  cuts<-cutree(ahcGSR075dif,k=i) #finomhangolás
  silhouettes<-silhouette(as.integer(cuts), distGSR075dif)
  mean_silhouettes[i-1]<-mean(silhouettes[,3])
}
plot(mean_silhouettes, xaxt='n', ylim=c(0:1), main="Silhouette átlag-indexek az Euklidészi távolságok\n alapján Ward módszerével (GSR075dif)",xlab="klaszterek száma", ylab="átlagos Silhouette index", sub=" ")
axis(side=1, labels = c("2 klaszter", "3 klaszter", "4 klaszter", "5 klaszter"), at=c(1:4)) # plot the changes of Silhouette indices
#Silhouette indexek ábrázolása

dev.off()
