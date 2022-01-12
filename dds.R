library(data.table)
library(gtools)
CalculDeMatBeta <- function(vecSA,decomposition) #vecSA Nom exact, decomposition si ils vont ensembles
{
  #iNPUTS
  vecSA <- c("D01T03","D05T09","D07T08","D10T12","D17T18")
  decomposition <- c(2,1,2)
  vecSAname <- NULL
  #Loading
  p <- "E:/Mémoire/Calibration/valueadd.csv"
  df <- fread(p)
  df <- data.frame(df)
  for (i in 1:length(vecSA))
  {
    vecSAname <- c(vecSAname,df$Value.added.source.industry[which(df$SINDY==vecSA[i])[1]])
  }
  
  #Retraitement
  df <- df[,c(3,4,6,7,17)]
  df$In <- df$Value.added.source.industry
  df$Codein <- df$SINDY
  df$Codeout <- df$XINDY
  df$Out <- df$Country.of.final.demand
  df <- df[,c(5:9)]

  matC <- matrix(rep(0,length(vecSA)**2),ncol=length(vecSA))
  for (i in 1:length(vecSA))
  {
    for (j in 1:length(vecSA))
    {
      matC[i,j] <- df$Value[intersect(which(df$Codein==vecSA[i]),which(df$Codeout==vecSA[j]))]
    }
  }
  cd <- cumsum(decomposition)
  cd <- c(0,cd)
  matB  <- matrix(rep(0,length(decomposition)**2),ncol=length(decomposition))
  for (i in 1:length(decomposition))
  {
    for(j in 1:length(decomposition))
    {
      if (i==j)
      {
        sequenceinout <- (cd[i]+1):cd[i+1]
        perm <- permutations(n=length(sequenceinout),r=2,v=sequenceinout,repeats.allowed = T)
        somme <- 0
        for (k in 1:dim(perm)[1])
        {
          somme <- somme + matC[perm[k,1],perm[k,2]]
        }
        matB[i,j]<-somme
      }else
      {
        sequencein <- (cd[i]+1):cd[i+1] 
        sequenceout <- (cd[j]+1):cd[j+1] 
        somme <- 0
        for (k in 1:length(sequencein))
        {
          for(m in 1:length(sequenceout))
          {
            somme <- somme +matC[sequencein[k],sequenceout[m]]
          }
        }
        matB[i,j] <- somme
      }
    }
  }
  MatBtemp <- matB
  #Symétrisation
  for (i in 1:dim(matB)-1)
  {
    for (j in 1:(dim(matB)-i))
    {
      matB[i,j+i] <- (MatBtemp[i,j+i]+MatBtemp[j+i,i])/2
      matB[j+i,i] <- (MatBtemp[i,j+i]+MatBtemp[j+i,i])/2
    }
  }
  
  
  #VALEUR AJOUTE MATRIX symetrise
  matB
  
  #A FAIRE SUR LE NOMBRE D'ENTREPRISES
  p2 <- "E:/Mémoire/Calibration/NombreentrepriseOCDE.csv"
  df2 <- fread(p2)
  df2 <- data.frame(df2)
  #2015 année la plus recente
  
  df2 <- df2[which(df2$TIME==2015),]
  df2 <- df2[,c(2,6,8,17)]
  #Trier par secteur d'activité
  nbrentreprises  <- rep(0,length(vecSA))
  for(i in 1:length(vecSA))
  {
     
  }
  nbrfi <- sum(df2$Value[which(df2$ISIC4==SA[1])],na.rm = T)
  nbrmpci <- sum(df2$Value[which(df2$ISIC4==SA[2]|df2$ISIC4==SA[3])],na.rm = T)
  nbrpublic <- sum(df2$Value[which(df2$ISIC4==SA[4])],na.rm = T)
  nbrit<- sum(df2$Value[which(df2$ISIC4==SA[5])],na.rm = T)*0.7
  nbrmld <- (nbrit/0.7)*0.3+sum(df2$Value[which(df2$ISIC4==SA[4])],na.rm=T)*0.1
  nbrenergie <- sum(df2$Value[which(df2$ISIC4==SA[6]|df2$ISIC4==SA[7])],na.rm = T)*0.8
  
  
  nbrentreprises <- c(nbrfi,nbrit,nbrpublic,nbrmld,nbrmpci,nbrenergie)
  names(nbrentreprises) <- c("Fi","It","Public","Mld","Mpci","En")
  df3 <- data.frame(nbrentreprises)
  
  
}




list_sophos_pays <- c("Australia","Belgium","Brazil","Canada","China","Colombia","Czech Republic","France","Germany","India","Italy","Japan","Malaysia",
                      "Mexico","Netherlands","Nigeria","Philippines","Poland","Singapore","South Africa","Spain","Sweden","Turkey","UAE","United Kingdom","United States")
hit <- c(0.48,0.6,0.65,0.39,0.45,0.44,0.52,0.52,0.57,0.82,0.41,0.42,0.58,0.44,0.55,0.53,0.3,0.28,0.4,0.24,0.53,0.6,0.63,0.49,0.48,0.59)
cost <- c(1122914.16,374027.59,629770.42,404424.29,828611.80,694719.81 ,260975.12 ,474477.95,472077.84,1107407.16,443552.04,2194600.43,1059055.95,465155.11,
          860709.42,591011.54,877232.14,NA,832423.13,266817.18,283629.64,2749667.80,356818.65,696305.10,839796.42,622596.18) #en dollard MEXICO
assurance_ransomware <- c(0.65,0.69,0.68,0.62,0.82,0.57,0.48,0.61,0.50,0.8,0.68,0.58,0.57,0.61,0.64,0.45,0.49,0.44,0.56,0.70,0.70,0.75,0.57,0.55,0.7,0.75)
df2 <- data.frame(list_sophos_pays,hit,assurance_ransomware,cost)
df2











