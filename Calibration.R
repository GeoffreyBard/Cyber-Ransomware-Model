calibration <- function()
{
  #Information pris de l'OCDE
  p <- "E:/Download/Download Chrome/SDBS_BDI_ISIC4_11112021204502075.csv"
  df <- read.csv(p)
  indice <- which(df$TIME==2015)
  df2 <- df[indice,]
  
  nbrfi <- sum(df2$Value[which(df2$ISIC4=="Financial and insurance activities")],na.rm = T)
  
  nbrmpci <- sum(df2$Value[which(df2$ISIC4=="Manufacturing"|df2$ISIC4=="Construction")],na.rm = T)
  
  nbrpublic <- sum(df2$Value[which(df2$ISIC4=="Administrative and support service activities")],na.rm = T)
  
  
  
  nbrit<- sum(df2$Value[which(df2$ISIC4=="Information and communication")],na.rm = T)*0.7
  
  nbrmld <- (nbrit/0.7)*0.3+sum(df2$Value[which(df2$ISIC4=="Administrative and support service activities")],na.rm=T)*0.1
  
  nbrenergie <- sum(df2$Value[which(df2$ISIC4=="Mining and quarrying"|df2$ISIC4=="Electricity, gas, steam and air conditioning supply")],na.rm = T)*0.8
  
  
  nbrentreprises <- c(nbrfi,nbrit,nbrpublic,nbrmld,nbrmpci,nbrenergie)
  names(nbrentreprises) <- c("Fi","It","Public","Mld","Mpci","En")
  df3 <- data.frame(nbrentreprises)
  
  #Information pris de l'OCDE
  L1 <- c(1807540.4,73788.6,489509.6,99999.3,937980.9,44270.5)
  L2 <- c(59776.8,1449626.9,218000,69275.3,258560.5,15669.6)
  L3 <- c(66512.4,53563.8,10668188.9,101061.9,337685,25584.1)
  L4 <- c(20919.7,47420.3,158868.1,2066784.1,181497.6,9201.7)
  L5 <- c(69686.2,159807.3,1006409.4,243677.8,14107528.2,118965.5)
  L6 <- c(28496.8,43066.9,439927.1,93020.9,1519799.6,986841.4)
  MatB <- matrix(c(L1,L2,L3,L4,L5,L6), nrow = 6, ncol=6, byrow=TRUE)
  MatBtemp <- MatB
  
  #Symétrie de la matrice car flux osef
  
  for (i in 1:5)
  {
    for (j in 1:(6-i))
    {
      MatB[i,j+i] <- (MatBtemp[i,j+i]+MatBtemp[j+i,i])/2
      MatB[j+i,i] <- (MatBtemp[i,j+i]+MatBtemp[j+i,i])/2
    }
  }
  
  #Moyenne par nombre entreprises
  
  for (i in 1:6)
  {
    for (j in 1:6)
    {
      MatB[i,j] <- MatB[i,j]/(nbrentreprises[i]+nbrentreprises[j])
    }
  }
  
  
  
  #Pour Mp et Ci
  nbrmp <- 648
  nbrci <- 547
  summ <- nbrmp+nbrci
  coef1 <- nbrmp/summ
  coef2 <- nbrci/summ
  
  
  #Secteur touch by ransomware
  touch_mpci <- (0.46*coef1+0.49*coef2)
  touch <- c(0.48,0.56,0.45,0.6,touch_mpci,0.55)
  names(touch) <- names(nbrentreprises) 
  #Secteur Couvert assurance by ransomware
  couvert_mpci <- 0.63*coef1+0.62*coef2
  couvert <- c(0.72,0.7,0.51,0.63,couvert_mpci,0.62)
  names(couvert) <- names(nbrentreprises) 
  
  #Normalization vecteur
  touch <- touch/sum(touch)
  couvert <- couvert/sum(couvert)
  
  for (i in 1:6)
  {
    for (j in 1:6)
    {
      MatB[i,j] <- MatB[i,j]*(1+(touch[i]+touch[j]))
    }
  }
  
  
  
  #Normalisation 
  MatB <- MatB/sum(MatB)
  
  
  
  #Gamma proche de 1 jour avec loi Exp 
  Gamma <- rep(1,6)
  for (i in 1:6)
  {
    Gamma[i] <- Gamma[i]+rexp(1,rate=1/couvert[i])
  }
  
  
  #Normalisation Nbrentreprise
  nbrentreprises <- nbrentreprises/sum(nbrentreprises)
  
  #Sum touch pour proba dans SIR
  touch[2] <- touch[1]+touch[2]
  touch[3] <- touch[2]+touch[3]
  touch[4] <- touch[3]+touch[4]
  touch[5] <- touch[4]+touch[5]
  touch[6] <- touch[5]+touch[6]
  
  
  
  MatB*2.556*10**-7
  df <- c(MatB,Gamma,touch,nbrentreprises)
  
  

  return(df)
}