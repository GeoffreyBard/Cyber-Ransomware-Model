  library(deSolve)
  library(ggplot2)
  library(gridExtra)
  
  
  calibration <- function()
  {
    #Information pris de l'OCDE
    p <- "E:/Download/Download Chrome/valueadd.csv"
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
    nbrentreprisespasnorm <- nbrentreprises
    nbrentreprises <- nbrentreprises/sum(nbrentreprises)
    
    #Sum touch pour proba dans SIR
    touch[2] <- touch[1]+touch[2]
    touch[3] <- touch[2]+touch[3]
    touch[4] <- touch[3]+touch[4]
    touch[5] <- touch[4]+touch[5]
    touch[6] <- touch[5]+touch[6]
    
    
    
    MatB <- MatB*1.845*10^-5
    df <- c(MatB,Gamma,touch,nbrentreprises,nbrentreprisespasnorm)
    
    
    
    return(df)
  }
  
  sirmg2 <- function (t, state, parameters) {
    ## first extract the state variables
    S1 <- state[1]
    S2 <- state[2]
    S3 <- state[3]
    S4 <- state[4]
    S5 <- state[5]
    S6 <- state[6]
    
    I1 <- state[7]
    I2 <- state[8]
    I3 <- state[9]
    I4 <- state[10]
    I5 <- state[11]
    I6 <- state[12]
    
    R1 <- state[13]
    R2 <- state[14]
    R3 <- state[15]
    R4 <- state[16]
    R5 <- state[17]
    R6 <- state[18]
    ## now extract the parameters
    beta <- matrix(c(parameters[1],parameters[2],parameters[3],parameters[4],parameters[5],parameters[6],parameters[7],parameters[8],
                     parameters[9],parameters[10],parameters[11],parameters[12],parameters[13],parameters[14],parameters[15],parameters[16],
                     parameters[17],parameters[18],parameters[19],parameters[20],parameters[21],parameters[22],parameters[23],parameters[24],
                     parameters[25],parameters[26],parameters[27],parameters[28],parameters[29],parameters[30],parameters[31],parameters[32],
                     parameters[33],parameters[34],parameters[35],parameters[36]),6,6)
    gamma <- parameters[37:42]
    N <- sum(S1,S2,S3,S4,S5,S6)+sum(I1,I2,I3,I4,I5,I6)
    ## now code the model equations
    dS1dt <- -S1*(beta[1,1]*I1+beta[1,2]*I2+beta[1,3]*I3+beta[1,4]*I4+beta[1,5]*I5+beta[1,6]*I6)
    dS2dt <- -S2*(beta[2,1]*I1+beta[2,2]*I2+beta[2,3]*I3+beta[2,4]*I4+beta[2,5]*I5+beta[2,6]*I6)
    dS3dt <- -S3*(beta[3,1]*I1+beta[3,2]*I2+beta[3,3]*I3+beta[3,4]*I4+beta[3,5]*I5+beta[3,6]*I6)
    dS4dt <- -S4*(beta[4,1]*I1+beta[4,2]*I2+beta[4,3]*I3+beta[4,4]*I4+beta[4,5]*I5+beta[4,6]*I6)
    dS5dt <- -S5*(beta[5,1]*I1+beta[5,2]*I2+beta[5,3]*I3+beta[5,4]*I4+beta[5,5]*I5+beta[5,6]*I6)
    dS6dt <- -S6*(beta[6,1]*I1+beta[6,2]*I2+beta[6,3]*I3+beta[6,4]*I4+beta[6,5]*I5+beta[6,6]*I6)
    
    dI1dt <- S1*(beta[1,1]*I1+beta[1,2]*I2+beta[1,3]*I3+beta[1,4]*I4+beta[1,5]*I5+beta[1,6]*I6) - gamma[1] * I1
    dI2dt <- S2*(beta[2,1]*I1+beta[2,2]*I2+beta[2,3]*I3+beta[2,4]*I4+beta[2,5]*I5+beta[2,6]*I6) - gamma[2] * I2
    dI3dt <- S3*(beta[3,1]*I1+beta[3,2]*I2+beta[3,3]*I3+beta[3,4]*I4+beta[3,5]*I5+beta[3,6]*I6) - gamma[3] * I3
    dI4dt <- S4*(beta[4,1]*I1+beta[4,2]*I2+beta[4,3]*I3+beta[4,4]*I4+beta[4,5]*I5+beta[4,6]*I6) - gamma[4] * I4
    dI5dt <- S5*(beta[5,1]*I1+beta[5,2]*I2+beta[5,3]*I3+beta[5,4]*I4+beta[5,5]*I5+beta[5,6]*I6) - gamma[5] * I5
    dI6dt <- S6*(beta[6,1]*I1+beta[6,2]*I2+beta[6,3]*I3+beta[6,4]*I4+beta[6,5]*I5+beta[6,6]*I6) - gamma[6] * I6
    
    dR1dt <- gamma[1] * I1
    dR2dt <- gamma[2] * I2
    dR3dt <- gamma[3] * I3
    dR4dt <- gamma[4] * I4
    dR5dt <- gamma[5] * I5
    dR6dt <- gamma[6] * I6
    ## combine results into a single vector
    dxdt <- c(dS1dt,dS2dt,dS3dt,dS4dt,dS5dt,dS6dt,dI1dt,dI2dt,dI3dt,dI4dt,dI5dt,dI6dt,dR1dt,dR2dt,dR3dt,dR4dt,dR5dt,dR6dt)
    ## return result as a list!
    list(dxdt)
  }




p <- "E:/Download/Download Chrome/SSIS_BSC_ISIC4_14112021160808943.csv"
df <- read.csv(p)
indice <- which(df$TIME==2015)
df <- df[indice,]
list <- c(1,3,5,9,13,14,20,21,27,37,38,41,42)
pays <- unique(df$Country)[order(unique(df$Country))]
pays <- pays[list]
nbr <- c()
for (i in pays)
{
  nbr <- c(nbr,sum(df$Value[which(df$Country==i)],na.rm = T))
}
#Normalisation
nbr_PAYS <- nbr
nbr <- nbr/sum(nbr)
cout_pays <- c(1122914.16,374027.59,404424.29,260975.12,474477.95,472077.84,443552.04,2194600.43,860709.42,283629.64,2749667.8,839796.42,622770.42)
cout_moyen_1000emp <-c(505827,981140)
df_cout <- data.frame(pays = pays,nbr = nbr,cout=cout_pays)





p <- "E:/Download/Download Chrome/valueadd.csv"
df <- read.csv(p)
indice <- which(df$TIME==2015)
df2 <- df[indice,]

nbrfi <- sum(df2$Value[which(df2$ISIC4=="Financial and insurance activities")],na.rm = T)

nbrmpci <- sum(df2$Value[which(df2$ISIC4=="Manufacturing"|df2$ISIC4=="Construction")],na.rm = T)

nbrpublic <- sum(df2$Value[which(df2$ISIC4=="Administrative and support service activities")],na.rm = T)



nbrit<- sum(df2$Value[which(df2$ISIC4=="Information and communication")],na.rm = T)*0.7

nbrmld <- (nbrit/0.7)*0.3+sum(df2$Value[which(df2$ISIC4=="Administrative and support service activities")],na.rm=T)*0.1

nbrenergie <- sum(df2$Value[which(df2$ISIC4=="Mining and quarrying"|df2$ISIC4=="Electricity, gas, steam and air conditioning supply")],na.rm = T)*0.8







p <- "E:/Download/Download Chrome/SSIS_BSC_ISIC4_14112021163330219.csv"
df2 <- read.csv(p)
indice <- which(df$TIME==2015)
df2 <- df2[indice,]

tnompci <- sum(df2$Value[which(df2$ISIC4.1=="Manufacturing"|df2$ISIC4.1=="Construction")],na.rm = T)

tnopublic <- sum(df2$Value[which(df2$ISIC4.1=="Administrative and support service activities")],na.rm = T)



tnoit<- sum(df2$Value[which(df2$ISIC4.1=="Information and communication")],na.rm = T)*0.7

tnomld <- (tnoit/0.7)*0.3+sum(df2$Value[which(df2$ISIC4.1=="Administrative and support service activities")],na.rm=T)*0.1

tnoenergie <- sum(df2$Value[which(df2$ISIC4.1=="Mining and quarrying"|df2$ISIC4.1=="Electricity, gas, steam and air conditioning supply")],na.rm = T)*0.8

tnofi <- tnoit


tnofi <- tnofi/nbrfi
tnompci <- tnompci/nbrmpci
tnopublic <- tnopublic/nbrpublic
tnoit<- tnoit/nbrit
tnomld <- tnomld/nbrmld
tnoenergie <- tnoenergie/nbrenergie
cout_par_secteur <- c(tnofi,tnompci,tnopublic,tnoit,tnomld,tnoenergie)
#Seminormalization
cout_par_secteur <- cout_par_secteur/mean(cout_par_secteur)






#Initialisation
params <- calibration()
MatB <- matrix(params[1:36], nrow = 6, ncol=6, byrow=TRUE)
beta <- matrix(params[1:36], nrow = 6, ncol=6, byrow=TRUE)
gamma <- params[37:42]
touch <- params[43:48]
nbrentreprises <- params[49:54]
nbr_SA <- params[55:60]





parms <- c(beta = beta,gamma =  gamma)
# times stamps
times <- seq(from = 0,to = 100)
# initial conditions
N=4064278

alea_punif <- function(nbrentreprises,touch)
{
  N=4064278
  S1 <- N*nbrentreprises[1]
  S2 <- N*nbrentreprises[2]
  S3 <- N*nbrentreprises[3]
  S4 <- N*nbrentreprises[4]
  S5 <- N*nbrentreprises[5]
  S6 <- N*nbrentreprises[6]
  I1=0;I2=0;I3=0;I4=0;I5=0;I6=0
  rand <- runif(1,0,1)
  if(rand<touch[1]){I1=1;S1=S1-1}else if(rand<touch[2]&rand>touch[1]){I2=1;S2=S2-1}else if(rand<touch[3]&rand>touch[2]){I3=1;S3=S3-1}else if(rand<touch[4]&rand>touch[3]){I4=1;S4=S4-1}else if(rand<touch[5]&rand>touch[4]){I5=1;S5=S5-1}else{I6=1;S6=S6-1}
  y0 <- c(S1 = S1 ,S2= S2,S3 = S3 ,S4= S4 ,S5 = S5,S6= S6,I1=I1,I2 = I2,I3=I3,I4 = I4,I5=I5,I6 = I6,R1 = 0,R2=0 ,R3 = 0, R4=0, R5 = 0, R6=0)
  return(y0)
  
}
ignite_punif<- function(nbrentreprises,touch)
{
  N=4064278
  S1 <- N*nbrentreprises[1]
  S2 <- N*nbrentreprises[2]
  S3 <- N*nbrentreprises[3]
  S4 <- N*nbrentreprises[4]
  S5 <- N*nbrentreprises[5]
  S6 <- N*nbrentreprises[6]
  alpha <- 7*10^-3
  I1=S1*alpha;I2=S2*alpha;I3=S3*alpha;I4=S4*alpha;I5=S5*alpha;I6=S6*alpha
  S1 <- N*nbrentreprises[1]-I1
  S2 <- N*nbrentreprises[2]-I2
  S3 <- N*nbrentreprises[3]-I3
  S4 <- N*nbrentreprises[4]-I4
  S5 <- N*nbrentreprises[5]-I5
  S6 <- N*nbrentreprises[6]-I6
  y0 <- c(S1 = S1 ,S2= S2,S3 = S3 ,S4= S4 ,S5 = S5,S6= S6,I1=I1,I2 = I2,I3=I3,I4 = I4,I5=I5,I6 = I6,R1 = 0,R2=0 ,R3 = 0, R4=0, R5 = 0, R6=0)
  return(y0)
  
}
alea_port_equi <- function(nbrentreprises,touch)
{
  N=4064278
  repart <- runif(6,0.12,0.2)
  repart <- repart/sum(repart)
  S1 <- N*repart[1]
  S2 <- N*repart[2]
  S3 <- N*repart[3]
  S4 <- N*repart[4]
  S5 <- N*repart[5]
  S6 <- N*repart[6]
  I1=0;I2=0;I3=0;I4=0;I5=0;I6=0
  rand <- runif(1,0,1)
  if(rand<touch[1]){I1=1;S1=S1-1}else if(rand<touch[2]&rand>touch[1]){I2=1;S2=S2-1}else if(rand<touch[3]&rand>touch[2]){I3=1;S3=S3-1}else if(rand<touch[4]&rand>touch[3]){I4=1;S4=S4-1}else if(rand<touch[5]&rand>touch[4]){I5=1;S5=S5-1}else{I6=1;S6=S6-1}
  y0 <- c(S1 = S1 ,S2= S2,S3 = S3 ,S4= S4 ,S5 = S5,S6= S6,I1=I1,I2 = I2,I3=I3,I4 = I4,I5=I5,I6 = I6,R1 = 0,R2=0 ,R3 = 0, R4=0, R5 = 0, R6=0)
  return(y0)
}
ignite_port_equi <- function(nbrentreprises,touch)
{
  N=4064278
  repart <- runif(6,0.12,0.2)
  repart <- repart/sum(repart)
  S1 <- N*repart[1]
  S2 <- N*repart[2]
  S3 <- N*repart[3]
  S4 <- N*repart[4]
  S5 <- N*repart[5]
  S6 <- N*repart[6]
  alpha <- 7*10^-3
  I1=S1*alpha;I2=S2*alpha;I3=S3*alpha;I4=S4*alpha;I5=S5*alpha;I6=S6*alpha
  S1 <- N*repart[1]-I1
  S2 <- N*repart[2]-I2
  S3 <- N*repart[3]-I3
  S4 <- N*repart[4]-I4
  S5 <- N*repart[5]-I5
  S6 <- N*v[6]-I6
  y0 <- c(S1 = S1 ,S2= S2,S3 = S3 ,S4= S4 ,S5 = S5,S6= S6,I1=I1,I2 = I2,I3=I3,I4 = I4,I5=I5,I6 = I6,R1 = 0,R2=0 ,R3 = 0, R4=0, R5 = 0, R6=0)
  return(y0)
}




# y0 <- alea_punif(nbrentreprises,touch)
y0 <- ignite_punif(nbrentreprises,touch)
alea=F


out <- ode(func = sirmg2,y = y0,times = times,parms = parms,method = 'lsodar')

out <- data.frame(out)

out$S <- out$S1+out$S2+out$S3+out$S4+out$S5+out$S6
out$I <- out$I1+out$I2+out$I3+out$I4+out$I5+out$I6
out$R <- out$R1+out$R2+out$R3+out$R4+out$R5+out$R6

colors <- c("Finances/Assurances" = "red1","IT/technologies/télécoms" = "red4","Service public" = "tomato1","Medias/Loisirs" = "tomato4","Manufacture/Construction" = "violetred","Energie/Gas/Extraction" = "violetred4")


if (alea==F)
{
  g1<- ggplot(out, aes(x = time)) + 
    geom_line(aes(y=out$I1,color="Finances/Assurances"),size = 1.5) + 
    geom_line(aes(y=out$I2,color="IT/technologies/télécoms"),size = 1.5) + 
    geom_line(aes(y=out$I3,color="Service public"),size = 1.5) +
    geom_line(aes(y=out$I4,color="Medias/Loisirs"),size = 1.5) + 
    geom_line(aes(y=out$I5,color="Manufacture/Construction"),size = 1.5) +
    geom_line(aes(y=out$I6,color="Energie/Gas/Extraction"),size = 1.5) + 
    labs(x = 'jours',y = "Nombre d'entreprises infectées",color = "Legend")  +
    ylim(c(0,max(out$I1,out$I2,out$I3,out$I4,out$I5,out$I6)))+
    xlim(c(0,15))

  
  g2<- ggplot(out, aes(x = time)) + 
    geom_line(aes(y=out$I,color="I1"),size = 1.5) + 
    labs(x = 'jours',y = "Nombre d'entreprises infectées")+
    xlim(c(0,15))
  F4 <- grid.arrange(g1,g2)
  
}else{
  if (y0[7]==1)
  {
    infect <- "Le premier infecté appartient à Finances/Assurances "
  }else if(y0[8]==1)
  {
    infect <- "Le premier infecté appartient à IT/technologies/télécoms "
  }else if(y0[9]==1)
  {
    infect <- "Le premier infecté appartient à Service public "
  }else if(y0[10]==1)
  {
    infect <- "Le premier infecté appartient à Medias/Loisirs "
  }else if(y0[11]==1)
  {
    infect <- "Le premier infecté appartient à Manufacture/Construction "
  }else
  {
    infect <- "Le premier infecté appartient à Energie/Gas/Extraction "
  }
  g3<- ggplot(out, aes(x = time)) + 
    geom_line(aes(y=out$I1,color="Finances/Assurances"),size = 1.5) + 
    geom_line(aes(y=out$I2,color="IT/technologies/télécoms"),size = 1.5) + 
    geom_line(aes(y=out$I3,color="Service public"),size = 1.5) +
    geom_line(aes(y=out$I4,color="Medias/Loisirs"),size = 1.5) + 
    geom_line(aes(y=out$I5,color="Manufacture/Construction"),size = 1.5) +
    geom_line(aes(y=out$I6,color="Energie/Gas/Extraction"),size = 1.5) + 
    labs(x = 'jours',y = "Nombre d'entreprises infectées",color = "Legend",caption=infect)  +
    
    ylim(c(0,max(out$I1,out$I2,out$I3,out$I4,out$I5,out$I6)))+
    xlim(c(0,15))

  
  g4<- ggplot(out, aes(x = time)) + 
    geom_line(aes(y=out$I,color="red"),size = 1.5) + 
    labs(x = 'jours',y = "Nombre d'entreprises infectées")+
    xlim(c(0,15))
  F5 <-grid.arrange(g3,g4)
  
}

#FORMULE TAILLE FINALE EPIDEMIE
Nbr_infectes_SA <- function(y0,MatB,gamma,k,X)
{
  c <- 0
  for (l in 1:6)
  {
    c <- c+(MatB[k,l]/gamma[l])*(X[l]-y0[l])
  }
  d <-0
  for (l in 1:6)
  {
    d <- d+(MatB[k,l]/gamma[l])*y0[6+l]
  }
  X[k] <- y0[k]*exp(c)-d
  return(X)
}

Calcul_taille <- function(X,y0,MatB,gamma)
{
  for (j in 1:10000)
  {
    for (i in 1:6)
    {
      X <- Nbr_infectes_SA(y0,MatB,gamma,i,X)
    }
  }
  return(X)
}

X <- y0[1:6]
nbr_safe <-Calcul_taille(X,y0,MatB,gamma)  
N-nbr_safe #Nbr d'infectés
Sains <- c(c(out$S1[1]+out$I1[1]),
           c(out$S2[1]+out$I2[1]),
           c(out$S3[1]+out$I3[1]),
           c(out$S4[1]+out$I4[1]),
           c(out$S5[1]+out$I5[1]),
           c(out$S6[1]+out$I6[1]))
nbr_infect_SA <- Sains-nbr_safe #Nbr_infectes par SA

prop_SA <- nbr_infect_SA/Sains


cout_moyen_sanspayer <- 732520
cout_moyen_payer <- 1448458




pf=10000
repart <- runif(6,0.12,0.2)
repart <- repart/sum(repart)

nbr_SA
nbr_PAYS 

#PORTEFEUILLE UNIFORME
portfeuilleA <- matrix(rep(0,13*6), nrow = 6, ncol=13, byrow=TRUE)
for(i in 1:6)
{
  for(j in 1:13)
  {
    portfeuilleA[i,j] <- nbr_SA[i]*nbr_PAYS[j]
  }
}
portfeuilleA <- portfeuilleA/sum(portfeuilleA)*10000

touchA <- matrix(rep(0,13*6), nrow = 6, ncol=13, byrow=TRUE)
for(i in 1:6)
{
  for(j in 1:13)
  {
    touchA[i,j] <- prop_SA[i]*portfeuilleA[i,j]
  }
  
}

coutA <- matrix(rep(0,13*6), nrow = 6, ncol=13, byrow=TRUE)
for(i in 1:6)
{
  for(j in 1:13)
  {
    coutA[i,j] <- cout_par_secteur[i]*df_cout$cout[j]*touchA[i,j]
  }
}

  
#PortefeuilleB IT Finances Medias(30%) Energie, Manifacutre, Service Public(3%) 
portfeuilleB <- matrix(rep(0,13*6), nrow = 6, ncol=13, byrow=TRUE)
repart_SA <- c(0.3,0.3,1/30,0.3,1/30,1/30)
for(i in 1:6)
{
  for(j in 1:13)
  {
    portfeuilleB[i,j] <- repart_SA[i]*nbr_PAYS[j]
  }
}
portfeuilleB <- portfeuilleB/sum(portfeuilleB)*10000

touchB <- matrix(rep(0,13*6), nrow = 6, ncol=13, byrow=TRUE)
for(i in 1:6)
{
  for(j in 1:13)
  {
    touchB[i,j] <- prop_SA[i]*portfeuilleB[i,j]
  }
  
}

coutB <- matrix(rep(0,13*6), nrow = 6, ncol=13, byrow=TRUE)
for(i in 1:6)
{
  for(j in 1:13)
  {
    coutB[i,j] <- cout_par_secteur[i]*df_cout$cout[j]*touchB[i,j]
  }
}

#PortefeuilleC Uniquement Français, sur des societes IT et Finances
portfeuilleC <- matrix(rep(0,13*6), nrow = 6, ncol=13, byrow=TRUE)
nbr_SA <- c(0.5,0.5,0,0,0,0)
for(i in 1:6)
{
  for(j in 1:13)
  {
    if (j==5)
    {
      portfeuilleC[i,j] <- nbr_SA[i]*nbr_PAYS[j]
    }else
    {
      portfeuilleC[i,j] <- 0
    }
    
  }
}
portfeuilleC <- portfeuilleC/sum(portfeuilleC)*10000

touchC <- matrix(rep(0,13*6), nrow = 6, ncol=13, byrow=TRUE)
for(i in 1:6)
{
  for(j in 1:13)
  {
    touchC[i,j] <- prop_SA[i]*portfeuilleC[i,j]
  }
  
}

coutC <- matrix(rep(0,13*6), nrow = 6, ncol=13, byrow=TRUE)
for(i in 1:6)
{
  for(j in 1:13)
  {
    coutC[i,j] <- cout_par_secteur[i]*df_cout$cout[j]*touchC[i,j]
  }
}

sum(coutC)

s = c("Portefeuille A" = "red4","Portefeuille B"= "green4","Portefeuille C"= "blue4")
dfcout <- data.frame(portefeuille = c("Portefeuille A","Portefeuille B","Portefeuille C"),cout=c(sum(coutA),sum(coutB),sum(coutC)))
p1<-ggplot(data=dfcout, aes(x=portefeuille, y=cout)) +
  geom_bar(stat="identity",fill = s)+    
  labs(x = 'portefeuilles',y = "Cout des portefeuilles",s = "Legend") +
  geom_text(aes(label=c("Nombre d'entreprises touchées :")), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=c(round(sum(touchA)),round(sum(touchB)),round(sum(touchC)))), vjust=2.6, color="white", size=6.5)+
  geom_text(aes(label=c("Cout Moyen d'une entreprise :")), vjust=7.6, color="white", size=3.5)+
  geom_text(aes(label=c(round(sum(coutA)/sum(touchA)),round(sum(coutB)/sum(touchB)),round(sum(coutC)/sum(touchC)))), vjust=5.6, color="white", size=6.5)

I1A <- sum(portfeuilleA[1,])*out$I1/Sains[1]
I2A <- sum(portfeuilleA[2,])*out$I2/Sains[2]
I3A <- sum(portfeuilleA[3,])*out$I3/Sains[3]
I4A <- sum(portfeuilleA[4,])*out$I4/Sains[4]
I5A <- sum(portfeuilleA[5,])*out$I5/Sains[5]
I6A <- sum(portfeuilleA[6,])*out$I6/Sains[6]
time <- seq(from = 0,to = 100)
dfA <- data.frame(I1A,I2A,I3A,I4A,I5A,I6A,time)
colors <- c("Finances/Assurances" = "red1","IT/technologies/télécoms" = "red4","Service public" = "tomato1","Medias/Loisirs" = "tomato4","Manufacture/Construction" = "violetred","Energie/Gas/Extraction" = "violetred4")
gA<- ggplot(dfA, aes(x = time)) + 
  geom_line(aes(y=dfA$I1,color="Finances/Assurances"),size = 1.5) + 
  geom_line(aes(y=dfA$I2,color="IT/technologies/télécoms"),size = 1.5) + 
  geom_line(aes(y=dfA$I3,color="Service public"),size = 1.5) +
  geom_line(aes(y=dfA$I4,color="Medias/Loisirs"),size = 1.5) + 
  geom_line(aes(y=dfA$I5,color="Manufacture/Construction"),size = 1.5) +
  geom_line(aes(y=dfA$I6,color="Energie/Gas/Extraction"),size = 1.5) + 
  labs(x = 'jours',y = "Nombre d'entreprises infectées dans portefeuille A",color = "Legend")  +
  ylim(c(0,max(dfA$I1,dfA$I2,dfA$I3,dfA$I4,dfA$I5,dfA$I6)))+
  xlim(c(0,15))


I1B <- sum(portfeuilleB[1,])*out$I1/Sains[1]
I2B <- sum(portfeuilleB[2,])*out$I2/Sains[2]
I3B <- sum(portfeuilleB[3,])*out$I3/Sains[3]
I4B <- sum(portfeuilleB[4,])*out$I4/Sains[4]
I5B <- sum(portfeuilleB[5,])*out$I5/Sains[5]
I6B <- sum(portfeuilleB[6,])*out$I6/Sains[6]
time <- seq(from = 0,to = 100)
dfB <- data.frame(I1B,I2B,I3B,I4B,I5B,I6B,time)
colors <- c("Finances/Assurances" = "red1","IT/technologies/télécoms" = "red4","Service public" = "tomato1","Medias/Loisirs" = "tomato4","Manufacture/Construction" = "violetred","Energie/Gas/Extraction" = "violetred4")

gB<- ggplot(dfB, aes(x = time)) + 
  geom_line(aes(y=dfB$I1,color="Finances/Assurances"),size = 1.5) + 
  geom_line(aes(y=dfB$I2,color="IT/technologies/télécoms"),size = 1.5) + 
  geom_line(aes(y=dfB$I3,color="Service public"),size = 1.5) +
  geom_line(aes(y=dfB$I4,color="Medias/Loisirs"),size = 1.5) + 
  geom_line(aes(y=dfB$I5,color="Manufacture/Construction"),size = 1.5) +
  geom_line(aes(y=dfB$I6,color="Energie/Gas/Extraction"),size = 1.5) + 
  labs(x = 'jours',y = "Nombre d'entreprises infectées dans portefeuille B",color = "Legend")  +
  ylim(c(0,max(dfB$I1,dfB$I2,dfB$I3,dfB$I4,dfB$I5,dfB$I6)))+
  xlim(c(0,15))

I1C <- sum(portfeuilleC[1,])*out$I1/Sains[1]
I2C <- sum(portfeuilleC[2,])*out$I2/Sains[2]
I3C <- sum(portfeuilleC[3,])*out$I3/Sains[3]
I4C <- sum(portfeuilleC[4,])*out$I4/Sains[4]
I5C <- sum(portfeuilleC[5,])*out$I5/Sains[5]
I6C <- sum(portfeuilleC[6,])*out$I6/Sains[6]
time <- seq(from = 0,to = 100)
dfC <- data.frame(I1C,I2C,I3C,I4C,I5C,I6C,time)
colors <- c("Finances/Assurances" = "red1","IT/technologies/télécoms" = "red4","Service public" = "tomato1","Medias/Loisirs" = "tomato4","Manufacture/Construction" = "violetred","Energie/Gas/Extraction" = "violetred4")

gC<- ggplot(dfC, aes(x = time)) + 
  geom_line(aes(y=dfC$I1,color="Finances/Assurances"),size = 1.5) + 
  geom_line(aes(y=dfC$I2,color="IT/technologies/télécoms"),size = 1.5) + 
  geom_line(aes(y=dfC$I3,color="Service public"),size = 1.5) +
  geom_line(aes(y=dfC$I4,color="Medias/Loisirs"),size = 1.5) + 
  geom_line(aes(y=dfC$I5,color="Manufacture/Construction"),size = 1.5) +
  geom_line(aes(y=dfC$I6,color="Energie/Gas/Extraction"),size = 1.5) + 
  labs(x = 'jours',y = "Nombre d'entreprises infectées dans portefeuille C",color = "Legend")  +
  ylim(c(0,max(dfC$I1,dfC$I2,dfC$I3,dfC$I4,dfC$I5,dfC$I6)))+
  xlim(c(0,15))
F6 <- grid.arrange(gA,gB,gC,p1)



class <- c("Enforcer","Sniper","Clockwork"
           ,"Mechanic","Syndicate","Bodyward"
           ,"Scrap","Innovator","Academy","Protector",
           "Twinshot","Mercenary","Bruiser",
           "Mutant","Yordle","Arcanist","Chemtech","Assasin","Enchanter",
           "Challenger","Imperial","Sister","Scholar","Colossus","Socialite",
           "Transformer","Gluton","Cuddly")

classFR <- c("Justicier","Sniper","Mécanique"
           ,"Mechanic","Pègre","Garde du corps"
           ,"Ferrailleur","Innovateur","Étudiant","Protecteur",
           "Répétireur","Mercenaire","Combattant",
           "Mutant","Yordle","Arcanist","Techno-Chimiste","Assasin","Enchanter",
           "Challenger","Imperial","Soeur","Érudit","Colosse","Vedette",
           "Transformeur","Gluton","Câlin")

sort(classFR)
