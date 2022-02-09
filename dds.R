library(data.table)
library(ggplot2)
library(gtools)
library(deSolve)
library(RColorBrewer)
library(gridExtra)
library(doParallel)
library(compiler)
#Inputs à avoir dans l'appli


#FUNCTION
CalculMatriceBeta <- function(va,vecSA,decomposition)
{
  #Get from csv
  matC <- matrix(rep(0,length(vecSA)**2),ncol=length(vecSA))
  for (i in 1:length(vecSA))
  {
    for (j in 1:length(vecSA))
    {
      matC[i,j] <- va$Value[intersect(which(va$Codein==vecSA[i]),which(va$Codeout==vecSA[j]))]
    }
  }
  #Manage decomposition (unique pour l'instant)
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
  return(matB)
}

global_ocde <- function(ocde,vecSA,vecSAname)
{
  nbre_global <- unlist(lapply(vecSA, function(x){sum(ocde$Value[which(ocde$Code==x&ocde$Variable=="Number of enterprises")],na.rm = T)}))
  
  turnover_global <- unlist(lapply(vecSA, function(x){sum(ocde$Value[which(ocde$Code==x&ocde$Variable=="Turnover")],na.rm = T)}))
  
  employe_global <- unlist(lapply(vecSA, function(x){sum(ocde$Value[which(ocde$Code==x&ocde$Variable=="Number of employees")],na.rm = T)}))
  
  df <- data.frame("Secteur"=vecSAname,"Code"=vecSA,"Entreprises"=nbre_global,"EntreprisesNorm"=nbre_global/sum(nbre_global),"Turnover"=turnover_global,"Employees"=employe_global)
  
  #Sophois touch et couvert
  touch <- unlist(lapply(1:length(vecSA), function(x){ocde$touch[which(df$Code[x]==ocde$Code)[1]]}))
  
  touchNormCum <- cumsum(touch/sum(touch))
  
  couvert <- unlist(lapply(1:length(vecSA), function(x){ocde$couvert[which(df$Code[x]==ocde$Code)[1]]}))
  
  couvertNormCum <- cumsum(couvert/sum(couvert))
  
  #Coutsecteur
  coutsecteur <- df$Turnover/df$Entreprises
  coutsecteur <- coutsecteur/mean(coutsecteur)
  
  df <- data.frame(df,touch,touchNormCum,couvert,couvertNormCum,coutsecteur)
  
  #Color Graph
  redcolor <- c(brewer.pal(n = 9, name = "Reds"),"#400000")
  bluecolor <- c(brewer.pal(n = 9, name = 'Blues'),"#03095B")
  greencolor <-  c(brewer.pal(n = 9, name = 'Greens'),"#01300E")
  sacolor <-  c(brewer.pal(n = 10, name = 'Spectral'))
  
  df <- data.frame(df,"redcolor"=redcolor[seq(1,10,by=(10/dim(df)[1]))],"bluecolor"=bluecolor[seq(1,10,by=(10/dim(df)[1]))],"greencolor"=greencolor[seq(1,10,by=(10/dim(df)[1]))],"sacolor"=greencolor[seq(1,10,by=(10/dim(df)[1]))])
  
  return(df)
}

pays_ocde <- function(ocde,vecSA,vecSAname)
{
  
  list_pays <- unique(ocde$Country[order(ocde$Country)])
  
  nbre_pays <- unlist(lapply(list_pays, function(x){sum(ocde$Value[which(ocde$Country==x&ocde$Variable=="Number of enterprises")],na.rm = T)}))
  
  turnover_pays <- unlist(lapply(list_pays, function(x){sum(ocde$Value[which(ocde$Country==x&ocde$Variable=="Turnover")],na.rm = T)}))
  
  employe_pays <- unlist(lapply(list_pays, function(x){sum(ocde$Value[which(ocde$Country==x&ocde$Variable=="Number of employees")],na.rm = T)}))
  
  df <- data.frame("Pays"=list_pays,"Entreprises"=nbre_pays,"Turnover"=turnover_pays,"Employees"=employe_pays)
  
  list_sophos_pays <- c("Australia","Belgium","Brazil","Canada","China","Colombia","Czech Republic","France","Germany","India","Italy","Japan","Malaysia",
                        "Mexico","Netherlands","Nigeria","Philippines","Poland","Singapore","South Africa","Spain","Sweden","Turkey","UAE","United Kingdom","United States")
  hit <- c(0.48,0.6,0.65,0.39,0.45,0.44,0.52,0.52,0.57,0.82,0.41,0.42,0.58,0.44,0.55,0.53,0.3,0.28,0.4,0.24,0.53,0.6,0.63,0.49,0.48,0.59)
  cost <- c(1122914.16,374027.59,629770.42,404424.29,828611.80,694719.81 ,260975.12 ,474477.95,472077.84,1107407.16,443552.04,2194600.43,1059055.95,465155.11,
            860709.42,591011.54,877232.14,780351.1,832423.13,266817.18,283629.64,2749667.80,356818.65,696305.10,839796.42,622596.18) 
  df2 <- data.frame("Pays"=list_sophos_pays,hit,cost)
  return(merge(df,df2,by="Pays"))
}

NormalizationByAll <- function(va,vecSA,decomposition,vecSAname,globalocde,paysocde,matB,ocde,loigamma,beta)
{
  #PREMIERE NORMALIZATION BY NBR D ENTREPRISES DES DEUX SECTEURS, LA SYMETRIE EST CONSERVE AINSI QUE DE TOUCH SUR SOPHOS
  for (i in 1:length(vecSA))
  {
    for (j in 1:length(vecSA))
    {
      matB[i,j] <- matB[i,j]/(globalocde$Entreprises[i]+globalocde$Entreprises[j])
      matB[i,j] <- matB[i,j]*(1+(ocde$touch[which(ocde$Code==vecSA[i])[1]]+ocde$touch[which(ocde$Code==vecSA[j])[1]]))
    }
  }
  
  #LOI A DEFINIR ET JUSTIFIE + COUVERT SELON SOPHOS 
  Gamma <- rep(1,length(vecSA))
  if(loigamma=="exp"){
    for(i in 1:length(vecSA))
    {
      Gamma[i] <- Gamma[i]+rexp(1,rate=1/ocde$couvert[which(ocde$Code==vecSA[i])[1]])
    }
  }
  
  #Normalization Beta
  matB <- matB/sum(matB)
  
  #Value Beta
  matB <-beta*matB
  
  
  
  return(list(matB,Gamma))
  
}

Ajout_sophos_touch_couvert<- function(ocde)
{
  #Touch et couvert selon sophos
  indicesa <- sapply(1:10,function(x){which(ocde$Name==unique(ocde$Name)[x])})
  ocde <- data.frame(ocde,"touch"=rep(NA,dim(ocde)[1]),"couvert"=rep(NA,dim(ocde)[1]))
  touch_sophos <- c(0.55,0.46,0.55,0.49,0.49,0.49,0.5,0.56,0.6,0.54)
  couvert_sophos <- c(0.62,0.63,0.62,0.62,0.62,0.62,0.68,0.71,0.66,0.61)
  for(i in 1:10)
  {
    ocde$touch[unlist(indicesa[i])]=touch_sophos[i];
    ocde$couvert[unlist(indicesa[i])]=couvert_sophos[i]
  }
  
  return(ocde)
}

ignitepf <- function(choix,globalocde,Npop)
{
  #Alea punif REPRESENTATIF DE LA POPULATION ACTUELLE, ET INCENDIE 
  if(choix==1)
  {
    rand <- runif(1,0,1)
    touch <- c(0,globalocde$touchNormCum)
    y0 <- c()
    for(i in 1:dim(globalocde)[1])
    {
      if(touch[i]<rand&rand<touch[i+1]){assign(paste('I', i, sep=''),1)}else{assign(paste('I', i, sep=''),0)}
      if(get(paste0("I",i))==0){assign(paste('S', i, sep=''),Npop*globalocde$EntreprisesNorm[i])}else{assign(paste('S', i, sep=''),Npop*globalocde$EntreprisesNorm[i]-1)}
      assign(paste('R', i, sep=''), 0)
    }
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("S",i)))}
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("I",i)))}
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("R",i)))}
    return(y0)
    
    #ignite_punif
  }else if(choix==2){

    repart <- runif(dim(globalocde)[1],1/(dim(globalocde)[1]+1),(1/(dim(globalocde)[1]-1)))
    repart <- repart/sum(repart)
    touch <- c(0,globalocde$touchNormCum)
    rand <- runif(1,0,1)
    y0 <- c()
    for(i in 1:dim(globalocde)[1])
    {
      if(touch[i]<rand&rand<touch[i+1]){assign(paste('I', i, sep=''),1)}else{assign(paste('I', i, sep=''),0)}
      if(get(paste0("I",i))==0){assign(paste('S', i, sep=''),Npop*repart[i])}else{assign(paste('S', i, sep=''),Npop*repart[i]-1)}
      assign(paste('R', i, sep=''), 0)
    }
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("S",i)))}
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("I",i)))}
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("R",i)))}
    return(y0)
    
    #alea_port_equi
  }else if(choix==3){

    repart <- runif(dim(globalocde)[1],1/(dim(globalocde)[1]+1),(1/(dim(globalocde)[1]-1)))
    repart <- repart/sum(repart)
    y0 <- c()
    alpha <- 7*10^-3
    for(i in 1:dim(globalocde)[1])
    {
      assign(paste('I', i, sep=''),Npop*repart[i]*alpha)
      assign(paste('S', i, sep=''),Npop*globalocde$EntreprisesNorm[i]-get(paste0("I",i)))
      assign(paste('R', i, sep=''), 0)
    }
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("S",i)))}
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("I",i)))}
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("R",i)))}
    return(y0)
    #ignite_port_equi 
  }else{
    alpha <- 7*10^-3
    y0 <- c()
    for(i in 1:dim(globalocde)[1])
    {
      assign(paste('I', i, sep=''),Npop*globalocde$EntreprisesNorm[i]*alpha)
      assign(paste('S', i, sep=''),Npop*globalocde$EntreprisesNorm[i]-get(paste0("I",i)))
      assign(paste('R', i, sep=''), 0)
    }
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("S",i)))}
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("I",i)))}
    for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("R",i)))}
    return(y0)
  }
  
}

sirmg2 <- function (t, state, parameters)
{
  ## Initialize S I R à T=0 and N
  N=0
  taille <- length(state)/3
  for(i in 1:taille)
  {
    assign(paste('S', i, sep=''), state[i])
    assign(paste('I', i, sep=''), state[i+taille])
    assign(paste('R', i, sep=''), state[i+(taille*2)])
    N=N+state[i]+ state[i+taille]
  }
  
  ##Initialize Beta
  beta <- NULL
  for(i in 1:taille**2)
  {
    beta <- c(beta,parameters[i])
  }
  beta <- matrix(beta,taille,taille)
  
  #Initialize Gamma
  gamma <- parameters[((taille**2)+1):((taille**2)+1+taille)]
  
  
  ## now code the model equations
  dxdt <- NULL
  for(i in 1:taille)
  {
    assign(paste0('dS', i, "dt"), -1*get(paste0("S",i))*(sum(sapply(1:taille,function(x){beta[i,x]*get(paste0("I",x))}))))
    assign(paste0('dI', i, "dt"), get(paste0("S",i))*(sum(sapply(1:taille,function(x){beta[i,x]*get(paste0("I",x))})))-gamma[i]*get(paste0("I",i)))
    assign(paste0('dR', i, "dt"), gamma[i]*get(paste0("I",i)))
  }
  
  for(i in 1:taille)
  {
    dxdt <- c(dxdt,get(paste0('dS', i, "dt")))
  }
  for(i in 1:taille)
  {
    dxdt <- c(dxdt,get(paste0('dI', i, "dt")))
  }
  for(i in 1:taille)
  {
    dxdt <- c(dxdt,get(paste0('dR', i, "dt")))
  }
  ## return result as a list!
  list(dxdt)
}

calcul_out <- function(y0,times,parms)
{
  out <- data.frame(ode(func = sirmg2,y = y0,times = times,parms = unlist(parms),method = 'lsodar'))
  names(out) <- c("time",unlist(lapply(1:(length(y0)/3),function(x){paste0("S",x)})),unlist(lapply(1:(length(y0)/3),function(x){paste0("I",x)})),unlist(lapply(1:(length(y0)/3),function(x){paste0("R",x)})))
  out$S <- rowSums(out[,2:(length(y0)/3+1)])
  out$I <- rowSums(out[,(length(y0)/3+2):(2*(length(y0)/3)+1)])
  out$R <- rowSums(out[,(2*(length(y0)/3)+2):(3*(length(y0)/3)+1)])
  return(out)
}

graph_infected <- function(choix,out,globalocde,times,y0)
{
  #IGNITE
  color <- as.vector(globalocde$sacolor)
  names(color) <- globalocde$Secteur
  if (choix==2|choix==4)
  {
    g1<- ggplot(out, aes(x = times))
    for(i in 1:dim(globalocde)[1])
    {
      if(i==1){g1 <- g1+geom_line(aes(y=I1,color=names(color)[1]),size = 1.5)}
      if(i==2){g1 <- g1+geom_line(aes(y=I2,color=names(color)[2]),size = 1.5)}
      if(i==3){g1 <- g1+geom_line(aes(y=I3,color=names(color)[3]),size = 1.5)}
      if(i==4){g1 <- g1+geom_line(aes(y=I4,color=names(color)[4]),size = 1.5)}
      if(i==5){g1 <- g1+geom_line(aes(y=I5,color=names(color)[5]),size = 1.5)}
      if(i==6){g1 <- g1+geom_line(aes(y=I6,color=names(color)[6]),size = 1.5)}
      if(i==7){g1 <- g1+geom_line(aes(y=I7,color=names(color)[7]),size = 1.5)}
      if(i==8){g1 <- g1+geom_line(aes(y=I8,color=names(color)[8]),size = 1.5)}
      if(i==9){g1 <- g1+geom_line(aes(y=I9,color=names(color)[9]),size = 1.5)}
      if(i==10){g1 <- g1+geom_line(aes(y=I10,color=names(color)[10]),size = 1.5)}
    }

    g1 <-  g1 + labs(x = 'jours',y = "Nombre d'entreprises infectées",color = "Legend")+ylim(c(0,max(out[,(dim(globalocde)[1]+2):(dim(globalocde)[1]*2+1)])))
    
    g2<- ggplot(out, aes(x = time)) + 
      geom_line(aes(y=I,color="Infectés cumulés"),size = 1.5) + 
      labs(x = 'jours',y = "Nombre d'entreprises infectées")
    
    return(list(g1,g2))    
    #ALEA
  }else{
    
    infect <- paste("Le premier infecté appartient à",globalocde$Secteur[which(y0[(dim(globalocde)[1]+1):(dim(globalocde)[1]*2)]==1)])

    g3<- ggplot(out, aes(x = time)) 
    for(i in 1:dim(globalocde)[1])
    {
      if(i==1){g3 <- g3+geom_line(aes(y=I1,color=names(color)[1]),size = 1.5)}
      if(i==2){g3 <- g3+geom_line(aes(y=I2,color=names(color)[2]),size = 1.5)}
      if(i==3){g3 <- g3+geom_line(aes(y=I3,color=names(color)[3]),size = 1.5)}
      if(i==4){g3 <- g3+geom_line(aes(y=I4,color=names(color)[4]),size = 1.5)}
      if(i==5){g3 <- g3+geom_line(aes(y=I5,color=names(color)[5]),size = 1.5)}
      if(i==6){g3 <- g3+geom_line(aes(y=I6,color=names(color)[6]),size = 1.5)}
      if(i==7){g3 <- g3+geom_line(aes(y=I7,color=names(color)[7]),size = 1.5)}
      if(i==8){g3 <- g3+geom_line(aes(y=I8,color=names(color)[8]),size = 1.5)}
      if(i==9){g3 <- g3+geom_line(aes(y=I9,color=names(color)[9]),size = 1.5)}
      if(i==10){g3 <- g3+geom_line(aes(y=I10,color=names(color)[10]),size = 1.5)}
      
    }
    g3<-g3+labs(x = 'jours',y = "Nombre d'entreprises infectées",color = "Legend",caption=infect)+ylim(c(0,max(out[,(dim(globalocde)[1]+2):(dim(globalocde)[1]*2+1)])))
    
    g4<- ggplot(out, aes(x = time)) + 
      geom_line(aes(y=I,color="Infectés cumulés"),size = 1.5) + 
      labs(x = 'jours',y = "Nombre d'entreprises infectées")
    return(list(g3,g4))
  }
}

#FORMULE TAILLE FINALE EPIDEMIE
Nbr_infectes_SA <- function(y0,MatB,gamma,k,X)
{
  c <- 0
  for (l in 1:dim(MatB)[1])
  {
    c <- c+(MatB[k,l]/gamma[l])*(X[l]-y0[l])
  }
  d <-0
  for (l in 1:dim(MatB)[1])
  {
    d <- d+(MatB[k,l]/gamma[l])*y0[dim(MatB)[1]+l]
  }
  X[k] <- y0[k]*exp(c)-d
  return(X)
}

Calcul_taille <- function(X,y0,MatB,gamma,nbr)
{
  for (j in 1:nbr)
  {
    for (i in 1:dim(MatB)[1])
    {
      X <- Nbr_infectes_SA(y0,MatB,gamma,i,X)
    }
  }
  return(X)
}

#PORTEFEUILLE REPRESENTATIF DE LA COMPOSITION ACTUELLE
portA <- function(globalocde,paysocde,pf,prop_SA)
{
  #CONSTRUCTION DU PF
  pA <- matrix(rep(0,dim(paysocde)[1]*dim(globalocde)[1]), nrow = dim(globalocde)[1], ncol=dim(paysocde)[1], byrow=TRUE)
  for(i in 1:dim(globalocde)[1])
  {
    for(j in 1:dim(paysocde)[1])
    {
      pA[i,j] <- globalocde$Entreprises[i]*paysocde$Entreprises[j]
    }
  }
  pA <- pA/sum(pA)*pf #Taille
  #QUI EST TOUCHE
  touchA <- matrix(rep(0,dim(paysocde)[1]*dim(globalocde)[1]), nrow = dim(globalocde)[1], ncol=dim(paysocde)[1], byrow=TRUE)
  for(i in 1:dim(globalocde)[1])
  {
    for(j in 1:dim(paysocde)[1])
    {
      touchA[i,j] <- prop_SA[i]*pA[i,j]
    }
  }
  coutA <-  matrix(rep(0,dim(paysocde)[1]*dim(globalocde)[1]), nrow = dim(globalocde)[1], ncol=dim(paysocde)[1], byrow=TRUE)
  for(i in 1:dim(globalocde)[1])
  {
    for(j in 1:dim(paysocde)[1])
    {
      coutA[i,j] <- (globalocde$coutsecteur[i]*paysocde$cost[j])*touchA[i,j]
    }
  }
  return(list(pA,touchA,coutA))
  
}

#PORTEFEUILLE ALEATOIRE
portB <- function(globalocde,paysocde,pf,prop_SA)
{
  #CONSTRUCTION DU PF
  pB <- matrix(rep(0,dim(paysocde)[1]*dim(globalocde)[1]), nrow = dim(globalocde)[1], ncol=dim(paysocde)[1], byrow=TRUE)
  repart_SA <- runif(dim(globalocde)[1],0,1)
  repart_SA <- repart_SA/sum(repart_SA)
  for(i in 1:dim(globalocde)[1])
  {
    for(j in 1:dim(paysocde)[1])
    {
      pB[i,j] <- repart_SA[i]*paysocde$Entreprises[j]
    }
  }
  pB <- pB/sum(pB)*pf #Taille
  #QUI EST TOUCHE
  touchB <- matrix(rep(0,dim(paysocde)[1]*dim(globalocde)[1]), nrow = dim(globalocde)[1], ncol=dim(paysocde)[1], byrow=TRUE)
  for(i in 1:dim(globalocde)[1])
  {
    for(j in 1:dim(paysocde)[1])
    {
      touchB[i,j] <- prop_SA[i]*pB[i,j]
    }
  }
  coutB <-  matrix(rep(0,dim(paysocde)[1]*dim(globalocde)[1]), nrow = dim(globalocde)[1], ncol=dim(paysocde)[1], byrow=TRUE)
  for(i in 1:dim(globalocde)[1])
  {
    for(j in 1:dim(paysocde)[1])
    {
      coutB[i,j] <- (globalocde$coutsecteur[i]*paysocde$cost[j])*touchB[i,j]
    }
  }
  return(list(pB,touchB,coutB))
  
}

#PortefeuilleC Uniquement Français, sur les 2 premières sociétés

portC <- function(globalocde,paysocde,pf,Country_portC,SA_portC,prop_SA)
{
  #Recuperer name
  nom_sa <- c("Mining and quarrying","Manufacturing","Electricity, gas, water supply, sewerage, waste and remediation services","Construction","Wholesale and retail trade; repair of motor vehicles"
              ,"Transportation and storage","Accommodation and food services","Information and communication","Real estate activities","Other business sector services")
  
  #CONSTRUCTION DU PF
  pC <- matrix(rep(0,dim(paysocde)[1]*dim(globalocde)[1]), nrow = dim(globalocde)[1], ncol=dim(paysocde)[1], byrow=TRUE)
  
  names(SA_portC) <- nom_sa
  
  indice_SA <- NULL
  for(i in 1:dim(globalocde)[1])
  {
    indice_SA <- c(indice_SA,which(names(SA_portC)==globalocde$Secteur[i]))
  }
  SA_portC <- SA_portC[indice_SA]
  if(sum(SA_portC)!=0){SA_portC <- SA_portC/sum(SA_portC)}
  for(i in 1:dim(globalocde)[1])
  {
    
    for(j in 1:length(Country_portC))
    {
        indice <- which(paysocde$Pays==Country_portC[j])
        pC[i,indice] <-paysocde$Entreprises[indice]*SA_portC[i]
    }
    
  }
  if(sum(pC)!=0)
  {
    pC <- pC/sum(pC)*pf #Taille
  }

  #QUI EST TOUCHE
  touchC <- matrix(rep(0,dim(paysocde)[1]*dim(globalocde)[1]), nrow = dim(globalocde)[1], ncol=dim(paysocde)[1], byrow=TRUE)
  for(i in 1:dim(globalocde)[1])
  {
    for(j in 1:dim(paysocde)[1])
    {
      touchC[i,j] <- prop_SA[i]*pC[i,j]
    }
  }
  coutC <-  matrix(rep(0,dim(paysocde)[1]*dim(globalocde)[1]), nrow = dim(globalocde)[1], ncol=dim(paysocde)[1], byrow=TRUE)
  for(i in 1:dim(globalocde)[1])
  {
    for(j in 1:dim(paysocde)[1])
    {
      coutC[i,j] <- (globalocde$coutsecteur[i]*paysocde$cost[j])*touchC[i,j]
    }
  }
  return(list(pC,touchC,coutC))
  
}

graphpf <- function(pfA,touchA,coutA,pfB,touchB,coutB,pfC,touchC,coutC,out,Sains,times,globalocde)
{
  pfcolor = c("Portefeuille A" = "red4","Portefeuille B"= "green4","Portefeuille C"= "blue4")
  coutpf <- data.frame(portefeuille = c("Portefeuille A","Portefeuille B","Portefeuille C"),cout=c(sum(coutA),sum(coutB),sum(coutC)))
  
  #COUT DES PF
  p1<-ggplot(data=coutpf, aes(x=portefeuille, y=cout)) +
    geom_bar(stat="identity",fill = pfcolor)+    
    labs(x = 'portefeuilles',y = "Cout des portefeuilles",s = "Legend") +
    geom_text(aes(label=c("Nombre d'entreprises touchées :")), vjust=1.6, color="white", size=3.5)+
    geom_text(aes(label=c(round(sum(touchA)),round(sum(touchB)),round(sum(touchC)))), vjust=2.6, color="white", size=6.5)+
    geom_text(aes(label=c("Cout Moyen d'une entreprise :")), vjust=7.6, color="white", size=3.5)+
    geom_text(aes(label=c(round(sum(coutA)/sum(touchA)),round(sum(coutB)/sum(touchB)),round(sum(coutC)/sum(touchC)))), vjust=5.6, color="white", size=6.5)
  
  #Assignation des variables
  dfA <- data.frame(times)
  dfB <- data.frame(times)
  dfC <- data.frame(times)
  color <- as.vector(globalocde$sacolor)
  names(color) <- globalocde$Secteur
  nA <- c("times")
  nB <- c("times")
  nC <- c("times")
  for(i in 1:length(globalocde$Secteur))
  {
    dfA <- data.frame(dfA,assign(paste('I', i, "A",sep=''), sum(pfA[i,])*out[,paste0("I",i)]/Sains[i]))
    dfB <- data.frame(dfB,assign(paste('I', i, "B",sep=''), sum(pfB[i,])*out[,paste0("I",i)]/Sains[i]))
    dfC <- data.frame(dfC,assign(paste('I', i, "C",sep=''), sum(pfC[i,])*out[,paste0("I",i)]/Sains[i]))
    nA<-c(nA,paste0('I', i, "A"))
    nB<-c(nB,paste0('I', i, "B"))
    nC<-c(nC,paste0('I', i, "C"))
  }
  
  names(dfA) <- nA
  names(dfB) <- nB
  names(dfC) <- nC

  gA<- ggplot(dfA, aes(x = times))
  gB<- ggplot(dfB, aes(x = times))
  gC<- ggplot(dfC, aes(x = times))
  for(i in 1:dim(globalocde)[1])
  {
    if(i==1){gA <- gA+geom_line(aes(y=I1A,color=names(color)[1]),size = 1.5);gB <- gB+geom_line(aes(y=I1B,color=names(color)[1]),size = 1.5);gC <- gC+geom_line(aes(y=I1C,color=names(color)[1]),size = 1.5)}
    if(i==2){gA <- gA+geom_line(aes(y=I2A,color=names(color)[2]),size = 1.5);gB <- gB+geom_line(aes(y=I2B,color=names(color)[2]),size = 1.5);gC <- gC+geom_line(aes(y=I2C,color=names(color)[2]),size = 1.5)}
    if(i==3){gA <- gA+geom_line(aes(y=I3A,color=names(color)[3]),size = 1.5);gB <- gB+geom_line(aes(y=I3B,color=names(color)[3]),size = 1.5);gC <- gC+geom_line(aes(y=I3C,color=names(color)[3]),size = 1.5)}
    if(i==4){gA <- gA+geom_line(aes(y=I4A,color=names(color)[4]),size = 1.5);gB <- gB+geom_line(aes(y=I4B,color=names(color)[4]),size = 1.5);gC <- gC+geom_line(aes(y=I4C,color=names(color)[4]),size = 1.5)}
    if(i==5){gA <- gA+geom_line(aes(y=I5A,color=names(color)[5]),size = 1.5);gB <- gB+geom_line(aes(y=I5B,color=names(color)[5]),size = 1.5);gC <- gC+geom_line(aes(y=I5C,color=names(color)[5]),size = 1.5)}
    if(i==6){gA <- gA+geom_line(aes(y=I6A,color=names(color)[6]),size = 1.5);gB <- gB+geom_line(aes(y=I6B,color=names(color)[6]),size = 1.5);gC <- gC+geom_line(aes(y=I6C,color=names(color)[6]),size = 1.5)}
    if(i==7){gA <- gA+geom_line(aes(y=I7A,color=names(color)[7]),size = 1.5);gB <- gB+geom_line(aes(y=I7B,color=names(color)[7]),size = 1.5);gC <- gC+geom_line(aes(y=I7C,color=names(color)[7]),size = 1.5)}
    if(i==8){gA <- gA+geom_line(aes(y=I8A,color=names(color)[8]),size = 1.5);gB <- gB+geom_line(aes(y=I8B,color=names(color)[8]),size = 1.5);gC <- gC+geom_line(aes(y=I8C,color=names(color)[8]),size = 1.5)}
    if(i==9){gA <- gA+geom_line(aes(y=I9A,color=names(color)[9]),size = 1.5);gB <- gB+geom_line(aes(y=I9B,color=names(color)[9]),size = 1.5);gC <- gC+geom_line(aes(y=I9C,color=names(color)[9]),size = 1.5)}
    if(i==10){gA <- gA+geom_line(aes(y=I10A,color=names(color)[10]),size = 1.5);gB <- gB+geom_line(aes(y=I10B,color=names(color)[10]),size = 1.5);gC <- gC+geom_line(aes(y=I10C,color=names(color)[10]),size = 1.5)}
  }
  
  gA <-gA+labs(x = 'jours',y = "Nombre d'entreprises infectées dans portefeuille A",color = "Legend")+ylim(c(0,max(dfA[,2:dim(dfA)[2]])))
  gB <-gB+labs(x = 'jours',y = "Nombre d'entreprises infectées dans portefeuille B",color = "Legend")+ylim(c(0,max(dfB[,2:dim(dfB)[2]])))
  gC <-gC+labs(x = 'jours',y = "Nombre d'entreprises infectées dans portefeuille C",color = "Legend")+ylim(c(0,max(dfC[,2:dim(dfC)[2]])))
  
  return(list(gA,gB,gC,p1))
  
}


#MAIN
# vecSA=c("D05T09","D10T33","D35T39","D41T43","D45T47","D49T53","D55T56","D58T63","D68","D69T82")
# Npop=4064278
# beta= 1.845e-02
# loigamma="exp"
# times=40
# choix=3
# pf=10000
# Country_portC=c("France")
# SA_portC=runif(10,1,2)
simulation <- function(vecSA,Npop,beta,loigamma,times,choix,pf,Country_portC,SA_portC) #vecSA Nom exact, decomposition si ils vont ensembles
{
  #Inputs de base
  decomposition <- rep(1,length(vecSA))
  #Loading inputs
  path <- dirname(rstudioapi::getSourceEditorContext()$path) 
  va <- fread(paste0(path,"/DatabaseApres/valueadd.csv"))
  ocde <- fread(paste0(path,"/DatabaseApres/ocde.csv"))
  
  Npop <- as.numeric(Npop)
  beta <- as.numeric(beta)
  choix <- as.numeric(choix)
  pf <- as.numeric(pf)
  SA_portC <- as.numeric(SA_portC)
  times <- seq(from = 0,to = times)
  

  #Name des SA
  vecSAname <- unlist(lapply(1:length(vecSA),function(x){va$In[which(va$Codein==vecSA[x])[1]]}))

  #Sophos
  ocde <- Ajout_sophos_touch_couvert(ocde)
  
  #Datframe par Secteur 
  globalocde <- global_ocde(ocde,vecSA,vecSAname)
  
  #Dataframe par Pays
  paysocde <- pays_ocde(ocde,vecSA,vecSAname)

  #Matrice 
  matB <- CalculMatriceBeta(va,vecSA,decomposition)
  parms <- NormalizationByAll(va,vecSA,decomposition,vecSAname,globalocde,paysocde,matB,ocde,loigamma,beta)
  
  #Etat initial 
  try(y0 <- ignitepf(choix,globalocde,Npop),silent=T)
  
  #Model
  out <- calcul_out(y0,times,parms)
  
  #Graph infected
  f1 <- graph_infected(choix,out,globalocde,times,y0)  

  #Calcul TAILLE EPIDEMIE
  nbr_safe <-Calcul_taille(y0[1:dim(globalocde)[1]],y0,parms[[1]],parms[[2]],10000)  

  nbr_infect <- Npop - nbr_safe

  Sains <- unlist(lapply(1:dim(globalocde)[1],function(x){y0[x]+y0[x+dim(globalocde)[1]]}))
  nbr_infect_SA <- Sains-nbr_safe #Nbr_infectes par SA

  
  prop_SA <- nbr_infect_SA/Sains

  
  #Portefeuille
  pfA <- portA(globalocde,paysocde,pf,prop_SA)[[1]]
  touchA <- portA(globalocde,paysocde,pf,prop_SA)[[2]]
  coutA <- portA(globalocde,paysocde,pf,prop_SA)[[3]]

  pfB <- portB(globalocde,paysocde,pf,prop_SA)[[1]]
  touchB <- portB(globalocde,paysocde,pf,prop_SA)[[2]]
  coutB <- portB(globalocde,paysocde,pf,prop_SA)[[3]]
  pfC <- portC(globalocde,paysocde,pf,Country_portC,SA_portC,prop_SA)[[1]]
  touchC <- portC(globalocde,paysocde,pf,Country_portC,SA_portC,prop_SA)[[2]]
  coutC <- portC(globalocde,paysocde,pf,Country_portC,SA_portC,prop_SA)[[3]]
  
  #Graph des portefeuilles 
  f2 <- graphpf(pfA,touchA,coutA,pfB,touchB,coutB,pfB,touchC,coutC,out,Sains,times,globalocde)
  return(list(f1[[1]],f1[[2]],f2[[1]],f2[[2]],f2[[3]],f2[[4]]))

}



calculscr <- function(vecSA,Npop,beta,loigamma,times,choix,pf,Country_portC,SA_portC)
{
  decomposition <- rep(1,length(vecSA))
  #Loading inputs
  path <- dirname(rstudioapi::getSourceEditorContext()$path) 
  va <- fread(paste0(path,"/DatabaseApres/valueadd.csv"))
  ocde <- fread(paste0(path,"/DatabaseApres/ocde.csv"))
  
  Npop <- as.numeric(Npop)
  beta <- as.numeric(beta)
  choix <- as.numeric(choix)
  pf <- as.numeric(pf)
  SA_portC <- as.numeric(SA_portC)
  times <- seq(from = 0,to = times)
  
  
  #Name des SA
  vecSAname <- unlist(lapply(1:length(vecSA),function(x){va$In[which(va$Codein==vecSA[x])[1]]}))
  
  #Sophos
  ocde <- Ajout_sophos_touch_couvert_c(ocde)
  
  #Datframe par Secteur 
  globalocde <- global_ocde_c(ocde,vecSA,vecSAname)
  
  #Dataframe par Pays
  paysocde <- pays_ocde(ocde,vecSA,vecSAname)
  
  #Matrice 
  vecA <- NULL
  vecB <- NULL
  vecC <- NULL
  matB <- CalculMatriceBeta_c(va,vecSA,decomposition)
  cl <- makeCluster(detectCores()-2)
  clusterEvalQ(cl = cl, {library(dplyr),library(data.table),library(ggplot2),library(gtools),library(deSolve),library(RColorBrewer),library(gridExtra),library(doParallel),library(compiler)})
  clusterExport(cl=cl,list("vecA","vecB","vecC",'matB',"NormalizationByAll_c",
                           'ignitepf_c','va','vecSA',
                           'decomposition',"vecSAname","globalocde",
                           "paysocde","ocde",'loigamma', "choix", "Calcul_taille_c","Npop",
                           "pf","portA_c","portB_c","portC_c","times"),envir = environment())
  res <- parLapply(cl, 1:length(files), function(i)
  {
    
  
    print(i)
    parms <- NormalizationByAll_c(va,vecSA,decomposition,vecSAname,globalocde,paysocde,matB,ocde,loigamma,beta)
    try(y0 <- ignitepf_c(choix,globalocde,Npop),silent=T)
    #Model
    
    #Calcul TAILLE EPIDEMIE
    nbr_safe <-Calcul_taille_c(y0[1:dim(globalocde)[1]],y0,parms[[1]],parms[[2]],10000)  
    
    nbr_infect <- Npop - nbr_safe
    
    Sains <- unlist(lapply(1:dim(globalocde)[1],function(x){y0[x]+y0[x+dim(globalocde)[1]]}))
    
    nbr_infect_SA <- Sains-nbr_safe #Nbr_infectes par SA
    
    
    prop_SA <- nbr_infect_SA/Sains
    
    
    #Portefeuille
    coutA <- portA_c(globalocde,paysocde,pf,prop_SA)[[3]]
    
    coutB <- portB_c(globalocde,paysocde,pf,prop_SA)[[3]]
    
    coutC <- portC_c(globalocde,paysocde,pf,Country_portC,SA_portC,prop_SA)[[3]]
    
    vecA <- c(vecA, sum(coutA)) 
    vecB <- c(vecB, sum(coutB)) 
    vecC <- c(vecC, sum(coutC)) 
  }
  )
  stopCluster(cl)
  registerDoSEQ()
  print("end of clusters")
  
  SCRA <- mean(vecA) - VaR(vecA,p = 0.995)
  SCRB <- mean(vecB) - VaR(vecB,p = 0.995)
  SCRC <- mean(vecC) - VaR(vecC,p = 0.995)
  
  return(SCRA,SCRB,SRCC)
}

calculscr_c(vecSA,Npop,beta,loigamma,times,choix,pf,Country_portC,SA_portC)



CalculMatriceBeta_c <- cmpfun(CalculMatriceBeta)
global_ocde_c <- cmpfun(global_ocde)
pays_ocde_c <- cmpfun(pays_ocde)
NormalizationByAll_c <- cmpfun(NormalizationByAll)
Ajout_sophos_touch_couvert_c <- cmpfun(Ajout_sophos_touch_couvert)
ignitepf_c <- cmpfun(ignitepf)
sirmg2_c <- cmpfun(sirmg2)
calcul_out_c <- cmpfun(calcul_out)
graph_infected_c <- cmpfun(graph_infected)
Nbr_infectes_SA_c <- cmpfun(Nbr_infectes_SA)
Calcul_taille_c <- cmpfun(Calcul_taille)
portA_c <- cmpfun(portA)
portB_c <- cmpfun(portB)
portC_c <- cmpfun(portC)
graphpf_c <- cmpfun(graphpf)
simulation_c <- cmpfun(simulation)
calculscr_c <- cmpfun(calculscr)


