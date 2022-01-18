library(data.table)
library(gtools)
library(deSolve)

#Inputs à avoir dans l'appli
vecSA <- c("D05T09","D10T33","D35T39","D41T43","D45T47")
decomposition <- c(1,1,1,1,1)
loigamma <- c("exp")
beta <- 0.3
choix <- 1
N=4064278
Npop=10000 #Taille pf

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

NormalizationByAll <- function(va,vecSA,decomposition,vecSAname,globalocde,paysocde,matB,ocde,loigamma)
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

    
  #alea_port_equi
  }else if(choix==3){
      repart <- runif(dim(globalocde)[1],1/(dim(globalocde)[1]+1),(1/(dim(globalocde)[1]-1)))
      repart <- repart/sum(repart)
      y0 <- c()
      for(i in 1:dim(globalocde)[1])
      {
        if(touch[i]<rand&rand<touch[i+1]){assign(paste('I', i, sep=''),1)}else{assign(paste('I', i, sep=''),0)}
        if(get(paste0("I",i))==0){assign(paste('S', i, sep=''),Npop*repart[i])}else{assign(paste('S', i, sep=''),Npop*globalocde$repart[i]-1)}
        assign(paste('R', i, sep=''), 0)
      }
      for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("S",i)))}
      for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("I",i)))}
      for(i in 1:dim(globalocde)[1]){y0 <- c(y0,get(paste0("R",i)))}
      return(y0)
  
  #ignite_port_equi 
  }else{
      repart <- runif(dim(globalocde)[1],1/(dim(globalocde)[1]+1),(1/(dim(globalocde)[1]-1)))
      repart <- repart/sum(repart)
      y0 <- c()
      alpha <- 7*10^-3
      for(i in 1:dim(globalocde)[1])
      {
        assign(paste('I', i, sep=''),Npop*repart*alpha)
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



main <- function(vecSA,decomposition) #vecSA Nom exact, decomposition si ils vont ensembles
{
  #Loading inputs
  path <- dirname(rstudioapi::getSourceEditorContext()$path) 
  va <- fread(paste0(path,"/DatabaseApres/valueadd.csv"))
  ocde <- fread(paste0(path,"/DatabaseApres/ocde.csv"))
  
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
  params <- NormalizationByAll(va,vecSA,decomposition,vecSAname,globalocde,paysocde,matB,ocde,loigamma)
  
  

  

  

  



   
  

  
  
  
  
  #4 FONCTIONS POUR ALIMENTER LE PREMIER STATE 
  # alea_punif <- function(nbrentreprises,touch)
  # {
  #   N=4064278
  #   S1 <- N*nbrentreprises[1]
  #   S2 <- N*nbrentreprises[2]
  #   S3 <- N*nbrentreprises[3]
  #   S4 <- N*nbrentreprises[4]
  #   S5 <- N*nbrentreprises[5]
  #   S6 <- N*nbrentreprises[6]
  #   I1=0;I2=0;I3=0;I4=0;I5=0;I6=0
  #   rand <- runif(1,0,1)
  #   if(rand<touch[1]){I1=1;S1=S1-1}else if(rand<touch[2]&rand>touch[1]){I2=1;S2=S2-1}else if(rand<touch[3]&rand>touch[2]){I3=1;S3=S3-1}else if(rand<touch[4]&rand>touch[3]){I4=1;S4=S4-1}else if(rand<touch[5]&rand>touch[4]){I5=1;S5=S5-1}else{I6=1;S6=S6-1}
  #   y0 <- c(S1 = S1 ,S2= S2,S3 = S3 ,S4= S4 ,S5 = S5,S6= S6,I1=I1,I2 = I2,I3=I3,I4 = I4,I5=I5,I6 = I6,R1 = 0,R2=0 ,R3 = 0, R4=0, R5 = 0, R6=0)
  #   return(y0)
  #   
  # }
  # ignite_punif<- function(nbrentreprises,touch)
  # {
  #   N=4064278
  #   S1 <- N*nbrentreprises[1]
  #   S2 <- N*nbrentreprises[2]
  #   S3 <- N*nbrentreprises[3]
  #   S4 <- N*nbrentreprises[4]
  #   S5 <- N*nbrentreprises[5]
  #   S6 <- N*nbrentreprises[6]
  #   alpha <- 7*10^-3
  #   I1=S1*alpha;I2=S2*alpha;I3=S3*alpha;I4=S4*alpha;I5=S5*alpha;I6=S6*alpha
  #   S1 <- N*nbrentreprises[1]-I1
  #   S2 <- N*nbrentreprises[2]-I2
  #   S3 <- N*nbrentreprises[3]-I3
  #   S4 <- N*nbrentreprises[4]-I4
  #   S5 <- N*nbrentreprises[5]-I5
  #   S6 <- N*nbrentreprises[6]-I6
  #   y0 <- c(S1 = S1 ,S2= S2,S3 = S3 ,S4= S4 ,S5 = S5,S6= S6,I1=I1,I2 = I2,I3=I3,I4 = I4,I5=I5,I6 = I6,R1 = 0,R2=0 ,R3 = 0, R4=0, R5 = 0, R6=0)
  #   return(y0)
  #   
  # }
  # alea_port_equi <- function(nbrentreprises,touch)
  # {
  #   N=4064278
  #   repart <- runif(6,0.12,0.2)
  #   repart <- repart/sum(repart)
  #   S1 <- N*repart[1]
  #   S2 <- N*repart[2]
  #   S3 <- N*repart[3]
  #   S4 <- N*repart[4]
  #   S5 <- N*repart[5]
  #   S6 <- N*repart[6]
  #   I1=0;I2=0;I3=0;I4=0;I5=0;I6=0
  #   rand <- runif(1,0,1)
  #   if(rand<touch[1]){I1=1;S1=S1-1}else if(rand<touch[2]&rand>touch[1]){I2=1;S2=S2-1}else if(rand<touch[3]&rand>touch[2]){I3=1;S3=S3-1}else if(rand<touch[4]&rand>touch[3]){I4=1;S4=S4-1}else if(rand<touch[5]&rand>touch[4]){I5=1;S5=S5-1}else{I6=1;S6=S6-1}
  #   y0 <- c(S1 = S1 ,S2= S2,S3 = S3 ,S4= S4 ,S5 = S5,S6= S6,I1=I1,I2 = I2,I3=I3,I4 = I4,I5=I5,I6 = I6,R1 = 0,R2=0 ,R3 = 0, R4=0, R5 = 0, R6=0)
  #   return(y0)
  # }
  # ignite_port_equi <- function(nbrentreprises,touch)
  # {
  #   N=4064278
  #   repart <- runif(6,0.12,0.2)
  #   repart <- repart/sum(repart)
  #   S1 <- N*repart[1]
  #   S2 <- N*repart[2]
  #   S3 <- N*repart[3]
  #   S4 <- N*repart[4]
  #   S5 <- N*repart[5]
  #   S6 <- N*repart[6]
  #   alpha <- 7*10^-3
  #   I1=S1*alpha;I2=S2*alpha;I3=S3*alpha;I4=S4*alpha;I5=S5*alpha;I6=S6*alpha
  #   S1 <- N*repart[1]-I1
  #   S2 <- N*repart[2]-I2
  #   S3 <- N*repart[3]-I3
  #   S4 <- N*repart[4]-I4
  #   S5 <- N*repart[5]-I5
  #   S6 <- N*v[6]-I6
  #   y0 <- c(S1 = S1 ,S2= S2,S3 = S3 ,S4= S4 ,S5 = S5,S6= S6,I1=I1,I2 = I2,I3=I3,I4 = I4,I5=I5,I6 = I6,R1 = 0,R2=0 ,R3 = 0, R4=0, R5 = 0, R6=0)
  #   return(y0)
  # }
}





df2 <- data.frame(list_sophos_pays,hit,assurance_ransomware,cost)









