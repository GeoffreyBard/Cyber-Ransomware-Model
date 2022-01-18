library(data.table)
library(gtools)

Retreatment <- function()
{
  library(data.table)
  library(gtools)
  #ValueAdd
  
  path <- dirname(rstudioapi::getSourceEditorContext()$path) #
  p <- paste0(path,"/DatabaseAvant/valueadd.csv")
  df <- fread(p)
  df <- data.frame(df)
  df <- df[,c(3,4,6,7,17)]
  df$In <- df$Value.added.source.industry
  df$Codein <- df$SINDY
  df$Codeout <- df$XINDY
  df$Out <- df$Country.of.final.demand
  df <- df[,c(5:9)]
  
  
  #TurnOver et reste
  p2 <- paste0(path,"/DatabaseAvant/turnover.csv")
  df2 <- fread(p2)
  df2 <- data.frame(df2)
  df2 <- df2[which(df2$TIME==2015),]
  #Retreatment des Variables
  df2 <- df2[,c(-1,-3,-7,-8,-9,-10,-11,-12,-17,-18,-20,-21)]
  df2 <- df2[which(df2$Variable=="Turnover"|df2$Variable=="Turnover"|df2$Variable=="Number of employees"|df2$Variable=="Number of enterprises"),]
  
  #Unit Code, currency  --< Seulement pour Turnover, car reste c'est nbr (vérifié) --> Go mettre en Euro
  df2[which(df2$Unit.Code==""),5]="CLP"
  rate <- unique(df2$Unit.Code[which(df2$Variable=="Turnover")]) #Le vide c'est Chili, le code c'est CLP
  indice <- which(df2$Variable=="Turnover")
  
  #Solution 
  p3 <- paste0(path,"/DatabaseAvant/forex.csv")
  forex <- fread(p3)
  forex <- forex[grep("USD/",forex$slug),]
  forex <- forex[unlist(sapply(intersect(unique(forex$currency),rate),function(x) grep(x,forex$currency))),]
  forex <- forex[grep("2015",forex$date),]
  rates <- data.frame("value"=rep(0,length(unique(forex$currency))),"currency"=rep(NA,length(unique(forex$currency))))
  c=1
  for (i in unique(forex$currency))
  {
    
    rates[c,1]= mean(colMeans(forex[which(forex$currency==i),c(3,6)]))
    rates[c,2]=i
    c=c+1
  }
  rates <- rbind(rates,c((7.03+6.94+6.96)/3,"HRK")) #SOMME DE CONVERSION DE JANVIER JUILLET ET DECEMBRE D APRES XE.COM from usd to hrk
  fwrite(rates, paste0(path,"/DatabaseAvant/exrate.csv"))
  
  
  #Transformation en USD
  for(i in unique(rates$currency))
  {
    for (j in length(which(df2$Unit.Code==i)))
    {
      df2$Value[which(df2$Unit.Code==i)[j]]=df2$Value[which(df2$Unit.Code==i)[j]]*(1/as.numeric(rates[which(rates$currency==i),1]))    
    }
  }
  
  #Transformation en EURO
  for(i in which(df2$Variable=="Turnover"))
  {
    df2$Value[i]=df2$Value[i]*as.numeric(rates[11,1])    
  }
  
  #On vire les currencys
  df2 <- df2[,c(-5,-6)]
  
  #Transformation en Millions
  c[which(df2$PowerCode.Code==0&df2$Variable=="Turnover")]=df2$Value[which(df2$PowerCode.Code==0&df2$Variable=="Turnover")]*10**(-6)
  #QUe units pour autre chose que turnover, donc on vire powercode
  df2 <- df2[,c(-5,-6)]
  
  #Même base de travail pour les database (en terme de secteurs d'activité)
  #Unique sur ce datbase
  sector_name1<- unique(df$In)
  sector_code1 <- unique(df$Codein)
  sector_name2<- unique(df2$ISIC4.1)
  sector_code2 <- unique(df2$ISIC4)
  
  #BASE 2 RETREATMENT SECTOR
  #D35_39
  df2$ISIC4[which(df2$ISIC4=="35"|df2$ISIC4=="36_39")]="D35T39"  
  df2$ISIC4.1[which(df2$ISIC4=="D35T39")]=unique(df$In[which(df$Codein=="D35T39")])
  
  #D69T82
  df2$ISIC4[which(df2$ISIC4=="69_75"|df2$ISIC4=="77_82")]="D69T82"  
  df2$ISIC4.1[which(df2$ISIC4=="D69T82")]=unique(df$In[which(df$Codein=="D69T82")])
  
  #REMPLACEMENT DES NOMS
  df2$ISIC4[which(df2$ISIC4=="05_09")]="D05T09"
  df2$ISIC4[which(df2$ISIC4=="10_33")]="D10T33"  
  df2$ISIC4[which(df2$ISIC4=="41_43")]="D41T43"  
  df2$ISIC4[which(df2$ISIC4=="45_47")]="D45T47"  
  df2$ISIC4[which(df2$ISIC4=="49_53")]="D49T53"  
  df2$ISIC4[which(df2$ISIC4=="55_56")]="D55T56"
  df2$ISIC4[which(df2$ISIC4=="58_63")]="D58T63"  
  df2$ISIC4[which(df2$ISIC4=="68")]="D68"  
  df2$ISIC4[which(df2$ISIC4=="05_39")]="D05T39"
  df2$ISIC4[which(df2$ISIC4=="05_82")]="D05T82"  
  df2$ISIC4[which(df2$ISIC4=="45_82")]="D45T82"  
  df2<- df2[-which(df2$ISIC4=="D05T39" | df2$ISIC4=="05_82_LESS_K" & df2$ISIC4=="45_82_LESS_K"),]
  
  #SUPPRESSION DES DEUX SEGMENTS LESS K
  indice1 <- c(which(df2$ISIC4=="05_82_LESS_K"),which(df2$ISIC4=="45_82_LESS_K"))
  df2 <- df2[-indice1,]
  
  
  #BASE1 RETREATMENT SECTOR
  indicerestant <- c(3,7,29,30,33,34,35,36,41,42)
  df <- df[unlist(lapply(sector_code1[indicerestant],function(x) which(x==df$Codein))),]
  df <- df[unlist(lapply(sector_code1[indicerestant],function(x) which(x==df$Codeout))),]
  autre <- data.frame("Value"=rep(0,11),"In"=rep("Industry (except construction)",11),"Codein"=rep("D05T39",11),"Codeout"=rep(NA,11),"Out"=rep(NA,11))
  for(i in 0:9)
  {
    indice <- ((i*3)+1):((i*3)+3)
    n <- i+1
    autre[n,1]=sum(df$Value[which(df$Codein=="D05T09"|df$Codein=="D10T33"|df$Codein=="D35T39")[indice]])
    autre[n,4]=unique(df$Codeout[which(df$Codein=="D05T09"|df$Codein=="D10T33"|df$Codein=="D35T39")[indice]])
    autre[n,5]=unique(df$Out[which(df$Codein=="D05T09"|df$Codein=="D10T33"|df$Codein=="D35T39")[indice]])
  }
  autre[11,1]=sum(c(autre[1,1],autre[2,1],autre[3,1]))
  autre[11,4]="D05T39"
  autre[11,5]=unique(df$Out[which(df$Codein=="D05T09"|df$Codein=="D10T33"|df$Codein=="D35T39")[28]])
  
  df <- rbind(df,autre)
  unique(df[,2])
  unique(df[,3])
  for(i in 1:dim(df)[1])
  {
    indice <- which(unique(df[,3])==df[i,4])
    df[i,5]=unique(df[,2])[indice]
  }
  
  
  unique(df$Codein)
  
  unique(df2$ISIC4)
  names(df2)[3:4]=c("Code","Name")
  
  fwrite(df, paste0(path,"/DatabaseApres/valueadd.csv"))
  fwrite(df2,paste0(path,"/DatabaseApres/ocde.csv"))
  
}

Retreatment()
