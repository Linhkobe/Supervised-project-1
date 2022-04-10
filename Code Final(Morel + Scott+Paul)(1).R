library(sp)
library(rJava)
library(OpenStreetMap)
library(rgdal)
library(latticeExtra)
library(lubridate)
library(dendextend)
library(ggplot2)
library(ggdendro)
library(FactoMineR)


lambert93<-  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
wgs84    <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

############################ PROG PRINCIPAL #################################

#1. Lecture du fichier
file.name<-"rcp2.6_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20060101-21001231.txt"
varnames<-strsplit("Date Latitude Longitude Altitude  tasminAdjust tasmaxAdjust tasAdjust prtotAdjust prsnAdjust hussAdjust sfcWindAdjust",split=" ")
varnames<-unlist(varnames)
varnames<-varnames[nchar(varnames)>0]
DATA<-read.table(file=file.name,sep=";",dec=".",skip=75,header=FALSE,col.names=varnames)
View(DATA)
#2. crֳ©ation du dataframe des points spatiaux
df.points<-unique(DATA[,c(3,2)])
df.points$nval<-NA
for(i in 1: nrow(df.points))
  df.points$nval[i]<-sum(DATA$Longitude==df.points$Longitude[i] & DATA$Latitude==df.points$Latitude[i])
rownames(df.points)<-paste0("P",1:nrow(df.points))
df.points

#3. rajoute la variable 'name' dans le dataframe DATA
DATA$name<-rep(rownames(df.points),df.points$nval)
DATA$name<-factor(DATA$name,levels=paste0("P",1:45))

#3. rajoute la variable 'mois' dans le dataframe DATA
#DATA$mois<- substr(DATA$Date,5,6)
#DATA$jour<- substr(DATA$Date,7,8)
#head(DATA$mois)




DATA$Date<-ymd(DATA$Date)
DATA$Year<-strftime(DATA$Date, "%Y")
DATA$Month<-strftime(DATA$Date, "%m")
DATA$Day<-strftime(DATA$Date, "%d")

DATA1<-DATA[DATA$Year<2036 & DATA$Year>=2006,]


#Graph comparatifs avec Choix des points


GraphiqueComparatifs<-function(annיedיbut,annיefin,varname,P)
{
  resultat<-data.frame(ncol=12)
  resultat1<-data.frame(ncol=12)
  DATA1<<-DATA[DATA$Year<annיefin & DATA$Year>=annיedיbut,]
  var<-DATA1[,varname]
  tableau<<-tapply(X=as.vector(var),INDEX=list(DATA1$name,DATA1$Month),FUN="mean")
  
  for (i in P){
    x<-data.frame(tableau[i,])
    resultat<-cbind(resultat,x)
  }
  resultat<-resultat[,-1]
  colnames(resultat)=P
  
  
  ecartype<-(tapply(var,list(DATA1$name,DATA1$Month),"sd"))
  
  for (i in P){
    z<-data.frame(ecartype[i,])
    resultat1<-cbind(resultat1,z)
  }
  resultat1<-resultat1[,-1]
  test<-paste("sd",P)
  colnames(resultat1)=test
  
  suppressWarnings({if (varname == "tasAdjust"){Moyennepassי<-c(-0.3,0.5,2.2,4.1,8.2,11.2,13.4,13.2,10.3,7.1,2.6,0.7)}
    else if (varname == "sfcWindAdjust"){Moyennepassי<-c(3,3.1,3.3,3.3,2.7,2.4,2.3,2.1,2.4,2.8,2.9,3.1)}
    else if (varname == "prtotAdjust"){Moyennepassי<-c(28.7/31,26.8/28,26.6/31,44.6/30,85.3/31,70.6/30,53/31,67.2/31,64.3/30,53.1/31,38.1/30,33.3/31)}
    else {print("Pas de donnיes passיes")}})
  
  taille<-(tapply(var,list(DATA1$name,DATA1$Month),length))
  n<<-taille[1,]
  
  suppressWarnings({
    if (varname == "tasAdjust"){
      ylim<-c(-1,22)
      titre<-paste("Comparaison spatiale et temporelle des tempיratures moyennes de",annיedיbut,"א",annיefin)
      ylab<-"C°"}
    else if (varname == "tasminAdjust") {
      ylim<-c(-1,22)
      titre<-paste("Comparaison spatiale des tempיratures minimales de",annיedיbut,"א",annיefin)
      ylab<-"C°"}
    else if (varname == "tasmaxAdjust") {
      ylim<-c(0,30)
      titre<-paste("Comparaison spatiale des tempיratures maximales de",annיedיbut,"א",annיefin)
      ylab<-"C°"}
    else if (varname == "sfcWindAdjust"){ylim<-c(-1, 5)
    titre<-paste("Comparaison spatiale et temporelle de la vitesse du vent de",annיedיbut,"א",annיefin)
    ylab<-"Vitesse du vent (m/s)"}
    else if (varname == "prtotAdjust") {
      ylim<-c(-1,5)
      titre<-paste("Comparaison spatiale et temporelle des prיcipitations moyennes de",annיedיbut,"א",annיefin)
      ylab<-"Hauteur des prיcipitations en mm"}
    else if  (varname == "prsnAdjust") {
      ylim<-c(0,1)
      titre<-paste("Comparaison spatiale des prיcipitations solides moyennes de",annיedיbut,"א",annיefin)
      ylab<-"Hauteur des prיcipitations en mm"}
    else if  (varname == "hussAdjust") {
      ylim<-c(0, 0.02)
      titre<-paste("Comparaison spatiale de l'humiditי moyenne de",annיedיbut,"א",annיefin)
      ylab<-"Kg(eau)/Kg(air humide"}
    else {print("Erreur dans la saisie")}})
  
  if((varname == "tasAdjust" )||(varname == "prtotAdjust")||(varname == "sfcWindAdjust")){
    Moyennepassי<<-data.frame(Moyennepassי)
    tableaufinal<<-t(round(data.frame(resultat,Moyennepassי,resultat1,n),digits = 2))
    plot(x=c(1:12), y=Moyennepassי, col='black', ylim=ylim, type='l', xlab='Mois', ylab=ylab, main=titre)
    c<-1
    for (i in P){
      c<-1+c
      moy<-tableau[i,]
      et<-ecartype[i,]
      points(x=c(1:12), y=moy, col=c, type='l') 
      segments(x0=c(1:12),x1=c(1:12),y0=moy,y1=moy-1.96*((et)/sqrt(n)),col=c)
      segments(x0=c(1:12),x1=c(1:12),y0=moy,y1=moy+1.96*((et)/sqrt(n)),col=c)
    }
    legend("topright",legend=c("1971-2000",P),col=seq(1,length(P)+1),lty=1, cex=0.7)
    statT<<-(tableau-Moyennepassי)/((ecartype)/sqrt(n))
    statT<<-data.frame(statT)
    
    pvaleur = data.frame(matrix(
      vector(), 0, 12, dimnames=list(c(), c("1","2","3","4","5","6","7","8","9","10","11","12"))),
      stringsAsFactors=F)
    pvaleurfinal = t(data.frame(matrix(
      vector(), 0,45)))
    
    
    for (j in 1:12){
      for (i in 1:45) {
        pv<-data.frame(2*(1-pt(abs(statT[i,j]), tableaufinal[12,j])))
        pvaleur<-rbind(pvaleur,pv)
        
      }
      pvaleurfinal<-cbind(pvaleurfinal,pvaleur)
      pvaleur = data.frame(matrix(
        vector(), 0, 12, dimnames=list(c(), c("1","2","3","4","5","6","7","8","9","10","11","12"))),
        stringsAsFactors=F)
    }
    pvaleurfinal<-format(pvaleurfinal,scientific=T,digits=3)
    
    
    
    
    for(i in 1: 45){
      rownames(pvaleurfinal)<-paste0("P",1:45)
    }
    for(i in 1:12){
      colnames(pvaleurfinal)<-(1:12)
    }
    View(pvaleurfinal)
  }
  else {
    tableaufinal<<-t(round(data.frame(resultat,resultat1,n),digits = 2))
    ybase<-rep(0,12)
    plot(x=c(1:12), y=ybase, col='white', ylim=ylim, type='l', xlab='Mois', ylab=ylab, main=titre)
    c<-0
    for (i in P){
      c<-1+c
      moy<-tableau[i,]
      sd<-data.frame(ecartype[i,])
      et<-ecartype[i,]
      points(x=c(1:12), y=moy, col=c, type='l')
      segments(x0=c(1:12),x1=c(1:12),y0=moy,y1=moy-1.96*((et)/sqrt(n)),col=c)
      segments(x0=c(1:12),x1=c(1:12),y0=moy,y1=moy+1.96*((et)/sqrt(n)),col=c)
    }
    legend("topright",legend=P,col=seq(1,length(P)), lty=1, cex=0.7)
  }
  View(tableaufinal)
  
}


P<-c("P8","P45","P12","P8","P14") #On choisit ici les diffיrents points que l'on veut afficher
GraphiqueComparatifs(2035,2065,"tasminAdjust",P) #On choisit l'annיe de dיbut, de fin et la variable que l'on veut afficher





############################### ACP, CAH et classification des courbes  ##########################

# Les lignes de codes qui suivent permettent simplement de crיer les dataframes des diffיrentes statistiques climatiques agrיgיes par annיes 

DATAACP<-DATA[DATA$Year<2101 & DATA$Year>=2006,]
tmoy.year<-tapply(DATAACP$tasAdjust,INDEX =list(DATAACP$Year,DATAACP$name),FUN="mean")
tmin.year<-tapply(DATAACP$tasminAdjust,list(DATAACP$Year,DATAACP$name),"mean")
tmax.year<-tapply(DATAACP$tasmaxAdjust,list(DATAACP$Year,DATAACP$name),"mean")
pmoy.year<-tapply(DATAACP$prtotAdjust,list(DATAACP$Year,DATAACP$name),"mean")
sfcmoy.year<-tapply(DATAACP$sfcWindAdjust,list(DATAACP$Year,DATAACP$name),"mean")
huss.year<-tapply(DATAACP$hussAdjust,list(DATAACP$Year,DATAACP$name),"mean")

#Donnיes agrיgיs par mois
tmoy.month<-tapply(DATAACP$tasAdjust,list(paste(DATAACP$Year,DATAACP$Month),DATAACP$name),"mean")
tmin.month<-tapply(DATAACP$tasminAdjust,list(paste(DATAACP$Year,DATAACP$Month),DATAACP$name),"mean")
tmax.month<-tapply(DATAACP$tasmaxAdjust,list(paste(DATAACP$Year,DATAACP$Month),DATAACP$name),"mean")
pmoy.month<-tapply(DATAACP$prtotAdjust,list(paste(DATAACP$Year,DATAACP$Month),DATAACP$name),"mean")
sfcmoy.month<-tapply(DATAACP$sfcWindAdjust,list(paste(DATAACP$Year,DATAACP$Month),DATAACP$name),"mean")
huss.month<-tapply(DATAACP$hussAdjust,list(paste(DATAACP$Year,DATAACP$Month),DATAACP$name),"mean")

#Fonction ACP + Classification des donnיes agrיgיes par annיes ou par mois (CAH)

ACPandClassif<-function(statclim, nbdimACP, clustmethod, nbclass){
  moyenne<-apply(as.matrix(statclim),1, "mean")
  statclimcorrect<-sweep(statclim,1,moyenne,"-")
  ACP<-PCA(statclimcorrect, scale.unit = TRUE, ncp = nbdimACP, graph = TRUE)
  f<-ACP$var$coord
  Dist<-dist(f)
  CAH<-hclust(Dist, method=clustmethod)
  Classif<-cutree(CAH, nbclass)
  dendro<-ggplot(color_branches(CAH, k=nbclass, col="black"), labels=TRUE)+ggtitle("Dendrogramme des points spatiaux classifiיs par l'humiditיs agrיgיes par mois")
  return(list(dend=dendro,class=Classif,statclim.diff=statclimcorrect))
}

# Ces diffיrentes lignes de code permettent d'appeller les variables de la fonction 

ACPCL<-ACPandClassif(statclim=huss.month, nbdimACP=5, clustmethod="ward.D2", nbclass=5)
D<-ACPCL$statclim.diff
Class<-ACPCL$class
palette<-rainbow(n=5) # Crייe la palette de couleur א utiliser pour caractיriser les classes
Color<-palette[Class] # Stock dans une variable Color, les couleurs associיes par classe
ACPCL$dend # Appelle le dendrogramme de la fonction

# Le code suivant permet de crיer un dataframe avec les moyennes des diffיrentes classes en colonne et les annיes en ligne
nbclass<-5
MOY<-matrix(NA,nrow(D),nbclass)
for (k in 1:nbclass){
  select<- which(Class == k)
  MOY[ , k] <- apply( D[ , select] , 1 , mean )
}
MOYfinal<-data.frame(MOY)

# Le code suivant permet de rיaliser la classification des courbes par classes.

# On commencer par crיer une fenךtre graphique vide avec les bonnes longueurs des axes
plot(NA,NA,xlim=range(as.numeric(row.names(MOYfinal))), ylim=range(MOYfinal),xlab="Mois", ylab="Humiditיs", main="Classification des courbes des diffיrentes classes d'humiditיs agrיgיes par mois")
for (l in 1:ncol(MOYfinal)){
  lines(x=as.numeric(row.names(MOYfinal)),y=MOYfinal[,l],col=palette[l])
} # On finit par tracer avec une boucle les clases 


# Cartographie des points spatiaux de la plaine en fonction de leurs classes.

long<-as.numeric(df.points$Longitude)
lat<-as.numeric(df.points$Latitude)
spdf.points<-SpatialPointsDataFrame(coords=cbind(long,lat),data=df.points)
proj4string(spdf.points) <- CRS(wgs84)
plot(spdf.points, col=Color, main="Carte de la plaine de la Limagne des points spatiaux classifiיs par l'humiditיs du vent agrיgיes par mois", cex.main=0.75)


## Crיation de donnיes agrיgיs par mois
#Tempיrature moyenne
tmoym<-tapply(DATA1$tasAdjust,list(DATA1$name,DATA1$Month),"mean")
tmoym0.20<-tapply(DATA1$tasAdjust,list(DATA1$name,DATA1$Month),quantile, probs =0.20)
tmoym0.80<-tapply(DATA1$tasAdjust,list(DATA1$name,DATA1$Month),quantile, probs =0.80)
#Tempיrature minimale
tmin<-tapply(DATA1$tasminAdjust,list(DATA1$name,DATA1$Month),"mean")
tmin0.20<-tapply(DATA1$tasminAdjust,list(DATA1$name,DATA1$Month),quantile, probs =0.20)
tmin0.80<-tapply(DATA1$tasminAdjust,list(DATA1$name,DATA1$Month),quantile, probs =0.80)
#Tempיrature maximale
tmax<-tapply(DATA1$tasmaxAdjust,list(DATA1$name,DATA1$Month),"mean")
tmax0.20<-tapply(DATA1$tasmaxAdjust,list(DATA1$name,DATA1$Month),quantile, probs =0.20)
tmax0.80<-tapply(DATA1$tasmaxAdjust,list(DATA1$name,DATA1$Month),quantile, probs =0.80)
#Tempיrature la plus basse
tlowest<-tapply(DATA1$tasminAdjust,list(DATA1$name,DATA1$Month),"min")
#Tempיrature la plus יlevיe
thighest<-tapply(DATA1$tasmaxAdjust,list(DATA1$name,DATA1$Month),"max")
#Hauteur de prיcipitations
pmoy<-tapply(DATA1$prtotAdjust,list(DATA1$name,DATA1$Month),"mean")
pmoy0.20<-tapply(DATA1$prtotAdjust,list(DATA1$name,DATA1$Month),quantile, probs =0.20)
pmoy0.80<-tapply(DATA1$prtotAdjust,list(DATA1$name,DATA1$Month),quantile, probs =0.80)
psmoy<-tapply(DATA1$prsnAdjust,list(DATA1$name,DATA1$Month),"mean")
#Hauteur de prיcipitations maximale 
phighest<-tapply(DATA1$prtotAdjust,list(DATA1$name,DATA1$Month),"max")
#Humiditי
Hmoy<-tapply(DATA1$hussAdjust,list(DATA1$name,DATA1$Month),"mean")
#Vitesse du vent 
sfcmoy<-tapply(DATA1$sfcWindAdjust,list(DATA1$name,DATA1$Month),"mean")
#Vitesse du vent maximale
sfchighest<-tapply(DATA1$sfcWindAdjust,list(DATA1$name,DATA1$Month),"max")


#########################################################################################
## Diffיrente mיthode de calcul de l'interpolation spatiale
#Distance inverse
interp.inv.dist <-function(x,y,z,gr.x, gr.y,p) {
  I<-length(gr.x)
  J<-length(gr.y)
  MAP<-matrix(NA,I,J)
  for(i in 1:I) for(j in 1:J) {
    w<-sqrt((x - gr.x[i])^2 + (y - gr.y[j])^2)
    w[w==0]<-0.000001
    w<- 1 / (w)^p
    MAP[i,j]<- sum(w*z)/sum(w)
  }
  return(MAP)
}

#Loi exponentielle
interp.exp <-function(x,y,z,gr.x, gr.y,p) {
  I<-length(gr.x)
  J<-length(gr.y)
  MAP<-matrix(NA,I,J)
  for(i in 1:I) for(j in 1:J) {
    w<-sqrt((x - gr.x[i])^2 + (y - gr.y[j])^2)
    w<- exp(-abs(w/p))
    MAP[i,j]<- sum(w*z)/sum(w)
  }
  return(MAP)
}

#Loi gaussienne
interp.gauss <-function(x,y,z,gr.x, gr.y,p) {
  I<-length(gr.x)
  J<-length(gr.y)
  MAP<-matrix(NA,I,J)
  for(i in 1:I) for(j in 1:J) {
    w<-sqrt((x - gr.x[i])^2 + (y - gr.y[j])^2)
    w<- exp(-((w/p)^2))
    MAP[i,j]<- sum(w*z)/sum(w)
  }
  return(MAP)
}
#########################################################################################
# Fonction pour calculer la valeur optimale de p par validation croisי
# Deux fonctions une avec crיation de graphique pour l'intיrprיtation visuelle et une sans graphique
optimal.p.sortie <-function(interp,x,y,z,gr.x, gr.y) {
  name <- deparse(substitute(interp))
  tot<-matrix(nrow=500,ncol=2)
  colnames(tot) <- c("p","EQM")
  l<-0
  for (p in seq (0.01,5, by=0.01)){
    
    z.pred<-rep(NA,45)
    for (i in 1:45){
      z.pred[i]<-interp(x[-i],y[-i],z[-i],x[i],y[i],p)
    }
    EQM<-mean((z - z.pred)^2)
    
    l<-l+1
    tot[l,1]<-p
    tot[l,2]<-EQM
  }
  tot<-as.data.frame(tot)
  plot(tot, main="Calcul de la valeur optimal de p",ylab = "Ecart quadratique moyen")
  p<-tot[tot$EQM==min(tot$EQM),"p"]
  ecart<-min(tot$EQM)
  return(list(p=p,EQM=ecart,methode=name))
}
optimal.p <-function(interp,x,y,z,gr.x, gr.y) {
  name <- deparse(substitute(interp))
  tot<-matrix(nrow=500,ncol=2)
  colnames(tot) <- c("p","EQM")
  l<-0
  for (p in seq (0.01,5, by=0.01)){
    
    z.pred<-rep(NA,45)
    for (i in 1:45){
      z.pred[i]<-interp(x[-i],y[-i],z[-i],x[i],y[i],p)
    }
    EQM<-mean((z - z.pred)^2)
    
    l<-l+1
    tot[l,1]<-p
    tot[l,2]<-EQM
  }
  tot<-as.data.frame(tot)
  p<-tot[tot$EQM==min(tot$EQM),"p"]
  ecart<-min(tot$EQM)
  return(list(p=p,EQM=ecart,methode=name))
}
#########################################################################################
## Fonction pour simplifier la crיation de lיgende
f.niveaux<-function(statc,nb,mois){
  DATA1[DATA1$Month==mois,]
  min<-floor(min(statc))
  max<-ceiling(max(statc))
  ec<-max-min
  pas<-ec/nb
  niv<-seq(min,max,by=pas)
  return(niv)
}
#########################################################################################
## Fonction pour simplifier la crיation de lיgende
f.mois<-function(mois){
  month<-c("Janvier","Fיvrier","Mars","Avril","Mai","Juin","Juillet","Ao�t","Septembre","Octobre","Novembre","Dיcembre")
  id<-c("01":"12")
  id1<-as.data.frame(cbind(id,month))
  mois1<-id1[id1$id==mois,"month"]
  return(mois1)
}
f.titre<-function(statc,mois,annee){
  suppressWarnings({
    if(statc==tmoym | statc=="tmoym"){
      title=paste("Tempיrature moyenne du mois de",f.mois(mois),annee)}
    else if(statc==tmin | statc=="tmin"){
      title=paste("Tempיrature minimale moyenne du mois de",f.mois(mois),annee)}
    else if(statc==tmax | statc=="tmax"){
      title=paste("Tempיrature maximale moyenne du mois de",f.mois(mois),annee)}
    else if(statc==tlowest | statc=="tlowest"){
      title=paste("Tempיrature minimale atteinte en",f.mois(mois),annee)}
    else if(statc==thighest | statc=="thighest"){
      title=paste("Tempיrature maximale atteinte en",f.mois(mois),annee)}
    else if(statc==pmoy | statc=="pmoy"){
      title=paste("Prיcipitation moyenne du mois de",f.mois(mois),annee)}
    else if(statc==psmoy | statc=="psmoy"){
      title=paste("Prיcipitation solide moyenne du mois de",f.mois(mois),annee)}
    else if(statc==phighest | statc=="phighest"){
      title=paste("Prיcipitation maximale atteinte en",f.mois(mois),annee)}
    else if(statc==Hmoy | statc=="Hmoy"){
      title=paste("Humiditי moyenne du mois de",f.mois(mois),annee)}
    else if(statc==sfchighest | statc=="sfchighest") {
      title=paste("Vitesse moyenne du vent au mois de",f.mois(mois),annee)} 
  })
  return(title)
}
#########################################################################################
## Fonction pour crייr une carte d'un mois spיcifiי, d'une statistique spיcifiי, avecd es donnיes agrיgיes sur les mois de 2001 א 2036
# statc: choisir n'importe quelle statistique climatique agrיgיs par mois
# mois: choisir un mois, en choisisant son numיro correspondant (ex: 3 pour mars)
# nb: permet de choisir le nombre de classe (choisir un nombre impair pour avoir du blanc qui sיpare les deux couleurs) 
Carte.mois<-function(statc,mois,nb){
  #mise en place de la dimension de la carte
  x<- df.points$Longitude
  y<- df.points$Latitude
  z<- statc[,mois] 
  gr.x<-seq(3,3.5,length=150)
  gr.y<-seq(45,46.5,length=450)
  ##Calcul optimal de p
  p1<-as.data.frame(optimal.p.sortie(interp.gauss,x,y,z,gr.x, gr.y))
  p2<-as.data.frame(optimal.p.sortie(interp.exp,x,y,z,gr.x, gr.y))
  p3<-as.data.frame(optimal.p.sortie(interp.inv.dist,x,y,z,gr.x, gr.y))
  p<-rbind(p1,p2,p3)
  meth<-p[p$EQM==min(p$EQM),"methode"]
  p<-p[p$EQM==min(p$EQM),"p"]
  #Utilisation de la mיthode la plus optimale
  if(meth=="interp.inv.dist"){MAP<-interp.inv.dist(x,y,z,gr.x, gr.y,p)}
  else if (meth=="interp.exp"){MAP<-interp.exp(x,y,z,gr.x, gr.y,p)}
  else {MAP<-interp.gauss(x,y,z,gr.x, gr.y,p)}
  #Crיation de la palette de couleur
  pal1<-colorRampPalette(c("blue","white"))((nb+1)/2)
  pal2<-colorRampPalette(c("white","red"))((nb+1)/2)
  palette<-c(pal1,pal2[-1])
  #Crיation des bornes
  min<-floor(min(statc[,mois]))
  max<-ceiling(max(statc[,mois]))
  ec<-max-min
  pas<-ec/nb
  niveaux<-seq(min,max,by=pas)
  legend<-paste(round(niveaux[1:nb], digits=2) ,"-", round(niveaux[2:nb+1], digits=2))
  titre<-f.titre(statc,mois,"")
  #Crיation du graphique
  image(gr.x,gr.y,(MAP),col=palette,breaks=niveaux, main=titre, ylab="Latitude",xlab="Longitude")
  contour(gr.x,gr.y,MAP,levels=niveaux,add=TRUE)
  points(x,y,pch=16)
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  legend("topright",legend=legend,fill=palette,inset = c(-0.33, 0))
  #Enregistrement en pdf
  pdf(paste0("Carte/Carte_",deparse(substitute(statc)),"_",f.mois(mois),".pdf"), height=10,width=3,paper = "a4")
  image(gr.x,gr.y,(MAP),col=palette,breaks=niveaux, main=titre,mar=c(5,4,4,6), ylab="Latitude",xlab="Longitude")
  contour(gr.x,gr.y,MAP,levels=niveaux,add=TRUE)
  points(x,y,pch=16)
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  legend("topright",legend=legend,fill=palette,inset = c(-0.5, 0))
  dev.off()
  return(meth)
}
# Exemple d'utilisation ici on rיalise une carte des donnיes agrיgיes de la prיcipitation moyenne au mois de janvier avec 13 couleurs diffיrentes
Carte.mois(tmoym,1,13)
###########################################################################################
Carte.annee<-function(statc,annee,mois,nb){
  
  data12<-DATA[DATA$Year==annee,]
  var<-as.data.frame(data12[,statc])
  data12<-data12[c(12,13,14,15)]
  data12<-cbind(var,data12)
  data12<-tapply(data12[,1],list(data12$name,data12$Month),"mean")
  #mise en place de la dimension de la carte
  x<- df.points$Longitude
  y<- df.points$Latitude
  z<- data12[,mois] 
  gr.x<-seq(3,3.5,length=150)
  gr.y<-seq(45,46.5,length=450)
  ##Calcul optimal de p
  p1<-as.data.frame(optimal.p.sortie(interp.gauss,x,y,z,gr.x, gr.y))
  p2<-as.data.frame(optimal.p.sortie(interp.exp,x,y,z,gr.x, gr.y))
  p3<-as.data.frame(optimal.p.sortie(interp.inv.dist,x,y,z,gr.x, gr.y))
  p<-rbind(p1,p2,p3)
  meth<-p[p$EQM==min(p$EQM),"methode"]
  p<-p[p$EQM==min(p$EQM),"p"]
  #Utilisation de la mיthode la plus optimale
  if(meth=="interp.inv.dist"){MAP<-interp.inv.dist(x,y,z,gr.x, gr.y,p)
  }  else if (meth=="interp.exp"){MAP<-interp.exp(x,y,z,gr.x, gr.y,p)
  } else {MAP<-interp.gauss(x,y,z,gr.x, gr.y,p)}
  #Permet ici de dיterminer des bornes
  mois1<-paste0("","0",mois,"")
  if (mois==10|mois==11|mois==12){
    mois1<-paste0("",mois,"")
  }
  dataaaa<-DATA[DATA$Month==mois1,statc]
  if (statc=="prtotAdjust" |statc=="prsnAdjust"){
    dataaaa<-log(dataaaa+1)
  }
  min<-floor(min(dataaaa))
  max<-ceiling(max(dataaaa))
  ec<-max-min
  pas<-ec/nb
  niveaux<-seq(min,max,by=pas)
  pal1<-colorRampPalette(c("blue","white"))((nb+1)/2)
  pal2<-colorRampPalette(c("white","red"))((nb+1)/2)
  palette<-c(pal1,pal2[-1])
  legend<-paste(round(niveaux[0:nb], digits=2) ,"-", round(niveaux[1:nb+1], digits=2))
  #Dיfinition du titre
  stat<-statc
  l1<-c("tasminAdjust","tasmaxAdjust","tasAdjust","prtotAdjust","prsnAdjust","hussAdjust","sfcWindAdjust")
  l2<-c("tmin","tmax","tmoym","pmoy","psmoy","Hmoy","sfcmoy")    
  l<-as.data.frame(cbind(l1,l2))
  lstat<-l[l$l1==stat,"l2"]
  titre<-f.titre(lstat,mois,annee)
  #Crיation du graphique
  image(gr.x,gr.y,(MAP),col=palette,breaks=niveaux, main=titre, ylab="Latitude",xlab="Longitude")
  contour(gr.x,gr.y,MAP,levels=niveaux,add=TRUE)
  points(x,y,pch=16)
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  legend("topright",legend=legend,fill=palette,inset = c(-0.23, 0))
  return(meth)
}
###########################################################################################
Carte.annee.pdf<-function(statc,annee,mois,data,nb){
  
  data12<-DATA[DATA$Year==annee,]
  var<-as.data.frame(data12[,statc])
  data12<-data12[c(12,13,14,15)]
  data12<-cbind(var,data12)
  data12<-tapply(data12[,1],list(data12$name,data12$Month),"mean")
  #mise en place de la dimension de la carte
  x<- df.points$Longitude
  y<- df.points$Latitude
  z<- data12[,mois] 
  gr.x<-seq(3,3.5,length=150)
  gr.y<-seq(45,46.5,length=450)
  ##Calcul optimal de p
  #p1<-as.data.frame(optimal.p(interp.gauss,x,y,z,gr.x, gr.y))
  p2<-as.data.frame(optimal.p(interp.exp,x,y,z,gr.x, gr.y))
  p3<-as.data.frame(optimal.p(interp.inv.dist,x,y,z,gr.x, gr.y))
  p<-rbind(p2,p3)
  meth<-p[p$EQM==min(p$EQM),"methode"]
  p<-p[p$EQM==min(p$EQM),"p"]
  #Utilisation de la mיthode la plus optimale
  if(meth=="interp.inv.dist"){MAP<-interp.inv.dist(x,y,z,gr.x, gr.y,p)
  }  else if (meth=="interp.exp"){MAP<-interp.exp(x,y,z,gr.x, gr.y,p)
  } else {MAP<-interp.gauss(x,y,z,gr.x, gr.y,p)}
  #Permet ici de dיterminer des bornes
  mois1<-paste0("","0",mois,"")
  if (mois==10|mois==11|mois==12){
    mois1<-paste0("",mois,"")
  }
  dataaaa<-DATA[DATA$Month==mois1,statc]
  if (statc=="prtotAdjust" |statc=="prsnAdjust"){
    dataaaa<-log(dataaaa+1)
  }
  min<-floor(min(dataaaa))
  max<-ceiling(max(dataaaa))
  ec<-max-min
  pas<-ec/nb
  niveaux<-seq(min,max,by=pas)
  pal1<-colorRampPalette(c("blue","white"))((nb+1)/2)
  pal2<-colorRampPalette(c("white","red"))((nb+1)/2)
  palette<-c(pal1,pal2[-1])
  legend<-paste(round(niveaux[0:nb], digits=2) ,"-", round(niveaux[1:nb+1], digits=2))
  #Dיfinition du titre
  stat<-statc
  l1<-c("tasminAdjust","tasmaxAdjust","tasAdjust","prtotAdjust","prsnAdjust","hussAdjust","sfcWindAdjust")
  l2<-c("tmin","tmax","tmoym","pmoy","psmoy","Hmoy","sfcmoy")    
  l<-as.data.frame(cbind(l1,l2))
  lstat<-l[l$l1==stat,"l2"]
  titre<-f.titre(lstat,mois,annee)
  #Enregistrement en pdf
  pdf(paste0("Carte/Carte_",lstat,"_",f.mois(mois),"_",annee,".pdf"), height=10,width=5,paper="a4")
  image(gr.x,gr.y,(MAP),col=palette,breaks=niveaux, main=titre, ylab="Latitude",xlab="Longitude",mar=c(5,4,4,2),xlim=c(3,3.75))
  contour(gr.x,gr.y,MAP,levels=niveaux,add=TRUE)
  points(x,y,pch=16)
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  legend(3.505,46.5,legend=legend,fill=palette)
  dev.off()
}
###########################################################################################
Carte.annee.jpg<-function(statc,annee,mois,nb){
  
  data12<-DATA[DATA$Year==annee,]
  var<-as.data.frame(data12[,statc])
  data12<-data12[c(12,13,14,15)]
  data12<-cbind(var,data12)
  data12<-tapply(data12[,1],list(data12$name,data12$Month),"mean")
  #mise en place de la dimension de la carte
  x<- df.points$Longitude
  y<- df.points$Latitude
  z<- data12[,mois] 
  gr.x<-seq(3,3.5,length=150)
  gr.y<-seq(45,46.5,length=450)
  ##Calcul optimal de p
  #p1<-as.data.frame(optimal.p(interp.gauss,x,y,z,gr.x, gr.y))
  p2<-as.data.frame(optimal.p(interp.exp,x,y,z,gr.x, gr.y))
  p3<-as.data.frame(optimal.p(interp.inv.dist,x,y,z,gr.x, gr.y))
  p<-rbind(p2,p3)
  meth<-p[p$EQM==min(p$EQM),"methode"]
  p<-p[p$EQM==min(p$EQM),"p"]
  #Utilisation de la mיthode la plus optimale
  if(meth=="interp.inv.dist"){MAP<-interp.inv.dist(x,y,z,gr.x, gr.y,p)
  }  else if (meth=="interp.exp"){MAP<-interp.exp(x,y,z,gr.x, gr.y,p)
  } else {MAP<-interp.gauss(x,y,z,gr.x, gr.y,p)}
  #Permet ici de dיterminer des bornes
  mois1<-paste0("","0",mois,"")
  if (mois==10|mois==11|mois==12){
    mois1<-paste0("",mois,"")
  }
  dataaaa<-DATA[DATA$Month==mois1,statc]
  if (statc=="prtotAdjust" |statc=="prsnAdjust"){
    dataaaa<-log(dataaaa+1)
  }
  min<-floor(min(dataaaa))
  max<-ceiling(max(dataaaa))
  ec<-max-min
  pas<-ec/nb
  niveaux<-seq(min,max,by=pas)
  pal1<-colorRampPalette(c("blue","white"))((nb+1)/2)
  pal2<-colorRampPalette(c("white","red"))((nb+1)/2)
  palette<-c(pal1,pal2[-1])
  legend<-paste(round(niveaux[0:nb], digits=2) ,"-", round(niveaux[1:nb+1], digits=2))
  #Dיfinition du titre
  stat<-statc
  l1<-c("tasminAdjust","tasmaxAdjust","tasAdjust","prtotAdjust","prsnAdjust","hussAdjust","sfcWindAdjust")
  l2<-c("tmin","tmax","tmoym","pmoy","psmoy","Hmoy","sfcmoy")    
  l<-as.data.frame(cbind(l1,l2))
  lstat<-l[l$l1==stat,"l2"]
  titre<-f.titre(lstat,mois,annee)
  #Enregistrement en jpg
  jpeg(paste0("Carte/Carte_",lstat,"_",f.mois(mois),"_",annee,".jpg"), height=1280,width=580)
  image(gr.x,gr.y,(MAP),col=palette,breaks=niveaux, main=titre, ylab="Latitude",xlab="Longitude",mar=c(5,4,12,2),xlim=c(3,3.67))
  contour(gr.x,gr.y,MAP,levels=niveaux,add=TRUE)
  points(x,y,pch=16)
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  legend(3.505,46.5,legend=legend,fill=palette)
  dev.off()
}
###########################################################################################
## Crיation de trois fonctions similaires qui affiche une carte d'une statistique climatique non agrיgיs, pour un mois donnי, une annיe donnיe
# et avec le nombre de classe que l'on veut
# statc: choisir n'importe quelle statistique climatique non agrיgיs
# annיe: choisir une annיe (donc de 2006 א 2100)
# mois: choisir un mois, en choisisant son numיro correspondant (ex: 3 pour mars)
# nb: permet de choisir le nombre de classe (choisir un nombre impair pour avoir du blanc qui sיpare les deux couleurs) 

##Les diffיrences de ces 3 fonctions
# Carte.annee(statc,annee,mois,nb) : rיalise une carte oש l'on voit la sortie sur R
# Carte.annee.pdf(statc,annee,mois,nb) : rיalise une carte qui est enregistrי au format pdf dans un doc "Carte" qui doit ךtre crיe a la main
# Carte.annee.jpg(statc,annee,mois,nb) : rיalise une carte qui est enregistrי au format jpg dans un doc "Carte" qui doit ךtre crיe a la main

#Exemple d'utilisation
Carte.annee("prsnAdjust",2055,12,19)

# Avec cette fonction on peut enregistrי des cartes de 2036 א 2100 de la tempיrature moyenne du mois de juin, avec une dיcomposition en 19 classes
for (i in 2036:2100){
  Carte.annee.jpg("tasAdjust",i,06,19)
}

###########################################################
############# LES DONNÉES BRUTES ##########################
###########################################################

data1 <- DATA
data1
data1$Date <- ymd(data1$Date)
data1$Date
# Colonne de l'anné
data1$year <- strftime(data1$Date, "%Y")
data1$year
# Colonne de mois :
data1$month <- strftime(data1$Date, "%m")
data1$month
# Colonne de date :
data1$date <- strftime(data1$Date, "%d")
data1$date
View(data1)
class(data1$year)

data1$month <- as.numeric(as.character(data1$month))
class(data1$month)

data1$date <- as.numeric(as.character(data1$date))
class(data1$date)

#2. création du dataframe des points spatiaux
df.points<-unique(DATA[,c(3,2)])
df.points$nval<-NA
for(i in 1: nrow(df.points))
  df.points$nval[i]<-sum(DATA$Longitude==df.points$Longitude[i] & DATA$Latitude==df.points$Latitude[i])
rownames(df.points)<-paste0("P",1:nrow(df.points))
df.points

#3. rajoute la variable 'name' dans le dataframe DATA
DATA$name<-rep(rownames(df.points),df.points$nval)
data1$name<-rep(rownames(df.points),df.points$nval)


############################################################################################


############################################################################
####### FONCTION POUR VISUALISER LES DONNÉES BRUTES (LA VARIABLE CHOISIE) ##
####### EN FONCTION D'UN POINT SPATIAL CHOIS ET UNE ANNÉE CHOISIE ##########
############################################################################

func_facet <- function (df,var_name,point,année) {
  df <- filter(df,year==année & name==point)
  
  var<-df[,var_name]
  
  if (var_name=="tasminAdjust") {
    plot_1 <- ggplot(df, aes(x=Date)) +
      facet_wrap(~month,scales = "free_x") +
      geom_line(aes(y=tasminAdjust, colour="la température \n minimale")) +
      #theme_bw(base_size = 9) +
      theme(axis.text.x = element_text(angle = 50)) +
      ylab("Temperature") +
      ggtitle(paste("Présentation de la température minimale(ºC) pour le point",point,"en",année))+
      scale_colour_discrete( name="Variable")
    plot_1
  }
  else if (var_name=="tasmaxAdjust") {
    plot_1 <- ggplot(df, aes(x=Date)) +
      facet_wrap(~month,scales = "free_x") +
      geom_line(aes(y=tasmaxAdjust, colour="la température \n maximale")) +
      #theme_bw(base_size = 9) +
      theme(axis.text.x = element_text(angle = 50)) +
      ylab("Temperature") +
      ggtitle(paste("Présentation de la température maximale(ºC) pour le point",point,"en",année))+
      scale_colour_discrete( name="Variable")
    plot_1
  }
  else if (var_name=="tasAdjust") {
    plot_1 <- ggplot(df, aes(x=Date)) +
      facet_wrap(~month,scales = "free_x") +
      geom_line(aes(y=tasAdjust, colour="La température \n moyenne")) +
      #theme_bw(base_size = 9) +
      theme(axis.text.x = element_text(angle = 50)) +
      ylab("Temperature") +
      ggtitle(paste("Présentation de la température moyenne(ºC) pour le point",point,"en",année))+
      scale_colour_discrete( name="Variable")
    plot_1
  }
  else if (var_name=="prtotAdjust") {
    plot_1 <- ggplot(df, aes(x=Date)) +
      facet_wrap(~month,scales = "free_x") +
      geom_line(aes(y=prtotAdjust, colour="La précipitation \n totale")) +
      #theme_bw(base_size = 9) +
      theme(axis.text.x = element_text(angle = 50)) +
      ylab("Précipitation") +
      ggtitle(paste("Présentation de la précipitation totale(mm) pour le point",point,"en",année))+
      scale_colour_discrete( name="Variable")
    plot_1
  }
  else if (var_name=="prsnAdjust") {
    plot_1 <- ggplot(df, aes(x=Date)) +
      facet_wrap(~month,scales = "free_x") +
      geom_line(aes(y=prsnAdjust, colour="la précipitation \n solide")) +
      #theme_bw(base_size = 9) +
      theme(axis.text.x = element_text(angle = 50)) +
      ylab("Précipitation") +
      ggtitle(paste("Présentation de la précipitation solide(mm) pour le point",point,"en",année))+
      scale_colour_discrete( name="Variable")
    plot_1
  }
  else if (var_name=="hussAdjust") {
    plot_1 <- ggplot(df, aes(x=Date)) +
      facet_wrap(~month,scales = "free_x") +
      geom_line(aes(y=hussAdjust, colour="Humidité")) +
      #theme_bw(base_size = 9) +
      theme(axis.text.x = element_text(angle = 50)) +
      ylab("Humidité") +
      ggtitle(paste("Présentation de l'humidité(kg/kg) pour le point",point,"en",année))+
      scale_colour_discrete( name="Variable")
    plot_1
  }
  else {
    plot_1 <- ggplot(df, aes(x=Date)) +
      facet_wrap(~month,scales = "free_x") +
      geom_line(aes(y=sfcWindAdjust, colour="la vitesse \n du vent")) +
      #theme_bw(base_size = 9) +
      theme(axis.text.x = element_text(angle = 50)) +
      ylab("Vitesse") +
      ggtitle(paste("Présentation de la vitesse du vent(m/s) pour le point",point,"en",année))+
      scale_colour_discrete( name="Variable")
    plot_1
  }
  return(plot_1)
}
func_facet(data1,"prtotAdjust","P12",2021)


###########################################################################################################


#####################################################################################################
######## FONCTION DÉDIÉE AUX TEMPÉRATURE QUI PERMET DE VISUALISER LES 3 TYPES DE TEMPÉRATURE ########
########  EN FONCTION D'UNE ANNÉE CHOISI ET UN POINT SPATIAL CHOISI #################################
#####################################################################################################

func_temp <- function(df,point,année) {
  df <- filter(df,year==année & name==point)
  
  plot_temp <- ggplot(df, aes(x=Date)) +
    facet_wrap(~month,scales = "free_x") +
    geom_line(aes(y=tasminAdjust, colour="TempMin")) +
    geom_line(aes(y=tasmaxAdjust ,colour="TempMax"))+
    geom_line(aes(y=tasAdjust,colour="TempMoy")) +
    #theme_bw(base_size = 9) +
    theme(axis.text.x = element_text(angle = 50)) +
    scale_color_manual("Type of temp", breaks = 		c("TempMin","TempMax","TempMoy"),
                       values = c("blue","red","green")) +
    ylab("Temperature(ºC)") +
    ggtitle(paste("Présentation des températures pour le point",point,"en",année))  
  plot_temp
}
func_temp(data1,"P12",2021)


#############################################################################################




###############################################################
###### FONCTION POUR LA TENDANCE, SAISONNALITÉ ET RÉSIDUS #####
###############################################################

# VERSION 1:

filtrage <-function(x,mm) {
  n<-length(x)
  p<-length(mm$coef)
  y<-rep(NA,n)
  if(n>p) for(t in (mm$ic):(n-p+mm$ic)) y[t]<- sum(mm$coef*x[(t-mm$ic+1):(t+p-mm$ic)])
  return(y)
}

filtrage_1 <- function(df,var_name,point,date.debut,date.fin,mm_1,mm_2) {
  
  df <- filter(df,Date >= as.Date(date.debut) & Date <= as.Date(date.fin) & name==point)
  
  var<-df[,var_name]
  
  mm_ordre1 <- list(coef=rep(1/mm_1,mm_1), ic=(mm_1-1)/2+1)
  mm_ordre2 <- list(coef=rep(1/mm_2,mm_2), ic=(mm_2-1)/2+1)
  var_trend <- filtrage(var,mm_ordre1)
  var_season <- filtrage(var-var_trend,mm_ordre2)
  var_res <- var - (var_trend) - var_season
  
  time<-strptime(df$Date,format="%Y-%m-%d")
  
  time.debut<-strptime(date.debut,format="%Y-%m-%d")
  time.fin  <-strptime(date.fin  ,format="%Y-%m-%d")
  
  index.debut<- min(which(time>=time.debut))
  index.fin  <- max(which(time<=time.fin))
  
  if (var_name=="tasminAdjust") {
    plot_1 <- plot(x=time[index.debut:index.fin], y=var[index.debut:index.fin] , type="l",
                   xlab="Année", ylab="Température minimale(°C)",
                   main = paste("Température minimale de",date.debut,"à",date.fin,
                                "pour le point",point))
    lines(x=time[index.debut:index.fin], y=var_trend[index.debut :index.fin],col="red",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_res[index.debut:index.fin],col="green",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_season[index.debut:index.fin],col="blue",lwd=2)
    par(mar = c(5, 4, 2, 8),xpd = TRUE)
    legend("bottomright",inset = c(- 0.4, 0),legend=c("Tendance","Saisonnalité","Résidus"),fill=c("red","blue","green"),cex=0.6)
  }
  else if (var_name=="tasmaxAdjust") {
    plot_1 <- plot(x=time[index.debut:index.fin], y=var [index.debut:index.fin] , type="l",
                   xlab="Année", ylab="Température maximale(°C)",
                   main = paste("Température maximale de",date.debut,"à",date.fin,
                                "pour le point",point))
    lines(x=time[index.debut:index.fin], y=var_trend[index.debut :index.fin],col="red",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_res[index.debut:index.fin],col="green",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_season[index.debut:index.fin],col="blue",lwd=2)
    par(mar = c(5, 4, 2, 8),xpd = TRUE)
    legend("bottomright",legend=c("Tendance","Saisonnalité","Résidus"),fill=c("red","blue","green"),cex=0.6)
  }
  else if (var_name=="tasAdjust") {
    plot_1 <- plot(x=time[index.debut:index.fin], y=var [index.debut:index.fin] , type="l",
                   xlab="Année", ylab="Température moyenne(°C)",
                   main = paste("Température moyenne de",date.debut,"à",date.fin,
                                "pour le point",point))
    lines(x=time[index.debut:index.fin], y=var_trend[index.debut :index.fin],col="red",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_res[index.debut:index.fin],col="green",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_season[index.debut:index.fin],col="blue",lwd=2)
    par(mar = c(5, 4, 2, 8),xpd = TRUE)
    legend("bottomright",legend=c("Tendance","Saisonnalité","Résidus"),fill=c("red","blue","green"),cex=0.6)
  }
  else if (var_name=="prtotAdjust") {
    plot_1 <- plot(x=time[index.debut:index.fin], y=var [index.debut:index.fin] , type="l",
                   xlab="Année", ylab="Precipitations totales(mm)",
                   main = paste("Precipitations totales de",date.debut,"à",date.fin,
                                "pour le point",point))
    lines(x=time[index.debut:index.fin], y=var_trend[index.debut :index.fin],col="red",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_res[index.debut:index.fin],col="green",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_season[index.debut:index.fin],col="blue",lwd=2)
    par(mar = c(5, 4, 2, 8),xpd = TRUE)
    legend("bottomright",legend=c("Tendance","Saisonnalité","Résidus"),fill=c("red","blue","green"),cex=0.6)
  }
  else if (var_name=="prsnAdjust") {
    plot_1 <- plot(x=time[index.debut:index.fin], y=var [index.debut:index.fin] , type="l",
                   xlab="Année", ylab="Precipitations solides(mm)",
                   main = paste("Precipitations solides de",date.debut,"à",date.fin,
                                "pour le point",point))
    lines(x=time[index.debut:index.fin], y=var_trend[index.debut :index.fin],col="red",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_res[index.debut:index.fin],col="green",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_season[index.debut:index.fin],col="blue",lwd=2)
    par(mar = c(5, 4, 2, 8),xpd = TRUE)
    legend("bottomright",legend=c("Tendance","Saisonnalité","Résidus"),fill=c("red","blue","green"),cex=0.6)
  }
  else if (var_name=="hussAdjust") {
    plot_1 <- plot(x=time[index.debut:index.fin], y=var [index.debut:index.fin] , type="l",
                   xlab="Année", ylab="Humidité",
                   main = paste("Humidite specifique a 2 m (kg/kg) de",date.debut,"à",date.fin,
                                "pour le point",point))
    lines(x=time[index.debut:index.fin], y=var_trend[index.debut :index.fin],col="red",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_res[index.debut:index.fin],col="green",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_season[index.debut:index.fin],col="blue",lwd=2)
    par(mar = c(5, 4, 2, 8),xpd = TRUE)
    legend("bottomright",legend=c("Tendance","Saisonnalité","Résidus"),fill=c("red","blue","green"),cex=0.6)
  }
  else {
    plot_1 <- plot(x=time[index.debut:index.fin], y=var [index.debut:index.fin] , type="l",
                   xlab="Année", ylab="Vitesse du vent",
                   main = paste("Vitesse du vent a 10 m (m/s) de",date.debut,"à",date.fin,
                                "pour le point",point))
    lines(x=time[index.debut:index.fin], y=var_trend[index.debut :index.fin],col="red",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_res[index.debut:index.fin],col="green",lwd=2)
    lines(x=time[index.debut:index.fin], y=var_season[index.debut:index.fin],col="blue",lwd=2)
    par(mar = c(5, 4, 2, 8),xpd = TRUE)
    legend("bottomright",legend=c("Tendance","Saisonnalité","Résidus"),col=c("red","blue","green"),cex=0.6)
  }
  return(plot_1)
} 
filtrage_1(df=data1,var_name="tasminAdjust",point="P12","2009-01-01","2018-12-31",365,31)

# VERSION 2: GGPLOT2
filtrage <-function(x,mm) {
  n<-length(x)
  p<-length(mm$coef)
  y<-rep(NA,n)
  if(n>p) for(t in (mm$ic):(n-p+mm$ic)) y[t]<- sum(mm$coef*x[(t-mm$ic+1):(t+p-mm$ic)])
  return(y)
}

filtrage_2 <- function(df_3,var_name,point,date.debut,date.fin,mm_1,mm_2) {
  
  df_3 <- filter(df_3,Date >= as.Date(date.debut) & Date <= as.Date(date.fin) & name==point)
  
  var<-df_3[,var_name]
  
  mm_ordre1 <- list(coef=rep(1/mm_1,mm_1), ic=(mm_1-1)/2+1)
  mm_ordre2 <- list(coef=rep(1/mm_2,mm_2), ic=(mm_2-1)/2+1)
  var_trend <- filtrage(var,mm_ordre1)
  var_season <- filtrage(var - var_trend,mm_ordre2)
  var_res <- var - (var_trend) - var_season
  
  time<-strptime(df_3$Date,format="%Y-%m-%d")
  
  time.debut<-strptime(date.debut,format="%Y-%m-%d")
  time.fin  <-strptime(date.fin  ,format="%Y-%m-%d")
  
  T <- as.POSIXct(time)
  time_d <- as.POSIXct(time.debut)
  time_f <- as.POSIXct(time.fin)
  
  
  if (var_name=="tasminAdjust") {
    plot_1 <- ggplot(data=df_3,x=T) +
      ggtitle(paste("Température minimale(°C) de",date.debut,"à",date.fin,"\npour le point",point))+
      theme_bw()+
      xlab("Year")+
      ylab("Température")+
      geom_line(aes(x=T,y=var))+
      geom_line(aes(x=T, y=var_trend,col="Tendance"))+
      geom_line(aes(x=T, y=var_res,col="Résidus"))+
      geom_line(aes(x=T, y=var_season,col="Saisonalité"))+
      scale_colour_discrete( name="Type")
  }
  else if (var_name=="tasmaxAdjust") {
    plot_1 <- ggplot(data=df_3,x=T) +
      ggtitle(paste("Température maximale(°C) de",date.debut,"à",date.fin,"\npour le point",point))+
      theme_bw()+
      xlab("Year")+
      ylab("Température")+
      geom_line(aes(x=T,y=var))+
      geom_line(aes(x=T, y=var_trend,col="Tendance"))+
      geom_line(aes(x=T, y=var_res,col="Résidus"))+
      geom_line(aes(x=T, y=var_season,col="Saisonalité"))+
      scale_colour_discrete( name="Type")
  }
  else if (var_name=="tasAdjust") {
    plot_1 <- ggplot(data=df_3,x=T) +
      ggtitle(paste("Température moyenne(°C) de",date.debut,"à",date.fin,"\npour le point",point))+
      theme_bw()+
      xlab("Year")+
      ylab("Température")+
      geom_line(aes(x=T,y=var))+
      geom_line(aes(x=T, y=var_trend,col="Tendance"))+
      geom_line(aes(x=T, y=var_res,col="Résidus"))+
      geom_line(aes(x=T, y=var_season,col="Saisonalité"))+
      scale_colour_discrete( name="Type")
  }
  else if (var_name=="prtotAdjust") {
    plot_1 <- ggplot(data=df_3,x=T) +
      ggtitle(paste("Precipitations totales(mm) de",date.debut,"à",date.fin,"\npour le point",point))+
      theme_bw()+
      xlab("Year")+
      ylab("Precipitations totales")+
      geom_line(aes(x=T,y=var))+
      geom_line(aes(x=T, y=var_trend,col="Tendance"))+
      geom_line(aes(x=T, y=var_res,col="Résidus"))+
      geom_line(aes(x=T, y=var_season,col="Saisonalité"))+
      scale_colour_discrete( name="Type")
  }
  else if (var_name=="prsnAdjust") {
    plot_1 <- ggplot(data=df_3,x=T) +
      ggtitle(paste("Precipitations solides(mm) de",date.debut,"à",date.fin,"\npour le point",point))+
      theme_bw()+
      xlab("Year")+
      ylab("Precipitations")+
      geom_line(aes(x=T,y=var))+
      geom_line(aes(x=T, y=var_trend,col="Tendance"))+
      geom_line(aes(x=T, y=var_res,col="Résidus"))+
      geom_line(aes(x=T, y=var_season,col="Saisonalité"))+
      scale_colour_discrete( name="Type")
  }
  else if (var_name=="hussAdjust") {
    plot_1 <- ggplot(data=df_3,x=T) +
      ggtitle(paste("Humidite specifique a 2 m (kg/kg) de",date.debut,"à",date.fin,"\nnpour le point",point))+
      theme_bw()+
      xlab("Year")+
      ylab=("Humidité")+
        geom_line(aes(x=T,y=var))+
        geom_line(aes(x=T, y=var_trend,col="Tendance"))+
        geom_line(aes(x=T, y=var_res,col="Résidus"))+
        geom_line(aes(x=T, y=var_season,col="Saisonalité"))+
        scale_colour_discrete( name="Type")
  }
  else {
    plot_1 <- ggplot(data=df_3,x=T)+
      ggtitle(paste("Vitesse du vent a 10 m (m/s) de",date.debut,"à",date.fin,"\nnpour le point",point))+
      theme_bw()+
      xlab("Year")+
      ylab("Vitesse du vent")
    geom_line(aes(x=T,y=var))+
      geom_line(aes(x=T, y=var_trend))+
      geom_line(aes(x=T, y=var_res))+
      geom_line(aes(x=T, y=var_season))+
      scale_colour_discrete( name="Type")
  }
  return(plot_1)
} 
filtrage_2(data1,"tasAdjust","P12","2009-01-01","2018-12-31",365,31)
########################################################################
##### FUNCTION POUR RENVOIR LES DONNÉES UTILISÉES PAR  ################
############## ### LA FONCTION Filtrage_2 ###############################
########################################################################
data_1 <- function(df_1,var_name,date_d,date_f,point) {
  df_1 <- filter(df_1,Date >= as.Date(date_d) & Date <= as.Date(date_f) & name==point)
  
  var<-df_1[,var_name]
  
  données_filtrées <- data.frame(df_1)
  return(View(données_filtrées))
}
data_1(data1,"tasAdjust","2021-02-01","2022-12-31","P12")



##########################################################################################


###################################################################
########### FONCTION POUR VISUALISER LES DONNÉES BRUTES  ##########
################ AVEC LES COURBES DES QUANTILES ###################
###################################################################

func_quantile <- function(df_1,var_name,date_d,date_f,point) {
  df_1 <- filter(df_1,Date >= as.Date(date_d) & Date <= as.Date(date_f) & name==point)
  
  var<-df_1[,var_name]
  
  données_filtrées <- data.frame(df_1)
  if (var_name=="tasminAdjust") {
    d_1 <- ggplot(df_1, aes(x=Date,y=tasminAdjust)) +
      geom_line(col="gray22")+
      xlab("Année")+
      ylab("Température(ºC)")+
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 50)) +
      labs(title = paste("Température minimale(ºC) de",date_d,"à",date_f,
                         "\npour le point",point))+
      geom_hline(aes(yintercept=quantile(tasminAdjust,probs = 0.05),color="1er quantile(5%)"))+ 
      geom_hline(aes(yintercept=quantile(tasminAdjust,probs = 0.25),color="2ème quantile(25%)"))+
      geom_hline(aes(yintercept=quantile(tasminAdjust,probs = 0.50),color="3ème quantile(50%)"))+
      geom_hline(aes(yintercept=quantile(tasminAdjust,probs = 0.75),color="4ème quantile(75%)"))+
      geom_hline(aes(yintercept=quantile(tasminAdjust,probs = 0.95),color="5ème quantile(95%)"))+
      scale_colour_discrete( name="Type")
    d_1
  }
  else if (var_name=="tasmaxAdjust") {
    d_1 <- ggplot(df_1, aes(x=Date,y=tasmaxAdjust)) +
      geom_line(col="gray22")+
      xlab("Année")+
      ylab("Température(ºC)")+
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 50)) +
      labs(title = paste("Température maximale(ºC) de",date_d,"à",date_f,
                         "\npour le point",point))+
      geom_hline(aes(yintercept=quantile(tasmaxAdjust,probs = 0.05),color="1er quantile(5%)"))+ 
      geom_hline(aes(yintercept=quantile(tasmaxAdjust,probs = 0.25),color="2ème quantile(25%)"))+
      geom_hline(aes(yintercept=quantile(tasmaxAdjust,probs = 0.50),color="3ème quantile(50%)"))+
      geom_hline(aes(yintercept=quantile(tasmaxAdjust,probs = 0.75),color="4ème quantile(75%)"))+
      geom_hline(aes(yintercept=quantile(tasmaxAdjust,probs = 0.95),color="5ème quantile(95%)"))+
      scale_colour_discrete( name="Type")
    d_1
  }
  else if (var_name=="tasAdjust") {
    d_1 <- ggplot(df_1, aes(x=Date,y=tasAdjust)) +
      geom_line(col="gray22")+
      xlab("Année")+
      ylab("Température(ºC)")+
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 50)) +
      labs(title = paste("Température moyenne(ºC) de",date_d,"à",date_f,
                         "\npour le point",point))+
      geom_hline(aes(yintercept=quantile(tasAdjust,probs = 0.05),color="1er quantile(5%)"))+ 
      geom_hline(aes(yintercept=quantile(tasAdjust,probs = 0.25),color="2ème quantile(25%)"))+
      geom_hline(aes(yintercept=quantile(tasAdjust,probs = 0.50),color="3ème quantile(50%)"))+
      geom_hline(aes(yintercept=quantile(tasAdjust,probs = 0.75),color="4ème quantile(75%)"))+
      geom_hline(aes(yintercept=quantile(tasAdjust,probs = 0.95),color="5ème quantile(95%)"))+
      scale_colour_discrete( name="Type")
    d_1
  }
  else if (var_name=="prtotAdjust") {
    d_1 <- ggplot(df_1, aes(x=Date,y=prtotAdjust)) +
      geom_bar(stat="identity", width = .2,col="gray22") +
      ylab("Précipitations(mm)") +
      xlab("Année") +
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 50)) +
      labs(title = paste("Précipitations totales de",date_d,"à",date_f,
                         "\npour le point",point))+
      geom_hline(aes(yintercept=quantile(prtotAdjust,probs = 0.05),color="1er quantile(5%)"))+ 
      geom_hline(aes(yintercept=quantile(prtotAdjust,probs = 0.25),color="2ème quantile(25%)"))+
      geom_hline(aes(yintercept=quantile(prtotAdjust,probs = 0.50),color="3ème quantile(50%)"))+
      geom_hline(aes(yintercept=quantile(prtotAdjust,probs = 0.75),color="4ème quantile(75%)"))+
      geom_hline(aes(yintercept=quantile(prtotAdjust,probs = 0.95),color="5ème quantile(95%)"))+
      scale_colour_discrete( name="Type")
    d_1
  }
  else if (var_name=="prsnAdjust") {
    d_1<- ggplot(df_1, aes(x=Date,y=prsnAdjust)) +
      geom_line(col="gray22") +
      ylab("Précipitations(mm)") +
      xlab("Année") +
      theme_bw() +
      t#heme(axis.text.x = element_text(angle = 50)) +
    labs(title = paste("Précipitations solides(mm) de",date_d,"à",date_f,
                       "\npour le point",point))+
      geom_hline(aes(yintercept=quantile(prsnAdjust,probs = 0.05),color="1er quantile(5%)"))+ 
      geom_hline(aes(yintercept=quantile(prsnAdjust,probs = 0.25),color="2ème quantile(25%)"))+
      geom_hline(aes(yintercept=quantile(prsnAdjust,probs = 0.50),color="3ème quantile(50%)"))+
      geom_hline(aes(yintercept=quantile(prsnAdjust,probs = 0.75),color="4ème quantile(75%)"))+
      geom_hline(aes(yintercept=quantile(prsnAdjust,probs = 0.95),color="5ème quantile(95%)"))+
      scale_colour_discrete( name="Type")
    d_1
  }
  else if (var_name=="hussAdjust") {
    d_1 <- ggplot(df_1, aes(x=Date,y=hussAdjust)) +
      geom_line(col="gray22") +
      ylab("Humidité") +
      xlab("Année") +
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 50)) +
      labs(title = paste("Humidite specifique a 2 m (kg/kg) de",date_d,"à",date_f,
                         "\npour le point",point))+
      geom_hline(aes(yintercept=quantile(hussAdjust,probs = 0.05),color="1er quantile(5%)"))+ 
      geom_hline(aes(yintercept=quantile(hussAdjust,probs = 0.25),color="2ème quantile(25%)"))+
      geom_hline(aes(yintercept=quantile(hussAdjust,probs = 0.50),color="3ème quantile(50%)"))+
      geom_hline(aes(yintercept=quantile(hussAdjust,probs = 0.75),color="4ème quantile(75%)"))+
      geom_hline(aes(yintercept=quantile(hussAdjust,probs = 0.95),color="5ème quantile(95%)"))+
      scale_colour_discrete( name="Type")
    d_1
  }
  else {
    d_1 <- ggplot(df_1, aes(x=Date,y=sfcWindAdjust)) +
      geom_line(col="gray22") +
      ylab("Vitesse du vent") +
      xlab("Année") +
      theme_bw() +
      #theme(axis.text.x = element_text(angle = 50)) +
      labs(title = paste("Vitesse du vent a 10 m (m/s) de",date_d,"à",date_f,
                         "\npour le point",point))+
      geom_hline(aes(yintercept=quantile(sfcWindAdjust,probs = 0.05),color="1er quantile(5%)"))+ 
      geom_hline(aes(yintercept=quantile(sfcWindAdjust,probs = 0.25),color="2ème quantile(25%)"))+
      geom_hline(aes(yintercept=quantile(sfcWindAdjust,probs = 0.50),color="3ème quantile(50%)"))+
      geom_hline(aes(yintercept=quantile(sfcWindAdjust,probs = 0.75),color="4ème quantile(75%)"))+
      geom_hline(aes(yintercept=quantile(sfcWindAdjust,probs = 0.95),color="5ème quantile(95%)"))+
      scale_colour_discrete( name="Type")
    d_1
  }
  df<-data.frame(df_1$Date , df_1[,var_name])
  colnames(df)<-c('DATE',var_name)
  return(df)
}
func_quantile(data1,"tasAdjust","2021-02-01","2022-12-31","P12")
View(df)
