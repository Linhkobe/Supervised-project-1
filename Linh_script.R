

###########################################################
###################### RAW DATA ##########################
###########################################################
library(dplyr)
library(lubridate)
library(ggplot2)

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
####### FUNCTION TO VISUALIZE RAW DATA (CHOOSE A VARIABLE) #################
####### BASED ON A CHOSEN SPATIAL POINT AND A CHOSEN YEAR ####################
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
######## FUNCTION DEDICATED TO TEMPERATURES THAT ALLOWS YOU TO VIEW THE 3 TYPES OF TEMPERATURE ######
########  BASED ON A CHOSEN YEAR AND A CHOSEN SPATIAL POINT #########################################
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
###### FUNCTION FOR TREND, SEASONALITY AND RESIDUES ###########
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
############### FUNCTION TO RETURN DATA USED BY  #######################
############## ### THE FUNCTION Filtrage_2 ###############################
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
########### FUNCTION TO VISUALIZE RAW DATA  #######################
################ WITH QUANTILE CURVES #############################
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

