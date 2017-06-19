load("C:/Users/Elias/Desktop/Zaragoza/MD/Tarea breve/Q1251617.RData")
newrowA<-rep(0,25)
newrowB<-rep(0,25)
newrowC<-rep(0,25)
newrowD<-rep(0,25)
newrowE<-rep(0,25)

for (j in 1:nrow(Q1251617)){
  for (i in 1:(ncol(Q1251617)-1)){
    if(is.na(Q1251617[j,i])){}
    else{
      if(Q1251617[j,i]=="A") newrowA[i] =newrowA[i]+ Q1251617[j,26]/25
      if(Q1251617[j,i]=="B") newrowB[i] =newrowB[i]+ Q1251617[j,26]/25
      if(Q1251617[j,i]=="C") newrowC[i] =newrowC[i]+ Q1251617[j,26]/25
      if(Q1251617[j,i]=="D") newrowD[i] =newrowD[i]+ Q1251617[j,26]/25
      if(Q1251617[j,i]=="E") newrowE[i] =newrowE[i]+ Q1251617[j,26]/25
    }
  }
}

# for (j in 1:nrow(Q1251617)){
#   for (i in 1:(ncol(Q1251617)-1)){
#     if(is.na(Q1251617[j,i])){}
#     else{
#       if(Q1251617[j,i]=="A") newrowA[i] =newrowA[i]+ 1/25
#       if(Q1251617[j,i]=="B") newrowB[i] =newrowB[i]+ 1/25
#       if(Q1251617[j,i]=="C") newrowC[i] =newrowC[i]+ 1/25
#       if(Q1251617[j,i]=="D") newrowD[i] =newrowD[i]+ 1/25
#       if(Q1251617[j,i]=="E") newrowE[i] =newrowE[i]+ 1/25
#     }
#   }
# }

answercoeff<-rbind(newrowA)
answercoeff<-rbind(answercoeff,newrowB)
answercoeff<-rbind(answercoeff,newrowC)
answercoeff<-rbind(answercoeff,newrowD)
answercoeff<-rbind(answercoeff,newrowE)

##Primera aproximacion
q<-rep("",25)
for (i in 1:(ncol(answercoeff))){
  if(max(answercoeff[,i])==answercoeff[1,i]) q[i]<-"A"
  if(max(answercoeff[,i])==answercoeff[2,i]) q[i]<-"B"
  if(max(answercoeff[,i])==answercoeff[3,i]) q[i]<-"C"
  if(max(answercoeff[,i])==answercoeff[4,i]) q[i]<-"D"
  if(max(answercoeff[,i])==answercoeff[5,i]) q[i]<-"E"
}

#Calculo de puntuaciones finales de acuerdo con la primera aproximacion
calculaCalifications<-function(correctionpattern){
  calification<-rep(0,23)
  for (j in 1:nrow(Q1251617)){
    for (i in 1:(ncol(Q1251617)-1)){
      if(is.na(Q1251617[j,i])){}
      else{
        if(Q1251617[j,i]==correctionpattern[i]) calification[j]=calification[j]+1
      }
    }
  }
  return(calification)
}
calification<-calculaCalifications(q)
##Como primera aproximacion vemos que nos desviamos 2.3 puntos por alumno en la calificacion.
sum(abs(Q1251617[,26]-calification))/23

##De esto se puede ver que el numero minimo de errores en esta clasificacion es 5,
Q1251617[,26]-calification

##podemos tomar esto como inicio para hacer un algoritmo genetico que nos de la combinacion de respuestas
##Vamos a crear una poblacion de soluciones basadas en la solucion actual
populsol<-matrix(q,ncol=25)
posgen<-list()

#forzamos que todas las opciones en cada pregunta hayan sido contestadas bien por al menos un alumno
for (i in 1:25){
  dummiegen<-vector()
  if (answercoeff[1,i] !=0) dummiegen<-c(dummiegen,"A")
  if (answercoeff[2,i] !=0) dummiegen<-c(dummiegen,"B")
  if (answercoeff[3,i] !=0) dummiegen<-c(dummiegen,"C")
  if (answercoeff[4,i] !=0) dummiegen<-c(dummiegen,"D")
  if (answercoeff[5,i] !=0) dummiegen<-c(dummiegen,"E")
  posgen[[i]]<-dummiegen
}
##damos una representacion de las respuestas acorde con la cantidad de las respuestas que han recibido

nPob<-200
##Tambien podemos crear una poblacion de soluciones aleatorias
for (i in 1:nPob){
  for (j in 1:25){
    populsol[i,j]<-sample(posgen[[j]],1)
  }
}

for (i in 2:nPob){
  dummie<-q
  loc<-sample(1:25,2)
  dummie[loc[1]]<-sample(posgen[[loc[1]]],1)
  dummie[loc[2]]<-sample(posgen[[loc[2]]],1)
  populsol<-rbind(populsol,dummie)
}

##Ahora recorreremos posibles soluciones usando un algoritmo genetico:
calculaCalifications<-function(correctionpattern){
  calification<-rep(0,23)
  for (j in 1:nrow(Q1251617)){
    for (i in 1:(ncol(Q1251617)-1)){
      if(is.na(Q1251617[j,i])){}
      else{
        if(Q1251617[j,i]==correctionpattern[i]) calification[j]=calification[j]+1
      }
    }
  }
  return(calification)
}
calculaAdaptacion<-function(calificacion){
  return(sum(abs(Q1251617[,26]-calificacion))/23)
}

## Variables de dibujo
maxplot<-1
meanplot<-1
minplot<-1

Busquedasol<-function(){
  ptm <- proc.time()
  mincounter<-0
  counter<-0
  nCan<-5
  bestq<-1
  califications <- apply (populsol, 1, calculaCalifications)
  adapPobla<-apply (t(califications), 1, calculaAdaptacion)
  nIte<-100000

  for(iter in 1:nIte){
    dummie<-0
    ## reproductores
    iCandidatos <- sample (nPob, nCan)
    indice      <- order (adapPobla[iCandidatos])
    iPadres     <- iCandidatos[indice[1:2]]
    padres      <- populsol[iPadres,]
  
    ##Cruce de los padres
    pos   <- sample (25-1,1)
    hijo1<-padres[1,1:pos]
    hijo2<-padres[2,1:pos]
    hijo1<-c(hijo1,padres[2,-(1:pos)])
    hijo2<-c(hijo2,padres[1,-(1:pos)])
  
    ##mutacion de dos elementos
    mutCandidatos <- sample (nPob, 4)
    locmut<-sample(1:25,4)
  
    mut1<-populsol[mutCandidatos[1],]
    mut1[locmut[1]]<-sample(posgen[[locmut[1]]],1)
  
    mut2<-populsol[mutCandidatos[2],]
    mut2[locmut[2]]<-sample(posgen[[locmut[2]]],1)
  
    if(mean (adapPobla)<1.1*min(adapPobla)){##añadimos mas mutacion si la media esta muy cerca del minimo (muy centrado en una region)
  
      mut3<-populsol[mutCandidatos[3],]
      mut3[locmut[3]]<-sample(posgen[[locmut[3]]],1)
      populsol[mutCandidatos[3],]<-mut3
  
      mut4<-populsol[mutCandidatos[4],]
      mut4[locmut[4]]<-sample(posgen[[locmut[4]]],1)
      populsol[mutCandidatos[4],]<-mut4
      adapMutb<- c (calculaAdaptacion (calculaCalifications(mut3)),
                   calculaAdaptacion (calculaCalifications(mut4)))
      
      indice    <- order (adapPobla)
      populsol <- rbind (populsol[indice[1:(nPob-2)],],
                         mut3,mut4)
      adapPobla <- c (adapPobla[indice[1:(nPob-4)]], adapMutb)
    }
  
  
    ## evaluacion
    adapHijos <- c (calculaAdaptacion (calculaCalifications(hijo1)),
                    calculaAdaptacion (calculaCalifications(hijo2)))
    
    adapMut<- c (calculaAdaptacion (calculaCalifications(mut1)),
                calculaAdaptacion (calculaCalifications(mut2)))
    
    ## seleccion
    indice    <- order (adapPobla)
    populsol <- rbind (populsol[indice[1:(nPob-4)],],
                      hijo1, hijo2,mut1,mut2)
    adapPobla <- c (adapPobla[indice[1:(nPob-4)]], adapHijos,adapMut)
    cat (iter, ": media=", mean (adapPobla),
         " min=",          min  (adapPobla),
         " max=",          max  (adapPobla),
         " contador para nueva poblacion=", counter,"\n")
    ##Dibujo
    
    meanplot<-c(meanplot,mean(adapPobla))
    minplot<-c(minplot,min(adapPobla))
    maxplot<-c(maxplot,max(adapPobla))
    minmin <- min (minplot)
    maxmax <- max (maxplot)
    plot  (minplot,  col = 4, type="l",ylim = c (minmin, maxmax))
    lines(maxplot,type="l",col=1)
    lines(meanplot,type="l",col=2)
    
    
    if(mincounter==min(adapPobla)){
      counter=counter+1
    }else{
      counter<-0
    }
    mincounter<-min(adapPobla)
    if (counter==1000){
      cat("Creando nueva poblacion, dado el estancamiento de la anterior")
      newpop<-populsol[which.min(adapPobla),]
      populsol<-matrix(q,ncol=25)
      populsol[1,]<-newpop
      ##cat("\n",length(newpop))
      for (i in 2:nPob){
        dummie<-newpop
        loc<-sample(1:25,2)
        dummie[loc[1]]<-sample(posgen[[loc[1]]],1)
        dummie[loc[2]]<-sample(posgen[[loc[2]]],1)
        populsol<-rbind(populsol,dummie)
      }
      califications <- apply (populsol, 1, calculaCalifications)
      adapPobla<-apply (t(califications), 1, calculaAdaptacion)
      counter<-0
    }

    mincounter<-min(adapPobla)  
    if (min (adapPobla) == 0L)
    {
      solucion <- populsol[which(adapPobla==0L),]
      cat("solucion: ",solucion)
      return (iter)
    }
  }
  proc.time() - ptm
}
##combinacion con valores 0.6956522                                                                                                
##"C"    "A"    "C"    "D"    "A"    "D"    "D"    "A"    "D"    "B"    "E"    "E"    "B"    "B"    "B"    "A" 
##"D"    "B"    "C"    "B"    "D"    "E"    "B"    "C"    "B" 

##combinacion con valores 0.4347826                                                                                              
##"C"    "B"    "B"    "D"    "D"    "A"    "C"    "C"    "E"    "E"    "A"    "A"    "B"    "E"    "D"    "B" 
##"E"    "B"    "C"    "B"    "D"    "B"    "A"    "C"    "D" 

##combinacion con valores 0.3478261                                                                                             
##"C"    "A"    "C"    "D"    "E"    "E"    "E"    "C"    "B"    "E"    "A"    "E"    "B"    "B"    "B"    "A" 
##"D"    "B"    "A"    "A"    "E"    "D"    "B"    "E"    "D"

##solucion:  C B C D B D E C D E A A B A B A D B C B C E B C B

###################################################
###     Metodo de regresion                     ###
###################################################

library(dummy)
library(caret)

##Modelo con respuesta el mayor numero de respuestas

lmod<-lapply(lapply(apply(Q1251617[,2:25],2, table), sort),tail,n=1) #opcion mas elegida
solmod<-rapply(lmod, names)
for(i in 1:23){
  print(c(Q1251617[i,"total"],sum(Q1251617[i,2:25]==solmod, na.rm=TRUE)))}

##quitamos individuos con NA's
Q1251617noNA<-Q1251617[complete.cases(Q1251617),]

y <- as.numeric(Q1251617noNA[-25,26])
x<-dummy(Q1251617noNA[-25,-1], int=TRUE)
namesq<-names(x) #guardamos los nombres
x<-as.matrix(x) # reconvertimos en matriz

midatafr<-data.frame(x=x,total=y)
names(midatafr)<-c(namesq,"total")

trainctr<-trainControl(method="cv", number=20) #LOOCV n-1
larsTune<-train(x,y, method="lars",
                tuneGrid=data.frame(.fraction=seq(0.01, 0.99, length=50)),
                trControl=trainctr, normalize=FALSE, intercept=FALSE,
                type="lasso")

lars.model<-lars(x,y, type="lasso", normalize=FALSE, intercept=FALSE, max.steps=200)
summary(lars.model)

val<-predict(lars.model, x, type="coefficients")$coefficients[30,]
names(val)[val >0]

val<-predict(lars.model, x, type="coefficients")$coefficients[23,]
names(val)[val >0]

##NO ES SUFICIENTE, HAY INCERTIDUMBRE, AÑADIMOS 1 DATO, LA FILA 3 CAMBIANDO NA POR A

Q1251617[3,16]<-"A"
Q1251617[3,26]<-Q1251617[3,26]+1
Q1251617noNA<-Q1251617[complete.cases(Q1251617),]
y <- as.numeric(Q1251617noNA[-25,26])
x<-dummy(Q1251617noNA[-25,-1], int=TRUE)
namesq<-names(x) #guardamos los nombres
x<-as.matrix(x) # reconvertimos en matriz

midatafr<-data.frame(x=x,total=y)
names(midatafr)<-c(namesq,"total")

trainctr<-trainControl(method="cv", number=30) #LOOCV n-1
larsTune<-train(x,y, method="lars",
                tuneGrid=data.frame(.fraction=seq(0.01, 0.99, length=50)),
                trControl=trainctr, normalize=FALSE, intercept=FALSE,
                type="lasso")

lars.model<-lars(x,y, type="lasso", normalize=FALSE, intercept=FALSE, max.steps=200)
summary(lars.model)

val<-predict(lars.model, x, type="coefficients")$coefficients[23,]
names(val)[val >0]

val<-predict(lars.model, x, type="coefficients")$coefficients[30,]
names(val)[val >0]

######
#Posible solucion:
# "C" "B" "C" "D" "C" "D" "E" "C" "D" "B" "A" "A" "B" "B" "B" "A" "D" "B" "C" "B" "C" "E" "B" "C" "B"
#
####
#q<-c("C", "B" ,"C" ,"D", "C", "D", "E", "C" ,"D" ,"B", "A" ,"A" ,"B" ,"B", "B" ,"A" ,"D" ,"B", "C" ,"B", "C", "E", "B", "C", "B")
#q<-c("C", "B" ,"C" ,"D", "C", "D", "E", "C" ,"D" ,"B", "A" ,"A" ,"B" ,"B", "C" ,"A" ,"D" ,"B", "C" ,"B", "C", "E", "B", "C", "B")
##ESTA OFRECE LA APROXIMACION MAS CERCANA
q<-c("C", "B" ,"C" ,"D", "C", "D", "E", "C" ,"D" ,"E", "A" ,"A" ,"B" ,"B", "B" ,"A" ,"D" ,"B", "C" ,"B", "C", "E", "B", "C", "B")
#q<-c("C", "B" ,"C" ,"D", "C", "D", "E", "C" ,"D" ,"E", "A" ,"A" ,"B" ,"B", "C" ,"A" ,"D" ,"B", "C" ,"B", "C", "E", "B", "C", "B")
#Calculo de puntuaciones finales
calification<-calculaCalifications(q)
sum(abs(Q1251617[,26]-calification))/23

##De esto se puede ver que el numero minimo de errores en esta clasificacion es 2,
Q1251617[,26]-calification

Q1251617[13,]
Q1251617[19,]
Q1251617[21,]


##cambiando la pregunta 2
q<-c("C", "A" ,"C" ,"D", "C", "D", "E", "C" ,"D" ,"E", "A" ,"A" ,"B" ,"B", "B" ,"A" ,"D" ,"B", "C" ,"B", "C", "E", "B", "C", "B")
calification<-calculaCalifications(q)
sum(abs(Q1251617[,26]-calification))/23
Q1251617[,26]-calification

##cambiando la pregunta 5
q<-c("C", "B" ,"C" ,"D", "B", "D", "E", "C" ,"D" ,"E", "A" ,"A" ,"B" ,"B", "B" ,"A" ,"D" ,"B", "C" ,"B", "C", "E", "B", "C", "B")
calification<-calculaCalifications(q)
sum(abs(Q1251617[,26]-calification))/23
Q1251617[,26]-calification

Q1251617[4,]
Q1251617[6,]
Q1251617[7,]
Q1251617[9,]
Q1251617[13,]
Q1251617[14,]
Q1251617[15,]
Q1251617[16,]
Q1251617[17,]
Q1251617[19,]
Q1251617[21,]
Q1251617[23,]