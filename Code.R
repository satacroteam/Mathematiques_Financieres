

#Importation des données#

Bouygues=read.csv("~/Desktop/Projet Math Fi/Bouygues.csv",header=TRUE,sep=";")
Danone=read.csv("~/Desktop/Projet Math Fi/Danone.csv",header=TRUE,sep=";")
Bouygues$Date <- as.Date(Bouygues$Date, "%d/%m/%Y")
Danone$Date <- as.Date(Danone$Date, "%d/%m/%Y")

#Données Brutes Bouygues#
head(Bouygues)

#Données Brutes Danone#
head(Danone)

#Evolution des cours des actifs étudiés sur l'historique donné#

Date=as.Date(B’ouygues$Date,"%d/%m/%Y")
Cours = data.frame(Danone$Cours,Bouygues$Cours)
matplot(Date,Cours,xaxt="n",type="l",lty="solid",pch=c(1,1), col=c("blue","green"),xlab = "Dates",ylab = "Cours",main = "Evolution des cours des deux actifs du portefeuille sur la période étudiée")
axis(1,at=Bouygues$Date,labels=Bouygues$Date)
legend("topleft", legend = c("Danone", "Bouygues"), col = c("blue", "green"), pch = 5, bty = "n", pt.cex = 0.5, cex = 0.8, horiz = TRUE, inset = c(0.1, 0.1))

#Vecteur de rendements historiques#

Rendement1=NULL
Rendement2=NULL
Rendement=NULL

for ( i in 1:(length(Danone[,1])-1)) {
   Rendement1[i]=((Danone[i+1,2]/Danone[i,2])-1)*100
}


for ( i in 1:(length(Bouygues[,1])-1)) {
    Rendement2[i]=((Bouygues[i+1,2]/Bouygues[i,2])-1)*100
}

Rendement=cbind(Rendement1,Rendement2)
head(Rendement)

#Représentation graphique des rendements#

#Rendement = data.frame(Rendement)
#matplot(Date[-1],Rendement,xaxt="n",type="l",lty="solid",pch=c(1,1), col=c("blue","green"),xlab = "Dates",ylab = "Rendements",main = "Evolution des rendements des deux actifs du portefeuille sur la période étudiée")
#axis(1,at=Bouygues$Date,labels=Bouygues$Date)
#legend("topleft", legend = c("Danone", "Bouygues"), col = c("blue", "green"), pch = 5, bty = "n", pt.cex = 0.5, cex = 0.8, horiz = TRUE, inset = c(0.1, 0.1))


#Boîtes à moustache des rendements#

par(mfrow=c(1,2))
boxplot(Rendement[,1],xlab="Danone",ylab="Taux de rentabilité (%)")
boxplot(Rendement[,2],xlab="Bouygues",ylab="Taux de rentabilité (%)")
par(mfrow=c(1,1))

#Matrice de variance-covariance#

matcov=as.matrix(cov(Rendement))



#Représentation graphique en fonction de alpha du rendement et du risque #


x=seq(0,1,0.0001)

y=NULL
z=NULL

rendement=function(x){
    y=x*mean(Rendement[,1])+(1-x)*mean(Rendement[,2])
    return (y)
}

y=rendement(x)

risque=function(x){
    y=(x^2)*matcov[1,1]+((1-x)^2)*matcov[1,1]+2*x*(1-x)*matcov[1,2]
    return (y)
}



z=risque(x)

par(mfrow=c(2,1))
plot(rendement(x),type="l",ylim=c(0,0.7),xlab="Paramètre alpha (‰)",ylab="Rendement",col="green")
plot(risque(x),type="l",ylim=c(15,30),xlab="Paramètre alpha (‰)",ylab="Risque",col="blue")
par(mfrow=c(1,1))


#Graph=data.frame(y,z)
#matplot(x,Graph,type="l",lty="solid",pch=c(1,1), col=c("blue","green"),xlab = "",ylab = "",main = "")



#Représentation du risque en fonction du rendement#

risquerend=function(x){
    
    y=(x-mean(Rendement[,2]))/(mean(Rendement[,1])-mean(Rendement[,2]))
    
    z=risque(y)

    return (z)
}

plot(risquerend(y),y,type="l",xlab="Risque",ylab="Rendement",main="Rapport rendement-risque du portefeuille")

#En choisissant x=18% #

Graph=data.frame(x,y,z)

#Il y a deux rendements possibles pour un portefeuille avec un risque de 18% :
#Le premier est : alpha=0.5120 et alpha=0.1973

#On choisisra le portefeuille 1 ( 0.5120 ; 0.488 ) avec un rendement de 0.3886238
#Contre le portefeuille 2 ( 0.1973 ; 0.8027) avec un rendement de 0.2550063


#Le risque pour un portefeuille avec un rendement de 0.5% est 17.65527 avec alpha=0.7743167

min(Graph$z)

#Le risque minimal est de 16.07074 pour le portefeuille ( 0.5001 ; 0.4999 )





#rendrisque=function(x){
#a=NULL
#b=NULL
#c=NULL
#d=NULL
    
# a=matcov[1,1]+matcov[2,2]-2*matcov[1,2]

# b=2*matcov[1,2]-2*matcov[2,2]

# c=matcov[2,2]-x

    
    
#d=(b^2)-(4*a*c)
    
# return (d)

#}

