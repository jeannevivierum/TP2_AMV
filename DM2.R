# Partie 1

library(pls)
library(plotrix)
setwd("C:/Users/abels/OneDrive/Bureau/AMV/TP2/TP2_AMV")
data(oliveoil)
View(oliveoil)

sens.pls <- plsr(sensory ~ chemical, ncomp=4, scale = TRUE, data = oliveoil, validation = "LOO")
names(sens.pls)

sens.pls$validation$PRESS[5,]

barplot(sens.pls$validation$PRESS[5,]) # 1 comp 
barplot(sens.pls$validation$PRESS)
#barplot(plsr(sensory~chemical,scale=TRUE,data=oliveoil,validation="LOO")$validation$PRESS)

plot(sens.pls) # c'est nul
abline(a=0,b=1)
oliveoil$chemical
oliveoil$sensory

sens.pls1 <- plsr(sensory ~ chemical, ncomp = 1, scale = TRUE, data = oliveoil, validation = "LOO")
plot(sens.pls1)

corrplot(sens.pls,comps = 1:2, radii = c(sqrt(0.7),1), labels = "names")
# Acidity, K232, Peroxide

corrplot(sens.pls,comps = 2:3, radii = c(sqrt(0.7),1), labels = "names")

corrplot(sens.pls,comps = c(1,3), radii = c(sqrt(0.7),1), labels = "names")

corrplot(sens.pls, comps = c(2,4), radii = c(sqrt(0.7),1), labels = "names")

corrplot(sens.pls,comps = c(1,4), radii = c(sqrt(0.7),1), labels = "names")

corrplot(sens.pls,comps = c(3,4),radii = c(sqrt(0.7),1),labels = "names")

plot(cor(sens.pls$scores,oliveoil$chemical))

cor_comp <- cor(sens.pls$scores,oliveoil$chemical)
t_cor_comp <- t(cor_comp)

plot(x = t_cor_comp[,1],y = t_cor_comp[,2],xlim = c(-1,1),ylim = c(-1,1),type = "n",asp = 1)
text(x=t_cor_comp[,1],y=t_cor_comp[,2],
     label=paste("    ",colnames(oliveoil$chemical)))
axis(1, pos = c(0,0), labels=FALSE, at=seq(-1,1,.5))
axis(2, pos = c(0,0), labels=FALSE, at=seq(-1,1,.5))
draw.circle(x=0,y=0,radius = 1)
draw.circle(x=0,y=0,radius = sqrt(0.7))


plot(sens.pls$scores)
scores(sens.pls)
# fit <- lm(sensory ~ chemical, data = oliveoil)
# summary(fit)
# fit$fitted.values

sens <- oliveoil$sensory
cor_sens <- t(cor(sens.pls$scores,sens))

plot(x = cor_sens[,1], y =cor_sens[,2],xlim = c(-1,1),ylim=c(-1,1),type = "p",asp = 1,
     xlab = "Composante 1",ylab = "Composante 2")
text(x = cor_sens[,1], y = cor_sens[,2], label = paste("           ",colnames(sens)))
axis(1,pos = c(0,0), labels = FALSE, at = seq(-1,1,.5))
axis(2,pos = c(0,0), labels = FALSE, at = seq(-1,1,.5))
draw.circle(x=0,y=0,radius = 1)
draw.circle(x=0,y=0,radius = 0.6)

plot(x = cor_sens[,3], y =cor_sens[,4],xlim = c(-1,1),ylim=c(-1,1),type = "p",asp = 1,
     xlab = "Composante 3",ylab = "Composante 4")
text(x = cor_sens[,3], y = cor_sens[,4], label = paste("           ",colnames(sens)))
axis(1,pos = c(0,0), labels = FALSE, at = seq(-1,1,.5))
axis(2,pos = c(0,0), labels = FALSE, at = seq(-1,1,.5))
draw.circle(x=0,y=0,radius = 1)
draw.circle(x=0,y=0,radius = 0.6)

# cor_Y_g <- t(cor(sens.pls$Yscores,sens)) # cor_Y_g pour corrélation entre les Y et 
#                                          # g (composantes des Y)
# plot(x = cor_Y_g[,1], y = cor_Y_g[,2], xlim = c(-1,1),ylim = c(-1,1), type = "p",asp = 1,
#      xlab = "Composante 1", ylab = "Composante 2")
# text(x = cor_Y_g[,1], y = cor_Y_g[,2], label = paste("           ",colnames(sens)))
# axis(1,pos = c(0,0), labels = FALSE, at = seq(-1,1,.5))
# axis(2,pos = c(0,0), labels = FALSE, at = seq(-1,1,.5))
# draw.circle(x=0,y=0,radius = 1)
# draw.circle(x=0,y=0,radius = 0.7)
# 
# corrplot(sens.pls,comps = 1:2,labels = "names",ploty = TRUE)

###########################################################################################

# Partie 2
install.packages(pkgs='./DiscriMiner_0.1-29.tar.gz') #suppose que le fichier est dans le dossier courant
library(DiscriMiner)
library(ggplot2)
library(ggforce) #pour tracer un cercle en ggplot

oliveoil$origine = factor(c(rep("Grèce",5),
                     rep("Italie",5),
                     rep("Espagne",6)))

discPower(oliveoil[,1:2],oliveoil$origine)
# Peroxide
# K232
# yellow
# green

AFDoils <- desDA(oliveoil[,1:2],oliveoil$origine, covar = "total")
AFDoils$values
# entièreté de la variance expliquée en deux axes, plus de la moitié sur le 1er

oliveoil$AFDf1 <- AFDoils$scores[,1]
oliveoil$AFDf2 <- AFDoils$scores[,2]
# oliveoil$discor1 <- AFDoils$discor[,1]
# oliveoil$discor2 <- AFDoils$discor[,2]

plot(x=oliveoil$AFDf1,y=oliveoil$AFDf2,col=oliveoil$origine)
text(x=oliveoil$AFDf1, y=oliveoil$AFDf2, label = paste("         ",1:16))

# avec ggplot
ggplot(oliveoil,aes(AFDf1,AFDf2,colour = origine)) +
  geom_point() + geom_text(aes(label = paste("       ",1:16)))
#l'axe AFDf1 semble déjà suffisant pour bien discriminer
#pour le 2ème on sépare bien l'Espagne du reste mais Grèce et Italie sont confondus

plot(x=oliveoil$AFDf1, y=oliveoil$AFDf2,type="n")
text(x=oliveoil$AFDf1, y=oliveoil$AFDf2,label=1:16)

plot(x=AFDoils$discor[,1],y=AFDoils$discor[,2], asp = 1)

plot(x=AFDoils$discor[,1],y=AFDoils$discor[,2], type = "n", asp=1,ylim = c(-1,1), xlim = c(-1,1))
text(x=AFDoils$discor[,1],y=AFDoils$discor[,2],
     label=rownames(AFDoils$discor))
axis(1, pos = c(0,0), labels=FALSE, at=seq(-1,1,.5))
axis(2, pos = c(0,0), labels=FALSE, at=seq(-1,1,.5))
draw.circle(x=0,y=0,radius = 1)
draw.circle(x=0,y=0,radius = sqrt(0.7))
# on retrouve bien les variables d'avant en terme de qualité de représentation

# avec ggplot
dat <- data.frame(discor1 = AFDoils$discor[,1],
                  discor2 = AFDoils$discor[,2],
                  names = rownames(AFDoils$discor))
ggplot(dat,aes(discor1,discor2)) +
  geom_point() + geom_text(aes(label = rownames(AFDoils$discor))) +
  geom_circle(aes(x0 = 0,y0=0,r=1)) + geom_circle(aes(x0 = 0, y0=0, r = 0.7)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
# c'est un peu mieux avec le plot de base limite

PLSDAoils <- plsDA(oliveoil[,1:2],oliveoil$origine,autosel = FALSE, comps = 2)
plot(PLSDAoils)
PLSDAoils$confusion
