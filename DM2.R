# Partie 1

library(pls)
data(oliveoil)
View(oliveoil)

sens.pls <- plsr(sensory ~ chemical, ncomp=4, scale = TRUE, data = oliveoil, validation = "LOO")
names(sens.pls)

barplot(sens.pls$validation$PRESS[5,]) # 1 comp 
#barplot(plsr(sensory~chemical,scale=TRUE,data=oliveoil,validation="LOO")$validation$PRESS)

plot(sens.pls) # c'est nul
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


plot(cor(sens.pls$scores,oliveoil$chemical))

plot(sens.pls$scores)
scores(sens.pls)
# fit <- lm(sensory ~ chemical, data = oliveoil)
# summary(fit)
# fit$fitted.values

# Partie 2
install.packages(pkgs='./DiscriMiner_0.1-29.tar.gz') #suppose que le fichier est dans le dossier courant
library(DiscriMiner)
library(ggplot2)

oliveoil$origine = c(rep("Grèce",5),
                     rep("Italie",5),
                     rep("Espagne",6))

discPower(oliveoil[,1:2],oliveoil$origine)
# Peroxide
# K232
# yellow
# green
