library('pls')
library('ggplot2')
library('DiscriMiner')
data(oliveoil)

sens.pls <- plsr(sensory ~ chemical, ncomp = 4, scale = TRUE, data =
                   oliveoil, validation = "LOO")

names(sens.pls)

sens.pls$valid$PRESS

#barplot(sens.pls$valid$PRESS[1,], main='Yellow')
#barplot(sens.pls$valid$PRESS[2,], main='Green')
#barplot(sens.pls$valid$PRESS[3,], main='Brown')
#barplot(sens.pls$valid$PRESS[4,], main='Glossy')
#barplot(sens.pls$valid$PRESS[5,], main='Transp')
#barplot(sens.pls$valid$PRESS[6,], main='Syrup')

barplot(sens.pls$valid$PRESS[5,])

plot(sens.pls)

corrplot(sens.pls, labels = colnames(oliveoil[["chemical"]]), comps=1:2)
corrplot(sens.pls, labels = colnames(oliveoil[["chemical"]]), comps=3:4)

# Partie 2

# on crée la variable origin (à la shlag mais bon)
oliveoil$origin <- factor(rep(c("Grèce", "Italie", "Espagne"), c(5, 5, 6)))

discPower(oliveoil[,1:11], oliveoil$origin)
     
