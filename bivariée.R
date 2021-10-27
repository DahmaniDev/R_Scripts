library(questionr)
data(rp2012)
par(mfrow=c(1,2))
plot(rp2012$cadres, rp2012$proprio) #absence de corrélation
plot(rp2012$cadres, rp2012$dipl_sup)#présence d'une corrélation linéaire
cor(rp2012$cadres, rp2012$dipl_sup) # la valeur du coefficient d e pearson est proche de 1 ce qui confirme le résultat graphique
cor(rp2012$cadres, rp2012$dipl_sup, method="spearman")
cor(rp2012$cadres, rp2012$proprio) # la valeur est faible ce qui confirme l'absene de corrélation
cor(rp2012$cadres, rp2012$proprio, method="spearman")
###quali-quali
data("hdv2003")
cont=table(hdv2003$qualif,hdv2003$sexe)
mosaicplot(cont)
chisq.test(cont) #p-valus<0,05 on rejette l'hypothèse d'indépendance
##qauli-quanti
sportif=hdv2003[hdv2003$sport=="Oui",]
non_sportif=hdv2003[hdv2003$sport=="Non",]
mean(sportif$age)
mean(non_sportif$age) # les deux moyennes sont différentes, on peut mettre
#l'hypothèse que ces deux variables sont dépendantes.
#Mais s'agit t il d'une différence significative?
boxplot(hdv2003$age~hdv2003$sport) ## les deux boites sont différentes on peut conclure que les deux variable sont dépendnates.

hist(sportif$age) ##distribution s'éloigne de la loi normale
hist(non_sportif$age) ##distribution se raproche de la loi normale
shapiro.test((sportif$age))
shapiro.test((non_sportif$age))
##p-value<0,05 Le test est significatif dans les deux cas et rejette l'hypothèse d'une normalité des deux distributions.
#On ne peut pas exécuter le test de student
#test de wilcox
wilcox.test(hdv2003$age~hdv2003$sport)
#La valeur p étant à nouveau extrêmement petite, on peut rejeter l'hypothèse d'indépendance et considérer que les distributions des âges dans les deux sous-populations sont différentes
