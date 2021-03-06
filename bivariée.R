library(questionr)
data(rp2012)
par(mfrow=c(1,2))
plot(rp2012$cadres, rp2012$proprio) #absence de corr�lation
plot(rp2012$cadres, rp2012$dipl_sup)#pr�sence d'une corr�lation lin�aire
cor(rp2012$cadres, rp2012$dipl_sup) # la valeur du coefficient d e pearson est proche de 1 ce qui confirme le r�sultat graphique
cor(rp2012$cadres, rp2012$dipl_sup, method="spearman")
cor(rp2012$cadres, rp2012$proprio) # la valeur est faible ce qui confirme l'absene de corr�lation
cor(rp2012$cadres, rp2012$proprio, method="spearman")
###quali-quali
data("hdv2003")
cont=table(hdv2003$qualif,hdv2003$sexe)
mosaicplot(cont)
chisq.test(cont) #p-valus<0,05 on rejette l'hypoth�se d'ind�pendance
##qauli-quanti
sportif=hdv2003[hdv2003$sport=="Oui",]
non_sportif=hdv2003[hdv2003$sport=="Non",]
mean(sportif$age)
mean(non_sportif$age) # les deux moyennes sont diff�rentes, on peut mettre
#l'hypoth�se que ces deux variables sont d�pendantes.
#Mais s'agit t il d'une diff�rence significative?
boxplot(hdv2003$age~hdv2003$sport) ## les deux boites sont diff�rentes on peut conclure que les deux variable sont d�pendnates.

hist(sportif$age) ##distribution s'�loigne de la loi normale
hist(non_sportif$age) ##distribution se raproche de la loi normale
shapiro.test((sportif$age))
shapiro.test((non_sportif$age))
##p-value<0,05 Le test est significatif dans les deux cas et rejette l'hypoth�se d'une normalit� des deux distributions.
#On ne peut pas ex�cuter le test de student
#test de wilcox
wilcox.test(hdv2003$age~hdv2003$sport)
#La valeur p �tant � nouveau extr�mement petite, on peut rejeter l'hypoth�se d'ind�pendance et consid�rer que les distributions des �ges dans les deux sous-populations sont diff�rentes
