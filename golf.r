# -----------------------------------------------------------------------
# Correction de l'exercice "Entretien d'un terrain de golf" (slide 30)
# -----------------------------------------------------------------------

par(mfrow=c(1,2))  # Multi plots

# On commence par calculer l'aire totale du green (représentée par les zones 
# circulaires en vert kaki sur la carte) :
# A = pi * (10² + 10² + 7²) = 249*pi = 782.25 m²

A = 249*pi;

# On suppose la répartition des trous de taupes uniforme dans le plan.
# Le nombre de trous dans la green suit donc une loi de Poisson de 
# paramètre lambda, où lambda représente le nombre moyen de trous 
# que l'on peut espérer (craindre) découvrir dans le green. Ce nombre
# s'évalue par la produit de la densité de trous (115 /ha en moyenne) 
# par la surface de la zone concernée (782.25 m²). Rappel : 1 ha = 10000 m²

lambda = A*115/10000;

# En moyenne, on a donc 9 trous découverts sur la zone d'intérêt. 
#  Attention : il ne s'agit que d'une valeur moyenne. Le vrai nombre de trous 
# découverts par le gérant la veille du jour j est une loi de Poisson 
# de paramètre lambda. On considère tous les cas de figures entre 0 et
# 30 trous (nous vérifierons par la suite que la somme des probabilités 
# de tous ces cas représente la quasi-totalité de la masse de la distribution).

k = 0:30
p = exp(-lambda)*lambda^k/factorial(k)

# On trace la distribution
plot(k, p, pch=16, col='blue', xlab="# Trous")
lines(k, p, col='blue')

# On calcule la somme de toutes les valeurs de probabilités considérées
# Avec l'option "options(digits=16)", on contrôle que la somme de probabilités 
# représente plus de 99.999999% de la masse de la distribution théorique.
somme = sum(p)

# Un équipe coûte 1000€ et permet de reboucher exactement 8 trous avant la 
# compétition. Le budget du gérant étant de 2000€, il peut donc reboucher 
# au maximum 16 trous. La probabilité que la compétition puisse se tenir 
# est donc la probabilité P(k < 17). 

proba = sum(p[1:17])

# Note : attention, l'indice de tableau 17 correspond bien à P(k=16). La ligne
# d'instruction ci-dessus calcule donc bien la somme cumulée de P(k) pour k
# allant de 0 à 16 inclus. 

# -----------------------------------------------------------------------
# On trouve un probabilité P = 98.89% que la compétition puisse se tenir.
# -----------------------------------------------------------------------

# En admettant que le gérant décide de maintenir la compétition. Quel sera 
# son coût moyen ?  On commence par former un vecteur contenant le prix de 
# l'intervention en fonction du nombre de trous (pour k allant de 0 à 30).

cost = (floor((k-1)/8)+1)*1000
plot(cost, pch=16, col='blue', xlab="# Trous", ylab="Cout (euros)")

# On calcul alors la moyenne du vecteur de cout pondéré par les probabilités
moyenne = sum(cost*p) 

# -----------------------------------------------------------------------
# On trouve donc une valeur moyenne de 1554.76 €.
# -----------------------------------------------------------------------

# Au demeurant, il est intéressant de remarquer que rien ne garantit que le
# coût moyen d'intervention est inférieur au budget de 2000€. Par exemple, 
# si le prestataire facture un surcoût de 50 000 € à partir de le 3eme équipe
# d'intervention, alors le coût moyen de l'opération passe à 2107.85 euros.

cost_avec_surcout = cost
cost_avec_surcout[18:31] = cost_avec_surcout[18:31] + 50000
moyenne_avec_surcout = sum(cost_avec_surcout*p) 

# Pourtant, la probabilité que la compétition puisse se tenir ne change pas, 
# et vaut toujours 98.89 %. On est donc typiquement ici face à un cas où la 
# présence d'outliers (en l'occurence le surcoût pour affecter plus de 3 
# équipes à la tâche), rend la moyenne peu représentative. La probabilité que 
# le gérant puisse débourser la somme est très élevée, quand bien même le coût 
# moyen de l'opération est supérieur à son budget.

