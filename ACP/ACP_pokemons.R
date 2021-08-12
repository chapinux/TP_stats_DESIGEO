library(dplyr)
library(ggplot2)
library(ade4)
library(factoextra)
library(palmerpenguins)




pokemons <-  read.csv("~/coursDESIGEO/TP_stats_DESIGEO/ACP/pokemons.csv")

names(pokemons)

pokemons$Generation <- as.factor(pokemons$Generation)
pokemons$Sp..Atk %>%  unique()



p1 <-  ggplot(pokemons)+
  geom_point(aes(x=Attack, y= Defense, color=Generation, shape=Legendary))+
  theme_light()

p1

p2 <-  ggplot(pokemons)+
  geom_point(aes(x=Speed, y= HP, color=Generation, shape=Legendary))+
  theme_light()
p2


plot(penguins)
plot(penguins$body_mass_g, penguins$bill_length_mm)
plot(pokemons %>%  select(Attack, Defense, Speed, HP, Sp..Atk, Sp..Def))


p3 <- ggplot(penguins)+
  geom_point(aes(x=body_mass_g, y=bill_length_mm), color="darkcyan")+
  theme_light()+
  xlab("Variable A") + ylab("Variable B")
p3


#ACP

dataACP <- penguins %>% select(body_mass_g,bill_length_mm) %>% na.omit()  
dataACP$body_mass_g <-  dataACP$body_mass_g / 100

dataACP <- pokemons  %>% filter(Generation ==1) %>% select(Attack, Defense, Speed, HP, Sp..Def, Sp..Atk) %>% na.omit() 


res.pca <- dudi.pca(dataACP,
                    scannf = FALSE,   # Cacher le scree plot
                    nf = 5 ,scale = T           # Nombre d'axes gardés
  )

#scree plot
fviz_eig(res.pca)
# 
s.corcircle(res.pca$co)


percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
prctAxe1 <-  (res.pca$eig[1] /  sum(res.pca$eig)) %>%  percent()
prctAxe2 <-  (res.pca$eig[2] /  sum(res.pca$eig)) %>%  percent()

fviz_pca_var(res.pca) + xlab(paste0("Axe 1 (", prctAxe1,")")) + ylab(paste0("Axe 2 (", prctAxe2,")"))


# Graphique des individus. Coloration en fonction du cos2 (qualité de représentation). Les individus similaires sont groupés ensemble.
fviz_pca_ind(res.pca,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             label="none"
) +xlab(paste0("Axe 1 (", prctAxe1,")")) + ylab(paste0("Axe 2 (", prctAxe2,")"))




fviz_pca_ind(res.pca,
             repel = TRUE,
             label="none",
             habillage = pokemons %>% filter(Generation==1) %>% pull(Legendary)
) +xlab(paste0("Axe 1 (", prctAxe1,")")) + ylab(paste0("Axe 2 (", prctAxe2,")"))




#Graphique des variables. Coloration en fonction de la contribution des variables. Les variables corrélées positivement sont du même côté du graphique. Les variables corrélées négativement sont sur des côtés opposés du graphique.
fviz_pca_var(res.pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)





# point moyen
pm <-  c(mean(dataACP$body_mass_g, na.rm = T), mean(dataACP$bill_length_mm , na.rm = T))


matcov  <- cov(dataACP)
PC1 <-  eigen(matcov)$vectors[,1]
PC2 <-  eigen(matcov)$vectors[,2]

PC1 <- res.pca2$rotation[,1]
PC2 <- res.pca2$rotation[,2]


PC1 %*% PC2

slopeIntersect <-  function(v,P){
  
  v <-  v/sqrt(v[1]^2 + v[2]^2)
  
  intercept <-  (- v[2] * P[1] +   v[1]*P[2]) / v[1] 
  intercept <-  intercept %>% as.numeric()
  slope <- v[2] / v[1]  
  slope <-  slope %>%  as.numeric()
  return(list(slope=slope,intercept=intercept))
}

# vecteur directeur à partir de coeffeicient de y=lapha x ++ beta 
vectorDir <-  function ( alpha,beta){
  a <-  alpha 
  b <- -1 
  x <- b
  y <- -a 
  norm <-  sqrt(x^2 + y^2)
  return(c(x/norm, y/norm))   
}



slope1 <-  slopeIntersect(PC1,pm)$slope
slope2 <-  slopeIntersect(PC2,pm)$slope
intercept1 <-  slopeIntersect(PC1,pm)$intercept
intercept2 <-  slopeIntersect(PC2,pm)$intercept

vectorDir(slope1, intercept1) %*% vectorDir(slope2, intercept2)



p3 <- ggplot(dataACP)+
  geom_point(aes(x=body_mass_g , y=bill_length_mm), color="darkcyan")+
  geom_abline(slope = slope1, intercept= intercept1 ,  linetype = "dashed")+
  geom_abline(slope = slope2, intercept= intercept2 ,  linetype = "dashed")+
  xlab("Variable A") + ylab("Variable B")+coord_fixed()+
  annotate("text",  x=49, y=33 , label="Axe 2",angle = -62)+
  annotate("text",  x=65, y=58 , label="Axe 1", angle = 30)+
  theme_light()
p3


ggplot(res.pca$x)+
  geom_point(aes(x=PC1 , y=PC2), color="royalblue")+
  theme_light()
  



res.pca <-  prcomp(dataACP,scale. = F)
autoplot(res.pca, loadings=F, colour="royalblue")+xlab("Axe 1") + ylab("Axe 2")


res.pca$rotation

XX <- matrix(c(1,0),0, ncol=2, nrow=1)
YY <- matrix(c(0,1),0, ncol=2, nrow=1)

mat <- res.pca$rotation 
  
vecXX <-  XX %*% mat
vecYY <-  YY %*% mat

pm <- c(res.pca$x[,1] %>%  mean, res.pca$x[,2] %>%  mean)
SIX <- slopeIntersect(vecXX, pm )
SIY <- slopeIntersect(vecYY, pm )

autoplot(res.pca,  colour="royalblue")+xlab("Axe 1") + ylab("Axe 2")+
  geom_abline(slope = SIX$slope, intercept= SIX$intercept ,  linetype = "dashed")+
  geom_abline(slope = SIY$slope, intercept= SIY$intercept ,  linetype = "dashed")+
  annotate("text",  x=0.13 , y=-0.08 , label="Axe Variable A",angle =  -12 )+
  annotate("text",  x=0.1, y= 0.17 , label="Axe Variable B",angle = 40)


#scree plot
res.pca <-  prcomp(dataACP,scale. = F, rank. = 4)
fviz_eig(res.pca)




#bunnny
dfbunny <-  read.csv("~/coursDESIGEO/cours_stats_DESIGEO/ACP_Cours/data/bunny.obj", header = F, sep=" ")


dfbunny <- dfbunny %>%  filter(V1=="v") 
names(dfbunny) <-  c("type", "x", "y", "z")
dfbunny <-  dfbunny[,-1]
plot(dfbunny)


ggplot(dfbunny)+
  geom_point(aes(x=x, y=y), size = .1)

ggplot(dfbunny)+
  geom_point(aes(x=y, y=z), size = .1)


ggplot(dfbunny)+
  geom_point(aes(x=x, y=z), size = .1)

res.pca <- dudi.pca(dfbunny,
        scannf = FALSE,   # Cacher le scree plot
         nf =2 ,scale = T           # Nombre d'axes gardés
          )


fviz_pca_ind(res.pca)



# ellipse 2D 
x <- rnorm(5000, 3, 4)
y <- rnorm(5000, 3,2)
df <- data.frame(x=x, y=y)


ggplot(df)+
  geom_point(aes(x=x, y=y), colour= "white", fill="darkcyan",shape=21)+
  theme_light()+
  ggtitle("Inertie importante")+
  coord_fixed(xlim=c(-12,20), ylim=c(-5,10))
  

x <- rnorm(5000, 3, 1)
y <- rnorm(5000, 3,0.5)
df <- data.frame(x=x, y=y)


ggplot(df)+
  geom_point(aes(x=x, y=y), colour= "white", fill="darkcyan",shape=21)+
  theme_light()+
  ggtitle("Inertie faible")+
  coord_fixed(xlim=c(-12,20), ylim=c(-5,10))


generate_ellipse <- function(X,Y,Z,sdX=1, sdY=1, sdZ=1){
Xcoord <- rnorm(1000,mean = X, sd =   sdX )
Ycoord <- rnorm(1000,mean = Y, sd =   sdY )
Zcoord <- rnorm(1000,mean = Z, sd =   sdZ )
ell1 <-  data.frame(Xcoord, Ycoord, Zcoord)
return(ell1)
}

ell1 <-  generate_ellipse(5,0,0)
ell2 <-  generate_ellipse(-5,0,0)
ell3 <-  generate_ellipse(0,5,0)

ellipses <-  bind_rows(ell1, ell2,ell3)

ggpairs(ellipses)


res.pca <- dudi.pca(ellipses,scannf =F)

fviz_pca_var(res.pca)
fviz_pca_ind(res.pca)




angleX <-  pi/2
rotmatX <- matrix(data = c(1,0,0,0,cos(angleX),-sin(angleX),0,sin(angleX),cos(angleX)), nrow=3 , ncol=3)


angleY <-  pi*0.5 
rotmatY <- matrix(data = c(cos(angleY),0,sin(angleY),0,1,0,-sin(angleY),0,cos(angleY)), nrow=3 , ncol=3)


angleZ <-  pi *0.2
rotmatZ <- matrix(data = c(cos(angleZ),-sin(angleZ),0,sin(angleZ),cos(angleZ),0,0,0,1), nrow=3 , ncol=3)



ellipses %>%  as.matrix() %*% rotmatX %*% rotmatZ %*% rotmatY %>%  as.data.frame() %>% ggpairs
ellipses %>%  as.matrix() %*% rotmat %>%  as.data.frame() %>% ggpairs





library(GGally)
dfTracts <-  read.csv("~/coursIGAST/projet_AS/projetAnaSpat_Toronto/averageTotalIncomeByTract.csv")

names(dfTracts)

ggpairs(dfTracts %>% select(Area..sq.km., Population, Dwellings, Households, v_CA16_4957..Average.total.income.in.2015.among.recipients....) %>% na.omit())


dataPCA <- dfTracts %>% select(Area..sq.km., Population, Dwellings, Households, v_CA16_4957..Average.total.income.in.2015.among.recipients....) %>% na.omit()

res.pca <- dudi.pca(dataPCA,scannf =F, scale = T)

fviz_pca_var(res.pca)
fviz_pca_ind(res.pca)







