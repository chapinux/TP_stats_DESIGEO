library(palmerpenguins)
library(dplyr)
library(ggplot2)

names(penguins)



mydata <- penguins[, c("flipper_length_mm", "body_mass_g")] %>% na.omit()
mydata$type <-  "empirical"


mymodel <- lm(mydata$body_mass_g ~ mydata$flipper_length_mm)
b <- mymodel$coefficients[1] %>%  as.numeric()
a <- mymodel$coefficients[2] %>%  as.numeric()
pred1 <-  a*mydata$flipper_length_mm + b
dfpred1 <- data.frame(flipper_length_mm= mydata$flipper_length_mm,body_mass_g=pred1, type="predicted")
mydata <-  rbind(mydata, dfpred1)


#milieu de la droite 
Xmid <- (min(dfpred1$flipper_length_mm) + max(dfpred1$flipper_length_mm) ) /2
Ymid <- (min(dfpred1$body_mass_g) + max(dfpred1$body_mass_g) ) /2

#vecgteur directeur après rotation
rotateLine <- function(x,y, alpha){
  cosa <-  cos(alpha)
  sina <-  sin(alpha)
  return(list(xv= cosa*x - sina*y , yv=sina*x + cosa*y ))
}


line2_direction <-  rotateLine(1, a, pi/2 )
#pour un vecteur directeur v(A,B) , l'équation de la droite associée est 
#-Bx + Ay +C = 0

inter2  <- (line2_direction$yv * Xmid - line2_direction$xv*Ymid)  / line2_direction$xv
slope2 <-   line2_direction$yv / -line2_direction$xv 




ggplot(mydata) +
  geom_point(aes(x=flipper_length_mm, y = body_mass_g, color=type), size=1)+
  geom_abline(slope=a, intercept = b, color= "red")+
  geom_line(aes(group=flipper_length_mm,x=flipper_length_mm, y = body_mass_g), color= "red", size=.1)+
  xlim(170,235)+
  geom_abline(intercept = inter2 , slope = slope2, color= "pink")+
  coord_cartesian(ylim=c(2200,6500))+
  scale_color_manual(values=c("#999999", "#E69F00"))




# source de l'idée : https://stats.stackexchange.com/questions/2691/making-sense-of-principal-component-analysis-eigenvectors-eigenvalues




