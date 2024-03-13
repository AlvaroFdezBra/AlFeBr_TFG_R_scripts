########MODELOS PARAMETRICOS
# INSTALAR / CARGAR LIBRERIAS ---------------------------------------------

  # Lista de librerias necesarias
packages <- list("survival", "kableExtra", "dplyr", "stats", "ggplot2",
                 "survminer", "patchwork","gridExtra","fitdistrplus",
                 "SurvRegCensCov", "grid")

load_libraries <- function(packages){
# Package names
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
lapply(packages, library, character.only = TRUE) |>
  invisible()
}; load_libraries(packages)


# DEFINIR BASE DE DATOS DE TRABAJO ---------------------------------------
  #SE DEBEN ELIMINAR LOS DATOS QUE CONTIENEN 150ºC YA QUE NO PROPORCIONAN INFO

data <- subset(imotor, temp != 150)
rownames(data) <- c(1:dim(data)[1])

# AJUSTE WEIBULL -------------------------------------

fit <- survreg(data = data, Surv(time,status)~factor(temp),
               dist = "weibull")

fit_nocov <- survreg(data = data, Surv(time,status)~1,
                     dist = "weibull")






# Graficas funcion densidad y probabilidad; funcion riesgo y supervivencia

weibull_fF <- function(s, tmax, tmin, sigma, mu,return = "data"){
  #parámetros de la función
  s <- 1 #resolución de las funciones, a mayor valor más resolución 
  tmax <- 8064
  tmin <- 0
  #recordemos que la escala usada en la funcion de pweibull o dweibull
  #utilizan una parametrización diferente tal que:
  #factor forma: alfa = 1/sigma
  #factor escala: beta = exp(mu) = exp(intercept)
  shape = 1/sigma0
  scale = exp(mu0)
  
  x <- seq(from = 0, to = 8064, by = 1/s)
  l <- length(x)
  d <- dweibull(x, shape, scale)
  p <- pweibull(q = x, shape, scale)
  
  fx <- data.frame(x, y = d)
  
  Fx <- data.frame(x, y = p)
  
  #funciones distribución
  
  g1 <- ggplot()+
    geom_line(data = Fx, aes(x, y = Fx[,2]))+
    labs( x = "", y = "F(x)")+
    theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.ticks.x = element_line(colour = "black", linetype = "solid"),
          plot.title = element_text(hjust = 0.5)) 
  
  #funciones densidad
  
  g2 <- ggplot(fx, aes(x, y = fx[,2]))+
    geom_line()+
    labs( x = "Tiempo", y = "f(x)")+
    theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.ticks.x = element_line(colour = "black", linetype = "solid"),
          plot.title = element_text(hjust = 0.5)); 
  if(return == "data"){
    print(data.frame("Tiempo" = x,"F(t)" = p, "f(t)" = d))
  }else{
  return(g1/g2)
  }
  
}
weibull_fF(s = 1, tmax = 8064, tmin = 0, sigma, mu)

gamma <- c(0,unlist(fit$coefficients[2:3]))
theta <- -gamma
AF <- exp(theta)
mu = (fit$coefficients)[1]
sigma = fit$scale
alfa = exp(-mu/sigma)
beta = 1/sigma

table <- round(x = cbind(mu,sigma,gamma,theta,AF),4)  %>%
  cbind("alfa" = as.character(round(alfa,15)),"beta" = round(beta,4))
rownames(table)<-list("Z = 0", "Z = 1", "Z = 2")
kk <- kbl( table, booktabs = TRUE)  %>%
  kable_styling(row_label_position = "c", font_size = 17)

mu0 = (fit_nocov$coefficients)[1]
sigma0 = fit_nocov$scale
alfa0 = exp(-mu0/sigma0)
beta0 = 1/sigma0

weibull_Sh <- function(s, tmax, tmin, mu, sigma, alfa, return = "plot", tipo = "nocov"){
  
  x <- seq(from = 0, to = 8064, by = 1/s)
  l <- length(x)
  
  #con covariables
  sx <- vector("numeric", l)
  sx_cov <- vector("numeric",length(unique(data$temp))*l)
  
  for(i in 1:l){
    sx[i] <- exp(-alfa0*x[i]**(beta0))
    #if(i == l){
    # plot(sx)
    #}
  }
  
  for(j in 1:3){
    for(i in 1:l){
      sx_cov[i+l*((j)-1)] <- exp(-alfa*(x[i]*AF[j])**beta)
    }
    #if(j==3){
    # plot(sx_cov)
    #}
  }
  z <- rep(0:(length(unique(data$temp))-1),each = l)
  
  df_sx_cov<-data.frame("t" = x,z = c(z,rep("Sin covariables",l)),
                        "sx_cov" = c(sx_cov,sx))
  
  #otro metodo: S(x) = 1-F(x)
  #sx <- vector("numeric", 8065)
  #for(i in 1:8065){
  # sx[i]=1-Fx[i,2]
  # i=i+1
  #}
  df_sx <- data.frame("t"=x,sx)
  #otra forma de hacerlo tomando: 
  #sx <- exp(-a*x**b) ; donde:
  #a = 3.216402e-11; b = 2.832015e+00
  #obtenido del output de la función WeibullReg
  
  g4 <- ggplot(data = df_sx, aes(x = t,y = sx))+
    geom_line()+
    labs( x = "", y = "R(t)")+
    theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.ticks.x = element_line(colour = "black", linetype = "solid"),
          plot.title = element_text(hjust = 0.5)); 
  
  g5 <- ggplot(data = df_sx_cov[1:length(sx_cov),], aes(x = t[1:length(sx_cov)],
                                                        y = df_sx_cov[1:length(sx_cov),3],color = factor(z[1:length(sx_cov)])))+
    
    geom_line()+
    scale_color_manual(values = c("blue", "red", "firebrick"),
                       name = "Z")+
    labs( x = "Tiempo", y = "R(t)")+
    theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.ticks.x = element_line(colour = "black", linetype = "solid"),
          plot.title = element_text(hjust = 0.5))
  
  g5.1 <- ggplot(data = df_sx_cov, aes(x = t, y = df_sx_cov[,3],
                                       color = factor(z)))+
    geom_line()+
    scale_color_manual(values = c("blue", "red", "firebrick","black"),
                       name = "Z")+
    labs( x = "Tiempo", y = "R(t)")+
    theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.ticks.x = element_line(colour = "black", linetype = "solid"),
          plot.title = element_text(hjust = 0.5))
  
  hx_cov <- vector("numeric",length(unique(data$temp))*length(x))
  hx <- vector("numeric", length(x))
  
  ###con covariables
  for(j in 1:3){
    for(i in 1:l){
      hx_cov[i+l*(j-1)] <- AF[j]*(alfa*beta*(x[i]*AF[j])**(beta-1))
    }
  }
  df_hx_cov<- data.frame("t" = x,z = factor(z),hx_cov)
  
  
  ###sin covariables
  hx <- vector("numeric",length = l)
  for(i in 1:l){
    hx[i] <- alfa0*beta0*x[i]**(beta0-1) 
  }
  z <- rep(0:(length(unique(data$temp))-1),each = l)
  df_hx <- data.frame("t" = x, hx)
  
  
  g6 <- ggplot(df_hx_cov, aes(x = t, y = hx_cov, color = factor(z)))+
    geom_line()+
    scale_color_manual(values = c("blue", "red", "firebrick"),
                       name = "Z")+
    labs( x = "Tiempo", y = "h(t)")+
    coord_cartesian(ylim=c(0,0.0035))+
    geom_vline(xintercept = max(imotor[imotor$temp == 170 ,2]), color = "blue", linetype = "dotted")+
    geom_vline(xintercept = max(imotor[imotor$temp == 190 ,2]), color = "red", linetype = "dotted")+
    geom_vline(xintercept = max(imotor[imotor$temp == 220 ,2]), color = "firebrick", linetype = "dotted")+
    theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.ticks.x = element_line(colour = "black", linetype = "solid"),
          plot.title = element_text(hjust = 0.5))
  grid.text("t = 5448", x = 0.63, y = 0.25, just = "left", gp = gpar(col = "black", fontsize = 10))
  grid.text("t = 1680", x = 0.32, y = 0.23, just = "center", gp = gpar(col = "black", fontsize = 10))
  grid.text("t = 528", x = 0.23, y = 0.6, just = "right", gp = gpar(col = "black", fontsize = 10))
  ##se esperaba el solapamiento entre Z=0 y Z=1
  
  g7 <- ggplot(df_hx, aes(x = t, y = hx))+ 
    geom_line()+
    labs( x = "Tiempo", y = "h(t)")+
    theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.ticks.x = element_line(colour = "black", linetype = "solid"),
          plot.title = element_text(hjust = 0.5))
  
  fx_cov <- vector("numeric", l*3)
  Fx_cov <- vector("numeric", l*3)
  for(j in 0:2){
  for(i in 1:l){
  fx_cov[i+j*l] <- sx_cov[i+j*l]*hx_cov[i+j*l]
  Fx_cov[i+j*l] <- 1-sx_cov[i+j*l]
  }
  }
  
  df <- data.frame("y" = fx_cov, "t" = x, "z" = factor(z))
  g8 <- ggplot(df, aes(x = t, y = y, color = z))+ 
    geom_line(alpha = 0.8)+
    scale_color_manual(values = c("blue", "red", "firebrick"),
                       name = "Z")+
    labs( x = "", y = "f(t)")+
    theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.ticks.x = element_line(colour = "black", linetype = "solid"),
          plot.title = element_text(hjust = 0.5))
  
  df <- data.frame("y" = Fx_cov, "t" = x, "z"=factor(z))
  g9 <- ggplot(df, aes(x = t, y = y, color = factor(z)))+ 
    geom_line()+
    scale_color_manual(values = c("blue", "red", "firebrick"),
                       name = "Z")+
    labs( x = "Tiempo", y = "F(t)")+
    theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.ticks.x = element_line(colour = "black", linetype = "solid"),
          plot.title = element_text(hjust = 0.5))
  
  Hx_cov <- vector("numeric", l)
  for(j in 1:3){
    for(i in 1:l){
      Hx_cov[i+l*(j-1)] <- alfa*(x[i]*AF[j])**(beta)
    }
  }
  
  df_Hx <- data.frame("y" = Hx_cov, "t" = x, "z" = factor(z))
  g10 <- ggplot(df_Hx, aes(x = t, y = y, color = factor(z)))+ 
    geom_line()+
    geom_point(aes(x = 528, y = 0.5818), color = "black", show.legend = F)+
    geom_point(aes(x = 1680, y = 0.7412),color = "black", show.legend = F)+
    geom_point(aes(x = 5448, y = 1.2259),color = "black", show.legend = F)+
    scale_color_manual(values = c("blue", "red", "firebrick", "black"),
                       name = "Z")+
    labs( x = "Tiempo", y = "H(t)",)+
    geom_vline(xintercept = max(imotor[imotor$temp == 170 ,2]), color = "blue", linetype = "dotted")+
    geom_vline(xintercept = max(imotor[imotor$temp == 190 ,2]), color = "red", linetype = "dotted")+
    geom_vline(xintercept = max(imotor[imotor$temp == 220 ,2]), color = "firebrick", linetype = "dotted")+
    coord_cartesian(ylim = c(0,1.5))+
    theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
          axis.ticks.x = element_line(colour = "black", linetype = "solid"),
          plot.title = element_text(hjust = 0.5)) ; g10
  grid.text("t=528, y=0.58", x = 0.065, y = 0.5, just = "left", gp = gpar(col = "black", fontsize = 8))
  grid.text("t=1680, y=0.74", x = 0.215, y = 0.6, just = "center", gp = gpar(col = "black", fontsize = 8))
  grid.text("t=5448, y=1.23", x = 0.75, y = 0.8, just = "right", gp = gpar(col = "black", fontsize = 8))
  
  
  if(return == "data"){
    if(tipo == "cov"){
      return(data.frame("Tiempo" = x, "Z" = z, "F(t)" = Fx_cov,"f(t)" = fx_cov,
            "R(t)" <- sx_cov, "h(t)" = hx_cov, "H(t)" = Hx_cov))
    }else{
      return(data.frame("Tiempo" = x, "F(t)" = Fx[,2],"f(t)" =  fx[,2],
            "R(t)" <- sx, "h(t)" = hx))
    }
  }else{
return(grid.arrange(g4, g5, g7, g6, ncol=2, nrow=2))
  }
}

weibull_Sh(s = 1, tmax = 8064, tmin = 0,mu, sigma, alfa)


# OTROS -------------------------------------------------------------------


# Funcion para encontrar un valor---------------

min <- vector("integer",3)
tmin <- vector("numeric",3)
calc <- vector("numeric",3)
val <- 0.6
tol <- 0.0001
for(j in 1:3){
  a <- 1
  #print(j)
  for(i in 1:l){
    calc[j] <- abs(Hx_cov[l*(j-1)+i]-val)
    if((calc[j] < tol) & (a > calc[j])){
      a <- calc[j]
      min[j] <- i
      tmin[j] <- min[j]/s
    }
  }
}
##########intercepts ------------

mean = vector(mode="numeric", length= dim(data)[2])
median = vector(mode="numeric", length= dim(data)[2])
intercept = vector(mode="numeric", length= dim(data)[2])
regresion = vector(mode = "list", length = dim(data)[2])
for(i in 1:3){
  mean[i] = with(data,mean(time[temp == unique(temp)[i]]))
  median[i] = with(data,median(time[temp == unique(temp)[i]]))
  i = i+1
}

Fx <- vector("numeric",length = dim(data)[1])
y <- vector("numeric",length = dim(data)[1])
for(j in 1:3){
for(i in 1:10){
  Fx[i+10*(j-1)] <- (i - 0.5)/10
  y[i+10*(j-1)] = log(-log(1-Fx[i+10*(j-1)]))
}
}
  #en este casono b se estima como la exponencial del coeficiente de forma
  #(según la información de la funcion survreg, cito: 
                              #   survreg's intercept = log(rweibull scale)

  intercept = log(alfa)+beta*log(AF)
  slope = beta
  regresion <- paste("y =",round(intercept,2),"+",round(slope[1],2),"x")

  df_p <- data.frame("time" = data$time, y , z = paste("Z =",c(rep(0:2,each = 10))), "status" = data$status)
  
p <- ggplot(data = df_p, aes(x = log(time), y , color = factor(z), fill = factor(z))) +
  geom_point(aes(shape = factor(status)), size = 2)  +
  ylab("ln(-ln(1-F(x)))")+ xlab("ln(tiempo)")+
  guides(fill = "none")+
  geom_abline(slope =slope, intercept = intercept[1], color = "coral1") +
  geom_abline(slope = slope, intercept = intercept[2], color = "red") +
  geom_abline(slope = slope, intercept = intercept[3], color = "firebrick") +
  #geom_abline(slope = beta0, intercept = log(alfa0), color = "black") +
  scale_color_manual(values = c("Z = 0" = "coral1","Z = 1" = "red",
                                "Z = 2" = "firebrick"),name = "Z") +
  scale_fill_manual(values = c("Z = 0" = "coral1","Z = 1" = "red",
                               "Z = 2" = "firebrick")) + 
  scale_shape_manual(values = c(21, 22), labels = c("Censurado", "Averiado"),
                     name = "Estado") +
  theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
        axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
        axis.ticks.x = element_line(colour = "black", linetype = "solid"),
        plot.title = element_text(hjust = 0.5)) 
library(grid)
grid.text(regresion[3], x = 0.24, y = 0.75, just = "left", gp = gpar(col = "firebrick", fontsize = 10))
grid.text(regresion[2], x = 0.32, y = 0.18, just = "center", gp = gpar(col = "red", fontsize = 10))
grid.text(regresion[1], x = 0.66, y = 0.18, just = "right", gp = gpar(col = "coral1", fontsize = 10))

#Confidance intervals------

x <- c(0:max(imotor$time)); l <- length(x)
fit <- survreg(data = imotor[imotor$temp!=150,],
               Surv(time,status)~factor(temp))

coef <- c(fit$coefficients,log(fit$scale))
stde <- sqrt(c(diag(vcov(fit))[1:4]))
cil <-  coef-stde*1.96
ciu <-  coef+stde*1.96
ci <- data.frame("coef" = coef,cil,ciu)
rownames(ci) <- list("mu (intercept)", "theta Z=1", "theta Z=2", "log(Beta)")
thetaCI <- rbind("theta Z=0" = c(0,-stde[4]*1.96,stde[4]*1.96),cbind(-ci[2:3,]))
AFCI <- cbind(exp(thetaCI))
muCI <- unlist(ci[1,])
sigmaCI <- unlist(exp(ci[4,]))
betaCI <- unlist(1/exp(ci[4,]))
alfaCI<-matrix(ncol=3,nrow=3)
for(i in 1:3){
  for(j in 1:3){
alfaCI[i,j] <- exp(-1*(muCI[j]-thetaCI[i,j])/sigmaCI[j])
  }
}

#GRAFICO CURVAS DE CONFIANZA 95%
windows(width = 7.5, height = 5)
par(las = 1, oma = c(0,0.5,0,0), mar = c(4.5,4,2,2))
y <- matrix(ncol=6,nrow=l)#para almacenar los IC

for(i in 1:3){
  for(j in 1:3){
    if(j == 1){
  if(i == 1){
    k=0
plot(exp(-alfaCI[i,j]*(x*AFCI[i,j])**(betaCI[j])), type =c("l"),
     col = "black", xlim = c(0,8064), ylim = c(0,1),xlab = "tiempo", ylab = "S(t)")
legend("topright", paste(levels(factor(imotor$temp))[2:4],"ºC"),bg = "white",
       col = 1:4, lwd = 3, inset = 0.02, seg.len = 3)
  }else{
    lines(exp(-alfaCI[i,j]*(x*AFCI[i,j])**(betaCI[j])),col = i)
  }
    }else{
      y[,(i-1)*2+j-1] <- exp(-alfaCI[i,j]*(x*AFCI[i,j])**(betaCI[j]))
    }
    if(j == 3){
      k=k+1
      polygon(c(x, rev(x)), c(y[,(i-1)*2+j-1], rev(y[,(i-1)*2+j-2])),
              col = rgb(red = k*(i-1)*(i-3)/-2, green = k*(i-1)*(i-2)/6,
                        blue = k*(2-i)*(3-i)/2, alpha = 0.4), border = NA)
    }
  }
}


#IC PARA CURVAS DE RIESGO

for(i in 1:3){
  for(j in 1:3){
    if(j == 1){
      if(i == 1){
        k=0
        plot(AFCI[i,j]*alfaCI[i,j]*betaCI[j]*(x*AFCI[i,j])**(betaCI[j]-1)
          , type =c("l"), col = "black", xlim = c(0,8064), ylim = c(0,0.015),
          xlab = "tiempo", ylab = "h(t)")

      }else{
        lines(AFCI[i,j]*alfaCI[i,j]*betaCI[j]*(x*AFCI[i,j])**(betaCI[j]-1),
              col = i)
      }
    }else{
      y[,(i-1)*2+j-1] <- AFCI[i,j]*alfaCI[i,j]*betaCI[j]*(x*AFCI[i,j])**(betaCI[j]-1)
    }
    if(j == 3){
      k=k+1
      polygon(c(x, rev(x)), c(y[,(i-1)*2+j-1], rev(y[,(i-1)*2+j-2])),
              col = rgb(red = k*(i-1)*(i-3)/-2, green = k*(i-1)*(i-2)/6,
                        blue = k*(2-i)*(3-i)/2, alpha = 0.5), border = NA)
      if(i==3){
        
        legend("topright", paste(levels(factor(imotor$temp))[2:4],"ºC"),bg = "white",
               col = 1:4, lwd = 3, inset = 0.02, seg.len = 3)
      }
    }
  }
}
