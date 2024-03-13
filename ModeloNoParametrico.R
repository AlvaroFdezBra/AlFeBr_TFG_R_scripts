packages <- list("survival", "kableExtra", "dplyr", "stats", "ggplot2",
                 "survminer", "patchwork", "FHtest", "DataExplorer",
                 "kableExtra")

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
}

load_libraries(packages)


######Análisis de supervivencia#####
####KM análisis-------------
## KM tabla

KMCurv <- function(data, save = FALSE){
  
fit <- survfit(with(imotor,Surv(time,status)~1, stype=1, ctype=2,
                    conf.int = 0.95, conf.type = "plain"))

table <- tidy(fit, censored = TRUE)
colnames(table) <- list("Tiempo", "Nº en riesgo", "Eventos", "Censuras", "Estimado",
                        "Error estándar", "IC superior", "IC inferior")

kk <- kbl(digits = 4, table[,1:8], booktabs = TRUE) %>%
  kable_styling(row_label_position = "c", font_size = 17)

return(kk)

}
KMCurv(data)

##KM (gráfico)

KMCurv <- function(data){
  fit <- survfit(with(data,Surv(time,status) ~ 1, stype=1, ctype=2,
                      conf.int = 0.95, conf.type = "plain"))
  t <- fit$time
  KM_CI_low <- fit$lower
  KM_CI_up <- fit$upper
  KM_curv <- fit$surv
  df <- data.frame(t,KM_curv,KM_CI_up,KM_CI_low)
  
  return(df)
  
}
plot_KMCurv(data)

####KM estratificado------------

## KM estratificado (tabla)

fit <- survfit(with(imotor,Surv(time,status) ~ 1, stype=1, ctype=2,
                    conf.int = 0.95, conf.type = "plain"))

table <- tidy(fit, censored = TRUE)
colnames(table) <- list("Tiempo", "Nº en riesgo", "Eventos", "Censuras", "Estimado",
                      "Error estándar", "IC superior", "IC inferior","Temperatura")
kk <- kbl(digits = 4, table[,1:8], booktabs = TRUE) %>% pack_rows(
  index = c("Temperatura 150 ºC" = 1, "Temperatura 170 ºC" = 8, 
            "Temperatura 190 ºC" = 4, "Temperatura 220 ºC" = 3))  %>%
  kable_styling(row_label_position = "c", font_size = 17)
  save_kable(x = kk, file = "table_SurvivalCurveStrata.png")
  
k <- kbl(digits = 4, fit$lower)

## KM estratificado (grafico)
g5
ggsurvplot(fit, data = imotor,conf.int = T, legend.title = "Temperatura",
           legend.labs = c("150ºC","170ºC","190ºC","220ºC")) +
  labs(x = "Tiempo", y = "R(t)")


## KM cum. hazard
windows(width = 7.5, height = 5)
par(las = 1, oma = c(0,0.5,0,0), mar = c(4.5,4,2,2))
km_temp <- survfit(data = imotor, Surv(time,status)~temp)
plot(km_temp, fun = "cumhaz", ylim = c(0,1.2), col = 1:4, lwd = 2,
     xlab = "Tiempo", ylab = "H(t)")
legend("topleft", paste(levels(factor(imotor$temp)),"ºC"),bg = "white",
       col = 1:4, lwd = 3, inset = 0.02, seg.len = 3)

##### Hypothesis tests --------------

install.packages('BiocManager')
BiocManager::install('Icens')
install.packages(FHtest)
library(FHtest)

# FH test log-rango (pesos iguales)
test <- FHtestrcp( rho = 0, lambda = 0, data = imotor,
                   Surv(time,status) ~ strata((imotor$temp)))

test$n <- cbind(c(150,170,190,220),test$n,test$diff)
row.names(test$n) <- NULL
pvalue_logrango <- round(x = test$pvalue, 5)
statistic_logrango <- round(x = test$statistic, 2)

g <- kable(x = test$n, digits=3, row.names = NA,
           col.names = list("Temperatura [ºC]", "N", "obs - esp"),
           align = "c") 


g <- kable_styling(g ,row_label_position = "l", font_size = 17,
                   full_width = FALSE) %>%
  footnote(general = c(
    paste("Parametros: Rho = 0, Gamma = 0", "\n"),
    paste("P-valor =", pvalue_logrango, "\n"),
    paste(names(statistic_logrango), "=", statistic_logrango), "\n"),
    general_title = "",   footnote_as_chunk = T)

save_kable(g, file = "table_LogRango.png")

#####log rank test plot (+ preciso que FHtestrcp)
fit <- survdiff(Surv(time, status) ~ temp, imotor, rho = 0)
pvalue_logrango <- round(x = fit$pvalue, 5)
statistic_logrango <- round(fit$chisq, 5)
table <- tidy(fit)

g <- kable(x = table, digits=3, row.names = NA,
           col.names = list("Temperatura [ºC]", "N", "obs", "esp"),
           align = "c") 


g <- kable_styling(g ,row_label_position = "l", font_size = 17,
                   full_width = FALSE) %>%
  footnote(general = c(
    paste("Parametros: Rho = 0, Gamma = 0", "\n"),
    paste("P-valor =", pvalue_logrango, "\n"),
    paste("Chi cuadrado", "=", statistic_logrango), "\n"),
    general_title = "",   footnote_as_chunk = T)

fit <- survfit(with(imotor,Surv(time,status) ~ factor(temp), stype=1, ctype=1,
                    conf.int = 0.95, conf.type = "plain"))
ggsurvplot(fit, data = imotor,conf.int = T, legend.title = "Temperatura",
           legend.labs = c("150ºC", "170ºc", "190ºC", "220ºC"),
           legend = "right")+
  labs(x = "Tiempo", y = "prob. supervivencia")
  
# FH test (rho = 1, lambda = 0) survdiff approach

fit <- survdiff(data = imotor, Surv(time, status) ~ factor(temp), rho = 1)

pvalue_FHtest <- round(x = fit$pvalue, 5)
statistic_FHtest <- round(fit$chisq, 5)
table <- tidy(fit)

g <- kable(x = table, digits=3, row.names = NA,
           col.names = list("Temperatura [ºC]", "N", "obs", "esp"),
           align = "c") 


g <- kable_styling(g ,row_label_position = "l", font_size = 17,
                   full_width = FALSE) %>%
  footnote(general = c(
    paste("Parametros: Rho = 1, Gamma = 0", "\n"),
    paste("P-valor =", pvalue_FHtest, "\n"),
    paste("Chi cuadrado", "=", statistic_FHtest), "\n"),
    general_title = "",   footnote_as_chunk = T)

# FH test (+peso al final de la prueba)

FHtest <- function(data,select = "tabla"){
  
test <- FHtestrcp( rho = 1, lambda = 0, data = imotor,
                   Surv(time,status) ~ (temp))

test$n <- cbind(c(150,170,190,220),test$n,test$diff)
row.names(test$n) <- NULL
pvalue_fhtest <- round(x = test$pvalue, 5)
statistic_fhtest <- round(x = test$statistic, 2)

g <- kable(x = test$n, digits=3, row.names = NA,
           col.names = list("Temperatura [ºC]", "N", "obs - esp"),
           align = "c") 

g <- kable_styling(g ,row_label_position = "l", font_size = 17,
                   full_width = FALSE) %>%
  footnote(general = c(
    paste("Parametros: Rho = 1, Gamma = 0", "\n"),
    paste("P-valor =", pvalue_fhtest, "\n"),
    paste(names(statistic_fhtest), "=", statistic_fhtest), "\n"),
    general_title = "",   footnote_as_chunk = T)

if(select == "pval"){
  
return(pvalue_fhtest))

} else{
  
  return(g)
  
      }
}

#select = "pval" devuelve el p valor, else devuelve tabla

FHtest(data) 
pvalue_FHtest <- FHtest(data, select = "pval")[2]


####### Peto-Peto, Tarone-Ware, Gehan tests-------------------
PetoTaroneGehan <- function(data){

library(coin)
test_petopeto <- logrank_test(data = imotor ,
                              Surv(time,status) ~ factor(temp),
                              type = "Peto-Peto")

test_taroneware <- logrank_test(data = imotor ,
                                Surv(time,status) ~ factor(temp),
                                type = "Tarone-Ware")

test_gehan <- logrank_test(data = imotor ,
                                Surv(time,status) ~ factor(temp),
                                type = "Gehan-Breslow")

pvalue <- data.frame("Peto - Peto" = pvalue(test_petopeto),
                     "Tarone - Ware" = pvalue(test_taroneware),
                     "Gehan" = pvalue(test_gehan))
return(pvalue)
}
PetoTaroneGehan(data)
####### tabla comparativa entre diferentes métodos-----
v <- rbind(pvalue_logrango,pvalue_FHtest,pvalue_petopeto,
           pvalue_taronware,pvalue_gehan)

v <- rbind(pvalue_logrango,pvalue_FHtest,pvalue_petopeto,
           pvalue_taronware,pvalue_gehan)

rownames(v) <- list("Log-Rango (rho = gamma = 0)",
                    "FH (rho = 1, lambda = 0)", "Peto-Peto","Tarone Ware",
                    "Gehan")

colnames(v) <- list("pvalue")

g <- kable(x = v, digits=6, row.names = NA,
           col.names = list("P-valor"),
           align = "c") 

g <- kable_styling(g ,row_label_position = "l", font_size = 17,
                   full_width = FALSE)

# OTROS --------------------

  #FH test eq Mann–Whitney–Wilcoxon (+ peso al principio de la prueba)
  
  #NO ES UNA PRUEBA VALIDA SI LOS DATOS PRESENTAN CENSURAS

MannWhitneyWilcoxon <- function(data){
test <- FHtestrcp( rho = 0, lambda = 1, data = imotor,
                   Surv(time = time, event = status) ~ imotor$temp,
                   alternative = "different")

test$n <- cbind(c(150,170,190,220),test$n,test$diff)
row.names(test$n) <- NULL
pvalue_mannwhitney <- round(x = test$pvalue, 5)
statistic_mannwhitney <- round(x = test$statistic, 2)

g <- kable(x = test$n, digits=3, row.names = NA,
           col.names = list("Temperatura [ºC]", "N", "obs - esp"),
           align = "c") 


g <- kable_styling(g ,row_label_position = "l", font_size = 17,
                   full_width = FALSE) %>%
  footnote(general = c(
    paste("Parametros: Rho = 0, Gamma = 1", "\n"),
    paste("P-valor =", pvalue_mannwhitney, "\n"),
    paste(names(statistic_mannwhitney), "=", statistic_mannwhitney), "\n"),
    general_title = "",   footnote_as_chunk = T)

return(g)
}
MannWhitneyWilcoxon(data)
