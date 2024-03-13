packages <- list("survival", "kableExtra", "dplyr", "stats", "ggplot2",
                 "survminer", "patchwork", "FHtest", "DataExplorer",
                 "kableExtra", "BiocManager")

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

#### Google Trends gráfico------------

df <- cbind(data$Semana,data$`R: (España)`,data$`R: (España)`,
            data$`python: (España)`,data$`STATA: (España)`,data$`SAS: (España)`,
            data$`MATLAB: (España)`)

  #datos disponibles en google trends; buscar por categorias:
  #tiempo > 5años ; todo el mundo ; temática : estadísticas

df <- multiTimeline_2_
names <- gsub(pattern = "[^[:alnum:] ]|Todo el mundo",
              replacement = "", x =  names(df))
names(df) <- NA

i=2
while(i <= 6){
  
  if(i == 2){
    y <- (df[,i])  
    i=i+1
    next
  }
  
  y <- rbind(y, df[,i])
  i=i+1
}

df1 <- data.frame(df[,1], rep(names[2:6], each=260), y)
names(df1) <- list("Fecha","Lenguaje","Interés relativo")

ggplot(df1, aes(x = df1[,1], y = df1[,3], color = Lenguaje)) +
  geom_line()+
  labs(x = "Fecha", y = "Interés relativo", color = "Lenguajes") +
  scale_color_manual(labels = c("Python","R","SAS","SPSS","Stata"),
                     values = c("blue","orange","red","purple","black"))+
  theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
        axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
        axis.ticks.x = element_line(colour = "black", linetype = "solid"),
        plot.title = element_text(hjust = 0.5)) 

##### CARGAR Base de datos---------------
data <- with(imotor, data.frame("time" = time,"temp" = factor(temp),
                                "status" = factor(status)))

##### Analisis univariado ------------------------------------------------------

  # Tiempo: gráfica

data %>% ggplot() +
  geom_histogram(data = imotor, aes(x = time), color = "black",
                 bins = 18, alpha = 0.5, position = "stack", show.legend = T,
                 binwidth = 450, boundary = 0, closed = "left") +
  labs(title = "Histograma según temperatura", x = "Tiempo", y = "Frecuencia")+
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2),
                     labels = seq(from = 0, to = 10, by = 2),
                     position = "left")+
  scale_x_continuous(breaks = seq(from = 0, to = 8100, by = 900),
                     labels = seq(from = 0, to = 8100, by = 900),
                     position = "bottom")+
  theme_minimal()+
  theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype='solid'),
        axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype='solid'),
        axis.ticks = element_line(colour = "black", linetype = "solid"),
        plot.title = element_text(hjust = 0.5))

  # Tiempo: tabla 

a <- summary(imotor$time)[-4]
sd <- sd(imotor$time)
IQR <- IQR(imotor$time)
table <- c(a,sd,IQR)
names(table) <- list("Min.", "1er cuartil", "Mediana",
                     "3er cuartil", "Max.", "Desv est.", "IQR") 


table %>%
  kbl(digits = 3) %>%
  kable_styling()
  save_kable(file = "table_Univariado_temperatura.png")
  # Estado

data %>% ggplot(aes(y = factor(imotor$status))) +
  geom_bar() +
  scale_x_continuous(breaks = seq(from = 0, to = 24, by = 4),
                     labels = seq(from = 0, to = 24, by = 4))+
  scale_y_discrete(labels=c("0" = "Censura", "1" = "Avería"))+
  scale_fill_manual(name = "Estado", labels = c("Censurado","Averiado"))+
  labs(
    title = "Estado",
    x = "Frecuencia",
    y = "Estado"
  ) +
  theme_minimal()+
  theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype='solid'),
        axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype='solid'),
        axis.ticks = element_line(colour = "black", linetype = "solid"),
        plot.title = element_text(hjust = 0.5))

  #Temperatura

data %>% ggplot(aes(x=time, y = ""))+
  geom_boxplot()+
  labs(title = "Tiempo", x = "Tiempo", y = "") +
  theme_minimal()+
  theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype='solid'),
        axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype='solid'),
        axis.ticks.x = element_line(colour = "black", linetype = "solid"),
        plot.title = element_text(hjust = 0.5))



##### Bivariate Analysis--------------------

##Temperatura vs tiempo (histograma)
g1 <- ggplot(data) +
  geom_histogram(aes(x = time, fill = as.factor(temp)), color = "black",
                 bins = 18, alpha = 0.5, position = "stack", show.legend = T,
                 binwidth = 450, boundary = 0, closed = "left") +
  scale_fill_manual(values = c("150" = "darkorange", "170" = "coral1",
                               "190" = "firebrick1", "220" = "red"),
                               name = "Temperatura") +
  labs( x = "Tiempo", y = "Frecuencia")+
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2),
                     labels = seq(from = 0, to = 10, by = 2),
                     position = "left")+
  scale_x_continuous(breaks = seq(from = 0, to = 8100, by = 900),
                     labels = seq(from = 0, to = 8100, by = 900),
                     position = "bottom")+
  theme_minimal()+
  theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55,
                                   linetype='solid'),
        axis.line.y = element_line(colour = 'black', linewidth = 0.55,
                                   linetype='solid'),
        axis.ticks = element_line(colour = "black", linetype = "solid"),
        plot.title = element_text(hjust = 0.5))

## Temperatura vs tiempo (boxplot)
g2 <- ggplot(data, aes(x = time, y = temp)) +
  geom_boxplot()+
  labs( x = "Tiempo", y = "Temperatura") +
  scale_x_continuous(breaks = seq(from = 0, to = 8100, by = 900),
                     labels = seq(from = 0, to = 8100, by = 900),
                     position = "bottom")+
  theme_minimal()+
  theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype='solid'),
        axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype='solid'),
        axis.ticks = element_line(colour = "black", linetype = "solid"),
        plot.title = element_text(hjust = 0.5))

# Combinar g1 & g2

par(mfcol = c(1, 1))
g1 / g2

#Temperatura vs tiempo: estadísticos descriptivos

l <- unique(data$temp)
table <- matrix(nrow = length(l), ncol = 7) 
colnames(table) <- list("Min.", "1er cuartil", "Mediana",
                     "3er cuartil", "Max.", "Desv est.", "IQR") 
rownames(table) <- list("150ºC","170ºC","190ºC","220ºC")
i <- 1
while(i <= length(l)){

a <- summary(data[data$temp==l[i],][,1])[-4]
sd <- sd(data[data$temp==l[i],][,1])
IQR <- IQR(data[data$temp==l[i],][,1])
table[i,] <- cbind(t(a),sd,IQR)
i=i+1

}

table %>%
  kbl() %>%
  kable_styling()

## Estado vs temperatura (barplot)
data %>% ggplot(aes(x = as.factor(temp), fill = as.factor(status))) +
      geom_bar(position = "stack", show.legend = TRUE ) +
      scale_x_discrete()+
      scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2),
                         labels = seq(from = 0, to = 10, by = 2),
                         position = "left")+
      scale_fill_manual(name = "Estado", values = c("grey", "grey50"),
                        labels = c("Censurado","Averiado"))+
       labs(
             title = "Frecuencia de los estados según temperatura", 
             x = "Temperatura",
             y = "Frecuencia"
         ) +
       theme_minimal()+
theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55,
                                 linetype='solid'),
      axis.line.y = element_line(colour = 'black', linewidth = 0.55,
                                 linetype='solid'),
      axis.ticks = element_line(colour = "black", linetype = "solid"),
      plot.title = element_text(hjust = 0.5))

##Estado vs tiempo (boxplot)

g1 <- ggplot(data, aes(x = time, y = status)) +
  geom_boxplot()+
  labs(x = "Tiempo", y = "Estado") +
  scale_y_discrete(labels=c("0" = "Censura", "1" = "Avería"))+
  theme_minimal()+
  theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype='solid'),
        axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype='solid'),
        axis.ticks = element_line(colour = "black", linetype = "solid"),
        plot.title = element_text(hjust = 0.5))

##Estado vs tiempo (histograma)

g2 <- ggplot() +
  geom_histogram(data = imotor, aes(x = time, fill = as.factor(status)), color = "black",
                 bins = 18, alpha = 0.5, position = "stack", show.legend = T,
                 binwidth = 450, boundary = 0, closed = "left") +
  scale_fill_manual(values = c("0" = "white", "1" = "gray50"), name = "Estado",
                    labels = c("Censurado","Averiado")
                    ) +
  labs(x = "Tiempo", y = "Frecuencia")+
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2),
                     labels = seq(from = 0, to = 10, by = 2), position = "left")+
  scale_x_continuous(breaks = seq(from = 0, to = 8100, by = 900),
                     labels = seq(from = 0, to = 8100, by = 900),
                     position = "bottom")+
  theme_minimal()+
  theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55,
                                   linetype='solid'),
        axis.line.y = element_line(colour = 'black', linewidth = 0.55,
                                   linetype='solid'),
        axis.ticks = element_line(colour = "black", linetype = "solid"),
        plot.title = element_text(hjust = 0.5))

#Estado vs tiempo: estadísticos descriptivos

table <- matrix(ncol = 7, nrow = 2)
rownames(table) <- list("Censurados", "Averiados")
colnames(table) <- list("Min.", "1er cuartil", "Mediana",
                        "3er cuartil", "Max.", "Desv est.", "IQR") 

summ_cens <- t(summary(data[data$status==0,][,1])[-4])
sd_cens <- sqrt(var(averiados))
IQR_cens <- IQR(data[data$status==0,][,1])
table[1,] <- cbind(summ_cens,sd_cens,IQR_cens)

summ_cens <- t(summary(data[data$status==1,][,1])[-4])
sd_cens <- sqrt(var(averiados))
IQR_cens <- IQR(data[data$status==1,][,1])
table[2,] <- cbind(summ_cens,sd_cens,IQR_cens)

table %>%
  kbl() %>%
  kable_styling()

# combinar g1 & g2

par(mfcol = c(1, 1))
g1 / g2

##Scatter plot

ggplot(imotor,aes(x = time, y = temp))+ 
  geom_point(aes(colour = factor(status)), 
             alpha = .4, size = 2, 
             position = position_jitter(width=NULL,height=2,seed=10))+
  scale_colour_manual(values = c("red", "blue"),
                      breaks = c("0","1"),
                      labels = c("Censurado","Averiado"))+
  geom_hline(yintercept = 220, linetype = "dashed") +
  geom_hline(yintercept = 190, linetype = "dashed") +
  geom_hline(yintercept = 170, linetype = "dashed") +
  geom_hline(yintercept = 150, linetype = "dashed") +
  theme_minimal()+
  guides(colour = guide_legend(title = "Estado"))+
  labs(title = "Diagrama de dispersión", x = "Tiempo", y = "Temperatura")+
  theme(plot.title = element_text(hjust = 0.5))

  
##Gráfico supervivencia

dim(imotor)[1]

index <- seq(from = 1, to = dim(imotor)[1])
g <- cbind(index,imotor)

ggplot(g) +
  geom_point(data = subset(g, imotor$status == 0,
                           select = c("index", "time", "temp")),
             aes(x = time, y = index, colour = factor(temp))) +
  geom_segment(aes(x = 0, xend = time, y = index, yend = index,
                   colour = factor(temp))) +
  labs(title = "Tiempos de supervivencia para cada muestra", x = "Tiempo", y = "Identificador") +
  theme_minimal() +
  theme(axis.line.x = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
        axis.line.y = element_line(colour = 'black', linewidth = 0.55, linetype = 'solid'),
        axis.ticks.x = element_line(colour = "black", linetype = "solid"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  guides(colour = guide_legend(title = "Temperatura ºC"))

##imotor table Kable

df <- cbind(imotor[1:10,c(2,3)],imotor[11:20,c(2,3)],imotor[21:30,c(2,3)],
            imotor[31:40,c(2,3)])
colnames(df) <- list("Tiempo","Estado","Tiempo","Estado","Tiempo","Estado","Tiempo","Estado")


kable_output <- kable(df, "html") %>% 
  kable_styling(full_width = F) %>%              # Apply colors to each temperature group
  column_spec(1:2, background = "#90EE90") %>%  # Pink for 150ºC
  column_spec(3:4, background = "#FE6") %>%  # Light Blue for 170ºC
  column_spec(5:6, background = "#FFA07A") %>%  # Light Green for 190ºC
  column_spec(7:8, background = "red") %>%  # Light Salmon for 220ºC
  # Grouping rows by temperature (visual grouping, optional)
  add_header_above(c("Temperatura = 150ºC" = 2, "Temperatura = 170ºC" = 2,
                     "Temperatura = 190ºC" = 2, "Temperatura = 220ºC" = 2),
                   font_size = 15); kable_output
