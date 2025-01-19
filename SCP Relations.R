---
  title: "SCP Relations"
author: "Fioritto Alessandro 152205"
date: "13/12/2024"
output:
  ioslides_presentation: 
  css: ./style.css


install.packages("network")

#OPERAZIONI PRELIMINARI

library(moments)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(igraph)

data <- read_excel("relazioninew.xlsx")

g <- graph_from_data_frame(data, directed = TRUE)

nodi <- data$Node
  
archi <- data$Connection


###############################


ConnesseStrong <- components(g, mode = "strong")
SCC <- which.max(ConnesseStrong$csize)
NodiSCC <- which(ConnesseStrong$membership == SCC)
cat("Nodi nella componente fortemente connessa più grande:\n")
print(V(g)$name[NodiSCC])
SCCgraph<- induced_subgraph(g, NodiSCC)
plot(SCCgraph, vertex.color = "green", vertex.size = 25, edge.arrow.size = 0.5)

rate <- list(
  rate527 = 828,
  rate629 = 151,
  rate644 = 327,
  rate905 = 288,
  rate909 = 390,
  rate913 = 239,
  rate917 = 232,
  rate920 = 476,
  rate1007 = 380,
  rate1799 = 138,
  rate1908 = 85
)

MediaVotoSSC <- (rate$rate527 + rate$rate629 + rate$rate644 + rate$rate905 + rate$rate909 + rate$rate913  + rate$rate917  + rate$rate920  + rate$rate1007 + rate$rate1799 + rate$rate1908) / 11
print(MediaVotoSSC)

#numero archi 110
#media punteggio 321.3
#527 ha più punteggio e ha 10 archi

################################################################
#SOTTOGRAFI RANDOM

set.seed(43)
Random1 <- sample(V(g)$name, 11, replace = FALSE)
RandomGraph1 <- induced_subgraph(g, Random1)
plot(RandomGraph1, vertex.color = "orange", vertex.size = 20, edge.arrow.size = 0.5)

rate <- c(rate, list(
  rate1069 = 165,
  rate297 = 495,
  rate1733 = 1869,
  rate1174 = 153,
  rate67 = 558,
  rate774 = 81,
  rate1499 = 756,
  rate441 = 125,
  rate1753 = 327,
  rate1870 = 180,
  rate1032 = 723
))

MediaVotoRandom1 <- (rate$rate1069 + rate$rate297 + rate$rate1733 + rate$rate1174 + rate$rate67 + rate$rate774  + rate$rate1499  + rate$rate441  + rate$rate1753 + rate$rate1870 + rate$rate1032) / 11
print(MediaVotoRandom1)

ArchiRandom1 <- degree(g, v = Random1, mode = "all")
print(ArchiRandom1)
print(sum(ArchiRandom1)-11)

#numero archi 6
#media punteggio 274.5
#67 ha più punteggio e ha 1 arco 

#########


set.seed(413)
Random2 <- sample(V(g)$name, 11, replace = FALSE)
RandomGraph2 <- induced_subgraph(g, Random2)
plot(RandomGraph2, vertex.color = "orange", vertex.size = 20, edge.arrow.size = 0.5)

rate <- c(rate, list(
  rate457 = 703,
  rate1541 = 382,
  rate420 = 260,
  rate33 = 1075,
  rate1778 = 261,
  rate1388 = 61,
  rate1770 = 148,
  rate59 = 272,
  rate573 = 235,
  rate465 = 157,
  rate281 = 198
))

MediaVotoRandom2 <- (rate$rate457 + rate$rate1541 + rate$rate420 + rate$rate33 + rate$rate1778 + rate$rate1388  + rate$rate1770  + rate$rate59  + rate$rate573 + rate$rate465 + rate$rate281) / 11
print(MediaVotoRandom2)

ArchiRandom2 <- degree(g, v = Random2, mode = "all")
print(ArchiRandom2)
print(sum(ArchiRandom2)-11)

#numero archi 4
#media punteggio 341.0
#33 ha più punteggio e ha 0 archi

#########

set.seed(463)
Random3 <- sample(V(g)$name, 11, replace = FALSE)
RandomGraph3 <- induced_subgraph(g, Random3)
plot(RandomGraph3, vertex.color = "orange", vertex.size = 20, edge.arrow.size = 0.5)

rate <- c(rate, list(
  rate741 = 175,
  rate850 = 104,
  rate499 = 204,
  rate137 = 160,
  rate374 = 90,
  rate291 = 303,
  rate1458 = 32,
  rate1543 = 344,
  rate921 = 260,
  rate1303 = 106,
  rate894 = 153
))

MediaVotoRandom3 <- (rate$rate741 + rate$rate850 + rate$rate499 + rate$rate137 + rate$rate374 + rate$rate291  + rate$rate1458  + rate$rate1543  + rate$rate921 + rate$rate1303 + rate$rate894) / 11
print(MediaVotoRandom3)

ArchiRandom3 <- degree(g, v = Random3, mode = "all")
print(ArchiRandom3)
print(sum(ArchiRandom3)-11)

#numero archi 2
#media punteggio 175.5
#1543 ha più punteggio e ha 0 archi

################################################################
#Grado IN e OUT

NodoMaxIN <- names(which.max(table(data$Connection)))
GradoMaxIN <- max(table(data$Connection))
cat("Nodo con più connessioni in entrata:", NodoMaxIN, "con", GradoMaxIN, "connessioni\n") ##PRINT NON FUNZIONA

##Max in-degree non indicativo, 909 ha soltanto una connessione entrante al di fuori della SCC.

##max in-degree al di fuori dell SCC

#682    500  914   76  173  529 
#11     8    6    5    5    5 

#682 e 500 sono anche parte del diametro,


GradoIN <- degree(g, mode = "in")
GradoOUT <- degree(g, mode = "out")
NodoMaxUscente <- names(GradoOUT[which.max(GradoOUT)])
cat("Nodo con più connessioni in uscita:", NodoMaxUscente, "con", max(GradoOUT), "connessioni\n")


####################################################################
#MEDIA E MEDIANA GRADI (OUT)

MediaGradi <- mean(GradoOUT)
MedianaGradi <- median(GradoOUT)
cat("Media dei gradi:", MediaGradi, "\n")            ###### 1.885
cat("Mediana dei gradi:", MedianaGradi, "\n")        ######## OVVIAMENTE IL NUMERO DI GRADO PIU' FREQUENTE E' 0

##Calcolare la media/mediana dei gradi IN non è rilevante visto che vengono contati tutti almeno ad 1

####################################################################
#DISTANZA MEDIA NODI DIAMETRO RETE E DISTRIBUZIONE DISTANZE

# Distanza media
Dist <- distances(g)
Dist [is.infinite(Dist)] <- NA            #ci sono nodi non connessi, quindi elimino
DistMedia <- mean(Dist, na.rm = TRUE)
print(DistMedia)

#LA DISTANZA MEDIA E' 2.31, bassa indicherebbe che è molto connesso ma abbiamo 
#eliminato la maggior parte dei perchè nodi non hanno archi, 1575 nodi (78.79%)

#Tabella delle distanze tra i nodi

Dist <- distances(g, mode = "out")
Dist <- Dist[is.finite(Dist)] #ignora distanze infinite nodi sconnessi
TableDist <- table(Dist)
TableDist <- as.data.frame(TableDist)
colnames(TableDist) <- c("Distanza", "Frequenza")
print(TableDist)

######################################################################
#DIAMETRO RETE


Diametro <- diameter(g, directed = TRUE, unconnected = TRUE)
NodiDiametro <- get_diameter(g, directed = TRUE, weights = NA)
print(Diametro)
print(NodiDiametro)

#IL DIAMETRO E' 5, QUESTO ORDINE 52 8 500 231 682 182

rate <- c(rate, list(
  rate52 = 607,
  rate8 = 1182,
  rate500 = 1288,
  rate231 = 2503,
  rate682 = 3845,
  rate182 = 345
))

MediaVotoDiametro <- (rate$rate52 + rate$rate682 + rate$rate8 + rate$rate500 + rate$rate231 + rate$rate182) / 6
ArchiDiametro <- degree(g, v = NodiDiametro, mode = "all")
print(MediaVotoDiametro)
print(ArchiDiametro)
print(sum(ArchiDiametro)-6) ##per motivi quali sopra GRADOIN

####################################################################
# DISTRIBUZIONE GRADI OUT IN GRAFICO

distribuzione_out <- table(GradoOUT)
tabella_out <- as.data.frame(distribuzione_out)
colnames(tabella_out) <- c("Out-degree", "Frequenza")
print(tabella_out)

barplot(
  distribuzione_out,
  main = "Distribuzione del grado di uscita",
  xlab = "Grado di uscita (Out-degree)",
  ylab = "Frequenza",
  col = "#ffa600",
  border = "black"
)

######################################################################
#INDICE ASSIMMETRIA SKEWNESS


Skewness <- skewness(GradoOUT)                                ###### 44.640 skewnessIN alta, tanti nodi grado 1 
cat("Indice di asimmetria (skewness):", Skewness, "\n")       ###### 40.939 skewnessOUT alta, tanti nodi grado 0 (e 1) 


######################################################################
#GRAFICO LOG LOG PER DISTRIBUZIONE GRADI


degree_data <- as.data.frame(table(GradoOUT)) #dati in tabella
colnames(degree_data) <- c("Degree", "Frequency")

degree_data$Degree <- as.numeric(as.character(degree_data$Degree)) #colonne numeriche (dovrebbe funzionare anche senza ma no)
degree_data$Frequency <- as.numeric(degree_data$Frequency)

degree_data <- degree_data[degree_data$Degree > 0 & degree_data$Frequency > 0, ] # rimuove frequenze zero o gradi zero

fit <- lm(log10(Frequency) ~ log10(Degree), data = degree_data)

ggplot(degree_data, aes(x = Degree, y = Frequency)) +
  geom_point(color = "blue", size = 3) +  
  geom_abline(
    intercept = coef(fit)[1], 
    slope = coef(fit)[2], 
    color = "red", 
    size = 1, 
    linetype = "dashed"
  ) +  
  scale_x_log10() +  
  scale_y_log10() +  
  labs(
    title = "Distribuzione log-log dell'OUT-DEGREE",
    x = "Log10(Grado)",
    y = "Log10(Frequenza)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12)
  )


#######################################################################
#ASSORTATIVITA

archi2 <- as_data_frame(g, what = "edges")

# Aggiungi i gradi ai nodi corrispondenti agli archi
archi2$out_from <- GradoOUT[archi2$from]
archi2$out_to <- GradoOUT[archi2$to]
archi2$in_from <- GradoIN[archi2$from]
archi2$in_to <- GradoIN[archi2$to]

# Calcola i 4 tipi di assortatività
assortativita_out_out <- cor(archi2$out_from, archi2$out_to, use = "complete.obs")
assortativita_in_in <- cor(archi2$in_from, archi2$in_to, use = "complete.obs")
assortativita_out_in <- cor(archi2$out_from, archi2$in_to, use = "complete.obs")
assortativita_in_out <- cor(archi2$in_from, archi2$out_to, use = "complete.obs")

# Stampa i risultati
cat("Assortatività out-out:", assortativita_out_out, "\n") #Assortatività out-out: 0.09152157 
cat("Assortatività in-in:", assortativita_in_in, "\n") #Assortatività in-in: -0.4523078 
cat("Assortatività out-in:", assortativita_out_in, "\n") #Assortatività out-in: -0.4635584 
cat("Assortatività in-out:", assortativita_in_out, "\n") #Assortatività in-out: 0.5257277

#####################################################
#APPUNTI

#INDEGREE FALSATO, AUMENTATO DI 1 CONTA ANCHE CONNESSIONE A SE STESSI

#DISTRIBUZIONE GRADI STRANA, SEMBRA NORMALE MA TRONCATA A SINISTRA (?)


#####################################################





