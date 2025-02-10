---
  title: "Spotify relations"
author: "Fioritto Alessandro 152205"
date: "20/01/2025"
output:
  ioslides_presentation: 
  css: ./style.css

setwd("C:/Users/Aless/OneDrive/Desktop/Data Science LoL R/Esame-Advanced-Data-Science")
install.packages("moments")

###############OPERAZIONI PRELIMINARI

library(ggplot2)
library(dplyr)
library(tidyr)
library(moments)
library(tidyverse)
library(igraph)
library(poweRlaw)
library(ggraph)
library(scales)
library(ggthemes)
library(reshape2)

data <- read.csv("spotify_songs_2k.csv")
data <- data[!duplicated(data$song), ]


##########################################################################################

#Selezione attributi su nodi (es nodi$song)
nodi <- data %>%
  select(song, artist, duration_ms, year, popularity, danceability, energy, loudness, valence, tempo, genre)


# Calcolo della similarità (distanza euclidea tra caratteristiche selezionate, 7)
simil <- data %>%
  select(danceability, acousticness, instrumentalness, energy, loudness, valence, tempo) %>%
  mutate(across(c(danceability, acousticness, instrumentalness, energy, loudness, valence, tempo), scale)) #

distanzamatrix <- as.matrix(dist(simil))

# Crea archi (threshold regola i collegamenti)
threshold <- 1.1  # Regola il valore per aumentare o diminuire i collegamenti

# Creazione degli archi (relazioni tra canzoni simili)
archi <- which(distanzamatrix < threshold & row(distanzamatrix) != col(distanzamatrix), arr.ind = TRUE)
archi_df <- data.frame(
  Source = nodi$song[archi[, 1]],
  Target = nodi$song[archi[, 2]]
)

# Creazione del grafo
grafo <- graph_from_data_frame(d = archi_df, vertices = nodi, directed = FALSE)
layout <- layout_with_fr(grafo)

##########################################################################################
#GRADI 

grado <- degree(grafo)
gradotab <- table(grado)
print(gradotab) 

gradodf <- as.data.frame(gradotab)
colnames(gradodf) <- c("Degree", "Frequency")

# Conversione dei valori in formato numerico per il plot 
gradodf$Degree <- as.numeric(as.character(gradodf$Degree))
gradodf$Frequency <- as.numeric(gradodf$Frequency)

gradodf <- gradodf[gradodf$Degree > 0 & gradodf$Frequency > 0, ] #per evitare errori in regressione

log_degree <- log10(gradodf$Degree)
log_frequency <- log10(gradodf$Frequency)


model <- lm(log_frequency ~ log_degree)# Retta regressione loglog

# Coefficiente stimato (esponente nella legge di potenza)
summary(model)

#############


ggplot(model, aes(x = log_degree, y = log_frequency)) +
  geom_point(color = "#1DB954", shape = 16, size=2) +  
  geom_smooth(method = "lm", color = "darkgrey", linetype = "dashed", se = FALSE) +  
  labs(
    x = "Log10(Grado)",
    y = "Log10(Frequenza)",
    title = "Distribuzione dei Gradi in scala logaritmica"
  ) +
  theme_minimal()

#############
model <- lm(gradodf$Frequency ~ gradodf$Degree)


ggplot(model, aes(x = gradodf$Degree, y = gradodf$Frequency)) +
  geom_point(color = "#1DB954", shape = 16, size=2) +  
  labs(
    x = "Log10(Grado)",
    y = "Log10(Frequenza)",
    title = "Distribuzione dei Gradi in scala logaritmica"
  ) +
  theme_minimal() 


summary(model)
#############
#Max grado, componente connessa massima, diametro

nodomax <- names(grado[which.max(grado)])
cat("Nodo con più connessioni:", nodomax, "con", max(grado), "connessioni\n")

# Distanza media
dist <- distances(grafo)
dist [is.infinite(dist)] <- NA            #ci sono nodi non connessi, quindi elimino
distmedia <- mean(dist, na.rm = TRUE)
print(distmedia)

#LA DISTANZA MEDIA E' 4.44, abbastanza bassa 


#Tabella delle distanze tra i nodi e grafico 

tabelladist <- as.data.frame(table(dist))
colnames(tabelladist) <- c("Distanza", "Frequenza")
tabelladist$Distanza <- as.numeric(as.character(tabelladist$Distanza))
tabelladist <- tabelladist[tabelladist$Distanza > 0, ]  #elimina nodi distanza 0 (se stessi)

print(tabelladist)


ggplot(tabelladist, aes(x = Distanza, y = Frequenza)) +
  geom_point(color = "#1DB954", size = 3) +  
  geom_line(color = "#208A56", size = 1) + 
  labs(
    x = "Distanza tra nodi",
    y = "Frequenza",
    title = "Relazione tra distanza e frequenza delle distanze"
  ) +
  scale_x_continuous(breaks = seq(0, max(tabelladist$Distanza), by = 2)) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +  
  theme_classic(base_size = 14)  

diametro <- diameter(grafo, directed = FALSE, unconnected = TRUE)
nodidiametro <- get_diameter(grafo, directed = FALSE, weights = NA)
print(diametro)
cat("Prima canzone:", names(nodidiametro[1])) 
cat("Ultima canzone:", names(nodidiametro[22]))  
###entrambe canzoni pop e/o hiphop generi più frequenti

grafodiam <- induced_subgraph(grafo, nodidiametro)    

plot(
  grafodiam,
  layout = layout_in_circle,
  vertex.label = V(grafodiam)$genre,    #cambia per label diverso
  vertex.label.color = "#003366",
  vertex.color = "#1DB954",                  
  vertex.size = 20,                            
  edge.color = "#003d00",                         
  edge.width = 1,                              
)

#############
#relazione grado popolarità


graficoRGP <- data.frame(grado = degree(grafo), popolarità = V(grafo)$popularity)

ggplot(graficoRGP, aes(x = nodi$popularity, y = degree(grafo), color = nodi$year)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_gradientn(name = "Anno", colors = c("#003d00", "#009000", "#00c33c", "#1DD954")) +
  labs(title = "Relazione Grado-Popolarità", x = "Grado del Nodo", y = "Popolarità") +
  theme_minimal()

##Non sembrano esserci correlazioni significative tra grado (similarità tra canzoni) e popolarità delle canzoni:
##se ci fosse correlazione canzoni con più collegamenti = più simili tra loro in top 2000 = stile più popolare = più popolare

################
#rilevamento comunità e grafo


archi_df$weight <- 1 / (1 + distanzamatrix[archi]) # senza pesi non funziona louvain, peso in base a similarità

grafocom <- graph_from_data_frame(archi_df, directed = FALSE)
E(grafocom)$weight <- archi_df$weight  # Assegna pesi agli archi, altrimenti louvain non funziona

grafocom <- delete_vertices(grafocom, V(grafocom)[degree(grafocom) < 13]) # solo nodi con tante connessioni per migliorare visual

set.seed(112)
comunità <- cluster_louvain(grafocom) 
V(grafocom)$community <- membership(comunità)  # Assegna comunità ai nodi

#cross_community per trovare archi tra comunità
E(grafocom)$cross_community <- V(grafocom)[ends(grafocom, E(grafocom))[, 1]]$community != V(grafocom)[ends(grafocom, E(grafocom))[, 2]]$community

#V(grafocom)$node_size <- degree(grafocom) / 3 non cambia dimensione

trasparente <- alpha("red", 0) ##per eliminare i collegamenti tra nodi della stessa comunità

print(sum(!E(grafocom)$cross_community))## N archi tra nodi stessa comunità
print(sum(E(grafocom)$cross_community))## N archi tra nodi comunità diverse
print(table(V(grafocom)$community)) ##nodi comunità

###ci sono circa il doppio dei nodi intracomunità che quelli tra le comunità


# Plotta il grafo con colori sugli archi
ggraph(grafocom, layout = "fr") + 
  geom_edge_link(aes(color = factor(E(grafocom)$cross_community)), show.legend = FALSE) +  # Mappa colori tra comunità
  geom_node_point(aes(color = factor(V(grafocom)$community)), size = 3) +  
  scale_edge_color_manual(values = c(trasparente, "#1DB954")) +  
  scale_color_viridis_d(option = "plasma", name="Comunità") +  
  theme_void() +  
  ggtitle("Grafo delle comunità (in verde archi tra comunità diverse)")



## Trovando 7/9 comunità che sono ben connesse anche tra loro i generi assegnati 
## sono più o meno indicativi in quanto ci sono generi pop in qualsiasi delle comunità (genere pop è abbastanza generico)
## questoi cambia molto a seconda dei valori assegnati mentre il genere è più generico come classificazione
## spotify quindi classifica più in base a valori assegnati e ALTRE COSE AMICOCHAT

#################################################################################################
#INFERENZA SU ASSEGNAZIONE COMUNITA' 

##### NON ESEGUIRE PIU' DI UNA VOLTA, APPAIONO COLONNE EXTRA

commdata <- data.frame(
  song = V(grafocom)$name,         
  community = V(grafocom)$community  
) 
data <- merge(data, commdata, by.x = "song", by.y = "song", all.x = TRUE) #merge comunità e dataset originale

data <- data %>%
  mutate(community = replace_na(community, 0))

#######

data2 <- data

data2 <- data2 %>%
  separate_rows(genre, sep = ",") %>% 
  mutate(genre = trimws(genre))

data2 <- data2 %>%
  filter(genre != "set()", community != 0)

heatmap_prova <- data2 %>%
  group_by(community, genre) %>%
  summarise(count = n(), .groups = 'drop') %>%
  spread(genre, count, fill = 0) 

heatmap_long <- heatmap_prova %>%
  gather(key = "genre", value = "count", -community) %>%
  mutate(count_log = log10(count + 1))  


ggplot(heatmap_long, aes(x = as.factor(community), y = genre, fill = count_log)) + #SCALA LOGARITMICA PER MOSTRARE MEGLIO
  geom_tile() +
  scale_fill_viridis(option = "plasma", direction = -1, trans = "log10") +  
  labs(title = "Distribuzione dei generi nelle comunità",
       x = "Comunità",
       y = "Genere",
       fill = "Frequenza del genere" ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################################################################################
# ASSORTATIVITA' e CENTRALITA'

assortativita <- assortativity_degree(grafo, normalized = FALSE)
print(assortativita)

betwennessC <- betweenness(grafo, normalized = TRUE)  ##Senza normalized ho valori generici
betweennessDF <- data.frame(
  name = V(grafo)$name,  
  betweenness = betwennessC
)

betweennessDF <- betweennessDF %>%
  arrange(desc(betweenness)) %>%
  head(10)
print(head(betweennessDF))   ##Centralità betweenness non rilevante, nodo più centrale ha 0.02 (tanti percorsi tra canzoni)


#################################################################################################
##################################################################################################
#PROVE

print(transitivity(grafo, type = "global")) 
#TRANSITIVITA' GLOBALE GRAFO 0.4, DATO CI DICE CI SONO ALCUNI TRIANGOLI MA NON MOLTI
#creazione di comunità come sopra, forse non vale la pena citare

skewness <- (skewness(grado))
print(skewness)
#SKEWNESS GRADI 1.4, CODA A DESTRA QUINDI DISRIBUZIONE POWER LAW (verifica con altro)


# Visualizzazione del grafo (NON ESEGUIRE)
plot(
  grafo,
  layout = layout,
  vertex.size = 5,
  vertex.label = NA,             # Rimuove le etichette per evitare confusione
  edge.arrow.size = 0.5,         # Dimensione delle frecce
  main = "Grafo delle Canzoni Spotify"
)

#################################################################################################
#################################################################################################
#appunti


#COMMENTA CODICE, RIMUOVI OOPSIES
#################################################################################################
#################################################################################################



