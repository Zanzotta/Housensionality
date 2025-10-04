library(readr)
setwd("C:\\Users\\Andrea\\Documents\\MilanHousePrice -- Kaggle")
training_houses <- read.csv("training_houses.csv", stringsAsFactors = T)
training_houses <- training_houses[,-1]


# VARIABLES DESCRIPTION
# square_meters:                 dimension of the house in square meters
# bathrooms_number:              total number of bathrooms in the house
# lift:                          whether a lift is present or not in the building where the house is located
# rooms_number:                  total number of rooms in the house
# other_features:                list of additional characteristics of the house
# total_floors_in_building:      total number of floors in the building where the house is located
# car_parking:                   information on car parking facilities, if available
# availability:                  whether the house is already available, or, if not, when it will be available
# condominium_fees:              total amount of condominium fees
# year_of_construction:          year of construction of the house or of the building where it is located
# conditions:                    current conditions of the house
# zone:                          area of Milan where the house is located
# floor:                         in which floor of the building the house is located
# heating_centralized:           whether the heating system is centralized or not
# energy_efficiency_class:       energy efficiency class of the house

str(training_houses)


#--------------------------------#
# COMPRENSIONE DELLE VARIABILI ----
#--------------------------------#
library(tidyverse)
library(visdat)

summary(training_houses)

# ci sono valori mancanti?
vis_miss(training_houses)

#-----------------------------#
#--- CONDOMINIUM_FEES ----
#-----------------------------#
# Modificando le variabili senza modificarne il contenuto informativo ci permette di fare delle analisi in modo più semplice
training_houses$condominium_fees <- as.integer(training_houses$condominium_fees)

#-----------------------------#
#--- AVAILABILITY ----
#-----------------------------#
training_houses$availability
# Creo una variabile che mi permetta di dire se la casa è disponibile da domani oppure no. Dopo confronteremo questi gruppi con il prezzo
# della casa per vedere se eventuali tendenze esistano. Se eventuali tendenze per case che saranno libere non da subito esistono, si 
# dovrà ripensare l'efficacia del raggruppamento. 
# Viene creata una terza modalità per le case in cui l'informazione non è presente, queste case possono avere uno status appositamete
# sconsciuto perchè ci sono delle cose che non vanno
training_houses$availability <- as.character(training_houses$availability)
ind <- which((training_houses$availability != "available") & !is.na(training_houses$availability))
training_houses$availability[ind] <- "not available yet"
ind <- which(is.na(training_houses$availability))
training_houses$availability[ind] <- "unknown"
training_houses$availability <- as.factor(training_houses$availability)


training_houses %>% select(availability, selling_price) %>% group_by(availability) %>% summarise(prezzoMedio = mean(selling_price), prezzoMediano = median(selling_price), n = n(), max = max(selling_price), min = min(selling_price))
# Sembra esserci una differenza di prezzo, c'è da dire però che le case disponibili non subito sono in numero molto minore, essendo
# ignoto se sia un campione rappresentativo manteniamo il gruppo degli "unknown"

#-----------------------------#
#--- TOTAL_FLOORS_IN_BUILDING ----
#-----------------------------#
training_houses$total_floors_in_building
training_houses$total_floors_in_building <- as.integer(training_houses$total_floors_in_building)

#-----------------------------#
#--- CAR_PARKING ----
#-----------------------------#
# Stesso ragionamento di availability ma creando un'altra variabile
training_houses$car_parking
training_houses$binary_car_parking <- as.character(training_houses$car_parking)
ind <- which(training_houses$binary_car_parking != "no")
training_houses$binary_car_parking[ind] <- "yes"
training_houses$binary_car_parking <- as.factor(training_houses$binary_car_parking)


#-----------------------------#
#--- FLOOR ----
#-----------------------------#
# Trasform floor in una variabile numerica, sostituendo con un numero il piano corrispondente
training_houses$floor
training_houses$floor <- as.character(training_houses$floor)
# ground_floor = 0
# mezzanine = 0.5
# semi-basement = -0.5
ind <- which(training_houses$floor == "ground floor")
training_houses$floor[ind] <- 0
ind <- which(training_houses$floor == "mezzanine")
training_houses$floor[ind] <- 0.5
ind <- which(training_houses$floor == "semi-basement")
training_houses$floor[ind] <- -0.5
training_houses$floor <- as.numeric(training_houses$floor)

#-----------------------------#
#--- ZONE ----
#-----------------------------#
# Creo una varibile che raggruppi le zone in macroaree della città
# Macroaree:
# Municipio 1 -> Centro Storico
# Circonvalla -> Area interna alla circonvallazione ed esterna al municipio 1
# Esterno
training_houses$zone
zona <- numeric(dim(training_houses)[1])
ind <- which(training_houses$zone %in% c("quadronno - crocetta","carrobbio","lanza","moscova","porta venezia","san carlo","ticinese","turati","brera",
                            "duomo","porta vittoria","scala - manzoni","arco della pace","borgogna - largo augusto","cadorna - castello",
                            "corso genova","guastalla","missori","palestro","porta nuova","san vittore","arena","quadrilatero della moda",
                            "san babila","sant'ambrogio","vincenzo monti"))
zona[ind] <- "Centro_Storico"
ind <- which(training_houses$zone %in% c("navigli - darsena","bocconi","city life","dezza","garibaldi - corso como","monte rosa - lotto",
                            "piave - tricolore","washington","amendola - buonarroti","ascanio sforza","cadore","cenisio",
                            "frua","ghisolfa - mac mahon","isola","melchiorre gioia","pagano","repubblica","zara","centrale",
                            "de angeli","montenero","portello - parco vittoria","sempione","buenos aires","corso san gottardo",
                            "farini","indipendenza","morgagni","paolo sarpi","porta romana - medaglie d'oro","solari","wagner"))
zona[ind] <- "Circonvalla"
ind <- which(zona == 0)
zona[ind] <- "Esterno"
zona <- as.factor(zona)
training_houses$Macro_zone <- zona
# I precedenti NA vengono riempiti come "Esterno" perchè non c'è alcun motivo di omettere la zona dell'edificio quando questa è d pregio

# Riusciamo ad identificare davvero 3 fasce di prezzo?
training_houses %>% select(Macro_zone,selling_price) %>% group_by(Macro_zone) %>% 
  summarise(media = mean(selling_price), mediana = median(selling_price), n = n())

#-----------------------------#
#--- BATHROOMS_NUMBER ----
#-----------------------------#
# Controllo i valori "estremi" prima di trattare i mancanti"
# Dove manca il bagno?
bagni_NA <- training_houses[is.na(training_houses$bathrooms_number),c(1,2,4)]
# sostituiamo i bagni con NA guardando ad appartamenti con numero di stanze e metri quadri simili
default <- training_houses %>% select(square_meters, rooms_number,bathrooms_number) %>% 
  group_by(square_meters,rooms_number) %>% summarise(media = round(mean(as.numeric(bathrooms_number),na.rm=T)))
ind <- which((default$square_meters %in% bagni_NA$square_meters))  
bagni_NA <- inner_join(bagni_NA, default, join_by(square_meters,rooms_number))
bagni_NA <- bagni_NA %>% select(square_meters,rooms_number,media) %>% distinct()
# Sostituisco
ind <- which(is.na(training_houses$bathrooms_number))
midop <- inner_join(training_houses[is.na(training_houses$bathrooms_number),], bagni_NA, join_by(square_meters,rooms_number))
midop <- midop %>% mutate(bathrooms_number = media) 
midop <- midop[,-19]
training_houses <- training_houses[-which(is.na(training_houses$bathrooms_number)), ]
training_houses <- rbind(training_houses,midop)

#-----------------------------#
#--- CONDITIONS ----
#-----------------------------#
# Perchè la condizione della casa presenta NA
training_houses[is.na(training_houses$conditions),]
# Si suppone che se la condizione è assente è perchè la qualità è scarsa
training_houses$conditions <- as.character(training_houses$conditions)
training_houses <- training_houses %>% mutate(conditions = ifelse(is.na(conditions), "none", conditions))
training_houses$conditions <- as.factor(training_houses$conditions)

training_houses %>% group_by(conditions) %>% summarise(media = mean(selling_price), mediana = median(selling_price))
# Si suppone che ci si sia dimenticati di inserire la condizione della casa, perchè le foto spigavano bene la condizione,
# di conseguenza riempiamo none con un assegnazione casuale
midop <- training_houses %>% group_by(conditions,Macro_zone) %>% summarise(numero = n()) 
look <- training_houses %>% group_by(Macro_zone) %>% count() %>% select(Macro_zone,n)
midop <- left_join(midop,look)
midop <- midop %>% reframe(conditions = conditions, Macro_zone = Macro_zone, score = numero/n) %>% arrange(Macro_zone) #%>% print(n=100)
look <- midop %>% group_by(Macro_zone) %>% reframe(v = cumsum(score))
midop <- tibble(midop, cum_score = look[[2]])

ind <- which(training_houses$conditions == "none")
training_houses[ind,]

zona <- c("Centro_Storico","Circonvalla","Esterno")
for (i in 1: length(ind)) {
  val <- runif(1)
  if_else(training_houses$Macro_zone[ind] == zona[1], 
    # Vero
    if_else(val <= midop$cum_score[1], training_houses$conditions[ind[i]] <- midop$conditions[1],
    if_else((midop$cum_score[1] < val)&(val <= midop$cum_score[2]), training_houses$conditions[ind[i]] <-  midop$conditions[2], 
    if_else((midop$cum_score[2] < val)&(val <= midop$cum_score[3]), training_houses$conditions[ind[i]] <-  midop$conditions[3], 
            training_houses$conditions[ind[i]] <- midop$conditions[5]))),       
    # Falso == Circonvalla
    if_else(training_houses$Macro_zone[ind] == zona[2], 
    if_else(val <= midop$cum_score[6], training_houses$conditions[ind[i]] <- midop$conditions[6],
    if_else((midop$cum_score[6] < val)&(val <= midop$cum_score[7]), training_houses$conditions[ind[i]] <- midop$conditions[7], 
    if_else((midop$cum_score[7] < val)&(val <= midop$cum_score[8]), training_houses$conditions[ind[i]] <- midop$conditions[8], 
            training_houses$conditions[ind[i]] <- midop$conditions[10]))), 
    # Falso == Esterno
    if_else(val <= midop$cum_score[11], training_houses$conditions[ind[i]] <- midop$conditions[11],
    if_else((midop$cum_score[11] < val)&(val <= midop$cum_score[12]), training_houses$conditions[ind[i]] <- midop$conditions[12], 
    if_else((midop$cum_score[12] < val)&(val <= midop$cum_score[13]), training_houses$conditions[ind[i]] <- midop$conditions[13],
            training_houses$conditions[ind[i]] <- midop$conditions[15]))), 
    ))
}

#-----------------------------#
#--- LIFT ----
#-----------------------------#
# La variabile ascensore manca dove ci sono molti piani e le case non sono di nuova costruzione, si suppone che l'ascensore non ci sia
ind <- which(is.na(training_houses$lift))
training_houses$lift <- as.character(training_houses$lift)
training_houses$lift[ind] <- "no"
training_houses$lift <- as.factor(training_houses$lift)


#-----------------------------#
#--- CONDOMINIUM_FEES ----
#-----------------------------#
training_houses %>% select(square_meters,Macro_zone,floor,condominium_fees,total_floors_in_building) %>% 
  group_by(Macro_zone,floor,square_meters,total_floors_in_building) %>% summarise(med = mean(condominium_fees)) %>% print(n=700)
# Semplifico per zona
condo_NA <- training_houses %>% select(zone,condominium_fees) %>% 
  group_by(zone) %>% summarise(media = mean(condominium_fees,na.rm=T), mediana = median(condominium_fees, na.rm=T)) #%>% print(n=200)
# Meglio la media perchè tiene in considerazione anche i valori agli estremi
midop <- training_houses[is.na(training_houses$condominium_fees),]
midop <- inner_join(midop, condo_NA, join_by(zone))
midop <- midop %>% mutate(condominium_fees = round(media))
midop <- midop[,-c(19,20)]
ind <- which(is.na(training_houses$condominium_fees))
training_houses <- rbind(training_houses[-ind,],midop)


#-----------------------------#
#--- HEATING_CENTRALIZED ----
#-----------------------------#
# Le case con il riscaldamento decenralizzato costano mediamente di più?
training_houses %>% select(heating_centralized,selling_price) %>% 
  group_by(heating_centralized) %>% summarise(media = mean(selling_price), mediana = median(selling_price))
training_houses %>% select(conditions,heating_centralized,other_features,condominium_fees) %>% 
  group_by(heating_centralized,condominium_fees) %>% summarise(n=n()) %>% print(n=1000)
training_houses[is.na(training_houses$heating_centralized),]
# Si suppone che i NA siano dei casi in cui ci siano dele problematiche di cui non se ne vuole fare luce nell'annuncio
ind <- which(is.na(training_houses$heating_centralized))
training_houses$heating_centralized <- as.character(training_houses$heating_centralized)
training_houses$heating_centralized[ind] <- "problem"
training_houses$heating_centralized <- as.factor(training_houses$heating_centralized)

#-----------------------------#
#--- OTHER_FEATURES ----
#-----------------------------#
# Si suppone che se non ci siano caratteristiche aggiuntive queste in realtà non esistano, perchè non c'è motivo di non menzionarle
training_houses$other_features <- as.character(training_houses$other_features)
training_houses <- training_houses %>% mutate(other_features = ifelse(is.na(other_features), "none", other_features))
training_houses$other_features <- as.factor(training_houses$other_features)

library(tidytext)
dt <- tibble(text = training_houses$other_features)
dt <- dt %>% mutate(text = strsplit(as.character(text), "\\s\\|\\s")) %>% unnest(cols = text) %>% mutate(text = str_trim(text)) 
split_lengths <- sapply(dt$text, function(x) length(strsplit(as.character(x), "\\s\\|\\s")[[1]]))
dt <- dt %>% mutate(document = rep(1:nrow(dt), times = split_lengths))
term_counts <- dt %>% count(document, text) %>% pivot_wider(names_from = text, values_from = n, values_fill = 0)
# Creo una variabile punteggio, verrranno assegnati punteggi maggiori a quelle caratteristiche che compaiono poche volte,
# questo perchè sono consierabili caratteristiche di pregio
score_term <- enframe(-log(colSums(term_counts)/dim(training_houses)[1]))[-1,]
othFts <- training_houses %>% select(other_features) %>% separate_wider_delim(other_features,delim=" | ",names_sep = " ", too_few = "align_start")

score_feature <- numeric(dim(othFts)[1])
for (i in 1:length(score_feature)) {
  ind <- which(score_term$name %in% othFts[i,])
  score_feature[i] <- round(sum(score_term$value[ind]),3)
}

training_houses$score_feature <- score_feature

#-----------------------------#
#--- YEAR_OF_CONSTRUCTION ----
#-----------------------------#
summary(training_houses$year_of_construction)
hist(training_houses$year_of_construction)#[training_houses$year_of_construction>1900])
# Avere un abitazione costruita durante gli anni in cui si è costruito di più potrebbe venire occultato per problemi di
# vetustà degli impianti o problematiche non visibili legate al tempo
ind <- which(is.na(training_houses$year_of_construction))
set.seed(2606)
midop <- sample(1950:1977, length(ind), replace = T)
training_houses$year_of_construction[ind] <- midop

#-----------------------------#
#--- ENERGY_EFFICIENCY_CLASS ----
#-----------------------------#
# Dopo aver provato a vedere se esista una relazione tra infissi con triplo/doppio vetro e classe energeticq, questa non sussiste
# decidiamo di non considerare energy class. Questa variabile non incide molto nemmeno sul prezzo
ggplot(training_houses[!is.na(training_houses$energy_efficiency_class),]) + 
  geom_boxplot(aes(y=selling_price,x=as.factor(energy_efficiency_class),fill=energy_efficiency_class)) + 
  coord_flip() + theme_minimal()

#-----------------------------#
#--- VERIFICA INCONGRUENZE ----
#-----------------------------#
# Siccome floor e total_floor_building sembrano avere dei problemi ossia meno piani di quanti dovrebbero essercene, si decide
# di considerare solamente floor, ossia il piano in cui si trova l'appartamento
ind <- which(training_houses$floor/training_houses$total_floors_in_building > 1)
training_houses[ind,]

# elimino le case inesistenti, 1mq di abitazione a 12mq con 3 bagni
ind <- which(training_houses$square_meters < 15)
training_houses <- training_houses[-ind,]
# NOn ci sono problemi nelle case con + di 5 stanze
training_houses[((training_houses$rooms_number == "5+")&(training_houses$square_meters<150)),]
# Spese condominiali quasi nulle, è possibile?
training_houses[(training_houses$condominium_fees <= 54),]
training_houses[is.na(training_houses$condominium_fees),]
# NOn si procede in alcuna direzione, le spese condominiali non incidono sul prezzo della casa

# Quali edifici hanno l'ascensore con un edificio sviluppato un piano senza garage (possibile uso)
ind <- which((training_houses$lift == "yes")&(training_houses$total_floors_in_building<2)&(training_houses$car_parking == "no"))
training_houses <- training_houses[-ind,]
# Le case con l'ascensore ed 1 piano vengono eliminate
     
#--- VARIABILI MANCANTI
# L'anno di costruzione e la classe energetica sono variabili che non considereremo nell'analisi per l'alta presenza di NA ed 
# un apparente impossibilità a riempire i valori con una modlaità logicamente valida
new_training_houses <- training_houses[,-c(15,12,7,6)]
vis_miss(new_training_houses)
