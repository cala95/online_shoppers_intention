#Ucitavamo podatke iz data set-a
shoppers_intentions = read.csv("online_shoppers_intention.csv", stringsAsFactors = FALSE)

#Proveravamo strukturu data set-a
summary(shoppers_intentions)
str(shoppers_intentions)

#Vidimo da nema nedostajucih vrednosti, ali proveravamo za svaki slucaj
apply(X = shoppers_intentions, 
      MARGIN = 2, 
      FUN = function(x) length(which(is.na(x) == TRUE)))

#Provericemo da li postoje prazne vrednosti u character varijablama

char_vars <- c("Month","VisitorType")

apply(X = shoppers_intentions[,char_vars], 
      MARGIN = 2, 
      FUN = function(x) length(which(x == "")))

#Pretvaramo character varijable u kategoricke

shoppers_intentions[char_vars] <- lapply(shoppers_intentions[char_vars], factor)

levels(shoppers_intentions$VisitorType)

shoppers_intentions$VisitorType <- factor(shoppers_intentions$VisitorType, levels = c("New_Visitor","Returning_Visitor", "Other"))

table(shoppers_intentions$VisitorType)

levels(shoppers_intentions$Month)
#Dodacemo dva meseca koja nedostaju u faktor Month

shoppers_intentions$Month <- factor(shoppers_intentions$Month, levels = c(levels(shoppers_intentions$Month), "Jan", "Apr"))

#promenicemo redosled nivoa faktora Month

shoppers_intentions$Month <- factor(shoppers_intentions$Month, levels = c("Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sep","Oct","Nov","Dec"))

table(shoppers_intentions$Month)

#Pretvaramo logical varijable u kategoricke

shoppers_intentions$Weekend <- ifelse(test = shoppers_intentions$Weekend == TRUE, yes = 'Yes', no = 'No')

class(shoppers_intentions$Weekend)

shoppers_intentions$Revenue <- ifelse(test = shoppers_intentions$Revenue == TRUE, yes = 'Yes', no = 'No')

class(shoppers_intentions$Revenue)

logical_to_factor <- c("Weekend","Revenue")

shoppers_intentions[logical_to_factor] <- lapply(shoppers_intentions[logical_to_factor], factor)

table(shoppers_intentions$Weekend)

table(shoppers_intentions$Revenue)

#Pretvaramo integer varijable u kategoricke

int_to_factor <- c("OperatingSystems", "Browser", "Region", "TrafficType")

shoppers_intentions[int_to_factor] <- lapply(shoppers_intentions[int_to_factor], factor)

#Izlazna promenljiva je Revenue, pozitivna klasa je "Yes", odnosno da je sesija zavrsena kupovinom

#Ispitujemo povezanost faktor varijabli sa izlaznom varijablom Revenue
#install.packages("ggplot2")
library(ggplot2)

ggplot(data = shoppers_intentions, mapping = aes(x = OperatingSystems, fill = Revenue)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of user sessions") + xlab("OperatingSystems") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

# Pretpostavka je da su dve varijable nezavisne
# Ako je vrednost manja od 0.05 odbacujemo hipotezu, odnosno varijable su zavisne
# U suprotnom izbacujemo taj atribut iz analize jer ne postoji zavisnost sa izlaznom var

tbl1 <- table(shoppers_intentions$Revenue, shoppers_intentions$OperatingSystems)
chisq.test(tbl1)

ggplot(data = shoppers_intentions, mapping = aes(x = Browser, fill = Revenue)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of user sessions") + xlab("Browser") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

tbl2 <- table(shoppers_intentions$Revenue, shoppers_intentions$Browser)
chisq.test(tbl2)

ggplot(data = shoppers_intentions, mapping = aes(x = Region, fill = Revenue)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of user sessions") + xlab("Region") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

tbl3 <- table(shoppers_intentions$Revenue, shoppers_intentions$Region)
chisq.test(tbl3)

#P value je 0.32
#Region varijablu izbacujemo

shoppers_intentions$Region <- NULL

ggplot(data = shoppers_intentions, mapping = aes(x = TrafficType, fill = Revenue)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("PPercentage of user sessions") + xlab("TrafficType") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

tbl4 <- table(shoppers_intentions$Revenue, shoppers_intentions$TrafficType)
chisq.test(tbl4)

ggplot(data = shoppers_intentions, mapping = aes(x = VisitorType, fill = Revenue)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of user sessions") + xlab("VisitorType") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

tbl5 <- table(shoppers_intentions$Revenue, shoppers_intentions$VisitorType)
chisq.test(tbl5)

ggplot(data = shoppers_intentions, mapping = aes(x = Weekend, fill = Revenue)) +
  geom_bar(position = "fill", width = 0.5) +
  ylab("Percentage of user sessions") + xlab("Weekend") +
  theme_bw() + scale_y_continuous(labels = scales::percent)

tbl6 <- table(shoppers_intentions$Revenue, shoppers_intentions$Weekend)
chisq.test(tbl6)


#Ispitujemo povezanost numerickih varijabli sa izlaznom varijablom Revenue
str(shoppers_intentions)

ggplot(data = shoppers_intentions, aes(x = Administrative, fill = Revenue)) + geom_density(alpha = 0.5)

kruskal.test(Administrative ~ Revenue, data = shoppers_intentions) 

ggplot(data = shoppers_intentions, aes(x = Administrative_Duration, fill = Revenue)) + geom_density(alpha = 0.5)

kruskal.test(Administrative_Duration ~ Revenue, data = shoppers_intentions)

ggplot(data = shoppers_intentions, aes(x = Informational, fill = Revenue)) + geom_density(alpha = 0.5)

kruskal.test(Informational ~ Revenue, data = shoppers_intentions)

ggplot(data = shoppers_intentions, aes(x = Informational_Duration, fill = Revenue)) + geom_density(alpha = 0.5)

kruskal.test(Informational_Duration ~ Revenue, data = shoppers_intentions)

ggplot(data = shoppers_intentions, aes(x = ProductRelated, fill = Revenue)) + geom_density(alpha = 0.5)

kruskal.test(ProductRelated ~ Revenue, data = shoppers_intentions)

ggplot(data = shoppers_intentions, aes(x = ProductRelated_Duration, fill = Revenue)) + geom_density(alpha = 0.5)

kruskal.test(ProductRelated_Duration ~ Revenue, data = shoppers_intentions)

ggplot(data = shoppers_intentions, aes(x = BounceRates, fill = Revenue)) + geom_density(alpha = 0.5)

kruskal.test(BounceRates ~ Revenue, data = shoppers_intentions)

ggplot(data = shoppers_intentions, aes(x = ExitRates, fill = Revenue)) + geom_density(alpha = 0.5)

kruskal.test(ExitRates ~ Revenue, data = shoppers_intentions)
ggplot(data = shoppers_intentions, aes(x = PageValues, fill = Revenue)) + geom_density(alpha = 0.5)

kruskal.test(PageValues ~ Revenue, data = shoppers_intentions)

ggplot(data = shoppers_intentions, aes(x = SpecialDay, fill = Revenue)) + geom_density(alpha = 0.5)

kruskal.test(SpecialDay ~ Revenue, data = shoppers_intentions)

# Ne izbacujemo nijednu numericku varijablu jer postoji zavisnost svih varijabli sa izlaznom 

# Cuvamo sredjeni set podataka u RData formatu 
saveRDS(shoppers_intentions, "cleaned_data.RData")
