library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(maps)
library(mapproj)
library(maptools)
library(GGally)
library(ggalt)
library(mice)
library(class)
library(caret)
library(e1071)

BeersAndBrews = read.csv(file.choose(), header = TRUE)

#Part 8 - Make another column with the actual beer type based on the Style

grep("IPA", ignore.case = TRUE, BeersAndBrews$Style)
grep("Ale", ignore.case = TRUE, BeersAndBrews$Style)

IPABeers = BeersAndBrews %>% select(Beer_Name, ABV, IBU, Style, State) %>% filter(str_detect(Style, "IPA"))
AleBeers = BeersAndBrews %>% select(Beer_Name, ABV, IBU, Style, State) %>% filter(str_detect(Style, "Ale"))

IPABeers$State =  str_replace(IPABeers$State, " ", "")

IPABeers$State_Name = tolower(IPABeers$State_Name)
IPABeers$State = as.character(IPABeers$State)
IPABeers$State_Name = abbr2state(IPABeers$State)

IPABeers %>% filter(State == "AL")
NumberStates = count(IPABeers, State)

states = map_data("state")
map.df = merge(states, IPABeers, by.x = "region", by.y = "State_Name", all.x = T)
map.df = map.df[order(map.df$order),]
map.df = merge(NumberStates, map.df, by = "State")
colnames(map.df)[2] = "Amount"

map.df %>% ggplot(aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Amount)) + geom_path() +
  scale_fill_gradientn(colours = rev(heat.colors(10)), na.value = "grey90") + coord_map() + ggtitle("Heat Map of IPA Beers Brewed")

#Part 9
set.seed(6)
splitPerc = .70
summary(IPABeers)
IPABeers = select(IPABeers, -c(5,6))
IPABeers$Type = "IPA"

AleBeers = select(AleBeers, -c(5))
AleBeers$Type = "Ale"

beersALEIPA = rbind(IPABeers, AleBeers)

trainIndices = sample(1:dim(beersALEIPA)[1], round(splitPerc * dim(beersALEIPA)[1]))
train = beersALEIPA[trainIndices,]
test = beersALEIPA[-trainIndices,]

beersALEIPA %>% ggplot(aes(x = ABV, y = IBU, color = Type)) + geom_point()

#k = 3
classifications = knn(train[, c(2,3)], test[, c(2,3)], train$Type, prob = TRUE, k = 3)
table(classifications, test$Type)
confusionMatrix(table(classifications, test$Type))

# k = 7
classifications = knn(train[, c(2,3)], test[, c(2,3)], train$Type, prob = TRUE, k = 7)
table(classifications, test$Type)
confusionMatrix(table(classifications, test$Type))

#k = 13
classifications = knn(train[, c(2,3)], test[, c(2,3)], train$Type, prob = TRUE, k = 13)
table(classifications, test$Type)
confusionMatrix(table(classifications, test$Type))
