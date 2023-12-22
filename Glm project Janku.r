rm(list = ls())

library(dplyr) |> suppressPackageStartupMessages() 
library(ggplot2) |> suppressPackageStartupMessages() 
library(corrplot) |> suppressPackageStartupMessages() 
library(Metrics) |> suppressPackageStartupMessages() 
library(caret) |> suppressPackageStartupMessages() 

# the file fulldata.csv was obtained by merging together all the datasets for individual city parts
#  and adding a new cathegorical variable CityPart
data <- read.csv("fulldata.csv")
data <- data[, -which( names(data) %in% c("X", "Location") )] # we don't need index and location
to_factorise <- names(data)
to_factorise <- to_factorise[!to_factorise %in% c("Price", "Area", "No..of.Bedrooms", "Resale", "CityPart")]

# correlation matrix for the binary "yes/no" variables
corrplot( cor( data[ data$School != 9, c( "No..of.Bedrooms", to_factorise ) ] ) )

# rearranging variables for better visualisation of correlated clusters
group1 <- c( "MaintenanceStaff", "Gymnasium", "SwimmingPool", "LandscapedGardens", 
             "JoggingTrack", "RainWaterHarvesting", "ClubHouse", "X24X7Security", "MultipurposeRoom", "Children.splayarea",
             "IndoorGames", "Intercom", 
             "SportsFacility", "PowerBackup", 
             "CarParking", "StaffQuarter",  
             "VaastuCompliant", "Cafeteria", 
             "LiftAvailable", "GolfCourse" )
group2 <- c( "ShoppingMall", "ATM", "School", "Hospital" )
group3 <- c( "Wifi", "Wardrobe", "Gasconnection", "WashingMachine", "AC", "BED", "Microwave", "TV", "DiningTable",
             "Sofa", "Refrigerator" )
corrplot( cor( data[ data$School != 9, c( "No..of.Bedrooms", group1, group2, group3 )] ) )

# factorise the cathegorical variables
data[,to_factorise] <- lapply(data[,to_factorise], factor)
data$Resale <- data$Resale |> factor()
data$CityPart <- data$CityPart |> factor()

summary(data)

NAcount <- rowSums( data[,to_factorise] == 9 )
sum( NAcount != 0 )
sum( NAcount[ NAcount != 0 ] == length(to_factorise) )
data$had_NA <- (NAcount != 0)

summary(factor(data$No..of.Bedrooms))
ggplot(data, aes(x = factor(No..of.Bedrooms) ) ) + geom_bar()

ggplot(data %>% filter( No..of.Bedrooms < 5 ), aes(x = factor(No..of.Bedrooms), fill = CityPart ) ) + 
  geom_bar(position = "fill")
ggplot(data, aes(x = factor(No..of.Bedrooms), fill = Resale)) +
  geom_bar(position = "fill")

hist(data$Price, breaks = 50)
hist(data$Area, breaks = 50)

hist(log(data$Price), breaks = 50)
hist(log(data$Area), breaks = 50)

ggplot(data, aes(x=factor(No..of.Bedrooms), y=log(Area) ) ) + 
  geom_boxplot( aes( group = No..of.Bedrooms ), outlier.colour="black", outlier.shape=16,
                outlier.size=2, notch=FALSE)

ggplot(data, aes(x=factor(No..of.Bedrooms), y=log(Price) ) ) + 
  geom_boxplot( aes( group = No..of.Bedrooms ), outlier.colour="black", outlier.shape=16,
                outlier.size=2, notch=FALSE)

ggplot(data, aes(x = CityPart, fill = Resale)) + geom_bar()
ggplot(data, aes(x = CityPart, fill = Resale)) + geom_bar(position = "fill")
ggplot(data %>% filter( No..of.Bedrooms < 5 ), aes(x = CityPart, fill = factor(No..of.Bedrooms))) +
  geom_bar()
ggplot(data %>% filter( No..of.Bedrooms < 5 ), aes(x = CityPart, fill = factor(No..of.Bedrooms))) +
  geom_bar(position = "fill")

ggplot(data, aes(x=log(Price), y=log(Area) ) ) + 
  geom_point( aes(color = CityPart) )

ggplot(data %>% filter(No..of.Bedrooms < 5), aes(x=log(Price), y=log(Area) ) ) + 
  geom_point( aes(color = No..of.Bedrooms) )

ggplot(data, aes(x=log(Price), y=log(Area) ) ) + geom_point()

ggplot(data %>% filter(MaintenanceStaff != 9), aes(x=log(Price), y=log(Area) ) ) + 
  geom_point()   # removing 9s increases price/area correlation

# splitting data to train and test set, train set is 70%
set.seed(101) # for replicability
n = nrow(data)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train = data[trainIndex ,]
test = data[-trainIndex ,]

data2 <- train
data2$Area <- log(train$Area)
data2$Price <- log(train$Price)
model.poisson_all <- glm(No..of.Bedrooms ~ ., data = data2, family = "poisson" )
summary(model.poisson_all)

#===POISSON
model.poisson <- glm(No..of.Bedrooms ~ log(Price) + log(Area) + Resale, 
                     data = train, family = "poisson")
summary(model.poisson)
model.poisson2 <- glm(No..of.Bedrooms ~ log(Price) + log(Area) + Resale + CityPart, 
                      data = train, family = "poisson")
summary(model.poisson2)

anova(model.poisson, model.poisson2, test = "Chisq")
# adding CityPart significantly improves fit

model.poisson3 <- glm(No..of.Bedrooms ~ log(Price) + log(Area) + Resale + CityPart + Children.splayarea, 
                      data = train, family = "poisson")
summary(model.poisson3)

anova(model.poisson2, model.poisson3, test = "Chisq")
# adding Children.splayarea once more significantly improves fit


model.poisson4 <- glm(No..of.Bedrooms ~ log(Price) + log(Area) + Resale + 
                      CityPart + Intercom + PowerBackup + Children.splayarea + LiftAvailable,
                    data = train, family = "poisson" )
summary(model.poisson4)
anova(model.poisson3, model.poisson4, test = "Chisq")
# also improve fit
# so the best poisson model is probably the last one 

exp(model.poisson4$coefficients)

model.lm_all <- lm(No..of.Bedrooms ~ ., data = data2)
summary(model.lm_all)

#===GAUSS
model.gauss <- glm(No..of.Bedrooms ~ log(Price) + log(Area) + Resale,
                   data = train, family = gaussian( link = "identity") )
summary(model.gauss)
# model.gauss2 <- glm(No..of.Bedrooms ~ log(Price) + log(Area) + Resale + CityPart + 
#                       ftu_count + infrastruct_count + equip_count + neighborhood_count + had_NA,
#                    data = train, family = gaussian( link = "identity"))
model.gauss2 <- glm(No..of.Bedrooms ~ log(Price) + log(Area) + Resale + CityPart,
                    data = train, family = gaussian( link = "identity") )
summary(model.gauss2)
anova(model.gauss, model.gauss2, test = "Chisq")
# adding CityPart significantly improves fit
# also resale became significant now

model.gauss3 <- glm(No..of.Bedrooms ~ log(Price) + log(Area) + Resale + 
                      CityPart + Children.splayarea,
                    data = train, family = gaussian( link = "identity"))
summary(model.gauss3)
anova(model.gauss2, model.gauss3, test = "Chisq")
# also adding Children.splayarea significantly improves fit

model.gauss4 <- glm(No..of.Bedrooms ~ log(Price) + log(Area) + Resale + CityPart + 
                        SwimmingPool + RainWaterHarvesting + IndoorGames + Intercom + 
                        PowerBackup + Gasconnection + Children.splayarea + LiftAvailable,
                    data = train, family = gaussian( link = "identity"))
summary(model.gauss4)
anova(model.gauss3, model.gauss4, test = "Chisq")
# also improve fit
# so the best gaussian model is the last one 

confint(model.gauss4)

#===LM
model.lm <- lm( data = train, No..of.Bedrooms ~ log(Price) + log(Area) + Resale )
model.lm2 <- lm( data = train, No..of.Bedrooms ~ log(Price) + log(Area) + 
                   Resale + CityPart )
model.lm3 <- lm( data = train, No..of.Bedrooms ~ log(Price) + log(Area) +
                   Resale + CityPart + Children.splayarea )
model.lm4 <- lm( data = train, No..of.Bedrooms ~ log(Price) + log(Area) + Resale + CityPart + 
                        SwimmingPool + RainWaterHarvesting + IndoorGames + Intercom + 
                        PowerBackup + Gasconnection + Children.splayarea + LiftAvailable )
anova(model.lm, model.lm2)
anova(model.lm2, model.lm3)
anova(model.lm3, model.lm4)
# it's the same as gaussian

summary(model.lm4)
confint(model.lm4)

plot(model.lm4)
#plot(model.lm4,5)

# ------------------------> PREDICTION AND VALIDATION <------------------------

predicts.poisson = predict(model.poisson2, newdata = test, type = "response")
rmse( test$No..of.Bedrooms, predicts.poisson )
#factor( round(predicts.poisson) )
#factor( test$No..of.Bedrooms )
cv.poisson = confusionMatrix( factor( round(predicts.poisson) ),
                              factor( test$No..of.Bedrooms ) )
print(cv.poisson)

predicts.poisson = predict(model.poisson3, newdata = test, type = "response")
rmse( test$No..of.Bedrooms, predicts.poisson )
cv.poisson = confusionMatrix( factor( round(predicts.poisson) ),
                              factor( test$No..of.Bedrooms ) )
print(cv.poisson)

predicts.poisson = predict(model.poisson4, newdata = test, type = "response")
rmse( test$No..of.Bedrooms, predicts.poisson )
cv.poisson = confusionMatrix( factor( round(predicts.poisson) ),
                              factor( test$No..of.Bedrooms ) )
print(cv.poisson)

predicts.gauss <- predict(model.gauss2, newdata = test, type = "response")
predicts.gauss[ predicts.gauss < 0.5 ] = 1
rmse( test$No..of.Bedrooms, predicts.gauss )
cv.gauss = confusionMatrix( factor( round(predicts.gauss) ), 
                         factor( test$No..of.Bedrooms ) )
print(cv.gauss)

predicts.gauss <- predict(model.gauss3, newdata = test, type = "response")
predicts.gauss[ predicts.gauss < 0.5 ] = 1
rmse( test$No..of.Bedrooms, predicts.gauss )
cv.gauss = confusionMatrix( factor( round(predicts.gauss) ), 
                         factor( test$No..of.Bedrooms ) )
print(cv.gauss)
predicts.gauss <- predict(model.gauss4, newdata = test, type = "response")
predicts.gauss[ predicts.gauss < 0.5 ] = 1
rmse( test$No..of.Bedrooms, predicts.gauss )
cv.gauss = confusionMatrix( factor( round(predicts.gauss) ), 
                         factor( test$No..of.Bedrooms ) )
print(cv.gauss)

predicts.lm <- predict(model.lm2, newdata = test, type = "response")
predicts.lm[ predicts.lm < 0.5 ] = 1
rmse( test$No..of.Bedrooms, predicts.lm )
cv.lm = confusionMatrix( factor( round(predicts.lm) ), 
                         factor( test$No..of.Bedrooms ) )
print(cv.lm)

predicts.lm <- predict(model.lm3, newdata = test, type = "response")
predicts.lm[ predicts.lm < 0.5 ] = 1
rmse( test$No..of.Bedrooms, predicts.lm )
cv.lm = confusionMatrix( factor( round(predicts.lm) ), 
                         factor( test$No..of.Bedrooms ) )
print(cv.lm)

predicts.lm <- predict(model.lm4, newdata = test, type = "response")
predicts.lm[ predicts.lm < 0.5 ] = 1
rmse( test$No..of.Bedrooms, predicts.lm )
cv.lm = confusionMatrix( factor( round(predicts.lm) ), 
                         factor( test$No..of.Bedrooms ) )
print(cv.lm)
# best achieved accuracy is around 0.7776, with most errors being just +-1 room 
#   (its an ok model, although the interpretation is problematic with the negative intercept)

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(9,"Set1")
names(myColors) <- levels(factor(1:9))
colScale <- scale_colour_manual(name = "grp",values = myColors)

# test %>% filter(No..of.Bedrooms < 5)
ggplot(test %>% filter(No..of.Bedrooms < 7) , aes(x=log(Price), y=log(Area) ) ) + 
    geom_point( aes(color = factor( No..of.Bedrooms ) ) ) + 
    scale_color_brewer(palette = "PuOr")

ggplot(test, aes(x=log(Price), y=log(Area) ) ) + 
    geom_point( aes(color = factor( round(predicts.gauss) ) ) ) + 
    scale_color_brewer(palette = "PuOr")

ggplot(test, aes(x=log(Price), y=log(Area) ) ) + 
    geom_point( aes(color = ( round(predicts.gauss) == No..of.Bedrooms ) ) )
