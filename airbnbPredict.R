library(caret)
library(ggplot2)

dat <- read.table("data/trnTstData/trainingData.txt",sep="\t", stringsAsFactors=FALSE, header=T)

dat <- dat[!(colnames(dat) %in% c("filename", "name", "id",
                                  "url", "propType"))]

dat$city <- factor(dat$city)
dat$roomType <- factor(dat$roomType)
dat$roomType <- relevel(dat$roomType, "Private room")
dat$propType.t <- factor(dat$propType)

#################
### model 1  ###
#################
cor(data.frame(dat$stars, dat$reviews, dat$accommodates,
               dat$bathrooms, dat$bedrooms, dat$beds, dat$lat.az,
               dat$long.az, dat$multi))
#leaving out beds which is highly correlated w/ accommodates

tc <- trainControl(method="cv", number=10)
mfit.lm <- train(price ~ stars + reviews + accommodates + bathrooms +
                     bedrooms + roomType + city + lat.az + long.az +
                         propType.t + multi,
                 trControl=tc, method="lm", preProc = "center", data=dat)
summary(mfit.lm) # stars, reviews, and bathrooms don't seem to add much
print(mfit.lm)
plot(varImp(mfit.lm))

## TODO: add this later for real test set:
## predVals.lm <- predict(mfit.lm, testing)
## tstVals.lm <- data.frame(obs=testing$price, pred=predVals.lm)

## defaultSummary(tstVals.lm)

## mean(abs(tstVals.lm$obs - tstVals.lm$pred))

## lim <- c(min(dat$price)-10, max(dat$price)+10)
## plot(tstVals.lm, xlim=lim, ylim=lim)
## abline(0,1)

#################
### model 1a  ###
#################
#try all predictors (except high correlated beds), and city
#interactions observed in previous exploration 
mfit.lm <- train(price ~ stars + reviews + accommodates*city + bathrooms +
                     bedrooms + roomType*city + city + lat.az + long.az +
                         propType.t + multi,
                 trControl=tc, method="lm", preProc = "center", data=dat)
summary(mfit.lm) # stars, reviews, and bathrooms don't seem to add much
print(mfit.lm)
#interaction vars, though of interest, are of little importance here
plot(varImp(mfit.lm))

#################
### model 1b  ###
#################
#try city interactions everywhere
mfit.lm <- train(price ~ accommodates*city + 
                     bedrooms*city + roomType*city +  lat.az*city + long.az*city +
                         propType.t*city + multi*city,
                 trControl=tc, method="lm", preProc = "center", data=dat)

#lots more vars, but rmse and R2 about the same
summary(mfit.lm) 
print(mfit.lm)
plot(varImp(mfit.lm))

#################
### model 1c  ###
#################
#simplest model with all predictors apparently contributing
mfit.lm <- train(price ~ accommodates + 
                     bedrooms + roomType +  lat.az + long.az +
                         propType.t + multi + city,
                 trControl=tc, method="lm", preProc = "center", data=dat)
#same rmse as above, slight reduction in R2
summary(mfit.lm) 
print(mfit.lm)
plot(varImp(mfit.lm))

qqplot(dat$price, predVals.lm)

predVals.lm <- predict(mfit.lm, dat)

# residuals 
rs <- dat$price - predVals.lm
plot(rs, predVals.lm)

# accuracy -- underprediction at high prices
lim <- c(min(dat$price) - 10, max(dat$price) + 10)
plot(dat$price, predVals.lm, xlim=lim, ylim=lim)
abline(0,1)


################
### model 2 ###
################
# random forest model 
#500 trees, nodesize=5
mtryGrid <- expand.grid(mtry=seq(1,10))
mfit.rf <- train(price ~  accommodates + bedrooms + roomType +  lat.az + long.az +
                         propType.t + multi + city,
                 trControl=tc, data=dat, method="rf", importance = T,
                  preProc = c("center", "scale"), tuneGrid=mtryGrid, ntree=1000)


print(mfit.rf)
plot(mfit.rf) #best mtry=3

imp <- importance(mfit.rf$finalModel) 
varImpPlot(mfit.rf$finalModel, main="Variable importance: random forest")
plot(varImp(mfit.rf))

predVals.rf <- predict(mfit.rf, dat)
# tighter predictions compared lm, but now overestimates low and
# still underestimates high priced listings
plot(dat$price, predVals.rf, xlim=lim, ylim=lim)
abline(0,1)


######################################
## get testing data
######################################

tDat <- read.table("data/trnTstData/testData.txt",sep="\t", stringsAsFactors=FALSE, header=T)

tDat$city[grepl("Chicago", tDat$filename)] <- "Chicago"
tDat$city[grepl("Houston", tDat$filename)] <- "Houston"
tDat$city[grepl("Los-Angeles", tDat$filename)] <- "LA"
tDat$city[grepl("New-York", tDat$filename)] <- "NYC"
tDat$city[grepl("Philadelphia", tDat$filename)] <- "Philly"

cities <- unique(tDat$city)

cDat <- split(tDat, tDat$city)
for (i in seq_along(cities)) {
    cDat[[i]]$lat.z <- scale(cDat[[i]]$lat)
    cDat[[i]]$long.z <- scale(cDat[[i]]$long)
}
tDat <- do.call("rbind", cDat)

tDat$lat.az <- as.numeric(abs(tDat$lat.z))
tDat$long.az <- as.numeric(abs(tDat$long.z))

tDat$propType.t <- ifelse(tDat$propType %in% c("Apartment", "House"),
                         tDat$propType, "Other")

#ideally should include this in cleanAirbnb() to minimize missing
#owners of multiple listings
tDat$multi <- ifelse( (duplicated(tDat$user) | duplicated(tDat$user,
                                                      fromLast=T)), 1, 0)

datMult <- dat$user[dat$multi==1]
sum(tDat$user %in% datMult)
tDat$multi <- ifelse(tDat$user %in% datMult, 1, tDat$multi)


##############################
### model 1c  -- test data ###
##############################

predVals.lm.tst <- predict(mfit.lm, tDat)

# residuals 
rs <- tDat$price - predVals.lm.tst
plot(rs, predVals.lm.tst)

# accuracy
tlim <- c(min(tDat$price) - 10, max(tDat$price) + 10)
plot(tDat$price, predVals.lm.tst, xlim=tlim, ylim=tlim)
abline(0,1)

#similar performance on new data
tstVals.lm <- data.frame(obs=tDat$price, pred=predVals.lm.tst)
defaultSummary(tstVals.lm)

p.lm <- ggplot(tstVals.lm, aes(obs, pred)) + geom_point() +
     coord_cartesian(xlim = tlim, ylim = tlim) + geom_abline(intercept=0)

p.lm <- p.lm + ggtitle("Predictive model: linear regression") +
    xlab("Listing price") + ylab("Predicted listing price") +
                               theme(axis.title.x=element_text(size=18),
                               axis.text.x=element_text(size=16),
                               axis.title.y= element_text(size=18),
                               axis.text.y=element_text(size=16),
                               plot.title= element_text(size=20)) 


ggsave("pricePredScatter_lm.jpeg")


##############################
### model 2  -- test data ###
##############################

predVals.rf.tst <- predict(mfit.rf, tDat)

# residuals 
rs <- tDat$price - predVals.rf.tst
plot(rs, predVals.rf.tst)

# accuracy
plot(tDat$price, predVals.rf.tst, xlim=tlim, ylim=tlim)
abline(0,1)

tstVals.rf <- data.frame(obs=tDat$price, pred=predVals.rf.tst)
defaultSummary(tstVals.rf)

p.rf <- ggplot(tstVals.rf, aes(obs, pred)) + geom_point() +
    coord_cartesian(xlim = tlim, ylim = tlim) + geom_abline(intercept=0)

p.rf <- p.rf + ggtitle("Predictive model: random forest") +
    xlab("Listing price") + ylab("Predicted listing price") +
                               theme(axis.title.x=element_text(size=18),
                               axis.text.x=element_text(size=16),
                               axis.title.y= element_text(size=18),
                               axis.text.y=element_text(size=16),
                               plot.title= element_text(size=20)) 


ggsave("pricePredScatter_rf.jpeg")



