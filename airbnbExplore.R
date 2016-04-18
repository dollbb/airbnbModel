# explore and visualize some airbnb features in simple models
library(doBy)
library(ggplot2)

dat <- read.table("data/trnTstData/trainingData.txt",sep="\t", stringsAsFactors=FALSE, header=T)


dat$city[grepl("Chicago", dat$filename)] <- "Chicago"
dat$city[grepl("Houston", dat$filename)] <- "Houston"
dat$city[grepl("Los-Angeles", dat$filename)] <- "LA"
dat$city[grepl("New-York", dat$filename)] <- "NYC"
dat$city[grepl("Philadelphia", dat$filename)] <- "Philly"

cities <- unique(dat$city)

cdat <- split(dat, dat$city)
for (i in seq_along(cities)) {
    cdat[[i]]$lat.z <- scale(cdat[[i]]$lat)
    cdat[[i]]$long.z <- scale(cdat[[i]]$long)
}
dat <- do.call("rbind", cdat)

hist(dat$price)
summary(dat$price)

s <- summaryBy(price ~ city + roomType, data=dat, FUN=c(mean, sd, length))



p1 <- ggplot(s, aes(x=city, y=price.mean, fill=roomType)) +
    geom_bar(stat="identity", width=0.5, position=position_dodge(0.5))


p1 <- p1 + ylab("Price") + xlab("City") + ggtitle("Regional Airbnb prices by rental type") +
                               theme(axis.title.x=element_text(size=18),
                               axis.text.x=element_text(size=14),
                               axis.title.y= element_text(size=18),
                               axis.text.y=element_text(size=16),
                               legend.title=element_text(size=16),
                               legend.text= element_text(size=14),
                               plot.title= element_text(size=20)) +
                               guides(fill=guide_legend(title="Room type"))

ggsave("figs/priceTypeBars.jpeg")

dat$city <- factor(dat$city)
dat$roomType <- factor(dat$roomType)
dat$roomType <- relevel(dat$roomType, "Private room")


m1 <- lm(price ~ city * roomType, data=dat)
summary(m1)

#fix non-numeric
dat$accommodates[dat$accommodates=="16+"] <- 16
dat$accommodates <- as.numeric(dat$accommodates)

#missing beds field in 2 listings w/ 1 bed
dat$beds[is.na(dat$beds)] <- 1

cor(data.frame(dat$beds, dat$accommodates, dat$bathrooms))

m2 <- lm(price ~ scale(accommodates, scale=F) * city, data=dat)
#accommodates 16 looks like a high leverage point for philly
m2 <- lm(price ~ scale(accommodates, scale=F) * city, data=subset(dat,
                                                                  accommodates<16))
dat <- subset(dat, accommodates<16)


p2 <- ggplot(dat, aes(x=accommodates, y=price, color=city)) +
    geom_point() + geom_smooth(method=lm, se=T)

p2 <- p2 + ggtitle("Regional Airbnb prices by accommodation size") +
                               theme(axis.title.x=element_text(size=18),
                               axis.text.x=element_text(size=16),
                               axis.title.y= element_text(size=18),
                               axis.text.y=element_text(size=16),
                               legend.title=element_text(size=16),
                               legend.text= element_text(size=14),
                               plot.title= element_text(size=20)) 

ggsave("figs/priceAccomScatter.jpeg")


#higher prices tend to be central, maybe because of density
plot(dat$lat.z, dat$price)
plot(dat$long.z, dat$price)

#abs val as centrality measure
dat$lat.az <- abs(dat$lat.z)
dat$long.az <- abs(dat$long.z)

m3 <- lm(price ~ lat.az * city + long.az * city, data=dat)

p3.1 <- ggplot(dat, aes(x=lat.az, y=price, color=city)) + geom_point() 

p3.2 <- ggplot(dat, aes(x=long.az, y=price, color=city)) + geom_point() 



#check stars
m4 <- lm(price ~ scale(stars) * city, data=dat)

# most listings 0, 4, or 5 stars
p4 <- ggplot(dat, aes(x=stars, y=price, color=city)) +
    geom_point() + geom_smooth(method=lm, se=T)


#these are pretty much all apts, and some houses
summaryBy(price ~ propType, data=dat, FUN=c(mean, sd, length))
#houses not well represented in nyc, but may be worth exploring as a feature
dim(subset(dat, city=="NYC" & propType=="House"))
#collapse across non-apt/houses
dat$propType.t <- ifelse(dat$propType %in% c("Apartment", "House"),
                         dat$propType, "Other")

#not many multi listers, but potentially of interest
dat$multi <- ifelse( (duplicated(dat$user) | duplicated(dat$user,
                                                      fromLast=T)), 1, 0)

summaryBy(price ~ multi, data=dat, FUN = c(mean, sd, length))

#write table w/ transformed long/lat vars
write.table(dat, "data/trnTstData/trainingData.txt", row.names=F, sep="\t")



