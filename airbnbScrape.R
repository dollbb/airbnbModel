# scrapeAirbnb() scrapes visible front end of Airbnb listings for
# given city and accommodation type. writes data to tab delimited txt file.
#
# cleanAirbnb() cleans scraped data given directory of txt
# file(s), removing duplicates, private room accommodation type (very
# few listings), and optionally extreme outliers (less than Q1 -
# 3*IQR, greater than Q3 + 3*IQR). creates a subdirectory trnTstData
# containing random subsets of data for subsequent modeling:
# trainingData.txt, and testData.txt

library(rvest)
library(caret)


cities <- c("New-York--NY", "Los-Angeles",
            "Chicago--IL", "Houston--TX", "Philadelphia--PA") 
            
scrapeAirbnb <- function (city, pages=17, write=T, aptType="all") {
# get data from each visible page
# aptType = c("all", "entire", "private")
    aptType <- tolower(aptType)
    vars <- data.frame()
    for (i in 1:pages) {
        
        if (aptType == "all") {
            pageName <- paste0("https://www.airbnb.com/s/", city, "?page=", toString(i))
        } else if (aptType == "entire") {
            pageName <- paste0("https://www.airbnb.com/s/", city,
                               "?room_types%5B%5D=Entire+home%2Fapt&ss_id=1tzs46xn&page=",
                               toString(i), "&s_tag=ayaawVnC")
        } else if (aptType == "private") {
            pageName <- paste0("https://www.airbnb.com/s/", city,
                               "?room_types%5B%5D=Private+room&ss_id=1tzs46xn&page=",
                               toString(i), "&s_tag=wT7IJDNJ")
        }
        page <- read_html(pageName)
    
    lat <- as.numeric(html_attr(html_nodes(page, ".listing"), "data-lat"))
    long <- as.numeric(html_attr(html_nodes(page, ".listing"), "data-lng"))
    name <- html_attr(html_nodes(page, ".listing"), "data-name")
    url <- html_attr(html_nodes(page, ".listing"), "data-url")
    user <- as.numeric(html_attr(html_nodes(page, ".listing"), "data-user"))
    id <- as.numeric(html_attr(html_nodes(page, ".listing"), "data-id"))
    price <- as.numeric(html_text(html_nodes(page, ".price-amount")))
    stars <- as.numeric(html_attr(html_nodes(page, ".listing"), "data-star-rating"))
    reviews <- as.numeric(html_attr(html_nodes(page, ".listing"), "data-review-count"))   
    
    curFrame <- data.frame(lat, long, name, url, user, id, price, stars, reviews)

    vars <- rbind(vars, curFrame)
    }


#scrape details from individual listing pages
    for (j in 1:dim(vars)[1]) {
        indivPageName <- paste0("https://www.airbnb.com", vars$url[j])
        indivPage <- read_html(indivPageName)
        specsNames <- html_text(html_nodes(indivPage, ":nth-child(6) .col-md-6 div span"))
        specsNames <- specsNames[grep(specsNames, pattern=":")]
        specsVals <- html_text(html_nodes(indivPage, ":nth-child(6) .col-md-6 div strong"))
        
        if (length(grep(specsNames, pattern = "Accommodates"))>0) {
            vars$accommodates[j] <- specsVals[grep(specsNames, pattern = "Accommodates")]
        }
        if (length(grep(specsNames, pattern = "Bathrooms"))>0) {
            vars$bathrooms[j] <- specsVals[grep(specsNames, pattern = "Bathrooms")]
        }
        if (length(grep(specsNames, pattern = "Bedrooms"))>0) {
            vars$bedrooms[j] <- specsVals[grep(specsNames, pattern = "Bedrooms")]
        }    
        if (length(grep(specsNames, pattern = "Beds"))>0) {
            vars$beds[j] <- specsVals[grep(specsNames, pattern = "Beds")]
        }    
        if (length(grep(specsNames, pattern = "Property type"))>0) {
            vars$propType[j] <- specsVals[grep(specsNames, pattern ="Property type")]
        }
        if (length(grep(specsNames, pattern = "Room type"))>0) {
            vars$roomType[j] <- specsVals[grep(specsNames, pattern ="Room type")]            
        }    
    }
    
if (write) {
    write.table(vars, paste0("airbnb_scrape_", city, "_", aptType, ".txt"), sep="\t", row.names=F)
}
    
return(vars)    
}



cleanAirbnb <- function(datDir, removeOutliers = T, trainingProp = 0.8) {

    lchar <- substr(datDir,nchar(datDir), nchar(datDir))
    if (lchar != "/") { datDir <- paste0(datDir, "/") }

    filenames <- list.files(datDir, pattern=".txt")
    
    #add filenames
    dat <- data.frame()
    for (i in 1:length(filenames)) {
        fpath <- paste0(datDir, filenames[i])
        fdat <- read.table(fpath, sep="\t", header=T)
        fdat$filename <- filenames[i]        
        dat <- rbind(dat, fdat)
    }

    #very few shared rooms:
    dat <- subset(dat, roomType != "Shared room")

    #remove duplicates:
    dat <- dat[!(duplicated(dat$id)), ]

    #remove extreme outliers:
    if (removeOutliers) {
        upper <- quantile(dat$price)[4] + 3 * IQR(dat$price)
        lower <- quantile(dat$price)[2] - 3 * IQR(dat$price)
        dat <- subset(dat, price < upper & price > lower)
    }
           
    trnRows <- createDataPartition(dat$price, p=trainingProp, list=F)    
    trnDat <- dat[trnRows,]
    tstDat <- dat[-trnRows,]

    if (!(file.exists(paste0(datDir, "trnTstData")))) {
        dir.create(paste0(datDir, "trnTstData"))
    }
    
    if (length(dir(paste0(datDir,"trnTstData/"))) ==0) {
        write.table(trnDat, paste0(datDir, "trnTstData/", "trainingData.txt"), row.names=F, sep="\t")
        write.table(tstDat, paste0(datDir, "trnTstData/", "testData.txt"), row.names=F,sep="\t")            
    } else {
        stop(paste("directory", paste0(datDir, "trnTstData/"), "is not empty!!!"))
    }
    
    return(trnDat)
}

    




