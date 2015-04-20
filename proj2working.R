weather.data <- read.csv('repdata_data_StormData.csv.bz2', stringsAsFactors=FALSE)

num.instances <- aggregate(rep(1, 902297), by=list(str_trim(weather.data$EVTYPE)), FUN=sum)

most.deadly <- aggregate(weather.data$FATALITIES[(weather.data$FATALITIES>0)], by=list(toupper(str_trim(weather.data$EVTYPE[(weather.data$FATALITIES>0)]))), FUN=sum)
most.deadly <- merge(most.deadly, num.instances, by.x="Group.1", by.y="Group.1")
most.deadly <- cbind(most.deadly, avg.fatality=(most.deadly$x.x/most.deadly$x.y))
arrange(most.deadly, desc(x.x))

prop.damage <- aggregate(weather.data$PROPDMG[(weather.data$PROPDMG>0)], by=list(toupper(weather.data$EVTYPE[(weather.data$PROPDMG>0)]), toupper(str_trim(weather.data$PROPDMGEXP[(weather.data$PROPDMG>0)]))), FUN=sum)
crop.damage <- aggregate(weather.data$CROPDMG[(weather.data$CROPDMG>0)], by=list(toupper(weather.data$EVTYPE[(weather.data$CROPDMG>0)]), toupper(str_trim(weather.data$CROPDMGEXP[(weather.data$CROPDMG>0)]))), FUN=sum)
multiplier <- data.frame(indic=c("H","K","M","B"), factor=c(100, 1000, 1000000, 1000000000))

prop.damage <- merge(prop.damage, multiplier, by.x="Group.2", by.y="indic", all.x=TRUE)
prop.damage$factor[is.na(prop.damage$factor)] <- 1
prop.damage <- cbind(prop.damage, adj.dmg=(prop.damage$x * prop.damage$factor))
names(prop.damage) <- c("exp", "ev.type", "dmg", "factor", "adj.dmg")


crop.damage <- merge(crop.damage, multiplier, by.x="Group.2", by.y="indic", all.x=TRUE)
crop.damage$factor[is.na(crop.damage$factor)] <- 1
crop.damage <- cbind(crop.damage, adj.dmg=(crop.damage$x * crop.damage$factor))
names(crop.damage) <- c("exp", "ev.type", "dmg", "factor", "adj.dmg")

comb.dmg <- rbind(prop.damage, crop.damage)
total.damage <- aggregate(comb.dmg$adj.dmg, by=list(comb.dmg$ev.type), FUN=sum)
total.damage <- merge(total.damage, num.instances, by.x="Group.1", by.y="Group.1")
total.damage <- cbind(total.damage, avg.damage=(total.damage$x.x/total.damage$x.y))
arrange(total.damage, desc(x.x))
