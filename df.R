install.packages("padr")
library(padr)
install.packages("tidyr")
library(tidyr)
library(plotly)
head(oidsx)
oidsx <- oids
lapply(oidsx, class)
oidsx$order_timestamp <- mdy_hm(oidsx$order_timestamp)
minute(oidsx$order_timestamp) <- 0
oidsx$order_date <- mdy(oidsx$order_date)
oidsx$restaurant_name <- as.character(oidsx$restaurant_name)
oidsx$items_in_order  <- as.character(oidsx$items_in_order)
oidsx20799 <- oidsx[oidsx$restaurant_id %in% 20799, ]
nrow(oidsx20799)
hb20799 <- data.frame(matrix(NA, nrow = length(unique(oidsx20799$order_timestamp)), ncol = 3))
colnames(hb20799) <- c("Date.Hour", "Count", "Count_Cancellations")
hb20799$Date.Hour <- unique(oidsx20799$order_timestamp)
hb20799$Date.Hour <- ymd_hms(hb20799$Date.Hour)
for(i in 1:length(unique(oidsx20799$order_timestamp))) {
  hb20799$Count[i] <- nrow(oidsx20799[oidsx20799$order_timestamp %in% hb20799$Date.Hour[i],])
  hb20799$Count_Cancellations[i] <- nrow(oidsx20799[oidsx20799$order_timestamp %in% hb20799$Date.Hour[i] & oidsx20799$post_status %in% "Cancelled",])
}
head(hb20799)
hb20799oc <- hb20799[hb20799$Count_Cancellations != 0, c(1,3)]
head(hb20799oc)
ggplot(hb20799pad, aes(x = Date.Hour, y = Count)) + geom_bar(stat = "identity") +
  geom_point(aes(x = Date.Hour, y = Count_Cancellations), color = "Blue", data = hb20799oc)
ggplot(hb20799, aes(x = Date.Hour, y = Count)) + geom_point(color = "red") + geom_line(color = "White") + 
geom_point(aes(x = Date.Hour, y = Count_Cancellations), color = "Blue", data = hb20799oc) +
scale_x_datetime(date_breaks = "4 days", date_minor_breaks = "6 hours", date_labels = "%b-%d") +  
theme_dark() + theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
hb20799pad <- pad(hb20799)[hour(pad(hb20799)$Date.Hour) %in% 8:23,]
hb20799pad$Count[is.na(hb20799pad$Count)] <- 0
hb20799pad$Count_Cancellations[is.na(hb20799pad$Count_Cancellations)] <- 0
hb20799padg <- gather(hb20799pad, Y, Count, Count:Count_Cancellations)
ggplot(hb20799pad, aes(x = Date.Hour, y = Count)) + geom_point(color = "red") + geom_line(color = "White") + 
  geom_point(aes(x = Date.Hour, y = Count_Cancellations), color = "Blue", data = hb20799oc) +
  scale_x_datetime(date_breaks = "4 days", date_minor_breaks = "6 hours", date_labels = "%b-%d") +  
  theme_dark() + theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
table(hour(hb20799pad$Date.Hour))
table(day(hb20799pad$Date.Hour))
head(oidsx)
hb <- data.frame(matrix(NA, nrow = length(unique(oidsx$order_timestamp)), ncol = 2))
colnames(hb) <- c("Date.Hour", "Count")
hb$Date.Hour <- unique(oidsx$order_timestamp)
hb$Date.Hour <- ymd_hms(hb$Date.Hour)
for(i in 1:length(unique(oidsx$order_timestamp))) {
  hb$Count[i] <- nrow(subset(oidsx, order_timestamp == hb$Date.Hour[i]))
}
hb$month <- month(hb$Date.Hour)
ggplot(hbkx, aes(x = Date.Hour, y = Count)) + geom_point(color = "red") + geom_line(color = "White") +  theme_dark() +
                                            scale_x_datetime(date_breaks = "4 days", date_minor_breaks = "6 hours", date_labels = "%b-%d") +
                                            theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) + facet_wrap(~month, scales = "free_x")
ggplot(hbkx, aes(x = Date.Hour, y = Count)) + geom_point(color = "red") + geom_line(color = "White") +  theme_dark() +
  scale_x_datetime(date_breaks = "4 days", date_minor_breaks = "6 hours", date_labels = "%b-%d") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
hb  <- hb[order(hb$Date.Hour, decreasing = FALSE),]
head(hb)
table(hour(hb$Date.Hour))
hbk <- hb[(hour(hb$Date.Hour) %in% 8:23),]
table(hour(hbk$Date.Hour))
hbkx <- pad(hbk)
hbkx <- hbkx[hour(hbkx$Date.Hour) %in% 8:23,]
hbkx$month <- month(hbkx$Date.Hour)
hbkx[hbkx$Count == 0,]
head(hbkx)
tail(hbkx, 200)
table(hour(hbkx$Date.Hour))
table(is.na(hbkx$Count))
hbkx$Count[is.na(hbkx$Count)] <- 0
tsfd <- msts(hbkx$Count, seasonal.periods = c(16, 16*7))
affd <- auto.arima(tsfd, approximation = FALSE, trace = FALSE)
hbkx$month <- month(hbkx$Date.Hour)
tsfdtb <- tbats(tsfd)
accuracy(forecast(tsfdtb))
forecast(tsfdtb)
plot(forecast(tsfdtb))


  
  
  
  
  
