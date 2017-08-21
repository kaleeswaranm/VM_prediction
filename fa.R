#reading the main file

uni_data   <- read.csv("VMpredMaster.csv")

#cleaning the data

uni_data_f <- uni_data[,c(1,2,3,4,11,12,14,16,17,18,19)]
lapply(uni_data_f, class)
uni_data_f$restaurant_name      <- as.character(uni_data_f$restaurant_name)
uni_data_f$order_date           <- mdy(uni_data_f$order_date)
uni_data_f$order_timestamp      <- mdy_hm(uni_data_f$order_timestamp)
uni_data_f$items_in_order       <- as.character(uni_data_f$items_in_order)
uni_data_f$completed.orders     <- as.factor(uni_data_f$completed.orders)
uni_data_f$edited.and.completed <- as.factor(uni_data_f$edited.and.completed)
uni_data_f$edited.and.cancelled <- as.factor(uni_data_f$edited.and.cancelled)

#restaurants with most edits&completed and cancelled orders

rweco           <- NULL
rweco$rest_id   <- unique(uni_data_f$restaurant_id)
for(i in 1:length(unique(uni_data_f$restaurant_id))) {
  rweco$count[i] <- nrow(uni_data_f[uni_data_f$restaurant_id %in% rweco$rest_id[i] & uni_data_f$edited.and.completed %in% 1,])
}
rweco           <- as.data.frame(rweco)
rweco           <- rweco[order(rweco$count, decreasing = TRUE),]
colnames(rweco) <- c("rest_id", "count_edit")

rweca           <- as.data.frame(table(uni_data_f$restaurant_id[uni_data_f$completed.orders %in% 0]))
rweca           <- rweca[order(rweca$Freq, decreasing = TRUE),]
colnames(rweca) <- c("rest_id", "count_cancel")

ratto           <- as.data.frame(table(uni_data_f$restaurant_id))
ratto           <- ratto[order(ratto$Freq, decreasing = TRUE),]
colnames(ratto) <- c("rest_id", "total_orders")

data_summary    <- merge(ratto, rweco, by = "rest_id", all = TRUE)
data_summary    <- merge(data_summary, rweca, by = "rest_id", all = TRUE)
data_summary[is.na(data_summary)] <- 0
data_summary$editscancel          <- data_summary$count_edit + data_summary$count_cancel
data_summary    <- data_summary[order(data_summary$editscancel, decreasing = TRUE),]

#demand forecast

uni_data_df <- uni_data_f
for(i in 1:nrow(uni_data_df)) { print(i)
  if(minute(uni_data_f$order_timestamp[i]) >= 0 & minute(uni_data_f$order_timestamp[i]) < 30) {
    minute(uni_data_df$order_timestamp[i]) <- 0
  }
  else {
    minute(uni_data_df$order_timestamp[i]) <- 30
  }
}
dfdf              <- as.data.frame(table(uni_data_df$order_timestamp))
colnames(dfdf)    <- c("time", "count")
dfdf$time         <- ymd_hms(dfdf$time)
dfdf              <- pad(dfdf)
dfdf              <- dfdf[hour(dfdf$time) %in% 8:23,]
dfdf              <- dfdf[!(hour(dfdf$time) %in% 23 & minute(dfdf$time) %in% 30),]
dfdf[is.na(dfdf)] <- 0
ggplot(dfdf, aes(x = time, y = count)) + geom_line()
tsdfdf            <- msts(dfdf$count, seasonal.periods = c(31, 31 * 7))
tstbats           <- tbats(tsdfdf)
forecast(tstbats)
plot(forecast(tstbats))

#analysis on rest_id 5252

uni_data_5252    <- uni_data_f[uni_data_f$restaurant_id %in% 5252,]
minute(uni_data_5252$order_timestamp) <- 0
df5252           <- as.data.frame(table(uni_data_5252$order_timestamp))
colnames(df5252) <- c("time", "count")
df5252$time      <- ymd_hms(df5252$time)
df5252           <- pad(df5252)
df5252[is.na(df5252)]    <- 0
df5252 <- df5252[hour(df5252$time) %in% 8:23,]
ggplot(df5252[df5252$count != 0,], aes(x = time, y = count)) + geom_line() + geom_point(size = 0.03, color = "red") + 
                                                               scale_x_datetime(date_breaks = "4 days", date_labels = "%b-%d") +
                                                               theme(axis.text.x = element_text(angle = 90)) +
                                                               scale_y_continuous(breaks = seq(2,20,by=2))

#analysis on rest_id 20799

uni_data_20799    <- uni_data_f[uni_data_f$restaurant_id %in% 20799,]
minute(uni_data_20799$order_timestamp) <- 0
df20799           <- as.data.frame(table(uni_data_20799$order_timestamp))
colnames(df20799) <- c("time", "count")
df20799$time      <- ymd_hms(df20799$time)
df20799           <- pad(df20799)
df20799[is.na(df20799)] <- 0
df20799           <- df20799[hour(df20799$time) %in% 8:23,]
ggplot(df20799[df20799$count != 0,], aes(x = time, y = count)) + geom_line() + geom_point(size = 0.03, color = "red") + 
                                                                 scale_x_datetime(date_breaks = "4 days", date_labels = "%b-%d") +
                                                                 theme(axis.text.x = element_text(angle = 90)) +
                                                                 scale_y_continuous(breaks = seq(2,200,by=2))
tsdf20799 






