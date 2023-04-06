setwd("C:/Users/user/Desktop/yr/연구실 논문/data")
save.image("illegal_parking.RData")
load("illegal_parking.RData")
# prepare ---- 
library(sf)
library(spData) # for sf data
library(dplyr)
library(rgdal)
library(tidyverse)
library(reshape2)

options(scipen = 10000)
################# load shapefile###################
setwd(choose.dir())
oa <- readOGR("C:/Users/user/Desktop/yr/연구실 논문/data/통계지역경계(2016년+기준)/집계구.shp")

############ load illegal parking###################
setwd("C:/Users/user/Desktop/yr/연구실 논문/data")
parking <- read.csv("oa_time_ratio.csv")

#########illegal parking
parking$total <- 504213
parking$count <- (parking$ratio/100) * parking$total

parking$time <- as.numeric(parking$time)
parking$count <- as.numeric(parking$count)
# plot(parking$time, parking$count)
#day
# illegal parking by day
complete_oa <-parking[complete.cases(parking),]

head(complete_oa)
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
daily_data <- complete_oa %>%
  group_by(substr(time,5,8)) %>%
  summarise(day_parking = sum(count))



head(daily_data)
names(daily_data) <- c("date", "day_parking")
length(daily_data$date)
daily_data$date <- 1:365

plot(daily_data$date, daily_data$day_parking, type = "l", lwd = 2, col = "blue", 
     xlab = "date", ylab = "number of illegal parking per day", main = "Number of illegal parking per day in 2019")

#daily_time
daily_time_data <- complete_oa %>%
  group_by(time) %>%
  summarise(time_parking = sum(count))

daily_time_data
nrow(daily_time_data)
daily_time_data$time <- 1:nrow(daily_time_data)

plot(daily_time_data$time, daily_time_data$time_parking, type = "l", lwd = 2, col = "blue", 
     xlab = "date", ylab = "number of illegal parking per hour", main = "Illegal parking per hour in 2019")
     
     
# illegal parking by month
monthly_data <- complete_oa %>%
  group_by(mon) %>%
  summarise(mon_parking = sum(count))

ggplot(complete_oa, aes(x = mon, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Illegal Parking by Month",
       x = "Month",
       y = "Total Number of Illegal Parking") +
  scale_x_continuous(breaks = 1:12)

ggplot(monthly_data, aes(x = mon, y = mon_parking)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Illegal Parking by Month",
       x = "Month",
       y = "Total Number of Illegal Parking") +
  scale_x_continuous(breaks = 1:12)

#11월 가장 많음, 2월 가장 적음
head(complete_oa)

# illegal parking by day of week

complete_oa$time <- substr(complete_oa$time, 1, 8)
names(complete_oa)[c(3,5)] <- c("date", "day")
names(complete_oa)
complete_oa$weekday <- as.Date(complete_oa$date, format = "%Y%m%d")
complete_oa$weekday <- weekdays(complete_oa$weekday)

complete_oa$day_num <- format(my_date, "%u")
head(complete_oa)
#  월요일은 1 

complete_oa$day_num <- factor(complete_oa$day_num, levels = 1:7, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

ggplot(complete_oa, aes(x = day_num, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Illegal Parking by Day of the Week",
       x = "Day of the week",
       y = "Number of illegal parking") +
  scale_x_discrete(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))



weekday_parking <- complete_oa %>%
  group_by(day_num) %>%
  summarise(day_parking = sum(count))

weekday_parking

# A tibble: 7 x 2
# day_num day_parking
# <fct>         <dbl>
#   1 Mon          71807.
# 2 Tue          73958.
# 3 Wed          73563.
# 4 Thu          73465.
# 5 Fri          75269.
# 6 Sat          67011 
# 7 Sun          69140 


# illegal parking by oa
oa_total <- parking %>%
  group_by(TOT_REG_CD) %>%
  summarise(tot_parking = sum(count))


head(oa_total)
sum(parking$count) # 504213
hist(oa_total$tot_parking)
quantile(oa_total$tot_parking)
quantile(oa_total$tot_parking, prob=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,  0.8, 0.9, 1) )
sort(oa_total$tot_parking, decreasing = T)

# > quantile(oa_total$tot_parking, prob=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,  0.8, 0.9, 1) )
# 10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#   0    0    0    0    0    2    5   15   50 6651 

max(oa_total$tot_parking) # 6652
min(oa_total$tot_parking)


write.csv(oa_total, "oa_total.csv")


oa_total$tot_parking <- as.integer(oa_total$tot_parking)

# illegal parking by hour
parking_hour <- parking %>%
  group_by(hour) %>%
  summarise(hour_parking = sum(count))
parking_hour <- parking_hour[c(1:24),]

       
ggplot(parking_hour, aes(x = hour, y = hour_parking)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Illegal Parking by Hour",
       x = "Hour",
       y = "Number of illegal parking") + 
  scale_x_continuous(breaks = 0:23)

as.data.frame(parking_hour)
  
# > as.data.frame(parking_hour)
# hour hour_parking
# 1     0        11757
# 2     1         7428
# 3     2         3959
# 4     3         2512
# 5     4         1503
# 6     5         2221
# 7     6         1939
# 8     7         7656
# 9     8        21730
# 10    9        27747
# 11   10        37710
# 12   11        25337
# 13   12        22440
# 14   13        41054
# 15   14        38074
# 16   15        31810
# 17   16        42412
# 18   17        34571
# 19   18        22952
# 20   19        31010
# 21   20        40041
# 22   21        26198
# 23   22         9206
# 24   23        12946

#for choropleth map
parking
head(parking)
unique(complete_oa$day_type)
parking_mon_pivot
complete_oa$day_type <- ifelse(complete_oa$day_num %in% c("Sat", "Sun"), "weekend", "weekday")

parking_mon_pivot <- dcast(complete_oa,  TOT_REG_CD ~ mon, sum, value.var = "count")
parking_week_pivot <- dcast(complete_oa,  TOT_REG_CD ~ day_type, sum, value.var = "count")

oa_merge <- merge(oa,parking_mon_pivot, by = 'TOT_REG_CD' )
oa_merge <- merge(oa_merge,parking_week_pivot, by = 'TOT_REG_CD' )
names(oa_merge@data)[4:15] <- c(month.abb)
names(oa_merge@data)

oa_parking <- oa_merge


#########인구####################
setwd(choose.dir())
setwd("C:/Users/user/Desktop/yr/연구실 논문/data/인구")
dir <-  "C:/Users/user/Desktop/yr/연구실 논문/data/인구"
folder_dir <- list.dirs(dir)[2:13]

setwd(paste0(dir, "/LOCAL_PEOPLE_201901"))
list <- list.files()
length(list)
a <- read.csv(list[1], header = T)
# 기준일ID	시간대구분	행정동코드	집계구코드	총생활인구수	
# 남자0세부터9세생활인구수	남자10세부터14세생활인구수	
# 남자15세부터19세생활인구수	남자20세부터24세생활인구수	
# 남자25세부터29세생활인구수	남자30세부터34세생활인구수	남자35세부터39세생활인구수
# 남자40세부터44세생활인구수	남자45세부터49세생활인구수	남자50세부터54세생활인구수	
# 남자55세부터59세생활인구수	남자60세부터64세생활인구수	남자65세부터69세생활인구수
# 남자70세이상생활인구수	여자0세부터9세생활인구수	여자10세부터14세생활인구수	
# 여자15세부터19세생활인구수	여자20세부터24세생활인구수	여자25세부터29세생활인구수
# 여자30세부터34세생활인구수	여자35세부터39세생활인구수	여자40세부터44세생활인구수	
# 여자45세부터49세생활인구수	여자50세부터54세생활인구수	여자55세부터59세생활인구수	
# 여자60세부터64세생활인구수	여자65세부터69세생활인구수	여자70세이상생활인구수
df_final <- data.frame()
df_list <- list()

for (j in 1:12){
  
  setwd(folder_dir[j])
  list <- list.files()
  
  df1 <- data.frame()
  df2 <- data.frame()
  
  for ( i in list){
    df1 <- read.csv(i, skip = 1, header = F, stringsAsFactors = F) 
    df1 <- df1[, 1:5]
    df1[,5] <- as.numeric(df1[,5])
    
    df2 <- rbind(df2, df1)
    }
  
  df_list[j] <- df2
  df_final <- rbind(df_final, df2)
  
  print(i)
}

df_final
setwd( "C:/Users/user/Desktop/yr/연구실 논문/data")
write.csv(df_final, "집계구별 생활인구.csv")
##########################요일별 생활인구 집계##################################
names(df_final) <- c("date", "hour", "dong", "oa", "pop")

names(df_final)

defacto_time <- df_final %>%
  group_by(hour) %>%
  summarise(hour_pop = sum(pop))

df_final$date2 <- as.Date(as.character(df_final$date), format = "%Y%m%d")

df_final$day_num <- format(df_final$date2, "%u")

#  월요일은 1 

#df_final$weekday <- weekdays(my_date)


df_final$day_num <- factor(df_final$day_num, levels = 1:7, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

defacto_days <- df_final %>%
  group_by(day_num) %>%
  summarise(day_pop = sum(pop))


ggplot(defacto_days, aes(x = day_num, y = day_pop)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of de facto Population by Day of the Week",
       x = "Day of the week",
       y = "Number of de facto Population") +
  scale_x_discrete(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


#일별 생활인구
daily_pop <- df_final %>%
  group_by(date) %>%
  summarise(daily_pop = sum(pop))



head(daily_pop)
length(daily_pop$date)
daily_pop$date <- 1:352

plot(daily_pop$date, daily_pop$daily_pop, type = "l", lwd = 2, col = "blue", 
     xlab = "date", ylab = "number of illegal parking per day", main = "Number of de facto Population per day in 2019")

#############월별 생활인구 집계##############



head(df_final)
df_final$mon <- substr(df_final$date, 5,6)


defacto_mons <- df_final %>%
  group_by(mon) %>%
  summarise(mon_pop = sum(pop))


ggplot(defacto_mons, aes(x = mon, y = mon_pop)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of de facto Population by Month",
       x = "Month",
       y = "Number of de facto Population") +
  scale_x_discrete(breaks = 1:12)

ggplot(defacto_mons, aes(x = mon, y = mon_pop)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of de facto Population by Month",
       x = "Month",
       y = "Number of de facto Population") +
  scale_x_discrete(labels = 1:12)


library(reshape2)
head(df_final)
pivot_table <- dcast(df_final,  oa ~ mon, sum, value.var = "pop")
pop_mon <- pivot_table
pop_week <- dcast(df_final,  oa ~ day_type, sum, value.var = "pop")
  
df_final$day_type <- ifelse(df_final$day_num %in% c("Sat", "Sun"), "weekend", "weekday")

names(pop_mon) <- c("TOT_REG_CD", month.abb)
names(pop_week)[1] <- c("TOT_REG_CD")

oa_pop <- merge(oa, pop_mon, by = "TOT_REG_CD" )
oa_pop <- merge(oa_pop, pop_week, by = "TOT_REG_CD" )

writeOGR(oa_pop, dsn = 'C:/Users/user/Desktop/yr/연구실 논문/data', layer = 'oa_pop', driver = "ESRI Shapefile" )
writeOGR(oa_parking, dsn = 'C:/Users/user/Desktop/yr/연구실 논문/data', layer = 'oa_parking', driver = "ESRI Shapefile" )
save.image("illegal_parking")

#######################집계구별 생활인구 집계#################################
names(df_final)
defacto_oa_tot <- df_final %>%
  group_by(oa) %>%
  summarise(tot_pop = sum(pop))


head(defacto_oa_tot)
sum(defacto_oa_tot$tot_pop) # 91029731913
hist(defacto_oa_tot$tot_pop)
quantile(defacto_oa_tot$tot_pop)
quantile(defacto_oa_tot$tot_pop, prob=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,  0.8, 0.9, 1) )

write.csv(defacto_oa_tot, "집계구별 생활인구.csv")
############집계구별 조인##########
oa_total
defacto_oa_tot
names(defacto_oa_tot) <- c("TOT_REG_CD", "tot_pop")
parking_pop <- merge(oa_total, defacto_oa_tot, by = "TOT_REG_CD")
plot(parking_pop$tot_pop, parking_pop$tot_parking)

ggplot() +
  geom_point(mapping=aes(x=tot_pop, y=tot_parking), data=parking_pop)+
  labs(title = "the Number of illegal parking by de facto Population",
       x = "Number of illegal parking",
       y = "Number of de facto Population")



ggplot(defacto_mons, aes(x = mon, y = mon_pop)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of de facto Population by Month",
       x = "Month",
       y = "Number of de facto Population") +
  scale_x_discrete(labels = 1:12)

################################# 불법주정차 단계구분도############
library(rgdal)
library(RColorBrewer)
library(classInt)

# oa : shapefile
# oa_parking
# 월별 12개, 주중 주말별 2개, (시간대별 24개)

month.name[1]
paste0("Number of illegal parking by oa in ", month.name[1])

jan.groups <- classIntervals(oa_parking@data$Jan, 5, style = "jenks")
jan.colors <- findColours(jan.groups, brewer.pal(5, "Reds"))
plot(oa, col = jan.colors, border = "NA",
     main = "Number of illegal parking by oa in January")

legend.text <- vector()
for (i in 1:(length(jan.groups$brks)-1)) {
  legend.text[i] <-
    paste(as.integer(jan.groups$brks)[i:(i+1)], collapse = " - ")
}
legend("topleft", fill = attr(jan.colors, "palette"),
       legend = legend.text, border = NA, bg = "gray90",
       box.col = "gray")



oa_parking@data$Jan[is.na(oa_parking@data$Jan)] <- 0

data
names(oa_parking@data)[15]
colSums(is.na(oa_parking@data))
oa_parking@data[,4]


names(oa_parking@data)[4]
filename <- paste0("plot_", names(oa_parking@data)[i], ".jpeg")

for (i in 4: 15){
 
  filename <- paste0("plot_", names(oa_parking@data)[i], ".jpeg")
  jpeg(filename)
  
  groups <- classIntervals(oa_parking@data[,i], 5, style = "jenks")
  colors <- findColours(groups, brewer.pal(5, "Reds"))
  plot(oa, col = colors, border = "NA",
       main = paste0("Number of illegal parking by oa in ", month.name[i-3]))
  
  legend.text <- vector()
  for (j in 1:(length(groups$brks)-1)) {
    legend.text[j] <-
      paste(as.integer(groups$brks)[j:(j+1)], collapse = " - ")
  }
  legend("topleft", fill = attr(colors, "palette"),
         legend = legend.text, border = NA, bg = "gray90",
         box.col = "gray")
  
  dev.off()
  
}


########################주중주말 단계구분도#############
# 주중

sum(is.na(oa_parking@data$weekday))



groups <- classIntervals(oa_parking@data$weekday, 5, style = "jenks")
colors <- findColours(groups, brewer.pal(5, "Reds"))
plot(oa, col = colors, border = "NA",
     main = "Number of illegal parking by oa on weekdays")

legend.text <- vector()
for (i in 1:(length(groups$brks)-1)) {
  legend.text[i] <-
    paste(as.integer(groups$brks)[i:(i+1)], collapse = " - ")
}
legend("topleft", fill = attr(colors, "palette"),
       legend = legend.text, border = NA, bg = "gray90",
       box.col = "gray")

# 주말


groups <- classIntervals(oa_parking@data$weekend, 5, style = "jenks")
colors <- findColours(groups, brewer.pal(5, "Reds"))
plot(oa, col = colors, border = "NA",
     main = "Number of illegal parking by oa on weekends")

legend.text <- vector()
for (i in 1:(length(groups$brks)-1)) {
  legend.text[i] <-
    paste(as.integer(groups$brks)[i:(i+1)], collapse = " - ")
}
legend("topleft", fill = attr(colors, "palette"),
       legend = legend.text, border = NA, bg = "gray90",
       box.col = "gray")


#########불법 주정차 공간자기상관 측정###########
library(spdep)
nb.q <- poly2nb(oa, queen = T)
head(oa_total)
moran.mc(oa_total$tot_parking, nb2listw(nb.q, zero.policy = T), nsim = 99, zero.policy = T )

chr_oa_parking<- oa_parking

chr_oa_parking$weekday[is.na(chr_oa_parking$weekday)] <- 0
moran.mc(chr_oa_parking$weekday, nb2listw(nb.q, zero.policy = T), nsim = 99, zero.policy = T )
chr_oa_parking$weekend[is.na(chr_oa_parking$weekend)] <- 0
moran.mc(chr_oa_parking$weekend, nb2listw(nb.q, zero.policy = T), nsim = 99, zero.policy = T )

chr_oa_parking$G_tot <- as.numeric(localG(oa_total$tot_parking, nb2listw(nb.q, zero.policy = T), zero.policy=T))
chr_oa_parking$G_week <- as.numeric(localG(chr_oa_parking$weekday, nb2listw(nb.q, zero.policy = T), zero.policy=T))
chr_oa_parking$G_wnd <- as.numeric(localG(chr_oa_parking$weekend, nb2listw(nb.q, zero.policy = T), zero.policy=T))
chr_oa_parking

writeOGR(chr_oa_parking, dsn = 'C:/Users/user/Desktop/yr/연구실 논문/data', 
         layer = 'parking_localg', driver = "ESRI Shapefile",overwrite_layer = T  )

#local moran's I
# 연간 집계 
tot.lI <- localmoran(oa_total$tot_parking,nb2listw(nb.q, zero.policy = T), zero.policy = T)

lI.groups <- classIntervals(tot.lI[,1], 5, style = "quantile")
lI.colors <- findColours(lI.groups, brewer.pal(5, "PRGn"))
plot(oa, col = lI.colors, border = NA,
     main = "집계구별 집계 Local Moran's I")
legend.text <- vector()
for (i in 1:(length(lI.groups$brks)-1)) {
  legend.text[i] <-
    paste(round(lI.groups$brks, 2)[i:(i+1)], collapse = " - ")
}
legend("bottomleft", fill = attr(lI.colors, "palette"),
       legend = legend.text, border = NA, box.col = NA)

# 주중 집계 

week.lI <- localmoran(chr_oa_parking$weekday, nb2listw(nb.q, zero.policy = T), zero.policy = T)

lI.groups <- classIntervals(week.lI[,1], 5, style = "quantile")
lI.colors <- findColours(lI.groups, brewer.pal(5, "PRGn"))
plot(oa, col = lI.colors, border = NA,
     main = "주중 불법 주정차 건수 Local Moran's I")
legend.text <- vector()
for (i in 1:(length(lI.groups$brks)-1)) {
  legend.text[i] <-
    paste(round(lI.groups$brks, 2)[i:(i+1)], collapse = " - ")
}
legend("bottomleft", fill = attr(lI.colors, "palette"),
       legend = legend.text, border = NA, box.col = NA)
# 주말 집계 

wnd.lI <- localmoran(chr_oa_parking$weekend, nb2listw(nb.q, zero.policy = T), zero.policy = T)

lI.groups <- classIntervals(wnd.lI[,1], 5, style = "quantile")
lI.colors <- findColours(lI.groups, brewer.pal(5, "PRGn"))
plot(oa, col = lI.colors, border = NA,
     main = "주말 불법 주정차 건수 Local Moran's I")
legend.text <- vector()
for (i in 1:(length(lI.groups$brks)-1)) {
  legend.text[i] <-
    paste(round(lI.groups$brks, 2)[i:(i+1)], collapse = " - ")
}
legend("bottomleft", fill = attr(lI.colors, "palette"),
       legend = legend.text, border = NA, box.col = NA)

##############생활인구 단계구분도#################