#Importing game transactions data:
transactions<-read.csv(file = "transactions.csv")
getwd()
class(transactions)
head(transactions, 10) 
summary(transactions)
str(transactions)

#Creating a separate data set for data cleaning, keeping our original data set intact
transactions_clean<-transactions

#Removing some columns which are not relevant to our analysis
transactions_clean$developer_user_id<-NULL
View(transactions_clean)
transactions_clean$gan_screen_id<-NULL
transactions_clean$X_synced_at<-NULL

#Formatting the date column into separate columns for date and time
transactions_clean$date_time<-transactions$occured_at
transactions_clean$date<-as.Date(transactions_clean$date_time)
transactions_clean$time<-format(as.POSIXct(transactions_clean$date_time), format="%H:%M:%S")
transactions_clean$date_time<-NULL 
transactions_clean$occured_at<-NULL
names(transactions_clean)


#Converting empty strings in "validated" to 0 and changing the data type to int
transactions_clean$validated<-ifelse(is.na(transactions_clean$validated),0,transactions_clean$validated)
transactions_clean$validated<-as.factor(transactions_clean$validated)

#Device platform also needs to be converted to a factor variable. It contains two variables, iOS and Android and we will conduct analysis on these later
transactions_clean$device_platform<-as.factor(transactions_clean$device_platform)

#We only need data entries for "Crush Food games", as we are conducting our analysis on those. Hence we remove "Story Kitchen Diary" games
library(dplyr)
transactions_clean<-filter(transactions_clean, grepl('crush.food.games',app_id))
  
#Checking the total revenue we get from each country
transactions_clean$country_name<-as.factor(transactions_clean$country_name)
transactions_clean%>%
  group_by(country_tier)%>%
  summarize(revenue=sum(iap_revenue_usd))

#Creating a separate column which computes willingess to purchase. If purchase>20, we assume willingness to be high
transactions_clean<-transactions_clean%>%
  mutate(willingness_to_purchase=case_when(iap_revenue_usd>10~"High",
         iap_revenue_usd<10~"Low"))
transactions_clean$willingness_to_purchase<-as.factor(transactions_clean$willingness_to_purchase)

install.packages("raster")
library(raster)
#Cleaning the install sources column
transactions_clean$install_source<-as.factor(transactions_clean$install_source)
Sources1 <- c(transactions_clean$install_source) 
transactions_clean$install_source <-recode(Sources1,com.android.vending = "Google Play Store",com.sec.android.easyMover = "Google Play Store",  
                                        manual_install = "Google Play Store",  com.oppo.market = "Oppo App Store", com.huawei.appmarket = "Huawei App Store", 
                                        com.lenovo.anyshare.gps = "Google Play Store")

#Creating a data set which includes information for each tier
tier_data<-transactions_clean%>%
  group_by(country_tier, country_name)%>%
  summarise(country_name_count=n(), revenue=round(sum(iap_revenue_usd),2), revenue_per_count=revenue/country_name_count)%>%
  arrange(desc(revenue))
View(tier_data)
tier_data$country_tier<-as.factor(tier_data$country_tier)
tier_data$country_name<-as.character(tier_data$country_name)

#Creating a data set which includes information for each user
each_userdata<-transactions_clean%>%
  group_by(user_id,country_name,country_tier,user_currently_active,device_category, install_source, willingness_to_purchase,
           user_lifetime_seconds_in_app, user_lifetime_number_of_purchases, user_age_days, iap_revenue_user_currency, user_currency, device_platform)%>%
  summarise(total_purchases_usd= sum(iap_revenue_usd),count_total_purchases=n(), total_session_time_seconds=sum(session_time_seconds),total_ads_watched= sum(ads_watched_todate),
            total_games_played= sum(games_played_todate))%>%
  arrange(desc(total_purchases_usd))

each_userdata<-each_userdata[!duplicated(each_userdata$user_id),] #removing any duplicated user IDs
View(each_userdata)

#Cleaning the install sources column
Sources1 <- c(each_userdata$install_source) 
each_userdata$install_source  <- recode(Sources1,com.android.vending = "Google Play Store",com.sec.android.easyMover = "Google Play Store",  
                                        manual_install = "Google Play Store",  com.oppo.market = "Oppo App Store", com.huawei.appmarket = "Huawei App Store", 
                                        com.lenovo.anyshare.gps = "Google Play Store")

#Fomatting each user data set
each_userdata[each_userdata==''] <- NA
each_userdata<-na.omit(each_userdata)
each_userdata$user_id<-as.factor(each_userdata$user_id)
each_userdata$country_name<-as.factor(each_userdata$country_name)
each_userdata$country_tier[each_userdata$country_tier==''] <- NA
each_userdata$country_tier<-as.factor(each_userdata$country_tier)
each_userdata$user_currently_active[each_userdata$user_currently_active==''] <- NA
each_userdata$user_currently_active<-as.factor(each_userdata$user_currently_active)
each_userdata$device_category[each_userdata$device_category==''] <- NA
each_userdata$device_category<-as.factor(each_userdata$device_category)
each_userdata$user_currency<-as.factor(each_userdata$user_currency)
each_userdata$install_source <- as.factor(each_userdata$install_source)
unique(each_userdata$install_source)
is.na(each_userdata)
names(each_userdata)
as.factor(each_userdata$user_id)

#Checking the correlation of variables in our User data
library(corrplot)
install.packages("caret")
library(caret)
continuous_variables<-c(8,9,11,14,15,16,17,18)
cor(each_userdata[ ,continuous_variables],use="pairwise.complete.obs")
corrplot(cor(each_userdata[ ,continuous_variables],use="pairwise.complete.obs"))

#Checking the correlation of variables in our User data
library(corrplot)
install.packages("caret")
library(caret)
continuous_variables<-c(8,9,11,14,15,16,17,18)
cor(each_userdata[ ,continuous_variables],use="pairwise.complete.obs")
corrplot(cor(each_userdata[ ,continuous_variables],use="pairwise.complete.obs"))

tier_data$country_name<-ifelse(tier_data$country_name=="United States","USA",tier_data$country_name)
tier_data$country_name<-ifelse(tier_data$country_name=="United Kingdom","UK",tier_data$country_name)
tier_data$country_name<-as.factor(tier_data$country_name)
install.packages("maps")
library(maps)
library(ggplot2)
world_map <- map_data("world")
View(world_map)
#map.world_joined <- left_join(world_map, tier_data, by = c('region' = 'country_name'))
#map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
#points(map.world_joined$region, aes(x=long, y=lat), col=tier_data$country_tier, pch=16)

library(ggplot2)
# did android or ios users make more purchases
ggplot(each_userdata,aes(x = device_platform, y= total_purchases_usd)) +
  geom_boxplot()
ggplot(each_userdata,aes(x = device_platform, y= total_purchases_usd,)) +
  geom_col(aes(fill= country_tier))
# which tier generates most revenue 
ggplot(tier_data, aes(x = country_tier,y = revenue, colour = "blue")) +
  geom_col() +
  labs(title = "Revenue contribution per tier", x= "Tier", y= "Total revenue")+
  theme_bw()
# how much time do android and ios users spend on a session and which tier they belong to 
ggplot(each_userdata,aes(x = device_platform, y = total_session_time_seconds))+
  geom_col(aes(fill = country_tier)) +
  labs(title = "Time Spent on a session by Android and IOS users", x= "Device Platform", y= "Total Time Spent on a Session")+
  theme_bw()

# Top 10 performing countries in each tier 
tier3data <- tier_data %>% 
  filter(country_tier == "Tier 3")

tier3data <- tier3data %>%
  group_by(country_name)%>%
  arrange(desc(revenue))

tier3data <- tier3data[1:10,]


ggplot(tier3data, aes(x = reorder(country_name,-revenue), y= revenue))+
  geom_col(aes(fill = "lightblue"))+
  labs(title = "Top 10 revenue contributing countries in tier 3", x= "Country name", y= "Revenue")+
  theme_bw()

tier2data <- tier_data %>%
  filter(country_tier == "Tier 2") 

tier2data <- tier2data %>%
  group_by(country_name)%>%
  arrange(desc(revenue))

tier2data <- tier2data[1:10,]
ggplot(tier2data, aes(x= reorder(country_name,-revenue), y= revenue))+
  geom_col(aes(fill = "lightblue"))+
  labs(title = "Top 10 revenue contributing countries in tier 2", x= "Country name", y= "Revenue")+
  theme_bw()

tier1data <- tier_data %>%
  filter(country_tier == "Tier 1")


ggplot(tier1data, aes(x= reorder(country_name,-revenue), y= revenue))+
  geom_col(aes(fill = "lightblue"))+
  labs(title = "Top 10 revenue contributing countries in tier 1", x= "Country name", y= "Revenue")+
  theme_bw()  


#top 10 revenue generating countries across all tiers

tier_datanew <- tier_data %>%
  group_by(country_name)%>%
  arrange(desc(revenue))
tier_datanew <- tier_datanew[1:10,]
ggplot(tier_datanew, aes(x = country_name, y = revenue)) +
  geom_col(aes(fill = country_tier))
theme_bw()

#time series plot for revenue generated by top 10 revenue generating countries from January to March

transactions_clean$month<- format(as.Date(transactions_clean$date, format="%Y-%m-%d"),"%m")

View(transactions_clean)

library(dplyr)

country_revenue_month <- transactions_clean %>%
  group_by(country_name,month) %>%
  summarise(revenue =sum(iap_revenue_usd)) 

View(country_revenue_month)

country_revenue_month$month <- as.factor(country_revenue_month$month)

country_revenue_month %>%
  group_by(country_name, revenue, month) %>%
  arrange(desc(revenue))

View(country_revenue_month)

country_revenue_month <- country_revenue_month %>%
  select(country_name, revenue, month) %>%
  filter(country_name %in% c("United States","United Kingdom", "Japan", "Germany", "France", "Cananda", "Australia", "Italy", "Spain", "Brazil"))

View(country_revenue_month)


library(ggplot2)

ggplot(country_revenue_month, aes(x = month, y = revenue, group= country_name)) +
  geom_line(aes(color=country_name))


#revenue per count for top 10 revenue generating countries 

ggplot(tier_datanew, aes(x= reorder(country_name,-revenue_per_count), y= revenue_per_count))+
  geom_jitter()+
  labs(x= "Country name", y= "Revenue per count")+
  theme_bw()


#proportion of different devices used to play cooking crush games

ggplot(each_userdata, aes(x= device_category))+
  geom_bar()+ 
  labs(title = "Devices used to play game", x= "Type of Device", y= "Count")+
  theme_bw()+
  coord_flip( )  

# whether top 10 performing countries used Andriod or IOS 
# top 10 countries taken from tier_datanew 

Top10overall <- c("United States", "Germany", "United Kingdom", "Japan", "France", "Canada", "Australia", "Italy", "Spain", "Brazil")

userdata_top10 <- each_userdata %>% 
  group_by(country_name) %>%
  filter(country_name %in% Top10overall)

ggplot(userdata_top10, aes(x = device_platform, y = country_name)) +
  geom_count(aes(color = ..n..))+
  theme_bw()+
  coord_flip( ) +
  scale_x_discrete(guide=guide_axis(n.dodge=5))+
  labs (title = "Platforms used to download the game in Top 10 performing countries", x = "Type of Platform", y = "Country Name")
# which platforms were used most for top 10 performing countries of each tier 

Toptier1 <- c("United Sates", "Germany", "United Kingdom", "Japan", "France", "Canada", "Australia")
Toptier2 <- c("Spain", "Brazil", "Greece", "Netherlands", "Russia", "South Korea", "Austria", "South Africa", "Belgium", "Sweden")
Toptier3 <- c("Italy", "Turkey", "Qatar", "Ukraine", "Puerto Rico", "Kazakhstan", "Saudo Arabia", "Chile", "Vietnam", "Peru")

userdata_toptier1 <- each_userdata %>% 
  group_by(country_name) %>%
  filter(country_name %in% Toptier1)

ggplot(userdata_toptier1, aes(x = device_platform, y = country_name)) +
  geom_count(colour = "purple")+
  theme_bw()+
  coord_flip( ) +
  labs (title = "Platforms used to download the game Countries of Tier 1", x = "Type of Platform")

userdata_toptier2 <- each_userdata %>% 
  group_by(country_name) %>%
  filter(country_name %in% Toptier2)

ggplot(userdata_toptier2, aes(x = device_platform, y = country_name)) +
  geom_count(colour = "green")+
  theme_bw()+
  coord_flip( ) +
  labs (title = "Platforms used to download the game in Top 10 countries of Tier 2 ", x = "Type of Platform", y = "Country Name")

userdata_toptier3 <- each_userdata %>% 
  group_by(country_name) %>%
  filter(country_name %in% Toptier3)

ggplot(userdata_toptier3, aes(x = device_platform, y = country_name)) +
  geom_count(colour = "red")+
  theme_bw()+
  coord_flip( ) +
  scale_x_discrete(guide=guide_axis(n.dodge=3))+
  labs (title = "Platforms used to download the game in Top 10 countries of Tier 3 ", x = "Type of Platform", y= "Country Name")



# do top 10 performing countries use google play store or itunes 
as.factor(each_userdata$install_source) 
ggplot(userdata_top10, aes(x= install_source, y = country_name, colour = country_tier))+
  geom_jitter()+
  theme_bw()+
  scale_x_discrete(guide=guide_axis(n.dodge=3))+
  labs (title = "App Store used to download the game in Top 10 perfroming Countries", x = "Type of App Store", y = "Country Name")




# top 250 users to watch most ads - cut off 1080 - for sake of a readable graph 

each_userdata <- each_userdata %>%
  group_by(country_name)%>%
  arrange(desc(total_ads_watched))

eachuser_datatop500 <- each_userdata[1:250,]
as.factor(eachuser_datatop500$country_name)
as.factor(eachuser_datatop500$country_tier)
as.factor(eachuser_datatop500$device_platform)
as.factor(eachuser_datatop500$install_source)
# which countries these top 250 particpants belonged to and what device did theu use
eachuser_datatop500 %>%
  group_by(country_name)%>%
  ggplot(aes(x=country_name))+
  geom_bar(aes(fill = device_platform))+
  scale_x_discrete(guide=guide_axis(n.dodge=3))+
  theme_bw()+
  labs(title = "Countries and Device Platorms of Participants who watched most ads", x = "Country")
# which tier do these countries belong to 
eachuser_datatop500 %>%
  group_by(country_name)%>%
  ggplot(aes(x=country_name, y= country_tier))+
  geom_jitter(aes(colour = country_tier), cex= 3)+
  scale_x_discrete(guide=guide_axis(n.dodge=3))+
  theme_bw()+
  labs(title = "Countries and Tiers of Participants who watched most ads", x = "Country", y= "Tier")

unique(eachuser_datatop500$country_name)

# creating a new coloumn from installation source encoding the install_source column 
each_userdata$install_source <-ifelse(each_userdata$install_source == "com.android.vending" | each_userdata$install_source == "com.sec.android.easyMover"| each_userdata$install_source == "manual_install"|each_userdata$install_source == "com.lenovo.anyshare.gps","Google Play Store",each_userdata$install_source)
each_userdata$Installationsource <-ifelse(each_userdata$install_source == "com.huawei.appmarket","Huawei App Store", each_userdata$install_source)

as.factor(each_userdata$install_source)  
each_userdata$Installationsource <- NULL 
n_distinct(each_userdata$install_source)

#Applying arules to our data set to figure out associations 
#creating a separate data set
library(arulesViz)
#scatter plot with quality of rules
plot(Grules) #play around with changing axes
plot(Grules, method="grouped") #gives bubble chart
plot(Grules, method = "grouped", control = list(k = 10)) # change number of groups


arules_data<-transactions_clean[ ,c('willingness_to_purchase','device_category','country_name','country_tier','install_source')]
arules_data[arules_data==''] <- NA
arules_data<-na.omit(arules_data)
is.na(arules_data)
arules_data$device_category<-as.factor(arules_data$device_category)
arules_data$country_tier<-as.factor(arules_data$country_tier)
View(arules_data)

arules <- as(arules_data, "transactions") # Method to convert. 
class(arules)
summary(arules)

Grules <- apriori(arules)
Grules
summary(Grules)

Grules <- apriori(arules, parameter = list(support = 0.000011, conf= 0.25))
Grules 

summary(Grules)
inspect(Grules[1:41])
Grules

# By: Lift: This will show us highest values of lift at the top. 

Grules <- sort(Grules, by ="lift") 
inspect(Grules[1:10])

# Filter: Number of items

Grules1 <- apriori(arules, parameter = list(support = 0.011, conf= 0.025, maxlen = 3))
Grules1
inspect(Grules1[1:10])

#By Willingness to purchase= High
willingness_subrule <- apriori(arules, parameter = list (support = 0.0000011, conf = 0.25), 
                               appearance = list (rhs = "willingness_to_purchase=High", default = "lhs"))
willingness_subrule
inspect(sort(willingness_subrule, by = "lift"))

itemFrequency(arules)
itemFrequency(arules[,2:5])

itemFrequencyPlot(arules, support = 0.1)
itemFrequencyPlot(arules, topN = 10)

#Decision Tree
View(transactions_clean)
dt<-transactions_clean[,c("country_tier", "device_category", "device_platform",
                     "install_source","games_played_todate","willingness_to_purchase")]
dt[dt==''] <- NA
dt<-na.omit(dt)

dt[,5]<-scale(dt[,5])
install.packages("rpart")
install.packages("rpart.plot")

View(dt)
library(rpart)
library(rpart.plot)
library(ggplot2)

dt$country_tier<-as.factor(dt$country_tier)
dt$device_category<-as.factor(dt$device_category)
str(df)

ggplot(dt, aes(x=willingness_to_purchase))+
  geom_bar()
prop.table(table(dt$willingness_to_purchase))

set.seed(250)
train<-sample(1:nrow(dt), 8659)
train
test<- -train
test

# training and testing data
train.set <- dt[train,]
test.set <- dt[test,]

ggplot(dt, aes(x=willingness_to_purchase))+
  geom_bar()
prop.table(table(train.set$willingness_to_purchase))

train.set.var<-train.set[,1:6]
test.set.var<-test.set[,1:6]

train_class <-train.set$willingness_to_purchase
test_class<- test.set$willingness_to_purchase
test_class
train.set.var

tree_dt <- rpart(willingness_to_purchase~., data=train.set, method='class')
tree_dt
rpart.plot(tree_dt)

tree_pred <- predict(tree_df, test.set, type = 'class')

#Classification confusion matrix (actual vs predicted)
table_mat <- table(test.set$Willingness_To_Bale, tree_pred)
table_mat

rpart.plot(tree_df,type=3,digits=3,fallen.leaves=TRUE)
rpart.plot(tree_df)

accurracy_test <- sum(diag(table_mat) / sum(table_mat))
accurracy_test
