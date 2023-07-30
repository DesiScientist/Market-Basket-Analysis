library(ggplot2)
library(dplyr)
library(scales)
library(tidyverse)
library(corrplot)
library(lubridate)
#install.packages("scales")
#install.packages("tidyverse")
#install.packages("corrplot")
#install.packages("lubridate")
#****************************************** Data Cleaning **************************************************
#Installing Packages
install.packages("dplyr")
library(dplyr)

#Setting and getting working directory
getwd()

#Loading data set
dataset<-read.csv("Dataset.csv")
View(dataset)

#Overview 
summary(dataset)
str(dataset)

#Removing last 5 columns and column increment_id,sales_commission_code,working_date,BI.status,MV,M.Y,FY
dataset<-select(dataset,-c(22:26,13,20,10,14,8,15,19))


#Converting Date and time columns to appropriate formats
#Changing created_at column to date type
dataset$created_at<-as.Date(dataset$created_at,format = "%m/%d/%Y")
#changing Customer.Since column to date type
dataset$Customer.Since<-as.Date(paste("01",dataset$Customer.Since,sep="-"),format = "%d-%Y-%m")

#Changing Cutomer id type to int from chr
dataset$Customer.ID<-as.integer(dataset$Customer.ID)

#Reordering Columns
dataset<-dataset[,c('item_id','Customer.ID','created_at','price','discount_amount','qty_ordered','grand_total','category_name_1','status','payment_method','Month','Year','sku','Customer.Since')]

#Making tables for chr columns
table(dataset$category_name_1)
table(dataset$status)
table(dataset$payment_method)
table(dataset$sku)

#Converting category_name_1,status,, and payment_method to factors as these are categorical variables and only take up certain values
datasrt$category_name_1<-as.factor(dataset$category_name_1)
dataset$status<-as.factor(dataset$status)
dataset$payment_method<-as.factor(dataset$payment_method)
str(dataset)

#Remaking tables for factors
table(dataset$category_name_1)
table(dataset$status)
table(dataset$payment_method)
str(dataset)

#Handling NAs

#NAs in each column
na_count<-colSums(is.na(dataset))
na_count

#Removing NAs
dataset<-na.omit(dataset)

#Removing null characters from category_name, status, payment method,sku
dataset <- subset(dataset,status!='')
dataset <- subset(dataset,payment_method!='')
dataset <- subset(dataset,category_name_1!='')
dataset <- subset(dataset,sku!='')

#Now we have removed all rows with NAs and blank characters

#Feature Engineering (Modifying )

#removing \\N from status as only 4 occurrences
dataset <- subset(dataset,status!='\\N')

#removing \\Others in category_name_1 (look at this later)
dataset <- subset(dataset,category_name_1!='\\N')

#Handling status as there is too much variation and splitting it between complete, refund, and cancelled

# create a named vector of replacements
replace_status <- c('received' = 'complete', 'exchange' = 'complete', 'paid' = 'complete', 'cod' = 'complete',
                    'payment_review' = 'complete', 'pending' = 'complete', 'processing' = 'complete',
                    'refund' = 'order_refunded', 'pending_paypal' = 'order_refunded', 'closed' = 'canceled',
                    'fraud' = 'order_refunded', 'holded' = 'order_refunded','canceled'='canceled','complete'='complete',
                    'order_refunded'='order_refunded')

# replace values in the "status" column of the data
dataset$status <- replace_status[as.character(dataset$status)]
table(data_1$status)

#Handling payment_method as there is too much variation and splitting it between cod, easypaisa, jazzcash, payaxis, alfalah, and other
# create a named vector of replacements
payment_to_replace <- c('cashatdoorstep' = 'cod', 'Easypay_MA' = 'Easypaisa', 'easypay_voucher' = 'Easypaisa', 
                        'jazzvoucher' = 'jazzcash', 'internetbanking' = 'Payaxis', 'mygateway' = 'Payaxis', 
                        'marketingexpense' = 'Payaxis','apg'='other','bankalfalah'='bankalfalah','cod'='cod',
                        'customercredit'='other','financesettlement'='other','mcblite'='other','ublcreditcard'='ublcreditcard',
                        'Easypay'='Easypaisa','jazzwallet'='jazzcash','Payaxis'='Payaxis','productcredit'='other')

# replace values in the "payment_method" column of the data frame "df"
dataset$payment_method <- payment_to_replace[as.character(dataset$payment_method)]

write.csv(dataset,'Cleaned_data.csv',FALSE)


#****************************************** Loading Cleaned Data **************************************************

data<-read.csv("Cleaned_data.csv")

str(data)
View(data)

data <- data %>%
  mutate(total_sales = price * qty_ordered)
data$total_sales<-as.numeric(data$total_sales)

knitr::kable(colnames(data), row.names = FALSE)

#****************************************** EDA **************************************************

# group by year and month, and calculate total sales for each group
sales_by_month <- data %>%
  group_by(Year, Month) %>%
  summarise(total_sales = sum(qty_ordered))
View(sales_by_month)

# sales trends by quantity ordered per month
ggplot(sales_by_month, aes(x = Month, y = total_sales, group = Year, color = factor(Year))) +
  geom_line(size = 1.5) +
  xlab("Month") +
  ylab("Total Sales") +
  ggtitle("Sales Trends by Quantity per Month") +
  scale_color_manual(values = c("deepskyblue", "gold", "orangered")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")+
  scale_x_continuous(breaks = seq(1, 12, by = 1), 
                     labels = month.abb)


#sales per the quantity ordered
ggplot(data, aes(x = Month, y = total_sales/10^6, fill = Year)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "Total Sales per Month by Year", x = "Month", y = "Total Sales *10^6") +
  scale_x_continuous(breaks = seq(1, 12, by = 1), 
                     labels = month.abb)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#scale_y_continuous(labels = comma, limits = c(0, max(data$qty_ordered)))


#sales by category

# group by Year and Category, summarize total sales
yearly_sales_by_category <- data %>%
  group_by(Year, category_name_1, Month) %>%
  summarize(sales_category = sum(total_sales))

# plot total sales by category and year
ggplot(yearly_sales_by_category, aes(x = Year, y = sales_category, fill=category_name_1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Yearly Sales by Category", x = "Year", y = "Total Sales") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top")

#plot total sales by category and month
ggplot(yearly_sales_by_category, aes(x = Month, y = sales_category/10^6, fill=Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Yearly Sales by Category and Months", x = "Year", y = "Total Sales *10^6") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top",  legend.key.size = unit(1, "cm"))+
  scale_x_continuous(breaks = seq(1, 12, by = 1), 
                     labels = month.abb)+
  scale_y_continuous(limits = c(0, 200), labels = comma)

#sales by quantity w.r.t category yearly

sales_by_category_year <- data %>%
  group_by(category_name_1, Year) %>%
  summarise(qty_category_year = sum(qty_ordered)) 

ggplot(sales_by_category_year, aes(x = category_name_1, y = qty_category_year, fill = Year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "Total Sales by Quantity per Category by Year", x = "Category", y = "Total Quantity") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top", legend.key.size = unit(1, "cm")) +  # adjust legend key size) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()


#W.R.T status
library(dplyr)
library(ggplot2)

sales_by_status_year <- data %>%
  group_by(status, Year) %>%
  summarise(sales_status_year = sum(total_sales))

ggplot(sales_by_status_year, aes(x = status, y = sales_status_year, fill = Year)) +
  geom_col(position = "dodge") +
  labs(title = "Total Sales by Status and Year", x = "Status", y = "Total Sales") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ Year, scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#top 10 customers

top10_customers <- data %>%
  group_by(Customer.ID) %>%
  summarise(total_sales_customers = sum(total_sales)) %>%
  arrange(desc(total_sales_customers)) %>%
  head(10)
View(top10_customers)

ggplot(top10_customers, aes(x = reorder(Customer.ID, total_sales_customers), y = total_sales_customers/10^6)) +
  geom_col(fill = "deepskyblue") +
  labs(title = "Top 10 Customers by Total Sales", x = "Customer ID", y = "Total Sales *10^6") +
  coord_flip()

#W.R.T top 10 items by sales


top_items_sales <- data %>%
  group_by(item_id) %>%
  summarise(total_sales_items = sum(total_sales)) %>%
  arrange(desc(total_sales_items)) %>%
  head(10)

ggplot(top_items_sales, aes(x = total_sales_items, y = reorder(item_id, total_sales_items))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Items by Total Sales", x = "Total Sales", y = "Item ID") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 8)) +
  coord_flip()

#W.R.T last 10 items by sales


last_items_sales <- data %>%
  group_by(item_id, category_name_1) %>%
  summarise(total_sales_items_last = sum(total_sales)) %>%
  arrange(desc(total_sales_items_last)) %>%
  tail(100)
View(last_items_sales)

#W.R.T top 10 items by Quantity
top_items_quantity <- data %>%
  group_by(item_id) %>%
  summarise(total_items_quantity = sum(qty_ordered)) %>%
  arrange(desc(total_items_quantity)) %>%
  head(10)

ggplot(top_items_quantity, aes(x = total_items_quantity, y = reorder(item_id, total_items_quantity))) +
  geom_bar(stat = "identity", fill = "beige") +
  labs(title = "Top 10 Items by Total Quantity Ordered", x = "Total Quantity", y = "Item ID") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 8)) +
  coord_flip()

#W.R.T Prices

ggplot(data, aes(x = price, y = total_sales)) +
  geom_point() +
  labs(title = "Sales vs Prices", x = "Price", y = "Total Sales")


#Heat Map
# Calculate total sales by month and year
sales_by_month_year <- data %>%
  group_by(Year, Month) %>%
  summarize(total_sales_month_year = sum(total_sales))

# Create heat map
ggplot(sales_by_month_year, aes(x = Month, y = Year, fill = total_sales_month_year)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Total Sales by Month and Year", x = "Month", y = "Year", fill = "Total Sales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_continuous(breaks = seq(1, 12, by = 1), 
                     labels = month.abb)

#W.R.T Payment methods
sales_payment_method <- data %>%
  group_by(payment_method) %>%
  summarise(total_sales_payment = sum(total_sales))
  

ggplot(sales_payment_method, aes(x = total_sales_payment/10^6, y = reorder(payment_method, total_sales_payment))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Payment Method", x = "Total Sales *10^6", y = "Payment Method") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 8)) +
  coord_flip()

customers <- read.csv("Cleaned_data.csv")

customers <- customers %>%
  drop_na()
customers$created_at <- ymd_hms(customers$created_at)

customers <- customers %>%
  mutate(
    price = as.numeric(price),
    discount_amount = as.numeric(discount_amount),
    qty_ordered = as.numeric(qty_ordered),
    grand_total = as.numeric(grand_total)
  )

#total sales by month
customers %>%
  group_by(Year, Month) %>%
  summarize(total_sales = sum(grand_total)) %>%
  ggplot(aes(x=as.Date(paste(Year, Month, "01", sep="-")), y=total_sales)) +
  geom_line() +
  scale_x_date(date_labels="%b %Y", date_breaks="1 month")
#fluctuating trend, highest sales occuring in nov 2017 and have declined since then. 
#overall increase from 2016-2018 
#total sales by category

total_sales_by_category <- customers %>% 
  group_by(category_name_1) %>% 
  summarise(total_sales = sum(grand_total))

ggplot(total_sales_by_category, aes(x = category_name_1, y = total_sales)) +
  geom_bar(stat = "identity") +
  ggtitle("Total Sales by Category") +
  xlab("Category") +
  ylab("Total Sales")
#mobiles and tablets, appliances, entertainment, womens fashion largest categories contributing to sales.


orders_per_customer <- customers %>%
  group_by(Customer.ID) %>% 
  summarize(orders = n())
ggplot(orders_per_customer, aes(x = orders)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Number of Orders", y = "Number of Customers", title = "Order Frequency by Customer")

#purchase amount and payment method
ggplot(customers, aes(x=payment_method, y=grand_total)) +
  geom_boxplot() +
  xlab("Payment Method") +
  ylab("Grand Total")

#median value of purchase is relatively higher in cod and credit card as compared to easypaisa, the rang of the former 2 is also more indicating more variability. 

#correlation between order value and discount amount
cor(customers$grand_total, customers$discount_amount)
#correlation is not that strong in that the as the orer value increases, discount amount might not increase by a lot, so business should look at other factors  in determining the discount amount such as product category or customer loyalty.

#distribution of order amounts
ggplot(customers, aes(x = grand_total)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.5) +
  scale_x_continuous(labels = scales::dollar) +
  labs(title = "Distribution of Order Amounts", x = "Order Amount", y = "Count")
#mostly orders lie betweeen 0-100 while some are above 500. very less in increased order amounts

#****************************************** ARULES **************************************************
#****************************************** Segmentation Bases on Customer IDs **************************************************
#Segmentation

library(dplyr)
library(cluster)
#Loading data
View(data)

data$status<-as.factor(data$status)
data$payment_method<-as.factor(data$payment_method)

#sampled dataset to 20000 rows as this was the max rows could be taken further required more computation power 
str(data)
set.seed(1)
sampled_data<-data[sample(nrow(data),20000),]
#summaries of both the datasets were compared and they were similar which showed the sample was representative
summary(sampled_data)
summary(data)
#gower method and daisy function were used was used as it automatically calculates distance and makes a dissimilarity matrix for both numeric using euclidean distance and uses manhattan distance for categorical variables
Distance<-daisy(sampled_data[,c(-1:-3,-11:-14)],metric='gower')
Distance
summary(Distance)
#Distance converted to a matrix
distance_matrix<-as.matrix(Distance)
distance_matrix

#PAM method also called kmedioid clustering was used in place of kmeans as it only works for numerical data with a known distribution, and not fit for categorical data
#silouhette score for each respective cluster was calculated and the one with the max was selected
#silouhette score 0.3168614
clust_1<-pam(distance_matrix,diss=T,k=2)
sil_score<-silhouette(clust_1$clustering,distance_matrix)
mean_sil_score_1<-mean(sil_score[,3])
mean_sil_score_1
#silouhette score 0.299449
clust_2<-pam(distance_matrix,diss=T,k=3)
sil_score_1<-silhouette(clust_2$clustering,distance_matrix)
mean_sil_score_2<-mean(sil_score_1[,3])
mean_sil_score_2
#silouhette score 0.2884834
clust_3<-pam(distance_matrix,diss=T,k=4)
sil_score_2<-silhouette(clust_3$clustering,distance_matrix)
mean_sil_score_3<-mean(sil_score_2[,3])
mean_sil_score_3
table(clust_3$clustering)
#silouhette score 0.2991332
clust_4<-pam(distance_matrix,diss=T,k=5)
sil_score_3<-silhouette(clust_4$clustering,distance_matrix)
mean_sil_score_4<-mean(sil_score_3[,3])
mean_sil_score_4
table(clust_4$clustering)
#silouhette score 0.3134589
clust_5<-pam(distance_matrix,diss=T,k=6)
sil_score_5<-silhouette(clust_5$clustering,distance_matrix)
mean_sil_score_5<-mean(sil_score_5[,3])
mean_sil_score_5
table(clust_5$clustering)
#Each cluster size
#   1    2    3    4    5    6 
# 2759 2182 1969 7310 2177 3603
clust_6<-pam(distance_matrix,diss=T,k=7)
sil_score_6<-silhouette(clust_6$clustering,distance_matrix)
mean_sil_score_6<-mean(sil_score_6[,3])
mean_sil_score_6
table(clust_6$clustering)

data_<-cbind(sampled_data,clust_5$clustering)
colnames(data_)[ncol(data_)]<-'PAM_Clusters'
View(data_)

#aggretes for numerical data were calculated
aggregate(data_[,c(4:7)],list(data_$PAM_Clusters),mean)

#mode was used as a measure of central tendency for categorical data
#Mode function was defined
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_category<-aggregate(category_name_1~ PAM_Clusters,data_,Mode)
mode_category

mode_status<-aggregate(status~ PAM_Clusters,data_,Mode)
mode_status

mode_payment<-aggregate(payment_method~ PAM_Clusters,data_,Mode)
mode_payment

#****************************************** Logistic Regression **************************************************

#converting status, order quantity, payment method, category name and month as factors
data$status <- as.factor(data$status)
data$qty_ordered <- as.factor(data$qty_ordered)
data$payment_method <- as.factor(data$payment_method)
data$category_name_1 <- as.factor(data$category_name_1)
data$Month <- as.factor(data$Month)

str(data)

#basic data manipulation would be needed. As logistical regression work as binary variables
#we would convert order completed as '1' and order refunded and cancelled as 0

data <- data %>% 
  mutate(status_new = ifelse(status == 'complete', 1, 0))

#this new variable of status_new would be used which contains binary - 1 for completed, 0 for refunded or cancelled

m1 <- glm(status_new ~ payment_method + category_name_1 + Month + grand_total, data = data, family = binomial)

summary(m1)

exp(coef(m1))

########logistical regression decision tree
#making a new df
data2 <- data %>% 
  select(status_new, payment_method , category_name_1 , Month , grand_total )

set.seed(234)
576481*0.65

#splitting into test and train data

train_index <- sample(1:nrow(data2),374713)
test_index <- -train_index

training_data <- data2[train_index,]
testing_data <- data2[test_index,]

testing_class <- testing_data$status_new

library(rpart)
library(rpart.plot)

logistic_tree <- rpart(status_new~., data = training_data, method='class')

rpart.plot(logistic_tree,type=4,digits=3,fallen.leaves=TRUE)

#checking accuracy
tree_pred1 <- predict(logistic_tree, testing_data[,-5], type = 'class')

test <- testing_data[,1:5]
prediction = predict(logistic_tree, test, type='class')
mean(prediction == testing_class)
mean(prediction != testing_class)


