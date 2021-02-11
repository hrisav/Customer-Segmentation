
## Part 1
data = read.delim(file='purchases.txt', header=FALSE, sep='\t', dec='.')
head(data)
summary(data)

colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
data$days_since = round(as.numeric(difftime(time1 = "2016-01-01",
                                      time2 = data$date_of_purchase,
                                      units = "days")))


head(data)
summary(data)
install.packages("sqldf")
library(sqldf)

customers = sqldf("SELECT customer_id, MIN(days_since) AS recency, COUNT(*) AS frequency, AVG(purchase_amount) AS amount FROM data GROUP BY 1")
head(customers)
summary(customers)

hist(customers$recency,col='steelblue1')
hist(customers$frequency,col='rosybrown')
hist(customers$amount, breaks=100,col='tan1')

new_data = customers

# Remove customer id as a variable, store it as row names
head(new_data)
row.names(new_data) = new_data$customer_id
new_data$customer_id = NULL
head(new_data)

# Take the log-transform of the amount, and plot
new_data$amount = log(new_data$amount)
hist(new_data$amount,col='tan1')

# Standardize variables
new_data = scale(new_data)
head(new_data)

#K-means 
set.seed(123)

members = kmeans(x=new_data, centers=4, nstart = 25)

members$cluster
# Ploting K means cluster
install.packages('factoextra')
library(factoextra)
fviz_cluster(members, data = new_data,palette = "npg",
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
,main="K means cluster plot")
table(members$cluster)

df=aggregate(customers[,2:4], by=list(cluster=members$cluster), mean)
df
# computing total revenue
df$revenue=df$frequency*df$amount
df
#boxplot(recency~cluster,data=df)
customers_cluster = cbind(customers, members$cluster)
head(customers_cluster)
boxplot(recency~members$cluster,data=customers_cluster,col = c("green","yellow","purple","red"),names=c("Cluster1","cluster2","cluster3","cluster4"),xlab="cluster",ylab="recency",main="cluster vs recency")
boxplot(frequency~members$cluster,data=customers_cluster,col = c("green","yellow","purple","red"),names=c("Cluster1","cluster2","cluster3","cluster4"),xlab="cluster",ylab="frequency",main="cluster vs frequency")
boxplot(amount~members$cluster,data=customers_cluster,col = c("green","yellow","purple","red"),names=c("Cluster1","cluster2","cluster3","cluster4"),xlab="cluster",ylab="avg_amount",main="cluster vs avg_amount")
summary(customers_cluster)
# computing standard deviation
for (i in 1:5)
{ print(paste('Standard deviation of columns',':',sd(customers_cluster[,i])))
}
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
## Part-2

customers$segment = "Inactive"
customers$segment[which(customers$recency <=365 & customers$frequency > 3 & customers$amount >= 60)] = "Winners"
customers$segment[which(customers$recency <=365 & customers$frequency > 3 & customers$amount < 60)] = "Promising"
customers$segment[which(customers$recency <=365 & customers$frequency <= 3 & customers$amount >= 60)] = "Attention needed"
customers$segment[which(customers$recency <=365 & customers$frequency <= 3 & customers$amount < 60)] = "Loyal"
customers$segment[which(customers$recency > 365 & customers$recency <=365*3 & customers$frequency > 3 & customers$amount >= 60 )] = "Potential"
customers$segment[which(customers$recency > 365 & customers$recency <=365*3 & customers$frequency < 3 & customers$amount >= 60)] = "Extravagant"
customers$segment[which(customers$recency > 365 & customers$recency <=365*3 & customers$frequency > 3 & customers$amount < 60)] = " Inexpensive"
customers$segment[which(customers$recency > 365 & customers$recency <=365*3 & customers$frequency < 3 & customers$amount < 60)] = "Irrevocable"
table(customers$segment)
head(customers,30)
customers[sample(nrow(customers),20),]
aggregate(x = customers[, 2:4], by = list(customers$segment), mean,nrow=20)
