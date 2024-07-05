library(readxl)
data <- read_excel('Online Retail.xlsx')
#### 1.	Perform a preliminary data inspection and Data cleaning
#a.	Check for missing data and formulate apt strategy to treat them.
#b.	Are there any duplicate data records? Remove them if present.
#c.	Perform Descriptive analytics on the given data.
train <- data
## Structure
str(train)
nrow(train)
ncol(train)

## Null Values
f<- function(x){sum(is.na(x))}
apply(train,2,f)

## Drop Nas

train<-na.omit(train)
nrow(train)

f<- function(x){sum(is.na(x))}
apply(train,2,f)
## Duplicates

print(paste('total duplicates',sum(duplicated(train))))
train <- unique(train)


## Summary 
summary(train)

## Filter data with Negative quantity
train<- train[(train$Quantity>0)&(train$UnitPrice > 0),] 
nrow(train)

#### 2.	Cohort Analysis: A cohort is a group of subjects who share a defining characteristic. We can observe how a cohort behaves across time and compare it to other cohorts. 
#a.	Create month cohorts and analyse active  customers for each cohort.
#b.	Also Analyse the retention rate of customers. Comment.

library(lubridate) ## for month() and year()
f2 <- function(x){paste('1/',month(x),'/',year(x), sep = '')}
train$InvoiceMonth<-sapply(train$InvoiceDate,f2)

train$InvoiceMonth<-as.Date(train$InvoiceMonth, format = '%d/%m/%Y')

library(dplyr)
table <- train %>% 
  group_by(CustomerID) %>%
  summarise(CohortMonth = as.Date(min(InvoiceMonth)))

tb <- as.data.frame(table)
train <- merge(train,tb, by = 'CustomerID')
train
## Cohort index : no of months passed since first purchase
library(zoo)
monnb <- function(d) { 
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

train['CohortIndex'] <- mapply(mondf,train$CohortMonth,train$InvoiceMonth) +1



## Analyse customers 

head(train[,c('CustomerID','CohortIndex','InvoiceMonth','CohortMonth')])
  
# active monthly customers in each cohort
# retention---> % of customers left in the month against what we started

tf<-train%>%
  group_by(CohortIndex,CohortMonth) %>% 
  summarise(count = length(unique(CustomerID)))

tf_1 <- as.data.frame(tf)
library(tidyr)
counts <- tf_1 %>% spread(CohortIndex,sum(count), fill = 0)
head(counts)
rownames(counts)<- counts$CohortMonth
counts[,1]<- NULL

retention <-counts %>% mutate_at(vars(-c(1,2)),funs(round((./counts$`1`)*100,1)))
rownames(retention)<- rownames(counts)
library(ggplot2)
retention[retention ==0.0]<- NA
cc <- rainbow(ncol(as.matrix(retention)), start = 0, end = .3)
rc <- rainbow(nrow(as.matrix(retention)), start = 0, end = .3)
heatmap(as.matrix(retention),Rowv = NA,Colv = NA, col= rainbow(256),
        margins = c(5,10),
        xlab = 'CohortIndex', ylab = 'CohortMonth')


quant<- as.data.frame(train%>%
  group_by(CohortIndex,CohortMonth) %>% 
  summarise(MeanQuant = mean(Quantity)))

head(quant)

quant_pivot<-quant%>%spread(CohortIndex,sum(MeanQuant), fill = NA)

row.names(quant_pivot)<- quant_pivot$CohortMonth
quant_pivot[,1]<- NULL

heatmap(as.matrix(quant_pivot),Rowv = NA,Colv = NA, col= rainbow(256),
        margins = c(5,10),
        xlab = 'CohortIndex', ylab = 'CohortMonth')

# 3.	Build a RFM model – Recency Frequency and Monetary based on their behaviour.
# a.	Calculate RFM metrics.
# i.	Recency as the time in no. of days since last transaction
# ii.	Frequency as  count of purchases done 
# iii.	Monetary value  as total amount spend 
# b.	Build RFM Segments.
# i.	Give Recency Frequency and Monetary scores individually by dividing them in to quartiles.
# ii.	Combine three ratings to get a RFM segment (as strings)
# iii.	Get the RFM score by adding up the three ratings.

date_of_anaylysis <- as.Date(max(train$InvoiceDate)) + 1

train$TotalSum <- train$UnitPrice * train$Quantity

date_diff<- function(d){
  as.integer(as.Date(date_of_anaylysis) - as.Date(d))
}

rfm<-as.data.frame(train %>%
  group_by(CustomerID) %>%
  summarise(Recency = date_diff(max(InvoiceDate)) ,
            Frequency = length(InvoiceNo),
            Monetary_value = sum(TotalSum)
            ))
row.names(rfm)<-rfm$CustomerID
rfm[,1]<-NULL
# To analyse the data we rate the RFM values rather than using absolute...
# So we can decide on the ranges to put in each class by understanding the 
# distribution of the datab

summary(rfm)

rfm$R<-with(rfm,cut(rfm$Recency,
             breaks = quantile(rfm$Recency),labels = c(4,3,2,1),
             include.lowest = TRUE))

rfm$f<-with(rfm,cut(rfm$Frequency,
                    breaks = quantile(rfm$Frequency),
                    labels = c(1,2,3,4),
                    include.lowest = TRUE))

rfm$m<-with(rfm,cut(rfm$Monetary_value,
                    breaks = quantile(rfm$Monetary_value),
                    labels = c(1,2,3,4),
                    include.lowest = TRUE))
head(rfm)

rfm$RFM_Segment <- as.numeric(paste(as.character(rfm$R), as.character(rfm$f),as.character(rfm$m),sep = ""))

 
rfm[,c('R','f','m')]<-apply(rfm[,c('R','f','m')],2,as.numeric)
rfm$RFM_score<- apply(rfm[,c('R','f','m')], MARGIN = 1,sum)
head(rfm)

rfm <- subset(rfm , select = -c(R,f,m))

head(rfm)

rfm_analysis <- as.data.frame(rfm %>% 
  group_by(RFM_score)%>%
 summarise(size = length(Recency),r = mean(Recency),f = mean(Frequency),m = mean(Monetary_value)))


seg <- function(score){
  ifelse(score>=10,'Platinum',ifelse(score>=7,'Gold','Silver'))
}
rfm$cust_seg <- sapply(rfm$RFM_score,seg)

  cust_segmen<- as.data.frame(rfm%>%
  group_by(cust_seg) %>%
  summarise(size = length(Recency),r = mean(Recency),
            f = mean(Frequency),m = mean(Monetary_value))
)

##### 4.	Create clusters using k means clustering algorithm.
 # a.	Prepare the data for the algorithm.
#  i.	If the data is Un Symmetrically distributed, manage the skewness with appropriate transformation.
 # ii.	Standardize / scale the data.
#  b.	Decide the optimum number of clusters to be formed
#  c.	Analyse these clusters and comment on the results
 
library(NbClust)
data_an <- rfm[,c("Recency"  , "Frequency","Monetary_value")]
 
summary(data_an) 
var <- c("Recency"  , "Frequency","Monetary_value")
for (i in var){
print(ggplot(data_an, aes_string(x = i))+
  geom_histogram(aes(y = ..density..), alpha=.5,
                 position="identity" ,fill = 'skyblue'      
  ) +
  geom_density(color = 'blue'))
}

## Log Transformation

data_log<-as.data.frame(sapply(data_an,log))
head(data_log)


var <- c("Recency"  , "Frequency","Monetary_value")
for (i in var){
  print(ggplot(data_log, aes_string(x = i))+
          geom_histogram(aes(y = ..density..), alpha=.5,
                         position="identity" ,fill = 'skyblue'      
          ) +
          geom_density(color = 'blue'))
}

## Scale the data

data_std<-as.data.frame(sapply(data_log,scale))
library(NbClust)

set.seed(1234)
## find optimum no. of clusters
nc <- NbClust(data_std, min.nc=2, max.nc=8, method="kmeans")
nc
table(nc$Best.nc[1,])
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria",
)

kmeans.clus <- kmeans(x=data_std, centers = 3)
data_an['Clusters'] <- kmeans.clus$cluster

## plotting the clusters
library(cluster)
clusplot(data_std, kmeans.clus$cluster, 
         color=TRUE, labels=2, lines=1)


aggr <- aggregate(data_an,list(data_an$Clusters),mean)

clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(data_an$Clusters)),
                            aggr[,2:4]
                            )
names(data_an)

clus_avg <- aggregate(data_an[,1:3],list(data_an$Clusters),mean)
avg <- sapply(data_an[,1:3],mean)
rel <- clus_avg[,-1] /avg
rel <-as.data.frame(rel)
rel<-sapply(rel,round,2)
row.names(rel) <- paste('Cluster',clus_avg[,1])

##########
tot_avg<-sapply(rfm[,1:3],mean)
clsu_avg_2 <- aggregate(rfm[,1:3],list(rfm$cust_seg),mean)
rel_ <- clus_avg[,-1]/tot_avg -1
rel_ <- as.data.frame(sapply(rel_[,1:3],round,2))
row.names(rel_) <- clus_avg[,1]

#####
library(RColorBrewer)
par(mfrow = c(2,1))
coul = colorRampPalette(brewer.pal(9, "Blues"))
corrplot(t(rel[,-4]),is.corr=FALSE, col = coul(220), 
         method = 'color', number.cex = 0.7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90,tl.cex = 0.8 )
coul = colorRampPalette(brewer.pal(9, "Reds"))
corrplot(t(rel_),is.corr=FALSE, col = coul(220),
         method = 'color', number.cex = 0.7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90,tl.cex = 0.8, )

