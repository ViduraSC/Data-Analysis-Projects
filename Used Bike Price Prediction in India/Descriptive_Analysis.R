data=read.csv("D:/3RD YEAR/SEMESTER II/ST3082/Project III Materials//Used_Bikes.csv")
View(data)
head(data)
names(data)
summary(data)
str(data)
library(plyr)
library(dplyr)
library(stringr)
library(mgsub)
library(ggplot2)
library(corrplot)
library(psych)
library(packHV)
library(moments)
library(tidyverse)



################################   Pre- processing and Feature Engineering  #######################################

#Removing duplicates
sum(duplicated(data))
data=distinct(data)
nrow(data)
#Replacing "" with NA
data = replace(data, data=='', NA)
colSums(is.na(data))
#Removing records with price=0
data=data[data$price != 0, ] 
data=data[data$kms_driven != 0, ] 
data=data[data$age != 0, ] 
data=data[data$power != 0, ] 
nrow(data)

x=c(table(data$bike_name))
y=c()
for(i in 1:length(x)){
  if(x[i]>50){
    y[i]=names(x[i])
  }else{
    
    y[i]='Others'
  }
  
  
}
y=unique(y)
y=y[-1]

w=c()
for(i in 1:length(y)){
  w[i]=x[y[i]]
}

types=x[match(sort(w,decreasing=T),x)]

#Create city_New column
names(table(data$city))
x=c(table(data$city))
length(x)
y=c()
for(i in 1:length(x)){
  if(x[i]>50){
    y[i]=names(x[i])
  }else{
    
    y[i]='Others'
  }
  
  
}
y=unique(y)
y=y[-1]

w=c()
sum(w)
for(i in 1:length(y)){
  w[i]=x[y[i]]
}

types=x[match(sort(w,decreasing=T),x)]
brand_New=c()
for(i in 1:length(data$brand)){
  if((data$brand[i]=="Ideal")|(data$brand[i]=="Indian")|
     (data$brand[i]=="LML")|(data$brand[i]=="Rajdoot")|
     (data$brand[i]=="Yezdi")|(data$brand[i]=="MV")|(data$brand[i]=="Jawa")){
    brand_New[i]="Others"
    
  }else{
    brand_New[i]=data$brand[i]
  }
  
}
table(brand_New)
data=cbind(data,brand_New)
#Create brand_New column
table(data$brand)
brand_New=c()
for(i in 1:length(data$brand)){
  if((data$brand[i]=="Ideal")|(data$brand[i]=="Indian")|
     (data$brand[i]=="LML")|(data$brand[i]=="Rajdoot")|
     (data$brand[i]=="Yezdi")|(data$brand[i]=="MV")|(data$brand[i]=="Jawa")){
    brand_New[i]="Others"
  
  }else{
    brand_New[i]=data$brand[i]
  }
  
}
table(brand_New)
data=cbind(data,brand_New)
 


#Create the state column
# read in the second file with unique cities and their corresponding state
df= read.csv("D:/3RD YEAR/SEMESTER II/ST3082/Project III Materials//cities.csv")
sum(duplicated(df))
df=distinct(df)
table(df$State)
nrow(df)

state=c()
for(i in 1:nrow(df)){
  for(j in 1:nrow(data)){
  if(data$city[j]==df$city[i]){
    state[j]=df$State[i]
  }else{
    next
  }
  }
}

for(i in 1:length(state)){
  if((state[i]=="Arunachal Pradesh")|(state[i]=="Meghalaya")|
     (state[i]=="Sikkim")){
   state[i]="Others"
  }else{
   state[i]=state[i]
  }
}
table(state)
data=cbind(data,state)
table(data$owner)

#Factoring
data$owner= factor(data$owner,level=c("First Owner","Second Owner","Third Owner",
                                               "Fourth Owner Or More"))
data$brand_New = factor(data$brand_New,level=c("Bajaj","Benelli","BMW","Ducati","Harley-Davidson","Hero","Honda",
                                               "Hyosung","Kawasaki","KTM","Mahindra","Royal Enfield"
                                               ,"Suzuki","Triumph","TVS","Yamaha","Others"))
data$state = factor(data$state,level=c("Andhra Pradesh","Assam","Bihar","Chhattisgarh",
                                       "Delhi","Goa","Gujarat","Haryana",
                                       "Himachal Pradesh" ,"Jammu and Kashmir","Jharkhand", "Karnataka",
                                       "Kerala",    "Madhya Pradesh",       "Maharashtra",            "Odisha",
                                       "Puducherry" ,           "Punjab" ,        "Rajasthan",
                                       "Tamil Nadu",         "Telangana",     "Uttar Pradesh",       "Uttarakhand",
                                       "West Bengal" , "Others"))


str(data)
#Splitting the data in to training and testing sets
set.seed(100)
indexes=sample(1:nrow(data),0.2*nrow(data))
testset=data[indexes,]
trainset=data[-indexes,]
View(trainset)
View(testset)
head(trainset)
head(testset)
nrow(trainset)
nrow(testset)

options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(trainset$price,main="Price distribution",col="#9494b8",xlab="Bike Price(In Indian Rupees)")
boxplot.stats(trainset$price)$stats
x=which(trainset$price %in% boxplot.stats(trainset$price)$out)
length(x)
df1=data %>%  filter(row_number() %in% x)
View(df1)

options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(log(trainset$price),main="log(Price) distribution",col="#9494b8",xlab="log Bike Price(In Indian Rupees)")
boxplot.stats(log(trainset$price))$stats
x=which(log(trainset$price) %in% boxplot.stats(log(trainset$price))$out)
length(x)

#Mean of Delay time
mean(trainset$price)

#Skewness and kurtosis
skewness(trainset$price)#>1 positively skewed
kurtosis(trainset$price)
skewness(log(trainset$price))#>1 positively skewed
kurtosis(log(trainset$price))

tg1 = trainset %>%
  group_by(brand_New) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

ggplot(tg1, aes(x = reorder(brand_New, -count), y = count)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.7, fill = "steelblue") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "Brand Vs Bike Count", x = "Brand", y = "Bike Count") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(trainset, aes(Route_New, fill = Day)) +
  geom_bar(stat="count",position = "dodge") +
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_fill_brewer(palette = "Set1")+
  labs(x="Route type",y="Number of Incidents")

ggplot(trainset,aes(x=brand_New,y=price))+
  geom_boxplot(alpha=0.7,fill="steelblue")+
  #stat_summary(fun=mean,geom="point",shape=20,
  #            size=4,col="dark blue",fill="dark blue")+
  #scale_x_discrete(guide = guide_axis(angle = 90))+
  coord_flip()+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )+
  labs(x="Brand",y="Bike Price(In Indian Rupees)")
 
ggplot(trainset,aes(x=brand_New,y=price ))+
  geom_point(color="")+
  
  theme_minimal()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(x="Brand",y="Bike Price(In Indian Rupees)")

tg2 = ddply(trainset, "brand_New", summarise, price = median(price))

ggplot(tg2, aes(x = brand_New, y = price, group=1)) +
  geom_point(color = "red") +
  geom_line(color = "black") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Brand", y = "Median Bike Price (In Indian Rupees)") +
  theme_minimal()


tg3 = trainset %>%
  group_by(brand_New, owner) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

ggplot(tg3, aes(x = reorder(brand_New, -count), y = count, fill = owner, label = count)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.7) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs( x = "Brand", y = "Bike Count") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )
ggplot(data=trainset,aes(x=brand_New,y=price,fill=owner,color=owner))+
  geom_point()+
  
  theme_minimal()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(x="Brand",y="Bike Price(In Indian Rupees)")

ggplot(data=trainset,aes(x=age,y=price))+
  geom_point(color = "brown")+
  
  theme_minimal()+
  
  labs(x="Age",y="Bike Price(In Indian Rupees)")


ggplot(data=trainset, aes(x=kms_driven, y=price, fill=brand_New, color=brand_New)) +
  geom_point() +
  theme_minimal() +
  labs(x="kilometers driven", y="Bike Price (In Indian Rupees)") 





ggplot(data=trainset,aes(x=kms_driven,y=price,fill=owner,color=owner))+
  geom_point()+
  
  theme_minimal()+
  
  labs(x="kilometers driven",y="Bike Price(In Indian Rupees)")

ggplot(data=trainset,aes(x=power,y=price,fill=brand_New,color=brand_New))+
  geom_point()+
  
  theme_minimal()+
  
  labs(x="power cc",y="Bike Price(In Indian Rupees)")



tg4= trainset %>%
  group_by(power) %>%
  summarize(mean_price = mean(price))

ggplot(data = trainset, aes(x = power, y = price)) +
  geom_point() +
  geom_smooth(data = tg4, aes(x = power, y = mean_price), method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(x = "power cc", y = "Bike Price (In Indian Rupees)")


tg5 = trainset %>%
  group_by(state) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

ggplot(tg5, aes(x = reorder(state, -count), y = count, fill = count)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.7, color = "black") +
  scale_fill_continuous(low = "lightblue", high = "darkblue", 
                        guide = "none", aesthetics = "fill", 
                        breaks = seq(0, max(tg5$count), by = 5)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "State", y = "Bike Count") +
  theme(plot.title = element_text(hjust = 0.5))

tg6 = ddply(trainset, "state", summarise, price = median(price))

ggplot(tg6, aes(x = state, y = price, group=1)) +
  geom_point(color = "red") +
  geom_line(color = "black") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "State", y = "Median Bike Price (In Indian Rupees)") +
  theme_minimal()

#################################PLS###################################################
trainset_new=subset(trainset,select=-c(bike_name,city,brand))

library(mdatools)
library(caret)
dummy_coding= dummyVars(" ~ . ", data = trainset_new)
trainset_encoded=predict(dummy_coding, newdata =trainset_new)

View(trainset_encoded)

x=trainset_encoded[,-1]
x[,c(1,6,7)]=scale(x[,c(1,6,7)], center = TRUE, scale = TRUE)
y=as.matrix(trainset_encoded[,1])
set.seed(100)
ModelPLS = pls(x,y,cv=5, info = "Bike Price prediction")
#view summary of model fitting
summary(ModelPLS)

#visualize CV plots
dev.off()
plot(ModelPLS)
plotXScores(ModelPLS,show.label = TRUE)
plotXYLoadings(ModelPLS,show.label = TRUE)
plotVIPScores(ModelPLS,ncomp=19, type = "h",show.label = TRUE)
summary(ModelPLS$coeffs)
plot(ModelPLS$coeffs,show.label = TRUE)
summary(ModelPLS$res$cal)
#Checking Outliers
Model0=setDistanceLimits(ModelPLS,lim.type = ModelPLS$lim.type,alpha=0.05)
plotXYResiduals(Model0,show.labels=TRUE,labels="indices")
# Identify outlier indices
# get row indices for outliers in calibration set
outliers = which(categorize(Model0, ModelPLS$res$cal) == "outlier")
length(outliers)
trainset_new=trainset_new %>%  filter(!row_number() %in% outliers)

################################### Cluster Check ###################################################################

library(cluster)
library(Rtsne)

#Computing gower distance

df=trainset_new
dummy_coding= dummyVars(" ~ . ", data = df)
df_encoded=predict(dummy_coding, newdata =df)
df_encoded=as.data.frame(df_encoded)
View(df_encoded)
df_encoded[,c(1,2,7,8)]=scale(df_encoded[,c(1,2,7,8)], center = TRUE, scale = TRUE)
df_encoded[,-c(1,2,7,8)] = lapply(df_encoded[,-c(1,2,7,8)], factor)
set.seed(100)
gower_sam1=sample(1:nrow(df),size=1000,replace=FALSE)
df1=df_encoded[gower_sam1,]
gd1=daisy(df1,metric="gower")
gm1=as.matrix(gd1)

df1[which(gm1==min(gm1[gm1!=min(gm1)]),arr.ind = TRUE)[1,],]
df1[which(gm1==max(gm1[gm1!=max(gm1)]),arr.ind = TRUE)[1,],]

#Graph to identify the number of clusters
sil_dist=c(NA)
for(i in 2:10){
  pam_fit1=pam(gd1,diss=TRUE,k=i)
  sil_dist[i]=pam_fit1$silinfo$avg.width
}
dev.off()
plot(1:10,sil_dist,
     xlab = "k",
     ylab="Average silhoutte distance",
     lines(1:10,sil_dist))
################################################################################
#Suggestions for advanced analysis
#Correlation matrix
quantitative= trainset_new %>%
  select(c(age,power,kms_driven))%>%
  as.data.frame()
corr=round(cor(quantitative[,]),2)
library(ggcorrplot)
ggcorrplot(corr,hc.order=F,lab=TRUE)

# create a scatterplot matrix
pairs(quantitative)
library(ggpubr)
library(rstatix)
ggqqplot(trainset, "age", facet.by = "brand_New")
ggqqplot(trainset, "age", facet.by = "owner")
ggqqplot(trainset, "age", facet.by = "state")
ggqqplot(trainset, "power", facet.by = "brand_New")
ggqqplot(trainset, "power", facet.by = "owner")
ggqqplot(trainset, "power", facet.by = "state")
ggqqplot(trainset, "kms_driven", facet.by = "brand_New")
ggqqplot(trainset, "kms_driven", facet.by = "owner")
ggqqplot(trainset, "kms_driven", facet.by = "state")
ggqqplot(trainset, "price", facet.by = "state")
#Since ANOVA normality assumptions are violated we are using Kruskal wallis test
# Performing Kruskal-Wallis test
result = kruskal.test(price ~ state,
                      data = trainset)
print(result)
