data=read.csv("C:/Users/Admin/Desktop/university work/Level 3 sem 2/ST3082 - Statistical Learning - I/Project 01//ttc-bus-delay-data-2022.csv")

View(data)
head(data)
summary(data)
str(data)
library(dplyr)
library(stringr)
library(mgsub)
library(ggplot2)
library(corrplot)
library(psych)
install.packages("packHV")
library(packHV)
sum(duplicated(data))
data=distinct(data)
data <- replace(data, data=='', NA)
colSums(is.na(data))

data=data[data$Min.Delay != 0, ] 
nrow(data)
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


#Create Month column

data = data %>%
  mutate(Month = str_extract(Date, "[:alpha:]+")
         )%>%

  mutate(
    Month = ifelse( str_detect(Date, "Jan"),"January", Month)
  ) %>%

  mutate(
    Month = ifelse( str_detect(Date, "Feb"),"February", Month)
  ) %>%
  mutate(
    Month = ifelse( str_detect(Date, "Mar"),"March", Month)
  ) %>%
  mutate(
    Month = ifelse( str_detect(Date, "Apr"),"April", Month)
  ) %>%
  mutate(
    Month = ifelse( str_detect(Date, "May"),"May", Month)
  ) %>%
  mutate(
    Month = ifelse( str_detect(Date, "Jun"),"June", Month)
  ) 

#Cleaning Direction column
table(data$Direction)
which(data$Direction=="/")
which(data$Direction=="2")
which(data$Direction=="6")
which(data$Direction=="D")
which(data$Direction=="I")
which(data$Direction=="J")
which(data$Direction=="Q")
data=data %>%  filter(!row_number() %in% c(78,19277,21705,6757,11606,154,4310,10421,14092))
nrow(data)

#Create Route_New column
table(data$Location)
x=c(table(data$Route))
which(data$Route=="RAD")
which(data$Route=="OTC")
which(data$Route_New=="Blue Night Routes")
data$Hour[4641]
data$Route[5703]=1000
data$Route[7435]=1000
data$Route[19824]=1000
data$Route[13156]=1001
Route_New=c()

for(i in 1:nrow(data)){
  if((!is.na(data$Route[i]))&& (as.numeric(substr(data$Route[i],1,4))<=189)){
    Route_New[i]="Regular and limited service routes"
    
  }else if((!is.na(data$Route[i]))&&(as.numeric(substr(data$Route[i],1,4))>299)&(as.numeric(substr(data$Route[i],1,4))<400)){
    Route_New[i]="Blue Night Routes "
    
  }else if((!is.na(data$Route[i]))&&(as.numeric(substr(data$Route[i],1,4))>399)&(as.numeric(substr(data$Route[i],1,4))<500)){
    Route_New[i]="Community Routes"
    
  }else if((!is.na(data$Route[i]))&&(as.numeric(substr(data$Route[i],1,4))>899)&(as.numeric(substr(data$Route[i],1,4))<1000)){
    Route_New[i]="Express Routes"
  
  }else if(!is.na(data$Route[i])){
    Route_New[i]="Others"
  }else{
    Route_New[i]=NA
  }
}

data=cbind(data,Route_New)
sum(is.na(Route_New))
table(Route_New)

#Create Hour column

Hour=c()
for(i in 1:length(data$Time)){

    Hour[i]=as.numeric(substr(data$Time[i],1,regexpr(":",data$Time[i])-1))
    

}
as.character(Hour)
data=cbind(data,Hour)
sum(is.na(data$Hour))


#Create Incident_New column
table(data$Incident)
Incident_New=c()
for(i in 1:length(data$Incident)){
  if((data$Incident[i]=="Cleaning - Disinfection")|(data$Incident[i]=="Held By")|(data$Incident[i]=="Late Entering Service")){
      Incident_New[i]="Others"
  }else if(data$Incident[i]=="Road Blocked - NON-TTC Collision"){
      Incident_New[i]="Road Blocked"
  }else{
    Incident_New[i]=data$Incident[i]
  }
  
}
sum(is.na(Incident_New))
data=cbind(data,Incident_New)
Incident_New[59]
sum(is.na(data$Incident_New))
data$Incident[59]
data=cbind(data,Incident_New)




#Create is_Weekday column
is_Weekday=c()
for(i in 1:nrow(data)){
  if((data$Day[i]=="Saturday") | (data$Day[i]=="Sunday") ) {
    is_Weekday[i]="Weekend"
  
  }else{
    is_Weekday[i]="Weekday"
  }
}
is_Weekday
data=cbind(data,is_Weekday)

#Cleaning Data
y=c()

for(i in 1:nrow(data)){
  if((!is.na(data$Route_New[i]))&&( (data$Route_New[i]=="Regular and limited service routes")&(data$is_Weekday[i]=="Weekday")&(data$Hour[i]>1)&(data$Hour[i]<6))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&&( (data$Route_New[i]=="Regular and limited service routes")&(data$is_Weekday[i]=="Weekend")&(data$Hour[i]>1)&(data$Hour[i]<8))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&& ((data$Route_New[i]=="Blue Night Routes ")&(data$is_Weekday[i]=="Weekday")&(data$Hour[i]<1)&(data$Hour[i]>=6))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&& ((data$Route_New[i]=="Blue Night Routes ")&(data$is_Weekday[i]=="Weekend")&(data$Hour[i]<1)&(data$Hour[i]>=8))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&& ((data$Route_New[i]=="Community Routes")&(data$Hour[i]>0)&(data$Hour[i]<6))){
    y=append(y,i)
    
  }else{
    y=append(y,"")
  }
}

y=y[y != ""]
length(y)
as.numeric(y)
data=data %>%  filter(!row_number() %in% as.numeric((y)))

#Create Time_of_the_Day column

RBH=c()
for(i in 1:length(data$Time)){
  if((data$is_Weekday[i]=="Weekday")&(as.numeric(substr(data$Time[i],1,regexpr(":",data$Time[i])-1))>1)&(as.numeric(substr(data$Time[i],1,regexpr(":",data$Time[i])-1))<6)){
    RBH[i]="Others"
    
  }else if(data$is_Weekday[i]=="Weekday"){
    RBH[i]="Regular Bus Hours-Weekday"
    
  }else if((data$is_Weekday[i]=="Weekend")&(as.numeric(substr(data$Time[i],1,regexpr(":",data$Time[i])-1))>1)&(as.numeric(substr(data$Time[i],1,regexpr(":",data$Time[i])-1))<8)){
    RBH[i]="Others"
    
  }else{
   RBH[i]="Regular Bus Hours-Weekend"
  }
  
}
RBH
data=cbind(data,RBH)
sum(is.na(RBH))


#Factoring
data$Route_New = factor(data$Route_New,level=c("Regular and limited service routes","Blue Night Routes ","Community Routes","Express Routes","Others"))
data$Day = factor(data$Day,level=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
data$Month = factor(data$Month,level=c("January","February","March","April","May","June"))
data$Incident_New = factor(data$Incident_New,level=c("Cleaning - Unsanitary","Diversion","General Delay","Mechanical","Security","Vision","Collision - TTC","Emergency Services","Investigation","Operations - Operator","Road Blocked","Utilized Off Route","Others"))
#data$Time_of_the_day= factor(data$Time_of_the_day,level=c("Midnight-6AM","6AM-Noon","Noon-6PM","6PM-Midnight"))
data$Direction=factor(data$Direction,level=c("N","S","E","W","B"))
data$is_Weekday=factor(data$is_Weekday,level=c("Weekday","Weekend"))

data=subset(data,select=-Vehicle)

#Splitting the data in to training and testing
set.seed(100)
indexes=sample(1:nrow(data),0.2*nrow(data))
testset=data[indexes,]
trainset=data[-indexes,]
View(trainset)
head(trainset)
head(testset)
View(testset)
nrow(testset)

#Imputing missing values
colSums(is.na(trainset))

#Mode function.
getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode of Route function.
result1 <- getmode(v=trainset$Route)
print(result1)
# Calculate the mode of Direction function.
result2 <- getmode(v=trainset$Direction)
print(result2)
z1=which(is.na(trainset$Route))
length(z)
h1=c()
w1=c()
for(i in 1:length(z1)){
  h1[i]=trainset$Hour[z1[i]]
  w1[i]=trainset$is_Weekday[z1[i]]
}
z2=which(is.na(testset$Route))
length(z)
h2=c()
w2=c()
for(i in 1:length(z2)){
  h2[i]=trainset$Hour[z2[i]]
  w2[i]=trainset$is_Weekday[z2[i]]
}

trainset$Route[is.na(trainset$Route)] =result1
colSums(is.na(trainset))
trainset$Route_New[is.na(trainset$Route_New)] ="Regular and limited service routes"
colSums(is.na(trainset))
trainset=trainset %>%  filter(!row_number() %in% c(16196,18212))
testset$Route[is.na(testset$Route)] =result1
colSums(is.na(testset))
testset$Route_New[is.na(testset$Route_New)] ="Regular and limited service routes"
trainset$Direction[is.na(trainset$Direction)] =result2
testset$Direction[is.na(testset$Direction)] =result2
colSums(is.na(testset))
str(trainset)


#Descriptive Analysis
#1.Univariate Analysis


# 1) Distribution of Delay
#Histogram with density
ggplot(trainset,aes(x=Min.Delay))+
  geom_histogram(aes(y=..density..),colour="black",fill="blue",bins=40)+
  #geom_density(alpha=0.2,fill="blue")+
 
  labs(x="Delay(In minutes)",y="Density")+
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )
#Boxplot

ggplot(trainset,aes(y=Min.Delay))+
  geom_boxplot(alpha=0.05,fill="blue")+
  #stat_summary(geom="point",shape=20,
              # size=4,col="dark blue",fill="dark blue")+
  coord_flip()+
  labs(y="Delay(In Minutes)")+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )
#Histogram and Boxplot
options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(trainset$Min.Delay,main="Delay time distribution",col="#9494b8",xlab="Dealy time(In minutes)")
boxplot.stats(trainset$Min.Delay)$stats

#Numerical Summaries
#Mean of Delay time
mean(trainset$Min.Delay)
install.packages("moments")
library(moments)
#Skewness and kurtosis
skewness(trainset$Min.Delay)#>1 positively skewed
kurtosis(trainset$Min.Delay)
x=which(trainset$Min.Delay %in% boxplot.stats(trainset$Min.Delay)$out)
length(x)


# 2) Route

#Route with Direction Vs Number of Incidents
ggplot(trainset,aes(x=Route_New,fill=Direction))+
  geom_bar(stat='count',width=0.7,alpha=0.3)+
  #geom_text(aes(label=..count..),stat="count",vjust=-0.2)+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(x="Route Type",y="Number of Incidents")+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )
tg1=ddply(trainset,c("Route_New","Direction"),summarise,delay=sum(Min.Delay))

ggplot(tg1,aes(x=Route_New,y=delay,fill=Direction))+
  geom_bar(stat="identity",width=0.3,alpha=0.7)+
  #geom_text(aes(label=..count..),stat="identity",vjust=-0.2)+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(x="Route Type",y="Total Delayed time(In minutes")+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )


#bar plot - Route(First 10 routes according to Number of incidents)
x=c(table(trainset$Route))
y=c()
for(i in 1:length(x)){
  if(x[i]>360){
    y[i]=names(x[i])
  }else{
    
    y[i]='Others'
  }
  
  
}
y=unique(y)
y=y[-1]
#names(y)=c("Eglinton West","Jane","Finch West","Lawrence West","Ossington","Bathurst")
w=c()
for(i in 1:length(y)){
  w[i]=x[y[i]]
}

routes=x[match(sort(w,decreasing=T),x)]

names(routes)=c("Finch West","Eglinton West","Lawrence West","Ossington","Bathurst","Jane","Lawrence East","Dufferin","Eglinton East","Lansdowne")
barplot(routes,
        
        xlab = "Route Number",
        ylab = "Number of Incidents",
        ylim=c(0,max(routes)),
        names.arg = names(routes),
        col = "blue"        )
#ggplot(trainset, aes(x= Min.Delay,fill=Route_New))+geom_histogram(binwidth = 500,alpha=0.75)+facet_wrap(~Route_New)


# 3) bar plot - Time_of_the_Day



ggplot(trainset,aes(x=Time_of_the_day))+
  geom_bar(stat='count',width=0.7,alpha=0.3,fill="blue")+
  geom_text(aes(label=..count..),stat="count",vjust=-0.2)+
  #scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(x="Time of the Day",y="Number of Incidents")+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )
ggplot(trainset, aes(x= Min.Delay,fill=Time_of_the_day))+geom_histogram(binwidth = 100,alpha=0.75)+facet_wrap(~Time_of_the_day)

library(plyr)
tg2=ddply(trainset,c("Hour","Day"),summarise,delay=sum(Min.Delay))
ggplot(tg2,aes(x=Hour,y=delay,colour=Day,group=Day))+
  geom_point()+
  #geom_line()+
  labs()

# 4) bar plot - Incidents



ggplot(trainset,aes(x=Incident_New))+
  geom_bar(stat='count',width=0.7,alpha=0.3,fill="blue")+
  geom_text(aes(label=..count..),stat="count",vjust=-0.2)+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(x="Incident",y="Number of Incidents")+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )
tg3=ddply(trainset,c("Hour","Day"),summarise,delay=sum(Min.Delay))
ggplot(tg3,aes(x=Hour,y=delay,colour=Day,group=Day))+
  geom_bar(stat="identity",width=0.3,alpha=0.5)+
  #geom_line()+
  labs(x="Hour",y="Number of Incidents")
  ggplot(trainset, aes(x= Min.Delay,fill=Incident_New))+geom_histogram(binwidth = 100,alpha=0.75)+facet_wrap(~Incident_New)
  # 5) bar plot - Day of the Week
  
  
  
  ggplot(trainset,aes(x=Day))+
    geom_bar(stat='count',width=0.7,alpha=0.3,fill="blue")+
    geom_text(aes(label=..count..),stat="count",vjust=-0.2)+
    scale_x_discrete(guide = guide_axis(angle = 90))
  labs(x="Day of the Week",y="Number of Incidents")+
    theme(
      panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
      panel.background=element_blank(),axis.line=element_line(colour="black")
    )
  ggplot(trainset, aes(x= Min.Delay,fill=Day))+geom_histogram(binwidth = 100,alpha=0.75)+facet_wrap(~Day)
  

#2.Bivariate Analysis
  
  #1) Scatter Plot - Gap Vs delayed ti e
  #plot(trainset$Min.Gap, trainset$Min.Delay,
    #   xlab="Time gap in minutes with the next bus ", ylab="Delayed time(minutes)", pch=19)
  ggplot(trainset,aes(x=Min.Gap,y=Min.Delay))+
    geom_point(alpha=0.3,colour="blue")+
    labs(x="Gap(In minutes)",y="Delayed time(In minutes)")+
    theme(
      panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
      panel.background=element_blank(),axis.line=element_line(colour="black")
    )
  
  
  #2)Route Vs  delayed time
total_delay=aggregate(Min.Delay ~ Route_New, data = trainset, sum)
#v=c(total_delay[order(total_delay$Min.Delay),])
 # sum[data[trainset$Min.Delay~trainset$Route_New]]
  #sum(data$Min.Delay)
  
  ggplot(trainset,aes(x=Route_New,y=Min.Delay))+
    geom_boxplot(alpha=0.2,fill="blue")+
    #stat_summary(fun=mean,geom="point",shape=20,
                 #size=4,col="dark blue",fill="dark blue")+
    #scale_x_discrete(guide = guide_axis(angle = 90))+
    coord_flip()+
    labs(x="Route",y="Delayed time(in minutes)")+
    theme(
      panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
      panel.background=element_blank(),axis.line=element_line(colour="black")
    )
  
 


  # 3)  Incident Vs Delayed time
 
  
  #Boxplot 
  ggplot(trainset,aes(x=Incident_New,y=Min.Delay))+
    geom_boxplot(alpha=0.2,fill="blue")+
    #stat_summary(fun=mean,geom="point",shape=20,
     #            size=4,col="dark blue",fill="dark blue")+
    #scale_x_discrete(guide = guide_axis(angle = 90))+
    coord_flip()+
    labs(x="Incident",y="Delayed time(in minutes)")+
    theme(
      panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
      panel.background=element_blank(),axis.line=element_line(colour="black")
    )

  # 4) Boxplot - Day Vs Delayed time
  
  ggplot(trainset,aes(x=Day,y=Min.Delay))+
    geom_boxplot(alpha=0.2,fill="blue")+
    #stat_summary(fun=mean,geom="point",shape=20,
     #            size=4,col="dark blue",fill="dark blue")+
    #scale_x_discrete(guide = guide_axis(angle = 90))+
    coord_flip()+
    labs(x="Day of the week",y="Delayed time(in minutes)")+
    theme(
      panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
      panel.background=element_blank(),axis.line=element_line(colour="black")
    )

  
  
  
  
  # 5) Boxplot - Time of the day Vs Delayed time
  
  ggplot(trainset,aes(x=Time_of_the_day,y=Min.Delay))+
    geom_boxplot(alpha=0.2,fill="blue")+
    #stat_summary(fun=mean,geom="point",shape=20,
     #            size=4,col="dark blue",fill="dark blue")+
    scale_x_discrete(guide = guide_axis(angle = 90))+
    labs(x="Day of the week",y="Delayed time(in minutes)")+
    theme(
      panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
      panel.background=element_blank(),axis.line=element_line(colour="black")
    )
  
  #6) Boxplot - Month Vs Delayed time
  
  ggplot(trainset,aes(x=Month,y=Min.Delay))+
    geom_boxplot(alpha=0.2,fill="blue")+
    #stat_summary(fun=mean,geom="point",shape=20,
    #            size=4,col="dark blue",fill="dark blue")+
    scale_x_discrete(guide = guide_axis(angle = 90))+
    labs(x="Month",y="Delayed time(in minutes)")+
    theme(
      panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
      panel.background=element_blank(),axis.line=element_line(colour="black")
    )
  
  

  # Multivariate Analysis
  # Advanced Analysis Techniques
  #  Extract only Numeric Variables
  
  
  numericVar=subset(trainset,select=c(Min.Delay,Min.Gap))
  view(numericVar)
  
  # Detect Outliers
  # Graphical Representations of Outliers
  d2= outlier(numericVar)
  
  
  # Check What are the rows containing outliers
  md=mahalanobis(numericVar,center=colMeans(numericVar),cov=cov(numericVar))
  cutoff=(qchisq(p=0.999,df=ncol(numericVar)))
  names_outliers=which(md>cutoff)
  names_outliers
  length(names_outliers)
  
  
  
  
  aggregate(Route_New ~ Hour, data = trainset, sum) 
  
  
  
  
  
  