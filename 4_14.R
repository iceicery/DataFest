install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape2")
install.packages("stringr")
install.packages("lubridate")
install.packages("data.table")
install.packages("ggExtra")

library(ggplot2)
library(dplyr)
library(reshape2)
library(stringr)
library(lubridate)
library(data.table)
library(ggExtra)

DT<-read.csv("E:/MAS/R/DataFest/data/data.csv")

#code the variable
setnames(DT, "estimatedSalary","salary")
setnames(DT, "stateProvince","state")
setnames(DT, "avgOverallRating","rating")
setnames(DT, "normTitleCategory","category")
setnames(DT, "Dlenth","Dlength")
setnames(DT, "descriptionWordCount","Dcount")
setnames(DT, "licenseRequiredJob","license")
setnames(DT, "educationRequirements","education")
setnames(DT,"experienceRequired","workYear")

#change currency to usd
DT$salaryus <-with(DT,
                   ifelse(salaryCurrency=="CAD", DT$salary*0.79, DT$salary),
                   ifelse(salaryCurrency=="EUR", DT$salary*1.23, DT$salary)
                   )


#histogram 
ggplot(DT, aes(salaryus, colour = country)) +
  geom_freqpoly(binwidth = 500)
ggplot(DT, aes(salaryus, colour = education)) +
   geom_freqpoly(binwidth = 500)



# deal with time
DT$date <- as.Date(DT$date, "%m/%d/%Y")
DT$dataM <- parse_date_time(DT$data,"%m%Y")
table(format(dates, format = "%Y"))



#random chose data and do the scatter matrix
subset <-DT[sample(nrow(DT), 50), ]

pairs(~clicks+salaryus+numReviews+rating+Dlength+Dcount,data=subset, 
             main="Simple Scatterplot Matrix")

#mean salary of each category   



mean_salary<-aggregate( salaryus~ category,DT, mean )

#split data to diff category and count level no
#this don't work since data frame already specified. level will be the same.
#splitcity <- function(DT) {
# s <- split( DT, DT$category)
#  sapply( s, function(x) nlevels(x$city))
#}

#count catogory (number of job)
#library(plyr)
num_job <- dplyr::count(DT, DT$category)

#count location
#for test#dtt <- DT[sample(1:nrow(DT),100000),]
#don't know why#dtt$count <- rep(1,nrow(dtt))

dtt2 <- aggregate(count~category+city,DT,sum)
dtt3 <- aggregate(count~category,dtt2,sum)
dtt3


#count location
g <- c("category","category_no","location_no","salary","job_no")
g$category<- DT$category
g$salary<-mean_cat

fullData <-cbind(mean_salary,
                  job_num=num_job$n,
                  location_num=dtt3$count)

# classic plot 
p=ggplot(fullData, aes(x=location_num, y=salaryus, color=job_num, size=job_num)) +
  geom_point() +
  theme(legend.position="none")


# with marginal histogram
ggMarginal(p, type="histogram")
ggsave("E:/MAS/R/DataFest/data/fullData.pdf")

#interesting points
rich <-subset(fullData, salaryus> 100000)
rich
flexible <-subset(fullData, location_num>7500)
flexible
sorry<- subset(fullData, location_num<100 & salaryus<28000)
sorry
myfavorite<-subset(fullData, location_num>2500 & salaryus>75000)
myfavorite

#total earning
#comparing only two category
techsoftware <- subset(DT,category=="techsoftware")
#mean salary of each category
meanY_salary<-aggregate(salaryus~ workYear,techsoftware, mean )
dmanagement <- subset(DT,category=='management')
meanY_salary_M<-aggregate(salaryus~workYear,management,mean)
doctor <- subset(DT,category=='meddr')
meanY_salary_D<-aggregate(salaryus~workYear,management,mean)



ggplot(data=meanY_salary,aes(x=workYear,y=salaryus)) +
  geom_smooth(method = lm, color= "dark blue")
#manager
ggplot(data=meanY_salary_M,aes(x=workYear,y=salaryus)) +
  geom_smooth(method = lm, color="dark red")
ggplot(data=meanY_salary_D,aes(x=workYear,y=salaryus)) +
  geom_smooth(method = lm, color= "blue")

#t1 <-table(DT$country,DT$educationRequirements)
#t1
#t2<- table(DT$normTitleCategory,DT$country)
#t2
#t3 <-table(DT$supervisingJob,DT$educationRequirements,DT$licenseRequiredJob)
#t3
#t4<-table(newdata$supervisingJob,newdata$educationRequirements,newdata$licenseRequiredJob)



