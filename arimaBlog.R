search <- read.csv("latest.search.csv", stringsAsFactors = FALSE)
#search <- search[head(which(search$cloud.tco...Worldwide.>0),n=1):nrow(search),]
openstack.month <- "2009-09"
search <- search[which(search$Month == openstack.month):nrow(search),]
tokyo.month <- which(search$Month == "2015-10")
search$tokyo <- 0
search[tokyo.month:nrow(search),]$tokyo <- 1

search$austin <- 0
austin.month <- which(search$Month == "2016-04")
search[0:11+austin.month,]$austin <- 1
search[austin.month:nrow(search),]$austin <- 1

search$blog <- 0
blog.month = which(search$Month == "2016-01")
search[0:11+blog.month,]$blog <- 1
search[blog.month:nrow(search),]$blog <- 1

# test for stationary data
library(tseries)
adf.test(search$cloud.tco...Worldwide., alternative = "stationary")


library(ggplot2)
library(ggthemes)

library(forecast)
arimax <- auto.arima(search$cloud.tco...Worldwide.,xreg=data.frame(blog=search$blog), stepwise=FALSE, parallel = TRUE)
#arimax <- auto.arima(search$cloud.tco...Worldwide.,xreg=data.frame(blog=search$blog,austin=search$austin,tokyo=search$tokyo), stepwise=FALSE, parallel = TRUE)
#arimax <- auto.arima(search$cloud.tco...Worldwide.,xreg=data.frame(blog=search$blog,austin=search$austin), stepwise=FALSE, parallel = TRUE)
#arimax <- auto.arima(search$cloud.tco...Worldwide.,xreg=data.frame(search$tokyo,search$austin), stepwise=FALSE, parallel = TRUE)
p<-(1-pnorm(abs(arimax$coef)/sqrt(diag(arimax$var.coef))))*2
print(p)  
months <- sum(search$austin)

# 12 months
(12*arimax$coef["blog"])/sum(search[0:(11)+austin.month,]$cloud.tco...Worldwide.)
# all
(months*arimax$coef["blog"])/sum(search[0:(months-1)+austin.month,]$cloud.tco...Worldwide.)

#search$blog <- 0
#search$tokyo <- 0
#search$austin <- 0
#arimax <- auto.arima(search$cloud.tco...Worldwide.,xreg=search$blog, stepwise=FALSE, parallel = TRUE)
#search$noaustin <- 0
#awith <- predict(arimax, newxreg=search$noaustin) 
#awith <- predict(arimax, newxreg=data.frame(blog=search$blog,tokyo=search$tokyo,austin=search$austin))
#((nrow(search)-austin.month)*arimax$coef["blog"])/sum(search[austin.month:nrow(search),]$cloud.tco...Worldwide.)

awith.df <- data.frame(Month = search$Month, prediction=arimax$coef["intercept"]+arimax$coef["blog"]*search$blog)

#png("CloudTCOSearchVolumeMST.png", width=1600,height=750,res=150)
ggplot(search, aes(y=cloud.tco...Worldwide.,x=Month,group=1)) +
  #geom_rect(xmin=blog.month, xmax=blog.month+11, ymin=0, ymax=100, alpha=0.03, color="#cc0000",fill=NA) +
  #geom_rect(xmin=austin.month, xmax=austin.month+11, ymin=0, ymax=100, alpha=0.03, color="#00cc00",fill=NA) +
  #geom_rect(xmin=tokyo.month, xmax=tokyo.month+11, ymin=0, ymax=100, alpha=0.03, color="#0000cc",fill=NA) +
  geom_line(data=awith.df, aes(x=Month, y=prediction)) +
  geom_line(color="gray50") +
  geom_vline(xintercept=which(search$Month=="2016-01"), color="red") +
  #geom_point(aes(y=(awith$pred)),shape="-",color="blue") +
  #geom_point(data=data.frame(x="2016-01",y=search[search$Month=="2016-01",]$cloud.tco...Worldwide.),aes(x=x,y=y),color="red") +
  theme_tufte(base_family="Overpass") + 
  labs(y='"cloud tco" search volume on Google',x="") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) 
#dev.off()


summary(lm(cloud.tco...Worldwide.~blog,data=search))
summary(lm(cloud.tco...Worldwide.~austin,data=search))
