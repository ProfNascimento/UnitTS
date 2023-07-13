require(dplyr)
require(zoo)
require(xts)
library(lubridate)
require(ggplot2)

y15 <- read.csv("https://raw.githubusercontent.com/ProfNascimento/UnitTS/main/270008_2015_Humedad_.csv", sep=";")
y17 <- read.csv("https://raw.githubusercontent.com/ProfNascimento/UnitTS/main/270008_2017_Humedad_.csv", sep=";")
y21 <- read.csv("https://raw.githubusercontent.com/ProfNascimento/UnitTS/main/270008_2021_Humedad_.csv", sep=";")
head(y15)

outage.ts <- as.POSIXct(y15$momento, format = "%d-%m-%Y %H:%M")
outage.zoo <- zoo(y15$HR_Valor,outage.ts,frequency = 24)
complete.zoo <- merge(outage.zoo, zoo(, seq(start(outage.zoo), end(outage.zoo), 
                                            by = "1 hour")), all = TRUE)
complete.zoo[is.na(complete.zoo)] <- NA
str(complete.zoo)

test11=imputeTS::na_interpolation(complete.zoo)

db=rbind(y15,y17,y21)
db$month=month(as.POSIXlt(db$momento, format="%d-%m-%Y"))
db$year=year(as.POSIXlt(db$momento, format="%d-%m-%Y"))
db$day=day(as.POSIXlt(db$momento, format="%d-%m-%Y"))
db$hour=hour(as.POSIXlt(db$momento, format="%d-%m-%Y  %H:%M:%OS"))

db$year=as.factor(db$year)

ggplot(db)+ 
  geom_density(aes(HR_Valor/100,fill=year, colour = year),alpha = 0.1) + 
  xlim(c(0,1))+ xlab("Relative Humidity")+
  facet_wrap(~month)

db$index=as.numeric(paste(db[,"day"],
               db[,"hour"],sep = "."))

match(c(23.0,25.23),db$index)

p1=ggplot(db, aes(month, HR_Valor/100, color=year, group=interaction(year,month))) +
  geom_boxplot()+ylab("Relative Humidity") + xlab("MONTH") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  annotate("rect", xmin=2.5, xmax=3.5, ymin=0, ymax=Inf, alpha=0.2, fill="gray")+
  scale_x_continuous(breaks=seq(1,12,1), 
                     labels=c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))

p2=ggplot(db[db$month==3,],
       aes(index, HR_Valor/100, color = year,group=year))+ 
  geom_line() + xlab("Relative Humidity") +
  annotate("rect", xmin=23.0, xmax=25.23, ymin=0, ymax=Inf, alpha=0.2, fill="red")+
  ylab("Relative Humidity") + xlab("MARCH")

cowplot::plot_grid(p1, p2, labels = c('A', 'B'),nrow=2)

db=db[complete.cases(db), ]

##
eventdata1 <- xts(db[db$month==3 & db$year==2015,"HR_Valor"], order.by = db[db$month==3 & db$year==2015,"momento"])
attr(eventdata1, 'frequency') <- 24  # set frequency attribute
ts.d1=eventdata1 %>% stl(t.window=24, s.window="periodic", robust=TRUE) 
ts.d1 %>%autoplot()

eventdata2 <- xts(db[db$month==5 & db$year==2017,"HR_Valor"], order.by = db[db$month==5 & db$year==2017,"momento"])
attr(eventdata2, 'frequency') <- 24  # set frequency attribute
ts.d2=eventdata2 %>% stl(t.window=24, s.window="periodic", robust=TRUE) 
ts.d2 %>% autoplot()

eventdata3 <- xts(db[db$month==3 & db$year==2021,"HR_Valor"], order.by = db[db$month==3 & db$year==2021,"momento"])
attr(eventdata3, 'frequency') <- 24  # set frequency attribute
ts.d3=eventdata3 %>% stl(t.window=24, s.window="periodic", robust=TRUE) 
ts.d3 %>% autoplot()

trend.total=na.omit(cbind(
  c(db[db$month==3 & db$year==2015,"momento"],
    db[db$month==5 & db$year==2017,"momento"],
    db[db$month==3 & db$year==2021,"momento"]),
  c(rep(1,469),rep(2,634),rep(3,744)),
  c(ts.d1$time.series[,2],ts.d2$time.series[,2],ts.d3$time.series[,2])))
colnames(trend.total)=c("date","variable","value")

## CHANGE POINTS MARCH 2015
strucchange::breakpoints(as.numeric(trend.total[trend.total[,2]=="1",3]) ~ 1)

# PLOT CHANGE POINTS
require(ggfortify)
q1=na.omit(ts.d1$time.series[,2]) %>%
  changepoint:: cpt.meanvar() %>%  # Identify change points
  autoplot(xlab="MARCH '15", ylab="Relaty Humidity",ylim=c(50,100))+
  theme(axis.text.x=element_blank())

q2=na.omit(ts.d2$time.series[,2]) %>%
  changepoint:: cpt.meanvar() %>%  # Identify change points
  autoplot(xlab="MAY '17", ylab="Relaty Humidity",ylim=c(50,100))+
  theme(axis.text.x=element_blank())

q3=na.omit(ts.d3$time.series[,2]) %>%
  changepoint:: cpt.meanvar() %>%  # Identify change points
  autoplot(xlab="MARCH '21", ylab="Relaty Humidity",ylim=c(50,100))+
  theme(axis.text.x=element_blank())

cowplot::plot_grid(q1, q2, q3, labels = c('A', 'B', 'C'),nrow=3)

#################################
## SPC & EVA - Residuals TBATS ##
#################################
# 2015
tbats.d1=eventdata1 %>% tbats(seasonal.periods = 24)
#Percentage Error (PE)
AbPE1=((eventdata1[,1]-tbats.d1$fitted.values)/eventdata1[,1]) 
#PE TIME-INDEPENDENCE
plot(acf(all.mean+as.numeric(AbPE1)*all.mean)[1:20],main="ImPE 2015",ylim=c(-0.12,0.1))
plot(pacf(AbPE1),main="",ylim=c(-0.15,0.15))
Box.test(AbPE1,lag=20,"Ljung-Box")
#tseries::adf.test(AbPE1)

# 2017
tbats.d2=eventdata2 %>% tbats(seasonal.periods = 24)
AbPE2=((eventdata2[,1]-tbats.d2$fitted.values)/eventdata2[,1]) #percentage error
plot(acf(all.mean+as.numeric(AbPE2)*all.mean)[1:20],main="ImPE 2017",ylim=c(-0.12,0.1))
plot(pacf(AbPE2),main="",ylim=c(-0.15,0.15))
Box.test(AbPE2,lag=20,"Ljung-Box")

# 2021
tbats.d3=eventdata3 %>% tbats(seasonal.periods = 24)
AbPE3=((eventdata3[,1]-tbats.d3$fitted.values)/eventdata3[,1]) #percentage error
plot(acf(all.mean+as.numeric(AbPE3)*all.mean)[1:20],main="ImPE 2021",ylim=c(-0.12,0.1))
plot(pacf(AbPE3),main="",ylim=c(-0.15,0.15))
Box.test(AbPE3,lag=20,"Ljung-Box")

all.mean=mean(c(y15$HR_Valor,y17$HR_Valor,y21$HR_Valor),na.rm = TRUE)/100

## INVARIANTE Absolute Percentage Error (IAbPE) SUMMARY
apply(cbind(all.mean+as.numeric(AbPE1)*all.mean,
      all.mean+as.numeric(AbPE2)*all.mean,
      all.mean+as.numeric(AbPE3)*all.mean),2,summary)

ImPE.15=all.mean+as.numeric(AbPE1)*all.mean

as.data.frame(ImPE.15) %>%
  mutate(index=1:length(ImPE.15),
         LB = case_when(ImPE.15 < all.mean ~ ImPE.15,
                        ImPE.15 > all.mean ~ all.mean),
         UB = case_when(ImPE.15 > all.mean ~ ImPE.15,
                        ImPE.15 < all.mean ~ all.mean),
         C=all.mean) %>% 
  ggplot(aes(index,C)) + geom_ribbon(aes(ymin=LB,ymax=UB),fill = "steelblue2")+ 
  geom_line(color = "firebrick",size = 1)+ylab("ImPE (March '15)") + 
  theme(text = element_text(size = 20))
