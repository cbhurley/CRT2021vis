## ----setup, include=FALSE--------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=F)


## ---- echo=F, eval=T, out.width="25%", fig.align='center'------------------------------------------------------------------
knitr::include_graphics("figs/wainer.png")


## ---- eval=F---------------------------------------------------------------------------------------------------------------
## install.packages(c("tidyverse", "dataReporter", "timetk","palmerpenguins"))


## --------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(timetk) # for the data
library(palmerpenguins) # more data




## ---- getdata--------------------------------------------------------------------------------------------------------------

glimpse(bike_sharing_daily)


## ---- fixdata--------------------------------------------------------------------------------------------------------------
bike <- bike_sharing_daily
bike$season <- c("Winter","Spring", "Summer","Fall")[bike$season]
bike$season <- factor(bike$season, levels = c("Winter","Spring", "Summer","Fall"))  

bike$mnth <- factor(bike$mnth)

bike$holiday <- factor(c("No","Yes")[bike$holiday+1])
bike$workingday <- factor(c("No","Yes")[bike$workingday+1])

bike$yr <- factor(c("2011","2012")[bike$yr+1])

bike$weathersit <- c("clear", "cloudy", "lightP", "heavyP")[bike$weathersit]
bike$weathersit <- factor(bike$weathersit, levels= c("clear", "cloudy", "lightP", "heavyP"))

glimpse(bike)


## ---- report, eval=F-------------------------------------------------------------------------------------------------------
## dataReporter::makeDataReport(bike,replace=TRUE)


## --------------------------------------------------------------------------------------------------------------------------
table(bike$weathersit)
bike <- droplevels(bike)
table(bike$weathersit)


## ---- fig.width=3.5, fig.height=3, fig.align="center"----------------------------------------------------------------------
ggplot(data=bike, 
       mapping= aes(x=cnt)) + geom_histogram()
ggplot(data=bike, mapping= aes(x=cnt)) + 
  geom_histogram(fill="lightblue", color="navy")


## ---- fig.width=4.5, fig.height=3.5, fig.align="center"--------------------------------------------------------------------
ggplot(data=bike, mapping= aes(x=cnt, fill=yr)) + 
  geom_histogram(color="black")


## ----  fig.width=3.5, fig.height=3, fig.align="center"---------------------------------------------------------------------
ggplot(data=bike, mapping= aes(x=weathersit)) + 
  geom_bar(fill="lightblue", color="navy")


## ----  fig.width=4.5, fig.height=3, fig.align="center"---------------------------------------------------------------------
ggplot(data=bike, mapping= aes(x=weathersit, fill=yr)) + 
  geom_bar( )


## ----  fig.width=4.5, fig.height=3, fig.align="center"---------------------------------------------------------------------
ggplot(data=bike, mapping= aes(x=weathersit, fill=yr)) + 
  geom_bar(position="fill")


## ----  fig.width=4.5, fig.height=3, fig.align="center"---------------------------------------------------------------------

ggplot(data=bike, mapping= aes(x=mnth, y=cnt)) + 
  geom_col(color="lightblue", fill="lightblue")


ggplot(data=bike, mapping= aes(x=mnth, y=cnt, fill=yr)) + 
  geom_col()



## ----  fig.width=3.5, fig.height=3, fig.align="center"---------------------------------------------------------------------
ggplot(data=bike, aes(x=temp, y=cnt)) + geom_point()


## ----  fig.width=4.5, fig.height=3, fig.align="center"---------------------------------------------------------------------
ggplot(data=bike, aes(x=temp, y=cnt, color=yr)) + 
  geom_point()

ggplot(data=bike, aes(x=temp, y=cnt, color=yr, shape=season)) + 
  geom_point()




## ----  fig.width=4.5, fig.height=3, fig.align="center"---------------------------------------------------------------------
ggplot(data=bike, aes(x=temp, y=cnt, color=weekday)) + 
  geom_point()

bike$weekday <- factor(bike$weekday)

ggplot(data=bike, aes(x=temp, y=cnt, color=weekday)) + 
  geom_point()



## ----  fig.width=5.5, fig.height=3, fig.align="center"---------------------------------------------------------------------
ggplot(data=bike, aes(x=dteday, y=cnt)) + 
  geom_line()



## ----  fig.width=5.5, fig.height=3, fig.align="center"---------------------------------------------------------------------
library(lubridate)
ggplot(data=bike, aes(x=yday(dteday), y=cnt, color=yr)) + 
  geom_line()


## ---- fig.width=4.5, fig.height=3, fig.align="center", echo=F--------------------------------------------------------------

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar() 


ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species,
                 shape = species)) 


## ---- echo=F, eval=T, out.width="75%", fig.align='center'------------------------------------------------------------------
knitr::include_graphics("figs/aes.png")


## ----  fig.width=6.5, fig.height=3, fig.align="center"---------------------------------------------------------------------

ggplot(data=bike, aes(x=yday(dteday), y=cnt, color=yr)) + 
  geom_line()+
  scale_x_continuous(breaks=cumsum(c(1,31,28,31,30,31,30,31,31,30,31,30)),
                     labels=month.abb[1:12])+
  xlab("Date")


## ----  fig.width=6.5, fig.height=5.5, fig.align="center"-------------------------------------------------------------------

ggplot(data=bike, aes(x=temp, y=cnt, color=yr)) + 
  geom_point()+
  facet_wrap(~ season)


## ---- fig.width=3.5, fig.height=3, fig.align="center"----------------------------------------------------------------------
ggplot(data=bike, aes(x=temp, y=cnt)) + 
  geom_point()+
  geom_smooth()

p <- ggplot(data=bike, aes(x=temp, y=cnt)) + 
     geom_point()

p + geom_smooth(method="lm", se=FALSE)


## ---- fig.width=4.5, fig.height=3, fig.align="center"----------------------------------------------------------------------
ggplot(data=bike, aes(x=temp, y=cnt, color=yr)) + 
  geom_point()+
  geom_smooth(se=FALSE)



## ----  fig.width=9, fig.height=4, fig.align="center"-----------------------------------------------------------------------

ggplot(data=bike, aes(x=temp, y=cnt, color=yr)) + 
  geom_point()+
  facet_grid( workingday ~ season)


## ---- fig.width=9, fig.height=4, fig.align="center"------------------------------------------------------------------------
ggplot(data=bike, aes(x=temp, y=cnt, color=yr)) + 
  geom_point()+
  facet_grid( workingday ~ season)+
  geom_smooth(se=F)


## ----  fig.width=9, fig.height=4, fig.align="center"-----------------------------------------------------------------------
fit <- lm(cnt ~ temp+season+yr, data=bike)
bike %>% 
  mutate(fit = fitted(fit)) %>%
  ggplot(aes(x=temp, y=cnt, color=yr)) + 
  geom_point(size=1)+  # points smaller
  facet_grid( workingday ~ season)+
  geom_smooth(method=lm, se=F)+
  geom_line(aes(y=fit), linetype="twodash", size=1.5)


## ---- fig.width=9.5, fig.height=3, fig.align="center", echo=F--------------------------------------------------------------

ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = sex))+
  facet_wrap(~ species)


ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm, color=sex)) +
  geom_point()+
  facet_wrap(~ species)+
  geom_smooth(method="lm", se=FALSE)+
  scale_colour_discrete(na.translate = FALSE)


## ---- fig.width=3.5, fig.height=3, fig.align="center", eval=T--------------------------------------------------------------
ggplot(data=bike, aes(x=cnt)) + 
  geom_density(color="red")+
  geom_histogram(aes(y=after_stat(density)),fill="lightblue", color="navy", alpha=.5)


## ---- fig.width=4.5, fig.height=3, fig.align="center", eval=T--------------------------------------------------------------
ggplot(data=bike, aes(x=cnt, fill=yr)) + 
  geom_density(alpha=.5)


## ---- fig.width=4.5, fig.height=3, fig.align="center"----------------------------------------------------------------------
ggplot(data=bike, aes(y=cnt,x=season)) + 
  geom_boxplot(varwidth=TRUE) # width of box proportional to sqrt of nobs


## ---- fig.width=3.5, fig.height=3, fig.align="center", eval=T--------------------------------------------------------------
ggplot(data=bike, aes(y=cnt), x=0) + # any number will do
  geom_boxplot()  


## ---- fig.width=4.5, fig.height=3, fig.align="center"----------------------------------------------------------------------
ggplot(data=bike, aes(y=cnt,x=season)) + 
  geom_violin(fill="lightblue")

