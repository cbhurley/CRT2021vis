## ----setup, include=FALSE--------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=F)


## ---- eval=F---------------------------------------------------------------------------------------------------------------
## install.packages(c("GGally", "ISLR2", "naniar"))


## --------------------------------------------------------------------------------------------------------------------------
library(GGally)
library(ISLR2)
library(timetk) # for the data
library(palmerpenguins)
library(tidyverse)
library(naniar)

bike <- bike_sharing_daily
bike$season <- c("Winter","Spring", "Summer","Fall")[bike$season]
bike$season <- factor(bike$season, levels = c("Winter","Spring", "Summer","Fall"))  

bike$mnth <- factor(bike$mnth)

bike$holiday <- factor(c("No","Yes")[bike$holiday+1])
bike$workingday <- factor(c("No","Yes")[bike$workingday+1])

bike$yr <- factor(c("2011","2012")[bike$yr+1])

bike$weathersit <- c("clear", "cloudy", "lightP", "heavyP")[bike$weathersit]
bike$weathersit <- factor(bike$weathersit, levels= c("clear", "cloudy", "lightP", "heavyP"))
bike <- droplevels(bike)


## ----  fig.width=4.5, fig.height=4, fig.align="center"---------------------------------------------------------------------
ggplot(data=Bikeshare, aes(x=casual, y=registered)) + geom_point()


## ----  fig.width=4.5, fig.height=4, fig.align="center"---------------------------------------------------------------------
ggplot(data=Bikeshare, aes(x=casual, y=registered)) + 
  geom_jitter(alpha=.5, size=.5)


## ----  fig.width=5, fig.height=4, fig.align="center"-----------------------------------------------------------------------
p <- ggplot(data=Bikeshare, aes(x=casual, y=registered))
p+ geom_bin2d()


## ----  fig.width=5, fig.height=4, fig.align="center"-----------------------------------------------------------------------
p +  
  geom_bin2d() +
  scale_fill_gradient(low = "lightblue1", high = "steelblue4")

p +  
  geom_bin2d() +
  scale_fill_gradient(low = "lightblue1", high = "steelblue4" ,trans="log10")


## ----  fig.width=5, fig.height=4, fig.align="center"-----------------------------------------------------------------------
p +  
  stat_binhex() +
  scale_fill_gradient(low = "lightblue1", high = "steelblue4" ,trans="log10")


## ----  fig.width=5, fig.height=4, fig.align="center"-----------------------------------------------------------------------
p + stat_density2d()

## ----  fig.width=5, fig.height=4, fig.align="center", eval=F---------------------------------------------------------------
## # fills in the contours
## p + stat_density2d(aes(fill = after_stat(level)), geom = "polygon")


## ---- fig.width=4.5, fig.height=4, fig.align="center"----------------------------------------------------------------------
library(GGally)
ggpairs(bike, columns=c("casual","registered","cnt" ))


## ---- fig.width=4.5, fig.height=4, fig.align="center"----------------------------------------------------------------------

  
ggpairs(bike, mapping = aes(color = yr),
            columns=c("casual","registered","cnt" ),
  lower = list(continuous =  wrap("smooth", method="lm", se=F, alpha=.5)),
  diag = list(continuous = wrap("densityDiag", alpha=0.5 )))



## ---- fig.width=4.5, fig.height=4, fig.align="center"----------------------------------------------------------------------


ggpairs(bike, mapping = aes(color = yr),
        columns=c("casual","registered","cnt","yr"),
        lower = list(continuous =  wrap("smooth", method="lm", se=F, alpha=.5)),
        diag = list(continuous = wrap("densityDiag", alpha=0.5 )))
 


## ---- fig.width=6, fig.height=3, fig.align="center"------------------------------------------------------------------------

s <- sample(nrow(bike),20)
ggparcoord(bike[s,], columns=match(c("casual","registered","cnt"),names(bike)),
           groupColumn = "yr",
           showPoints=TRUE,
           alphaLines=0)

ggparcoord(bike[s,], columns=match(c("casual","registered","cnt"),names(bike)),
           groupColumn = "yr",
           showPoints=TRUE,
           alphaLines=1)

ggparcoord(bike[s,], columns=match(c("casual","registered","cnt"),names(bike)),
           groupColumn = "yr")


## ---- fig.width=6, fig.height=3, fig.align="center"------------------------------------------------------------------------

ggparcoord(bike[s,], columns=match(c("casual","registered","cnt"),names(bike)),
           groupColumn = "yr", scale="uniminmax")
ggparcoord(bike[s,], columns=match(c("casual","registered","cnt"),names(bike)),
           groupColumn = "yr", scale="globalminmax")



## ---- fig.width=6, fig.height=3, fig.align="center"------------------------------------------------------------------------

x <- rnorm(100)
y <- x+ .3*rnorm(100)
z <- -y+ .1*rnorm(100)
w <- -z+ .5*rnorm(100)
ggparcoord(data.frame(x,y,z,w), scale="uniminmax")


## ----fig.width=6, fig.height=3, fig.align="center"-------------------------------------------------------------------------
ggparcoord(bike, columns=match(c("casual","registered","cnt"),names(bike)),
           groupColumn = "workingday",
           alphaLines=.4)


## ----  fig.width=5, fig.height=4, fig.align="center"-----------------------------------------------------------------------
ggplot(data=bike, aes(x=casual, y=registered, color=workingday)) + geom_point()


## ---- fig.width=6, fig.height=5, fig.align="center", echo=F----------------------------------------------------------------
ggpairs(na.omit(penguins), mapping = aes(color = species),
            columns=c(3:7),
  lower = list(continuous =  wrap("smooth", method="lm", se=F, alpha=.5)),
  diag = list(continuous = wrap("densityDiag", alpha=0.5 )))


## ----fig.width=6, fig.height=3, fig.align="center", echo=F, eval=F---------------------------------------------------------
## 
## 
## ggparcoord(penguins, columns=which(sapply(penguins, is.numeric)),
##            groupColumn = "species",
##            alphaLines=.4)
## 
## ggparcoord(penguins, columns=which(sapply(penguins, is.numeric)),
##            groupColumn = "island",
##            alphaLines=.4)
## 
## ggparcoord(penguins, columns=which(sapply(penguins, is.numeric)),
##            groupColumn = "sex",
##            alphaLines=.4)
## 


## ----fig.align="center"----------------------------------------------------------------------------------------------------
m <- read_csv("marathon.csv",na=c(""," ","NA"))
vis_miss(m)


## ----fig.align="center"----------------------------------------------------------------------------------------------------
gg_miss_upset(m[,-c(22,20)])


## ---- echo=F, eval=T, out.width="25%", fig.align='center'------------------------------------------------------------------
knitr::include_graphics("plot.png")

