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
 


## ---- eval=F---------------------------------------------------------------------------------------------------------------
## # install.packages("devtools")
## remotes::install_github("yaweige/ggpcp", build_vignettes = TRUE)


## ---- fig.width=6, fig.height=3, fig.align="center", eval=T, echo=F--------------------------------------------------------

s <- sample(nrow(bike),20)
ggparcoord(bike[s,], columns=match(c("casual","registered","cnt"),names(bike)),
           groupColumn = "yr",
           showPoints=TRUE,
           alphaLines=0,scale="uniminmax")

ggparcoord(bike[s,], columns=match(c("casual","registered","cnt"),names(bike)),
           groupColumn = "yr",
           showPoints=TRUE,
           alphaLines=1,scale="uniminmax")

ggparcoord(bike[s,], columns=match(c("casual","registered","cnt"),names(bike)),
           groupColumn = "yr",scale="uniminmax")


## ---- fig.width=6, fig.height=3, fig.align="center"------------------------------------------------------------------------
library(ggpcp)
ggplot(bike[s,], aes(color=yr)) + geom_pcp(aes(vars=vars(casual,registered, cnt)))


## ---- fig.width=6, fig.height=3, fig.align="center"------------------------------------------------------------------------
ggplot(bike[s,], aes(color=yr)) + geom_pcp(aes(vars=vars(casual,registered, cnt)), method="raw")



## ---- fig.width=6, fig.height=3, fig.align="center"------------------------------------------------------------------------

x <- rnorm(100)
y <- x+ .3*rnorm(100)
z <- -y+ .1*rnorm(100)
w <- -z+ .5*rnorm(100)
ggplot(data.frame(x,y,z,w))+ geom_pcp(aes(vars=vars(everything())))


## ----fig.width=6, fig.height=3, fig.align="center"-------------------------------------------------------------------------
ggplot(bike, aes(color=workingday)) + geom_pcp(aes(vars=vars(casual,registered, cnt)), alpha=.4)



## ----  fig.width=5, fig.height=4, fig.align="center"-----------------------------------------------------------------------
ggplot(data=bike, aes(x=casual, y=registered, color=workingday)) + geom_point()


## ----fig.width=6, fig.height=3, fig.align="center"-------------------------------------------------------------------------
ggplot(bike, aes(vars=vars( workingday,casual,registered, cnt), color=workingday)) + 
  geom_pcp(alpha=.4)

ggplot(bike, aes(vars=vars( workingday,casual,registered, cnt), color=workingday)) + 
   geom_pcp_box(boxwidth = 0.1, fill="grey70")+
    geom_pcp(alpha=.4,boxwidth = 0.1)+
   geom_pcp_text(boxwidth = 0.1)




## ---- fig.width=6, fig.height=5, fig.align="center", echo=F, eval=F--------------------------------------------------------
## ggpairs(na.omit(penguins), mapping = aes(color = species),
##             columns=c(3:6),
##   lower = list(continuous =  wrap("smooth", method="lm", se=F, alpha=.5)),
##   diag = list(continuous = wrap("densityDiag", alpha=0.5 )))


## ----fig.width=6, fig.height=3, fig.align="center", echo=F, eval=F---------------------------------------------------------
## 
## ggplot(penguins, aes(vars=vars(bill_length_mm:body_mass_g), color=species))+
##         geom_pcp(alpha=.4)
## 
## 
## ggplot(penguins, aes(vars=vars(species, island, starts_with("body")), color=species))+
##   geom_pcp_box(boxwidth = 0.1, fill="grey70")+
##   geom_pcp(alpha=.4,boxwidth = 0.1)+
##   geom_pcp_text(boxwidth = 0.1)
## 


## ----fig.align="center"----------------------------------------------------------------------------------------------------
m <- read_csv("marathon.csv",na=c(""," ","NA"))
vis_miss(m)


## ----fig.align="center"----------------------------------------------------------------------------------------------------
gg_miss_upset(m[,-c(22,20)])


## ---- echo=F, eval=T,  out.width="30%",fig.align='center', fig.show='hold'-------------------------------------------------
knitr::include_graphics(c("figs/titanic.png"))
knitr::include_graphics(c("figs/cobh.png"))


## ----  fig.width=4.5, fig.height=3, fig.align="center"---------------------------------------------------------------------
#titanicanic  # a 4d array....
titanic <- as.data.frame(Titanic)
head(titanic)
titanic<- titanic[c(17:32,1:16),]

ggplot(data = titanic, aes(x = Class, y = Freq,fill=Survived)) +
  geom_col() 


## ----  fig.width=4.5, fig.height=3, fig.align="center"---------------------------------------------------------------------
ggplot(data = titanic, aes(x = Class, y = Freq,fill=Survived)) +
  geom_col(position="fill") 


## ----  fig.width=5.5, fig.height=3, fig.align="center"---------------------------------------------------------------------
ggplot(data = titanic, aes(x = Class, y = Freq,fill=Survived)) +
  geom_col(position="fill") + 
  facet_wrap( ~ Sex) + ylab("Rate")


## ----  fig.width=5.5, fig.height=4.5, fig.align="center"-------------------------------------------------------------------
ggplot(data = titanic, aes(x = Class, y = Freq,fill=Survived)) +
  geom_col(position="fill") + 
  facet_wrap( ~ Age+ Sex) + ylab("Rate")


## ----  fig.width=4.5, fig.height=4.5, fig.align="center"-------------------------------------------------------------------
titanicR <- 
titanic %>%  group_by(Class, Sex,Age) %>%
summarise(died=Freq[2], survived=Freq[1], rate=survived/sum(Freq))
titanicR

ggplot(data = titanicR, aes(x = Class, y = rate)) +
  geom_col(fill="lightblue") + 
  facet_wrap( ~ Age+ Sex) 


  


## ---- echo=F, eval=T,  out.width="50%",fig.align='center', fig.show='hold'-------------------------------------------------
knitr::include_graphics(c("figs/pcp.png"))

