---
title: "R Notebook"
output: html_notebook
---
## Baseball history analysis


Importing the nexessary libraries
```{r}
library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra)
options(warn=-1)
```
```{r}
salary<-read.csv('salary.csv')
head(salary)
str(salary)
```
```{r}
player<-read.csv('player.csv')
head(player)
str(player)
```
```{r}
options(repr.plot.width=5, repr.plot.height=3)
p1<-player %>%ggplot(aes(x=weight))+geom_histogram(fill="navyblue")
p2<-player %>%ggplot(aes(x=height))+geom_histogram(fill="navyblue")
grid.arrange(p1,p2,nrow=1,ncol=2,top="Distribution of palyers Weight and Height")
```
Weight of the players was measured in pounds and mostly weight ranges from 150 to 200 pounds. Height was measured in inches, most players has height 65 to 75.


Data Manipulation
Most of the date columns are factors, so convert it into date format. Also do computation on birth date and debut.
```{r}
player$debut<-as.Date(player$debut,"%Y-%m-%d")
player$final_game<-as.Date(player$final_game,"%Y-%m-%d")
player$dob <- as.Date(with(player, paste(birth_year, birth_month, birth_day,sep="-")), "%Y-%m-%d")
player$dage<-round(as.numeric(difftime(player$debut, player$dob,  unit="weeks"))/52.25,0)
head(player$dage)
```

```{r}
options(repr.plot.width=5, repr.plot.height=4)
player %>% select(name_given,dage)%>% arrange(desc(dage))%>%ggplot(aes(x=dage))+geom_density(fill="red4",alpha=0.5)+scale_x_continuous(limits=c(0,50))+labs(x="Debut_Age")
```

Most of the players started at the age of 22 to 25, Minimum age was at 16 and max age age was 36.

```{r}
player$years_played<-round(as.numeric(difftime(player$final_game, player$debut,  unit="weeks"))/52.25,0)
options(repr.plot.width=5, repr.plot.height=4)
player %>% select(name_given,years_played)%>% arrange(desc(years_played)) %>% filter(years_played>=25)%>%ggplot(aes(x=factor(name_given,levels=name_given),y=years_played,fill=years_played))+
geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90),legend.position="none")+labs(x="Player",title="Players who spent more than 25 years")+scale_fill_gradient(low = "#782B43", high = "#16B1F7")+
geom_hline(aes(yintercept=mean(years_played)),col="red",linetype="dashed")
```

```{r}
player %>%select(birth_country)%>%group_by(birth_country)%>%summarise(pcount=n())%>%arrange(desc(pcount))%>%filter(pcount>=10)%>%ggplot(aes(x=factor(birth_country,levels=birth_country),y=pcount))+geom_col(fill="brown")+theme(axis.text.x=element_text(angle=90))+labs(x="Country",y="No of Players",title="Players per country")
```
USA has got more number of baseball players , which was not at all comparable to other countries.



