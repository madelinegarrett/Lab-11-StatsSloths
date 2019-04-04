# Lab-11-StatsSloths

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse) 
install.packages("babynames") 
library(babynames)
```
## Investigating The Little Mermaid Effect
* To answer our clients question about popular movies impacting number of babies being born we investigated uusing both Regex and a Graph.

* Seeing these graphs which compares Ariel names to all A names in this time we can see that there was a distinct increase in names of babys being born named Ariel following the release of The Little Mermaid.  

* While it is clear that 
```{r}

Ariel <- filter(babynames, str_detect(babynames$name, "Ar[iy]+.l+[^a]?$")) %>%
  group_by(year) %>%
  summarize(sumprop = sum(prop))

AllNames <- filter(babynames, str_detect(babynames$name, "A")) %>%
  group_by(year) %>%
  summarize(sumprop = sum(prop))

ggplot(data = Ariel) +
  geom_line(aes(x = year, y = sumprop), color = "maroon") +
  geom_vline(xintercept=1989) + 
  ggtitle("The Name Ariel Over Time")

ggplot(data = Ariel) +
  geom_line(aes(x = year, y = sumprop), color = "maroon") +
  geom_vline(xintercept=1989) + 
  ggtitle("The Name Ariel In 1988 to 1990 ")+
  coord_cartesian(xlim = c(1988, 1990))
ggplot(data = AllNames)+
    geom_line(aes(x = year, y = sumprop), color = "maroon") +
    geom_vline(xintercept=1989) +
  ggtitle("A Names Over Time")+
  coord_cartesian(xlim = c(2000, 2017))
  


```


### Ariel and Rachel Regexs:
In 1973 There where 52 observations and 2 variations of Ariel. 
* Ariel 1973 : 52 observations, 2 variations
```{r}
filter(babynames,str_detect(babynames$name,"Ar[iy]+.l+[^a]?$"),year==1973,sex=="F")%>%
  summarise(sum(n))
```
In 1988 there were 1617 observations and 7 variations of Ariel.  
* Ariel 1988 : 1617 observations, 7 variations
```{r}
filter(babynames,str_detect(babynames$name,"Ar[iy]+.l+[^a]?$"),year==1988,sex=="F")%>%
  summarise(sum(n))
```
In 1990 there were 5366 5366 observations and 8 variations of Ariel. 
* Ariel 1990 : 5366 observations, 8 variations
```{r}
filter(babynames,str_detect(babynames$name,"Ar[iy]+.l+[^a]?$"),year==1990,sex=="F")%>%
  summarise(sum(n))
```


```{r, include = FALSE}
Rachel <- filter(babynames,str_detect(babynames$name,"Ra[ey]?ch.*l"),year%in%c(1973,1988,1990)) %>% filter(name!="Rachelann") %>% filter(name!="Rachelanne")
```
In 1973 there wher 8367 observations and 14 variations of Racheal.
* Rachel 1973 : 8367 observations, 14 variations 
```{r}
filter(Rachel, year == 1973)%>%
  summarise(sum(n))
```

In 1988 there were 19999 observations and 19 variations  of Racheal. 
* Rachel 1988 : 19999 observations, 19 variations 
```{r}
filter(Rachel, year == 1988)%>%
  summarise(sum(n))
```

In 1990 there were 20407 observations and 18 variations  of Racheal.
* Rachel 1990 : 20407 observations, 18 variations 
```{r}
filter(Rachel, year == 1990)%>%
  summarise(sum(n))
```

What are the chances a girl born in 1973 would be named either Rachel or Ariel (including various versions)? In 1988? In 1990? In 2017?
### Our Names:
* Kevin: Change in Proportion = -0.00742732
* Proportion in 1983 - 0.00943364
* Proportion in 2000 - 0.00652632 (Birth year)
* Proportion in 2017 - 0.00200632
```{r}
kevin_name <- filter(babynames, str_detect(babynames$name,"^Kev[aeiouy]n$")) %>%
  group_by(year) %>%
  summarize(proportion = sum(prop)) %>%
  filter(year%in%(1983:2017))
```


## Individual Sections:
### Madeline's Section:
```{r}
mad <-  filter(babynames, str_detect(babynames$name, "Mad")) %>%
  group_by(year) %>%
  summarize(sumprop = sum(prop))
madeline <- filter(babynames, str_detect(babynames$name, "Madeline")) %>%
  group_by(year) %>%
  summarize(sumprop = sum(prop))

ggplot(data = madeline) +
  geom_line(aes(x = year, y = sumprop), color = ) +
  geom_line(data = mad, aes(x = year, y = sumprop), color = "violet")
```

### Kevin's Section:
* My name is less popular now than in 2000, my birth year.
* My name makes up most of the names that start with my first three letters.
```{r}
kev <- filter(babynames,str_detect(babynames$name,"^Kev")) %>%
  group_by(year) %>%
  summarize(proportion = sum(prop))

kevin <- filter(babynames,str_detect(babynames$name,"^Kev[aeiouy]n$"), sex=="M") %>%
  group_by(year) %>%
  summarize(proportion = sum(prop))

ggplot() +
  geom_line(data = kev, mapping = aes(x = year, y = proportion, color = "orange")) +
  geom_line(data = kevin, mapping = aes(x = year, y = proportion, color = "blue")) +
  labs(title = "Kevin Over Time", x = "Year", y = "Proportion") +
  scale_color_discrete(name = "Name", labels = c("Kev", "Kevin"))
 ```
 ### Katie's Section:
 ```{r}
Kat <- filter(babynames, str_detect(babynames$name, "^Kat")) %>%
  group_by(year) %>%
  summarise(prop = sum(prop))
Katie <- filter(babynames, str_detect(babynames$name, "^Kat[iy]"))%>%
  group_by(year) %>%
  summarise(prop = sum(prop))
ggplot(data = Katie)+
  geom_line(mapping = aes(x=year, y = prop, color = "pink"))+
  geom_line(data = Kat, mapping = aes(x = year, y = prop, color = "green"))+
  labs(title = "Katie Over Time", x = "Year", y = "Proportion")+
  scale_color_discrete(name = "Name", labels = c("Kat", "Katie"))
```

## Team Summary:
