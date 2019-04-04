# Lab-11-StatsSloths

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse) 
install.packages("babynames") 
library(babynames)
```
## Investigating The Little Mermaid Effect
* To answer our clients question about popular movies impacting number of babies being born we investigated uusing both Regex and a Graph.

```{r}

Ariel <- filter(babynames, str_detect(babynames$name, "Ar[iy]+.l+[^a]?$")) %>%
  group_by(year) %>%
  summarize(sumprop = sum(prop))


ggplot(data = Ariel) +
  geom_line(aes(x = year, y = sumprop), color = "maroon") +
  geom_vline(xintercept=1989) +
  ggtitle("The Name Ariel Over Time")
```


### Ariel and Rachel Regexs:
In 1973 There where 52 versions of Ariel. 
* Ariel 1973 : 52
```{r}
filter(babynames,str_detect(babynames$name,"Ar[iy]+.l+[^a]?$"),year==1973,sex=="F")%>%
  summarise(sum(n))
```
In 1988 there were 1617 versions of Ariel.  
* Ariel 1988 : 1617
```{r}
filter(babynames,str_detect(babynames$name,"Ar[iy]+.l+[^a]?$"),year==1988,sex=="F")%>%
  summarise(sum(n))
```
In 1990 there were 5366 versions of Ariel. 
* Ariel 1990 : 5366
```{r}
filter(babynames,str_detect(babynames$name,"Ar[iy]+.l+[^a]?$"),year==1990,sex=="F")%>%
  summarise(sum(n))
```


```{r, include = FALSE}
Rachel <- filter(babynames,str_detect(babynames$name,"Ra[ey]?ch.*l"),year%in%c(1973,1988,1990)) %>% filter(name!="Rachelann") %>% filter(name!="Rachelanne")
```
In 1973 there wher 8367 versions of Racheal.
* Rachel 1973 : 8367
```{r}
filter(Rachel, year == 1973)%>%
  summarise(sum(n))
```

In 1988 there were 19999 versions of Racheal. 
* Rachel 1988 : 19999
```{r}
filter(Rachel, year == 1988)%>%
  summarise(sum(n))
```

In 1990 there were 20407 versions of Racheal.
* Rachel 1990 : 20407
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
