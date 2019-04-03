# Lab-11-StatsSloths

* Ariel 1973 : 52
```{r}
filter(babynames,str_detect(babynames$name,"Ar[iy]+.l+[^a]?$"),year==1973,sex=="F")%>%
  summarise(sum(n))
```
* Ariel 1988 : 1617
```{r}
filter(babynames,str_detect(babynames$name,"Ar[iy]+.l+[^a]?$"),year==1988,sex=="F")%>%
  summarise(sum(n))
```
* Ariel 1990 : 5366
```{r}
filter(babynames,str_detect(babynames$name,"Ar[iy]+.l+[^a]?$"),year==1990,sex=="F")%>%
  summarise(sum(n))
```
