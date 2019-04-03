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
```{r}
Rachel <- filter(babynames,str_detect(babynames$name,"Ra[ey]?ch.*l"),year%in%c(1973,1988,1990)) %>% filter(name!="Rachelann") %>% filter(name!="Rachelanne")
```
* Rachel 1973 : 8367
```{r}
filter(Rachel, year == 1973)%>%
  summarise(sum(n))
```
* Rachel 1988 : 19999
```{r}
filter(Rachel, year == 1988)%>%
  summarise(sum(n))
```
* Rachel 1990 : 20407
```{r}
filter(Rachel, year == 1990)%>%
  summarise(sum(n))
```

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
