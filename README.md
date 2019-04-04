# Lab-11-StatsSloths

## Team Section:
### Ariel and Rachel Regexs:
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
### Our Names:
* Kevin: Change in Proportion = -0.00742732
*Proportion in 1983 - 0.00943364
*Proportion in 2000 - 0.00652632 (Birth year)
*Proportion in 2017 - 0.00200632
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

## Team Summary:
