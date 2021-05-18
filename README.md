# Larhud
Repositório dos testes feitos para o LARHUD/IBICT/Ministério_da_tecnologia-UFRJ
---
title: "Larhud_teste"
author: "Jessica_Luzes"
date: "17/05/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

#install.packages("googlesheets4")
library(googlesheets4)
#install.packages("devtools")
devtools::install_github("tidyverse/googlesheets4")

googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1a9j3TqsOY5_rXt1T0Ch_Z7tbLTiEWFX9/edit#gid=1316062403")

```


```{r}
install.packages("ggplot2")
install.packages("dplyr")
install.packages("magrittr")
install.packages(c("readxl","writexl")) 
install.packages("janitor")
install.packages("plyr")
install.packages("ggthemes")
install.packages("readr")
```


```{r}
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggplot2)
library(dplyr)
library(readxl) 
library(writexl)
library(janitor)
library(readr)
library(tidyverse)
library(rio)
library(here)
```


```{r}
dados <- read.csv("Timeline Covid-19 - versão integral.xlsx - od1.csv")
```


```{r}
dados<-clean_names(dados)
clean_names(dados)
str(dados)
```

```{r}
colnames(dados)

```


```{r}
dados1 <- dados %>% 
  rename(Titulo = headline,  Jornais = media_credit, 
         Ano= year, Dia = day, Mês = month) %>% 
  select(Titulo, Jornais, Ano, Dia, Mês)
```

```{r}
colnames(dados1)

```
```{r}
dados1<-clean_names(dados1)
```

```{r}
dados1
```

```{r}
colnames(dados1)
```
```{r}
g2 <- dados1 %>%
  group_by(ano, mes) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ungroup()
```


```{r}
g2
```


```{r}
g3<-g2 %>% mutate(total=sum(n), prop=n/total)
```


```{r}
g3
```

```{r}
g3$mes <- as.factor(g3$mes)

```


```{r}

ggplot(data=g3, aes(x=prop, y=mes)) +
  geom_bar(stat="identity", position="dodge") +
  geom_label(aes(label =round(prop,3), group = mes),
             colour = "blue", position = position_dodge(width=1))+
  labs(y = 'Meses',
       x = 'Proporção_de_publicações_por_meses')+
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw(10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  ylab("Meses") +
      facet_wrap(~ano)

#Tratamento - Na nossa análise não existe NA, indicando erro...Observar dados sem ano...


```


```{r}
#Usar a função para tirar todos os nas
g4 <- g3 %>% 
  drop_na()

```

```{r}
names(g3)
```


```{r}
ggplot(data=g4, aes(x=prop, y=mes)) +
  geom_bar(stat="identity", position="dodge") +
  geom_label(aes(label =round(prop,3), group = mes),
             colour = "blue", position = position_dodge(width=1))+
  labs(y = 'Meses',
       x = 'Proporção_de_publicações_por_meses')+
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw(10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  ylab("Meses") +
      facet_wrap(~ano)

```



```{r}
g8 <- dados1 %>%
  group_by(ano, jornais, ano, mes) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n) * 100) %>% 
  ungroup()

```


```{r}
ggplot(g2, aes(x =  , y = n, fill = continentes, label = round(n, 1))) +
  geom_col() +
  geom_text(position = position_stack(vjust = 0.5)) +
  facet_wrap(~grandes_areas )+
  coord_flip()

```

