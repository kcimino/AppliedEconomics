library(tidyverse)
library(desc)
library(survey)
library(ggplot2)
library(foreign)
library(viridis)

# load data
init <- read.csv("ESS1-9e01_1.csv")

# create binary satdem, recode NAs, select only columns of interest, create satdem which is a binary
mydata <- init %>% 
  mutate(
    satecon = ifelse(stfeco > 5 & stfeco <11, 1, ifelse(stfeco == 5 | stfeco > 10, NA, 0)),
    stfdem = factor(ifelse(stfdem>10, NA, stfdem)),
    stfeco = factor(ifelse(stfeco>10, NA, stfeco)),
    hinctnta = factor(ifelse(hinctnta >10, NA, hinctnta)),
    hincfel = factor(ifelse(hincfel > 4, NA, hincfel))
  ) %>% 
  filter(ctzcntr==1 & !is.na(stfdem) & !is.na(satecon)) %>% 
  select(anweight, idno, stfeco, stfdem, hinctnta, hincfel, satecon)


# create design and weight appropriately (as guided by ESS documentation)
weights <- read.dta("ESS8SDDFe01_1.dta")
weights <- weights %>% filter(cntry == "RU")
weights.all <- left_join(mydata, weights, by = "idno")
rm("init")
design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, data = weights.all)
rm("weights.all")

# create model and get information from it
model.weighted <- svyglm(satecon~stfdem+hinctnta+hincfel, design=design)
summary(model.weighted)


# get r^2 because it's not available in survey package

svyvar(~satecon, design)
r.squared <- 1 - 0.1288195 / 0.18963
adj.r.squared <- 1 - (1 - r.squared) * ((2430 - 1)/(2430-3-1))


# plotting important ones

mydata %>% 
  ggplot(aes(x=factor(satecon), fill=factor(stfdem)))+
  geom_bar(position="fill") +
  scale_fill_viridis(discrete=TRUE)+
  labs(
    title= "Correlation Between sateco and stfdem",
    x = "Satisfaction With Economy",
    y = "Percentage",
    fill = "Satisfaction With Democracy"
  )+
  scale_x_discrete( 
    limits = c(factor(0) , factor(1)),
    labels = c("No", "Yes"))

mydata %>% 
  ggplot(aes(x=stfdem))+
  geom_bar(fill = "#2ca25f")+
  labs(
    title = "Distribution of Responses to stfdem",
    x = "Level of satisfaction with state of democracy"
  )

mydata %>% 
  ggplot(aes(x=satecon))+
  geom_bar(fill = "#2ca25f")+
  labs(
    title = "Distribution of Responses to stfeco",
    x="satisfied with economy")+
  scale_x_discrete( 
    limits = c(0 , 1),
    labels = c("No", "Yes"))

