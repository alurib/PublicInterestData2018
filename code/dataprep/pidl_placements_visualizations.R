## Data Preparation

load("placement_history_clean.Rdata")

library(dplyr)
library(forcats)
library(tidyverse)
library(readxl)
library(tidyr)
library(lubridate)
library(gridExtra)
library(scales)

## First of all, looking at fcare_placehist which is a mega join of all dat we had so far
## we see inconsistencies in number of missing values due to a child not exiting the system.
## Number of missing values in the following variables:
## (placement) exit_why = 99 NA's
## (foster_care) dis_why = 106 NA's
## (foster_care) sys_exit = 95 NA's
## (foster_care) dis_date = 126 NA's

## This might be due to inconsistency in the data or lag in updating the information.

## Adding Race variable to both care_placement and fcare_placehist

care_placement <- care_placement %>% 
  mutate(race_white = ifelse(is.na(race_white),0,race_white),
         race_black = ifelse(is.na(race_black),0,race_black),
         race2 = factor(ifelse(race_white == 1, "White","Other")),
         race3 = factor(ifelse(race_black == 1, "Black", ifelse(race=="White","White","Other"))),
         race4 = factor(ifelse(race=="White", "White", ifelse(race=="Black", "Black", ifelse(race == "Multi-Race", "Multi-Race","Other"))))
  )

fcare_placehist <- fcare_placehist %>% 
  mutate(race_white = ifelse(is.na(race_white),0,race_white),
         race_black = ifelse(is.na(race_black),0,race_black),
         race2 = factor(ifelse(race_white == 1, "White","Other")),
         race3 = factor(ifelse(race_black == 1, "Black", ifelse(race=="White","White","Other"))),
         race4 = factor(ifelse(race=="White", "White", ifelse(race=="Black", "Black", ifelse(race == "Multi-Race", "Multi-Race","Other"))))
  )

#---------------------------------------------------------------------------------

## Analysis on Initial Placement Type

## Getting the initial placement for each client in the data
fcare_placehist <- fcare_placehist %>% 
  mutate(client_id = as.numeric(as.character(client_id)))
initial_placement <- fcare_placehist[!duplicated(fcare_placehist[2]),]

## Plot of Placements Resource Category

ggplot(initial_placement, aes(place_resource_cat))+
  geom_bar( color = "black")+
  labs(y = "Number of Cases", x = "Placement Resource Category")+
  ggtitle("Number of Cases by Placement Resource Categories")

## The plot below shows the placement resource category by race while controlling for
## disproportionality (shows proportions in each bin by category of race).
ggplot(initial_placement, aes(place_resource_cat, fill = race2))+
  geom_bar(aes(y = ..prop.., fill = race2, group = race2), stat= "count", position = "dodge")+
  labs(y = "Number of Cases", x = "Placement Resource Category", fill = "Race")+
  ggtitle("Number of Cases by Placement Resource Categories")


#----------------------------------------------------------------------------------

## Analysis on Number of Placements

## This is a histogram for number of placements per client
ggplot(care_placement, aes(num_placements_client))+
  geom_histogram(position = "dodge", color = "black", binwidth = 1)+
  scale_x_continuous( breaks = seq(1,50,1))+
  labs(y = "Number of Cases", x = "Number of Placements")+
  ggtitle("Number of Cases by Number of Placements per Client")

## The plot is skewed meaning linear regression has to be done on 
## log transformed num_placements_client variable

## The plot below shows the number of placements per client by race while controlling for
## disproportionality (shows proportions in each bin by category of race).
ggplot(care_placement, aes(x = num_placements_client))+
  geom_freqpoly(aes(y= ..density.., group = race2, color = race2),
                binwidth = 1)+
  scale_x_continuous(breaks = seq(0,50,5))+
  scale_y_continuous(labels = percent)+
  labs(y = "Proportion by Race", x = "Number of Placements", fill = "Race")+
  ggtitle("Number of Placements per Client by Race")

## The plot below is the same version of plot above but without controlling for 
## disproportionality by races (Shows cases instead of proportions)

#ggplot(care_placement, aes(num_placements_client, color = race2))+
#  geom_freqpoly( binwidth = 1)+
#  scale_x_continuous( breaks = seq(0,50,1))+
#  labs(y = "Number of Cases", x = "Number of Placements", color = "Race")+
#  ggtitle("Number of Cases by Number of Placements per Client")



#--------------------------------------------------------------------------

## Analysis on Time in Care Overall

ggplot(care_placement, aes(time_in_care))+
  geom_histogram(position = "dodge", color = "black", binwidth = 54)+
  scale_x_continuous(breaks = seq(0,1000, 52) )+
  labs(y = "Number of Cases", x = "Total Time in Care (in weeks)")+
  ggtitle("Number of Cases by Time Spent in Care")

ggplot(care_placement, aes(time_in_care))+
  geom_freqpoly(aes(y = 52*..density.. , group = race2, color = race2), binwidth = 52)+
  scale_x_continuous(breaks = seq(0,1000, 52))+
  scale_y_continuous(labels = percent)+
  labs(y = "Proportion by Race", x = "Total Time in Care (in weeks)", color = "Race")+
  ggtitle("Time Spent in Care by Race")

