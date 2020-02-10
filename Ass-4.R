install.packages("ggplot2")
install.packages("psych")
install.packages("zeallot")
library(ggplot2)
require(reshape2)
library(ggthemes)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(lubridate)
library(readxl)
library(gridExtra)
library(grid)
#ITU_Key_2005_2019_ICT_data_with_LDCs_28Oct2019_Final <- read_excel("C:/Users/chat2/Downloads/ITU_Key_2005-2019_ICT_data_with LDCs_28Oct2019_Final.xls", range = cell_cols("A:P"),skip = 3)

View(ITU_Key_2005_2019_ICT_data_with_LDCs_28Oct2019_Final)
df<-ITU_Key_2005_2019_ICT_data_with_LDCs_28Oct2019_Final[-c(1,2),]

colnames(df) = df[1, ] # the first row will be the header
df = df[-1, ]  
  #2005 %>% select(df.2005)

fixed_telephone_sub<-df[2:5,]
mobile_cellular_telephone_sub<-df[8:11,]
active_mobile_broadbans_sub<-df[14:17,]
fixed_broadband_sub<-df[20:23,]
population_coveredby_cellular<-df[26:29,]
population_coveredby_3g<-df[32:35,]
population_covereby_lte<-df[38:41,]
international_band_width_usage<-df[48:51,]
individuals_using_internet <-df[69:72,]
#View(fixed_telephone_sub)
#fx<-select(fixed_telephone_sub,-c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))
#view(fx)
view(fixed_telephone_sub)
names(fixed_telephone_sub)[1]<- "Indicators"
names(mobile_cellular_telephone_sub)[1]<- "Indicators"
names(active_mobile_broadbans_sub)[1]<- "Indicators"
names(fixed_broadband_sub)[1]<- "Indicators"
names(population_coveredby_cellular)[1]<- "Indicators"
names(population_coveredby_3g)[1]<- "Indicators"
names(population_covereby_lte)[1]<- "Indicators"
names(international_band_width_usage)[1]<- "Indicators"
names(individuals_using_internet)[1] <- "Indicators"
view(new_fixed_telephone_sub)

new_fixed_telephone_sub<- select(fixed_telephone_sub, -c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))
new_mobile_cellular_telephone_sub<- select(mobile_cellular_telephone_sub, -c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))
new_active_mobile_broadbans_sub<- select(active_mobile_broadbans_sub, -c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))
new_fixed_broadband_sub<- select(fixed_broadband_sub, -c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))
new_population_coveredby_cellular<- select(population_coveredby_cellular, -c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))
new_population_coveredby_3g<- select(population_coveredby_3g, -c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))
new_population_covereby_lte<- select(population_covereby_lte, -c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))
new_international_band_width_usage <- select(international_band_width_usage, -c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))
new_individuals_using_internet <- select(individuals_using_internet, -c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))

view(new_fixed_telephone_sub)

row.names(new_fixed_telephone_sub)
new_fixed_telephone_sub_melt <- melt(new_fixed_telephone_sub,"Indicators")
new_mobile_cellular_sub_melt <- melt(new_mobile_cellular_telephone_sub,"Indicators")

g1 <- ggplot(new_fixed_telephone_sub_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Fixed Telephone Subscribers')
g1 
g2 <- ggplot(new_mobile_cellular_sub_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Mobile Cellular Subscribers')
g1
grid.arrange(g1, g2, nrow = 1,top='Comparison Chart')
g3 <- ggplot(new_fixed_telephone_sub_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Fixed Telephone Subscribers')
g3
g4 <- ggplot(new_mobile_cellular_sub_melt, aes(x = factor(variable),  y=as.double(value)))+ 
  geom_bar(aes(fill=Indicators),stat="identity")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Mobile Cellular Subscribers')

grid.arrange(g3, g4, nrow = 1,top="part-in-whole")

Xg5=factor(new_fixed_telephone_sub_melt$variable)
Yg5=as.double(new_fixed_telephone_sub_melt$value)

g5 <- ggplot(new_fixed_telephone_sub_melt, aes(x =factor(variable),levels = Yg5,group=Yg5,  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Fixed Telephone Subscribers')
Xg6=factor(new_mobile_cellular_sub_melt$variable)
Yg6=as.double(new_mobile_cellular_sub_melt$value)

g6 <- ggplot(new_mobile_cellular_sub_melt, aes(x = factor(variable),levels = Yg6,group=Yg6,  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Mobile Cellular Subscribers')

grid.arrange(g5, g6, nrow = 1,top="Ranking")

new_active_mobile_broadbans_sub_melt <- melt(new_active_mobile_broadbans_sub,"Indicators")
new_fixed_broadband_sub_melt <- melt(new_fixed_broadband_sub,"Indicators")

gb1 <- ggplot(new_active_mobile_broadbans_sub_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Active Mobile Broadband Subscribers')

gb2 <- ggplot(new_fixed_broadband_sub_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Fixed Broadband  Subscribers')

grid.arrange(gb1, gb2, nrow = 1,top='Comparison Chart')

gb3 <- ggplot(new_active_mobile_broadbans_sub_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Active Mobile Broadband Subscribers')

gb4 <- ggplot(new_fixed_broadband_sub_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Fixed Broadband  Subscribers')

grid.arrange(gb3, gb4, nrow = 1,top="part-in-whole")

Xgb5=factor(new_active_mobile_broadbans_sub_melt$variable)
Ygb5=as.double(new_active_mobile_broadbans_sub_melt$value)

gb5 <- ggplot(new_active_mobile_broadbans_sub_melt, aes(x =factor(variable),levels = Ygb5,group=Ygb5,  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Active Mobile Broadband Subscribers')


Xgb6=factor(new_mobile_cellular_sub_melt$variable)
Ygb6=as.double(new_mobile_cellular_sub_melt$value)

gb6 <- ggplot(new_fixed_broadband_sub_melt, aes(x = factor(variable),levels = Ygb6,group=Ygb6,  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Fixed Broadband Subscribers')

grid.arrange(gb5, gb6, nrow = 1,top="Ranking")

new_population_coveredby_cellular_melt <- melt(new_population_coveredby_cellular,"Indicators")
new_population_covereby_lte_melt <- melt(new_population_covereby_lte,"Indicators")

gc1 <- ggplot(new_population_coveredby_cellular_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Population Covered by Cellular Network')

gc2 <- ggplot(new_population_covereby_lte_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Population Covered by LTE/WIMAX')

grid.arrange(gc1, gc2, nrow = 1,top='Comparison Chart')

gc3 <- ggplot(new_population_coveredby_cellular_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Population Covered by Cellular Network')

gc4 <- ggplot(new_population_covereby_lte_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Population Covered by LTE/WIMAX')

grid.arrange(gc3, gc4, nrow = 1, top="part-in-whole")

Xgc5=factor(new_population_coveredby_cellular_melt$variable)
Ygc5=as.double(new_population_coveredby_cellular_melt$value)

gc5 <- ggplot(new_population_coveredby_cellular_melt, aes(x =factor(variable),levels = Ygc5,group=Ygc5,  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Population Covered by Cellular Network')


Xgc6=factor(new_population_covereby_lte_melt$variable)
Ygc6=as.double(new_population_covereby_lte_melt$value)

gc6 <- ggplot(new_population_covereby_lte_melt, aes(x = factor(variable),levels = Ygc6,group=Ygc6,  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Population Covered by LTE/WIMAX')

grid.arrange(gc5, gc6, nrow = 1,top="Ranking")



new_international_band_width_usage_melt <- melt(new_international_band_width_usage,"Indicators")

gd1 <- ggplot(new_international_band_width_usage_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('International Bandwidth Usage')

grid.arrange(gd1,top='Comparison Chart')
gd2 <- ggplot(new_international_band_width_usage_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('International Bandwidth Usage')
grid.arrange(gd2,top="part-in-whole")
Xgd=factor(new_international_band_width_usage_melt$variable)
Ygd=as.double(new_international_band_width_usage_melt$value)

gd3 <- ggplot(new_international_band_width_usage_melt, aes(x =factor(variable),levels = Ygd,group=Ygd,  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Individuals Using Internet')

grid.arrange(gd3,top="Ranking")
new_individuals_using_internet_melt <- melt(new_individuals_using_internet,"Indicators")

ge1 <- ggplot(new_individuals_using_internet_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Individuals Using Internet')

grid.arrange(ge1,top='Comparison Chart')
ge2 <- ggplot(new_individuals_using_internet_melt, aes(x = factor(variable),  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('International Bandwidth Usage')
grid.arrange(ge2,top="part-in-whole")

Xge=factor(new_individuals_using_internet_melt$variable)
Yge=as.double(new_individuals_using_internet_melt$value)

ge3 <- ggplot(new_individuals_using_internet_melt, aes(x =factor(variable),levels = Yge,group=Yge,  y=as.double(value))) +
  geom_bar(aes(fill=Indicators),stat="identity", position ="dodge2")+xlab('Year')+ylab('InHabitants (in millions)')+ggtitle('Individuals Using Internet')

grid.arrange(ge3,top="Ranking")



