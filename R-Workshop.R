#Workshop script of Gift for DS2F fellowship
#date of workshop is November 18 2022
#First, install and call your library


###########----
#install packages you need
install.packages("readr")
install.packages("tidyverse")
install.packages("readxl")


####-----
#load package
library(readxl)
library(tidyverse)
library(readr)



#####----
#set working directory 
setwd("/users/godsgiftnkechichukwuonye/Downloads")

#Loading data from excel sheet! Remember to install readxl
worksheet <- read_xlsx("workshop.xlsx", col_names = TRUE) #corrected mean the corrected tab in the excel sheet

#perform the head and tail function
head(worksheet) #this function provides information on the first six lines in the dataset including column names
tail(worksheet) #this function provides information on the last six lines in the dataset including column names

#perform summary statistics
summary(worksheet) #provides decriptive statistics to all the values in the dataset. Not appropriate unless you use the dollar sign to identify the single variable
summary(worksheet$`REPORTING LIMIT`) #descriptive statistics from the reporting limit only


#Data Wrangling===== 
#we will be working from the palmerpenguins dataset which is a dataset available on R
install.packages("palmerpenguins")
library(palmerpenguins)
head(penguins)
penguinsdata <- penguins #the <- represents assignment and it means we are telling R to rename penguins to penguinsdata
penguinsdata


#removing NAs: NA represents missing columns. We sometimes have missing cells in our spreadsheets. Look at
cleanpen<- na.omit(penguinsdata)
cleanpen




#Basic functions in tidyverse
#SELECT
#FILTER
#ARRANGE
#MUTATE
#SUMMARISE



glimpse(cleanpen)
#glimpse function #This is like a transposed version of print: columns run down the page, and data runs across.
#This makes it possible to see every column in a data frame. It's a little like str applied to a data frame but it tries to show you as much data as possible. (And it always shows the underlying data, even when applied to a remote data source.)



#distinct function to identify different levels of a column
distinct(cleanpen, island)

#How many levels are in the species column?
distinct(cleanpen, species)

#How many levels are in the species column?
distinct(cleanpen, year)




#short break: Two minutes


##################
#Select function------
#used for selecting specific columns from hundreds of columns and making a new dataset with it
newdata1 <- select (cleanpen, species, island, sex, year)
newdata1


#sometimes, we want to exclude only one column but keep others, we can also do that using the select function
newdata2<-select(cleanpen, -island)
newdata2

#selecting the first 6 columns and the last column from PenguinsData
penguins_data_a<- select(cleanpen, species:body_mass_g, year)
#selecting the last two columns from PenguinsData
penguins_data_b<-select(cleanpen, sex:year)

#How many observation and variables does your penguins_data_a and penguins_data_b have?

#pivot function=====
#pivot_wider: increases the number of column in the indicated cells to allow us look at the data differentially
penguins_wider1<- pivot_wider(cleanpen, names_from = sex, values_from = bill_depth_mm)
female<-na.omit(penguins_wider1$female)
summary(female)


penguins_wider2<- pivot_wider(cleanpen, names_from = island, values_from = bill_depth_mm)
torgersen<-na.omit(penguins_wider2$Torgersen)
summary(torgersen)


#pivot longer: adds multiple columns into a new column such as male and female to sex
penguins_longer<-pivot_longer(penguins_wider1,cols= male:female, names_to = "sex"
                              , values_to = "bill_depth_mm")




#FILTER FUNCTION-----
bodymasspen <- filter(cleanpen, body_mass_g >3600)
#only filters cells with bodymass greater than 3600
bodymasspen
penisland <- filter(cleanpen, island == "Dream")
penisland
#only filters the dream island from other islands
#ARRANGE FUNCTION----- 
#arranges the data set by year and bodymass
arrangedpen <- arrange(cleanpen, year, body_mass_g)
arrangedpen
#PIPING FUNCTION %>%----
#It takes the output of one function and passes it into another function as an argument. 
#This allows us to link a sequence of analysis steps.
pipedpen <- cleanpen %>%
  select(year, species, island,body_mass_g) %>%
  arrange(year)
pipedpen
summarisedpen <- cleanpen %>%
  drop_na(body_mass_g) %>%
  group_by(species) %>%
  summarise(meanbodymass = mean(body_mass_g/1000))
summarisedpen
pen_summary<-cleanpen %>%
  drop_na(body_mass_g, sex) %>%
  summarise(
    body_mass_kg_mean = mean(body_mass_g / 1000),
    body_mass_kg_min = min(body_mass_g / 1000),
    body_mass_kg_max = max(body_mass_g / 1000)
  ) %>%
  mutate(
    species_mean = mean(body_mass_kg_mean)
  )
pen_summary


#Plotting graphs in R
#tell R that year is a factor
cleanpen$year <- as.factor(cleanpen$year)

#####
#bar plot----
ggplot(cleanpen, aes(x= year)) + geom_bar()

ggplot(cleanpen, aes(x = year, fill = species)) + geom_bar()

bargraph <- ggplot(cleanpen, aes(x = year, y=bill_depth_mm, fill = island)) +
  geom_bar(stat="identity") +
  labs(y= "Bill Depth", x= "Year") 
bargraph

bargraph2 <- ggplot(cleanpen, aes(x = year, y=body_mass_g/12, fill = species)) +
  geom_bar(stat='identity') +
  labs(y= "Body Mass", x= "Year")
bargraph2

bargraph2 + coord_flip()
bargraph2 +coord_polar()

#Density Graph-----
ggplot(data=cleanpen, aes(x=body_mass_g)) + geom_density()
ggplot(data=cleanpen, aes(x=body_mass_g, fill= island)) + geom_density(alpha = 0.3)


#Boxplot------
ggplot(data=cleanpen, aes(x=year, y=body_mass_g, fill=species)) + geom_boxplot()

#scatter plots sgowing relationship between body mass and flipper length----
ScaGraph <- ggplot(data=cleanpen,  aes(body_mass_g, y=flipper_length_mm))
ScaGraph +geom_point()
ScaGraph +geom_point(aes(color=species))
ScaGraph +geom_point(aes(color=species))+
  geom_smooth()

