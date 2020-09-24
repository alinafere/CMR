## Consumer Marketing Research: Second R Session
## Author: Pieter Schoonees

# Setup -----------------------------------------------------------------------------

## Packages
library("tidyverse")
library("haven")
library("colourpicker")
library("ggthemes")

# Recap -----------------------------------------------------------------------------
## Load the Prestige data from file in current working directory
load("Prestige.RData") 

## Importing SPSS data
library("haven")
session2 <- read_sav("Session 5 example employees.sav")
head(session2)
dim(session2)
## Recap of last week
head(Prestige)
tail(Prestige)
summary(Prestige)
ggplot(Prestige, aes(x = prestige, y = logincome)) + geom_point()
ggplot(Prestige, aes(x = type, y = prestige)) + geom_boxplot()
ggplot(Prestige, aes(x = prestige)) + geom_histogram(bins = 15)
ggplot(Prestige, aes(x = type)) + geom_bar()

# Customizing ggplot2 graphics ------------------------------------------------------

## Load and explore the VW Golf data
load("vwgolf.RData")
View(vwgolf)
dim(vwgolf)
summary(vwgolf)

## Add geom
ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice)) + geom_point()
ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice)) + 
  geom_point() + geom_smooth(method = "lm")


## Colours
ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice)) + 
  geom_point( )

c("aquamarine3", "cornflowerblue", "cornsilk")

colours()

ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice)) + 
  geom_point(colour = "aquamarine3")

ggplot(vwgolf, aes(x = AskingPrice)) + 
  geom_density(fill = "orangered2")

## More finetuning
ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice)) + 
  geom_point(colour = "black", fill = "chartreuse3", shape = 21, 
             size = 3)

ggplot(vwgolf, aes(x = AskingPrice)) + 
  geom_density(fill = "bluec("white", "white", "white")", colour = "red", linetype = 5)

## Mapping aesthetics to variables
ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice, fill = Fuel)) + 
  geom_point(shape = 21, size = 3)

ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice, size = TopSpeed)) + 
  geom_point(shape = 21, fill = "turquoise3")

ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice, colour = NrOwners)) + 
  geom_point(size = 3)

ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice, 
                   colour = NrOwners, size = TopSpeed)) + 
  geom_point()

ggplot(vwgolf, aes(x = Mileage, fill = Fuel)) + 
  geom_density(alpha = 0.5)

## Manual scales
ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice, shape = Fuel)) + 
  geom_point(size = 3) + scale_shape_manual(values = c("B", "D"))

## Legends
ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice, shape = Fuel)) + 
  geom_point(size = 3, show.legend = FALSE) + 
  scale_shape_manual(values = c("B", "D"))

## Manual scales
ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice, colour = Imported)) + 
  geom_point() + scale_colour_manual(values = c("#1777E6", "#EB934C"))

ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice, fill = NrOwners)) + 
  geom_point(shape = 21, size = 3) + 
  scale_fill_gradient(low = "white", high = "magenta")

## Facets
ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice)) + 
  geom_point() + facet_grid(Class ~ Colour)

ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice)) + 
  geom_point() + facet_grid(Class ~ .)

ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice)) + 
  geom_point() + facet_grid(. ~ Colour)

## Themes
ggplot(vwgolf, aes(x = Mileage, fill = Fuel)) + 
  geom_density(alpha = 0.5) + theme_bw()

ggplot(vwgolf, aes(x = Mileage, fill = Fuel)) + 
  geom_density(alpha = 0.5) + theme_economist() +
  scale_fill_economist() 

ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice, colour = Fuel)) + 
  geom_point() + theme_wsj() + scale_colour_wsj()


# Data manipulation with dplyr ------------------------------------------------------

## Filtering observations
filter(vwgolf, Class == "Stationwagon")

filter(vwgolf, Colour == "Black")

filter(vwgolf, Colour == "Black", Class == "Stationwagon")

filter(vwgolf, Mileage <= 50000)

filter(vwgolf, is.na(Mileage))

filter(vwgolf, Mileage <= 50000 | is.na(Mileage))

filter(vwgolf, Fuel == "Diesel" & NrOwners < 3)
filter(vwgolf, Fuel == "Diesel", NrOwners < 3)

filter(vwgolf, Fuel == "Benzine" & !PrivateLastOwner)

## Selecting columns
PrestigeNum <- select(Prestige,
                      education, logincome, women, prestige)
PrestigeNum <- select(Prestige, education:prestige)
PrestigeNum <- select(Prestige, -type)

plot(PrestigeNum)

select(vwgolf, PriceNew, AskingPrice, Mileage)

select(vwgolf, Class:Colour)

select(vwgolf, AskingPrice, PriceNew, everything())

## Arranging rows
arrange(vwgolf, AskingPrice)

arrange(vwgolf, desc(AskingPrice))

arrange(vwgolf, Fuel, desc(AskingPrice))

## Mutating data frames
mutate(vwgolf, PriceDifference = PriceNew - AskingPrice)

mutate(vwgolf, Mileage = Mileage / 1000)

mutate(vwgolf, PriceDifference = PriceNew - AskingPrice, 
       Mileage = Mileage / 1000)

## Summarizing data
summarize(vwgolf,
          NumberOfCars = length(AskingPrice), 
          MeanPrice = mean(AskingPrice),
          MinPrice = min(AskingPrice),
          MaxPrice = max(AskingPrice),
          SDPrice = sd(AskingPrice))

summarize(vwgolf,
          MeanMileage = mean(Mileage, na.rm = TRUE),
          MaxMileage = max(Mileage, na.rm = TRUE), 
          MinMileage = min(Mileage, na.rm = TRUE))

summarize(group_by(vwgolf, Colour),
          NumberOfCars = length(AskingPrice), 
          MeanPrice = mean(AskingPrice),
          MinPrice = min(AskingPrice),
          MaxPrice = max(AskingPrice),
          SDPrice = sd(AskingPrice))

summarize(group_by(vwgolf, Colour, Fuel),
          MeanMileage = mean(Mileage, na.rm = TRUE),
          MaxMileage = max(Mileage, na.rm = TRUE), 
          MinMileage = min(Mileage, na.rm = TRUE))

## Putting it all together
summarize(
  group_by(
    mutate(
      select(
        filter(vwgolf, Fuel == "Diesel"), 
        AskingPrice, PriceNew, Mileage, Colour
      ),
      PriceDifference = PriceNew - AskingPrice
    ),
    Colour
  ),
  MeanPriceDifference = mean(PriceDifference),
  MeanMileage = mean(Mileage)
)

## Piping
vwgolf %>% 
  filter(Fuel == "Diesel") %>% 
  select(AskingPrice, PriceNew, Mileage, Colour) %>% 
  mutate(PriceDifference = PriceNew - AskingPrice) %>% 
  group_by(Colour) %>% 
  summarize(MeanPriceDifference = mean(PriceDifference), 
            MeanMileage = mean(Mileage))

## Storing the results
vwgolf_summary <- vwgolf %>% 
  filter(Fuel == "Diesel") %>% 
  select(AskingPrice, PriceNew, Mileage, Colour) %>% 
  mutate(PriceDifference = PriceNew - AskingPrice) %>% 
  group_by(Colour) %>% 
  summarize(MeanPriceDifference = mean(PriceDifference), 
            MeanMileage = mean(Mileage))

# Exercises -------------------------------------------------------------------------

## ----ex1.1
ggplot(vwgolf, aes(x = Mileage, y = AskingPrice)) + geom_point()

## ----ex1.2
ggplot(vwgolf, aes(x = Mileage, y = PriceNew - AskingPrice)) +
  geom_point()

## ----ex1.3
ggplot(vwgolf, aes(x = Mileage)) + geom_histogram(bins = 25)
ggplot(vwgolf, aes(x = Mileage)) + geom_density()

## ----ex1.4
ggplot(vwgolf, aes(x = Fuel, y = Mileage)) + geom_boxplot()

## ----ex2-1
ggplot(vwgolf, aes(x = Mileage, y = AskingPrice)) +
  geom_point(size = 3, shape = 22, fill = "purple")

## ----ex2-2
ggplot(vwgolf, aes(x = Mileage)) +
  geom_histogram(bins = 30, linetype = "dashed", colour = "red",
                 fill = "red", alpha = 0.5)

## ----ex3-1
ggplot(vwgolf, aes(x = PriceNew, y = AskingPrice, size = TopSpeed, fill = Mileage)) +
  geom_point(shape = 21)

## ----ex3-2
ggplot(vwgolf, aes(y = PriceNew, x = Colour, fill = Colour)) +
  geom_boxplot()

## ----ex4-1
ggplot(vwgolf, aes(y = AskingPrice, x = PriceNew, size = TopSpeed,
                   fill = Mileage)) + geom_point(shape = 21)

## ----ex4-2
price_plot <- ggplot(vwgolf,
                     aes(y = AskingPrice, x = PriceNew,
                         size = TopSpeed, fill = Mileage)) +
  geom_point(shape = 21) +
  scale_fill_continuous(low = "white", high = "red")
price_plot

## ----ex5-1
price_plot <- price_plot +
  labs(title = "Asking Price versus New Price",
      subtitle = "56 second-hand VW Golfs from Marktplaats.nl")
price_plot

## ----ex5-2
price_plot <- price_plot +
  labs(x = "New Price (euro)", y = "Asking Price (euro)")
price_plot

## ----ex5-3
price_plot <- price_plot + coord_equal()
price_plot
