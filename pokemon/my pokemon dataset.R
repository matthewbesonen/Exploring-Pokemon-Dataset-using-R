library(tidyverse)
library(dplyr)
library(tibble)

#Pokemon.csv table imported
#This dataset is about all 800 pokemon and their characteristics

#What variables does this dataset comtain?
#A unique ID for each pokemon (total 800)
#Name of each pokemon
#The Pokemon's types (there's a primary type and sometimes a secondary)
#Total stats and individual stats (6 individual stats)
#What Generation the pokemon is (1-6. THe higher the number, the newer the Pokemon)
#The Pokemon's legendary status (Boolean, True or False)

#This dataset is pretty big, where do I want to start?
#Probably best to start with a smaller amount of Pokemon. I'll split up the datset
#into 6 tables, one for each generation. Then I'll focus on one.

gen1 <- filter(Pokemon, Generation == 1)
gen2 <- filter(Pokemon, Generation == 2)
gen3 <- filter(Pokemon, Generation == 3)
gen4 <- filter(Pokemon, Generation == 4)
gen5 <- filter(Pokemon, Generation == 5)
gen6 <- filter(Pokemon, Generation == 6)

#We'll look into Generation 1 Because it has the most recognizable Pokemon
#Let's try and visualize the primary type distrubution of gen1 Pokemon
gen1 %>% 
  ggplot(aes(`Type 1`)) +
  geom_bar()
#This gives me a visualization of the type distrubution, but I want
#to add some color to the bars

gen1 %>% 
  ggplot(aes(`Type 1`), fill(gen1$`Type 1`)) +
  geom_bar()
#I thought that "Fill" would add color to the bars, I must've 
#implemented it incorrectly

gen1 %>% 
  ggplot(aes(`Type 1`)) +
  geom_bar(fill = "Red")
#Fill works in geom_bar, but I want each bar to be a different color
#instead of all being the same color

gen1 %>% 
  ggplot(aes(`Type 1`, fill = 'Type 1')) +
  geom_bar()
#I tried filling by the variable, but instead got a different shade of red
#RStudio must be recognizing my 'Type 1' as a single variable, so it 
#can't fill multiple colors

gen1 %>% 
  ggplot(aes(`Type 1`, fill = gen1$`Type 1`)) +
  geom_bar()
#There it is! RStudio must've just seen 'Type 1' as a text and not the variable.
#To get the variable "Type 1", I had to first search through
#the table "gen1" using the "4"
#I like this barchart, but it would nice to order it by the count descending

gen1 %>% 
  arrange(`Type 1`) %>% 
  ggplot(aes(`Type 1`, fill = gen1$`Type 1`)) +
  geom_bar()
#arrange doesn't give me what I want. Seems to break up the 
#bars in a way I don't understand

#I'm going to create a new table that stores the count and percentage of each type
gen1_order <- gen1 %>%
  count (`Type 1`) %>%
  mutate(perc = n / nrow(gen1))
#gen1_order now stores the count and percentage of each type

gen1_order %>% 
  ggplot(aes(x = reorder(`Type 1`, -perc), y = perc, fill = gen1_order$`Type 1`)) +
  geom_bar(stat = 'identity')
#Using gen1_order, we can graph the gen1 types in descending order!
#Analysis: The most common Pokemon types in Generation 1 is Water and Normal type.
#While other types like Ice, Fairy, Dragon, and Ghost are relatively rare.

#How does Generation 1's type distribution compare to the type distribution
#of all the generations combined?
Pokemon_order <- Pokemon %>%
  count (`Type 1`) %>%
  mutate(perc = n / nrow(gen1))

Pokemon_order %>% 
  ggplot(aes(x = reorder(`Type 1`, -perc), y = perc, fill = Pokemon_order$`Type 1`)) +
  geom_bar(stat = 'identity')
#Here is the type distribution across all generations
#Analysis: 3 New Pokemon Types were added after Generation 1! 
#We can now see Dark, Flying, and Steel types.
#These types wern't represented in Generation 1.
#The new primary type Flying is the most uncommon.
#Water and Normal types continue to lead the pack as the most common.
#Types like Fairy and Ice continue to be rare, but Dragon and Ghost have
#become more common.

#What does the distribution for secondary types look like?
#Some Pokemon don't have a secondary type, so we'll have to
#remove them from the distribution
Pokemon_order2 <- Pokemon %>%
  filter(!is.na(Pokemon$`Type 2`)) %>% 
  count (`Type 2`) %>%
  mutate(perc = n / nrow(Pokemon))

Pokemon_order2 %>% 
  ggplot(aes(x = reorder(`Type 2`, -perc), y = perc, fill = Pokemon_order2$`Type 2`)) +
  geom_bar(stat = 'identity')
#Here is the Secondary type distribution for all Pokemon.
#Analysis: Flying types dominate the secondary type distribution!
#This is a stark contrast to the primary type distribution where Flying type was
#the least common. Water type is also the 5th least common secondary
#type, comapred to being the 1st most common primary type.

#I'm satisfied with the Pokemon Type visualizations.
#Now I'm interested to see the power levels of Pokemon.
#More specifically, which generation has the most powerful Pokemon?
#The 'Total' variable is the sum of all the Pokemon's stats.
#We'll use 'Total' as our measure for a Pokemon's power.

min(Pokemon$Total)
#The lowest 'Total' a Pokemon has is 180

max(Pokemon$Total)
#The highest 'Total' a Pokemon has is 780
#This makes our Range of 'Total' 180-780

mean(Pokemon$Total)
#435
#The mean 'Total' for all Pokemon is 435
#This will be the benchmark by which I compare all
#other Generations by

mean(gen1$Total)
#426
mean(gen2$Total)
#418
mean(gen3$Total)
#436
mean(gen4$Total)
#459
mean(gen5$Total)
#434
mean(gen6$Total)
#436

#let's try and visualize these 'Total' means
#I'm going to store all of these means into a new tibble
total_means <- tibble(
  Generation = c("Gen1", "Gen2", "Gen3", "Gen4", "Gen5", "Gen6"),
  Means = c(426, 418, 436, 459, 434, 436)
)
print (total_means)

#And then create a bar chart of the new tibble
total_means %>% 
  ggplot(aes(x = Generation, y = Means, fill = Generation)) +
  geom_bar(stat = "identity")
#Here's a nice visualization of the mean 'Total' values
#accross all 6 Generations
#Analysis: All the generations are very close in 'Total' means,
#The lowest (Gen2 at 418), and the highest (Gen4 at 459),
#makes the range of the means only 41 (459-418).
#Even though the range is pretty low, this still means
#Generation 4 has the highest average power, and Generation 
#2 having the lowest average power.

#I'm curious to know if Generation 4 has the highest average
# 'Total' stats because they have the most legendary Pokemon.
#First, we must check to see if Legendaries have a higher
#average 'Total' than non-legendaries.

Legendaries <- filter(Pokemon, Legendary == TRUE)
#Storing all the Legnedaries in a variable
Nonlegendaries <- filter(Pokemon, Legendary == FALSE)
#Storing all the Non-legendaries in a variable

#Now to find the mean 'Total' of Legendaries and Non-legendaries
mean(Legendaries$Total)
#637
mean(Nonlegendaries$Total)
#417

#Legendaries have a much higher mean 'Total' than Non-legendaries,
#On average 220 more 'Total'. Now we know for sure that the 
#number of legendaries a Generation has could have an impact
#on it's mean 'Total'.
#Let's count how many Legendaries each Generation has

#TRUE = Legendary, FALSE = Non-Legendary
#Rate = Legendary/Non-Legendary
gen1 %>%
  count(Legendary == TRUE)
#TRUE = 6, FALSE = 160
#Rate = 0.0375
gen2 %>% 
  count(Legendary == TRUE)
#TRUE = 5, FALSE = 101
#Rate = 0.0495
gen3 %>% 
  count(Legendary == TRUE)
#TRUE = 18, FALSE = 142
#Rate = 0.1267
gen4 %>% 
  count(Legendary == TRUE)
#TRUE = 13, FALSE = 108
#Rate = 0.1203
gen5 %>% 
  count(Legendary == TRUE)
#TRUE = 15, FALSE = 150
#Rate = 0.1000
gen6 %>% 
  count(Legendary == TRUE)
#TRUE = 8, FALSE = 74
#Rate = 0.1081

#Let's format all these Legendary rates into a tibble
Legendary_rate <- tibble(
  Generation = c("Gen1", "Gen2", "Gen3", "Gen4", "Gen5", "Gen6"),
  Rate = c(0.0375, 0.0495, 0.1267, 0.1203, 0.1000, 0.1081)
)
print(Legendary_rate)

Legendary_rate %>% 
  ggplot(aes(x = Generation, y = Rate, fill = Generation)) +
  geom_bar(stat = "identity")
#This is a nice visualization of the rate of Legendaries 
#in each Generation.
#Analysis: Generation 3 has the highest ratio of Legendary Pokemon,
#and Generation 1 has the lowest ratio.
#My theory on the ratio of Legendaries affecting the mean 'Total'
#in each Generation is partially true, as Generation 4 (the Generation
#with the highest mean 'Total') has the second highest Legendary ratio.
#And Generation 2 (the Generation with the lowest mean 'Total') has
#the second lowest Legendary ratio.

#I'm making a tibble that stores the power of each Generation and the
#rate of legendaries
legendary_power_relationship <- tibble(
  generation = c("Gen1", "Gen2", "Gen3", "Gen4", "Gen5", "Gen6"),
  power = c(426, 418, 436, 459, 434, 436), #The mean 'TOtals'
  rate = c(0.0375, 0.0495, 0.1267, 0.1203, 0.1000, 0.1081)
)
#Power = the mean 'Total'
#Rate = the rate of Legendaries

legendary_power_relationship %>% 
  ggplot(aes(x = as.factor(rate), y = as.factor(power), fill = generation)) +
  geom_bar(stat = "identity")
#Here's the best visualization I could make to represent the relationship
#between the power in each Generation and the rate of Legendaries it has.
#Analysis: The three Generations with the lowest legendary rate (1,2,5)
#have the 3 lowest power. And the three Generations with the
#highest legendary rate (6,4,3) have the 3 highest power.
#This is a clear indication that there's a positive relationship
#between the power level of a Generation and the amount
#of Legendaries in it.

#In Summary:
#We saw the Type distributions for Generation 1 and
#all Generations.
#The power ('Total' mean) distribution across all Generations.
#Legendary Pokemon distrubution and power across all Generations.
#And the relationship between mean Generation power and Legendaries.




