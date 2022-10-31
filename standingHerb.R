standing <- read_csv("StandingHerbivoryDataMila.xlsx - Sheet1.csv")

standing$LeafWidth <- as.numeric(standing$LeafWidth)
standing$LeafLength <- as.numeric(standing$LeafLength)
standing$UH <- as.numeric(standing$UH)

# calculate the leaf area
standing <- standing %>% mutate(LeafArea = pi * LeafWidth / 2 * LeafLength / 2)

# calculate total herbivory
standing <- standing %>% mutate(totalHerb = UH + RH)

# calculate herbivory percentage
standing <- standing %>% mutate(uhHerbPerc = UH / LeafArea)
standing <- standing %>% mutate(rhHerbPerc = RH / LeafArea)

# what percent of leaves have rolled herbivory
ggplot(standing, aes(LeafNo, rhHerbPerc)) + 
  geom_boxplot()  + 
  scale_y_continuous(trans='log10')

# yes unrolled herb or no
standing <- standing %>% mutate(uhPresence = case_when(UH <= 0 ~ "none", UH > 0 ~ "some"))
standing %>% group_by(Sp, uhPresence) %>% summarize(n=n())

# remove the NA and unknown species
standing <- standing %>% filter(Sp == "giga"| Sp == "scaber"| Sp == "velutia"| Sp == "stricta")

# now calculate the percentage
105 / 215
137 / (78 + 137)
110 / (38 + 110)
101 / (101 + 62)

# rolled herb?
standing <- standing %>% mutate(rhPresence = case_when(RH <= 0 ~ "none", RH > 0 ~ "some"))

standing %>% group_by(Sp) %>% summarize(meanRolledHerbPerPlant = mean(rhHerbPerc, na.rm = T), 
                                        meanUnrolledHerbPerPlant = mean(uhHerbPerc, na.rm = T), )

# viz
standing %>% group_by(Sp, ID) %>% ggplot(aes(y = uhHerbPerc, x = LeafNo)) + geom_boxplot() + scale_y_continuous(trans='log10')

standing %>% ggplot(aes(y = uhHerbPerc, x = rhHerbPerc, color = LeafNo)) + geom_point() + facet_wrap(~ Sp)

standing %>% ggplot(aes(x = rhHerbPerc)) + facet_wrap(~ Sp) + geom_histogram() # for sure zero infalted

logit(standing) %>% group_by(Sp, ID) %>% ggplot(aes(y = rhHerbPerc, x = LeafNo)) + 
  geom_boxplot() + 
  facet_wrap(~ Sp) 

logit(standing$rhHerbPerc)

# how common is herbivory ? 
standing <- standing %>% mutate(herbPresence = case_when(totalHerb == 0 ~ 0, totalHerb > 0 ~ 1 ))

# on a given plant, how many leaves will have herbivory on average?
herbByPlant <- standing %>% 
  group_by(Sp, ID) %>% 
  summarize(numLeaves =n_distinct(LeafNo), numLeavesWHerb = sum(herbPresence == 1)) %>% 
  mutate(plantPercHerb = numLeavesWHerb / numLeaves) %>% 
  mutate(plantPercHerb = if_else(plantPercHerb > 1, 1, plantPercHerb))

herbByPlant %>% group_by(Sp) %>% 
  summarize(avgLeavesWHerb = mean(plantPercHerb, na.rm = T), 
            sdLeavesWHerb = sd(plantPercHerb, na.rm = T))

ggplot(herbByPlant, aes(Sp, plantPercHerb, fill = Sp)) +
  geom_boxplot() + 
  ylab("Percent of Leaves with Herbivory per Individual") +
  xlab("Species") +
  scale_fill_discrete(name = "Species", labels=c('R. gigante', 'C. scaber', "H. stricta", "H. velutia")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

# how much leaf area is lost due to herbivory on average
standingSummary <- standing %>% group_by(Sp) %>% 
  summarize(meanPercHerb = mean(herbPerc, na.rm = T), sdPercHerb = sd(herbPerc, na.rm = T))

ggplot(standing, aes(Sp, herbPerc, fill =Sp)) +
  geom_boxplot() +
  scale_y_continuous(trans = "pseudo_log") +
  ylab("Percent of Herbivory Per Leaf") +
  xlab("Species") +
  scale_fill_discrete(name = "Species", labels=c('R. gigante', 'C. scaber', "H. stricta", "H. velutia")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

ggplot(standingSummary, aes()) +
  geom_boxplot() +
  facet_wrap(~ Sp)


# does the amount of herbivory change depending on leaf age- is that different for rolled vs unrolled?

herbivoryLongForm <- standing %>% 
  select(c("Sp", "ID", "LeafNo", "uhHerbPerc", "rhHerbPerc")) %>% 
  pivot_longer(cols = c(uhHerbPerc, rhHerbPerc), 
               names_to = "Rolled",
               values_to = "percentHerb")

herbivoryLongForm %>% 
  subset(LeafNo != "Terminal" & Sp == "giga") %>% 
  ggplot(aes(LeafNo, percentHerb, fill = Rolled)) +
  geom_boxplot() + 
  scale_fill_discrete(name = "Roll Status", labels =c("Rolled", "Unrolled")) +
  xlab("Leaf Number") +
  ylab("Percent Herbivory") +
  scale_y_continuous(trans = "pseudo_log")
 # i'm not sure if boxplot or scatterplot is correct

library("gamlss")


dataForBetaRegStandingHerb %>% mutate(dataClass = case_when(percentHerb == 1 ~ "one",
                                           percentHerb == 0 ~ "zero",
                                            percentHerb != 0 & percentHerb != 1 ~ "between")) %>% 
  group_by(dataClass) %>% 
  summarize(n())

dataForBetaRegStandingHerb <- herbivoryLongForm %>% filter(percentHerb < 1) %>% na.omit()
m6 <- gamlss(percentHerb ~ LeafNo + Sp + Rolled,  family = BEZI, data = dataForBetaRegStandingHerb, trace = F)
summary(m6)   

summary(aov(data = dataForBetaRegStandingHerb, percentHerb ~ LeafNo + Sp + Rolled )) # yeah idk how to interpret the results


# i feel this viz isn't the most effective
longformSummary <- herbivoryLongForm %>% 
  group_by(Sp, Rolled, LeafNo) %>% 
  summarize(meanHerb = mean(percentHerb, na.rm = T), sdHerb = sd(percentHerb, na.rm = T))
  
longformSummary %>% 
  ggplot(aes(LeafNo, meanHerb, fill = Rolled)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Sp) +
  geom_errorbar(aes(ymin = meanHerb - sdHerb, ymax = meanHerb + sdHerb)) 

# I think the proper regression for this will be zero inflated beta


Sp.labs <- c("R. gigante", "C. scaber", "H. scaber", "H. velutina")
names(Sp.labs) <- c("giga", "scaber","stricta",  "velutia")


longformSummary %>% 
  subset(LeafNo != "Terminal") %>% 
  ggplot(mapping = aes(y = meanHerb, x = LeafNo, fill = Rolled)) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~Sp) +
  scale_fill_discrete(name = "Roll Status", labels =c("Rolled", "Unrolled")) +
  xlab("Leaf Number") +
  facet_wrap(~ Sp, 
             labeller = labeller(Sp = Sp.labs)) +
  theme(strip.text.x = element_text(face = "italic")) +
  ylab("Mean Percent Herbivory") 


herbivoryLongForm %>% 
  ggplot(aes(x = LeafNo, y = percentHerb )) +
  geom_point()
