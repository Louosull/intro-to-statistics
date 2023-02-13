#PACKAGES----
library(tidyverse)
library(here)
library(kableExtra)
library(GGally)
library(emmeans)
library(performance)

#DATA----
darwin <- read_csv(here("data", "darwin.csv"))

#FIRST ANALYSIS----

# check the structure of the data
glimpse(darwin)

# check data is in a tidy format
head(darwin)

# check variable names
colnames(darwin)


# clean up column names

darwin <- janitor::clean_names(darwin)

# check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

# check for typos by looking at distinct characters/values

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# missing values
darwin %>% 
  is.na() %>% 
  sum()

# quick summary

summary(darwin)

#1ST VISUALISATION----

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()

#going to try other types of graphs just to look at

# geom_boxplot()
# geom_violin()
# geom_histogram()
# Why not have a go and see what you can make?

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_boxplot()

darwin %>% 
  ggplot(aes(x=height))+
  geom_histogram() #does work 


#The graph clearly shows that the average height of the 'crossed' plants is greater than that of the 'selfed' plants. But we need to investigate further in order to determine whether the signal (any apparent differences in mean values) is greater than the level of noise (variance within the different groups).
#COMPARING GROUPS----
#Findin he mean and standard deviation of our groups as a way of comparing them 

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height)) #STANDARD DEVIATIO = ONAVERAE HOW FAR AWAY EACH POINT IS FROM The mean - lower = all points are on average closer to the mean 

# make a new object
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw() #dot is the mean an the line is the standard deviaion around it = plot of our previous mean and SD

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>%
  mutate(difference = Cross - Self)

#Our aim = to use use the height of the plants as a proxy for fitness and explicitly address whether there is a difference in the mean heights of the plants between these two groups
#Our goals - Estimate the mean heights of the plants in these two groups Estimate the mean difference in heights between these two groups Quantify our confidence in these differences

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary 
#o this is the calcuated mean differece in heights and the amount of variance 

#STANDARD ERROR----

#So we can think of it as a measure of the confidence we have in our estimate of a true population mean.
difference_summary_sdtale<- difference_summary %>% 
  mutate(se= sd/sqrt(n))
#estimates of averages or differences should always be accompanied by their measure of uncertainty.
#so this table is saying  the average difference in height was 2.62 ± 1.22 inches (mean ± SE)

#NORMAL ISTURBUTION----
#read it from chapter 12 

#the code below is the idealised normal distribution set up 
#Create a sequence of 100 equally spaced numbers between -4 and 4
x <- seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x)

#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

#as our sample size is great enough we are going to apply the central limit theorum meaning we can centre our curve at the mean (2.62) and 1 standadr deviation = 1.22 so 2 standadr deviations where 95% fall = 2 x 1.22  

#CONFIDENCE INTERVALS---- 

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

#so we are saying that we are confident we would capture the true mean in 95% of our experiments.

#CHAPTER 13---- 

lsmodel0 <- lm(formula = height ~ 1, data = darwin)

summary(lsmodel0)

broom::tidy(lsmodel0)

lsmodel1 <- lm(height ~ type, data=darwin) # linear model of the difference in means etween types 

# note that the following is identical

# lsmodel1 <- lm(height ~ 1 + type, data=darwin)

broom::tidy(lsmodel1)
summary(lsmodel1)

darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw() #ue his model summay to make a plot 
