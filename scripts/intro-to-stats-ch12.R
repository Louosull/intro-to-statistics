#PACKAGES----
library(tidyverse)
library(here)
library(kableExtra)
library(GGally)
library(emmeans)
library(performance)
library(rstatix)


#Chapter 12----

#DATA----
darwin <- read_csv(here("data", "darwin.csv"))
#Hypothesis being tested = whether inbreeding reduced the fitness of the selfed plants

#CLEANING DATA----

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
#trying some other graphs below 

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

# make a new object of the mean and standard deviation 
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot of this mean and standadr deivation for both groups 
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw() #dot is the mean an the line is the standard deviaion around it = plot of our previous mean and SD


# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

#ESTIMATION----

#Our aim = to use use the height of the plants as a proxy for fitness and explicitly address whether there is a difference in the mean heights of the plants between these two groups
#Our goals - Estimate the mean heights of the plants in these two groups Estimate the mean difference in heights between these two groups Quantify our confidence in these differences

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>%
  mutate(difference = Cross - Self) #new column with the difference between the height of crossed from selfed 

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

#Standard error is a measure of uncertainty, the larger the standard error the more noise around our data and the more uncertainty we have. The smaller the standard error the more confidence we can have that our difference in means is real.

#Null hypothesis - there is no difference in the mean height of self vs crossed plants

#Alternate hypothesis - inbreeding reduces the fitness of the selfed plants, observed as selfed plants on average being smaller than crossed plants

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
#remeber z-distribution = normal distribution 

#linear models  = comparision of difference between two groups or comparison of trend for two factors 

#COMPARING GROUPS----

lsmodel0 <- lm(formula = height ~ 1, data = darwin) #one model type for comparing grous using the least of two squares method/function

#lm() = specifies we want to analyse a response variable (height) as a function of an explanatory variable using the tilde symbol (~).
#The simplest possible model ignores any explanatory variables, instead the 1 indicates we just want to estimate an intercept. Without explanatory variables this means the formula will just estimate the overall mean height of all the plants in the dataset.

#SUMMARIES FOR MODELS----

summary(lsmodel0) #to produce a summary of the linear model we have just made 

broom::tidy(lsmodel0) #produces a tibble of the info above = table of coefficients where the 18.9 is the estimate of the model coefficient which is the mean in this case

#these tibbles provide info as the intercept but what does this mean? In this case can prove its the overall mean of all plant heights

mean(darwin$height) # same as the coefficient on the tibble 

#COMPARING MEANS----
#want a a linear model that analyses the difference in average plant height as a function of pollination type.

lsmodel1 <- lm(height ~ type, data=darwin) # the linear model of the difference in means etween types as above wanted

# note that the following is identical

# lsmodel1 <- lm(height ~ 1 + type, data=darwin)

broom::tidy(lsmodel1) # now the model contains the intercept (height) as well as the pollination type 
#now intercept = the mean of the crossed plants as it does it alphabetiicaly (crossed over selfed) while the typecross second row = the difference in the mean height of the two groups
#where this linear model indicates the average height of Crossed plants is 20.2 inches, and Selfed plants are an average of 2.6 inches shorter

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height)) #confirm this 


summary(lsmodel1)#summary of everything so far 

darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw() #ue his model summay to make a plot 

#STANDARD ERROR----
#SEM = standadr error of the mean as we have seen before 
#SED = standard error of the difference in means 

#CONDFIDENCE LEVELS----
broom::tidy(lsmodel1, conf.int=T) # Because this follows the same layout as the table of coefficients, the output intercept row gives a 95% CI for the height of the crossed plants and the second row gives a 95% interval for the difference in height between crossed and selfed plants. The lower and upper bounds are the 2.5% and 97.5% of a t-distribution

#now going touse confidence intervals and p values to answer our original question (wether there is a difference in average plant height as a function of pollination type.)

#ANSWERING THE QUESTIONS----
#read this bit and normal distribution bit if needed 
#null hypotheis = there is no difference between the crossed and selfed = the mean difference is 0 

#to test this hypothesis = check whether or not the predicted value of our null hypothesis (a difference of zero) lies inside the 95% CI for the difference of the mean
#i.e if it does with a high level of confidence then can accept this as true for the whole popuation 

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95) #wont let me do it as the packagaes dont work BUT produces a graph of the estimated mean difference with an approx 95% CI.

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy() #table of coefficients for the selfed info to get both (like the one before but where selfed is the intercept)

#EMMEANS----

means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means # doing the same thing as the one above 

#CHECKING ASSUMPTIONS----

#Checking:
# 1. that the residual/unexplained variance in our data is approximately normally distributed.

#2.that the residual/unexplained variance is approximately equal between our groups

#if it IS normally distributed and the variance between the groups IS EQUAL than we can do all the stuff we have so far confidently 

#NORMAL DISTRIUBTION ----
performance::check_model(lsmodel1, check=c("normality","qq"))
plot(lsmodel1, which=c(2,2))

#we have made QQ plots for our redisuals/varaince = QQ plot is a classic way of checking whether a sample distribution is the same as another (or theoretical distribution). They look a bit odd at first, but they are actually fairly easy to understand, and very useful! The qqplot distributes your data on the y-axis, and a theoretical normal distribution on the x-axis. If the residuals follow a normal distribution, they should meet to produce a perfect diagonal line across the plot.
#we find that our data most of the reduals fall along the diganol line and follow a normal dsitrubtion HOWEVER some do not - this makes our data not perfect but not awful 

#EQUAL VARIANCE----
performance::check_model(lsmodel1, check="homogeneity")
# read chapter 13 bit 
#SUMMARY---- 
#So remember a linear model sets one factor level as the 'intercept' estimates its mean, then draws a line from the first treatment to the second treatment, the slope of the line is the difference in means between the two treatments.

#The difference in means is always accompanied by a standard error of the difference (SED), and this can be used to calculate a 95% confidence interval. If this confidence interval does not contain the intercept value, we can reject the null hypothesis that there is 'no effect'.

#CHAPTER 14 ---- 

# STUDENTS T-TEST ----

#The one sample t-test: takes the mean of a sample and compares it with 
#the null hypothesis of zero

lm(y ~ 1)

lm (height~1)

#The two sample t-test which compares the difference between the means of two 
#samples against a null hypothesis of no difference between the means of 
#the two populations

#when sample sizes are large the t-distribution is roughly equal to a normal (z)
#distribution. However, when sample sizes are small the t-distribution has a 
#shorter and wider distribution


df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

# ALL THIS IS JUST TO GENERATE THE TABLE (IN GRAPH FORM) OF THE CRITICAL VALUES
# FOR THE DIFFERENCE DEGREES OF FREEDOM. Its the same values as the critical
# values tables usually provided in text books

lsmodel1 <- lm(height ~ type, data = darwin) #linar model viewed again 
summary(lsmodel1) #his coducst some tests for us anyway 

tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]] #finding the observed value of t 

# PAIRED T-TEST ----

lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)
summary(lsmodel_darwin)

#The second row now compares the mean heights of Crossed and Selfed plants 
#when they are in the same pair

#Rows 3 to 16 compare the average difference of each pair (Crossed and Selfed 
#combined) against pair 1

#generating the confidence intervals for the paired t-test
lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) # just show first two rows 
#sO HERE WE TRID A paired test ut i produced greater uncertianity so wont use this now 

# REPEATING THE EXPERIMENT TO INCREASE RELIABILITY ----
set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

# the new dataframe y contains the results of 20 new experiments

y

y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% 
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n()) 

#Shows how many of the repeats were significant and insignificant


y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()

#Summary so far = calculate P-values and test statistical significance for our experiments using linear models. We also compared the linear model structures for producing a paired vs. unpaired t-test.

#However we also learned to appreciate the potential issues around making Type 1 and Type 2 errors, and how an appreciation of confidence intervals and standardised effect sizes can be used to assess these.

#Chapter 15---- 

#So far we have used linear models for analyses between two 'categorical' explanatory variables e.g. t-tests. But what about when we have a 'continuous' explanatory variable? For that we need to use a regression analysis

#the regression analysis is interpreting the strength of the 'signal' (the change in mean values according to the explanatory variable), vs the amount of 'noise' (variance around the mean)
#With regression, we can test the biological hypothesis that wood density can be used to predict timber hardness, and use this regression to predict timber hardness for new samples of known density


#DATA READ IN AND CHECK----

#DATA----
janka <- read_csv(here("data", "janka.csv"))

#FIRST ANALYSIS----

# check the structure of the data
glimpse(janka)

# check data is in a tidy format
head(janka)

# check variable names
colnames(janka)


# clean up column names

janka <- janitor::clean_names(janka)

# check for duplication
janka %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
janka %>% 
  summarise(min=min(hardness, na.rm=TRUE), 
            max=max(hardness, na.rm=TRUE),
            min=min(dens, na.rm=TRUE), 
            max=max(dens, na.rm=TRUE))


# missing values
janka %>% 
  is.na() %>% 
  sum()

# quick summary

summary(janka)

#FIRST PLOT----

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()
#Wood density and timber hardness appear to be positively related, and the relationship appears to be fairly linear. We can look at a simple strength of this association between dens and hardness using correlation

#Pearsons R---- 
correlation <- cor(janka$dens, janka$hardness, method = 'pearson')
correlation #checking the results 

#Correlation coefficients range from -1 to 1 for perfectly negative to perfectly positive linear relationships. The relationship here appears to be strongly positive. Correlation looks at the association between two variables, but we want to go further - we are arguing that wood density causes higher values of timber hardness. In order to test that hypothesis we need to go further than correlation and use regression.

#Regression in R----

janka_ls1 <- lm(hardness ~ dens, data = janka) #te linear reresion in R  
summary(janka_ls1)

janka_ls1 %>% 
  broom::tidy() #explianed - intercept estimate means the mean, the standard error on the first row i for the intercept data and t statsitic 
#dens line esimate is the difference of dens mean from the hardness mean, stadard error = SED, 


#This linear model will estimate a 'line of best fit' using the method of 'least squares' to minimise the error sums of squares (the average distance between the data points and the regression line).

# specify linear model method for line fitting

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm") #ading the linear regression line to our plot o far 



#MAN CENTRED REGRESSION----

dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))


janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

#The second row is labelled 'dens'. Density is our explanatory variable, and the slope is estimated against it. So if 57.5 is the value of the regression slope (with its standard error) - then the timber hardness is predicted to increase by 57.5 on the janka scale for every unit change of density.

#According to our model summary, this estimated change in the mean is statistically significant - so for this effect size and sample size it is unlikely that we would observe this relationship if the null hypothesis (that we cannot predict timber hardness from wood density) were true.

#CONFIDENCE INTERVALS----

broom::tidy(janka_ls1, conf.int=T, conf.level=0.95)
