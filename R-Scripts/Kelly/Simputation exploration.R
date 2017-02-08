library(tools)
library(devtools)
devtools::install_github('benjones2/WOODCARB3R', build_vignettes = TRUE)
library(WOODCARB3R)

dat <- iris
dat
dat[1:3,1] <- dat[3:7,2] <- dat[8:10,5] <- NA  #emptying a few fields
#1 through 3, column 1. 3 through 7, column 2. 8 through 10, column 5.
head(dat,10)

library(magrittr) 
library(simputation)
library(magrittr)    # load the %>% operator
library(simputation) 
imputed <- dat %>%  #Using the %>% operator from the popular magrittr allows for a very compact specification
  impute_lm(Sepal.Width + Sepal.Length ~ Petal.Width + Species) %>% #first impute Sepal.Width and Sepal.Length by regression on Petal.Width and Species.
  impute_cart(Species ~ .) #imputes Species using a decision tree model (CART) using every other variable as a predictor (including the ones just imputed).
head(imputed,10)


#Failure of imputation= a warning message is issued and the value is not imputed. (NA)
#Example= if one of the predictors is missing. So if the predictor is NA, then variable wanting to be imputed stays NA.

da1 <- dat %>% impute_lm(Sepal.Length ~ Sepal.Width + Species) %>% head(3)
print(da1)
#Works for the first 2, but does not work for the 3rd value because Sepal.Width is unknown.
#ones that can be imputeted will be, and remaining values can be computed using a new linear model, such as group median
#shown below

da2 <- impute_median(da1, Sepal.Length ~ Species)
head(da2,3)
#the above is simple, but could possibly be explored more. (median of two previous values)



#Exploring Grouping

#split data into groups before estimating the imputation model and predicting missing values. 
#2 ways. 1. use the | operator to specify grouping variables.

# We first need to complete 'Species'. Here, we use sequential 
# hot deck after sorting by Petal.Length
dat %<>% impute_shd(Species ~ Petal.Length) 

# Now impute Sepal.Length by regressing on 
# Sepal.Width, computing a model for each Species.
dat %>% impute_lm(Sepal.Length ~ Sepal.Width | Species) %>% head(3)

dat %>% impute_lm(Sepal.Length ~ Sepal.Width | Species) %>% head(70)
#The first example shows first 3 of "setosa", second example gets to "versicolor at 51 in the list"
#alphabetically grouping?


#The second way is to use the group_by command from dplyr
#dplyr=A fast, consistent tool for working with data frame like objects, both in memory and out of memory.

library(dplyr)
dat %>% dplyr::group_by(Species) %>% 
  impute_lm(Sepal.Length ~ Sepal.Width) %>% 
  head(3)

#Note: by using group_by, we also transformed the data.frame to a tibble
#There are two main differences in the usage of a data frame vs a tibble:
#printing, and subsetting.

#A little on tibbles...
#Tibbles have a refined print method that shows only the first 10 rows, and all the columns that fit on screen. 
#This makes it much easier to work with large data


#Questions & thoughts:
#What type of imputation?
#more examples to explore
#spreadsheet is currently using the previous known value as constant
#linear model, regression, median
#do we want to group by a certain variable?
#should some variables remain NA for some outside reason?
#are tibbles important...

#Helpful sites:

#Simputation package example followed:
#https://www.r-bloggers.com/announcing-the-simputation-package-make-imputation-simple/

#Further discussion of Simputation package
#https://cran.r-project.org/web/packages/simputation/vignettes/intro.html

#Tibble info:
#https://blog.rstudio.org/2016/03/24/tibble-1-0-0/
