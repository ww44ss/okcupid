# Exploratory Analysis of OkCupid dataset
Winston Saunders  
August 27, 2016  


##EXEC SUMMARY  
This explores several relationships in the OkCupid data [published on CRAN](https://cran.rstudio.com/web/packages/okcupiddata/index.html). Results explored include:  
0. Ages of OkCupid users.   
1. The Correlation of Religion and Drinking Habits.  
2. Changing drinking habits with age.  
3. Religious Affiliation with Age.  
4. TBD 

##Getting the Data
The OkCupid data is published on [CRAN](https://cran.rstudio.com/web/packages/okcupiddata/index.html) as a package for R suers. The data set consists of user profile data for 59,946 San Francisco OkCupid users (a free online dating website) from June 2012. The data are describded in the paper: Albert Y. Kim, Adriana Escobedo-Land (2015). OkCupid Profile Data for Introductory Statistics and Data Science Courses. Journal of Statistics Education, 23(2), which is found [here]( http://www.amstat.org/publications/jse/v23n2/kim.pdf)



The raw data is loaded as a library


```r
## load data
library(okcupiddata)
```

and it consist of these data fields (detailed descriptions of which can be found in the reference above). 


```r
## column names
profiles %>% colnames
```

```
##  [1] "age"         "body_type"   "diet"        "drinks"      "drugs"      
##  [6] "education"   "ethnicity"   "height"      "income"      "job"        
## [11] "last_online" "location"    "offspring"   "orientation" "pets"       
## [16] "religion"    "sex"         "sign"        "smokes"      "speaks"     
## [21] "status"      "essay0"
```


## Basic Age and Sex Distributions 

The simplest and most obvious thing is to look at first is the sex and age distribution of OkCupid users in a histogram.


```r
    ## eliminate NAs and restrict age
    cleaned <- filter(profiles, !is.na(age), !is.na(sex), age > 18, age < 80) %>% 
            as_data_frame %>%
            select(sex, age)
    ## make data descriptive
    cleaned$sex<- cleaned$sex %>% gsub("m", "male", .) %>% gsub("f", "female", .)
```




<img src="okcupid_exploratory_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />



Overall the number of men in 35680 and the number of females is 23955, with an overall ratio of about 1.5:1. 

A couple of notable features are that the distributions peaks in the late 20's and that there is a long tail on the distribution.

### Digging deeper into the Male - Female Population Difference

We can dig deeper into the differences in the age group distributions by looking at the differences between them.


```r
    ## compute age groups using mutate
    analyzed <- cleaned %>% mutate(age_group = 2 + 4 *  age %/% 4)
    analyzed$age_group <- analyzed$age_group %>% as.factor
    
    ## group_by data and summarize by sex and age
    sex_count <- group_by(analyzed[,c("age_group", "sex")], age_group, sex) %>% summarize(n_sex = n())
    ## count the total number of males and females
    age_count <- group_by(analyzed[,c("age_group")], age_group) %>% summarize(n_age = n())
    ## join the data
    analyzed <- left_join(sex_count, age_count, by = "age_group") %>% mutate(freq = n_sex/n_age, freq = ifelse(is.na(freq), 0, freq), delta_percent = 200*(freq - 0.5))
```

A graph of the data shows clearly the diffences in the numbers of male oand female users, with men outnumbering women by an approximately 3:2 ratio for ages below 50, while in the 60's to 70's, women outnumber men by about 6:5.  


<img src="okcupid_exploratory_files/figure-html/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />


###Age distribution of male and female populations

The above data show differences male to female populations by age, but they do not reveal what the differences are. We can explore more intrinsic male and female behavior by separating the male and females populations and normalizing them. 








The populations have a gamma distribution-like shape with a mean near 28.



<img src="okcupid_exploratory_files/figure-html/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

The cummulative plot highlights the longer tail on the female age distribution. <br>
A normalized bar chart shows the differences in age population by sex, again with women being more numerous at ages above about 50. 

<img src="okcupid_exploratory_files/figure-html/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />





## RELIGION AND DRINKING



```
## Joining, by = c("religious_affil", "sex")
```

![](okcupid_exploratory_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```
## Joining, by = c("age_range", "sex")
```

![](okcupid_exploratory_files/figure-html/unnamed-chunk-14-1.png)<!-- -->![](okcupid_exploratory_files/figure-html/unnamed-chunk-14-2.png)<!-- -->
##SEX VERSUS RELIGION

```
## Joining, by = "sex"
```

![](okcupid_exploratory_files/figure-html/unnamed-chunk-15-1.png)<!-- -->



## SENTIMENT

Let's try looking at the sentiment of the texts
