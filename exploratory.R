## OKCupid Data Explore

## Winston Saunders
## v1.1 modified normalization to just be a loop 

library(okcupiddata)
library(dplyr)
library(ggplot2)


## RELIGION AND DRINKING

    ## clean religion data into broad category
    bnG <- filter(profiles, !is.na(drinks), !is.na(religion), !is.na(sex)) %>% as_data_frame
    
    # # A tibble: 38,729 x 22
    #       age     body_type               diet
    #       <int>       <chr>               <chr>
    # 1     22 a little extra   strictly anything
    # 2     35        average        mostly other
    # 3     29        average     mostly anything
    # 4     31        average     mostly anything
    # 5     24           <NA>   strictly anything
    # 6     37       athletic     mostly anything
    # 7     28        average     mostly anything
    # 8     24           <NA>              <NA>
    # 9     30         skinny     mostly anything
    # 10    29           thin     mostly anything
    # # ... with 38,719 more rows, and 19 more
    # #   variables: drinks <chr>, drugs <chr>,
    # #   education <chr>, ethnicity <chr>,
    # #   height <int>, income <int>, job <chr>,
    # #   last_online <time>, location <chr>,
    # #   offspring <chr>, orientation <chr>,
    # #   pets <chr>, religion <chr>, sex <chr>,
    # #   sign <chr>, smokes <chr>, speaks <chr>,
    # #   status <chr>, essay0 <chr>
    

    ## get affiliation (strip modifiers) using gsub and simple regex
    bnG$religious_affil <- gsub(' [A-z ]*', '', bnG$religion) %>% as.factor()
    
    ## convert bnG$drinks to factor
    bnG$drinks <- bnG$drinks %>% as.factor()
    
    ## group the data 
    bnG_rd <- group_by(bnG[,c("drinks", "religious_affil", "sex")], drinks, religious_affil, sex) %>% summarize(n = n())
    
    #Source: local data frame [106 x 4]
    #Groups: drinks, relig [?]
    #
    #         drinks        relig   sex     n
    #         <fctr>       <fctr> <chr> <int>
    # 1  desperately  agnosticism     f    25
    # 2  desperately  agnosticism     m    37
    # 3  desperately      atheism     f    18
    # 4  desperately      atheism     m    40
    # 5  desperately     buddhism     f     5
    # 6  desperately     buddhism     m    15
    # 7  desperately  catholicism     f     4
    # 8  desperately  catholicism     m     7
    # 9  desperately christianity     f     7
    # 10 desperately christianity     m     7
    # # ... with 96 more rows
    
    
    ## Now need to normalize the drinker 
    ##  types to the total drinkers by religious affiliation
    
    ## Use a loop thru. *recommendations for more expedient code???
            
            ## add normalizing column
            bnG_rd$NN <- 1
            
            ## loop thru each row, filter, and sum
            i <- 1
            for (i in 1:nrow(bnG_rd)){
                
                temp <- filter(bnG_rd, religious_affil == bnG_rd$religious_affil[i], sex == bnG_rd$sex[i])
                bnG_rd$NN[i] <- sum(temp$n)
            }
    
    ## normalize
    
    bnG_final <- mutate(bnG_rd, freq = n/NN)
    
    # Source: local data frame [106 x 6]
    # Groups: drinks, religious_affil [54]
    # 
    #         drinks religious_affil   sex     n    NN        freq
    #         <fctr>          <fctr> <chr> <int> <dbl>       <dbl>
    # 1  desperately     agnosticism     f    25  3184 0.007851759
    # 2  desperately     agnosticism     m    37  5421 0.006825309
    # 3  desperately         atheism     f    18  1882 0.009564293
    # 4  desperately         atheism     m    40  4921 0.008128429
    # 5  desperately        buddhism     f     5   791 0.006321113
    # 6  desperately        buddhism     m    15  1090 0.013761468
    # 7  desperately     catholicism     f     4  2059 0.001942691
    # 8  desperately     catholicism     m     7  2613 0.002678913
    # 9  desperately    christianity     f     7  2606 0.002686109
    # 10 desperately    christianity     m     7  3052 0.002293578
    # # ... with 96 more rows
    
    ## Clean up for display
    
    ## reorder factor
    bnG_final$drinks <- factor(bnG_final$drinks, levels = c("not at all", 
                                                           "rarely",
                                                           "socially",
                                                           "often",
                                                           "very often",
                                                           "desperately"))
    
    ## express sex in actual words
    
    bnG_final$sex<- gsub("m", "male", bnG_final$sex)
    bnG_final$sex<- gsub("f", "female", bnG_final$sex)
    
    # # A tibble: 106 x 6
    #         drinks        relig   sex     n    NN        freq
    #         <fctr>       <fctr> <chr> <int> <dbl>       <dbl>
    # 1  desperately  agnosticism     f    25  3184 0.007851759
    # 2  desperately  agnosticism     m    37  3184 0.011620603
    # 3  desperately      atheism     f    18  1882 0.009564293
    # 4  desperately      atheism     m    40  1882 0.021253985
    # 5  desperately     buddhism     f     5   791 0.006321113
    # 6  desperately     buddhism     m    15   791 0.018963338
    # 7  desperately  catholicism     f     4  2059 0.001942691
    # 8  desperately  catholicism     m     7  2059 0.003399709
    # 9  desperately christianity     f     7  2606 0.002686109
    # 10 desperately christianity     m     7  2606 0.002686109
    # # ... with 96 more rows
    
    p <- ggplot(bnG_final, aes(x = drinks, y = religious_affil, size = freq)) +
        geom_point(alpha = 0.9, fill = "#FF9933", pch=21, color = "darkred") +
        ggtitle("okcupid: drinks versus religion") + facet_grid(sex~.) + 
        ylab("religious affiliation")
    

    print(p)
    
    bnG_final <- group_by(bnG_final, religious_affil)
    
    p1 <- ggplot(bnG_final, aes(x = drinks, y = 100*freq, color = sex))+#, color = religious_affil)) + 
        geom_point(size = 2, pch = 21, fill = "#FF9933", alpha = 0.8) +
        facet_grid(religious_affil~.) +
        ggtitle("okcupid: religion versus drinking") + 
        scale_y_log10(breaks = c(0.1,  1, 10, 100)) + 
        ylab("percent")
    
    print(p1)
    
## AGE AND DRINKING
    
    ## clean data into broad category
    bnG <- filter(profiles, !is.na(drinks), !is.na(age), !is.na(sex)) %>% as_data_frame
    
    ## set factor order
    bnG$drinks <- factor(bnG$drinks, levels = c("not at all", 
                                                            "rarely",
                                                            "socially",
                                                            "often",
                                                            "very often",
                                                            "desperately"))
    
    bnG$sex<- gsub("m", "male", bnG$sex)
    bnG$sex<- gsub("f", "female", bnG$sex)
    
    ggplot(bnG, aes(x = drinks, y = age)) + 
        geom_violin(alpha = 0.3, size = 1, fill = "#FF9933", color = "darkred") + 
        ggtitle("okcupid: drinks versus age") + facet_grid(sex~.)
    
    bnG <- mutate(bnG, age_range = 2.5 + 5 * floor(age / 5) )
    
    bnG <- group_by(bnG, age_range, sex, drinks) %>% summarize(n=n())
    
    ## Use a loop thru. *recommendations for more expedient code???
    
    ## add normalizing column
    bnG$NN <- 1
    
    ## loop thru each row, filter, and sum
    i <- 1
    for (i in 1:nrow(bnG)){
        
        temp <- filter(bnG, age_range == bnG$age_range[i], sex == bnG$sex[i])
        bnG$NN[i] <- sum(temp$n)
    }
    
    bnG <- mutate(bnG, pct = 100*n/NN)
    bnG <- filter(bnG, age_range >= 20)
    
    p2 <- ggplot(bnG, aes(x = drinks, y = age_range, size = pct)) +
        geom_point(alpha = 0.9, fill = "#FF9933", pch=21, color = "darkred") +
        ggtitle("okcupid: drinks versus age") + facet_grid(sex~.) + 
        ylab("age_range")
    
    print(p2)
    
    p3 <- ggplot(bnG, aes(x = age_range, y = pct, color = drinks)) + geom_line(size = 2) +
        facet_grid(sex~.) +
        ggtitle("okcupid: drinking versus age") + 
        scale_y_log10(breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50, 100))
    
    print(p3)
    