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
    #       age     body_type                diet
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
    
    # Source: local data frame [106 x 4]
    # Groups: drinks, religious_affil [?]
    # 
    #         drinks religious_affil   sex     n
    #         <fctr>          <fctr> <chr> <int>
    # 1  desperately     agnosticism     f    25
    # 2  desperately     agnosticism     m    37
    # 3  desperately         atheism     f    18
    # 4  desperately         atheism     m    40
    # 5  desperately        buddhism     f     5
    # 6  desperately        buddhism     m    15
    # 7  desperately     catholicism     f     4
    # 8  desperately     catholicism     m     7
    # 9  desperately    christianity     f     7
    # 10 desperately    christianity     m     7
    # # ... with 96 more rows
    
    
    bnG_r <- group_by(bnG[,c("religious_affil", "sex")], religious_affil, sex) %>% summarize(NN = n())
    
    # Source: local data frame [18 x 3]
    # Groups: religious_affil [?]
    # 
    #    religious_affil   sex    NN
    #             <fctr> <chr> <int>
    # 1      agnosticism     f  3184
    # 2      agnosticism     m  5421
    # 3          atheism     f  1882
    # 4          atheism     m  4921
    # 5         buddhism     f   791
    # 6         buddhism     m  1090
    # 7      catholicism     f  2059
    # 8      catholicism     m  2613
    # 9     christianity     f  2606
    # 10    christianity     m  3052
    # 11        hinduism     f   143
    # 12        hinduism     m   293
    # 13           islam     f    34
    # 14           islam     m    90
    # 15         judaism     f  1476
    # 16         judaism     m  1537
    # 17           other     f  3300
    # 18           other     m  4237
    
    
    
    
    #bnG_rd <- merge(bnG_rd, bnG_r, by = c("religious_affil", "sex")) %>% as_data_frame
    
    # # A tibble: 106 x 5
    #    religious_affil   sex      drinks     n    NN
    #             <fctr> <chr>      <fctr> <int> <int>
    # 1      agnosticism     f desperately    25  3184
    # 2      agnosticism     f      rarely   263  3184
    # 3      agnosticism     f    socially  2412  3184
    # 4      agnosticism     f  very often    27  3184
    # 5      agnosticism     f  not at all   112  3184
    # 6      agnosticism     f       often   345  3184
    # 7      agnosticism     m      rarely   492  5421
    # 8      agnosticism     m    socially  3987  5421
    # 9      agnosticism     m  very often    39  5421
    # 10     agnosticism     m  not at all   244  5421
    # # ... with 96 more rows
    
    
    
    bnG_final <- left_join(bnG_rd, bnG_r) %>% mutate(freq = n/NN, freq = ifelse(is.na(freq), 0, freq))

   
    
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
    
    
    
    p <- ggplot(bnG_final, aes(religious_affil, weight = 100*freq)) +
        geom_bar(aes(fill = drinks), alpha = 0.9) +
        ggtitle("okcupid: drinking and religion") + facet_grid(sex~.) +
        coord_flip() + 
        scale_fill_brewer(palette = 4, direction = 1) + 
        ylab("percent") + 
        xlab("religious affiliation")
    
    print(p)
    
    
## AGE AND DRINKING
    
    ## clean data into broad category
    bnA <- filter(profiles, !is.na(drinks), !is.na(age), !is.na(sex)) %>% as_data_frame
    
    ## set factor order
    bnA$drinks <- factor(bnA$drinks, levels = c("not at all", 
                                                            "rarely",
                                                            "socially",
                                                            "often",
                                                            "very often",
                                                            "desperately"))
    
    bnA$sex<- gsub("m", "male", bnA$sex)
    bnA$sex<- gsub("f", "female", bnA$sex)
    
    ggplot(bnA, aes(x = drinks, y = age)) + 
        geom_violin(alpha = 0.3, size = 1, fill = "#FF9933", color = "darkred") + 
        ggtitle("okcupid: drinks versus age") + facet_grid(sex~.)
    
    bnA <- mutate(bnA, age_range = 2.5 + 5 * floor(age / 5) )
    
    bnA_sd <- group_by(bnA, age_range, sex, drinks) %>% summarize(n=n())
    
    bnA_d <- group_by(bnA, age_range, sex) %>% summarize(NN=n())
    
    bnA_final <- left_join(bnA_sd, bnA_d)
    
    bnA_final <- mutate(bnA_final, percent = 100*n/NN)
    bnA_final <- filter(bnA_final, age_range >= 20)
    
    p <- ggplot(bnA_final, aes(age_range, weight = percent)) +
        geom_bar(aes(fill = drinks), alpha = 0.9) +
        ggtitle("okcupid: drinking and age") + facet_grid(sex~.) +
        coord_flip() + 
        scale_fill_brewer(palette = 4, direction = 1) + 
        ylab("percent") + 
        xlab("age")
    
    print(p)
    
    print(p2)
    
    p3 <- ggplot(bnA_final, aes(x = age_range, y = percent, color = drinks)) + geom_line(size = 2) +
        facet_grid(sex~.) +
        ggtitle("okcupid: drinking versus age") + 
        scale_y_log10(breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50, 100))
    
    print(p3)
    
##SEX RATIOS

    ## RELIGION AND DRINKING
    
    ## clean religion data into broad category
    bnG <- filter(profiles, !is.na(drinks), !is.na(religion), !is.na(sex)) %>% as_data_frame
    
    # # A tibble: 38,729 x 22
    #       age     body_type                diet
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
    
    # Source: local data frame [106 x 4]
    # Groups: drinks, religious_affil [?]
    # 
    #         drinks religious_affil   sex     n
    #         <fctr>          <fctr> <chr> <int>
    # 1  desperately     agnosticism     f    25
    # 2  desperately     agnosticism     m    37
    # 3  desperately         atheism     f    18
    # 4  desperately         atheism     m    40
    # 5  desperately        buddhism     f     5
    # 6  desperately        buddhism     m    15
    # 7  desperately     catholicism     f     4
    # 8  desperately     catholicism     m     7
    # 9  desperately    christianity     f     7
    # 10 desperately    christianity     m     7
    # # ... with 96 more rows
    
    
    bnG_r <- group_by(bnG[,c("religious_affil", "sex")], religious_affil, sex) %>% summarize(NN = n())
    
    # Source: local data frame [18 x 3]
    # Groups: religious_affil [?]
    # 
    #    religious_affil   sex    NN
    #             <fctr> <chr> <int>
    # 1      agnosticism     f  3184
    # 2      agnosticism     m  5421
    # 3          atheism     f  1882
    # 4          atheism     m  4921
    # 5         buddhism     f   791
    # 6         buddhism     m  1090
    # 7      catholicism     f  2059
    # 8      catholicism     m  2613
    # 9     christianity     f  2606
    # 10    christianity     m  3052
    # 11        hinduism     f   143
    # 12        hinduism     m   293
    # 13           islam     f    34
    # 14           islam     m    90
    # 15         judaism     f  1476
    # 16         judaism     m  1537
    # 17           other     f  3300
    # 18           other     m  4237
    
    
    
    
    #bnG_rd <- merge(bnG_rd, bnG_r, by = c("religious_affil", "sex")) %>% as_data_frame
    
    # # A tibble: 106 x 5
    #    religious_affil   sex      drinks     n    NN
    #             <fctr> <chr>      <fctr> <int> <int>
    # 1      agnosticism     f desperately    25  3184
    # 2      agnosticism     f      rarely   263  3184
    # 3      agnosticism     f    socially  2412  3184
    # 4      agnosticism     f  very often    27  3184
    # 5      agnosticism     f  not at all   112  3184
    # 6      agnosticism     f       often   345  3184
    # 7      agnosticism     m      rarely   492  5421
    # 8      agnosticism     m    socially  3987  5421
    # 9      agnosticism     m  very often    39  5421
    # 10     agnosticism     m  not at all   244  5421
    # # ... with 96 more rows
    
    
    
    bnG_final <- left_join(bnG_rd, bnG_r) %>% mutate(freq = n/NN, freq = ifelse(is.na(freq), 0, freq))
    
    
    
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
    
    
    
    p <- ggplot(bnG_final, aes(religious_affil, weight = 100*freq)) +
        geom_bar(aes(fill = drinks), alpha = 0.9) +
        ggtitle("okcupid: drinking and religion") + facet_grid(sex~.) +
        coord_flip() + 
        scale_fill_brewer(palette = 4, direction = 1) + 
        ylab("percent") + 
        xlab("religious affiliation")
    
    print(p)
    
    