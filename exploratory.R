## OKCupid Data Explore

library(okcupiddata)
library(dplyr)
library(ggplot2)




profiles_m <- profiles

## RELIGION AND DRINKING

    ## clean religion data into broad category
    bnG <- filter(profiles_m, !is.na(drinks), !is.na(religion), !is.na(sex)) %>% as_data_frame
    
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
    

    ## get affiliation (strip modifiers) using gsu and regex
    bnG$relig <- gsub(' [A-z ]*', '', bnG$religion) %>% as.factor()
    ## convert bnG$drinks to factor
    bnG$drinks <- bnG$drinks %>% as.factor()
    ## group the data 
    bnG_rd <- group_by(bnG[,c("drinks", "relig", "sex")], drinks, relig, sex) %>% summarize(n = n())
    
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
    
    
    bnG_r <- group_by(bnG, relig, sex) %>% summarize(n = n())
    
    # Source: local data frame [18 x 3]
    # Groups: relig [?]
    # 
    #           relig   sex     n
    #          <fctr> <chr> <int>
    # 1   agnosticism     f  3184
    # 2   agnosticism     m  5421
    # 3       atheism     f  1882
    # 4       atheism     m  4921
    # 5      buddhism     f   791
    # 6      buddhism     m  1090
    # 7   catholicism     f  2059
    # 8   catholicism     m  2613
    # 9  christianity     f  2606
    # 10 christianity     m  3052
    # 11     hinduism     f   143
    # 12     hinduism     m   293
    # 13        islam     f    34
    # 14        islam     m    90
    # 15      judaism     f  1476
    # 16      judaism     m  1537
    # 17        other     f  3300
    # 18        other     m  4237
    
    ## Now need to normalize the religiously categorized drinker types to the total drinkers by religious affiliation
    ## This is more complex, so create a lookup function
            
            relig_count <- function(x) {
                if (x %in% bnG_r$relig) {
                    y = bnG_r$n[which(x == bnG_r$relig)] %>% as.numeric
                } else {
                    y = 0 }
                
                y[1]
            }
            
            ## apply the function to the bnG_rd data to create a list of total drinkers by affiliation
            vv <- lapply(bnG_rd$relig, relig_count) %>% as.list
            ## convert to data
            vvv <-do.call(rbind,lapply(vv,as.data.frame))
            
            colnames(vvv) <- "NN"
            NN <- vvv %>% as_data_frame
            
            # # A tibble: 106 x 1
            #       NN
            # *  <dbl>
            # 1   3184
            # 2   3184
            # 3   1882
            # 4   1882
            # 5    791
            # 6    791
            # 7   2059
            # 8   2059
            # 9   2606
            # 10  2606
            # # ... with 96 more rows
    
    bnG_rd <- bnG_rd %>% as_data_frame
            
    ## right now cbind does not support tibbles (issue opened in May)
    bnG_rd <- cbind(bnG_rd, NN) %>% as_data_frame
    
    bnG_final <- mutate(bnG_rd, freq = n/NN)
    
    
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
    
    p <- ggplot(bnG_final, aes(x = drinks, y = relig, size = freq)) +
        geom_point(alpha = 0.9, fill = "#FF9933", pch=21, color = "darkred") +
        ggtitle("okcupid: drinking and religion") + facet_grid(sex~.)
    

    print(p)
    