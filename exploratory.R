## OKCupid Data Explore

library(okcupiddata)
library(dplyr)
library(ggplot2)




profiles_m <- profiles

## RELIGION AND DRINKING

    ## clean religion data into broad category
    bnG <- filter(profiles_m, !is.na(drinks), !is.na(religion), !is.na(sex))

    ## get affiliation
    bnG$relig <- gsub(' [A-z ]*', '', bnG$religion) %>% as.factor()
    
    bnG$drinks <- bnG$drinks %>% as.factor()
    
    bnG_rd <- group_by(bnG[,c("drinks", "relig", "sex")], drinks, relig, sex) %>% summarize(n = n())
    
    bnG_r <- group_by(bnG, relig, sex) %>% summarize(n = n())
    
    
            relig_count <- function(x) {
                if (x %in% bnG_r$relig) {
                    y = bnG_r$n[which(x == bnG_r$relig)] %>% as.numeric
                } else {
                    y = 0 }
                
                y[1]
            }
            
            vv <- lapply(bnG_rd$relig, relig_count) %>% as.list
            vvv <-do.call(rbind,lapply(vv,as.data.frame))
            colnames(vvv) <- "NN"
            NN <- vvv %>% as_data_frame
    
    bnG_rd <- bnG_rd %>% as_data_frame
            
    ## right now cbind does not support tibbles (issue opened in May)
    bnG_rd <- cbind(bnG_rd, NN) %>% as_data_frame
    
    bnG_final <- mutate(bnG_rd, freq = n/NN)
    
    p <- ggplot(bnG_final, aes(x = drinks, y = relig, size = freq)) +
        geom_point(alpha = 0.9, color = "#4466FF") +
        ggtitle("okcupid: drinking and religion") + facet_grid(sex~.)
    

    print(p)
    