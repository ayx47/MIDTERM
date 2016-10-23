library(tidyr)
callRow <- function(x) {
    rownum <- which(x == c(YEAR09_NAC4[1:311,1]))
    return(rownum)
}

# fileName <- "YEAR09_NAC4.txt"
# conn <- file(fileName, open="r")
# linn <- readLines(conn)
# for (i in callRow(x)) {

output <- function(x) {
    NAC4 <- YEAR09_NAC4
    code1111 <- YEAR09_NAC4[callRow(x),-1:-2]
    # code1111 <- t(code1111)
    Total <- unite(code1111[1:11],"Total",c(TOTAL1, TOTAL2, TOTAL3, TOTAL4, TOTAL5, TOTAL6, TOTAL7, TOTAL8, TOTAL9, TOTAL10, TOTAL1_2), sep = ",")
    MT <- unite(code1111[12:22], "MT", c(MT1, MT2, MT3, MT4, MT5, MT6, MT7, MT8, MT9, MT10, MT1_2), sep = ",")
    FT <- unite(code1111[23:33], "FT", c(FT1, FT2, FT3, FT4, FT5, FT6, FT7, FT8, FT9, FT10, FT1_2), sep = ",")
    WHT <- unite(code1111[34:44], "WHT", c(WHT1, WHT2, WHT3, WHT4, WHT5, WHT6, WHT7, WHT8, WHT9, WHT10, WHT1_2), sep = ",")
    agrt <- cbind(Total, MT, FT, WHT)

    test <- data.frame(t(agrt))
    names(test)[1] <- "randomName"
    # test
    # separate(test,test[,1],c("job1", "job2"), sep = ",")
    
    # messy <- data.frame(
    #     #    name = c("Totaltest"), 
    #     test
    # )
    # messy
    print(separate(test,randomName,c("job1", "job2", "job3", "job4", "job5", "job6", "job7", "job8", "job9", "job10", "job1_2"), sep = ","))
    
}
# }

# callRow(1111)
# 
# which(1112 == c(YEAR09_NAC4[1:301,1]))
