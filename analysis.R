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
    agrt <- merge(Total, MT)
    # agrt
    test <- t(agrt)
    # test
    # separate(test,test[,1],c("job1", "job2"), sep = ",")
    
    messy <- data.frame(
        #    name = c("Totaltest"), 
        test
    )
    messy
    print(separate(messy,test,c("job1", "job2", "job3", "job4", "job5", "job6", "job7", "job8", "job9", "job10", "job1_2"), sep = ","))
    
}
# }


# callRow(1111)
# 
# which(1112 == c(YEAR09_NAC4[1:301,1]))
