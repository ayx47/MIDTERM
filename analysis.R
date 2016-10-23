library(tidyr)
callRow <- function(x) {
    rownum <- which(x == c(YEAR09_NAC4[1:311,1]))
    return(rownum)
}

output <- function(x) {
    NAC4 <- YEAR09_NAC4
    code1111 <- YEAR09_NAC4[callRow(x),-1:-2]
    # code1111 <- t(code1111)
    Total <- unite(code1111[1:11],"Total",c(TOTAL10, TOTAL1,TOTAL1_2, TOTAL2, TOTAL3, TOTAL4, TOTAL5, TOTAL6, TOTAL7, TOTAL8, TOTAL9), sep = ",")
    MT <- unite(code1111[12:22], "MT", c(MT10, MT1, MT1_1, MT2, MT3, MT4, MT5, MT6, MT7, MT8, MT9), sep = ",")
    FT <- unite(code1111[23:33], "FT", c(FT10, FT1, FT1_2, FT2, FT3, FT4, FT5, FT6, FT7, FT8, FT9, FT10, FT1_2), sep = ",")
    WHT <- unite(code1111[34:44], "WHT", c( WHT10, WHT1, WHT1_2, WHT2, WHT3, WHT4, WHT5, WHT6, WHT7, WHT8, WHT9), sep = ",")
    WHM <- unite(code1111[45:55],"WHM",c(WHM10, WHM1, WHM1_2, WHM2, WHM3, WHM4, WHM5, WHM6, WHM7, WHM8, WHM9, WHM10, WHM1_2), sep = ",")
    WHF <- unite(code1111[56:66], "WHF", c(WHF10, WHF1, WHF1_2, WHF2, WHF3, WHF4, WHF5, WHF6, WHF7, WHF8, WHF9, WHF10, WHF1_2), sep = ",")
    agrt <- cbind(Total, MT, FT, WHT, WHM, WHF)

    test <- data.frame(t(agrt))
    names(test)[1] <- "randomName"

    print(separate(test,randomName,c("job1", "job2", "job3", "job4", "job5", "job6", "job7", "job8", "job9", "job10", "job1_2"), sep = ","))
}