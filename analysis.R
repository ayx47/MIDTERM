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
    FT <- unite(code1111[23:33], "FT", c(FT10, FT1, FT1_2, FT2, FT3, FT4, FT5, FT6, FT7, FT8, FT9), sep = ",")
    WHT <- unite(code1111[34:44], "WHT", c( WHT10, WHT1, WHT1_2, WHT2, WHT3, WHT4, WHT5, WHT6, WHT7, WHT8, WHT9), sep = ",")
    WHM <- unite(code1111[45:55],"WHM",c(WHM10, WHM1, WHM1_2, WHM2, WHM3, WHM4, WHM5, WHM6, WHM7, WHM8, WHM9), sep = ",")
    WHF <- unite(code1111[56:66], "WHF", c(WHF10, WHF1, WHF1_2, WHF2, WHF3, WHF4, WHF5, WHF6, WHF7, WHF8, WHF9), sep = ",")
    BLKT <- unite(code1111[67:77],"BLKT", c(BLKT10, BLKT1, BLKT1_2, BLKT2, BLKT3, BLKT4, BLKT5, BLKT6, BLKT7, BLKT8, BLKT9), sep = ",")
    BLKM <- unite(code1111[78:88],"BLKM", c(BLKM10, BLKM1, BLKM1_2, BLKM2, BLKM3, BLKM4, BLKM5, BLKM6, BLKM7, BLKM8, BLKM9), sep = ",")
    BLKF <- unite(code1111[89:99], "BLKF", c(BLKF10, BLKF1, BLKF1_2, BLKF2, BLKF3, BLKF4, BLKF5, BLKF6, BLKF7, BLKF8, BLKF9), sep = ",")
    
>>>>>>> Stashed changes
    agrt <- cbind(Total, MT, FT, WHT, WHM, WHF)

    test <- data.frame(t(agrt))
    names(test)[1] <- "randomName"
    print(separate(test,randomName,c("Total Employment", "Executive/Senior Level Officials & Managemeners", "First/Mid Level Officials & Managers",
                                     "Professionals", "Technicians", "Sales Workers", "Office & Clerical Workers", "Craft Workers", "Operatives", 
                                     "Laborers", "Service Workers"), sep = ","))
}
    