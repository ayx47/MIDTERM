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
    MT <- unite(code1111[12:22], "MT", c(MT10, MT1, MT1_2, MT2, MT3, MT4, MT5, MT6, MT7, MT8, MT9), sep = ",")
    FT <- unite(code1111[23:33], "FT", c(FT10, FT1, FT1_2, FT2, FT3, FT4, FT5, FT6, FT7, FT8, FT9), sep = ",")
    WHT <- unite(code1111[34:44], "WHT", c( WHT10, WHT1, WHT1_2, WHT2, WHT3, WHT4, WHT5, WHT6, WHT7, WHT8, WHT9), sep = ",")
    WHM <- unite(code1111[45:55],"WHM",c(WHM10, WHM1, WHM1_2, WHM2, WHM3, WHM4, WHM5, WHM6, WHM7, WHM8, WHM9), sep = ",")
    WHF <- unite(code1111[56:66], "WHF", c(WHF10, WHF1, WHF1_2, WHF2, WHF3, WHF4, WHF5, WHF6, WHF7, WHF8, WHF9), sep = ",")
    BLKT <- unite(code1111[67:77],"BLKT", c(BLKT10, BLKT1, BLKT1_2, BLKT2, BLKT3, BLKT4, BLKT5, BLKT6, BLKT7, BLKT8, BLKT9), sep = ",")
    BLKM <- unite(code1111[78:88],"BLKM", c(BLKM10, BLKM1, BLKM1_2, BLKM2, BLKM3, BLKM4, BLKM5, BLKM6, BLKM7, BLKM8, BLKM9), sep = ",")
    BLKF <- unite(code1111[89:99], "BLKF", c(BLKF10, BLKF1, BLKF1_2, BLKF2, BLKF3, BLKF4, BLKF5, BLKF6, BLKF7, BLKF8, BLKF9), sep = ",")
    HISPT <- unite(code1111[100:110], "HISPT", c(HISPT10, HISPT1, HISPT1_2, HISPT2, HISPT3, HISPT4, HISPT5, HISPT6, HISPT7, HISPT8, HISPT9), sep = ",")
    HISPM <- unite(code1111[111:121], "HISPM", c(HISPM10, HISPM1, HISPM1_2, HISPM2, HISPM3, HISPM4, HISPM5, HISPM6, HISPM7, HISPM8, HISPM9),sep = ",")
    HISPF <- unite(code1111[122:132], "HISPF", c(HISPF10, HISPF1, HISPF1_2, HISPF2, HISPF3, HISPF4, HISPF5, HISPF6, HISPF7, HISPF8, HISPF9),sep = ",")
    ASIANT <- unite(code1111[133:143], "ASIANT", c(ASIANT10, ASIANT1, ASIANT1_2, ASIANT2, ASIANT3, ASIANT4, ASIANT5, ASIANT6, ASIANT7, ASIANT8, ASIANT9),sep = ",")
    ASIANM <- unite(code1111[144:154], "ASIANM", c(ASIANM10, ASIANM1, ASIANM1_2, ASIANM2, ASIANM3, ASIANM4, ASIANM5, ASIANM6, ASIANM7, ASIANM8, ASIANM9),sep = ",")
    ASIANF <- unite(code1111[155:165], "ASIANF", c(ASIANF10, ASIANF1, ASIANF1_2, ASIANF2, ASIANF3, ASIANF4, ASIANF5, ASIANF6, ASIANF7, ASIANF8, ASIANF9),sep = ",")
    AIANT <- unite(code1111[166:176], "AIANT", c(AIANT10, AIANT1, AIANT1_2, AIANT2, AIANT3, AIANT4, AIANT5, AIANT6, AIANT7, AIANT8, AIANT9),sep = ",")
    AIANM <- unite(code1111[177:187], "AIANM", c(AIANM10, AIANM1, AIANM1_2, AIANM2, AIANM3, AIANM4, AIANM5, AIANM6, AIANM7, AIANM8, AIANM9),sep = ",")
    AIANF <- unite(code1111[188:198], "AIANF", c(AIANF10, AIANF1, AIANF1_2, AIANF2, AIANF3, AIANF4, AIANF5, AIANF6, AIANF7, AIANF8, AIANF9),sep = ",")
    NHOPIT <- unite(code1111[199:209], "NHOPIT", c(nhopiT10, nhopiT1, nhopiT1_2, nhopiT2, nhopiT3, nhopiT4, nhopiT5, nhopiT6, nhopiT7, nhopiT8, nhopiT9),sep = ",")
    NHOPIM <- unite(code1111[210:220], "NHOPTM", c(NHOPIM10, NHOPIM1, NHOPIM1_2, NHOPIM2, NHOPIM3, NHOPIM4, NHOPIM5, NHOPIM6, NHOPIM7, NHOPIM8, NHOPIM9),sep = ",")
    NHOPIF <- unite(code1111[221:231], "NHOPTF", c(NHOPIF10, NHOPIF1, NHOPIF1_2, NHOPIF2, NHOPIF3, NHOPIF4, NHOPIF5, NHOPIF6, NHOPIF7, NHOPIF8, NHOPIF9),sep = ",")
    agrt <- cbind(Total, MT, FT, WHT, WHM, WHF, BLKT, BLKM, BLKF, HISPT, HISPM, ASIANT, ASIANM, ASIANF, AIANT, AIANM, AIANF, NHOPIT, NHOPIM, NHOPIF)

    test <- data.frame(t(agrt))
    names(test)[1] <- "randomName"
    print(separate(test,randomName,c("Total Employment", "Executive/Senior Level Officials & Managemeners", "First/Mid Level Officials & Managers",
                                     "Professionals", "Technicians", "Sales Workers", "Office & Clerical Workers", "Craft Workers", "Operatives", 
                                     "Laborers", "Service Workers"), sep = ","))
}
    