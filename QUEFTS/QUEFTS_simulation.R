source("QUEFTS_functions.R")

settings <- data.frame(
    waterlimited_yield=6000,
    recover_N=0.5,
    recover_P=0.1,
    recover_K=0.5,
    AvgTemp= 20,
    crop="soybean"
)

##set new total N rate based on assumtions on nitrogen fixation n.fix=x in legumes
n.fix <- 111 * 2 ###kg N fixed Salvagiotti08, correct for recovery (x2)

##set ferilizer rates
tN <- 0 + n.fix	
tP <- 30
tK <- 50

soil_properties <- list(
    olsenP=5.0,
    exchK=5.0,
    SOC=20,
    pH=6
)

tYld.control <- QUEFTS_example(soil_properties, kgNha=tN, kgPha=0, kgKha=0, settings)
tYld.treat <- QUEFTS_example(soil_properties, kgNha=tN, kgPha=tP, kgKha=0, settings)

##calculate response
tResponse<-tYld.treat-tYld.control

##show results
tYld.control
tYld.treat
tResponse