Data4list <- split(KLEMS_4, KLEMS_4$Industry)
dlist4 <- lapply(seq_along(Data4list), function(x) as.data.frame(Data4list[[x]])) 

lapply(seq_along(dlist4), function(x) {
  assign(paste0("Data4.", x), Data4list[[x]], envir=.GlobalEnv)
}
)

#rm(list = ls()[grep("^Data2.", ls())])
#rm(list = ls()[grep("^Agg2.", ls())])

R2_4D <- data.frame(matrix(NA, ncol=13, nrow=160))
names(R2_4D) <- c("Industry", "R2_Eq1", "R2_Eq2", "R2_Eq3", "R2_Eq4", "Bg_Eq1", "Bg_Eq2",
                  "Bg_Eq3", "Bg_Eq4", "Mono", "Non-Mono", "Concavity", "Non-Concavity")

Coef_4D <- data.frame(matrix(NA, ncol=33, nrow=160))
names(Coef_4D) <- c("Industry",
                    "eq1_(Intercept)", "eq1_lnK", "eq1_lnL", "eq1_lnE", "eq1_lnM", "eq1_lnY", "eq1_t", "eq1_dK",
                    "eq2_(Intercept)", "eq2_lnK", "eq2_lnL", "eq2_lnE", "eq2_lnM", "eq2_lnY", "eq2_t", "eq2_dL",
                    "eq3_(Intercept)", "eq3_lnK", "eq3_lnL", "eq3_lnE", "eq3_lnM", "eq3_lnY", "eq3_t", "eq3_dE",
                    "eq4_(Intercept)", "eq4_lnK", "eq4_lnL", "eq4_lnE", "eq4_lnM", "eq4_lnY", "eq4_t", "eq4_dM")

Err_4D <- data.frame(matrix(NA, ncol=33, nrow=160))
names(Err_4D) <- c("Industry",
                   "eq1_(Intercept)", "eq1_lnK", "eq1_lnL", "eq1_lnE", "eq1_lnM", "eq1_lnY", "eq1_t", "eq1_dK",
                   "eq2_(Intercept)", "eq2_lnK", "eq2_lnL", "eq2_lnE", "eq2_lnM", "eq2_lnY", "eq2_t", "eq2_dL",
                   "eq3_(Intercept)", "eq3_lnK", "eq3_lnL", "eq3_lnE", "eq3_lnM", "eq3_lnY", "eq3_t", "eq3_dE",
                   "eq4_(Intercept)", "eq4_lnK", "eq4_lnL", "eq4_lnE", "eq4_lnM", "eq4_lnY", "eq4_t", "eq4_dM")

pval_4D <- data.frame(matrix(NA, ncol=33, nrow=160))
names(pval_4D) <- c("Industry",
                    "eq1_(Intercept)", "eq1_lnK", "eq1_lnL", "eq1_lnE", "eq1_lnM", "eq1_lnY", "eq1_t", "eq1_dK",
                    "eq2_(Intercept)", "eq2_lnK", "eq2_lnL", "eq2_lnE", "eq2_lnM", "eq2_lnY", "eq2_t", "eq2_dL",
                    "eq3_(Intercept)", "eq3_lnK", "eq3_lnL", "eq3_lnE", "eq3_lnM", "eq3_lnY", "eq3_t", "eq3_dE",
                    "eq4_(Intercept)", "eq4_lnK", "eq4_lnL", "eq4_lnE", "eq4_lnM", "eq4_lnY", "eq4_t", "eq4_dM")

Elast_4D <- data.frame(matrix(NA, ncol=26, nrow=160))
names(Elast_4D) <- c("Industry", "e_KK", "e_KL", "e_KE", "e_KM", "e_KS", "e_LK", "e_LL", "e_LE", "e_LM", "e_LS",
                     "e_EK", "e_EL", "e_EE", "e_EM", "e_ES", "e_MK", "e_ML", "e_ME", "e_MM", "e_MS",
                     "e_SK", "e_SL", "e_SE", "e_SM", "e_SS")

dElast_4D <- data.frame(matrix(NA, ncol=26, nrow=160))
names(dElast_4D) <- c("Industry", "de_KK", "de_KL", "de_KE", "de_KM", "de_KS",
                      "de_LK", "de_LL", "de_LE", "de_LM", "de_LS", "de_EK", "de_EL", "de_EE", "de_EM", "de_ES",
                      "de_MK", "de_ML", "de_ME", "de_MM", "de_MS", "de_SK", "de_SL", "de_SE", "de_SM", "de_SS")

GlobalTest <- data.frame(matrix(NA, ncol=17, nrow=0))
names(GlobalTest) <- c("Industry", "Country", "Year", "ActSK", "ActSL", "ActSE", "ActSM", "FitSK", "FitSL", "FitSE", "FitSM",
                       "ResSK", "ResSL", "ResSE", "ResSM", "Monotonicity", "Concavity")

for(i in c(1:17)){ #1:61
  data <- get(paste0("Data4.",i))
  
  success <- FALSE
  
  data$pK <- data$K / data$Kc
  data$pL <- data$L / data$Lc
  data$pE <- data$E / data$Ec
  data$pM <- data$M / data$Mc
  data$pS <- data$S / data$Sc
  
  data$pGO <- data$GO / data$GOc
  data$TC <- data$K + data$L + data$E + data$M + data$S
  
  data$SK <- data$K / data$TC
  data$SL <- data$L / data$TC
  data$SE <- data$E / data$TC
  data$SM <- data$M / data$TC
  data$SS <- data$S / data$TC
  
  data$lnK <- log(data$pK/data$pS)
  data$lnL <- log(data$pL/data$pS)
  data$lnE <- log(data$pE/data$pS)
  data$lnM <- log(data$pM/data$pS)
  data$lnC <- log(data$TC/data$pS)
  
  data$lnK2 <- 0.5*(data$lnK)^2
  data$lnL2 <- 0.5*(data$lnL)^2
  data$lnE2 <- 0.5*(data$lnE)^2
  data$lnM2 <- 0.5*(data$lnM)^2
  
  data$lnKL <- data$lnK*data$lnL
  data$lnKE <- data$lnK*data$lnE
  data$lnKM <- data$lnK*data$lnM
  
  data$lnLE <- data$lnL*data$lnE
  data$lnLM <- data$lnL*data$lnM
  
  data$lnEM <- data$lnE*data$lnM
  
  data$lnY <- log(data$GOc)
  data$lnY2 <- 0.5*data$lnY^2
  
  data$lnKY <- data$lnK*data$lnY
  data$lnLY <- data$lnL*data$lnY
  data$lnEY <- data$lnE*data$lnY
  data$lnMY <- data$lnM*data$lnY
  
  data$t <- data$Year - 2010
  data$t2 <- 0.5*data$t^2
  
  data$lnKt <- data$lnK*data$t
  data$lnLt <- data$lnL*data$t
  data$lnEt <- data$lnE*data$t
  data$lnMt <- data$lnM*data$t
  
  data$dK <- NA
  data$dL <- NA
  data$dE <- NA
  data$dM <- NA
  
  data$lnK1 <- NA
  data$lnL1 <- NA
  data$lnE1 <- NA
  data$lnM1 <- NA
  data$lnY1 <- NA
  
  data$t1 <- NA
  
  data$dK1 <- NA
  data$dL1 <- NA
  data$dE1 <- NA
  data$dM1 <- NA
  
  for(j in c(1:nrow(data))){
    ann <- data$Year[j]
    ctr <- data$Country[j]
    ind <- data$Industry[j]
    
    k1_id <- data$SK[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    l1_id <- data$SL[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    e1_id <- data$SE[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    m1_id <- data$SM[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    s1_id <- data$SS[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    
    k2_id <- data$SK[data$Year == (ann - 2) & data$Country == ctr & data$Industry == ind]
    l2_id <- data$SL[data$Year == (ann - 2) & data$Country == ctr & data$Industry == ind]
    e2_id <- data$SE[data$Year == (ann - 2) & data$Country == ctr & data$Industry == ind]
    m2_id <- data$SM[data$Year == (ann - 2) & data$Country == ctr & data$Industry == ind]
    s2_id <- data$SS[data$Year == (ann - 2) & data$Country == ctr & data$Industry == ind]
    
    try(c(
      data$dK[j] <- k1_id,
      data$dL[j] <- l1_id,
      data$dE[j] <- e1_id,
      data$dM[j] <- m1_id,
      
      data$dK1[j] <- k2_id,
      data$dL1[j] <- l2_id,
      data$dE1[j] <- e2_id,
      data$dM1[j] <- m2_id), silent = TRUE)#- m2_id), silent = TRUE)
  }
  
  for(j in c(1:nrow(data))){
    ann <- data$Year[j]
    ctr <- data$Country[j]
    ind <- data$Industry[j]
    
    ln_k <- data$lnK[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    ln_l <- data$lnL[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    ln_e <- data$lnE[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    ln_m <- data$lnM[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    ln_y <- data$lnY[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    t1 <- data$t[data$Year == (ann - 1) & data$Country == ctr & data$Industry == ind]
    
    try(c(
      data$lnK1[j] <- ln_k, #- data$lnK[j],
      data$lnL1[j] <- ln_l, #- data$lnL[j],
      data$lnE1[j] <- ln_e, #- data$lnE[j],
      data$lnM1[j] <- ln_m, #- data$lnM[j],
      data$lnY1[j] <- ln_y, #- data$lnY[j],
      data$t1[j] <- t1), silent = TRUE)# - data$lnY[j]), silent = TRUE)
  }
  
  data <- na.omit(data)
  
  eq1 <- SK ~ lnK + lnL + lnE + lnM + lnY + t + dK #+ Country
  eq2 <- SL ~ lnK + lnL + lnE + lnM + lnY + t + dL #+ Country
  eq3 <- SE ~ lnK + lnL + lnE + lnM + lnY + t + dE #+ Country
  eq4 <- SM ~ lnK + lnL + lnE + lnM + lnY + t + dM #+ Country
  
  # eq1 <- SK ~ lnK + lnL + lnE + lnM + lnY + t + Country
  # eq2 <- SL ~ lnK + lnL + lnE + lnM + lnY + t + Country
  # eq3 <- SE ~ lnK + lnL + lnE + lnM + lnY + t + Country
  # eq4 <- SM ~ lnK + lnL + lnE + lnM + lnY + t + Country
  
  eqlist <- list (eq1, eq2, eq3, eq4)
  
  restrict1 <- "eq1_lnL - eq2_lnK = 0"
  restrict2 <- "eq1_lnE - eq3_lnK = 0"
  restrict3 <- "eq1_lnM - eq4_lnK = 0"
  restrict4 <- "eq2_lnE - eq3_lnL = 0"
  restrict5 <- "eq2_lnM - eq4_lnL = 0"
  restrict6 <- "eq3_lnM - eq4_lnE = 0"
  restrict7 <- "eq1_dK - eq2_dL = 0"
  restrict8 <- "eq1_dK - eq3_dE = 0"
  restrict9 <- "eq1_dK - eq4_dM = 0"
  
  restrict <- c(restrict1, restrict2, restrict3, restrict4, restrict5, restrict6,
                restrict7, restrict8, restrict9)
  
  inst1 <- ~lnK1 + lnL1 + lnE1 + lnM1 + lnY1 + t1 + dK1
  inst2 <- ~lnK1 + lnL1 + lnE1 + lnM1 + lnY1 + t1 + dL1
  inst3 <- ~lnK1 + lnL1 + lnE1 + lnM1 + lnY1 + t1 + dE1
  inst4 <- ~lnK1 + lnL1 + lnE1 + lnM1 + lnY1 + t1 + dM1
  
  inst <- list(inst1, inst2, inst3, inst4)
  
  try(c(system_eqn <- systemfit(eqlist, data = data, method="3SLS", inst = inst,
                                method3sls="GMM", restrict.matrix = restrict, maxiter = 100),
        R2 <- c((summary(system_eqn)[[10]][[1]])$r.squared, (summary(system_eqn)[[10]][[2]])$r.squared, 
                (summary(system_eqn)[[10]][[3]])$r.squared, (summary(system_eqn)[[10]][[4]])$r.squared,
                pbgtest(SK ~ lnK + lnL + lnE + lnM + lnY + t + dK, order.by = data$Year, data=data, type = "Chisq")[4],
                pbgtest(SL ~ lnK + lnL + lnE + lnM + lnY + t + dL, order.by = data$Year, data=data, type = "Chisq")[4],
                pbgtest(SE ~ lnK + lnL + lnE + lnM + lnY + t + dE, order.by = data$Year, data=data, type = "Chisq")[4],
                pbgtest(SM ~ lnK + lnL + lnE + lnM + lnY + t + dM, order.by = data$Year, data=data, type = "Chisq")[4]),
        success <- TRUE
  ), silent = TRUE)
  
  R2_4D[i,1] <- i
  Coef_4D[i,1] <- i
  Err_4D[i,1] <- i
  pval_4D[i,1] <- i
  Elast_4D[i,1] <- i
  dElast_4D[i,1] <- i
  
  if (success == FALSE | any(R2[1:4] < 0)){
    print(i)
  } else {
    
    for (j in 1:8){
      R2_4D[i,j+1] <- R2[j] #(summary(system_eqn)[[10]][[1]])$r.squared
    }
    
    for (j in 1:8){
      Coef_4D[i,j+1] <- (summary(system_eqn)[[10]][[1]])$coefficients[j,1]
      Coef_4D[i,j+9] <- (summary(system_eqn)[[10]][[2]])$coefficients[j,1]
      Coef_4D[i,j+17] <- (summary(system_eqn)[[10]][[3]])$coefficients[j,1]
      Coef_4D[i,j+25] <- (summary(system_eqn)[[10]][[4]])$coefficients[j,1]
      Err_4D[i,j+1] <- (summary(system_eqn)[[10]][[1]])$coefficients[j,2]
      Err_4D[i,j+9] <- (summary(system_eqn)[[10]][[2]])$coefficients[j,2]
      Err_4D[i,j+17] <- (summary(system_eqn)[[10]][[3]])$coefficients[j,2]
      Err_4D[i,j+25] <- (summary(system_eqn)[[10]][[4]])$coefficients[j,2]
      pval_4D[i,j+1] <- (summary(system_eqn)[[10]][[1]])$coefficients[j,4]
      pval_4D[i,j+9] <- (summary(system_eqn)[[10]][[2]])$coefficients[j,4]
      pval_4D[i,j+17] <- (summary(system_eqn)[[10]][[3]])$coefficients[j,4]
      pval_4D[i,j+25] <- (summary(system_eqn)[[10]][[4]])$coefficients[j,4]
    }
    
    N <- nrow(data)
    Ntrue <- sum(((fitted(system_eqn)[,1] + fitted(system_eqn)[,2] + fitted(system_eqn)[,3] + fitted(system_eqn)[,4] < 1) &
                    (fitted(system_eqn)[,1] > 0 & fitted(system_eqn)[,2] > 0)) &
                    (fitted(system_eqn)[,3] > 0 & fitted(system_eqn)[,4] > 0))
    Nfalse <- N - Ntrue
    
    R2_4D[i,10] <- Ntrue
    R2_4D[i,11] <- Nfalse
    
    #nc <- length(unique(data$Country)) - 1
    
    Elast <- array(rep(NA, 25), dim=25)
    dElast <- array(rep(NA, 25), dim=25)

    SKav <- mean(data$SK)
    SLav <- mean(data$SL)
    SEav <- mean(data$SE)
    SMav <- mean(data$SM)
    SSav <- mean(data$SS)

    SKsd <- sd(data$SK)
    SLsd <- sd(data$SL)
    SEsd <- sd(data$SE)
    SMsd <- sd(data$SM)
    SSsd <- sd(data$SS)

    Lambda <- (summary(system_eqn)[[10]][[1]])$coefficients[8]

    # Alpha_K <- (summary(system_eqn)[[10]][[1]])$coefficients[1] / (1 - Lambda)
    # Alpha_L <- (summary(system_eqn)[[10]][[2]])$coefficients[1] / (1 - Lambda)
    # Alpha_E <- (summary(system_eqn)[[10]][[3]])$coefficients[1] / (1 - Lambda)
    # Alpha_M <- (summary(system_eqn)[[10]][[4]])$coefficients[1] / (1 - Lambda)
    # Alpha_S <- 1 - Alpha_K - Alpha_L - Alpha_E - Alpha_S

    Beta_KK <- (summary(system_eqn)[[10]][[1]])$coefficients[2]
    Beta_KL <- (summary(system_eqn)[[10]][[1]])$coefficients[3]
    Beta_KE <- (summary(system_eqn)[[10]][[1]])$coefficients[4]
    Beta_KM <- (summary(system_eqn)[[10]][[1]])$coefficients[5]
    Beta_KS <- - Beta_KK - Beta_KL - Beta_KE - Beta_KM

    Beta_LK <- Beta_KL
    Beta_LL <- (summary(system_eqn)[[10]][[2]])$coefficients[3]
    Beta_LE <- (summary(system_eqn)[[10]][[2]])$coefficients[4]
    Beta_LM <- (summary(system_eqn)[[10]][[2]])$coefficients[5]
    Beta_LS <- - Beta_LK - Beta_LL - Beta_LE - Beta_LM

    Beta_EK <- Beta_KE
    Beta_EL <- Beta_LE
    Beta_EE <- (summary(system_eqn)[[10]][[3]])$coefficients[4]
    Beta_EM <- (summary(system_eqn)[[10]][[3]])$coefficients[5]
    Beta_ES <- - Beta_EK - Beta_EL - Beta_EE - Beta_EM
    
    Beta_MK <- Beta_KM
    Beta_ML <- Beta_LM
    Beta_ME <- Beta_EM
    Beta_MM <- (summary(system_eqn)[[10]][[4]])$coefficients[5]
    Beta_MS <- - Beta_MK - Beta_ML - Beta_ME - Beta_MM

    Beta_SK <- Beta_KS
    Beta_SL <- Beta_LS
    Beta_SE <- Beta_ES
    Beta_SM <- Beta_MS
    Beta_SS <- - Beta_SK - Beta_SL - Beta_SE - Beta_SM

    data$kk <- (Beta_KK * Lambda + data$SK^2 - data$SK)/data$SK
    data$ll <- (Beta_LL * Lambda + data$SL^2 - data$SL)/data$SL
    data$ee <- (Beta_EE * Lambda + data$SE^2 - data$SE)/data$SE
    data$mm <- (Beta_MM * Lambda + data$SM^2 - data$SM)/data$SM
    data$ss <- (Beta_SS * Lambda + data$SS^2 - data$SS)/data$SS

    Ntrue <- sum((data$kk < 0 & data$ll < 0 & data$ee < 0 & data$mm < 0 & data$ss < 0))
    Nfalse <- N - Ntrue

    R2_4D[i,12] <- Ntrue
    R2_4D[i,13] <- Nfalse

    Elast[1] <- (Beta_KK * Lambda + SKav^2 - SKav) / SKav
    Elast[2] <- (Beta_KL * Lambda + SKav * SLav) / SKav
    Elast[3] <- (Beta_KE * Lambda + SKav * SEav) / SKav
    Elast[4] <- (Beta_KM * Lambda + SKav * SMav) / SKav
    Elast[5] <- (Beta_KS * Lambda + SKav * SSav) / SKav

    Elast[6] <- (Beta_LK * Lambda + SLav * SKav) / SLav
    Elast[7] <- (Beta_LL * Lambda + SLav^2 - SLav) / SLav
    Elast[8] <- (Beta_LE * Lambda + SLav * SEav) / SLav
    Elast[9] <- (Beta_LM * Lambda + SLav * SMav) / SLav
    Elast[10] <- (Beta_LS * Lambda + SLav * SSav) / SLav

    Elast[11] <- (Beta_EK * Lambda + SEav * SKav) / SEav
    Elast[12] <- (Beta_EL * Lambda + SEav * SLav) / SEav
    Elast[13] <- (Beta_EE * Lambda + SEav^2 - SEav) / SEav
    Elast[14] <- (Beta_EM * Lambda + SEav * SMav) / SEav
    Elast[15] <- (Beta_ES * Lambda + SEav * SSav) / SEav

    Elast[16] <- (Beta_MK * Lambda + SMav * SKav) / SMav
    Elast[17] <- (Beta_ML * Lambda + SMav * SLav) / SMav
    Elast[18] <- (Beta_ME * Lambda + SMav * SEav) / SMav
    Elast[19] <- (Beta_MM * Lambda + SMav^2 - SMav) / SMav
    Elast[20] <- (Beta_MS * Lambda + SMav * SSav) / SMav

    Elast[21] <- (Beta_SK * Lambda + SSav * SKav) / SSav
    Elast[22] <- (Beta_SL * Lambda + SSav * SLav) / SSav
    Elast[23] <- (Beta_SE * Lambda + SSav * SEav) / SSav
    Elast[24] <- (Beta_SM * Lambda + SSav * SMav) / SSav
    Elast[25] <- (Beta_SS * Lambda + SSav^2 - SSav) / SSav

    eLambda <- (summary(system_eqn)[[10]][[1]])$coefficients[8,2]

    eKK <- (summary(system_eqn)[[10]][[1]])$coefficients[2,2]
    dBeta_KK <- eKK * Lambda + eLambda * (summary(system_eqn)[[10]][[1]])$coefficients[2]
    eKL <- (summary(system_eqn)[[10]][[1]])$coefficients[3,2]
    dBeta_KL <- eKL * Lambda + eLambda * (summary(system_eqn)[[10]][[1]])$coefficients[3]
    eKE <- (summary(system_eqn)[[10]][[1]])$coefficients[4,2]
    dBeta_KE <- eKE * Lambda + eLambda * (summary(system_eqn)[[10]][[1]])$coefficients[4]
    eKM <- (summary(system_eqn)[[10]][[1]])$coefficients[5,2]
    dBeta_KM <- eKM * Lambda + eLambda * (summary(system_eqn)[[10]][[1]])$coefficients[5]
    dBeta_KS <- sqrt(dBeta_KK^2+dBeta_KL^2+dBeta_KE^2+dBeta_KM^2)

    dBeta_LK <- dBeta_KL
    eLL <- (summary(system_eqn)[[10]][[2]])$coefficients[3,2]
    dBeta_LL <- eLL * Lambda + eLambda * (summary(system_eqn)[[10]][[2]])$coefficients[3]
    eLE <- (summary(system_eqn)[[10]][[2]])$coefficients[4,2]
    dBeta_LE <- eLE * Lambda + eLambda * (summary(system_eqn)[[10]][[2]])$coefficients[4]
    eLM <- (summary(system_eqn)[[10]][[2]])$coefficients[5,2]
    dBeta_LM <- eLM * Lambda + eLambda * (summary(system_eqn)[[10]][[2]])$coefficients[5]
    dBeta_LS <- sqrt(dBeta_LK^2+dBeta_LL^2+dBeta_LE^2+dBeta_LM^2)

    dBeta_EK <- dBeta_KE
    dBeta_EL <- dBeta_LE
    eEE <- (summary(system_eqn)[[10]][[3]])$coefficients[4,2]
    dBeta_EE <- eEE * Lambda + eLambda * (summary(system_eqn)[[10]][[3]])$coefficients[4]
    eEM <- (summary(system_eqn)[[10]][[3]])$coefficients[5,2]
    dBeta_EM <- eEM * Lambda + eLambda * (summary(system_eqn)[[10]][[3]])$coefficients[5]
    dBeta_ES <- sqrt(dBeta_EK^2+dBeta_EL^2+dBeta_EE^2+dBeta_EM^2)

    dBeta_MK <- dBeta_KM
    dBeta_ML <- dBeta_LM
    dBeta_ME <- dBeta_EM
    eMM <- (summary(system_eqn)[[10]][[4]])$coefficients[5,2]
    dBeta_MM <- eMM * Lambda + eLambda * (summary(system_eqn)[[10]][[4]])$coefficients[5]
    dBeta_MS <- sqrt(dBeta_MK^2+dBeta_ML^2+dBeta_ME^2+dBeta_MM^2)

    dBeta_SK <- dBeta_KS
    dBeta_SL <- dBeta_LS
    dBeta_SE <- dBeta_ES
    dBeta_SM <- dBeta_MS
    dBeta_SS <- sqrt(dBeta_SK^2+dBeta_SL^2+dBeta_SE^2+dBeta_SM^2)

    corKK <- cor(data$SK,data$SK)
    corKL <- cor(data$SK,data$SL)
    corKE <- cor(data$SK,data$SE)
    corKM <- cor(data$SK,data$SM)
    corKS <- cor(data$SK,data$SS)

    corLK <- cor(data$SL,data$SK)
    corLL <- cor(data$SL,data$SL)
    corLE <- cor(data$SL,data$SE)
    corLM <- cor(data$SL,data$SM)
    corLS <- cor(data$SL,data$SS)

    corEK <- cor(data$SE,data$SK)
    corEL <- cor(data$SE,data$SL)
    corEE <- cor(data$SE,data$SE)
    corEM <- cor(data$SE,data$SM)
    corES <- cor(data$SE,data$SS)

    corMK <- cor(data$SM,data$SK)
    corML <- cor(data$SM,data$SL)
    corME <- cor(data$SM,data$SE)
    corMM <- cor(data$SM,data$SM)
    corMS <- cor(data$SM,data$SS)

    corSK <- cor(data$SS,data$SK)
    corSL <- cor(data$SS,data$SL)
    corSE <- cor(data$SS,data$SE)
    corSM <- cor(data$SS,data$SM)
    corSS <- cor(data$SS,data$SS)

    vKK <- (SKav^2*SKsd^2 + SKav^2*SKsd^2 + 2*SKav*SKav*SKsd*SKsd*corKK + (1+corKK)*SKsd^2*SKsd^2)/N
    vKL <- (SKav^2*SLsd^2 + SLav^2*SKsd^2 + 2*SKav*SLav*SKsd*SLsd*corKL + (1+corKL)*SKsd^2*SLsd^2)/N
    vKE <- (SKav^2*SEsd^2 + SEav^2*SKsd^2 + 2*SKav*SEav*SKsd*SEsd*corKE + (1+corKE)*SKsd^2*SEsd^2)/N
    vKM <- (SKav^2*SMsd^2 + SMav^2*SKsd^2 + 2*SKav*SMav*SKsd*SMsd*corKM + (1+corKM)*SKsd^2*SMsd^2)/N
    vKS <- (SKav^2*SSsd^2 + SSav^2*SKsd^2 + 2*SKav*SSav*SKsd*SSsd*corKS + (1+corKS)*SKsd^2*SSsd^2)/N

    vLK <- (SLav^2*SKsd^2 + SKav^2*SLsd^2 + 2*SLav*SKav*SLsd*SKsd*corLK + (1+corLK)*SLsd^2*SKsd^2)/N
    vLL <- (SLav^2*SLsd^2 + SLav^2*SLsd^2 + 2*SLav*SLav*SLsd*SLsd*corLL + (1+corLL)*SLsd^2*SLsd^2)/N
    vLE <- (SLav^2*SEsd^2 + SEav^2*SLsd^2 + 2*SLav*SEav*SLsd*SEsd*corLE + (1+corLE)*SLsd^2*SEsd^2)/N
    vLM <- (SLav^2*SMsd^2 + SMav^2*SLsd^2 + 2*SLav*SMav*SLsd*SMsd*corLM + (1+corLM)*SLsd^2*SMsd^2)/N
    vLS <- (SLav^2*SSsd^2 + SSav^2*SLsd^2 + 2*SLav*SSav*SLsd*SSsd*corLS + (1+corLS)*SLsd^2*SSsd^2)/N

    vEK <- (SEav^2*SKsd^2 + SKav^2*SEsd^2 + 2*SEav*SKav*SEsd*SKsd*corEK + (1+corEK)*SEsd^2*SKsd^2)/N
    vEL <- (SEav^2*SLsd^2 + SLav^2*SEsd^2 + 2*SEav*SLav*SEsd*SLsd*corEL + (1+corEL)*SEsd^2*SLsd^2)/N
    vEE <- (SEav^2*SEsd^2 + SEav^2*SEsd^2 + 2*SEav*SEav*SEsd*SEsd*corEE + (1+corEE)*SEsd^2*SEsd^2)/N
    vEM <- (SEav^2*SMsd^2 + SMav^2*SEsd^2 + 2*SEav*SMav*SEsd*SMsd*corEM + (1+corEM)*SEsd^2*SMsd^2)/N
    vES <- (SEav^2*SSsd^2 + SSav^2*SEsd^2 + 2*SEav*SSav*SEsd*SSsd*corES + (1+corES)*SEsd^2*SSsd^2)/N

    vMK <- (SMav^2*SKsd^2 + SKav^2*SMsd^2 + 2*SMav*SKav*SMsd*SKsd*corMK + (1+corMK)*SMsd^2*SKsd^2)/N
    vML <- (SMav^2*SLsd^2 + SLav^2*SMsd^2 + 2*SMav*SLav*SMsd*SLsd*corML + (1+corML)*SMsd^2*SLsd^2)/N
    vME <- (SMav^2*SEsd^2 + SEav^2*SMsd^2 + 2*SMav*SEav*SMsd*SEsd*corME + (1+corME)*SMsd^2*SEsd^2)/N
    vMM <- (SMav^2*SMsd^2 + SMav^2*SMsd^2 + 2*SMav*SMav*SMsd*SMsd*corMM + (1+corMM)*SMsd^2*SMsd^2)/N
    vMS <- (SMav^2*SSsd^2 + SSav^2*SMsd^2 + 2*SMav*SSav*SMsd*SSsd*corMS + (1+corMS)*SMsd^2*SSsd^2)/N

    vSK <- (SSav^2*SKsd^2 + SKav^2*SSsd^2 + 2*SSav*SKav*SSsd*SKsd*corSK + (1+corSK)*SSsd^2*SKsd^2)/N
    vSL <- (SSav^2*SLsd^2 + SLav^2*SSsd^2 + 2*SSav*SLav*SSsd*SLsd*corSL + (1+corSL)*SSsd^2*SLsd^2)/N
    vSE <- (SSav^2*SEsd^2 + SEav^2*SSsd^2 + 2*SSav*SEav*SSsd*SEsd*corSE + (1+corSE)*SSsd^2*SEsd^2)/N
    vSM <- (SSav^2*SMsd^2 + SMav^2*SSsd^2 + 2*SSav*SMav*SSsd*SMsd*corSM + (1+corSM)*SSsd^2*SMsd^2)/N
    vSS <- (SSav^2*SSsd^2 + SSav^2*SSsd^2 + 2*SSav*SSav*SSsd*SSsd*corSS + (1+corSS)*SSsd^2*SSsd^2)/N

    dElast[1] <- sqrt(Elast[1]^2*SKsd^2/N + dBeta_KK^2 + vKK)/SKav/sqrt(N) #KK
    dElast[2] <- sqrt(Elast[2]^2*SKsd^2/N + dBeta_KL^2 + vKL)/SKav/sqrt(N) #KL
    dElast[3] <- sqrt(Elast[3]^2*SKsd^2/N + dBeta_KE^2 + vKE)/SKav/sqrt(N) #KE
    dElast[4] <- sqrt(Elast[4]^2*SKsd^2/N + dBeta_KM^2 + vKM)/SKav/sqrt(N) #KM
    dElast[5] <- sqrt(Elast[5]^2*SKsd^2/N + dBeta_KS^2 + vKS)/SKav/sqrt(N) #KS

    dElast[6] <- sqrt(Elast[6]^2*SLsd^2/N + dBeta_LK^2 + vLK)/SLav/sqrt(N) #LK
    dElast[7] <- sqrt(Elast[7]^2*SLsd^2/N + dBeta_LL^2 + vLL)/SLav/sqrt(N) #LL
    dElast[8] <- sqrt(Elast[8]^2*SLsd^2/N + dBeta_LE^2 + vLE)/SLav/sqrt(N) #LE
    dElast[9] <- sqrt(Elast[9]^2*SLsd^2/N + dBeta_LM^2 + vLM)/SLav/sqrt(N) #LM
    dElast[10] <- sqrt(Elast[10]^2*SLsd^2/N + dBeta_LS^2 + vLS)/SLav/sqrt(N) #LS

    dElast[11] <- sqrt(Elast[11]^2*SEsd^2/N + dBeta_EK^2 + vEK)/SEav/sqrt(N) #EK
    dElast[12] <- sqrt(Elast[12]^2*SEsd^2/N + dBeta_EL^2 + vEL)/SEav/sqrt(N) #EL
    dElast[13] <- sqrt(Elast[13]^2*SEsd^2/N + dBeta_EE^2 + vEE)/SEav/sqrt(N) #EE
    dElast[14] <- sqrt(Elast[14]^2*SEsd^2/N + dBeta_EM^2 + vEM)/SEav/sqrt(N) #EM
    dElast[15] <- sqrt(Elast[15]^2*SEsd^2/N + dBeta_ES^2 + vES)/SEav/sqrt(N) #ES

    dElast[16] <- sqrt(Elast[16]^2*SMsd^2/N + dBeta_MK^2 + vMK)/SMav/sqrt(N) #MK
    dElast[17] <- sqrt(Elast[17]^2*SMsd^2/N + dBeta_ML^2 + vML)/SMav/sqrt(N) #ML
    dElast[18] <- sqrt(Elast[18]^2*SMsd^2/N + dBeta_ME^2 + vME)/SMav/sqrt(N) #ME
    dElast[19] <- sqrt(Elast[19]^2*SMsd^2/N + dBeta_MM^2 + vMM)/SMav/sqrt(N) #MM
    dElast[20] <- sqrt(Elast[20]^2*SMsd^2/N + dBeta_MS^2 + vMS)/SMav/sqrt(N) #MS

    dElast[21] <- sqrt(Elast[21]^2*SSsd^2/N + dBeta_SK^2 + vSK)/SSav/sqrt(N) #SK
    dElast[22] <- sqrt(Elast[22]^2*SSsd^2/N + dBeta_SL^2 + vSL)/SSav/sqrt(N) #SL
    dElast[23] <- sqrt(Elast[23]^2*SSsd^2/N + dBeta_SE^2 + vSE)/SSav/sqrt(N) #SE
    dElast[24] <- sqrt(Elast[24]^2*SSsd^2/N + dBeta_SM^2 + vSM)/SSav/sqrt(N) #SM
    dElast[25] <- sqrt(Elast[25]^2*SSsd^2/N + dBeta_SS^2 + vSS)/SSav/sqrt(N) #SS

    for (j in 1:25){
      Elast_4D[i,j+1] <- Elast[j]
      dElast_4D[i,j+1] <- dElast[j]
    }
  }
}

write.csv(R2_4D, "R2_4D.csv")
write.csv(Elast_4D, "Elast_4D.csv")
write.csv(dElast_4D, "dElast_4D.csv")

write.csv(Coef_4D, "Coef_4D.csv")
write.csv(Err_4D, "Err_4D.csv")
write.csv(pval_4D, "pval_4D.csv")

rm(list = ls()[grep("^Data4.", ls())])
rm(list = ls()[grep("^Agg4.", ls())])
rm(dlist4)
