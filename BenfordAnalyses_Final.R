setwd("C:/Users/Desktop/BenfordAnalyses/Data")

library(BenfordTests)
library(gtrendsR)
library(reshape2)
library(dplyr)
library(benford)

#### -------------------------- BenfordTests-Package --------------####
#This is BenfordTests version 1.2.0.
#Academic users, please be sure to use: citation("BenfordTests").
#Feel free to contact the Maintainer about liscensing, feature requests, etc.
#Use suppressPackageStartupMessages to eliminate package startup messages.


#https://cran.r-project.org/web/packages/BenfordTests/BenfordTests.pdf
##### PROBE Adults #####
#############All
PROBEdata <- read.csv("Sample_Data.csv", sep = ',', head = T, fileEncoding = "UTF-8")

#----BEWARE----#
# These cutoffs are the upper and lower limits of detection of the instrument. Because the sample data has been artifically generated, these will not be relevant and should be updated for 
As.pr1 <- PROBEdata$As %>% 
  as.data.frame() %>% 
  filter(PROBEdata$As > 0.27) %>% 
  as.matrix()

Be.pr1 <- PROBEdata$Be %>%
  as.data.frame() %>% 
  filter(PROBEdata$Be > 0.045) %>% 
  as.matrix()

Cd.pr1 <- PROBEdata$Cd %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Cd > 0.1) %>% 
  as.matrix()

Co.pr1 <- PROBEdata$Co %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Co > 0.01) %>% 
  as.matrix()

Cr.pr1 <- PROBEdata$Cr %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Cr > 0.04) %>% 
  as.matrix()

Hg.pr1 <- PROBEdata$Hg %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Hg > 0.29) %>% 
  as.matrix()

Ir.pr1 <- PROBEdata$Ir. %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Ir. > 5) %>% 
  as.matrix()

Mn.pr1 <- PROBEdata$Mn %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Mn > 0.78) %>% 
  as.matrix()

Mo.pr1 <- PROBEdata$Mo %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Mo > 0.31) %>% 
  as.matrix()

Ni.pr1 <- PROBEdata$Ni %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Ni > 0.35) %>% 
  as.matrix()

Pb.pr1 <- PROBEdata$Pb %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Pb > 1.03) %>% 
  as.matrix()

Pd.pr1 <- PROBEdata$Pd. %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Pd. > 15) %>% 
  as.matrix()

Pt.pr1 <- PROBEdata$Pt. %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Pt > 5) %>% 
  as.matrix()

Rh.pr1 <- PROBEdata$Rh. %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Rh. > 15) %>% 
  as.matrix()

Sb.pr1 <- PROBEdata$Sb %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Sb > .15) %>% 
  as.matrix()

Sn.pr1 <- PROBEdata$Sn %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Sn > .095) %>% 
  as.matrix()

Tl.pr1 <- PROBEdata$Tl %>% 
  as.data.frame() %>% 
  filter(PROBEdata$Tl > .015) %>% 
  as.matrix()

U.pr1 <- PROBEdata$U. %>% 
  as.data.frame() %>% 
  filter(PROBEdata$U > .0015) %>% 
  as.matrix()

V.pr1 <- PROBEdata$V %>% 
  as.data.frame() %>% 
  filter(PROBEdata$V > .024) %>% 
  as.matrix()

W.pr1 <- PROBEdata$W %>% 
  as.data.frame() %>% 
  filter(PROBEdata$W > 0.005) %>% 
  as.matrix()


#### All elements using JS test

pr1.all <- (c(As.pr1,Be.pr1,Cd.pr1,Co.pr1,Cr.pr1,Hg.pr1,Ir.pr1,Mn.pr1,Mo.pr1,
                Ni.pr1,Pb.pr1,Pd.pr1,Pt.pr1,Rh.pr1,Sb.pr1,Sn.pr1,Tl.pr1,U.pr1,
                V.pr1,W.pr1))

pr1.all <- (data.frame(newcol = c(t(pr1.all)), stringsAsFactors=FALSE))
pr1.all <- as.numeric(pr1.all$newcol)

#Judge-Schechter Mean Deviation
JS.pr1.all <- meandigit.benftest(pr1.all, pvalmethod = "asymptotic")


signifd.analysis(pr1.all, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

title(main = "Sample_Data Adult All")

summary(pr1.all)

##############Essential
pr1.ess <- (c(Co.pr1,Mo.pr1))
pr1.ess <- (data.frame(newcol = c(t(pr1.ess)), stringsAsFactors=FALSE))
pr1.ess <- as.numeric(pr1.ess$newcol)

#Judge-Schechter Mean Deviation
JS.pr1.ess <- meandigit.benftest(pr1.ess, pvalmethod = "asymptotic")

data <- signifd.analysis(pr1.ess, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "Sample_Data Adult Essential")

#Trial for graphs
freq <- as.numeric(data$summary[1, ])
pvals <- as.numeric(data$summary[2, ])
CI_lower <- as.numeric(data$CIs[1, ])
CI_median <- as.numeric(data$CIs[2, ])
CI_upper <- as.numeric(data$CIs[3, ])
plot(1:9, freq, type = "p", ylim = c(0, 0.6), xlab = "Digit", ylab = "Frequency",
     pch = 19, col = "red", main = "Sample_Data Adult Essential")

# Add confidence intervals
arrows(1:9, CI_lower, 1:9, CI_upper, length = 0.05, angle = 90, code = 3, col = "lightblue")

# Add confidence band (optional)
lines(1:9, CI_median, col = "black", lty = 2)  # Median CI

# Add reference line for ideal Benford's Law distribution (optional)
benford_ideal <- log10(1 + 1/(1:9))  # Expected frequencies from Benford's Law
lines(1:9, benford_ideal, col = "blue", lwd = 2)

# Add text for p-values (optional)
text(1:9, freq + 0.05, labels = round(pvals, 4), col = "darkred", cex = 0.8)

# Legend (optional)
legend("topright", legend = c("Observed", "95% CI", "Ideal Benford's Law"),
       col = c("red", "lightblue", "blue"), pch = c(19, NA, NA), lty = c(NA, 1, 1))

##############Non essential
pr1.non <- (c(As.pr1,Be.pr1,Cd.pr1,Hg.pr1,Ir.pr1,Pb.pr1,Pd.pr1,Pt.pr1,
              Rh.pr1,Sb.pr1,Tl.pr1,U.pr1,V.pr1,W.pr1))

pr1.non <- (data.frame(newcol = c(t(pr1.non)), stringsAsFactors=FALSE))
pr1.non <- as.numeric(pr1.non$newcol)

#Judge-Schechter Mean Deviation
JS.pr1.non <- meandigit.benftest(pr1.non, pvalmethod = "asymptotic")


signifd.analysis(pr1.non, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "Sample_Data Adult Non Essential")

##############Beneficial
pr1.benf <- (c(Cr.pr1,Mn.pr1,Ni.pr1,Sn.pr1))

pr1.benf <- (data.frame(newcol = c(t(pr1.benf)), stringsAsFactors=FALSE))
pr1.benf <- as.numeric(pr1.benf$newcol)

#Judge-Schechter Mean Deviation
JS.pr1.benf <- meandigit.benftest(pr1.benf, pvalmethod = "asymptotic")


signifd.analysis(pr1.benf, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "Sample_Data Adult Beneficial")

#----------------- The following code uses the original datasets and will therefore not be compatible with the sample data --------#
##### PROBE Adolescents ######
#### All elements using JS test
PROBEteendata <- read.csv("PROBE_teen.csv", sep = ',')

As.pr2 <- PROBEteendata$As %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$As > 0.27) %>% 
  as.matrix()

Cd.pr2 <- PROBEteendata$Cd %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Cd > 0.1) %>% 
  as.matrix()

Co.pr2 <- PROBEteendata$Co %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Co > 0.01) %>% 
  as.matrix()

Cr.pr2 <- PROBEteendata$Cr %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Cr > 0.04) %>% 
  as.matrix()

Hg.pr2 <- PROBEteendata$Hg %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Hg > 0.29) %>% 
  as.matrix()

Ir.pr2 <- PROBEteendata$Ir %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Ir > 5) %>% 
  as.matrix()

Mn.pr2 <- PROBEteendata$Mn %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Mn > 0.78) %>% 
  as.matrix()

Mo.pr2 <- PROBEteendata$Mo %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Mo > 0.31) %>% 
  as.matrix()

Ni.pr2 <- PROBEteendata$Ni %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Ni > 0.35) %>% 
  as.matrix()

Pb.pr2 <- PROBEteendata$Pb %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Pb > 1.03) %>% 
  as.matrix()

Pd.pr2 <- PROBEteendata$Pd %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Pd > 15) %>% 
  as.matrix()

Pt.pr2 <- PROBEteendata$Pt %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Pt > 5) %>% 
  as.matrix()

Rh.pr2 <- PROBEteendata$Rh %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Rh > 15) %>% 
  as.matrix()

Sb.pr2 <- PROBEteendata$Sb %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Sb > .15) %>% 
  as.matrix()

Sn.pr2 <- PROBEteendata$Sn %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Sn > .095) %>% 
  as.matrix()

Tl.pr2 <- PROBEteendata$Tl %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$Tl > .015) %>% 
  as.matrix()

U.pr2 <- PROBEteendata$U %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$U > .0015) %>% 
  as.matrix()

V.pr2 <- PROBEteendata$V %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$V > .024) %>% 
  as.matrix()

W.pr2 <- PROBEteendata$W %>% 
  as.data.frame() %>% 
  filter(PROBEteendata$W > 0.005) %>% 
  as.matrix()

########## All
pr2.all <- (c(As.pr2,Cd.pr2,Co.pr2,Cr.pr2,Hg.pr2,Ir.pr2,Mn.pr2,Mo.pr2,
              Ni.pr2,Pb.pr2,Pd.pr2,Pt.pr2,Rh.pr2,Sb.pr2,Sn.pr2,Tl.pr2,U.pr2,
              V.pr2,W.pr2))

pr2.all <- (data.frame(newcol = c(t(pr2.all)), stringsAsFactors=FALSE))
pr2.all <- as.numeric(pr2.all$newcol)

#Judge-Schechter Mean Deviation
JS.pr2.all <- meandigit.benftest(pr2.all, pvalmethod = "asymptotic")

signifd.analysis(pr2.all, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

title(main = "PROBE Adolescent All")



##############Essential
pr2.ess <- (c(Co.pr2,Mo.pr2))
pr2.ess <- (data.frame(newcol = c(t(pr2.ess)), stringsAsFactors=FALSE))
pr2.ess <- as.numeric(pr2.ess$newcol)

#Judge-Schechter Mean Deviation
JS.pr2.ess <- meandigit.benftest(pr2.ess, pvalmethod = "asymptotic")

signifd.analysis(pr2.ess, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "PROBE Adolescent Essential")



##############Non essential
pr2.non <- (c(As.pr2,Cd.pr2,Hg.pr2,Ir.pr2,Pb.pr2,Pd.pr2,Pt.pr2,
              Rh.pr2,Sb.pr2,Tl.pr2,U.pr2,V.pr2,W.pr2))

pr2.non <- (data.frame(newcol = c(t(pr2.non)), stringsAsFactors=FALSE))
pr2.non <- as.numeric(pr2.non$newcol)

#Judge-Schechter Mean Deviation
JS.pr2.non <- meandigit.benftest(pr2.non, pvalmethod = "asymptotic")


signifd.analysis(pr2.non, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "PROBE Adolescent Non Essential")

##############Beneficial
pr2.benf <- (c(Cr.pr2,Mn.pr2,Ni.pr2,Sn.pr2))

pr2.benf <- (data.frame(newcol = c(t(pr2.benf)), stringsAsFactors=FALSE))
pr2.benf <- as.numeric(pr2.benf$newcol)

#Judge-Schechter Mean Deviation
JS.pr2.benf <- meandigit.benftest(pr2.benf, pvalmethod = "asymptotic")


signifd.analysis(pr2.benf, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "PROBE Adolescent Beneficial")


##### Burlo ##################
burlo <- read.csv("burlo_all.csv", sep = ',')
View(burlo)

#Should split by sample type - urine and hair and blood
############ Hair #####
burlo.hair.all <- cbind(burlo[,c(11,12)])

## All
burlo.hair.all <- (data.frame(newcol = c(t(burlo.hair.all)), stringsAsFactors=FALSE))
burlo.hair.all <- na.omit(burlo.hair.all)
burlo.hair.all <- as.numeric(burlo.hair.all$newcol)

#Judge-Schechter Mean Deviation
JS.burlo.hair.all <- meandigit.benftest(burlo.hair.all, pvalmethod = "asymptotic")

signifd.analysis(burlo.hair.all, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "BURLO Hair All")

print(JS.burlo.hair.all)
## Benf
burlo.hair.benf <- (data.frame(newcol = c(t(burlo[,11])), stringsAsFactors=FALSE))
burlo.hair.benf <- na.omit(burlo.hair.benf)
burlo.hair.benf <- as.numeric(burlo.hair.benf$newcol)

#Judge-Schechter Mean Deviation
JS.burlo.hair.benf <- meandigit.benftest(burlo.hair.benf, pvalmethod = "asymptotic")

signifd.analysis(burlo.hair.benf, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "BURLO Hair Beneficial")

print(JS.burlo.hair.benf)
## Non essential
burlo.hair.noness <- (data.frame(newcol = c(t(burlo[,12])), stringsAsFactors=FALSE))
burlo.hair.noness <- na.omit(burlo.hair.noness)
burlo.hair.noness <- as.numeric(burlo.hair.noness$newcol)

#Judge-Schechter Mean Deviation
JS.burlo.hair.noness <- meandigit.benftest(burlo.hair.noness, pvalmethod = "asymptotic")

signifd.analysis(burlo.hair.noness, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "BURLO Hair Non Essential")

print(JS.burlo.hair.noness)
############ Urine #####
burlo.urine <- cbind(burlo[,c(5:10)])

As.b <- burlo.urine$As %>% 
  as.data.frame() %>% 
  filter(burlo.urine$As > 0.92) %>% 
  as.matrix()

Cd.b <- burlo.urine$Cd %>% 
  as.data.frame() %>% 
  filter(burlo.urine$Cd > 0.014) %>% 
  as.matrix()

Hg.b <- burlo.urine$Hg %>% 
  as.data.frame() %>% 
  filter(burlo.urine$Hg > 0.45) %>% 
  as.matrix()

Mn.b <- burlo.urine$Mn %>% 
  as.data.frame() %>% 
  filter(burlo.urine$Mn > 0.02) %>% 
  as.matrix()

Pb.b <- burlo.urine$Pb %>% 
  as.data.frame() %>% 
  filter(burlo.urine$Pb > 0.13) %>% 
  as.matrix()

Se.b <- burlo.urine$Se %>% 
  as.data.frame() %>% 
  as.matrix()

## All
burlo.urine.all <- c(As.b, Cd.b, Hg.b, Mn.b, Pb.b, Se.b)

burlo.U.all <- (data.frame(newcol = c(t(burlo.urine.all)), stringsAsFactors=FALSE))
burlo.U.all <- na.omit(burlo.U.all)
burlo.U.all <- as.numeric(burlo.U.all$newcol)

#Judge-Schechter Mean Deviation
JS.burlo.U.all <- meandigit.benftest(burlo.U.all, pvalmethod = "asymptotic")

signifd.analysis(burlo.U.all, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "BURLO Urine All")

## Benf
burlo.urine.benf <- c(Mn.b, Se.b)

burlo.U.benf <- (data.frame(newcol = c(t(burlo.urine.benf)), stringsAsFactors=FALSE))
burlo.U.benf <- na.omit(burlo.U.benf)
burlo.U.benf <- as.numeric(burlo.U.benf$newcol)

#Judge-Schechter Mean Deviation
JS.burlo.U.benf <- meandigit.benftest(burlo.U.benf, pvalmethod = "asymptotic")

signifd.analysis(burlo.U.benf, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "BURLO Urine Beneficial")

## Non essential
burlo.urine.noness <- c(As.b, Cd.b, Hg.b, Pb.b)
burlo.U.noness <- (data.frame(newcol = c(t(burlo.urine.noness)), stringsAsFactors=FALSE))
burlo.U.noness <- na.omit(burlo.U.noness)
burlo.U.noness <- as.numeric(burlo.U.noness$newcol)

#Judge-Schechter Mean Deviation
JS.burlo.U.noness <- meandigit.benftest(burlo.U.noness, pvalmethod = "asymptotic")

signifd.analysis(burlo.U.noness, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "BURLO Urine Non Essential")



##### UNICEF ##################
UNIdata <- read.csv("UNIMICEDATA.csv", sep = ';')


As.uni <- UNIdata$As..µg.L. %>% 
  as.data.frame() %>%
  filter(UNIdata$As..µg.L. > 0.27) %>% 
  as.matrix() %>% 
  as.numeric()

Cd.uni <- UNIdata$Cd..µg.L. %>% 
  as.data.frame() %>% 
  filter(UNIdata$Cd..µg.L. > 0.1) %>% 
  as.matrix() %>% 
  as.numeric()

Hg.uni <- UNIdata$Hg...µg.L. %>% 
  as.data.frame() %>% 
  filter(UNIdata$Hg...µg.L. > 0.29) %>% 
  as.matrix() %>% 
  as.numeric()

Mn.uni <- UNIdata$Mn..µg.L. %>% 
  as.data.frame() %>% 
  filter(UNIdata$Mn..µg.L. > 0.78) %>% 
  as.matrix() %>% 
  as.numeric()

Pb.uni <- UNIdata$Pb..µg.L. %>% 
  as.data.frame() %>% 
  filter(UNIdata$Pb..µg.L. > 1.03) %>% 
  as.matrix() %>% 
  as.numeric()

#############All
uni.all <- (c(As.uni,Cd.uni,Hg.uni,Mn.uni,Pb.uni))

uni.all <- (data.frame(newcol = c(t(uni.all)), stringsAsFactors=FALSE))
uni.all <- as.numeric(uni.all$newcol)

#Judge-Schechter Mean Deviation
JS.uni.all <- meandigit.benftest(uni.all, pvalmethod = "asymptotic")

signifd.analysis(uni.all, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "UNICEF All")

############## Beneficial
uni.benf <- (c(Mn.uni))

uni.benf <- (data.frame(newcol = c(t(uni.benf)), stringsAsFactors=FALSE))
uni.benf <- as.numeric(uni.benf$newcol)

#Judge-Schechter Mean Deviation
JS.uni.benf <- meandigit.benftest(uni.benf, pvalmethod = "asymptotic")

signifd.analysis(uni.benf, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "UNICEF Beneficial")

##############Non essential
uni.non <- (c(As.uni,Cd.uni,Hg.uni,Pb.uni))

uni.non <- (data.frame(newcol = c(t(uni.non)), stringsAsFactors=FALSE))
uni.non <- as.numeric(uni.non$newcol)

#Judge-Schechter Mean Deviation
JS.uni.non <- meandigit.benftest(uni.non, pvalmethod = "asymptotic")

signifd.analysis(uni.non, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "UNICEF Non essential")


##### MICE #######
#############All
library(readxl)
mice <- read_excel("Mice.xlsx")

data.mice <- mice %>% 
  filter(Tissue == "Blood")
data.mice <- cbind(data.mice[,c(17,18)])

data.mice <- data.mice %>% 
  filter(`55Mn Final` > 0.78) %>% 
  filter(`208Pb Final` > .0103)

data.mice <- data.frame(newcol = c(t(data.mice)), stringsAsFactors=FALSE)
data.mice[data.mice == 0] <- NA
data.mice <- na.omit(data.mice)
data.mice <- as.numeric(data.mice$newcol)


#Judge-Schechter Mean Deviation
JS.mice.all <- meandigit.benftest(data.mice, pvalmethod = "asymptotic")

signifd.analysis(data.mice, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

title(main = "MICE Blood All")

print(JS.mice.all)
############## Beneficial
data.mice.benf <- mice %>% 
  filter(Tissue == "Blood") %>% 
  select("55Mn Final") %>% 
  filter(`55Mn Final` > 0.78) 
data.mice.benf <- data.frame(newcol = c(t(data.mice.benf)), stringsAsFactors=FALSE)
data.mice.benf[data.mice.benf == 0] <- NA
data.mice.benf <- as.numeric(data.mice.benf$newcol)
data.mice.benf <- na.omit(data.mice.benf)


#Judge-Schechter Mean Deviation
JS.mice.benf <- meandigit.benftest(data.mice.benf, pvalmethod = "asymptotic")

signifd.analysis(data.mice.benf, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "MICE Blood Beneficial")


print(JS.mice.benf)
##############Non essential
data.mice.noness <- mice %>% 
  filter(Tissue == "Blood") %>% 
  select("208Pb Final") %>% 
  filter(`208Pb Final` > .0103)
data.mice.noness <- data.frame(newcol = c(t(data.mice.noness)), stringsAsFactors=FALSE)
data.mice.noness[data.mice.noness == 0] <- NA
data.mice.noness <- as.numeric(data.mice.noness$newcol)
data.mice.noness <- na.omit(data.mice.noness)

#Judge-Schechter Mean Deviation
JS.mice.noness <- meandigit.benftest(data.mice.noness, pvalmethod = "asymptotic")

signifd.analysis(data.mice.noness, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

title(main = "MICE Blood Nonessential")

print(JS.mice.noness)
##### MICE data Trial #####
mice.trial <- read.csv("Mice_Treatment.csv")

#T1 - Control
mice.1 <- mice.trial[1:74,1:5] %>% 
  filter(X55Mn.Final > 0.78) %>% 
  filter(X208Pb.Final > .0103)

#T2 - Pb
mice.2 <- mice.trial[75:138,1:5] %>% 
  filter(X55Mn.Final > 0.78) %>% 
  filter(X208Pb.Final > .0103)

#T3 - Mn
mice.3 <- mice.trial[139:212,1:5] %>% 
  filter(X55Mn.Final > 0.78) %>% 
  filter(X208Pb.Final > .0103)

#T4 - Pb + Mn
mice.4 <- mice.trial[213:368,1:5] %>% 
  filter(X55Mn.Final > 0.78) %>% 
  filter(X208Pb.Final > .0103)

########## Control ####
mice.1 <- mice.1[,4:5]

mice.1.all <- data.frame(newcol = c(t(mice.1)), stringsAsFactors=FALSE)
mice.1.all[mice.1.all == 0] <- NA
mice.1.all <- as.numeric(mice.1.all$newcol)
mice.1.all <- na.omit(mice.1.all)

JS.mice.1.all <- meandigit.benftest(mice.1.all, pvalmethod = "asymptotic")

signifd.analysis(mice.1.all, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "Mice Blood Control All")

print(JS.mice.1.all)
#---------------------------
mice.1.benf <- mice.1 %>%
  select("X55Mn.Final")

mice.1.benf <- data.frame(newcol = c(t(mice.1.benf)), stringsAsFactors=FALSE)
mice.1.benf[mice.1.benf == 0] <- NA
mice.1.benf <- as.numeric(mice.1.benf$newcol)
mice.1.benf <- na.omit(mice.1.benf)

JS.mice.1.benf <- meandigit.benftest(mice.1.benf, pvalmethod = "asymptotic")

signifd.analysis(mice.1.benf, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

title(main = "Mice Blood Control Beneficial")

print(JS.mice.1.benf)
#---------------------------
mice.1.noness <- mice.1 %>%
  select("X208Pb.Final")

mice.1.noness <- data.frame(newcol = c(t(mice.1.noness)), stringsAsFactors=FALSE)
mice.1.noness[mice.1.noness == 0] <- NA
mice.1.noness <- as.numeric(mice.1.noness$newcol)
mice.1.noness <- na.omit(mice.1.noness)

JS.mice.1.noness <- meandigit.benftest(mice.1.noness, pvalmethod = "asymptotic")

signifd.analysis(mice.1.noness, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "Mice Blood Control Non Essential")

print(JS.mice.1.noness)
########### Pb Exp ############
mice.2.all <- mice.2[,4:5]
mice.2.all <- data.frame(newcol = c(t(mice.2)), stringsAsFactors=FALSE)
mice.2.all[mice.2.all == 0] <- NA
mice.2.all <- as.numeric(mice.2.all$newcol)
mice.2.all <- na.omit(mice.2.all)

JS.mice.2.all <- meandigit.benftest(mice.2.all, pvalmethod = "asymptotic")

signifd.analysis(mice.2.all, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

title(main = "Mice Blood Pb All")

print(JS.mice.2.all)
#---------------------------
mice.2.benf <- mice.2 %>%
  select("X55Mn.Final")

mice.2.benf <- data.frame(newcol = c(t(mice.2.benf)), stringsAsFactors=FALSE)
mice.2.benf[mice.2.benf == 0] <- NA
mice.2.benf <- as.numeric(mice.2.benf$newcol)
mice.2.benf <- na.omit(mice.2.benf)

JS.mice.2.benf <- meandigit.benftest(mice.2.benf, pvalmethod = "asymptotic")

signifd.analysis(mice.2.benf, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "Mice Blood Pb Beneficial")

print(JS.mice.2.benf)
#---------------------------
mice.2.noness <- mice.2 %>%
  select("X208Pb.Final")

mice.2.noness <- data.frame(newcol = c(t(mice.2.noness)), stringsAsFactors=FALSE)
mice.2.noness[mice.2.noness == 0] <- NA
mice.2.noness <- as.numeric(mice.2.noness$newcol)
mice.2.noness <- na.omit(mice.2.noness)

JS.mice.2.noness <- meandigit.benftest(mice.2.noness, pvalmethod = "asymptotic")

signifd.analysis(mice.2.noness, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "Mice Blood Pb Non Essential")

print(JS.mice.2.noness)

########## Mn Exp ####################
mice.3.all <- mice.3[,4:5]
mice.3.all <- data.frame(newcol = c(t(mice.3.all)), stringsAsFactors=FALSE)
mice.3.all[mice.3.all == 0] <- NA
mice.3.all <- as.numeric(mice.3.all$newcol)
mice.3.all <- na.omit(mice.3.all)

JS.mice.3.all <- meandigit.benftest(mice.3.all, pvalmethod = "asymptotic")

signifd.analysis(mice.3.all, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

title(main = "Mice Blood Mn All")

print(JS.mice.3.all)
#---------------------------
mice.3.benf <- mice.3 %>%
  select("X55Mn.Final")

mice.3.benf <- data.frame(newcol = c(t(mice.3.benf)), stringsAsFactors=FALSE)
mice.3.benf[mice.3.benf == 0] <- NA
mice.3.benf <- as.numeric(mice.3.benf$newcol)
mice.3.benf <- na.omit(mice.3.benf)

JS.mice.3.benf <- meandigit.benftest(mice.3.benf, pvalmethod = "asymptotic")

signifd.analysis(mice.3.benf, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "Mice Blood Mn Beneficial")

print(JS.mice.3.benf)
#---------------------------
mice.3.noness <- mice.3 %>%
  select("X208Pb.Final")

mice.3.noness <- data.frame(newcol = c(t(mice.3.noness)), stringsAsFactors=FALSE)
mice.3.noness[mice.3.noness == 0] <- NA
mice.3.noness <- as.numeric(mice.3.noness$newcol)
mice.3.noness <- na.omit(mice.3.noness)

JS.mice.3.noness <- meandigit.benftest(mice.3.noness, pvalmethod = "asymptotic")

signifd.analysis(mice.3.noness, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "Mice Blood Mn Non Essential")

print(JS.mice.3.noness)
########## Co Exp ####################
mice.4.all <- mice.4[,4:5]
mice.4.all <- data.frame(newcol = c(t(mice.4.all)), stringsAsFactors=FALSE)
mice.4.all[mice.4.all == 0] <- NA
mice.4.all <- as.numeric(mice.4.all$newcol)
mice.4.all <- na.omit(mice.4.all)

JS.mice.4.all <- meandigit.benftest(mice.4.all, pvalmethod = "asymptotic")

signifd.analysis(mice.4.all, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

title(main = "Mice Blood Co Exp All")

print(JS.mice.4.all)
#---------------------------
mice.4.benf <- mice.4 %>%
  select("X55Mn.Final")

mice.4.benf <- data.frame(newcol = c(t(mice.4.benf)), stringsAsFactors=FALSE)
mice.4.benf[mice.4.benf == 0] <- NA
mice.4.benf <- as.numeric(mice.4.benf$newcol)
mice.4.benf <- na.omit(mice.4.benf)

JS.mice.4.benf <- meandigit.benftest(mice.4.benf, pvalmethod = "asymptotic")

signifd.analysis(mice.4.benf, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "Mice Blood Co-Exposure Benificial")

print(JS.mice.4.benf)
#---------------------------
mice.4.noness <- mice.4 %>%
  select("X208Pb.Final")

mice.4.noness <- data.frame(newcol = c(t(mice.4.noness)), stringsAsFactors=FALSE)
mice.4.noness[mice.4.noness == 0] <- NA
mice.4.noness <- as.numeric(mice.4.noness$newcol)
mice.4.noness <- na.omit(mice.4.noness)

JS.mice.4.noness <- meandigit.benftest(mice.4.noness, pvalmethod = "asymptotic")

signifd.analysis(mice.4.noness, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "Mice Blood Co Exp Non Essential")

print(JS.mice.4.noness)
# check precision
data <- c(1000, 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150, 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150, 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150
          , 1050, 1010, 1080, 1100, 1150)

result <- meandigit.benftest(data, pvalmethod = "asymptotic")

print(result)

