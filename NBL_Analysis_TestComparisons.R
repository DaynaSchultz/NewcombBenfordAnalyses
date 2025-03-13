setwd("C:/Users/Desktop/BenfordAnalyses")

library(BenfordTests)
library(gtrendsR)
library(reshape2)
library(dplyr)
library(benford)

#### -----------------------==-- Benford Analysis Package --------------####

#-------------PROBE data -----------------
PROBEdata <- read.csv("PROBE.csv", sep = ',')
View(PROBEdata)

data <- cbind(PROBEdata[,3:22])

data <- (data.frame(newcol = c(t(data)), stringsAsFactors=FALSE))
data1 <- as.numeric(data$newcol)
data1 <- na.omit(data1)
data1 <- (data$newcol * 100)


#### -------------------------- BenfordTests-Package --------------####

#first apply the first digit function to each element of a given vector
#chose first two digits as standard

qbenf <- qbenf(digits = 1)

sigdig <- signifd(data1, digits = 2)

#https://cran.r-project.org/web/packages/BenfordTests/BenfordTests.pdf
##### PROBE #####
#############All
#Chi
BTchi <- chisq.benftest(data1, pvalmethod = "asymptotic")

signifd.analysis(data1, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

#Euclidean
ED.all <- edist.benftest(data1)

#Hotelling T-square
HT.all <- jointdigit.benftest(data1, eigenvalues = "all", tol = 1e-15)

#Joenssen's JP-Square
JP.all <- jpsq.benftest(data1)

#KS
KS.all <- ks.benftest(data1)

#Chebyshev Distance Test
CD.all <- mdist.benftest(data1)

#Judge-Schechter Mean Deviation
JS.all <- meandigit.benftest(data1, pvalmethod = "asymptotic")

#Freedman-Watson U-Square
FWU.all <- usq.benftest(data1)


#### All elements using JS test
PROBEdata <- read.csv("PROBE.csv", sep = ',')
View(PROBEdata)

data <- cbind(PROBEdata[,3:22])

data <- (data.frame(newcol = c(t(data)), stringsAsFactors=FALSE))
data1 <- as.numeric(data$newcol)
data1 <- na.omit(data1)

#Judge-Schechter Mean Deviation
JS.probea.all <- meandigit.benftest(data1, pvalmethod = "asymptotic")


signifd.analysis(data1, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

title(main = "PROBE Adult All")

##############Essential
data.ess <- cbind(PROBEdata[,c(6,10,11,12)])
data.ess <- data.frame(newcol = c(t(data.ess)), stringsAsFactors=FALSE)
data.ess <- as.numeric(data.ess$newcol)
data.ess <- na.omit(data.ess)

#Judge-Schechter Mean Deviation
JS.ess <- meandigit.benftest(data.ess, pvalmethod = "asymptotic")


signifd.analysis(data.ess, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

##############Non essential
data.noness <- cbind(PROBEdata[,c(3:5,7:9,13:22)])
data.noness <- data.frame(newcol = c(t(data.noness)), stringsAsFactors=FALSE)
data.noness <- as.numeric(data.noness$newcol)
data.noness <- na.omit(data.noness)

#Judge-Schechter Mean Deviation
JS.noness <- meandigit.benftest(data.noness, pvalmethod = "asymptotic")


signifd.analysis(data.noness, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))


#### PROBE adult####
##############Essential
probea.ess <- cbind(PROBEdata[,c(6,10,11)])

probea.ess <- (data.frame(newcol = c(t(probea.ess)), stringsAsFactors=FALSE))
probea.ess <- as.numeric(probea.ess$newcol)
probea.ess <- na.omit(probea.ess)

#Judge-Schechter Mean Deviation
JS.probea.ess <- meandigit.benftest(probea.ess, pvalmethod = "asymptotic")


signifd.analysis(probea.ess, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

title(main = "PROBE Adult Essential")

############## Essential
probea.potess <- cbind(PROBEdata[,c(3,7,12,18,21)])

probea.potess <- (data.frame(newcol = c(t(probea.potess)), stringsAsFactors=FALSE))
probea.potess <- as.numeric(probea.potess$newcol)
probea.potess <- na.omit(probea.potess)

#Judge-Schechter Mean Deviation
JS.probea.potess <- meandigit.benftest(probea.potess, pvalmethod = "asymptotic")


signifd.analysis(probea.potess, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))

title(main = "PROBE Adults Potentially Essential")

##############Non essential
probea.noness <- cbind(PROBEdata[,c(4,5,8,9,13:17,19,20,22)])

probea.noness <- (data.frame(newcol = c(t(probea.noness)), stringsAsFactors=FALSE))
probea.noness <- as.numeric(probea.noness$newcol)
probea.noness <- na.omit(probea.noness)

#Judge-Schechter Mean Deviation
JS.probea.noness <- meandigit.benftest(probea.noness, pvalmethod = "asymptotic")


signifd.analysis(probea.noness, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main="PROBE Adult Non Essential")


##### PROBE teenagers ######
PROBETeendata <- read.csv("PROBE_teen.csv", sep = ',')

PROBEtteen <- cbind(PROBETeendata[,2:20])


### All
PROBEtteen1 <- (data.frame(newcol = c(t(PROBEtteen)), stringsAsFactors=FALSE))
PROBEtteen1 <- na.omit(PROBEtteen1)
PROBEtteen1 <- as.numeric(PROBEtteen1$newcol)


#Judge-Schechter Mean Deviation
JS.probet.all <- meandigit.benftest(PROBEtteen1, pvalmethod = "asymptotic")

signifd.analysis(PROBEtteen1, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "PROBE Teen All")

##############Essential
PROBEteen.ess <- cbind(PROBETeendata[,c(4,8,9)])

PROBEteen.ess <- (data.frame(newcol = c(t(PROBEteen.ess)), stringsAsFactors=FALSE))
PROBEteen.ess <- na.omit(PROBEteen.ess)
PROBEteen.ess <- as.numeric(PROBEteen.ess$newcol)

#Judge-Schechter Mean Deviation
JS.probeT.ess <- meandigit.benftest(PROBEteen.ess, pvalmethod = "asymptotic")


signifd.analysis(PROBEteen.ess, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "PROBE Teen Essential")

##############Non essential
PROBEteen.noness <- cbind(PROBETeendata[,c(2,3,5,6,7,10:20)])

PROBEteen.noness <- (data.frame(newcol = c(t(PROBEteen.noness)), stringsAsFactors=FALSE))
PROBEteen.noness <- na.omit(PROBEteen.noness)
PROBEteen.noness <- as.numeric(PROBEteen.noness$newcol)

#Judge-Schechter Mean Deviation
JS.noness <- meandigit.benftest(PROBEteen.noness, pvalmethod = "asymptotic")


signifd.analysis(PROBEteen.noness, graphical_analysis = TRUE, freq = FALSE,
                 alphas = 20, tick_col = "red", ci_col = "lightblue",
                 ci_lines = c(0.05))
title(main = "PROBE Teen Non Essential")
