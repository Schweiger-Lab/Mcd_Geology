### 6. PLSDA part 1: Determining kappas #
### Anna Schweiger, adapted by Zane Cleghorn and Ceili DeMarais #########

# Setup 
library(caret)
library(reshape)
library(corrplot)
library(agricolae)

# Input/Output paths
input_fp  <- file.path("R_output", "RandUnitCoord_200.csv")
pdf_fp    <- file.path("R_output", "PLSDA_kappas.pdf")
csv_fp    <- file.path("R_output", "PLSDA_tukey.csv")

# Read data 
dati <- read.csv(input_fp, check.names = FALSE)

## Preparation ##
nsims <- 10 # iterations, as it increase time increases but better average
classi <- as.factor(dati$Code) # define classes

wvl <- colnames(dati[,-1]) # define wvl range for spectral matrix, check your column names
spec <- dati[,which(colnames(dati)%in%as.character(wvl))] ### make spec matrix

set.seed(1840) ### create random number for obs in each Code/class 
rndid <- with(dati, ave(1:nrow(dati), Code, FUN=function(x) {sample.int(length(x))}))

## Partition  ##
set.seed(1080) # for consistency
inTrain <- createDataPartition(y =classi, p = .7, list = FALSE) # e.g. 70% spectra per class
training <- spec[inTrain,seq(1,ncol(spec), by=20)] # resampling optional
testing <- spec[-inTrain,seq(1,ncol(spec), by=20)] # pick all that was not picked in inTrain
trainclass <- classi[inTrain] # define units for training
testclass <- classi[-inTrain] # define units for testing

k = 15 # tune, max number of kappas tested

#table(testclass) # verify
#table(trainclass)
#table(dati$Code)

## Finding components  ## 

mods <- list()
for (nsim in seq(nsims)){
  #inTrain <- rndid[[nsim]]<= ??? # e.g. 35 samples per Code for training #30% total train
  inTrain <- createDataPartition(y =classi, p = 0.7, list = FALSE) ## or create partitioning using fractions, e.g. 70:30
  print(nsim)
  flush.console()
  set.seed(nsim)
  traini <- spec[inTrain,seq(1,length(wvl), by=10)] 
  testi <- spec[-inTrain,seq(1,length(wvl), by=10)]
  trainclass <- classi[inTrain];testclass <- classi[!(inTrain)]
  plsFit <- train(traini, trainclass, method = "simpls", tuneLength = k,
                  probMethod="Bayes", trControl = trainControl(method="boot")) 
  mods[[nsim]] <- plsFit
}

#head(mods[[2]]$results) # verify

ncomps <- vector(length = nsims) # create vector
for (i in 1:nsims){ 
  ncomps[i]<-mods[[i]]$finalModel$ncomp # determining number of components in each model
}
#table(ncomps) # verify

## Kappa statistic ##

kappas <- data.frame(ncomps= 1:k,matrix(NA, nrow = k, ncol = length(mods))) # creating kappa matrix
for (i in 1:length(mods)){
  kappas[,i+1] <- mods[[i]]$results$Kappa # defines the coefficenets
}

## Tukey test ##

kapp <- as.data.frame(as.numeric(t(kappas[,-1]))) # combines all kappas
kapp <- cbind(kapp, rep(1:k, each=length(mods))) # labels kappas per model
names(kapp) <- c("Kappa", "ncomps") # nicer labeling

kapp$ncomps <- as.factor(kapp$ncomps) # change to factor

modi <- lm(Kappa~ncomps, kapp) # combine and reformat data
tuk <- HSD.test(modi,"ncomps") # perform tukey test

tuk_dat <- as.data.frame(tuk$groups) # convert tukey test into dataframe
tuk_dat$var <- as.numeric(row.names(tuk_dat)) # add the component column
tuk_dat <- tuk_dat[order(tuk_dat$var,decreasing = F),] # reorder from small to large
letters <- as.character(tuk_dat$groups) # constrain order of latter for graph

## Kappa Tukey plot ## 

pdf(pdf_fp, width = 5, height = 4)
par(bty = "l")
boxplot(kapp$Kappa ~ kapp$ncomps, ylim = c(0,1),
        xlab = "Number of components", ylab = "Kappa")
text(x = 1:k, y = rep(1, k), letters)
dev.off()

# Save data
write.csv(tuk_dat, file = csv_fp, row.names = FALSE)
