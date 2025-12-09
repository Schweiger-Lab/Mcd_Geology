### 7: PLSDA part2 #
### Anna Schweiger, adapted by Zane Cleghorn and Ceili DeMarais #########


# Setup 
library(caret)
library(reshape)
library(corrplot)
library(agricolae)
library(ggplot2)

# Input/Output paths
input_fp <- file.path("R_output", "RandUnitCoord_200.csv")

out_finmods   <- file.path("R_output", "PLSDA_finmods.rds")
out_scores    <- file.path("R_output", "PLSDA_Scores.csv")
out_probmean  <- file.path("R_output", "PLSDA_probmean.csv")
out_probplot  <- file.path("R_output", "probability_plot.pdf")
out_confumean <- file.path("R_output", "PLSDA_confumean.csv")
out_confuperc <- file.path("R_output", "PLSDA_confuperc.csv")
out_corrplot  <- file.path("R_output", "PLSDA_corrplot.pdf")
out_loadings  <- file.path("R_output", "PLSDA_loadings.pdf")
out_summary   <- file.path("R_output", "PLSDA_summary.csv")

# Read in Data
dati <- read.csv(input_fp, check.names = FALSE)

## Preperation ##
nsims <- 10 # iterations, as it increase time increases but better average
classi <- as.factor(dati$Code)# define classes
wvl <- colnames(dati[,-1]) # define wvl range for spectral matrix, check your column names
spec <- dati[, wvl]
nwaves <- 1
spec <- dati[,which(colnames(dati)%in%as.character(wvl))] ### make spec matrix
nwaves <- 1 # measure at every nwave


## Final model ##
compi <- 13 ### select number of components based on Kappa plot / Tukey test from pt 1
finmods <- list() #

for (nsim in 1:nsims){
  print(nsim)
  flush.console()
  set.seed(nsim)
  #inTrain <- rndid[[nsim]]<= ??? # number train per code
  inTrain <- createDataPartition(y =classi, p = 0.7, list = FALSE)
  training <- spec[inTrain,seq(1,length(wvl), by=nwaves)]
  testing <- spec[-inTrain,seq(1,length(wvl), by=nwaves)]
  trainclass <- as.factor(classi[inTrain]); testclass <- as.factor(classi[!(inTrain)])
  
  finalModel <- plsda(training,trainclass, ncomp=compi, probMethod = "Bayes", method = "simpls")
  finmods[[nsim]] <- finalModel
}

saveRDS(finmods, out_finmods)


# Scores
scores_list <- lapply(seq_along(finmods), function(i) {
  sc <- as.data.frame(finmods[[i]]$scores)
  sc$Code <- as.character(trainclass)
  sc$Sim <- i
  sc
})
scores_df <- do.call(rbind, scores_list)
write.csv(scores_df, out_scores, row.names = FALSE)

## Probabilities and confusion matrix ##
probis <- list()
confus <- list()

for (nsim in seq(nsims)){
  print(nsim)
  flush.console()
  set.seed(nsim)
  #inTrain <- rndid[[nsim]]<= ???
  inTrain <- createDataPartition(y = classi, p = 0.7, list = FALSE)
  testing <- spec[-inTrain,seq(1,length(wvl), by=nwaves)]
  testclass <- as.factor(classi[-inTrain])
  
  plsProbs <- predict(finmods[[nsim]], newdata = testing, type = "prob")
  plsClasses <- predict(finmods[[nsim]], newdata = testing)
  confus[[nsim]] <- confusionMatrix(data = plsClasses, testclass)
  
  probs <- as.data.frame(plsProbs)
  names(probs) <- sapply(strsplit(names(probs),split = ".n"),"[",1)
  probs <- cbind(testclass, probs)
  probis[[nsim]] <- probs 
}

## Probability mean ##

arr <- array(unlist(probis), dim = c(dim(probis[[1]]), nsims))
prob_mean <- apply(arr, 1:2, mean)
prob_mean <- as.data.frame(prob_mean)
prob_mean$testclass <- probis[[1]]$testclass
colnames(prob_mean) <- colnames(probis[[1]])
write.csv(prob_mean, out_probmean, row.names = FALSE)

## Probability Plot ##
pp <- melt(prob_mean, id = "testclass")
pp$position <- ifelse(pp$testclass == pp$variable, 2, 1)
pp$testclass <- factor(pp$testclass, levels = rev(levels(pp$testclass)))

pdf(out_probplot, width = 6, height = 4)
ggplot(pp, aes(x = testclass, y = value, fill = variable, group = position)) +
  geom_bar(stat = "identity", position = "fill") +
  theme(legend.title = element_blank()) +
  labs(x = "Code", y = "") +
  coord_flip()
dev.off()

## Confusion matrix! ##
tabs <- lapply(confus, function(x) x$table)
tabsi <- Reduce('+', tabs)
tab_mean <- as.data.frame.matrix(tabsi / length(confus))
write.csv(tab_mean, out_confumean, row.names = TRUE)

sums <- colSums(tab_mean)
tabs_perc <- sweep(tab_mean, 2, sums, FUN = "/")
write.csv(tabs_perc, out_confuperc, row.names = TRUE)

# Confu plot
pdf(out_corrplot, width = 7, height = 6, pointsize = 9)
corrplot(as.matrix(tabs_perc), addCoef.col = 1, tl.srt = 70,
         col = colorRampPalette(c("black","brown","gold","forestgreen"))(20),
         tl.col = 1, tl.offset = 1.5, cl.ratio = 0.2,
         cl.align.text = "l", cl.cex = 0.9, mar = c(1,3,3,3))
mtext("Prediction", 2, line = 2.5, cex = 1.2)
mtext("Reference", at = 2, line = 1, cex = 1.2)
dev.off()

## Importance of bands ##
lls <- lapply(finmods, function(m) abs(loadings(m)[, 1:compi]))
sumis <- lapply(lls, rowSums)
mm <- apply(simplify2array(sumis), 1, mean)
ss <- apply(simplify2array(sumis), 1, sd)
mm_df <- data.frame(mean = mm, sd = ss)

pdf(out_loadings, width = 6, height = 4)
plot(mm_df$mean, type = "l", bty = "l", ylab = "abs(loadings)", xlab = "Bands")
polygon(x = c(1:length(mm), length(mm):1),
        y = c(mm_df$mean - mm_df$sd, rev(mm_df$mean + mm_df$sd)),
        col = "grey", border = "grey")
lines(mm_df$mean, type = "l")
dev.off()

## Model statistics ##

accu   <- sapply(confus, function(x) sum(diag(x$table)) / sum(x$table))
kappas <- sapply(confus, function(x) x$overall["Kappa"])
sensi  <- sapply(confus, function(x) mean(x$byClass[, "Sensitivity"], na.rm = TRUE))
specifi<- sapply(confus, function(x) mean(x$byClass[, "Specificity"], na.rm = TRUE))
preci  <- sapply(confus, function(x) mean(x$byClass[, "Precision"], na.rm = TRUE))

ssum <- data.frame(
  Stat = c("Accuracy","Kappa","Sensitivity","Specificity","Precision"),
  Mean = c(mean(accu), mean(kappas), mean(sensi), mean(specifi), mean(preci)),
  SD   = c(sd(accu), sd(kappas), sd(sensi), sd(specifi), sd(preci))
)

write.csv(ssum, out_summary, row.names = FALSE)

