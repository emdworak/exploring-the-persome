# A1. Reliability analyses for the SPI data set ----
library(psych) # make this package active
library(psychTools) # make this one active as well

## Find alpha and omega for each scale ----
omA <- omega(spi[selectFromKeys(spi.keys$Agree)],plot=FALSE)
omC <- omega(spi[selectFromKeys(spi.keys$Consc)],plot=FALSE)
omN <- omega(spi[selectFromKeys(spi.keys$Neuro)],plot=FALSE)
omE <- omega(spi[selectFromKeys(spi.keys$Extra)],plot=FALSE)
omO <- omega(spi[selectFromKeys(spi.keys$Open)],plot=FALSE)

omega.h <- c(omA$omega_h,omC$omega_h,omN$omega_h,omE$omega_h,omO$omega_h)
omega.t <- c(omA$omega.tot,omC$omega.tot,omN$omega.tot,omE$omega.tot,omO$omega.tot)
alphas <- c(omA$alpha,omC$alpha ,omN$alpha,omE$alpha,omO$alpha)

omega.df <- data.frame(omgega_total = omega.t,alpha=alphas,omega_h = omega.h)
rownames(omega.df) <- cs(Agreeableness, Conscientiousness, Neuroticism,
                         Extraversion, Openness)

#find the scale scores for the Big 5 and little 27
spi.scales <- scoreItems(spi.keys,spi) #find scores as well as scale statistics
spi.scores <- data.frame(spi[1:10],spi.scales$scores) #combine demographics and scores
R5<- lowerCor(spi.scores[11:15])
basic.stats <- cbind(omega.df,R5)

# A2. Regression analyses for the SPI data set ----
library(psych) #make this package active
library(psychTools) #make this one active as well

spi.scales <- scoreItems(spi.keys,spi) #find scores as well as scale statistics
spi.scores <- data.frame(spi[1:10],spi.scales$scores) #combine demographics and scores
set.seed(42) #set the random seed to a memorable value
ss <- sample(1:4000,2000,replace=FALSE) #randomly choose 2000 subjects
spi5.R <- setCor(y=1:10,x=11:15,data=spi.scores[ss,],plot=FALSE)
spi27.R <- setCor(y=1:10,x=16:42,data=spi.scores[ss,],plot=FALSE)
spi135.R <- setCor(y=1:10,x=11:145,data=spi[ss,],plot=FALSE)

spi.scales <- scoreItems(spi.keys,spi) #find scores as well as scale statistics
sc.demos <- data.frame(spi[1:10],spi.scales$scores) #combine demographics and scores
#sc.demos <-cbind(spi[1:10],sc$scores) #combine with scores with demographics
set.seed(42) #for reproducible results
ss <- sample(1:nrow(sc.demos),nrow(sc.demos)/2)
#derivation multiple Rs
sc.5 <- setCor(y=1:10,x=11:15, data=sc.demos[ss,], plot=FALSE)
sc.27 <- setCor(y=1:10,x=16:42, data=sc.demos[ss,], plot=FALSE)
sc.135 <- setCor(y=1:10,x=11:145,data=spi[ss,] ,plot=FALSE)
#now cross validate
cv.5 <- crossValidation(sc.5,sc.demos[-ss,])


cv.27 <- crossValidation(sc.27,sc.demos[-ss,])
cv.135 <- crossValidation(sc.135,spi[-ss,])
cross.valid.df <- data.frame(cv5=cv.5$crossV, cv.27=cv.27$crossV, cv135=cv.135$crossV)
cross.valid.df.sorted <- dfOrder(cross.valid.df,1)
bs <- bestScales(spi[ss,],criteria=colnames(spi)[1:10], folds=10, n.item=20,
                 dictionary=spi.dictionary,cut=.05)
bs.cv <- crossValidation(bs,spi[-ss,])
cross.valid.df.bs <- cbind(cross.valid.df,bs=bs.cv$crossV)
cv.df.bs.sorted <- dfOrder(cross.valid.df.bs,1)
matPlot(cv.df.bs.sorted[c(2,4,6,8)],minlength=8,
        main="Cross validation of multiple regression on spi data",
        xlas=3, ylab="Cross Validated R", pch=15:19)
legend(1,.6,cs(135,27,bestS,b5),lty=c(3,2,4,1),col=c(3,2,4,1))
bs.spi.smoke <- bs$items$smoke
df2latex(bs.spi.smoke[c(2,3,5)])

# A3. Graphical displays: The Manhattan plot ----
labels <- names(spi.keys)
labels <- abbreviate(labels,minlength=8)
op <- par(mfrow=c(2,3)) #two row by three column display
man <- manhattan(spi,criteria=cs(health,exer,smoke),
                 keys=spi.keys,abs=FALSE,labels=labels)
man <- manhattan(spi,criteria=cs(health,exer,smoke),keys=spi.keys,abs=FALSE,
                 labels=labels,log.p = TRUE,main="")
op <- par(mfrow=c(1,1) ) #put it back to the normal condition

# A4. Studies 3 and 4: Predicting 19 criteria from 696 items and scales ----

## Download the SAPA data from dataverse ----
###  Set up R for downloading the data ----
# Install the dataverse.
require(devtools) # You may need to run install.packages("devtools")
library(devtools)

# You will need to install dataverse if you do not already have it installed
# install_version("dataverse", version = "0.2.0", repos = "http://cran.us.r-project.org")

# Load the appropriate packages
library(psych)
library(data.table)
library(dataverse)

### Pull the data from dataverse ----
# Load the publicly-available SAPA data

# Load SAPA data from 08 Dec 2013 to 26 Jul 2014
dataset1 <- get_dataset("doi:10.7910/DVN/SD7SVE")
writeBin(get_file("sapaTempData696items08dec2013thru26jul2014.tab", "doi:10.7910/DVN/SD7SVE"), "sapaTempData696items08dec2013thru26jul2014.tab")
sapaTempData696items08dec2013thru26jul2014 <- fread("sapaTempData696items08dec2013thru26jul2014.tab", na.strings=getOption("<NA>","NA"))
sapaTempData696items08dec2013thru26jul2014 <- as.data.frame(sapaTempData696items08dec2013thru26jul2014)

# Load SAPA data from 26 Jul 2014 to 22 Dec 2015
dataset2 <- get_dataset("doi:10.7910/DVN/GU70EV")
writeBin(get_file("sapaTempData696items26jul2014thru22dec2015.tab", "doi:10.7910/DVN/GU70EV"), "sapaTempData696items26jul2014thru22dec2015.tab")
sapaTempData696items26jul2014thru22dec2015 <- fread("sapaTempData696items26jul2014thru22dec2015.tab", na.strings=getOption("<NA>","NA"))
sapaTempData696items26jul2014thru22dec2015 <- as.data.frame(sapaTempData696items26jul2014thru22dec2015)

# Load SAPA data from 22 Dec 2015 to 07 Feb 2017
dataset3 <- get_dataset("doi:10.7910/DVN/TZJGAT")
writeBin(get_file("sapaTempData696items22dec2015thru07feb2017.tab", "doi:10.7910/DVN/TZJGAT"), "sapaTempData696items22dec2015thru07feb2017.tab")
sapaTempData696items22dec2015thru07feb2017 <- fread("sapaTempData696items22dec2015thru07feb2017.tab", na.strings=getOption("<NA>","NA"))
sapaTempData696items22dec2015thru07feb2017 <- as.data.frame(sapaTempData696items22dec2015thru07feb2017)

### Import Item Info ----
#Import "ItemInfo696" from dataset3 ("doi:10.7910/DVN/TZJGAT")

writeBin(get_file("ItemInfo696.tab", "doi:10.7910/DVN/TZJGAT"), "ItemInfo696.tab")

sapa.icar.dictionary <- fread("ItemInfo696.tab", header=TRUE, na.strings=getOption("<NA>","NA"))

sapa.icar.dictionary <- as.data.frame(sapa.icar.dictionary)

# data.tables can't have rownames, so it shows up as variable V1. Give ItemInfo696 its rownames back.
rownames(sapa.icar.dictionary) <- sapa.icar.dictionary$V1

# Remove the now-superfluous first column.
sapa.icar.dictionary <- sapa.icar.dictionary[,-1]

### Remove Temporary Files ----
#Delete the files created by the "writeBin" function.
file.remove(c("sapaTempData696items08dec2013thru26jul2014.tab", "sapaTempData696items22dec2015thru07feb2017.tab", 
              "sapaTempData696items26jul2014thru22dec2015.tab", "ItemInfo696.tab"))

rm(dataset1, dataset2, dataset3)

#save(sapaTempData696items08dec2013thru26jul2014, sapaTempData696items26jul2014thru22dec2015, 
#     sapaTempData696items22dec2015thru07feb2017, sapa.icar.dictionary, file = "sapa_data.rda")

## Do the analyses ----
full.sapa <- rbind(sapaTempData696items08dec2013thru26jul2014,
                   sapaTempData696items26jul2014thru22dec2015,
                   sapaTempData696items22dec2015thru07feb2017)
sapa <- char2numeric(full.sapa) #convert string character data to numeric data
spi.items <- selectFromKeys(spi.keys)
#set the number of multiple cores to take advantage of them
#does not work on PCs
options("mc.cores" = 4)
sapa.spi <- scoreItems(spi.keys,sapa,impute="none") #find the scale scores and reliabililties
spi.scores <- sapa.spi$scores
demos <- sapa[c(2:10,14:23)] #choose the 19 demographic data
demos.spi <- cbind(demos,spi.scores)
set.seed (42)
ss <- sample(1:nrow(demos.spi),nrow(demos.spi)/2)
demos.b5 <- setCor(y=1:19,x=20:24,data=demos.spi[ss,],
                   plot=FALSE) #do the multiple regressions
summary(demos.b5)
demos.27 <- setCor(y=1:19,x=25:51,data=demos.spi[ss,],plot=FALSE)
demos.135 <-setCor(y =1:19,x=spi.items,data=sapa[ss,],plot=FALSE)
#now cross validate
pred.b5 <- predict.psych(demos.b5,data=demos.spi[-ss,] )
pred.27 <- predict.psych(demos.27,data=demos.spi[-ss,] )
cross.valid.b5 <- diag(cor2(pred.b5,demos.spi[-ss,1:19]))
cross.valid.l27 <- diag(cor2(pred.27,demos.spi[-ss,1:19]))

#sapa <- read.file() #goes to my directory to find the file
#load(sapa) #one extra step required
#sapa <- char2numeric(sapa) #makes the fields numeric
criteria <- colnames(sapa)[c(2:10,14:23)] #choose 19 criteria
spi.items <- selectFromKeys(spi.keys)
options("mc.cores"=8) #I am using a mac with multiple cores
scores <- scoreIrt.2pl(spi.keys,sapa) #ldo IRT scoring of the data
big.scores <- cbind(sapa[criteria],scores)
set.seed(42) #for reproducible results
ss <- sample(1:nrow(big.scores),nrow(big.scores)/2)
#derivation multiple Rs
sc.5 <- setCor(y=criteria,x=20:24, data=big.scores[ss,], plot=FALSE)
sc.27 <- setCor(y=criteria,x=25:51, data=big.scores[ss,], plot=FALSE)
sc.135 <- setCor(y=criteria, x=spi.items,data=sapa[ss,] ,plot=FALSE)
#now cross validate
cv.5 <- crossValidation(sc.5,big.scores[-ss,])
cv.27 <- crossValidation(sc.27,big.scores[-ss,])
cv.135 <- crossValidation(sc.135,sapa[-ss,])
cross.valid.df <- data.frame(cv5=cv.5$crossV, cv.27=cv.27$crossV,
                             cv135=cv.135$crossV)
cross.valid.df.sorted <- dfOrder(cross.valid.df,1)
#ItemInfo is a dictionary of all of the sapa items
#this information is also on DataVerse
ItemInfo <- sapa.icar.dictionary
bs.sapa<- bestScales(sapa[ss,],criteria=criteria, folds=10, n.item=20,
                     dictionary=ItemInfo[,1:2],cut=.05)
bs.cv <- crossValidation(bs.sapa,sapa[-ss,])
#combine the best scales
cross.valid.df <- data.frame(cv5=cv.5$crossV, cv.27=cv.27$crossV,

                             cv135=cv.135$crossV,cvbs= bs.cv$crossV)
cross.valid.df.sorted <- dfOrder(cross.valid.df,1)
matPlot(cross.valid.df.sorted[c(2,4,6,8)],
        main="Cross validation of multiple regression on sapa data",
        xlas=3, ylab="Cross Validated R",pch=15:18)
legend(1,.5,cs(bestS,27,135,b5),lty=c(4,2,3,1),col=c(4,2,3,1),pch=c(18,16,17,15))
#now try profiles
R.big <- cor(sapa[ss,24:719],sapa[ss,criteria],use="pairwise")
R.pheno <- cor(sapa[ss,criteria],use="pairwise")
R.profile <- cor(R.big)
sapa.pheno.profile <- lowerUpper(R.pheno,R.profile)
corPlot(sapa.pheno.profile,xlas=3,main="Phenotypic (lower) and Profile (upper) correlations")

# A5. Study 5: Applying large sample profiles to a smaller sample ----
#we need to change sex to gender to the next step with the spi
colnames(spi)[2] <- "gender"
colnames(sc.demos)[2] <-"gender"
small.crit <- criteria[criteria %in% colnames(spi)[1:10]]
R.spi.sapa <- cor(sapa[spi.items],sapa[small.crit],use="pairwise") #these are the profiles

small.5 <- setCor(y=small.crit,x = 20:24,data=big.scores,plot=FALSE)
small.27 <- setCor(y=small.crit,x = 25:51,data=big.scores,plot=FALSE)
small.135 <- setCor(y=small.crit,x=spi.items,data=sapa,plot=FALSE)
small.prof <- R.spi.sapa
small.bs <- bestScales(sapa[c(small.crit,spi.items)],criteria=small.crit, folds=10,
                       n.item=20,dictionary=ItemInfo[,1:2],cut=.05)

## Validation with SPI data ----
#now validate with the spi data
valid.5 <- crossValidation(small.5,sc.demos)
valid.27 <- crossValidation(small.27,sc.demos)
valid.135 <- crossValidation(small.135,spi)
valid.bs <- crossValidation(small.bs,spi)
small.prof.valid <- crossValidation(small.prof,spi)
small.valid.df <- data.frame(cv5=valid.5$crossV, cv.27=valid.27$crossV,
                             cv135=valid.135$crossV,cvbs= valid.bs$crossV,profile = small.prof.valid$crossV)
small.valid.df.sorted <- dfOrder(small.valid.df,1)
matPlot(small.valid.df.sorted[c(2,4,6,8,10)],
        main="Cross validation of multiple regression from SAPA on spi data",
        xlas=3, ylab="Cross Validated R",pch=15:19)
legend(1,.5,cs(bestS,profile,27,135,b5),lty=c(4,5,2,3,1),col=c(4,5,2,3,1),
       pch=c(18,19,16,17,15))