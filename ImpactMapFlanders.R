#hoedje met tabel voor DD en ND afzonderlijk
hoedje <- hoedje[,1:4]
colnames(hoedje) <- c("X", "Y", "DD", "ND")
hoedje$TD <- hoedje$DD + hoedje$ND
hoedjeDD <- matrix(hoedje$DD[1:160000], nrow=length(X))
rownames(hoedjeDD) <- Y
colnames(hoedjeDD) <- X
hoedjeND <- matrix(hoedje$ND[1:160000], nrow=length(X))
rownames(hoedjeND) <- Y
colnames(hoedjeND) <- X
hoedje

#Vd from VLOPS
DepositionAH <- read.table("~/VLOPS/VLOPS output/AllHabitats/DepositionAH.lpt", skip=16, header=F, nrows=116023)
tail(DepositionAH)
DepositionAH <- cbind(DepositionAH$V1, DepositionAH$V2, DepositionAH$V10)
colnames(DepositionAH) <- c("Nr", "ID", "Vd")
DepositionAH <- as.data.frame(DepositionAH)
DepositionAH$Vd <- DepositionAH$Vd/1000
write.csv(DepositionAH, "DepositionAH.csv")

Sources <- read.csv("~/Regional Model/GIS/BWK_natura2000/FlandersPoints1000m.csv")
Receptor <- read.csv("~/Regional Model/GIS/BWK_natura2000/AllHabitats_CSV.csv")

Receptor$X <- Receptor$X_MIN + 50
Receptor$Y <- Receptor$Y_MIN + 50 

Receptor <- cbind(Receptor$ID, Receptor$X, Receptor$Y, Receptor$KDWallmin, DepositionAH$Vd, Receptor$TNDmean)
Receptor <- as.data.frame(Receptor)
colnames(Receptor) <- c("ID", "X", "Y", "CL", "Vd", "TND")
Receptor$X <- round(Receptor$X)
Receptor$Y <- round(Receptor$Y)

Sources <- cbind(Sources$ID, Sources$X, Sources$Y)
Sources <- as.data.frame(Sources)
colnames(Sources) <- c("ID", "X", "Y")
Sources$X <- as.integer(Sources$X)
Sources$Y <- as.integer(Sources$Y)
Sources$TIS <- rep(0, nrow(Sources))

#Sources in same gridspacing as receptor (cfr resolution 'hoedje' 100m)
Sources$X <- Sources$X - 50
Sources$Y <- Sources$Y - 49
Sources$ID <- 1:nrow(Sources)


#ImpactMap using TIS (total impact score)
# and IC (impact class based on critical habitat cell)
#Sources500[1:5,5:6]
Scores <- apply(Sources500, 1, function(x)
{
        print(x[1])
        impacttable <- as.data.frame(matrix(ncol=9, nrow=nrow(Receptor)))
        colnames(impacttable) <- c("ID", "dep", "CL", "Vd", "IS", "X", "Y", "DD", "ND")
        impacttable$ID <- Receptor$ID
        impacttable$CL <- Receptor$CL
        impacttable$Vd <- Receptor$Vd
        impacttable$TND <- Receptor$TND
        #Convert to 'hoedje' coordinates
        impacttable$X <- Receptor$X - x[2]
        impacttable$Y <- Receptor$Y - x[3]
        #Create lookuptable 
        impacttable <- impacttable[which(impacttable$X <= 20000 & impacttable$X >= -19900),]
        impacttable <- impacttable[which(impacttable$Y <= 20000 & impacttable$Y >= -19900),]
        print(nrow(impacttable))
        mat <- apply(impacttable, 1, function(y){
                DD <- hoedjeDD[as.character(y[7]), as.character(y[6])]
                ND <- hoedjeND[as.character(y[7]), as.character(y[6])]
                cbind(DD, ND)
                #hoedje[which(hoedje$X==y[6] & hoedje$Y==y[7]),3:4]
        })
        mat <- data.frame(matrix(unlist(mat), nrow=nrow(impacttable),byrow=T))
        impacttable$DD <- mat$X1
        impacttable$ND <- mat$X2
        impacttable$dep <- (5000/8784)*(((impacttable$Vd/0.88)*impacttable$DD)+impacttable$ND)
        impacttable$IS <- impacttable$dep/impacttable$CL
        TIS <- sum(impacttable$IS)
        SC <- 100 * max(subset(impacttable, TND > CL, select = IS)) #CHC: TDN > CL!!
        return(cbind(TIS, SC))
        #return(((cbind(sum(impacttable$IS), (100*max((impacttable$IS)))))))
        
})


Scores<- as.data.frame(Scores)
Scores <- t(Scores)
Sources500$TIS <- Scores[,1]
Sources500$IC <- Scores[,2]
Sources500$TISuCL <- Scores[,3]
Sources500$TISaCL <- Scores[,4]

write.csv(Sources500, "Rastermap500.csv")


#Analysis
lm <- lm(Sources500$IC ~ Sources500$TIS) #Classic linear model
summary(lm)
par(mfrow=c(2,2))
plot(lm)

lm2 <- lm(log(Sources500$TIS) ~ log(Sources500$IC))
summary(lm2)
par(mfrow=c(2,2))
plot(lm2)
