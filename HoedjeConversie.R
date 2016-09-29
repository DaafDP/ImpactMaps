library(raster)
hoedjeDD <- expand.grid(hoedje$X, hoedje$Y)
X <- c(-199:200)
X <- X*100
Y <- X
hoedjeDD <- matrix(hoedje$DD[1:160000], nrow=length(X))
rownames(hoedjeDD) <- Y
colnames(hoedjeDD) <- X
hoedjeND <- matrix(hoedje$ND[1:160000], nrow=length(X))
rownames(hoedjeND) <- Y
colnames(hoedjeND) <- X

#Sources with Resolution 500
SourcesR500 <- Sources[,2:3]
SourcesAppend <- cbind((SourcesR500$X+500), Sources$Y)
colnames(SourcesAppend) <- c("X", "Y")
SourcesR500 <- rbind(SourcesR500, SourcesAppend)
SourcesAppend <- cbind(Sources$X, (SourcesR500$Y+500))
colnames(SourcesAppend) <- c("X", "Y")
SourcesR500 <- rbind(SourcesR500, SourcesAppend)
SourcesAppend <- cbind((SourcesR500$X + 500), (SourcesR500$Y+500))
colnames(SourcesAppend) <- c("X", "Y")
SourcesR500 <- rbind(SourcesR500, SourcesAppend)
SourcesR500 <- cbind(c(1:109088),SourcesR500)
colnames(SourcesR500) <- c("ID", "X", "Y")
SourcesR500$TIS <- rep(0, nrow(SourcesR500))
SourcesR500$IC <- rep(0, nrow(SourcesR500))
