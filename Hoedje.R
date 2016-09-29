hoedje <- hoedje[,1:4]
colnames(hoedje) <- c("X", "Y", "DD", "ND")
hoedje$TD <- hoedje$DD + hoedje$ND
write.csv(hoedje, "hoedje.csv")

Receptor <- cbind(Receptor$ID, Receptor$X, Receptor$Y, rep(1, nrow(Receptor)), rep(15, nrow(Receptor)))
Receptor <- as.data.frame(Receptor)
colnames(Receptor) <- c("ID", "X", "Y", "Vd", "CL")

Sources <- cbind(Sources$ID, Sources$X, Sources$Y, Sources$impactscor)
Sources <- as.data.frame(Sources)
colnames(Sources) <- c("ID", "X", "Y", "TIS")

Sources$TIS<- apply(Sources, 1, function(x)
        {
        print(x[1])
        impacttable <- as.data.frame(matrix(ncol=9, nrow=nrow(Receptor)))
        colnames(impacttable) <- c("ID", "dep", "CL", "Vd", "IS", "X", "Y", "DD", "ND")
        impacttable$ID <- Receptor$ID
        impacttable$CL <- Receptor$CL
        impacttable$vd <- Receptor$Vd
        impacttable$X <- x[2] - Receptor$X
        impacttable$Y <- x[3] - Receptor$Y
        #Create lookuptable 
        LookupTable <- hoedje[which(hoedje$X <= max(impacttable$X) & hoedje$X >= min(impacttable$X)),]
        LookupTable <- LookupTable[which(LookupTable$Y <= max(LookupTable$Y) & LookupTable$Y >= min(impacttable$Y)),]
        mat <- apply(impacttable, 1, function(y){
                LookupTable[which(LookupTable$X==y[6] & LookupTable$Y==y[7]),3:4]
                })
        mat <- data.frame(matrix(unlist(mat), nrow=nrow(impacttable), byrow=T))
        impacttable$DD <- mat$X1
        impacttable$ND <- mat$X2
        impacttable$dep <- (5000/8784)*(((impacttable$vd/0.88)*impacttable$DD)+impacttable$ND)
        impacttable$IS <- impacttable$dep/impacttable$CL
        return(sum(impacttable$IS))
        
})

write.csv(Sources, "Rastermap.csv")
