library(ggplot2)
Receptor$diff <- Receptor$TND - Receptor$CL
ggplot(Receptor, aes(x=diff)) + geom_histogram(binwidth = 1, colour = "black", fill = "white") + 
        geom_vline(aes(xintercept = 0), color = "red", linetype = "dashed", size = 1) +
        ylab("Frequentie (aantal Natura2000-hectarecellen)") + xlab("kgN/ha.yr") +
        ggtitle("Overschrijding kritische depositiewaarde")


#Calculate the part of TIS under CL (TISuCL) and the part above CL (TISaCL)
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
        TISuCL <- sum(subset(impacttable, TND < CL, select = IS))
        TISaCL <- TIS - TISuCL 
        SC <- 100 * max(subset(impacttable, TND > CL, select = IS)) #CHC: TDN > CL!!
        return(cbind(TIS, SC, TISuCL, TISaCL))
})

Scores<- as.data.frame(Scores)
Scores <- t(Scores)
Sources500$TIS <- Scores[,1]
Sources500$IC <- Scores[,2]
Sources500$TISuCL <- Scores[,3]
Sources500$TISaCL <- Scores[,4]

write.csv(Sources500, "Rastermap500bis.csv")
