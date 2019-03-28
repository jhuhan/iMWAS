####iMWAS: computing iMWAS statistics
####---------Input:
####---------------gwas:gwas z-score
####---------------weight:snp weight on methylation level
####---------------genoref: genotype reference data
####---------Return:
####---------------res: iMWAS z-score and p-value
MWAS<-function(gwas, weight, genoref){
	#impute missing values for reference genotype data
	if(sum(is.na(geno))>0){
   		colmean<-colMeans(geno,na.rm=TRUE)
   		id<-apply(geno,2,anyNA)
   		id<-which(id==TRUE)
   		for(j in 1:length(id)){
     			geno[is.na(geno[,id[j]])==TRUE,id[j]]=colmean[id[j]]
   		}
	}	
	
	#common snp set
	snps.com <- intersect(
		intersect(weight[,1], gwas[,1]),
	     	colnames(geno)
	)	
	
	#common subset data    
	idx1 <- match(snps.com, gwas[,1])
	idx2 <- match(snps.com, colnames(geno))
	idx3 <- match(snps.com, weight[,1])
	gwas.cg <- gwas[idx1,2]
	geno <- geno[,idx2]
	weight <- weight[idx3,2]
	
	#iMWAS statistics
	z <- gwas.cg %*% weight
	z.cor <- cor(geno)
	se <- sqrt(weight %*%  z.cor %*%  weight)
	
	z <- z/se
	p=pnorm(abs(z),lower.tail=F)*2
	res <- data.frame(z=z, p=p)
}

