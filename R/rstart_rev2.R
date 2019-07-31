cl.write<-function(out.fw){
	if(.Platform$OS.type=="windows"){
		write.table(out.fw,file="clipboard",sep='\t',row.names=TRUE,col.names=NA)
	}
	if(.Platform$OS.type=="unix"){
		zz<-pipe('pbcopy','w')
		write.table(out.fw,file=zz,sep='\t',row.names=TRUE,col.names=NA)
		close(zz)
	}
}

cl.read<-function(h=TRUE){
	if(.Platform$OS.type=="windows"){
		if(h){out <- read.table("clipboard",header=TRUE,sep='\t')}
		else{out <- read.table("clipboard",header=FALSE,sep='\t')}
	}
	if(.Platform$OS.type=="unix"){
		if (h){out<-read.table(pipe("pbpaste"),header=TRUE,sep='\t')}
		else{out<-read.table(pipe("pbpaste"),header=FALSE,sep='\t')}
	}
	out
}

saveR=function(robj,fname,direc=""){
	fn=paste(direc,fname,sep="")
	fn=paste(fn,"R",sep=".")
	dump(robj,file=fn)
}

readR=function(fname,direc=""){
	fn=paste(direc,fname,sep="")
	fn=paste(fn,"R",sep=".")
	source(fn)
}
cmdel=function(st){
	u=as.character(st)
	u=strsplit(u,",")[[1]]
	u=as.numeric(u)
	n=length(u)
	w=10^seq(3*(n-1),0,by=-3)
	sum(u*w)
}
cmdel.all=function(dat){
	n=length(dat)
	out=numeric(n)
	for(i in 1:n){
		out[i]=cmdel(dat[i])
	}
	out
}
