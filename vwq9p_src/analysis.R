#reading in data (requires R ~3.2.5+; can take a bit)
qms=read.csv('https://osf.io/ey9hs/?action=download')

#adjusting LIWC frequencies by word count
lid=grep('Sixltr|OtherP',names(qms))
cats=names(qms)[lid[1]:lid[2]]
qms[,cats]=qms[,cats]*(1+qms$WC*.0001)

#installing and loading plotting package
install.packages('devtools')
devtools::install_github('miserman/splot')
library('splot')

#setting up colors and line types for plots
ns=unique(qms$name)
nl=length(ns)
disp=data.frame(name=ns,lty=rep(1:9,nl)[1:nl])
disp$col=ifelse(grepl('Hit|Rod',disp$name),'#880101','#aaaaaa')
disp$lty[grepl('Hit|Rod',disp$name)]=1:2

#setting up options so this doesn't have to be entered into each call
opt=list(
  data=quote(qms), points=FALSE, lines='loess', leg=FALSE, colors=disp$col,lty=disp$lty, title=FALSE, add=quote({
    mnc=names(sort(apply(sapply(cdat$`.^^.`,'[[','y'),2,mean)[-grep('Hit|Rod',names(cdat$`.^^.`))],T)[1:3])
    legend('top',sort(mnc),lty=disp[disp$name%in%mnc,2],col=disp[disp$name%in%mnc,3],lwd=2,bty='n')
    legend('topright',c('Adolf Hitler','Elliot Rodger'),col=disp$col[1],lty=1:2,lwd=2,bty='n')
    legend('topleft',c('Killers','Comparisons'),lty=1,lwd=2,col=disp[1:2,3],bty='n')
  })
)

#Figure 1. death frequency across segments
splot(death~index*name,myl=c(0,1.3),mxl=c(3,98),labx='Segment',laby='death',options=opt)

#Figure 2. anger frequency across segments
splot(anger~index*name,myl=c(-.05,2.24),mxl=c(3,98),labx='Segment',laby='anger',options=opt)

#sentiment analysis
#this is already included in the data as it can take a little while to process
install.packages('sentimentr')
library('sentimentr')

fsent=sentiment(qms$text,lexicon::hash_sentiment_huliu)

qms$sentiment=lapply(split(fsent$sentiment,fsent$element_id),mean)

#Correlations between sentiment and LIWC categories
cats=c('sentiment','negemo','anx','anger','sad','posemo')

#Table 2. between all segments
cor(qms[,cats])

#between text means
cor(t(sapply(split(qms[,cats],qms$name),colMeans)))

#Average between segments within author
Reduce('+',lapply(split(qms[,cats],qms$name),cor))/length(ns)

#Figure 3. sentiment across segments
opt$add=parse(text=paste(c(gsub('top\\"','bottom\\"',opt$add),'}'),collapse=';'))
splot(sentiment~index*name,myl=c(-.15,.29),mxl=c(3,98),labx='Segment',options=opt)

#function for cluster analysis
clplot=function(cs,main,hang=-1){
  if(missing(main))
    main=sub(', (?=[A-z]+$)',ifelse(length(cs)>2,', and ',' and '),paste(cs,collapse=', '),perl=TRUE)
  ms=matrix(NA,nl,length(cs),dimnames=list(ns,cs))
  for(n in ns) ms[n,]=colMeans(qms[qms$name==n,cs])
  plot(hclust(dist(ms),'ward.D2'),main=main,hang=hang,axes=FALSE)
}

#Figure 4. comparing cluster analyses when pronouns are and are not included
op=par(mfrow=c(2,1),mar=c(1,0,1,0))
clplot(c('anger','death'))
clplot(c('i','we','anger','death'))
par(op)

#Figure 5. we and they across segments for two authors
splot(cbind(we,they)~index,between=name,data=qms[grep('Hit|Rod',qms$name),],
  ndisp=F,title=F,laby='Affiliative Reference',labx='Segment')

#making 3-level segment and centered focus variables
qms$Segment=factor(ifelse(qms$index<33,0,ifelse(qms$index>66,2,1)),label=c('Beginning','Middle','End'),ordered=T)
qms$Centered_focusfuture=c(sapply(split(qms$focusfuture,qms$name),function(l)l-mean(l))[,unique(qms$name)])

#Figure 6. future focus within 3 levels of segmentation
splot(Centered_focusfuture~name*Segment,data=qms,title=F,note=F,labx=F,
  lpos='bottomright',labels.trim=F,mxl=c(1,length(ns)),myl=c(-.43,.4))

#Figures 7, 8, and 9. developmental categories across segments of the Rodger text
sud=qms[grep('Rod',qms$name),]
opt=list(data=sud,lines='loess',points=F,mv.scale=T,labels=F)

splot(cbind(family,friend)~index,myl=c(-.7,1.3),options=opt)

splot(cbind(female,male,sexual)~index,myl=c(-.7,.45),lpos='bottom',options=opt)

splot(cbind(sentiment,affect)~index,myl=c(-1.25,.8),lpos='bottom',options=opt)

#function to rank authors by the grand mean of a category
ams=function(cat){
  td=matrix(qms[,cat],100)
  res=data.frame(
    mean=colMeans(td),
    sd=apply(td,2,sd),
    row.names=unique(qms$name)
  )
  res[order(res[,1]),]
}

#a few categories of interest
ams('anger')
ams('death')
ams('i')
ams('Comma')
