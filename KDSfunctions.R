#Rounds a number based on given critical value.  critround(x,y), x is number to round, y is cutoff decimal point for up or down (defaults to .5)
critround<-function(x,y=0.5){
  if((x-(floor(x)))<y){return(floor(x))
  }else{
    return(ceiling(x))
  }
}

#finds the n, excluding NAs, of an object
n<-function(x){
  if(is.factor(x)){
    return(length(na.exclude(x)))
  }else{return(x)}}

#summary of important information by question.  cprsummary(x,y), where x is variable to analyse and y is item wtih all participants represented
#NOTE: incorporates smartmatrix and smartlabel functions below.
cprsummary<-function(x,y=participant){
  subsample<-function(x,y){
    if((n(x)/n(y))==1){
      response<-'N'}
    else{
      response<-'n'}
    return(response)}
  total<-n(x)
  breakdown<-smartmatrix(x)
  colnames(breakdown)<-smartlabel(x)
  rownames(breakdown)<-c('Number of participants')
  nas<-length(grep('not applicable',x,ignore.case=TRUE))
  whichn<-subsample(x,y)
  if(((length(grep('yes',x,ignore.case=TRUE)))+(length(which(x=='No'))))>=1){
    cc<-list('Use N or n?'=whichn,'Total participants who responded'=total,'Response breakdown'=breakdown,'Responses of >>Not Applicable<<'=nas)
  }else{
    ave<-smartaverage(x)
    cc<-list('Use N or n?'=whichn,'Total participants who responded'=total,'Response breakdown'=breakdown,'Responses of >>Not Applicable<<'=nas,'Response average'=ave)}
  return(cc)
}

#copies sentence summarizing questionnaire item to clipboard.  x=item # and y=general topic
cprreport<-function(x,y){
  item<-as.character(y)
  out<-c('out of')
  part<-c('participants, or')
  rate<-c('%, rated')
  end<-c('as good to very good.')
  avewas<-c('The average rating was ')
  if(((length(grep('poor',x,ignore.case=TRUE)))+(length(grep('good',x,ignore.case=TRUE))))>=1){
    gvg<-((length(which(x=='Good')))+(length(which(x=='Very Good'))))
    tot<-((n(x))-(length(which(na.exclude(x=='Not Applicable')))))
    pct<-round(((gvg/tot)*100),digits=1)
    aver<-smartaverage(x)
    cat(gvg,out,tot,part,pct,rate,item,end,avewas,aver,file='clipboard')
  }else{if(((length(grep('difficult',x,ignore.case=TRUE)))+(length(grep('easy',x,ignore.case=TRUE))))>=1){
    gvg<-((length(which(x=='Easy')))+(length(which(x=='Very Easy'))))
    tot<-((n(x))-(length(which(na.exclude(x=='Not Applicable')))))
    pct<-round(((gvg/tot)*100),digits=1)
    aver<-smartaverage(x)
    cat(gvg,out,tot,part,pct,rate,item,end,avewas,aver,file='clipboard')
  }else{if(((length(which(x==1)))+(length(which(x==2)))+(length(which(x==3)))+(length(which(x==4)))+(length(which(x==5)))+(length(which(x==6)))+(length(which(x==7)))+(length(which(x==8)))+(length(which(x==9)))+(length(which(x==10)))+(length(grep('zero',x,ignore.case=TRUE))))>=1){
    gvg<-((length(which(x==8)))+(length(which(x==9)))+(length(which(x==10))))
    tot<-((n(x))-(length(which(na.exclude(x=='Not Applicable')))))
    pct<-round(((gvg/tot)*100),digits=1)
    aver<-smartaverage(x)
    cat(gvg,out,tot,part,pct,rate,item,end,avewas,aver,file='clipboard')
  }else{if(((length(grep('yes',x,ignore.case=TRUE)))+(length(which(x=='No'))))>=1){
    gvg<-((length(which(x=='Yes'))))
    tot<-((n(x))-(length(which(na.exclude(x=='Not Applicable')))))
    pct<-round(((gvg/tot)*100),digits=1)
    resp<-c('%, responded Yes when asked')
    cat(gvg,out,tot,part,pct,resp,item,file='clipboard')
  }else{message('ERROR: item reponses not of type Likert, Difficult/easy, or 10 point')}}}}}

#t.test on likert, easy/difficult, 0-10 items
cprcompare<-function(u,v){
  require(plyr)
  if(((length(grep('poor',u,ignore.case=TRUE)))+(length(grep('good',u,ignore.case=TRUE))))>=1){
    a<-mapvalues(u,from=c('Very Good','Good','Okay','Poor','Very Poor'),to=c(5,4,3,2,1))
    b<-mapvalues(v,from=c('Very Good','Good','Okay','Poor','Very Poor'),to=c(5,4,3,2,1))
    w<-as.numeric(as.vector(a))
    z<-as.numeric(as.vector(b))
    answer<-t.test(w,z)
  }else{if(((length(grep('difficult',u,ignore.case=TRUE)))+(length(grep('easy',u,ignore.case=TRUE))))>=1){
    a<-mapvalues(u,from=c('Very Easy','Easy','Okay','Difficult','Very Difficult'),to=c(5,4,3,2,1))
    b<-mapvalues(v,from=c('Very Easy','Easy','Okay','Difficult','Very Difficult'),to=c(5,4,3,2,1))
    w<-as.numeric(as.vector(a))
    z<-as.numeric(as.vector(b))
    answer<-t.test(w,z)
  }else{if(((length(which(u==1)))+(length(which(u==2)))+(length(which(u==3)))+(length(which(u==4)))+(length(which(u==5)))+(length(which(u==6)))+(length(which(u==7)))+(length(which(u==8)))+(length(which(u==9)))+(length(which(u==10)))+(length(grep('zero',u,ignore.case=TRUE))))>1){
    w<-as.numeric((u))
    z<-as.numeric((v))
    answer<-t.test(w,z)}
  }
  }
  return(answer)
}


#randomize conditions.
#randomize(x,y,w,z), x is number of conditions, y is number of participants, w is participant ids, and z is condition names
#w and z are optional
randomize<-function(x,y,w=NA,z=NA){
  mat<-matrix(data=NA,nrow=x,ncol=y)
  if(is.na(w)){rownames(mat)<-c(1:x)}
  else{rownames(mat)<-w}
  if(is.na(z)){colnames(mat)<-c(1:y)}
  else{colnames(mat)<-z}
  for (i in 1:x){
    mat[i,]<-sample(1:y,y,replace=F)
  }
  return(mat)
}

#calculates rau based on correct and total items
raucalc<-function(x='Number correct',y='Total (correct+incorrect)'){
  a1<-asin(sqrt(x/(n(y)+1)))
  a2<-asin(sqrt((x+1)/(n(y)+1)))
  t<-a1+a2
  rau<-((46.47324337*t)-23)
  return(rau)
}

#creates a list of x copied y times
copy<-function(x,y){
  b<-numeric(length=y)
  for(i in 1:y){
    b[i]<-x
  }
  return(b)}

#looks for x in y
look <- function(x, y)
{
  counter <- 0
  for (i in 1:length(y))
  {
    if (y[i] == x)
    {
      counter <- counter + 1
    }
  }
  if (counter >= 1)
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}

#fits a functino to an audio and returns the area under the audiogram
audioarea<-function()
{
  fn<-as.character(readline('Enter file name containing audiograms>>'))
  audios<-read.csv(fn)
  attach(audios)
  
  cols<-ncol(audios)
  rows<-nrow(audios)
  
  freqs<-as.numeric(audios[1,2:cols])
  freqs2<-as.numeric(freqs^2)
  
  
  mat<-matrix(data=NA,nrow=(rows-1),ncol=3)
  colnames(mat)<-c('Subject','Ear','Area')
  
  for(i in 1:(rows-1))
  {
    thresh<-as.numeric(audios[i+1,2:cols])
    
    model<-lm(thresh~freqs+freqs2)
    
    integrand<-function(x)
    {model$coefficients[1]+(model$coefficients[2]*x)+(model$coefficients[3]*(x^2))}
    
    area<-integrate(integrand,250,8000)
    
    mat[i,1]<-as.character(audios[i+1,1])
    mat[i,3]<-area$value
    
    if(((rows-1)-i)/(rows-1)>=0.5)
    {
      mat[i,2]<-'Right'
    }
    else
    {
      mat[i,2]<-'Left'
    }
  }
  
  write.csv(mat,file='area.csv',row.names=FALSE)
  
}

#logical fxn.  returns true if x is within +/- z of y
wthin<-function(x,y,z)
{
  lwr<-(x-z)
  upr<-(x+z)
  if(y<=upr&y>=lwr)
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}

#takes audios from 2 csvs and returns matrix of all audios that are matched.
matchfinder<-function(){
  nr1<-nrow(data1)
  nr2<-nrow(data2)
  nc1<-(ncol(data1))
  m<-matrix(data=NA,nrow=(nr1*nr2),ncol=2)
  colnames(m)<-c('SubID Data1','SubID Data2')
  r<-1
  
  wthin<-function(x,y,z)
  {
    lwr<-(x-z)
    upr<-(x+z)
    if(y<=upr&y>=lwr)
    {
      return(TRUE)
    }
    else
    {
      return(FALSE)
    }
  }
  
  for(i in 1:nr1)
  {
    for(k in 1:nr2)
    {
      p<-0
      for(j in 2:(nc1))
      {
        if(wthin(as.numeric(data1[i,j]),as.numeric(data2[k,j]),5))
        {
          p<-p+1
        }
      }
      if(p==(nc1-1))
      {
        if((as.character(data1[i,1])!=(as.character(data2[k,1]))))
        {
          m[r,1]<-as.character(data1[i,1])
          m[r,2]<-as.character(data2[k,1])
          r<-r+1
        }
      }
    }
  }
  return(na.exclude(m))
}

##FOLLOWING ARE PART OF SOURCE CODE FOR FIGURE GENERATOR
#You may find smart average function helpful (calculates average value of likert and 10 point scales)
#used to scale the yaxis on a plot based on the max value of a matrix input
ymax<-function(x){
  mround <- function(x,base){
    base*ceiling(x/base)}
  if(max(x)>40){
    response<-mround(max(x),10)
  }else{
    if(max(x)>20){
      response<-mround(max(x),5)
    }else{
      if(max(x)>14){
        response<-20
      }else{if(max(x)>10){
        response<-14
      }else{if(max(x)>5){
        response<-10
      }else{response<-5}
      }
      }
    }
  }
  return(response)}

#give title with appropriate n/N and N/As only if NA present.  x=question in plot (not matrix!), y=variable with all participants represented, z=desired title in quotes
givetitle<-function(x,y,z){
  w<-as.character(z)
  subsample<-function(x,y){
    if((n(x)/n(y))==1){
      response<-'N'}
    else{
      response<-'n'}
    return(response)}
  if(length(grep('not applicable',x,ignore.case=TRUE))>0){
    response<-bquote(~.(print(w))~~(.(subsample(x,y))~'='~.(n(x)))~(N/As:.(length(grep('not applicable',x,ignore.case=TRUE)))))
  }else{
    response<-bquote(~.(print(w))~~(.(subsample(x,y))~'='~.(n(x))))}
  return(response)}

#functions to set up matrices
#custom matrices
smartmatrix<-function(x){
  if(((length(grep('poor',x,ignore.case=TRUE)))+(length(grep('good',x,ignore.case=TRUE))))>1){
    vp<-length(which(x=='Very Poor'))
    p<-length(which(x=='Poor'))
    ok1<-length(which(x=='Okay'))
    g<-length(which(x=='Good'))
    vg<-length(which(x=='Very Good'))
    response<-(matrix(c(vp,p,ok1,g,vg),byrow=TRUE,ncol=5))
  }else{
    if(((length(grep('difficult',x,ignore.case=TRUE)))+(length(grep('easy',x,ignore.case=TRUE))))>1){
      vd<-length(which(x=='Very Difficult'))
      d<-length(which(x=='Difficult'))
      ok2<-length(which(x=='Okay'))
      e<-length(which(x=='Easy'))
      ve<-length(which(x=='Very Easy'))
      response<-(matrix(c(vd,d,ok2,e,ve),byrow=TRUE,ncol=5))
    }else{
      if(((length(grep('yes',x,ignore.case=TRUE)))+(length(which(x=='No'))))>1){
        yes<-length(grep('yes',x,ignore.case=TRUE))
        no<-length(which(x=='No'))
        response<-(matrix(c(yes,no),byrow=TRUE,ncol=2))
      }else{
        if(((length(which(x==1)))+(length(which(x==2)))+(length(which(x==3)))+(length(which(x==4)))+(length(which(x==5)))+(length(which(x==6)))+(length(which(x==7)))+(length(which(x==8)))+(length(which(x==9)))+(length(which(x==10)))+(length(grep('zero',x,ignore.case=TRUE))))>1){
          n0<-length(grep('zero',x,ignore.case=TRUE))
          n1<-length(which(x==1))
          n2<-length(which(x==2))
          n3<-length(which(x==3))
          n4<-length(which(x==4))
          n5<-length(which(x==5))
          n6<-length(which(x==6))
          n7<-length(which(x==7))
          n8<-length(which(x==8))
          n9<-length(which(x==9))
          n10<-length(which(x==10))
          response<-(matrix(c(n0,n1,n2,n3,n4,n5,n6,n7,n8,n9,n10),byrow=TRUE,ncol=11))
        }else{
          response<-'Error: Check data source for errors.  Note: You may receive this error if all values are blank, Not Applicable, or Okay'}
      }
    }
  }
  return(response)}

smartlabel<-function(x){
  if(((length(grep('poor',x,ignore.case=TRUE)))+(length(grep('good',x,ignore.case=TRUE))))>=1){
    response<-c('Very Poor\n(1)','Poor\n(2)','Okay\n(3)','Good\n(4)','Very Good\n(5)')
  }else{
    if(((length(grep('yes',x,ignore.case=TRUE)))+(length(which(x=='No'))))>=1){
      response<-c('Yes','No')
    }else{
      if(((length(which(x==1)))+(length(which(x==2)))+(length(which(x==3)))+(length(which(x==4)))+(length(which(x==5)))+(length(which(x==6)))+(length(which(x==7)))+(length(which(x==8)))+(length(which(x==9)))+(length(which(x==10)))+(length(grep('zero',x,ignore.case=TRUE))))>=1){
        response<-c('0 (Not\nat all\nsatisfied)','1','2','3','4','5 (Neutral)','6','7','8','9','10\n((Extremely\nsatisfied)')
      }else{
        if(((length(grep('difficult',x,ignore.case=TRUE)))+(length(grep('easy',x,ignore.case=TRUE))))>=1){
          response<-c('Very\nDifficult (1)','Difficult\n(2)','Okay\n(3)','Easy\n(4)','Very\nEasy (5)')
        }else{
          response<-'Error: Check data source for errors.  Note: You may receive this error if all values are blank, Not Applicable, or Okay'}
      }
    }
  }
  return(response)}

smartaverage<-function(x){
  if(((length(grep('poor',x,ignore.case=TRUE)))+(length(grep('good',x,ignore.case=TRUE))))>1){
    response<-(((length(which(x=='Very Poor')))+(2*(length(which(x=='Poor'))))+(3*(length(which(x=='Okay'))))+(4*(length(which(x=='Good'))))+(5*(length(which(x=='Very Good')))))/(length(which(na.exclude(x!='Not Applicable')))))
  }else{
    if(((length(grep('difficult',x,ignore.case=TRUE)))+(length(grep('easy',x,ignore.case=TRUE))))>1){
      response<-(((length(which(x=='Very Difficult')))+(2*(length(which(x=='Difficult'))))+(3*(length(which(x=='Okay'))))+(4*(length(which(x=='Easy'))))+(5*(length(which(x=='Very Easy')))))/(length(which(na.exclude(x!='Not Applicable')))))
    }else{
      if(((length(which(x==1)))+(length(which(x==2)))+(length(which(x==3)))+(length(which(x==4)))+(length(which(x==5)))+(length(which(x==6)))+(length(which(x==7)))+(length(which(x==8)))+(length(which(x==9)))+(length(which(x==10)))+(length(grep('zero',x,ignore.case=TRUE))))>1){
        response<-(((length(which(x=='Zero')))+(length(which(x==1)))+(2*(length(which(x==2))))+(3*(length(which(x==3))))+(4*(length(which(x==4))))+(5*(length(which(x==5))))+(6*(length(which(x==6))))+(7*(length(which(x==7))))+(8*(length(which(x==8))))+(9*(length(which(x==9))))+(10*(length(which(x==10)))))/(length(which(na.exclude(x!='Not Applicable')))))
      }else{
        response<-'Error: Check data source for errors.  Note: You may receive this error if all values are blank, Not Applicable, or Okay'}
    }
  }
  return(round(response,digits=2))}

smartlines<-function(x){
  if(max(x)>30){
    abline(h=seq(0,ymax(x),by=5),lty='dotted',col='gray50')
  }else{
    abline(h=seq(0,ymax(x),by=1),lty='dotted',col='gray50')
  }
}

#x=variable, y=item w/ all participants, z=title, a=cex.main value, w=include average yes 1/no 0
smartchart<-function(x='variable',y='item w/ all participants',z='title',a='cex.main',w='include average 1 yes 0 no'){
  pt<-(a/12)
  if(w==1){
    barplot(smartmatrix(x),col='cornflowerblue',
            ylab="Number of particpants",ylim=c(0,(ymax(smartmatrix(x)))),
            main=givetitle(x,y,z),
            sub=bquote("Average:"~.(smartaverage(x))),
            names.arg=smartlabel(x),cex.main=pt,cex.lab=1.2,cex.sub=1.2)
    mtext('Subjective Rating',side=1,line=2.5,cex=1.25)
    smartlines(smartmatrix(x))
  }else{if(w==0){
    barplot(smartmatrix(x),col='cornflowerblue',
            ylab="Number of particpants",ylim=c(0,(ymax(smartmatrix(x)))),
            main=givetitle(x,y,z),
            names.arg=smartlabel(x),cex.main=pt,cex.lab=1.2,cex.sub=1.2)
    mtext('Subjective Rating',side=1,line=2.5,cex=1.25)
    smartlines(smartmatrix(x))
  }
  }
}





likertmatrix<-function(x){
  vp<-length(which(x=='Very Poor'))
  p<-length(which(x=='Poor'))
  ok<-length(which(x=='Okay'))
  g<-length(which(x=='Good'))
  vg<-length(which(x=='Very Good'))
  return(matrix(c(vp,p,ok,g,vg),byrow=TRUE,ncol=5))}

easematrix<-function(x){
  vd<-length(which(x=='Very Difficult'))
  d<-length(which(x=='Difficult'))
  ok<-length(which(x=='Okay'))
  e<-length(which(x=='Easy'))
  ve<-length(which(x=='Very Easy'))
  return(matrix(c(vd,d,ok,e,ve),byrow=TRUE,ncol=5))}

tenptmatrix<-function(x){
  n0<-length(grep('zero',x,ignore.case=TRUE))
  n1<-length(which(x==1))
  n2<-length(which(x==2))
  n3<-length(which(x==3))
  n4<-length(which(x==4))
  n5<-length(which(x==5))
  n6<-length(which(x==6))
  n7<-length(which(x==7))
  n8<-length(which(x==8))
  n9<-length(which(x==9))
  n10<-length(which(x==10))
  return(matrix(c(n0,n1,n2,n3,n4,n5,n6,n7,n8,n9,n10),byrow=TRUE,ncol=11))}

yesnomatrix<-function(x){
  yes<-length(grep('yes',x,ignore.case=TRUE))
  no<-length(grep('no',x,ignore.case=TRUE))
  return(matrix(c(yes,no),byrow=TRUE,ncol=2))}


#averaging ease ratings
easeaverage<-function(x){
  response<-(((length(which(x=='Very Difficult')))+(2*(length(which(x=='Difficult'))))+(3*(length(which(x=='Okay'))))+(4*(length(which(x=='Easy'))))+(5*(length(which(x=='Very Easy')))))/(length(which(na.exclude(x!='Not Applicable')))))
  return(round(response,digits=2))}

#give ease average verbal label
ease.ave.name<-function(x){
  if(easeaverage(x)>=4.5){response<-'Very Easy'
  }else{if(easeaverage(x)>=3.5){response<-'Easy'
  }else{if(easeaverage(x)>=2.5){response<-'Okay'
  }else{if(easeaverage(x)>=1.5){response<-'Difficult'
  }else{response<-'Very Difficult'}}}}
  return(response)}

#averaging likert scales
likertaverage<-function(x){
  response<-(((length(which(x=='Very Poor')))+(2*(length(which(x=='Poor'))))+(3*(length(which(x=='Okay'))))+(4*(length(which(x=='Good'))))+(5*(length(which(x=='Very Good')))))/(length(which(na.exclude(x!='Not Applicable')))))
  return(round(response,digits=2))}

#give likert average name
likert.ave.name<-function(x){
  if(likertaverage(x)>=4.5){response<-'Very Good'
  }else{if(likertaverage(x)>=3.5){response<-'Good'
  }else{if(likertaverage(x)>=2.5){response<-'Okay'
  }else{if(likertaverage(x)>=1.5){response<-'Poor'
  }else{response<-'Very Poor'}}}}
  return(response)}

#10 point average
tenptaverage<-function(x){
  response<-(((length(which(x=='Zero')))+(length(which(x==1)))+(2*(length(which(x==2))))+(3*(length(which(x==3))))+(4*(length(which(x==4))))+(5*(length(which(x==5))))+(6*(length(which(x==6))))+(7*(length(which(x==7))))+(8*(length(which(x==8))))+(9*(length(which(x==9))))+(10*(length(which(x==10)))))/(length(which(na.exclude(x!='Not Applicable')))))
  return(response)}

#y-scaling function
FUNCTIONNAME<-function(x){
  if(max(x)>25)
  {response<-30
  }else{
    if((max(x)>20))
    {response<-25
    }else{
      if(max(x)>14)
      {response<-20
      }else{
        if(max(x)>10)
        {response<-14
        }else{
          if(max(x)>5)
          {response<-10
          }else{
            response<-5}
        }
      }
    }
  }
  return(response)}

