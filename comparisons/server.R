library(shiny)
library(datasets)

shinyServer(function(input,output,session){
  
  output$result<-renderText({
    
    datasetInput<-reactive({
      switch(input$dataset,
             'x'=x,
             'a'=a,
             'b'=b,
             'c'=c,
             'd'=d,
             'e'=e,
             'f'=f,
             'g'=g,
             'h'=h,
             'i'=i,
             'j'=j,
             'z'=z,
             'gg'=gg,
             'hh'=hh,
             'ii'=ii,
             'jj'=jj,
             'kk'=kk,
             'll'=ll,
             'mm'=mm,
             'nn'=nn,
             'oo'=oo,
             'pp'=pp,
             'qq'=qq,
             'rr'=rr,
             'ss'=ss,
             'tt'=tt,
             'uu'=uu,
             'vv'=vv,
             'ww'=ww,
             'xx'=xx,
             'yy'=yy,
             'zz'=zz,
             'z1'=z1)
    })
    
    if(input$x==1){
      
      N1<-(input$a+input$b+input$c+input$d+input$e)
      N2<-(input$f+input$g+input$h+input$i+input$j)
      
      m1<-matrix(data=NA,ncol=N1,nrow=1)
      m2<-matrix(data=NA,ncol=N2,nrow=1)
      
      if(input$a!=0){for(u in 1:input$a){
        m1[,u]<-5
      }}
      if(input$b!=0){for(u in 1:input$b){
        m1[,(input$a+u)]<-4
      }}
      if(input$c!=0){for(u in 1:input$c){
        m1[,(input$a+input$b+u)]<-3
      }}
      if(input$d!=0){for(u in 1:input$d){
        m1[,(input$a+input$b+input$c+u)]<-2
      }}
      if(input$e!=0){for(u in 1:input$e){
        m1[,(input$a+input$b+input$c+input$d+u)]<-1
      }}
      if(input$f!=0){for(u in 1:input$f){
        m2[,u]<-5
      }}
      if(input$g!=0){for(u in 1:input$g){
        m2[,(input$f+u)]<-4
      }}
      if(input$h!=0){for(u in 1:input$h){
        m2[,(input$f+input$g+u)]<-3
      }}
      if(input$i!=0){for(u in 1:input$i){
        m2[,(input$f+input$g+input$h+u)]<-2
      }}
      if(input$j!=0){for(u in 1:input$j){
        m2[,(input$f+input$g+input$h+input$i+u)]<-1
      }}
      
      t1<-t.test(as.numeric(m1),as.numeric(m2))
      ele3<-round(t1$p.value,digits=5)
      ele1<-if(t1$p.value<0.0001){'<0.0001'}else{ele3}
      ele2<-c('p-value=')
      mean1<-round(mean(m1),digits=2)
      mean2<-round(mean(m2),digits=2)
      me1<-c('\nMean of current study:')
      me2<-c('\nMean of past study:')
      message<-c(ele2,ele1,me1,mean1,me2,mean2)
      return(message)
      
    }else{
      
      N1<-(input$gg+input$hh+input$ii+input$jj+input$kk+input$ll+input$mm+input$nn+input$oo+input$pp+input$z)
      N2<-(input$qq+input$rr+input$ss+input$tt+input$uu+input$vv+input$ww+input$xx+input$yy+input$zz+input$z1)
      
      m3<-matrix(data=NA,ncol=N1,nrow=1)
      m4<-matrix(data=NA,ncol=N2,nrow=1)
      
      if(input$gg!=0){for(u in 1:input$gg){
        m3[,u]<-1
      }}
      if(input$hh!=0){for(u in 1:input$hh){
        m3[,(input$gg+u)]<-2
      }}
      if(input$ii!=0){for(u in 1:input$ii){
        m3[,(input$gg+input$hh+u)]<-3
      }}
      if(input$jj!=0){for(u in 1:input$jj){
        m3[,(input$gg+input$hh+input$ii+u)]<-4
      }}
      if(input$kk!=0){for(u in 1:input$kk){
        m3[,(input$gg+input$hh+input$ii+input$jj+u)]<-5
      }}
      if(input$ll!=0){for(u in 1:input$ll){
        m3[,(input$gg+input$hh+input$ii+input$jj+input$kk+u)]<-6
      }}
      if(input$mm!=0){for(u in 1:input$mm){
        m3[,(input$gg+input$hh+input$ii+input$jj+input$kk+input$ll+u)]<-7
      }}
      if(input$nn!=0){for(u in 1:input$nn){
        m3[,(input$gg+input$hh+input$ii+input$jj+input$kk+input$ll+input$mm+u)]<-8
      }}
      if(input$oo!=0){for(u in 1:input$oo){
        m3[,(input$gg+input$hh+input$ii+input$jj+input$kk+input$ll+input$mm+input$nn+u)]<-9
      }}
      if(input$pp!=0){for(u in 1:input$pp){
        m3[,(input$gg+input$hh+input$ii+input$jj+input$kk+input$ll+input$mm+input$nn+input$oo+u)]<-10
      }}
      if(input$z!=0){for(u in 1:input$z){
        m3[,(input$gg+input$hh+input$ii+input$jj+input$kk+input$ll+input$mm+input$nn+input$oo+input$pp+u)]<-0
      }}
      if(input$qq!=0){for(u in 1:input$qq){
        m4[,u]<-1
      }}
      if(input$rr!=0){for(u in 1:input$rr){
        m4[,(input$qq+u)]<-2
      }}
      if(input$ss!=0){for(u in 1:input$ss){
        m4[,(input$qq+input$rr+u)]<-3
      }}
      if(input$tt!=0){for(u in 1:input$tt){
        m4[,(input$qq+input$rr+input$ss+u)]<-4
      }}
      if(input$uu!=0){for(u in 1:input$uu){
        m4[,(input$qq+input$rr+input$ss+input$tt+u)]<-5
      }}
      if(input$vv!=0){for(u in 1:input$vv){
        m4[,(input$qq+input$rr+input$ss+input$tt+input$uu+u)]<-6
      }}
      if(input$ww!=0){for(u in 1:input$ww){
        m4[,(input$qq+input$rr+input$ss+input$tt+input$uu+input$vv+u)]<-7
      }}
      if(input$xx!=0){for(u in 1:input$xx){
        m4[,(input$qq+input$rr+input$ss+input$tt+input$uu+input$vv+input$ww+u)]<-8
      }}
      if(input$yy!=0){for(u in 1:input$yy){
        m4[,(input$qq+input$rr+input$ss+input$tt+input$uu+input$vv+input$ww+input$xx+u)]<-9
      }}
      if(input$zz!=0){for(u in 1:input$zz){
        m4[,(input$qq+input$rr+input$ss+input$tt+input$uu+input$vv+input$ww+input$xx+input$yy+u)]<-10
      }}
      if(input$z1!=0){for(u in 1:input$z1){
        m4[,(input$qq+input$rr+input$ss+input$tt+input$uu+input$vv+input$ww+input$xx+input$yy+input$zz+u)]<-0
      }}
      
      t2<-t.test(as.numeric(m3),as.numeric(m4))
      ele4<-round(t2$p.value,digits=5)
      ele5<-if(t2$p.value<0.0001){'<0.0001'}else{ele4}
      ele2<-c('p-value=')
      mean3<-round(mean(m3),digits=2)
      mean4<-round(mean(m4),digits=2)
      me1<-c('\nMean of current study:')
      me2<-c('\nMean of past study:')
      message<-c(ele2,ele5,me1,mean3,me2,mean4)
      return(message)
    }
  })
  
    observeEvent(input$reset,{
      updateNumericInput(session,'a',value=0)
      updateNumericInput(session,'b',value=0)
      updateNumericInput(session,'c',value=0)
      updateNumericInput(session,'d',value=0)
      updateNumericInput(session,'e',value=0)
      updateNumericInput(session,'f',value=0)
      updateNumericInput(session,'g',value=0)
      updateNumericInput(session,'h',value=0)
      updateNumericInput(session,'i',value=0)
      updateNumericInput(session,'j',value=0)
      updateNumericInput(session,'zz',value=0)
      updateNumericInput(session,'z',value=0)
      updateNumericInput(session,'gg',value=0)
      updateNumericInput(session,'hh',value=0)
      updateNumericInput(session,'ii',value=0)
      updateNumericInput(session,'jj',value=0)
      updateNumericInput(session,'kk',value=0)
      updateNumericInput(session,'ll',value=0)
      updateNumericInput(session,'mm',value=0)
      updateNumericInput(session,'nn',value=0)
      updateNumericInput(session,'oo',value=0)
      updateNumericInput(session,'pp',value=0)
      updateNumericInput(session,'qq',value=0)
      updateNumericInput(session,'rr',value=0)
      updateNumericInput(session,'ss',value=0)
      updateNumericInput(session,'tt',value=0)
      updateNumericInput(session,'uu',value=0)
      updateNumericInput(session,'vv',value=0)
      updateNumericInput(session,'ww',value=0)
      updateNumericInput(session,'xx',value=0)
      updateNumericInput(session,'yy',value=0)
      updateNumericInput(session,'z1',value=0)
    })
  
})
