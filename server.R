library(shiny)
require(rCharts)
load('biz.RData')
load("arizona.tract10.RData")
load("bizframe-geocoded.RData")
library(devtools) 
attach(new.biz)
require(ggplot2)
require(maps)
require(rCharts)
require(rjson)
require(sp)
require(reshape2)
library(RColorBrewer)
options(RCHART_WIDTH = 600,RCHART_HEIGHT=600)

map<-function(...){
  tapply(biz.frame$review_count,biz.frame$fips,mean,na.rm=T)->x
  data.frame(fips=rownames(x),avg_review=x)->y
  match(table=arizona.tract10$fips,x=y$fips)->ind
  data.frame(fips=arizona.tract10$fips,avg_review=NA,col='azure',stringsAsFactors=F)->avgReviewDf
  avgReviewDf$avg_review[ind]<-y$avg_review
  myCol <- colorRampPalette(c("lightblue","blue4"))(100)
  cut(log(avgReviewDf$avg_review),100)->buckets
  as.numeric(buckets)->bucketNum
  cbind(avgReviewDf,bucketNum)->avgReviewDf
  for (i in 1:nrow(avgReviewDf)){
    if (!is.na(avgReviewDf$bucketNum[i])) {
      avgReviewDf$col[i]<-myCol[avgReviewDf$bucketNum[i]]
    }
  }
  plot(arizona.tract10, xlim=c(min(new.biz$longitude)-0.1,max(new.biz$longitude)+0.1), 
       ylim=c(min(new.biz$latitude)-0.1,max(new.biz$latitude)+0.1), 
       col=avgReviewDf$col,border='gray')
  legend("topright",legend=levels(buckets)[c(1,20,40,60,80,100)],
         col=myCol[c(1,20,40,60,80,100)],pch=16,title='log(Avg Reviews) ',cex=0.8) 
}


map.1<-function(...){
  tapply(biz.frame$review_count,biz.frame$fips,mean,na.rm=T)->x
  data.frame(fips=rownames(x),avg_review=x)->y
  match(table=arizona.tract10$fips,x=y$fips)->ind
  data.frame(fips=arizona.tract10$fips,avg_review=NA,col='azure',stringsAsFactors=F)->avgReviewDf
  avgReviewDf$avg_review[ind]<-y$avg_review
  myCol <- colorRampPalette(c("lightblue","blue4"))(100)
  cut(log(avgReviewDf$avg_review),100)->buckets
  as.numeric(buckets)->bucketNum
  cbind(avgReviewDf,bucketNum)->avgReviewDf
  for (i in 1:nrow(avgReviewDf)){
    if (!is.na(avgReviewDf$bucketNum[i])) {
      avgReviewDf$col[i]<-myCol[avgReviewDf$bucketNum[i]]
    }
  }
  plot(arizona.tract10, xlim=c(min(new.biz$longitude)-0.1,max(new.biz$longitude)+0.1), 
       ylim=c(min(new.biz$latitude)-0.1,max(new.biz$latitude)+0.1), 
       col=avgReviewDf$col,border='gray')
  mycol=c('#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')
  round(sort(tapply(new.biz$review_count,new.biz$region,mean,na.rm=T)),0)->avg.review 
  data.frame(group=names(avg.review),avg.review,row.names=NULL)->df
  df$col<-mycol  
  match(x=new.biz$region,table=df$group)->group_index
  df$col[group_index]->new.biz$col
  df$avg.review[group_index]->new.biz$avg_count
  plot(arizona.tract10, xlim=c(min(new.biz$longitude)-0.1,max(new.biz$longitude)+0.1), 
       ylim=c(min(new.biz$latitude)-0.1,max(new.biz$latitude)+0.1), 
       col=avgReviewDf$col,border='gray')
  points(new.biz$long,new.biz$lat,col=new.biz$col,pch=19,cex=0.8)
  legend("topright",legend=paste(df$group,'-',round(df$avg.review,3)),
         col=df$col,pch=16,title='Regions',cex=0.8)
}



shinyServer(
  function(input, output) {
    output$chart1= renderChart2({   
      p1<-rPlot(input$var4,input$var5, data = new.biz, type = 'point', color = input$color)
      p1$params$width <- 520
      p1$params$height <- 450
      return(p1)
    })
    
    output$myplot=renderPlot({
      v1<-as.numeric(input$var1)
      v2<-as.numeric(input$var2)
      environment<-environment()    
      plot(qplot(y=new.biz[,5],x=new.biz[,v1],fill=new.biz[,v2],data=new.biz,geom='bar',
                 stat="identity",position="dodge")+xlab(toupper(names(new.biz)[v1]))+ylab('REVIEW COUNTS')+
             labs(fill=toupper(names(new.biz)[v2])))
      output$down<-downloadHandler(
        filename=function(){
          paste('Review_Counts by',input$var3,sep='.')
          
        },
        content=function(file){
          if(input$var3=='png')
            png(file)
          else
            pdf(file)
          v1<-as.numeric(input$var1)
          v2<-as.numeric(input$var2)
          environment<-environment()   
          plot(qplot(y=new.biz[,5],x=new.biz[,v1],fill=new.biz[,v2],data=new.biz,geom='bar',
                     stat="identity",position="dodge")+xlab(toupper(names(new.biz)[v1]))+ylab('REVIEW COUNTS')+
                 labs(fill=toupper(names(new.biz)[v2]))+ggtitle(paste('Review Counts by', names(new.biz[v1]))))
          dev.off()
        }
      )  
    }) 
    output$temp<-renderPlot({
      power<-as.numeric(input$slide) 
      category<-as.numeric(input$Category)
      ts_review<-review_count^power
      par(mfrow=c(2,1))
      boxplot(ts_review,main='Boxplot by Review Counts',
              ylab='Review Counts')
      boxplot(ts_review~new.biz[,category],data=new.biz,
        main=paste('Boxplot by Review Counts by',toupper(names(new.biz)[category])),ylab='Review Counts')
    }) 
    output$map<-renderPlot({
      if(input$var6=='Tracts')
        return(map())
      else
        return(map.1())
    },height=450,width=600)
    
    output$myChart2<-renderMap({
      map3 <- Leaflet$new()
      map3$setView(c(33.49637,-112.0026), zoom = 11)
      biz.count200<-new.biz[new.biz$review_count>=200,]
      for (i in 1:dim(biz.count200)[1]){
        map3$marker(c(biz.count200$lat[i],biz.count200$long[i]),
                    bindPopup = paste(biz.count200$food_type[i],biz.count200$stars[i],sep=','))
      }
      map3   
    })
  })

