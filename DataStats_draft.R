convert.magic <- function(obj,types){
  out <- lapply(1:length(obj),FUN = function(i){FUN1 <- switch(types[i],character = as.character,numeric = as.numeric,factor = as.factor); FUN1(obj[,i])})
  names(out) <- colnames(obj)
  as.data.frame(out)
  return(out)
}

path<-"D:/NEW TIAA DATA/NEW INFOGROUP DATA - NOV 21/Unzipped files"
pathreport<-"D:/Data Cleaning and Univariate Analysis/Univariate Analysis/Result Univariate Analysis/third party"
filename<-c("TIAA_PARTICIPANT_INDIVIDUALS_D171121_fixed.csv")
filepath<-paste(rep(path, each=length(filename)),filename, sep='/')
report<-paste('VitalStats_v01_',filename,sep='')

setwd(pathreport)
library(data.table)
results<-fread(file=filepath,header=TRUE,sep=",") #Update file arguement
  #setkey(results,pin)
  #results<-results[pin!=0]
  names(results)<-gsub("^\\s+|\\s+$", "", names(results))
  names(results)<-gsub("\\.\\.",".",names(results))
  names(results)<-gsub("\\.","_", names(results))
  #results$DIST_USD_CONV<-as.numeric(results$DIST_USD_CONV)
  #df_datatype<-data.frame(Field=names(results), datatype=sapply(results, class), row.names=NULL)
  #write.csv(df_datatype, paste(pathreport, 'Datatype.csv', sep=''), row.names=FALSE)
  #df_datatype<-read.csv(paste(pathreport, 'Datatype.csv', sep=''), header=TRUE)
  #results<-convert.magic(results, df_datatype$datatype)
  
#  output<-data.frame()
quants=c(0,0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99,1.00)
percentile<-quote({  
  ls.1=(quantile(results[[field]],quants, na.rm=TRUE))
  names(ls.1)=quants
  as.list(ls.1)
})

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#results<-data.table(results)
count<-1
Obs<-c()
Name<-c()
Type<-c()
numobs<-c()
nmiss<-c()
per_miss<-c()
uniq<-c()
mean_or_top1<-c()
min_or_top2<-c()
p1_or_top3<-c()
p5_or_top4<-c()
p25_or_top5<-c()
median_or_bot5<-c()
p75_or_bot4<-c()
p95_or_bot3<-c()
p99_or_bot2<-c()
max_or_bot1<-c()
for(field in names(results)){
  print(field)
  Obs<-c(Obs,count)
  #print(Obs)
  Name<-c(Name, field)
  Type<-c(Type, class(results[[field]]))
  numobs<-c(numobs, length(results[[field]]))
  #print(Name)
  #results[[field]]<-ifelse(results[[field]]=='#NA', NA, results[[field]])
  nmiss<-c(nmiss,sum(is.na(results[[field]]) | (results[[field]]=="")))
  #print(nmiss)
  per_miss<-c(per_miss, sprintf("%.0f%%", nmiss[length(nmiss)]/nrow(results) * 100))
  #print(per_miss)
  uniq<-c(uniq, length(unique(results[[field]])))
  print(class(results[[field]]))
  switch(class(results[[field]]),
         
         numeric={
           dist_field<-results[,eval(percentile)]
           mean_or_top1<-c(mean_or_top1, as.character(mean(results[[field]], na.rm=TRUE)))
           min_or_top2<-c(min_or_top2, as.character(dist_field[,"0", with=FALSE]))
           p1_or_top3<-c(p1_or_top3, as.character(dist_field[,"0.01", with=FALSE]))
           p5_or_top4<-c(p5_or_top4, as.character(dist_field[,"0.05", with=FALSE]))
           p25_or_top5<-c(p25_or_top5, as.character(dist_field[,"0.25", with=FALSE]))
           median_or_bot5<-c(median_or_bot5, as.character(dist_field[,"0.5", with=FALSE]))
           p75_or_bot4<-c(p75_or_bot4, as.character(dist_field[,"0.75", with=FALSE]))
           p95_or_bot3<-c(p95_or_bot3, as.character(dist_field[,"0.95", with=FALSE]))
           p99_or_bot2<-c(p99_or_bot2, as.character(dist_field[,"0.99", with=FALSE]))
           max_or_bot1<-c(max_or_bot1, as.character(dist_field[,"1", with=FALSE]))
         },
         integer={
           dist_field<-results[,eval(percentile)]
           mean_or_top1<-c(mean_or_top1, as.character(mean(results[[field]], na.rm=TRUE)))
           min_or_top2<-c(min_or_top2, as.character(dist_field[,"0", with=FALSE]))
           p1_or_top3<-c(p1_or_top3, as.character(dist_field[,"0.01", with=FALSE]))
           p5_or_top4<-c(p5_or_top4, as.character(dist_field[,"0.05", with=FALSE]))
           p25_or_top5<-c(p25_or_top5, as.character(dist_field[,"0.25", with=FALSE]))
           median_or_bot5<-c(median_or_bot5, as.character(dist_field[,"0.5", with=FALSE]))
           p75_or_bot4<-c(p75_or_bot4, as.character(dist_field[,"0.75", with=FALSE]))
           p95_or_bot3<-c(p95_or_bot3, as.character(dist_field[,"0.95", with=FALSE]))
           p99_or_bot2<-c(p99_or_bot2, as.character(dist_field[,"0.99", with=FALSE]))
           max_or_bot1<-c(max_or_bot1, as.character(dist_field[,"1", with=FALSE]))
         },
         integer64={
           dist_field<-results[,eval(percentile)]
           mean_or_top1<-c(mean_or_top1, as.character(mean(results[[field]], na.rm=TRUE)))
           min_or_top2<-c(min_or_top2, as.character(dist_field[,"0", with=FALSE]))
           p1_or_top3<-c(p1_or_top3, as.character(dist_field[,"0.01", with=FALSE]))
           p5_or_top4<-c(p5_or_top4, as.character(dist_field[,"0.05", with=FALSE]))
           p25_or_top5<-c(p25_or_top5, as.character(dist_field[,"0.25", with=FALSE]))
           median_or_bot5<-c(median_or_bot5, as.character(dist_field[,"0.5", with=FALSE]))
           p75_or_bot4<-c(p75_or_bot4, as.character(dist_field[,"0.75", with=FALSE]))
           p95_or_bot3<-c(p95_or_bot3, as.character(dist_field[,"0.95", with=FALSE]))
           p99_or_bot2<-c(p99_or_bot2, as.character(dist_field[,"0.99", with=FALSE]))
           max_or_bot1<-c(max_or_bot1, as.character(dist_field[,"1", with=FALSE]))
         },
         factor={
           dist_field<-results[,list(freq=.N), field]    
           ix<-order(dist_field$freq, decreasing=TRUE)
           mean_or_top1<-c(mean_or_top1, paste(dist_field[[field]][ix[1]], "::", dist_field[ix[1],'freq', with=FALSE], sep=''))
           min_or_top2<-c(min_or_top2,paste(dist_field[[field]][ix[2]], "::", dist_field[ix[2],'freq', with=FALSE], sep=''))
           p1_or_top3<-c(p1_or_top3, paste(dist_field[[field]][ix[3]], "::", dist_field[ix[3],'freq', with=FALSE], sep=''))
           p5_or_top4<-c(p5_or_top4, paste(dist_field[[field]][ix[4]], "::", dist_field[ix[4],'freq', with=FALSE], sep=''))
           p25_or_top5<-c(p25_or_top5, paste(dist_field[[field]][ix[5]], "::", dist_field[ix[5],'freq', with=FALSE], sep=''))
           if(nrow(dist_field)>5){
           median_or_bot5<-c(median_or_bot5, paste(dist_field[[field]][ix[length(ix)-4]], "::", dist_field[ix[length(ix)-4],'freq', with=FALSE], sep=''))
           p75_or_bot4<-c(p75_or_bot4, paste(dist_field[[field]][ix[length(ix)-3]], "::", dist_field[ix[length(ix)-3],'freq', with=FALSE], sep=''))
           p95_or_bot3<-c(p95_or_bot3, paste(dist_field[[field]][ix[length(ix)-2]], "::", dist_field[ix[length(ix)-2],'freq', with=FALSE], sep=''))
           p99_or_bot2<-c(p99_or_bot2, paste(dist_field[[field]][ix[length(ix)-1]], "::", dist_field[ix[length(ix)-1],'freq', with=FALSE], sep=''))
           max_or_bot1<-c(max_or_bot1, paste(dist_field[[field]][ix[length(ix)]], "::", dist_field[ix[length(ix)],'freq', with=FALSE], sep=''))
           } else{
             median_or_bot5<-c(median_or_bot5, "NA::NA")
             p75_or_bot4<-c(p75_or_bot4, "NA::NA")
             p95_or_bot3<-c(p95_or_bot3, "NA::NA")
             p99_or_bot2<-c(p99_or_bot2, "NA::NA")
             max_or_bot1<-c(max_or_bot1, "NA::NA")
           }
         },
         character={
           dist_field<-results[,list(freq=.N), field]    
           ix<-order(dist_field$freq, decreasing=TRUE)
           mean_or_top1<-c(mean_or_top1, paste(dist_field[[field]][ix[1]], "::", dist_field[ix[1],'freq', with=FALSE], sep=''))
           min_or_top2<-c(min_or_top2,paste(dist_field[[field]][ix[2]], "::", dist_field[ix[2],'freq', with=FALSE], sep=''))
           p1_or_top3<-c(p1_or_top3, paste(dist_field[[field]][ix[3]], "::", dist_field[ix[3],'freq', with=FALSE], sep=''))
           p5_or_top4<-c(p5_or_top4, paste(dist_field[[field]][ix[4]], "::", dist_field[ix[4],'freq', with=FALSE], sep=''))
           p25_or_top5<-c(p25_or_top5, paste(dist_field[[field]][ix[5]], "::", dist_field[ix[5],'freq', with=FALSE], sep=''))
           if(nrow(dist_field)>5){
             median_or_bot5<-c(median_or_bot5, paste(dist_field[[field]][ix[length(ix)-4]], "::", dist_field[ix[length(ix)-4],'freq', with=FALSE], sep=''))
             p75_or_bot4<-c(p75_or_bot4, paste(dist_field[[field]][ix[length(ix)-3]], "::", dist_field[ix[length(ix)-3],'freq', with=FALSE], sep=''))
             p95_or_bot3<-c(p95_or_bot3, paste(dist_field[[field]][ix[length(ix)-2]], "::", dist_field[ix[length(ix)-2],'freq', with=FALSE], sep=''))
             p99_or_bot2<-c(p99_or_bot2, paste(dist_field[[field]][ix[length(ix)-1]], "::", dist_field[ix[length(ix)-1],'freq', with=FALSE], sep=''))
             max_or_bot1<-c(max_or_bot1, paste(dist_field[[field]][ix[length(ix)]], "::", dist_field[ix[length(ix)],'freq', with=FALSE], sep=''))
           } else{
             median_or_bot5<-c(median_or_bot5, "NA::NA")
             p75_or_bot4<-c(p75_or_bot4, "NA::NA")
             p95_or_bot3<-c(p95_or_bot3, "NA::NA")
             p99_or_bot2<-c(p99_or_bot2, "NA::NA")
             max_or_bot1<-c(max_or_bot1, "NA::NA")
           }
         },
         logical={
           dist_field<-results[,list(freq=.N), field]    
           ix<-order(dist_field$freq, decreasing=TRUE)
           mean_or_top1<-c(mean_or_top1, paste(dist_field[[field]][ix[1]], "::", dist_field[ix[1],'freq'], sep=''))
           min_or_top2<-c(min_or_top2,paste(dist_field[[field]][ix[2]], "::", dist_field[ix[2],'freq', with=FALSE], sep=''))
           p1_or_top3<-c(p1_or_top3, paste(dist_field[[field]][ix[3]], "::", dist_field[ix[3],'freq', with=FALSE], sep=''))
           p5_or_top4<-c(p5_or_top4, paste(dist_field[[field]][ix[4]], "::", dist_field[ix[4],'freq', with=FALSE], sep=''))
           p25_or_top5<-c(p25_or_top5, paste(dist_field[[field]][ix[5]], "::", dist_field[ix[5],'freq', with=FALSE], sep=''))
           if(nrow(dist_field)>5){
           median_or_bot5<-c(median_or_bot5, paste(dist_field[[field]][ix[length(ix)-4]], "::", dist_field[ix[length(ix)-4],'freq', with=FALSE], sep=''))
           p75_or_bot4<-c(p75_or_bot4, paste(dist_field[[field]][ix[length(ix)-3]], "::", dist_field[ix[length(ix)-3],'freq', with=FALSE], sep=''))
           p95_or_bot3<-c(p95_or_bot3, paste(dist_field[[field]][ix[length(ix)-2]], "::", dist_field[ix[length(ix)-2],'freq', with=FALSE], sep=''))
           p99_or_bot2<-c(p99_or_bot2, paste(dist_field[[field]][ix[length(ix)-1]], "::", dist_field[ix[length(ix)-1],'freq', with=FALSE], sep=''))
           max_or_bot1<-c(max_or_bot1, paste(dist_field[[field]][ix[length(ix)]], "::", dist_field[ix[length(ix)],'freq', with=FALSE], sep=''))
           }else{
             median_or_bot5<-c(median_or_bot5, "NA::NA")
             p75_or_bot4<-c(p75_or_bot4, "NA::NA")
             p95_or_bot3<-c(p95_or_bot3, "NA::NA")
             p99_or_bot2<-c(p99_or_bot2, "NA::NA")
             max_or_bot1<-c(max_or_bot1, "NA::NA")
           }
         })
count<-count+1
}
  output<-data.frame(Obs=Obs, Name=Name,Type=Type,numobs=numobs, nmiss=nmiss, per_miss=per_miss, unique=uniq, mean_or_top1=mean_or_top1, min_or_top2=min_or_top2,
                     p1_or_top3=p1_or_top3, p5_or_top4=p5_or_top4, p25_or_top5=p25_or_top5,median_or_bot5=median_or_bot5, p75_or_bot4=p75_or_bot4, p95_or_bot3=p95_or_bot3, p99_or_bot2=p99_or_bot2, max_or_bot1=max_or_bot1)
  print(output)

  write.csv(output, paste(pathreport, report, sep='/'), row.names=FALSE)
  
  
#write.csv(results[sample(.N,10000)], "D:/Data Cleaning and Univariate Analysis/Sample Data/kepler_201708_sample.csv", row.names=F)
