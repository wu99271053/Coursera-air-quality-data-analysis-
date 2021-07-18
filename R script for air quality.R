# Function1 pollutantmean,calculate the mean of the pollutant within a given time. 
pollutantmean<-function(directory, pollutant, id=1:332)

{
  a<-list.files(directory)
# two empty vector for recording the values and the numbers of value 
  m<-rep(NA,times=length(id))
  l<-rep(NA,times=length(id))

  for (i in id){
# loop over the directory and read all the csv 
    b<-read.csv(a[i])
    m[i]<-sum(b[,pollutant][!is.na(b[,pollutant])])

    l[i]<-length(b[,pollutant][!is.na(b[,pollutant])])

  }
  sum(m,na.rm=TRUE)/sum(l,na.rm=TRUE)
  
  # Function 2, see how many 
  complete<-function(directory,id=1:332){
  a<-list.files(directory)
  b<-rep(NA,times=length(id))
  l<-rep(NA,times=length(id))
  for (i in id){
    c<-read.csv(a[i])
    b[i]<-sum(complete.cases(c))

    l[i]<-i
}
  return(rbind(b[!is.na(b)],l[!is.na(l)]))


  }

corr<-function(directory,threshold=0){
  a<-list.files(directory)
  b<-rep(NA,times=332)
  l<-rep(NA,times=332)
  for (i in 1:332){
    c<-read.csv(a[i])
    b[i]<-sum(complete.cases(c))
    if(b[i]>=threshold{
      l[i]<-cor(c[,2][complete.cases(c)],c[,3][complete.cases(c)])
    }
  }
  return(as.numeric(head(l[!is.na(l)])))
  }
