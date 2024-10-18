library(readxl)
foldername <-'C:/Users/Jitse/Documents/University/PhD - Amsterdam/Stuff for Publication Vista Alegre'
  filename <- 'Va_qtz_vein_1'
filetypein<-'.xlsx'
filetypeout<-'.csv'
sheetnumber=2
data<-read_excel(paste(foldername,'/',filename,filetypein,sep=""),sheet = 2)
data<-as.data.frame(data)
samplesize<-100

#1 Create groups
groups<-unique(data[,c(1,7)])

# Create output
output<-list()
for(i in 1:5){output[[i]]<-data.frame()}
  
# Randomly select minerals  
for(i in 1:length(groups[,1])){
  subgroups<-list()
  outputnumber<-1
  nr_minerals<-groups[i,2]
  if(i>1){outputnumber<-sum(ceiling(groups[c(0:(i-1)),2]))+1}
  for(j in 1:5){
    subgroups[[j]]<-data[data[,1]==groups[i,1],j+1]
    subgroups[[j]]<-subgroups[[j]][is.na(subgroups[[j]])==FALSE]
    if(length(subgroups[[j]])>0){
      if(nr_minerals<1){
        for(k in 1:samplesize){
          if(runif(1)<nr_minerals){output[[j]][outputnumber,k]<-sample(subgroups[[j]],1)}
          else{output[[j]][outputnumber,k]<-NA}}}
      if(nr_minerals==1){
        for(k in 1:samplesize){
          output[[j]][outputnumber,k]<-sample(subgroups[[j]],1)}}
      if(nr_minerals>1){
        for(k in 1:samplesize){
          output[[j]][outputnumber:(outputnumber+nr_minerals-1),k]<-sample(subgroups[[j]],nr_minerals)}}
    }
}}

#refining output
totalsize<-sum(ceiling(groups[,2]))
for(i in 1:length(output)){
  for(j in 1:samplesize){
    unique_minerals_temp<-sort(unique(output[[i]][,j]))
    output[[i]][1:length(unique_minerals_temp),j]<-unique_minerals_temp
    if(length(unique_minerals_temp)<totalsize)
      for(k in c((length(unique_minerals_temp)+1):totalsize)){
        output[[i]][[k,j]]<-NA
      }
  }
  while(sum(is.na(output[[i]][length(output[[i]][,1]),]))==samplesize){
    output[[i]]<-output[[i]][(1:length(output[[i]][,1])-1),]}
  output[[i]][is.na(output[[i]])]<-""
}

#Export data
for(i in 1:5){
if(dir.exists(paste(foldername,names(data)[i+1],sep='/'))==FALSE){
  dir.create(paste(foldername,names(data)[i+1],sep='/'))
  }
  write.csv(output[[i]],paste(foldername,names(data)[i+1],paste(filename,filetypeout,sep=""),sep='/'),row.names = FALSE)
}

















for(i in 1:3){
  data[[i]]<-as.list(read_excel(paste(foldername,'/',filename,filetypein,sep=""),sheet = i))
  for(j in 1:length(data[[i]])){data[[i]][[j]]<-data[[i]][[j]][is.na(data[[i]][[j]])==FALSE]}
}

for(j in 1:length(data[[1]])){
  samplegroup1 <-c(data[[1]][[j]],data[[2]][[j]])
  samplegroup2 <-data[[2]][[j]]
  tempdataframe<-data.frame(zeros(length(data[[1]]),samplesize))
  for(i in 1:samplesize){
    sample1<-sample(samplegroup1,1)
    samplegroup3 <-c(data[[3]][[j]],data[[2]][[j]])
    samplegroup3 <-samplegroup3[(samplegroup3==sample1)==FALSE]
    sample2<-sample(samplegroup3,1)
    samplegroup2 <-data[[2]][[j]]
    samplegroup2 <-samplegroup2[(samplegroup2==sample1)==FALSE]
    samplegroup2 <-samplegroup2[(samplegroup2==sample2)==FALSE]
    samples<-c(sample1, sample2, sample(samplegroup2,3))
    tempdataframe[,i]<-samples
  }
  if(dir.exists(paste(foldername,names(data[[1]])[j],sep='/'))==FALSE){
    dir.create(paste(foldername,names(data[[1]])[j],sep='/'))}
  write.csv(tempdataframe,paste(foldername,names(data[[1]])[j],paste(filename,filetypeout,sep=""),sep='/'),row.names = FALSE)
}





library(readxl)
foldername <-'C:/Users/Jitse/Documents/PHREEQC_R_Input/Equilibrium Phases Groups'
filename <- '1b'
filetypein<-'.xlsx'
filetypeout<-'.csv'
data=list()
samples_per_group <-(c(1,1,1,1,1,3,4))
samplesize<-100
sort_by_first_three_letters='on'

for(i in 1:length(samples_per_group)){
  data[[i]]<-as.list(read_excel(paste(foldername,'/',filename,filetypein,sep=""),sheet = i))
  for(j in 1:length(data[[i]])){data[[i]][[j]]<-data[[i]][[j]][is.na(data[[i]][[j]])==FALSE]}
}


for(j in 1:length(data[[1]])){
  tempdataframe<-data.frame(zeros(sum(samples_per_group),samplesize))
  for(i in 1:samplesize){
    temp_selection<-character()
    for(k in 1:length(samples_per_group)){
      if(length(data[[k]][[j]])>0){
        if(samples_per_group[k]>1){
          if(sort_by_first_three_letters=='on'){
            temp_selection_2<-sample(data[[k]][[j]],1)
            for(samplenr in 2:samples_per_group[k]){
              first_three_letters<-character()
              for(ts in 1:length(temp_selection_2)){
                ftl1<-strsplit(temp_selection_2[ts],"")[[1]][1:3]
                first_three_letters<-c(first_three_letters,paste(ftl1[1],ftl1[2],ftl1[3],sep=""))
              }
              ftl2<-first_three_letters[1]
              while(sum(ftl2==first_three_letters)>0){
                new_sample<-sample(data[[k]][[j]],1)
                ftl2<-strsplit(new_sample,"")[[1]][1:3]
                ftl2<-paste(ftl2[1],ftl2[2],ftl2[3],sep="")
              }
              temp_selection_2<-c(temp_selection_2,new_sample)
            }
            temp_selection<-c(temp_selection,temp_selection_2)
          }
          else{temp_selection<-c(temp_selection,sample(data[[k]][[j]],samples_per_group[k]))}
        }
        else{temp_selection<-c(temp_selection,sample(data[[k]][[j]],samples_per_group[k]))}
      }
    }
    temp_selection<-unique(temp_selection)
    while(length(temp_selection)<sum(samples_per_group)){
      temp_selection<-c(temp_selection,"")
    }
    tempdataframe[,i]<-temp_selection
    
  }
  if(dir.exists(paste(foldername,names(data[[1]])[j],sep='/'))==FALSE){
    dir.create(paste(foldername,names(data[[1]])[j],sep='/'))}
  write.csv(tempdataframe,paste(foldername,names(data[[1]])[j],paste(filename,filetypeout,sep=""),sep='/'),row.names = FALSE)
}

