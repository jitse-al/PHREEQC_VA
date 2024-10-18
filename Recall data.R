outputvectorfolder<-'C:/Users/Jitse/Documents/PHREEQC_R_Input/output/'
outputvectorfolder<-'D:/Thermodynamic Data Output PHREEQC/output/'
current_site<-'Va_zeolites_2'

outputvector=vector(mode = 'list')
current_folder<-paste(outputvectorfolder,current_site,sep="")
first_level<-list.files(current_folder)
first_level<-first_level[(first_level=="summary")==FALSE]
for(i in c(first_level)){
  second_level<-list.files(paste(current_folder,i,sep='/'))
  second_level<-second_level[(second_level=="summary")==FALSE]
  for(j in c(second_level)){
    third_level<-list.files(paste(current_folder,i,j,sep='/'))
    third_level<-third_level[(third_level=="summary")==FALSE]
    for(k in c(third_level)){
      output<-read.csv(paste(paste(current_folder,i,j,k,'output.csv',sep='/')))
      eval(parse(text = paste("outputvector$",i,"$",j,"$",k,"<-output",sep="")))
    }
  }
}
