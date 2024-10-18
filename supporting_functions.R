export_output_hist_boxplots <-function(output,
                                       hist_division,
                                       names_output_log,
                                       names_output_normal,
                                       foldername,
                                       current_equilibrium_phases){
  #Split output into 2 dataframes. One for phases, one for other stuff
  output_phases_start <-c(1:length(output))[names(output)==to_Column_name(paste("si_",current_equilibrium_phases[1],sep=""))]
  output_phases <- output[,output_phases_start:(output_phases_start+length(current_equilibrium_phases)-1)]
  output_other <- output[,c((1:(output_phases_start-1)),((output_phases_start+length(current_equilibrium_phases)):length(output)))]
  output_phases[output_phases<0]<--1
  output_phases <- output_phases + 1
  i <- 1
  non_produced_names <- c()
  while(i<=length(output_phases)){
    if(sum(output_phases[,i])==0){
      non_produced_names <-c(non_produced_names,names(output_phases)[i])
      output_phases_selecter <- ones(1,length(output_phases))
      output_phases_selecter[i] <- 0
      output_phases <- output_phases[,output_phases_selecter == 1, drop = FALSE]

      
    }
    else{i <- i + 1}
  }

  #Determine max/min for each species.
  if(length(output_phases)>0){
    range_other <- output_other[1:(hist_division+1),]
    
    
    
    for(i in 1:length(range_other)){
      #DETERMINE BOUNDARIES FOR HISTOGRAM
      temp_max<-max(output_other[,i])
      temp_min<-min(output_other[,i])
      if(sum(names(output_other)[i]==names_output_log)>0){
        output_phases<-output_phases[output_other[,i]>0,,drop = FALSE]
        output_other<-output_other[output_other[,i]>0,,drop = FALSE]
        output_other[,i] <- log10(output_other [,i])
        names(output_other)[i] <-paste(names(output_other)[i],'log10', sep = "")
        names(range_other)[i] <- names(output_other)[i]
        temp_max<-max(output_other[,i])
        temp_min<-min(output_other[,i])
      }
      range_other[,i] <- seq(0,1, by = 1/hist_division)*(temp_max-temp_min)+temp_min
      
      #CALCULATE HISTOGRAM, FOR COUNTS FOR NORMALIZATION
      hist_other <- hist(output_other[,i],range_other[,i],plot = FALSE)
      
      #SET VALUES FOR HISTOGRAM PLOT
      jpeg(paste(foldername,'/histogram_',names(output_other)[i],'_',toString(hist_division),'.jpg',sep = ""),width = 1800, height = 900, units= 'px', quality = 300)
      par(mfrow=c(length(output_phases)+1,1),mar=c(0.5,20,0,0),ps = 25,las = 2)
      
      #DEFINE OUTPUT
      #CREATE OUTPUTFOLDER
      output_hist_1    <-output_phases[1:hist_division,,drop = FALSE]
      names(output_hist_1) <- gsub("si_","",names(output_hist_1)) 
      output_hist_2    <-output_hist_1
      output_boxplot_1 <-output_hist_1[1:5,,drop=FALSE]
      output_boxplot_2 <-output_hist_1[1:5,,drop=FALSE]
      
      for(j in 1:length(output_phases)){
        #DEFINE OUTPUT PER PHASE: HIST1/BOXPLOT1 FOR NON-NORMALIZED VALUES
        hist_temp <- hist((output_other[,i]*output_phases[,j])[output_phases[,j]>0],range_other[,i],plot = FALSE)
        if(max(hist_temp$counts)>0){hist_temp$counts <- hist_temp$counts/max(hist_temp$counts)}
        
        
        if(j<length(output_phases)){plot(hist_temp, 
                                         axes = FALSE,
                                         main = NULL,
                                         ann = FALSE,
                                         col = rgb(1,0,0,0.4))}
        else{plot(hist_temp,
                  yaxt = 'n',
                  main = NULL,
                  ann = FALSE,
                  col = rgb(1,0,0,0.4))}
        
        mtext(names(output_hist_1)[j],side = 2, las = 1, ps = 12)
        output_hist_1[,j]<-hist_temp$counts
        boxtemp <- boxplot((output_other[,i]*output_phases[,j])[output_phases[,j]>0],plot = FALSE)
        output_boxplot_1[,j]<-boxtemp$stats
        output_boxplot_2[,j]<-boxtemp$stats
        
        #NORMALIZE OUTPUT in HIST2/BOXPLOT2
        hist_temp2 <- hist_temp
        hist_temp2$counts <- hist_temp$counts/hist_other$counts
        hist_temp2$counts[hist_other$counts == 0] <-0
        hist_temp2$counts <- hist_temp2$counts/max(hist_temp2$counts)
        plot(hist_temp2, axes = FALSE,main = NULL,col = rgb(0,0,1,0.4),add = TRUE)
        output_hist_2[,j] <- hist_temp2$counts
        hist_sum <- sum(hist_temp$counts)
        hist_temp_sum <- 0
        quart_1_ind <- 0
        quart_2_ind <- 0
        quart_3_ind <- 0
        for(k in 1:hist_division){
          hist_temp_sum <- hist_temp_sum+output_hist_2[k,j]
          if(quart_1_ind & hist_temp_sum > 0.25*hist_sum){
            output_boxplot_2[2,j]<-hist_temp$mids[k]
            quart_1_ind <- 1}
          if(quart_2_ind & hist_temp_sum > 0.50*hist_sum){
            output_boxplot_2[3,j]<-hist_temp$mids[k]
            quart_2_ind <- 1}
          if(quart_3_ind & hist_temp_sum > 0.75*hist_sum){
            output_boxplot_2[4,j]<-hist_temp$mids[k]
            quart_3_ind <- 1}
        }
        
      }
      dev.off()
      jpeg(paste(foldername,'/boxplot_raw_',names(output_other)[i],'.jpg',sep = ""),width = 1800, height = 900, units= 'px', quality = 300)
      par(mar=c(10,6,0.5,0.5),ps=16)
      boxplot(output_boxplot_1, las = 2,ylab = names(output_other)[i])
      dev.off()
      jpeg(paste(foldername,'/boxplot_corrected_',names(output_other)[i],'.jpg',sep = ""),width = 1800, height = 900, units= 'px', quality = 300)
      par(mar=c(10,6,0.5,0.5),ps=16)
      boxplot(output_boxplot_2, las = 2,ylab = names(output_other)[i])
      dev.off()
      write.csv(output_boxplot_1,paste(foldername,'/boxplot_raw_',names(output_other)[i], '.csv',sep=""))
      write.csv(output_boxplot_2,paste(foldername,'/boxplot_corrected_',names(output_other)[i], '.csv',sep=""))
      write.csv(output_hist_1,paste(foldername,'/histogram_counts_raw_',names(output_other)[i], '.csv',sep=""))
      write.csv(output_hist_2,paste(foldername,'/histogram_counts_corrected_',names(output_other)[i], '.csv',sep=""))
      
    }
    write.csv(range_other,paste(foldername,'/histogram_ranges',names(output_other)[i], '.csv',sep=""))
  }
  else{write('No minerals have been deposited - no output genereated',paste(foldername,'/no_data', '.txt',sep=""))}
}

solution_species_replacer <- function(inputfile_3,current_solution_species,solution_species_replacement_text){
  ssrt1<-'deleter#'
  inputfile_3<-gsub(paste(solution_species_replacement_text,ssrt1,sep=''),"",inputfile_3)
  l1<-length(current_solution_species)
  l2<-length(current_solution_species[,1])
  for(i in 1:l2){
    for(j in 1:4){
      ssrt_temp<-paste(solution_species_replacement_text,"TEXT_",as.character(i),'_',as.character(j),'_',ssrt1,sep='')
      inputfile_3<-gsub(ssrt_temp,"",inputfile_3)
    }
    for(j in 1:l1){
      ssrt_temp<-paste(solution_species_replacement_text,"TEXT_",as.character(i),'_',as.character(j),'#',sep="")
      inputfile_3<-gsub(ssrt_temp,as.character(current_solution_species[i,j]),inputfile_3)
    }
  }
  return(inputfile_3)
}




phases_replacer <-function(inputfile,current_phases,phases_replacement_type){
  inputfile<-gsub('#PHASES_deleter#',"",inputfile)
  for(i in 1:length(current_phases[,1])){
    current_replacement_start <- paste('#PHASES_TEXT_',toString(i),sep="")
    inputfile <- gsub(paste(current_replacement_start,"_deleter#",sep=""),"",inputfile)
    current_phase <- current_phases[i,]
    for(j in c(1,2,3,5)){
      inputfile <- gsub(paste(current_replacement_start,"_",toString(j),"#",sep=""),current_phase[1,j],inputfile)
    }
    if(current_phase[1,4]=="No"){inputfile<- gsub(paste(current_replacement_start,"_4#",sep=""),"-no_check",inputfile)}
    if(phases_replacement_type == "even"){
      inputfile <- gsub(paste(current_replacement_start,"_6#",sep=""),toString(current_phase[1,6]+(2*rand(1)-1)*current_phase[1,7]),inputfile)
      for(j in seq(8,28,by=2)){
        inputfile <- gsub(paste(current_replacement_start,"_",toString(j),"_deleter#",sep=""),"",inputfile) 
        inputfile <- gsub(paste(current_replacement_start,"_",toString(j),"#",sep=""),toString(current_phase[1,j]+(2*rand(1)-1)*current_phase[1,j+1]),inputfile) 
      }
    }
    else{if(phases_replacement_type == "normal"){
      inputfile <- gsub(paste(current_replacement_start,"_6#",sep=""),toString(current_phase[1,6]+randn(1)*current_phase[1,7]),inputfile)
      for(j in seq(8,28,by=2)){
        inputfile <- gsub(paste(current_replacement_start,"_",toString(j),"_deleter#",sep=""),"",inputfile) 
        inputfile <- gsub(paste(current_replacement_start,"_",toString(j),"#",sep=""),toString(current_phase[1,j]+randn(1)*current_phase[1,j+1]),inputfile) 
      }
    }
    }
  }
  return(inputfile)
}



collide_si_columns <- function(output,column_names_for_colliding){
  output_new<-data.frame(-999*ones(length(output[,1]),length(column_names_for_colliding)))
  names(output_new)<-names(column_names_for_colliding)
  output_names <- names(output)
  output_si_numbers<-c(1:length(output_names))[(output_names==gsub('si_','',output_names))==FALSE]
  output_non_si_numbers<-c(1:length(output_names))[(output_names==gsub('si_','',output_names))==TRUE]
  output_si_old<-output[,output_si_numbers]
  output_deleter<-c(1:length(output_si_old))
  output_names<-names(output_si_old)
  for(i in 1:length(output_si_old)){
    current_name<-output_names[i]
    for(j in 1:length(column_names_for_colliding)){
      potential_colliding_names<-column_names_for_colliding[[j]]
      if(sum(current_name==potential_colliding_names)>0){
        output_deleter[i] <- 0
        output_new[,j]<-output_new[,j]*abs(output_si_old[,i])
      }
      if(current_name==names(column_names_for_colliding)[j]){
        output_deleter[i] <- 0
        output_new[,j]<-output_new[,j]*abs(output_si_old[,i])
      }
      }
    }

  output_new<-cbind(output_new,output_si_old[,output_deleter>0,drop = FALSE])
  output_new[output_new<0]<--999
  output_non_si_numbers_1<-output_non_si_numbers[output_non_si_numbers<min(output_si_numbers)]
  output_non_si_numbers_2<-output_non_si_numbers[output_non_si_numbers>max(output_non_si_numbers_1)]
   output_new<-cbind(output[,output_non_si_numbers_1,drop=FALSE],
                output_new,
                output[,output_non_si_numbers_2,drop=FALSE])
  return(output_new)
  }



# collide_si_columns <- function(output,column_names_for_colliding){
#   for(i in 1:length(column_names_for_colliding)){
#     new_column_name <- names(column_names_for_colliding)[i]
#     eval(parse(text=(paste('old_column_names <- column_names_for_colliding$',new_column_name))))
#     if(sum(names(column_names_for_colliding)[i]==names(output))>0){
#       first_column<-c(1:length(output))[names(column_names_for_colliding)[i]==names(output)]}
#     else{
#       first_column<-c(1:length(output))[names(output)==old_column_names[1]]
#       names(output)[first_column]<-new_column_name}
#     new_column <- output[,first_column]
#     delete_column_numbers<-ones(1,length(output))
#     for(j in 1:length(old_column_names)){
#       potential_deleter <- c(1:length(output))[names(output)==old_column_names[j]]
#       if(isempty(potential_deleter)==FALSE){
#         new_column <- new_column*abs(output[,names(output)==old_column_names[j]])
#         if((potential_deleter == first_column) == FALSE){delete_column_numbers[potential_deleter]<-0
#         }
#       }
#     }
#     new_column[new_column<0]<- -999
#     output[,first_column]<-new_column
#     output<-output[,delete_column_numbers==1]
#   }
#   return(output)
# }


to_Column_name<-function(text){
  text<-gsub('\\-','.',text)
  text<-gsub('\\(','.',text)
  text<-gsub('\\)','.',text)
  text<-gsub(' ','.',text)
  text<-gsub(':','.',text)
  return(text)
}

si_names <- function(input){
  names_input_2<-gsub("si_","",names(input))
  names_input_2 <- names_input_2[(names_input_2==names(input)) == FALSE]
  return(names_input_2)
}


#for(cs in 1:length(solutions_temp$Na)){
run_PHREEQC_per_solution <-  function(cs,
                                      solutions_temp,
                                      inputfile_2,
                                      sol_Na_text,
                                      sol_K_text,
                                      sol_Ca_text,
                                      sol_Mg_text,
                                      sol_C_text,
                                      sol_Alkalinity_text,
                                      sol_S_text,
                                      sol_Cl_text,
                                      sol_Fe_text,
                                      sol_F_text,
                                      sol_Si_text,
                                      sol_pH_text,
                                      sol_pe_text,
                                      temperatures,
                                      reaction_steps_all,
                                      reaction_pressure_range,
                                      reaction_steps_text,
                                      temperature_text,
                                      reaction_pressure_text, 
                                      samplesize,
                                      pe_range,
                                      limit_nr_tries,
                                      delete_knobs2_text,
                                      convtext2,
                                      tol_text2,
                                      ssz_text2,
                                      pessz_text2,
                                      conv_range,
                                      tol_range,
                                      ssz_range,
                                      pessz_range,
                                      current_phases,
                                      phases_replacement_type,
                                      phases_exist,
                                      O_variability,
                                      O_variability_type,
                                      O_replacement_text,
                                      O_quantity,
                                      solution_species_replacement_text,
                                      current_solution_species){
  
  if(phases_exist){inputfile_3<-phases_replacer(inputfile_2,current_phases,phases_replacement_type)}
  else{inputfile_3<-inputfile_2}
  current_solution <- solutions_temp[cs,]
  #REPLACE SOLUTION_SPECIES
  if(length(current_solution_species)>0){
    inputfile_3<-solution_species_replacer(inputfile_3,current_solution_species,solution_species_replacement_text)
  }
  
  #REPLACE OXYGEN CONTENT
  switch(O_variability_type,
         'even' = {inputfile_3 <- gsub(O_replacement_text,toString((rand()-0.5)/100*O_quantity*O_variability+O_quantity),inputfile_3)},
         'normal' = {inputfile_3 <- gsub(O_replacement_text,toString((randn())/100*O_quantity*O_variability+O_quantity),inputfile_3)})

  #REPLACE SOLUTION COMPOSITION
  inputfile_3 <- gsub(sol_Na_text,toString(current_solution[1]),inputfile_3)
  inputfile_3 <- gsub(sol_K_text,toString(current_solution[2]),inputfile_3)
  inputfile_3 <- gsub(sol_Ca_text,toString(current_solution[3]),inputfile_3)
  inputfile_3 <- gsub(sol_Mg_text,toString(current_solution[4]),inputfile_3)
  inputfile_3 <- gsub(sol_C_text,toString(current_solution[5]),inputfile_3)
  inputfile_3 <- gsub(sol_Alkalinity_text,toString(current_solution[6]),inputfile_3)
  inputfile_3 <- gsub(sol_S_text,toString(current_solution[7]),inputfile_3)
  inputfile_3 <- gsub(sol_Cl_text,toString(current_solution[8]),inputfile_3)
  inputfile_3 <- gsub(sol_Fe_text,toString(current_solution[9]),inputfile_3)
  inputfile_3 <- gsub(sol_F_text,toString(current_solution[10]),inputfile_3)
  inputfile_3 <- gsub(sol_Si_text,toString(current_solution[11]),inputfile_3)
  inputfile_3 <- gsub(sol_pH_text,toString(current_solution[12]),inputfile_3)
  inputfile_3 <- gsub(sol_pe_text,toString(pe_range[1]+rand(1)*(pe_range[2]-pe_range[1])),inputfile_3)
  #SET TEMPERATURE/PRESSURE/REACTION STEPS
  temperatures_temp <- temperatures[sample(c(1:length(temperatures)),samplesize)]
  temperatures_temp <- gsub(',',' ',toString(temperatures_temp))
  reaction_steps <- reaction_steps_all[sample(c(1:length(reaction_steps_all)),samplesize)]
  reaction_steps <- gsub(',',' ',toString(reaction_steps))
  reaction_pressure <-character()
  for(rp in 1:samplesize){
    reaction_pressure <- paste(reaction_pressure,toString(round(reaction_pressure_range[1]+rand(1)*(reaction_pressure_range[2]-reaction_pressure_range[1]))))}
  
  #REPLACE REACTION STEPS
  inputfile_3<-gsub(reaction_steps_text,reaction_steps,inputfile_3)
  #REPLACE TEMPERATURE
  inputfile_3<-gsub(temperature_text,temperatures_temp,inputfile_3)
  #REPLACE PRESSURE
  inputfile_3<-gsub(reaction_pressure_text,reaction_pressure,inputfile_3)
  #RUN INPUT
  nr_tries<-0
  convergence_error<-1
  input_temp <- inputfile_3
  write(inputfile_3,'C:/Users/Jitse/Documents/inputfile_thermoddem.pqi')
  while(convergence_error == 1 & nr_tries < limit_nr_tries){
    try({
      phrRunString(input_temp)
      convergence_error<-0}, silent = TRUE)
    if(convergence_error){
      print(phrGetErrorStrings())
      nr_tries<-nr_tries+1
      #RESET KNOBS IN CASE OF CONVERGENCE ERROR
      input_temp <- gsub(delete_knobs2_text,"",inputfile_3)
      input_temp <- gsub(convtext2,toString(conv_range[1]*10^(rand(1)*log10(conv_range[2]/conv_range[1]))),input_temp)
      input_temp <- gsub(tol_text2,toString(tol_range[1]*10^(rand(1)*log10(tol_range[2]/tol_range[1]))),input_temp)
      input_temp <- gsub(ssz_text2,toString(ssz_range[1]+rand(1)*(ssz_range[2]-ssz_range[1])),input_temp)
      input_temp <- gsub(pessz_text2,toString(pessz_range[1]+rand(1)*(pessz_range[2]-pessz_range[1])),input_temp)
    }
  }
  write(input_temp,file = 'C:/Users/Jitse/Documents/test_output5.txt')
  #COLLECT OUtPUT
  so <- phrGetSelectedOutput()
  so_temp <- so$n1
  if(length(so_temp[,1])>samplesize){
    so_temp <- so_temp[(length(so_temp[,1])-(samplesize-1)):length(so_temp[,1]),]}
  so_temp[is.na(so_temp)] <- -999.999
  so_temp$Solution <- as.numeric(row.names(solutions_temp)[cs])
  names(current_solution)<-paste("Initial_",names(current_solution),sep="")
  for(csp in 1:length(current_solution)){
    eval(parse(text=(paste("so_temp$",names(current_solution)[csp],"<-current_solution$",names(current_solution)[csp],sep=""))))
  }
  return(so_temp)
  #return(phrGetErrorStrings())
  }
#}
  


recalculate_oxygen_per_db<-function(current_reaction,current_database,ignoredspecies_per_database,ignoredspecies_numbers_O_per_database){
  eval(parse(text = paste('ignored_species <-ignoredspecies_per_database$',current_database,sep="")))
  eval(parse(text = paste('recalculated_oxygen <-ignoredspecies_numbers_O_per_database$',current_database,sep="")))
  o_nr<-c(1:length(current_reaction))[names(current_reaction)=='O']
  for(i in 1:length(ignored_species)){
    r_nr<-c(1:length(current_reaction))[names(current_reaction)==ignored_species[i]]
    if(length(r_nr)>0){
      current_reaction[o_nr]<-current_reaction[o_nr]-recalculated_oxygen[i]*current_reaction[r_nr]
      current_reaction[r_nr]<-0
    }}
  current_reaction<-current_reaction[,current_reaction>0]
  return(current_reaction)}

