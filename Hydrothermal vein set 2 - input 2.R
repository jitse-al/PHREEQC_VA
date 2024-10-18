#SET WORKING DIRECTORY
working_directory <- 'C:/Users/Jitse/Documents/PHREEQC_R_Input'

#SET CURRENT IMPACT STRUCTURE
impact_structure <- 'Vista Alegre'

#SET SITES TO BE INVESTIGATED
used_sites       <- c('Va_zeolites_2')
used_databases   <- c('llnl','thermoddem','YMP','Soltherm_O','sit')

#SET KNOBS
iterations            <- 300
convergence_tolerance <- 5e-08
tolerance             <- 5e-13
step_size             <- 50
pe_step_size          <- 4

#SET pe RANGE
pe_range <- c(0, 8)

#SET PARAMS FOR KNOBS IN CASE OF CONVERGENCE ERRORS  
limit_nr_tries<-50

conv_range  <- c(1e-09, 5e-06)
tol_range   <- c(1e-14, 5e-11)
ssz_range   <- c(30   , 90   )
pessz_range <- c(3    , 13   )

#SET OXYGEN VARIABILITY (IN PERCENT, PER 3 O-ATOMS). EITHER "even" or "normal" distribution
O_variability <- 5
O_variability_type <- 'normal'

#SET REACTION PRESSURE
reaction_pressure_range <- c(70,110)

#SET TEMPERATURES AND REACTION STEPS
temperatures <- seq(0,300,by = 0.01)
reaction_steps_max <- 10000
reaction_steps_all <- (reaction_steps_max^(1/50000))^(c(1:50000))

#SELECT NUMBER OF REACTION STEPS/TEMPERATURES/pressure
samplesize <- 5
sample_solutions <- 5

#SELECT RANDOMIZATION TYPE OF PHASES PARAMETERS. EITHER "even" OR "normal"
phases_replacement_type <- "even"

#SET OUTPUTFOLDER
outputfolder <- paste(working_directory,'output', sep = "/")
lowest_output <- 'off'

#SET SUMMARY LEVELS. 1 = highest, 3 = lowest
summary_levels<-c(1,2)

#SET PARAMATERS OUTPUT: names_Output_log are converted to logarithmic
names_output_log <-c("reaction","C.4..mol.kgw.","Ca.mol.kgw.","Cl.mol.kgw.","F.mol.kgw.","Fe.mol.kgw.","K.mol.kgw.","Mg.mol.kgw.","Na.mol.kgw.","S.6..mol.kgw.","Si.mol.kgw.","Initial_Na","Initial_K","Initial_Ca","Initial_Mg","Initial_C.4.","Initial_Alkalinity_CO3_2","Initial_S.6._SO4_2","Initial_Cl","Initial_Fe","Initial_F","Initial_Si_SiO2","Initial_pH")
names_output_normal <- c("Alk.eq.kgw.","Alkalinity.mol.kgw.","pH","temp.C.","Solution")
hist_division = 100
if(hist_division>samplesize*sample_solutions){hist_division<-samplesize*sample_solutions}


column_names_for_colliding <- list(si_Calcite =c("si_Calcite","si_calcite"),
                                   si_Analcime =c("si_Analcime","si_analcite","si_Analcime(Si-Al_is_2.0)-Zeo19","si_Analcime(Si-Al_is_2.5)-Zeo19"),
                                   si_Thomsonite =c("si_Thomsonite-Zeo19"),
                                   si_Levyne_Phillipsite =c("si_Phillipsite","si_Phillipsite(K)-Zeo19","si_Phillipsite(KNa)-Zeo19","si_Phillipsite(Na)","si_Phillipsite(Na)-Zeo19","si_Phillipsite_Na"),
                                   si_Stilbite =c("si_Stilbite","si_stilbite","si_Stilbite(CaNa)-Zeo19"),
                                   si_Chabazite =c("si_Chabazite","si_Chabazite-Zeo19"),
                                   si_Heulandite =c("si_Heulandite","si_heulandi","si_Heulandite","si_Heulandite(Ca)","si_Heulandite(Ca)-Zeo19","si_Heulandite(CaKNa)-Zeo19","si_Heulandite_Ca"),
                                   si_Clay_Mg =c("si_celadoni","si_Celadonite","si_Celadonite-Mg","si_Edenite(alpha)","si_HMontmorillonite-HCMg","si_HSaponite-Ca","si_HSaponite-FeCa","si_HSaponite-FeK","si_HSaponite-FeMg","si_HSaponite-FeNa","si_HSaponite-K","si_HSaponite-Mg","si_HSaponite-Na","si_HVermiculite-Ca","si_Montmorillonite(HcMg)","si_Montmorillonite-HCMg","si_Saponite(Ca)","si_Saponite(FeCa)","si_Saponite(FeK)","si_Saponite(FeMg)","si_Saponite(FeNa)","si_Saponite(K)","si_Saponite(Mg)","si_Saponite(Na)","si_Saponite(SapCa)","si_Saponite_SapCa","si_Saponite_SapCa(4.151H2O)","si_Saponite-Ca","si_Saponite-Cs","si_Saponite-FeCa","si_Saponite-FeK","si_Saponite-FeMg","si_Saponite-FeNa","si_Saponite-H","si_Saponite-K","si_Saponite-Mg","si_Saponite-Na","si_Smectite_Reykjanes","si_Smectite-high-Fe-Mg","si_Smectite-low-Fe-Mg","si_Talc","si_talc","si_Vermiculite(Ca)","si_Vermiculite_SO","si_Vermiculite-Ca","si_VermiculiteSO"),
                                   si_80b_2_6 =c("si_celadoni","si_Edenite(alpha)","si_HSaponite-FeCa","si_HSaponite-FeK","si_HSaponite-FeMg","si_HSaponite-FeNa","si_HVermiculite-Ca","si_phlog-Na","si_phlogopi","si_Ripidolite-14A","si_Ripidolite-7A","si_Saponite(FeCa)","si_Saponite(FeK)","si_Saponite(FeMg)","si_Saponite(FeNa)","si_Saponite(SapCa)","si_Saponite_SapCa","si_Saponite_SapCa(4.151H2O)","si_Saponite-FeCa","si_Saponite-FeK","si_Saponite-FeMg","si_Saponite-FeNa","si_sepiolit","si_Smectite_Reykjanes","si_Smectite-high-Fe-Mg","si_talc-Tsc","si_Vermiculite(Ca)","si_Vermiculite_SO","si_Vermiculite-Ca","si_VermiculiteSO"),
                                   si_80b_2_7 =c("si_celad-Fe","si_celadoni","si_Celadonite","si_Celadonite-Mg","si_HSaponite-FeCa","si_HSaponite-FeK","si_HSaponite-FeMg","si_HSaponite-FeNa","si_Saponite(FeCa)","si_Saponite(FeK)","si_Saponite(FeMg)","si_Saponite(FeNa)","si_Saponite-FeCa","si_Saponite-FeK","si_Saponite-FeMg","si_Saponite-FeNa","si_Smectite_Reykjanes","si_Smectite-high-Fe-Mg","si_Smectite-low-Fe-Mg","si_talc-Tsc"),
                                   si_80b_2_5 =c("si_celad-Fe","si_celadoni","si_Celadonite","si_Celadonite(Fe)","si_Celadonite-Fe","si_Celadonite-Mg","si_Ferroaluminoceladonite","si_Glauconite","si_HNontronite-K","si_HSaponite-FeCa","si_HSaponite-FeK","si_HSaponite-FeMg","si_HSaponite-FeNa","si_Nontronite(K)","si_Nontronite-Cs","si_Nontronite-K","si_Saponite(FeCa)","si_Saponite(FeK)","si_Saponite(FeMg)","si_Saponite(FeNa)","si_Saponite-FeCa","si_Saponite-FeK","si_Saponite-FeMg","si_Saponite-FeNa","si_Smectite-high-Fe-Mg","si_Smectite-low-Fe-Mg"),
                                   si_80a_3_3_7 =c("si_celadoni","si_HMontmorillonite-HCCa","si_HMontmorillonite-HCK","si_HMontmorillonite-HCMg","si_HMontmorillonite-HCNa","si_montm-Ca","si_montm-K","si_montm-Na","si_Montmorillonite(HcCa)","si_Montmorillonite(HcK)","si_Montmorillonite(HcMg)","si_Montmorillonite(HcNa)","si_Montmorillonite-HCCa","si_Montmorillonite-HCK","si_Montmorillonite-HCMg","si_Montmorillonite-HCNa","si_Smectite(MX80)","si_Smectite(MX80:3.989H2O)","si_Smectite(MX80:5.189H2O)","si_Smectite_MX80(3.989H2O)","si_Smectite_MX80(5.189H2O)","si_Smectite_Reykjanes","si_Smectite-high-Fe-Mg","si_Smectite-low-Fe-Mg","si_SmectiteMX80"))



#SET SPECIES TO BE IGNORED PER REACTION, I.E. SPECIES THAT ARE NOT PRESENT IN THE DATABASE. OXYGEN WILL BE RECALCULATED.
ignoredspecies_per_database = list(llnl       = c(''),
                                   thermoddem = c(''),
                                   YMP        = c('Ti'),
                                   Soltherm_O = c(''),
                                   sit        = c('Ti'))
ignoredspecies_numbers_O_per_database = list(llnl       = c(0),
                                             thermoddem = c(0),
                                             YMP        = c(2),
                                             Soltherm_O = c(0),
                                             sit        = c(2))

#####LOADING STARTS HERE
{#Load phreeqc
library(phreeqc)
  library(pracma)
  library(plyr)
  library(parallel)
  
  
  
  #LOAD SUPPORTING FUNCTIONS
  supporting_functions_folder <- working_directory
  supporting_functions_filename <- 'supporting_functions.R'
  source(paste(supporting_functions_folder,supporting_functions_filename,sep="/"))
  clear(c('supporting_functions_folder','supporting_functions_filename'))
  
  #REWRITE COLUMN NAMES
  for(i in 1:length(column_names_for_colliding)){
    for(j in 1:length(column_names_for_colliding[[i]]))
      column_names_for_colliding[[i]][j]<-to_Column_name(column_names_for_colliding[[i]][j])
  }
#Load databases
database_location <-paste(working_directory,'database', sep = "/")
database_names <-c('llnl.dat','sit.dat','thermoddemv1.10_11dec2014.dat',
                   'tk1-ympR5_output_Alk_LLNLGAMMA3.dat',
                   'tk-slt.h06_output_O_Alk_LLNLGAMMA3.dat',
                   'tk-slt.h06_output_S_Alk_LLNLGAMMA3.dat')
database_names2<-c('llnl','sit','thermoddem','YMP','Soltherm_O','Soltherm_S')
databases <-vector(mode='list',length=length(database_names2))
names(databases)<-database_names2
for(i in c(1:length(database_names))){
  executionstring=paste('databases$',database_names2[i],sep="")
  database_temp<-scan(paste(database_location,database_names[i], sep="/"),what='character', sep='\n')
  executionstring = paste(executionstring,'<-','database_temp',sep="")
  eval(parse(text = executionstring))
}
clear(c('executionstring','database_temp','database_names','database_location'))
#LOAD INPUTFILE
inputfile_location <- working_directory
inputfile_name <- 'inputfile.pqi'
inputfile <- scan(paste(inputfile_location,inputfile_name, sep="/"),what='character', sep='\n')
clear(c('inputfile_location','inputfile_name'))
#LOAD SOLUTION INPUT (LIST)
solutions_location <- paste(working_directory,'Solutions', sep = "/")
solutions_filename <- 'Va_zeolites_2.csv'
solutions <-read.csv(paste(solutions_location,solutions_filename, sep="/"), sep='\t',header = TRUE)
clear(c('solutions_location','solutions_filename'))
#LOAD EQUILIBRIUM PHASES INPUTS
equilibrium_phases_foldername <-paste(working_directory,'Equilibrium Phases',impact_structure, sep = "/")
equilibrium_foldernames <- dir(equilibrium_phases_foldername)
equilibrium_phases_all <- vector(mode='list',length=length(equilibrium_foldernames))
names(equilibrium_phases_all)<-equilibrium_foldernames
for(i in equilibrium_foldernames){
  equilibrium_filenames <- dir(paste(equilibrium_phases_foldername,i,sep="/"))
  equilibrium_phases <-vector(mode='list',length=length(equilibrium_filenames))
  if(length(equilibrium_filenames>0)){
    names(equilibrium_phases)<-paste('File',equilibrium_filenames,sep="")
    for(j in equilibrium_filenames){
      executionstring <- paste('equilibrium_phases$File',j,' <-',sep="")
      temp_phase <- read.csv2(paste(equilibrium_phases_foldername,i,j,sep='/'),header = TRUE,sep=',')
      eval(parse(text= paste(executionstring,'temp_phase',sep="") ))}
  }
  eval(parse(text = paste('equilibrium_phases_all$',i,'<- equilibrium_phases',sep="")))
}
clear(c('equilibrium_phases_foldername','equilibrium_foldernames','equilibrium_filenames','equilibrium_phases','executionstring','temp_phase'))
##LOAD REACTION STEPS AND TEMPERATURES
#temperature_reaction_file_location <-'C:/Users/Jitse/Documents/PHREEQC_R_Input/Temperatures and Reaction Amounts'
#temperature_reaction_file_name <- 'Temperatures_reactions_LLNL_SOLTHERM_THERMODDEM_YMP.csv'
#temperature_reaction <- read.csv(paste(temperature_reaction_file_location,temperature_reaction_file_name, sep="/"), sep='\t',header = FALSE)
#names(temperature_reaction) <- c('Temperature','Reaction.Steps')
#LOAD REACTION TYPES
reaction_type_foldername <- paste(working_directory,'Reaction Types',impact_structure, sep = "/")
reaction_type_filenames <- dir(reaction_type_foldername)
reaction_types <-vector(mode='list',length=length(reaction_type_filenames))
names(reaction_types)<-paste('Reaction.Type.',reaction_type_filenames,sep="")
for(i in reaction_type_filenames){
  reaction_type_temp <-read.csv(paste(reaction_type_foldername,i,sep = '/'),sep = '\t')
  eval(parse(text=paste('reaction_types$Reaction.Type.',i,'<- reaction_type_temp',sep = "")))
}
reaction_randomizer<-'on'
nr_randomized_reactions<-10

clear(c('reaction_type_foldername','reaction_type_filenames','reaction_type_temp'))
#LOAD PHASES
phases_foldername <- paste(working_directory,'Phases',impact_structure, sep = "/")
phases_foldernames <- dir(phases_foldername)
phases_all <- vector(mode='list',length=length(phases_foldernames))
names(phases_all)<-phases_foldernames
for(i in phases_foldernames){
  phases_filenames <- dir(paste(phases_foldername,i,sep="/"))
  phases <-vector(mode='list',length=length(phases_filenames))
  if(length(phases_filenames>0)){
    names(phases)<-paste('File',phases_filenames,sep="")
    for(j in phases_filenames){
      executionstring <- paste('phases$File',j,' <-',sep="")
      temp_phase <- read.csv(paste(phases_foldername,i,j,sep='/'),header = TRUE,sep=',')
      eval(parse(text= paste(executionstring,'temp_phase',sep="") ))}
  }
  eval(parse(text = paste('phases_all$',i,'<- phases',sep="")))
}
clear(c('phases_foldername','phases_foldernames','phases_filenames','phases','executionstring','temp_phase'))

#LOAD SOLUTION_SPECIES
solution_species_foldername <- paste(working_directory,'Solution Species',impact_structure, sep = "/")
solution_species_foldernames <- dir(solution_species_foldername)
solution_species_all <- vector(mode='list',length=length(solution_species_foldernames))
names(solution_species_all)<-solution_species_foldernames
for(i in solution_species_foldernames){
  solution_species_filenames <- dir(paste(solution_species_foldername,i,sep="/"))
  solution_species <-vector(mode='list',length=length(solution_species_filenames))
  if(length(solution_species_filenames>0)){
    names(solution_species)<-paste('File',solution_species_filenames,sep="")
    for(j in solution_species_filenames){
      executionstring <- paste('solution_species$File',j,' <-',sep="")
      temp_phase <- read.csv(paste(solution_species_foldername,i,j,sep='/'),header = TRUE,sep=',')
      eval(parse(text= paste(executionstring,'temp_phase',sep="") ))}
  }
  eval(parse(text = paste('solution_species_all$',i,'<- solution_species',sep="")))
}
clear(c('solution_species_foldername','solution_species_foldernames','solution_species_filenames','solution_species','executionstring','temp_phase'))
}


#DEFINE TEXT REPLACEMENT
{
itertext   <- '####iterations####'
convtext   <- '####convergence_tolerance####'
tol_text   <- '####tolerance####'
ssz_text   <- '####step_size####'
pessz_text <- '####pe_step_size####'
delete_knobs2_text <- '####KNOBS 2 INPUT ####'
convtext2           <- '####convergence_tolerance_2####'
tol_text2           <- '####tolerance_2####'
ssz_text2           <- '####step_size_2####'
pessz_text2         <- '####pe_step_size_2####'

sol_pe_text           <- '####sol_pe_text####'
sol_pH_text           <- '####sol_pH_text####'
sol_Alkalinity_text   <- '####sol_Alkalinity_text####'
sol_C_text            <- '####sol_C_text####'
sol_Ca_text           <- '####sol_Ca_text####'
sol_Cl_text           <- '####sol_Cl_text####'
sol_F_text            <- '####sol_F_text####'
sol_Fe_text           <- '####sol_Fe_text####'
sol_K_text            <- '####sol_K_text####'
sol_Mg_text           <- '####sol_Mg_text####'
sol_Na_text           <- '####sol_Na_text####'
sol_S_text            <- '####sol_S_text####'
sol_Si_text           <- '####sol_Si_text####'
replacement_text_eq_phases_start   <- '#EQUI_PHASE_'
replacement_text_eq_phases_end   <- '#'
replacement_text_re_types_start   <- '#REACTION_COMPONENT '
replacement_text_re_types_end   <- '#'
temperature_text <- '####TEMPERATURE####'
reaction_steps_text <- '####REACTION STEPS####'
reaction_pressure_text <- '####reaction_pressure####'
solution_species_replacement_text<-'#SOLUTION_SPECIES_'
}

#PREPARING AND RUNNING OF INPUT
for(current_site in used_sites){
  if(exists(paste('Reaction.Type.',toString(current_site),'.csv',sep=""),where = reaction_types)){
    #LOAD USED REACTION SET
    eval(parse(text = paste('used_reactions <- reaction_types$Reaction.Type.',current_site,'.csv',sep="")))
    if(reaction_randomizer == 'on'){
      used_reactions_2<-used_reactions
    }
    outputvector=vector(mode = 'list')
    for(current_database in used_databases){
      if(current_database == 'sit'){temperatures <- temperatures/2}
      if(exists(paste('File',toString(current_site),'.csv',sep=""),where = eval(parse(text = paste("equilibrium_phases_all$",current_database,sep=""))))){
        if(exists(current_database,where = phases_all)){phases_exist<-exists(paste('File',toString(current_site),'.csv',sep=""),where = eval(parse(text = paste("phases_all$",current_database,sep=""))))}
        else{phases_exist<-FALSE}
        #LOAD DATABASE FOR PHREEQC
        phrLoadDatabaseString(eval(parse(text = paste('databases$',current_database,sep=""))))
        #LOAD USED PHASES
        if(phases_exist){
          eval(parse(text = paste('used_phases <- phases_all$', current_database,sep="")))
          eval(parse(text = paste('current_phases <- used_phases$File', current_site,'.csv',sep="")))
        }
        else{current_phases = NULL}
        #LOAD USED_SOLUTIONS_species
        if(exists(current_database,where=solution_species_all)){
          eval(parse(text = paste('current_solution_species <- solution_species_all$', current_database, sep='')))
          eval(parse(text = paste('current_solution_species <- current_solution_species$File', current_site,'.csv',sep="")))}
        else{current_solution_species = NULL}
        #LOAD USED EQUILIBRIUM PHASES
        eval(parse(text = paste('used_equilibrium_phases <- equilibrium_phases_all$', current_database,sep="")))
        eval(parse(text = paste('used_equilibrium_phases <- used_equilibrium_phases$File', current_site,'.csv',sep="")))
        if(reaction_randomizer == 'on'){
          used_reactions<-used_reactions_2[sample(nr_randomized_reactions, 1:length(used_reactions_2[,1])),]
        }
        for(cr in 1:length(used_reactions[,1])){
          current_reaction <- used_reactions[cr,]
          current_reaction <- recalculate_oxygen_per_db(current_reaction,current_database,ignoredspecies_per_database,ignoredspecies_numbers_O_per_database)
          inputfile_1 <- inputfile
          #REPLACE REACTION STRINGS IN INPUTFILE --> INPUTFILE_1
          for(ccmp in 1:length(current_reaction)){
            if(names(current_reaction)[ccmp]=='O'){
            O_replacement_text<- paste(replacement_text_re_types_start, toString(ccmp),replacement_text_re_types_end,sep="")
            O_quantity<-current_reaction[ccmp]}
            else{inputfile_1 <- gsub(paste(replacement_text_re_types_start, toString(ccmp),replacement_text_re_types_end,sep=""),
                                     paste(names(current_reaction)[ccmp], toString(current_reaction[ccmp]),sep = " "),
                                     inputfile_1)}
            
          }
          #REPLACE INITIAL KNOBS
          inputfile_1<-gsub(itertext,iterations,inputfile_1)
          inputfile_1<-gsub(convtext,convergence_tolerance,inputfile_1)
          inputfile_1<-gsub(tol_text,tolerance,inputfile_1)
          inputfile_1<-gsub(ssz_text,step_size,inputfile_1)
          inputfile_1<-gsub(pessz_text,pe_step_size,inputfile_1)
          for(ce in 1:length(used_equilibrium_phases[1,])){
            current_equilibrium_phases <- used_equilibrium_phases[,ce]
            current_equilibrium_phases<-current_equilibrium_phases[(current_equilibrium_phases=="")==FALSE]
            #REPLACE EQUILIBRIUM PHASES --> INPUTFILE_2
            inputfile_2 <- inputfile_1
            for(cep in 1:length(current_equilibrium_phases)){
              inputfile_2<-gsub(paste(replacement_text_eq_phases_start, toString(cep),replacement_text_eq_phases_end,sep=""),
                                current_equilibrium_phases[cep],
                                inputfile_2)
            }
            output <-data.frame();
            solutions_temp <- solutions[sample(c(1:length(solutions$Na)),sample_solutions),]
            cs <- c(1:length(solutions_temp$Na))
            
            cluster_temp<-makeCluster(6,type = "PSOCK")
            clusterEvalQ(cluster_temp,{
              library(phreeqc)
              library(pracma)
              library(plyr)
              phrGetErrorStringsOn()})
            clusterExport(cluster_temp, c("run_PHREEQC_per_solution", "solutions_temp","inputfile_2","sol_Na_text","sol_K_text","sol_Ca_text","sol_Mg_text","sol_C_text","sol_Alkalinity_text","sol_S_text","sol_Cl_text","sol_Fe_text","sol_F_text","sol_Si_text","sol_pH_text","sol_pe_text","temperatures","reaction_steps_all","reaction_pressure_range","reaction_steps_text","temperature_text","reaction_pressure_text","samplesize","pe_range","limit_nr_tries","delete_knobs2_text","convtext2","tol_text2","ssz_text2","pessz_text2","conv_range","tol_range","ssz_range","pessz_range","databases","current_database","current_phases","phases_replacement_type","phases_replacer","O_variability","O_variability_type","O_replacement_text","O_quantity","solution_species_replacement_text","current_solution_species","solution_species_replacer"))
            clusterEvalQ(cluster_temp,phrLoadDatabaseString(eval(parse(text = paste('databases$',current_database,sep="")))))
            so_temp <- clusterApply(cluster_temp, cs, run_PHREEQC_per_solution,solutions_temp,
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
                                    current_solution_species)
            stopCluster(cluster_temp)
            so_temp
            
            
            for(cs in 1:length(solutions_temp$Na)){
              output <- suppressMessages(join(output, so_temp[[cs]], type = "full"))
            }
            #WRITE OUTPUT TO FILE
            current_output_site_folder      <- paste(outputfolder,current_site,sep='/')
            current_output_equiphase_folder <- paste(current_output_site_folder,names(used_equilibrium_phases)[ce],sep='/')
            current_database_folder         <- paste(current_output_equiphase_folder,current_database,sep='/')
            current_reaction_type_folder    <- paste(current_database_folder,paste('Reaction_Type_',toString(cr),sep=""),sep="/")
            
            if(dir.exists(outputfolder) == FALSE){dir.create(outputfolder)}
            if(dir.exists(current_output_site_folder) == FALSE){dir.create(current_output_site_folder)}
            if(dir.exists(current_output_equiphase_folder) == FALSE){dir.create(current_output_equiphase_folder)}
            if(dir.exists(current_database_folder) == FALSE){dir.create(current_database_folder)}
            if(dir.exists(current_reaction_type_folder) == FALSE){dir.create(current_reaction_type_folder)}
            write.csv(output,paste(current_reaction_type_folder,'/output.csv',sep=""))
            
            output_phases_start <-c(1:length(output))[names(output)==gsub(' ','.',gsub('\\-','.',gsub('\\)','.',gsub('\\(','.',paste("si_",current_equilibrium_phases[1],sep="")))))]
            output_phases_end <-output_phases_start+length(current_equilibrium_phases)-1
            output[,output_phases_start:output_phases_end][output[,output_phases_start:output_phases_end]>0] <- 0
            output[,output_phases_start:output_phases_end][output[,output_phases_start:output_phases_end]<0] <- -999
            if(lowest_output == 'on'){
              export_output_hist_boxplots(output,
                                          hist_division,
                                          names_output_log,
                                          names_output_normal,
                                          current_reaction_type_folder,
                                          current_equilibrium_phases)
            }
            print(paste('Output written in:',current_reaction_type_folder))
            eval(parse(text = paste("outputvector$",names(used_equilibrium_phases)[ce],"$",current_database,"$Reaction_Type_",toString(cr),"<-output",sep="")))
          }
        }
      }
      
      if(current_database == 'sit'){temperatures <- temperatures*2}  
    }
    
    output_collided_equiphases <- data.frame()
    for(outp1 in 1:length(outputvector)){
      eval(parse(text = paste('outputvector_temp1 <-outputvector$',names(outputvector)[outp1],sep="")))
      output_collided_databases <- data.frame()
      for(outp2 in 1:length(outputvector_temp1)){
        eval(parse(text = paste('outputvector_temp2 <-outputvector_temp1$',names(outputvector_temp1)[outp2],sep="")))
        output_collided_reactions <-data.frame()
        for(outp3 in 1:length(outputvector_temp2)){
          eval(parse(text = paste('outputvector_temp3 <-outputvector_temp2$',names(outputvector_temp2)[outp3],sep="")))
          solution_column_temp<-c(1:length(outputvector_temp3[1,]))[names(outputvector_temp3)=="Solution"]
          if(solution_column_temp<length(outputvector_temp3[1,])){
            outputvector_temp3<-outputvector_temp3[,c(1:(solution_column_temp-1),(solution_column_temp+1):length(outputvector_temp3[1,]),solution_column_temp)]
          }
          output_collided_reactions<-rbind.fill(output_collided_reactions,outputvector_temp3)
          output_collided_reactions[is.na(output_collided_reactions)]<- -999
        }
        output_collided_reactions<-collide_si_columns(output_collided_reactions,column_names_for_colliding)
        if(sum(summary_levels==3)>0){
          summary_folder1 <- paste(outputfolder,current_site,names(outputvector)[outp1],names(outputvector_temp1)[outp2],"summary",sep="/")
        if(dir.exists(summary_folder1)==FALSE){dir.create(summary_folder1)}
        
        write.csv(output_collided_reactions,paste(summary_folder1,'output_collided_reactions.csv',sep='/'))
        export_output_hist_boxplots(output_collided_reactions,
                                    hist_division,
                                    names_output_log,
                                    names_output_normal,
                                    summary_folder1,
                                    si_names(output_collided_reactions))}
        
        #WRITE OUTPUT TO FILE
        output_collided_databases<-rbind.fill(output_collided_databases,output_collided_reactions)
        solutioncolumn<-c(1:length(output_collided_databases))[names(output_collided_databases)=='Solution']
        output_collided_databases<-cbind(output_collided_databases[,(c(1:length(output_collided_databases))==solutioncolumn)==FALSE,drop=FALSE],output_collided_databases[,solutioncolumn,drop=FALSE])
        output_collided_databases[is.na(output_collided_databases)]<- -999
      }
      output_collided_databases<-collide_si_columns(output_collided_databases,column_names_for_colliding)
      if(sum(summary_levels==2)>0){
      summary_folder2 <- paste(outputfolder,current_site,names(outputvector)[outp1],"summary",sep="/")
      if(dir.exists(summary_folder2)==FALSE){dir.create(summary_folder2)}
      write.csv(output_collided_databases,paste(summary_folder2,'output_collided_databases.csv',sep='/'))
      export_output_hist_boxplots(output_collided_databases,
                                  hist_division,
                                  names_output_log,
                                  names_output_normal,
                                  summary_folder2,
                                  si_names(output_collided_databases))
      }
      output_collided_equiphases <- rbind.fill(output_collided_equiphases,output_collided_databases)
      output_collided_equiphases[is.na(output_collided_equiphases)]<- -999
    }
    solutioncolumn<-c(1:length(output_collided_equiphases))[names(output_collided_equiphases)=='Solution']
    output_collided_equiphases<-cbind(output_collided_equiphases[,(c(1:length(output_collided_equiphases))==solutioncolumn)==FALSE,drop=FALSE],output_collided_equiphases[,solutioncolumn,drop=FALSE])
    output_collided_equiphases[is.na(output_collided_equiphases)]<- -999
    for(int1 in 1:length(output_collided_equiphases$Initial_Alkalinity_CO3_2)){
      if(output_collided_equiphases$Initial_Alkalinity_CO3_2[int1]==-999){
        for(int2 in 1:length(solutions)){
          eval(parse(text=paste("output_collided_equiphases$Initial_",names(solutions)[int2],"[",as.character(int1),"]<-solutions$",names(solutions)[int2],"[",as.character(output_collided_equiphases$Solution[int1]),"]",sep="")))
        }
      }
    }
    output_collided_equiphases<-collide_si_columns(output_collided_equiphases,column_names_for_colliding)
    if(sum(summary_levels==1)>0){
    summary_folder3 <- paste(outputfolder,current_site,"summary",sep="/")
    if(dir.exists(summary_folder3)==FALSE){dir.create(summary_folder3)}
    write.csv(output_collided_equiphases,paste(summary_folder3,'output_collided_equiphases.csv',sep='/'))
    export_output_hist_boxplots(output_collided_equiphases,
                                hist_division,
                                names_output_log,
                                names_output_normal,
                                summary_folder3,
                                si_names(output_collided_equiphases))}
  }
  else{print(paste('No reaction defined for site number ', current_site,sep=""))}

}

