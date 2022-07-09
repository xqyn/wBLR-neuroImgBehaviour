# install.packages('dplyr', dependencies = TRUE)
# install.packages('tidyverse', dependencies = TRUE)
# install.packages('ggplot2', dependencies = TRUE)
# install.packages('readr', dependencies = TRUE)
# Library -----------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(viridis)
library(ggplot2)
library(readr)

# Load the dataset--------------------------------------------------------

## Phenomics  ------------------------------------------------------------------------


#Function worked 
phs <- read_delim("../data/phs000607.v3.pht003445.v3.p2.c1.Neurodevelopmental_Genomics_Subject_Phenotypes.GRU-NPU.txt",
                  "\t",
                  escape_double = FALSE,
                  na = "NA",
                  comment = "#",
                  trim_ws = TRUE)
dim(phs)

# Transfer txt manually by excel 
# phs <- read.csv('../data/phs000607.v3.pht003445.v3.p2.c1.csv')
# print(paste('Number of subject:', dim(phs)[1]))
# print(paste('Number of columns:', dim(phs)[2]))


## Phenomics dictionary  ------------------------------------------------------------------------

phs_dict <- read.csv("../data/data_dict_pht003445.v3.p2.csv")
phs_dict <- phs_dict[!is.na(phs_dict$`Variable ID`),]
# chosse columns of interest
phs_dict <- phs_dict[,c(2,3,4,5)]


## Load big QC file ------------------------------------------------------------------

bigqc <- read.csv('../data/mosi_dataset_share_include_qc.csv')
colnames(bigqc)

# Df of PNC dataset
pnc <- bigqc[bigqc$site == 'pnc',]
maxage <- max(pnc$age)
minage <- min(pnc$age)
print(paste('Max age in PNC:', maxage))
print(paste('Min age in PNC:', minage))
print(paste('Group in PNC:', unique(pnc$group)))
# Choose only subjects in control group:
# dim(bigqc)
# bigqc_c <- bigqc[bigqc$group == 'Control',]
# dim(bigqc_c)

### Age distribution of big QC file ------------------------------------------------------------------

bigqc_age <- ggplot(as.data.frame(bigqc) , mapping=aes(x=age)) +
  geom_histogram(binwidth = 1,
                colour = 'black',
                fill = 'red',
                alpha = 0.5)+
  xlab('Age')+
  ylab('Number of subjects')+
  geom_vline(xintercept=maxage, linetype="dashed", color = "red", size=0.5)+
  geom_vline(xintercept=minage, linetype="dashed", color = "red", size=0.5)+
  theme_minimal()+
  ggtitle('Histogram of age distribution')
bigqc_age
ggsave(paste0("figure/bigqc_age.png"), 
       plot=bigqc_age, width=12, height=6, dpi=320)


geom_histogram(pnc,aes(x=age), fill = "blue", alpha = 0.2)

View(bigqc)

bigqc$sex <- as.factor(bigqc$sex)
colour.Chan <- c("dodgerblue","#E03E3E","#53BB79","#EF8228","#937264","#3043A2","#C25D7B",
                 "dodgerblue","#E03E3E","#53BB79","#EF8228","#937264","#3043A2","#C25D7B",
                 "dodgerblue","#E03E3E","#53BB79","#EF8228","#937264","#3043A2","#C25D7B",
                 "dodgerblue","#E03E3E","#53BB79","#EF8228","#937264","#3043A2","#C25D7B",
                 "dodgerblue","#E03E3E","#53BB79","#EF8228","#937264","#3043A2","#C25D7B",
                 "dodgerblue","#E03E3E","#53BB79","#EF8228","#937264","#3043A2","#C25D7B",
                 "dodgerblue","#E03E3E","#53BB79","#EF8228","#937264","#3043A2","#C25D7B")


MedianRIplot <-  ggplot(as.data.frame(bigqc) , aes(x=sex, y=age))+
  #geom_boxplot(width=0.5) +
  geom_violin(adjust=0.75)#, fill = bigqc$sex) + 
  scale_colour_manual(values =  c("dodgerblue","#E03E3E"))+
  geom_boxplot(width=.025) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
         axis.title.x = element_blank())+
  ggtitle(paste0(" - MedianRI of Peptide Intensity"))+
  theme(legend.position = "none")
MedianRIplot




### Distribution of subject per sites ------------------------------------------------------------------

site_sub <- table(bigqc$site)
site_sub <- as.data.frame(site_sub)
colnames(site_sub) <- c('site', 'num_sub')
#View(site_sub)

site_sub_plot <- ggplot(site_sub , aes(x=site, y=num_sub, fill=site)) +
  geom_bar(stat="identity")+
  xlab('')+
  ylab('Number of subjects')+
  theme_minimal()+
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  ggtitle('Number of subject per sites')+
  theme(legend.position = "none")
#site_sub_plot
ggsave(paste0("figure/number_of_sub_per_sites.png"), 
       plot=site_sub_plot, width=12, height=6, dpi=320)

### Choose subjects in bigQC belongs to phs -------------------------------------------------

# Subjects of big QC file
subid_pnc <- bigqc$participant_id[bigqc$site == 'pnc']
# Remove 'sub-'
subid_pnc <- sub('sub-','',subid_pnc) 
length(subid_pnc)

# Check overlapping subjects between bigqc and phs file
# Without unique
length(which(phs$SUBJID %in% subid_pnc)) # duplicates as some subjects were interviews 2 times
length(subid_pnc %in% unique(phs$SUBJID)) # no duplicates
length(subid_pnc %in% phs$SUBJID)

# Choose behavioral domains of interest --------------------------------------------------------
disease <- c('ADD',#Attention deficit disorder    1x
             'AGR',#Agoraphobia                   2x
             'CDD',#Conduct disorder              3x
             'EAT',#Eating disorder               4x
             'GAD',#Generalized anxiety disorder  5x
             'MAN',#Manic                         6x
             'DEP',#Depression                    7
             'OCD',#Obsessive compulsive disorder 8x
             'ODD',#Oppositional defiant disorder 9x
             'PAN',#Panic disorder                10x
             'PHB',#Specific phobia               11x
             'PSY',#Psychosis                     12x
             'PTD',#Post-Traumatic stress         13x
             #'SCR',#General Probes
             'SEP',#Separation Anxiety            14x
             'SOC',#Social anxiety                15x
             'SUI' #Suicide                       16
             ) 

# Preprocessing ---------------------------------------------------------------------


## Look at number of interview -----------------------------------------------------

# Look at the distribution of some information


#Load again
phs_ori <- read_delim("../data/phs000607.v3.pht003445.v3.p2.c1.Neurodevelopmental_Genomics_Subject_Phenotypes.GRU-NPU.txt",
                  "\t",
                  escape_double = FALSE,
                  na = "NA",
                  comment = "#",
                  trim_ws = TRUE)
#phs_ori <- read.csv('../data/phs000607.v3.pht003445.v3.p2.c1.csv')
INT_NUM <- as.data.frame(table(phs$INT_NUM))
colnames(INT_NUM) <- c('Interviews',
                       'Participant')
ggplot(INT_NUM, aes(x="", y=Participant, fill=Interviews)) +
  geom_col() +
  geom_text(aes(label = Participant),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE) +
  coord_polar(theta = "y")



INT_NUM_ori <- as.data.frame(table(phs_ori$INT_NUM))
colnames(INT_NUM_ori) <- c('Interviews',
                       'Participant')
interview <- ggplot(INT_NUM_ori, aes(x="", y=Participant, fill=Interviews)) +
  geom_col() +
  geom_text(aes(label = Participant),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE) +
  coord_polar(theta = "y")+ ylim('None')+
  ylab('')+xlab('') + theme_minimal()
interview

ggsave(paste0("figure/Number_of_interview.png"), 
       plot=interview, width=5, height=4, dpi=320)


#age dítribution
age_num<- phs[which(phs['INT_NUM'] == 1),]
age <- 2012 - age_num['Med_birth_year']

age <- data.frame(age = age[,1][!is.na(age[,1])]) # remove NA
print(paste('Max age:', max(age)))
print(paste('Min age:', min(age)))
# Age plot of phs
age_plot <- ggplot(age , aes(x=age)) +
  geom_histogram(binwidth = 1,
                 colour = 'black',
                 fill = '#0077A7',
                 alpha = 0.75)+
  xlab('Age')+
  ylab('Frequency variables')+
  theme_minimal()+
  ggtitle(paste0('Number of is values: ', length(age[,1]), '\n',
                 'Number of is removed NA: ', nrow(age_num) - length(age[,1])))
age_plot

ggsave(paste0("figure/age_plot_of_phs.png"), 
       plot=age_plot, width=10, height=8, dpi=320)


## Make dataframe for only disease-related domains --------

DD = data.frame()
for (i in disease){
  if (dim(DD)[0] != 0){
    assign(i,phs[startsWith(colnames(phs),i)]) # extract into its own df
    DD <- cbind(DD,  phs[startsWith(colnames(phs),i)])} # add all disease into one df
  else {
    assign(i,phs[startsWith(colnames(phs),i)])
    DD <- phs[startsWith(colnames(phs),i)]} 
}
#DD[1:10,1:8]
DD_ori <- DD
DD <- DD_ori
dim(DD)



## Check number of interview ---------------------------------------------------------

# It is necessary to remove duplicate subjects (choose a subject only once)

# There are some different intervew approaches for subjects of ages 11-17 years, choose only one type for further analysis
# https://ftp.ncbi.nlm.nih.gov/dbgap/studies/phs000607/phs000607.v3.p2/pheno_variable_summaries/phs000607.v3.pht003445.v3.Neurodevelopmental_Genomics_Subject_Phenotypes.data_dict.xml
# Middle Proband (ages 11-17 years) (5186)
# Middle Informant (5145)
# Young Proband Informant (for ages 8-10 years) (2361)
# Adult Proband (ages 18 years and up) (1863)

# Different type of interview
table(phs$INT_TYPE)
IS <- DD_ori
phs <- phs_ori
# Add interview number, type and subject id to df for QC
IS <- add_column(IS, phs$INT_TYPE, .after = 0)
IS <- add_column(IS, phs$INT_NUM, .after = 0)
IS <- add_column(IS, phs$SUBJID, .after = 0)
dim(IS)
# Alter names
colnames(IS)[1:3] <- c('SUBJID','INT_NUM','INT_TYPE')
#View(IS)

# Split dataset depends on interview type / number (which somehow the same)
# on number of interview
IS_1 <- IS[IS$INT_NUM == 1,]
IS_2 <- IS[IS$INT_NUM == 2,]

dim(IS_1)
dim(IS_2)

# Check whether subjects really interview with 2 approaches
length(which(IS_2$SUBJID %in% IS_1$SUBJID))
length(which(IS_1$SUBJID %in% IS_2$SUBJID))
print(paste('There are',length(which(IS_1$SUBJID %in% IS_2$SUBJID)),'subjects with two interviews'))

# Choose only overlapping of IS1 as IS1 may contain subjects from other age range
IS_1ol <- IS_1[which(IS_1$SUBJID %in% IS_2$SUBJID),]
length(which(IS_1ol$SUBJID %in% IS_2$SUBJID))
dim(IS_1ol) # now same with IS1

IS_1ol_saved <- IS_1ol
IS_2_saved<- IS_2
IS_1ol <- IS_1ol_saved
IS_1ol <- IS_1ol_saved

# remove first 3 columns for plots
IS_2 <- IS_2[,-c(1:3)]
IS_1ol <- IS_1ol[,-c(1:3)]

print(paste('over lap:',sum(subid_pnc %in% IS_1ol$SUBJID)))
print(paste('over lap:',sum(subid_pnc %in% IS_2$SUBJID)))
#Noew no more overlapping

#Check proportional of missing value ib IS1 and IS2

library(ggpubr) # merge plots
thres_line <- 0.2

## oNE intervew

IS1_proportion_na <- colMeans(is.na(IS_1ol))
IS_1_P <- ggplot(as.data.frame(IS1_proportion_na) , aes(x=IS1_proportion_na)) +
  stat_ecdf(geom = "point",
            colour = '#01A7EC') +
  geom_hline(yintercept=thres_line, linetype="dashed", color = "red", size=0.5)+
  xlab('Proportion of NaN')+
  ylab('Empirical Cumulative Density Function')+
  theme_minimal()+
  ggtitle('Empirical Cumulative Density Function of MP')
#IS_1_P

IS_1_missing_plot <- ggplot(as.data.frame(IS1_proportion_na) , aes(x=IS1_proportion_na)) +
  geom_histogram(binwidth = 0.02,
                 colour = 'black',
                 fill = 'red',
                 alpha = 0.5)+
  xlab('% of missing data')+
  ylab('Frequency variables')+
  geom_vline(xintercept=thres_line, linetype="dashed", color = "red", size=0.5)+
  theme_minimal()+
  ggtitle('Histogram of % missing - Middle Proband')
#IS_1_missing_plot

## TWO intervew
IS2_proportion_na <- colMeans(is.na(IS_2))
IS_2_P <- ggplot(as.data.frame(IS2_proportion_na) , aes(x=IS2_proportion_na)) +
  stat_ecdf(geom = "point",
            colour = '#01A7EC') +
  geom_hline(yintercept=thres_line, linetype="dashed", color = "red", size=0.5)+
  xlab('Proportion of NaN')+
  ylab('Empirical Cumulative Density Function')+
  theme_minimal()+
  ggtitle('Empirical Cumulative Density Function of MI')
#IS_2_P

IS_2_missing_plot <- ggplot(as.data.frame(IS2_proportion_na) , aes(x=IS2_proportion_na)) +
  geom_histogram(binwidth = 0.02,
                 colour = 'black',
                 fill = 'red',
                 alpha = 0.5)+
  xlab('% of missing data')+
  ylab('Frequency variables')+
  geom_vline(xintercept=thres_line, linetype="dashed", color = "red", size=0.5)+
  theme_minimal()+
  ggtitle('Histogram of % missing - Middle Informant')
#IS_2_missing_plot


IS_all <- ggarrange(IS_1_P, IS_1_missing_plot,
                    IS_2_P, IS_2_missing_plot,
                    ncol = 2, nrow = 2)
ggsave(paste0("figure/missing_comparison_2.png"),
       plot=IS_all, width=8, height=6,dpi=320)



#View(rbind(IS1_proportion_na, IS2_proportion_na))
IS1_vs_IS2 <- rbind(IS1_proportion_na, IS2_proportion_na)
#View(t(IS1_vs_IS2))
sum(IS1_proportion_na > IS2_proportion_na)
sum(IS2_proportion_na > IS1_proportion_na)
sum(IS2_proportion_na == IS1_proportion_na)
# It seems that IS2 contains more missing values than IS1
# For this reason together with IS1 also contain the same 'Proband' interview, 
# choose IS1 for further analysis

## Choose subjects interview datasets phs ---------------------------------------------------------------
# 
phs <- read_delim("../data/phs000607.v3.pht003445.v3.p2.c1.Neurodevelopmental_Genomics_Subject_Phenotypes.GRU-NPU.txt",
                   "\t", escape_double = FALSE, na = "NA",
                   comment = "#", trim_ws = TRUE)


#phs <- read.csv('../data/phs000607.v3.pht003445.v3.p2.c1.csv')

dim(phs[phs$INT_TYPE != 'MP',])
phs_MP <- phs[phs$INT_TYPE != 'MP',]
dim(phs_MP)

dim(phs[phs$INT_TYPE != 'MI',])
phs_MI <- phs[phs$INT_TYPE != 'MI',]
dim(phs_MI)

dim(phs[phs$INT_NUM == '1',])

phs_I1 <- phs[phs$INT_NUM == '1',]
dim(phs_I1)

length(unique(phs_I1$SUBJID))

# Choose only those subjects that have neuron images (pnc) in big QC files

phs_qc <- phs_I1[which(phs_I1$SUBJID %in% as.numeric(subid_pnc)),]

#phs_qc <- phs_I1[which(subid_pnc %in% phs_I1$SUBJID),]
dim(phs_qc)

#sum(phs_qc$SUBJID %in% phs_z$participant_id)
length(unique(phs_qc$SUBJID))

phs <-  phs_qc
dim(phs)

# New DD based on neuron image subjects

DD = data.frame()
for (i in disease){
  if (dim(DD)[1] != 0){
    assign(i,phs[startsWith(colnames(phs),i)]) # extract into its own df
    DD <- cbind(DD,  phs[startsWith(colnames(phs),i)])} # add all disease into one df
  else {
    assign(i,phs[startsWith(colnames(phs),i)])
    DD <- phs[startsWith(colnames(phs),i)]} 
}

DD_qc <- DD
DD <- DD_qc
dim(DD)


## Remove missing value --------------------------------------------------------------

# Change 9 to NA value
print(paste('Before:', sum(is.na(DD))))

# Option 1: find the columns that contains maxvalue = 9 and 
# change the 9 into NA
# DD[,apply(DD,2, max, na.rm=T) == 9][DD[,apply(DD,2, max, na.rm=T) == 9] == 9] <- NA

# Option 2: find the columns that contains unique value < 5 
# (as of c(0,1,9,NA) = lenght 4) and change the 9 into NA
for (i in colnames(DD)) {
  if (length(unique(DD[,i])) < 5) {
    DD[,i][DD[,i] == 9] <- NA
  }
}
print(paste('After:', sum(is.na(DD))))

#Calculate the amount of NA per variables
proportion_na <- colMeans(is.na(DD))

# Have a look 
#knitr::kable(head(t(proportion_na),20))
#proportion_na

# Plot
#plot(ecdf(proportion_na))
#print('Interpretation: Cumulative percentage of those proportion of NA that are < 0.5 (50%) are about 23%.')
# or
prop_missing <- ggplot(as.data.frame(proportion_na) , aes(x=proportion_na)) +
  stat_ecdf(geom = "point",
            colour = '#01A7EC') +
  geom_hline(yintercept=0.20, linetype="dashed", color = "red", size=0.5)+
  xlab('Proportion of NaN')+
  ylab('Empirical Cumulative Density Function')+
  theme_minimal()+
  ggtitle('Empirical Cumulative Density Function plot')
prop_missing
ggsave(paste0("figure/prop_missing_before_rm_20_percent.png"), 
       plot=prop_missing, width=4.5, height=4, dpi=320)

#Check the distribution of missing values

missing_plot <- ggplot(as.data.frame(proportion_na) , aes(x=proportion_na)) +
  geom_histogram(binwidth = 0.02,
                 colour = 'black',
                 fill = 'red',
                 alpha = 0.5)+
  xlab('% of missing data')+
  ylab('Frequency variables')+
  geom_vline(xintercept=0.20, linetype="dashed", color = "red", size=0.5)+
  theme_minimal()+
  ggtitle('Histogram of % missing data')
missing_plot
ggsave(paste0("figure/prop_missing_before_rm_20_percent_den.png"), 
       plot=missing_plot, width=4.5, height=4, dpi=320)


prop_df <- data.frame(proportion_na)
prop_df$Domains <- rownames(prop_df)

missing_plot_bar <- ggplot(prop_df , aes(x=Domains, y=proportion_na)) 
  geom_bar()+
  # geom_bar(binwidth = 0.02,
  #                colour = 'black',
  #                fill = 'red',
  #                alpha = 0.5)+
  # xlab('% of missing data')+
  ylab('Frequency variables')+
  # geom_vline(xintercept=0.20, linetype="dashed", color = "red", size=0.5)+
  theme_minimal()+
  ggtitle('Histogram of % missing data')
missing_plot_bar

## Check columns with high missing rate ------------------------------------
# NO NEED NOW
# DD_missing<-colnames(DD[,proportion_na>0.2])
# colnames(DD_missing)
# 
# 
# DD = data.frame()
# for (i in disease){
#   if (dim(DD) != 0){
#     assign(i,phs[startsWith(colnames(phs),i)]) # extract into its own df
#     DD <- cbind(DD,  phs[startsWith(colnames(phs),i)])} # add all disease into one df
#   else {
#     assign(i,phs[startsWith(colnames(phs),i)])
#     DD <- phs[startsWith(colnames(phs),i)]} 
# }
# dim(DD)
# list_high_missing <- list()
# for (i in disease){
#   list_high_missing[[i]]<- DD_missing[startsWith(DD_missing,i)]
# }
# list_high_missing
# list_high_missing$OCD
# 
# 
# phs_dict$missing <- phs_dict$Variable %in% DD_missing
# phs_dict$missing[which(phs_dict$missing)] <- 1
# phs_dict$missing[which(!phs_dict$missing)] <- 0
# 
# write.csv(phs_dict,file.path(paste0('./data/DD_dict_m.csv')), row.names = FALSE)

# Precossing impute  ----------------------------------------------------------------

# Check factor type of data
# 
# continous_data <- colnames(DD[,which(c(sapply(DD, function(x) any(x > 12))))])
# #View(DD[,continous_data])
# 
# #Remove variables with more than 50% data missing
# DDsaved1 <- DD
# DD <- DDsaved1
# 
# 
##  Remove proportion of missing value -------------------------------------
pm <- 0.20
DD<-DD[,proportion_na<pm]
DD_final <- DD
DD <- DD_final
dim(DD)
# 
# Pick out Cont vs Binary data ------------------------------------------------------
# # not need now
# continous_data <- colnames(DD[,which(c(sapply(DD, function(x) any(x > 12))))])
# 
# # Factor data (binary + nominal + orginal)
# factor_data <- colnames(DD)[!(colnames(DD) %in%continous_data)]
# 
# # or
# #for (i in length(factor_data)) {
# #  DD[(DD[!NA_index[,factor_data[i]],factor_data[i]]), factor_data[i]] <- sapply(DD[(DD[!NA_index[,factor_data[i]],factor_data[i]]), factor_data[i]], function(x) as.factor(x))
# #}


#for (i in factor_data) {
#  DD[i] <- sapply(DD[i], function(x) as.factor(x))
#}


write.csv(DD,file.path(paste0('./data/DD_no_impute_1262sub_in_bigqc.csv')), row.names = FALSE)

# Impute ------------------------------------------------------------------
# This step is already done, saved files are on github

DD <- DD_final

dim(DD)
imp_id <- 'impute_qc'
imp_dir <- paste0('./data./',imp_id,'/')
dir.create(imp_dir)
dir.create(paste0(imp_dir,'mean/'))

## imputed with mean -----------------------------------------------------------------------
#imputed by mean
#Set NA to mean value columns
DD_mean <- DD
for(i in 1:ncol(DD_mean)){
  DD_mean[is.na(DD_mean[,i]), i] <- mean(DD_mean[,i], na.rm = TRUE)
}
write.csv(DD_mean,file.path(paste0(imp_dir,'mean/0.mean_DD_imputed.csv')), row.names = FALSE)
dim(DD_mean)


## imputed with mice -----------------------------------------------------------------------

#disease
DD_imp <- DD
#a <- c('SUI', 'SOC')
imp <- data.frame()
DD_imp <- data.frame()
m <- pm*100 #Number of multiple imputations. 
method <- 'cart' #random forest
maxit	<- 8 #A scalar giving the number of iterations. The default is 5.
seed <- 500
path <- paste0(imp_dir,method,'/')
name <- paste0(method,'_')

for (i in disease){
    imp <- DD[startsWith(colnames(DD),i)]
    imp <- mice(imp, m=m, maxit = maxit, method = method, seed = seed)
    df_complete <- mice::complete(imp,2)
    dir.create(path)
    write.csv(df_complete, file.path(paste0(path,i,'_',name,imp_id,'.csv')), row.names = FALSE)
    if (dim(DD_imp) != 0){
      DD_imp <- cbind(DD_imp,  df_complete)} 
    else {
      DD_imp <- df_complete}
    }
write.csv(DD_imp,file.path(paste0(path,name,'DD_imputed.csv')), row.names = FALSE)
logs <- data.frame(m, method, maxit, seed, row.names = NULL)
write.table(logs,file.path(paste0(path,name,'logs.txt')), sep = "\t",
            row.names = FALSE, col.names = TRUE)
DD_imp_te <- read.csv(paste0(path,name,'DD_imputed.csv'))


# load random forest imputed data
# DD_mean <- read.csv('./data/impute/mean/0.mean_DD_imputed.csv')
# DD_rf <- read.csv('./data/impute/1/1.rf_DD_imputed.csv')
# DD_cart <- read.csv('./data/impute/2/2.cart_DD_imputed.csv')


## Iscore ------------------------------------------------------------------

# Iscore for evaluate imputation performance (very time consuming and computational expense)
# On progress

# methods <- c("mean","rf","cart")
# a[1]
# 
# df.imp.score <- data.frame()
# for (i in a[1]){
#   print(paste0('Running on: ', i))
#   imputations <- list()
#   imputations[[1]] <- as.matrix(DD_rf[startsWith(colnames(DD_rf),i)])
#   imputations[[2]] <- as.matrix(DD_cart[startsWith(colnames(DD_cart),i)])
#   imputations[[3]] <- as.matrix(DD_mean[startsWith(colnames(DD_mean),i)])
#   X.NA <- DD[startsWith(colnames(DD),i)]
#   imputations <- list(list(imputations[[1]]), list(imputations[[2]]),list(imputations[[3]]))
#   imp_score <- Iscores(imputations = imputations, methods = methods, X.NA = X.NA)
#   imp_score<- as.data.frame(imp_score)
#   imp_score$domain <- i
#   df.imp.score <- rbind(imp_score,df.imp.score)
# }
# write.csv(DD_imp,paste0('./data/imp_iscore.csv'), row.names = FALSE)
# 
# df.imp.score <- read.csv('./data/imp_iscore.csv')
# View(df.imp.score)
# 
# df.imp.score1 <- df.imp.score



# Import imputed DD datasets ----------------------------------------------------------------

imp_id <- 'impute_qc'
imp_dir <- paste0('./data./',imp_id,'/')

DD_mean <- read.csv(paste0(imp_dir,'mean/0.mean_DD_imputed.csv'))
DD_rf <- read.csv(paste0(imp_dir,'rf/rf_DD_imputed.csv'))
DD_cart <- read.csv(paste0(imp_dir,'cart/cart_DD_imputed.csv'))

dim(DD_mean)
dim(DD_rf)
dim(DD_cart)

# imp_type <- 'mean'
# DD <- DD_mean

imp_type <- 'rf'
DD <- DD_rf

# imp_type <- 'cart'
# DD <- DD_cart


## Test variables from imputed data --------------------------------------------------
DD <- DD_rf # Imputed with random forrest
DD <- DD_mean # Imputed with random forrest
DD <- DD_cart # Imputed with random forrest
k <- 1
print(paste('Category:', colnames(DD)[k]))
print(paste('Max:', max(DD[,k])))
print(paste('Min:', min(DD[,k])))
print(paste('Mean:', mean(DD[,k])))
print(paste('SD:', sd(DD[,k])))

var_dis <- ggplot(data.frame(colnam = DD[,k]) , aes(x=colnam)) +
  geom_histogram(binwidth = mean(DD[,k])/2,
                 colour = 'black',
                 fill = 'red',
                 alpha = 0.5)+
  xlab('Value')+
  ylab('Frequency')+
  theme_minimal()+
  ggtitle(paste(colnames(DD)[k], '- RF'))
var_dis
ggsave(paste0("figure/variales_dis_in_some_domain_RF.png"), 
       plot=var_dis, width=4.5, height=4, dpi=320)




# PCA -------------------------------------------------------------------------------
# following this: https://github.com/StatQuest/pca_demo/blob/master/pca_demo.R
colour.code <- c("#cf3c65","#5594e8","#50e152","#8f8918", "#2bcfcc", "#24dcaf","#188ead",
                 "#c61167","#75989c","#20a93f","#3789d2","#812efa","#2c2982","#a8a098","#924660", "#99f2ca")

## Domains -------------------------------------------------------------------------------

imp_type <- 'rf'
DD <- DD_rf
pca <- prcomp(t(DD))
dim(pca)

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per <- as.data.frame(pca.var.per)
pca.var.per$PC <- rownames(pca.var.per)

#Scree
pca_scree <- ggplot(data=pca.var.per[1:8,], aes(x = PC, y=pca.var.per)) +
  geom_bar(stat="identity",
           colour = 'black',
           fill="#1C96BA",
           width=0.95) +
  xlab("Principal Component") +
  ylab("Percent Variation") +
  theme_minimal() +
  ggtitle("Scree Plot of PCA for PhS")

ggsave(paste0("figure/pca_domain_",imp_type,"_pca_scree.png"), 
       plot=pca_scree, width=4.5, height=4, dpi=320)


pca.data <- data.frame(disease=rownames(pca$x),
                       PC1=pca$x[,1],
                       PC2=pca$x[,2])
pca.data$Category <- substr(rownames(pca.data), 1, 3) # add category corresponding to each disease
head(pca.data)


#complexity:close to 1 is better
#fit.metrics <- fit$complexity
#fit.metrics <- data.frame(fit.metrics)
#colnames(fit.metrics) <- 'values'
#fit.metrics$Cate <- rownames(fit.metrics)
#fit.metrics$Disease <- substr(rownames(fit.metrics), 1, 3)

pc1_plot <- ggplot(pca.data, aes(disease, PC1, fill=Category)) + 
  facet_wrap(1, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity",
           alpha = 0.75) + #make the bars
  coord_flip() + 
  scale_fill_manual(values = colour.code)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.5)+
  ylab("PC1 loading") + #improve y-axis label
  xlab("Category") +
  theme_bw(base_size=10) + #use a black-and0white theme with set font size
  theme(axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=1))
pc1_plot 

ggsave(paste0("figure/pca_domain_",imp_type,"_pc1.png"), 
       plot=pc1_plot, width=4.5, height=8, dpi=320)

pc2_plot <- ggplot(pca.data, aes(disease, PC2, fill=Category)) + 
  facet_wrap(1, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity",
           alpha = 0.75) + #make the bars
  coord_flip() + 
  scale_fill_manual(values = colour.code)+
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.5)+
  ylab("PC2 loading") + #improve y-axis label
  xlab("Category") +
  theme_bw(base_size=10) +#use a black-and0white theme with set font size
  theme(axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=1))
pc2_plot

ggsave(paste0("figure/pca_domain_",imp_type,"_pc2.png"), 
       plot=pc1_plot, width=4.5, height=8, dpi=320)


pca_plot <- ggplot(data=pca.data, aes(x=PC1, y=PC2, label=disease, colour = Category)) +
  geom_point(size =3) +
  scale_color_manual(values = colour.code)+
  xlab(paste("PC1 (EV: ", pca.var.per[1,1], "%)", sep="")) +
  ylab(paste("PC2 (EV: ", pca.var.per[2,1], "%", sep="")) +
  theme_minimal() +
  ggtitle("PCA for PhS")
pca_plot


ggsave(paste0("figure/pca_domain_",imp_type,".png"), 
       plot=pca_plot, width=4.5, height=4, dpi=320)

write.csv(pca.data,'./data/pca_data.csv', row.names = FALSE)


## Subject ---------------------------------------------------------

for (i in colnames(DD)){
  DD[,i] <- as.factor(DD[,i])
}

dim(DD)
length(rownames(DD))
length(phs_qc$SUBJID)
length(unique(phs_qc$SUBJID))

rownames(DD) <- phs_qc$SUBJID
pca <- prcomp(DD)
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per <- as.data.frame(pca.var.per)
pca.var.per$PC <- rownames(pca.var.per)

# #Plot
pca_scree<- ggplot(data=pca.var.per[1:8,], aes(x = PC, y=pca.var.per)) +
  geom_bar(stat="identity",
           colour = 'black',
           fill="#1C96BA",
           width=0.95) +
  xlab("Principal Component") +
  ylab("Percent Variation") +
  theme_minimal() +
  ggtitle(paste0("Scree Plot of PCA for PhS - ",imp_type))


ggsave(paste0("figure/pca_",imp_type,"_scree.png"), 
       plot=pca_scree, width=4, height=4, dpi=320)




pca.data <- data.frame(SUBJID=as.character(rownames(pca$x)),
                       PC1=pca$x[,1],
                       PC2=pca$x[,2],
                       PC3=pca$x[,3],
                       PC4=pca$x[,4],
                       PC5=pca$x[,5])
#pca.data$Category <- substr(rownames(pca.data), 1, 3) # add category corresponding to each disease
head(pca.data)
write.csv(pca.data,file.path(paste0('./data/pca/pca_',imp_type,'.csv')), row.names = FALSE)

k <- 500
x<-pca.data$PC1
y<-pca.data$PC2

contour_cols <- viridis(k, alpha = 0.5)
get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

dens <- get_density(x, y, k)
def.par<-par()


###
options(repr.plot.width = 2, repr.plot.height = 40, repr.plot.res = 100)
pca_plot <- ggplot(pca.data) +
  geom_point(aes(x=PC1, y=PC2), 
             col = contour_cols[findInterval(dens, seq(0, max(dens), length.out = k))], 
             pch = 16, 
             alpha = .35,
             size=2.5) +
 
  labs(title='GAMLSS',
       # subtitle='Location, scale, and shape\nare modeled as functions of x',
       x = 'PC1', 
       y='PC2')  +
  xlab(paste("PC1 (EV: ", pca.var.per[1,1], "%)", sep="")) +
  ylab(paste("PC2 (EV: ", pca.var.per[2,1], "%)", sep="")) +
  theme_minimal() +
  ggtitle(paste0("PCA for PhS - ",imp_type))

pca_plot


ggsave(paste0("figure/pca_",imp_type,".png"), 
       plot=pca_plot, width=4, height=4, dpi=320)


## UMAP --------------------------------------------------------------------

library(umap)

imp_type <- 'rf'
DD <- DD_rf

for (i in colnames(DD)){
  DD[,i] <- as.factor(DD[,i])
}

DD.umap = umap(DD,
               n_components = 5, 
               random_state = 15)


DD.umap <- DD.umap[["layout"]] 
DD.umap <- data.frame(layout) 
colnames(DD.umap) <- c('UMAP1','UMAP2','UMAP3','UMAP4','UMAP5')

###
k <- 2000
x<-DD.umap$UMAP1
y<-DD.umap$UMAP2

contour_cols <- viridis(k, alpha = 0.5)
get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

options(repr.plot.width = 2, repr.plot.height = 40, repr.plot.res = 100)
umap_plot <- ggplot(DD.umap) +
  geom_point(aes(x=UMAP1, y=UMAP2), 
             col = contour_cols[findInterval(dens, seq(0, max(dens), length.out = k))], 
             #col = 'red',
             pch = 16, 
             alpha = .15,
             size=2.5) +
  labs(title='GAMLSS',
       # subtitle='Location, scale, and shape\nare modeled as functions of x',
       x = 'UMAP1', 
       y='UMAP2')  +
  #xlab(paste("PC1 (EV: ", pca.var.per[1,1], "%)", sep="")) +
  #ylab(paste("PC2 (EV: ", pca.var.per[2,1], "%)", sep="")) +
  theme_minimal() +
  ggtitle(paste0("UMAP for PhS - ",imp_type))

umap_plot

ggsave(paste0("figure/umap_",imp_type,".png"), 
       plot=umap_plot, width=4, height=4, dpi=320)



# Z-score from NM ---------------------------------------------------------

phs_z <- read.csv('../data/phs_bigqc_site_Z_df.csv')
dim(phs_z)
View(phs_z)
# clean file
# clean sub
phs_z <- phs_z[,-1]
phs_z$participant_id <- sub('sub-','',phs_z$participant_id) 

# Order dataframe based on sub id
phs_z <- phs_z[ order(as.numeric(phs_z$participant_id)), ]

### blr qc

phs_z_blr <- read.csv('../data/phs_bigqc_site_Z_df_blr.csv')
dim(phs_z_blr)
View(phs_z_blr)
# clean file
# clean sub
phs_z_blr <- phs_z_blr[,-1]
phs_z_blr$participant_id <- sub('sub-','',phs_z_blr$participant_id) 

# Order dataframe based on sub id
phs_z_blr <- phs_z_blr[ order(as.numeric(phs_z_blr$participant_id)), ]

#


# reset index
rownames(phs_z) <- NULL   
View(phs_z)


phs_z_pca <- phs_z[,-1]
rownames(phs_z_pca) <- phs_z$participant_id
#View(phs_z_pca)

imp_type <- 'Z-score'

pca <- prcomp(phs_z_pca)
pca.var <- pca$sdev^2
pc <- .var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per <- as.data.frame(pca.var.per)
pca.var.per$PC <- rownames(pca.var.per)

# #Plot
pca_scree<- ggplot(data=pca.var.per[1:8,], aes(x = PC, y=pca.var.per)) +
  geom_bar(stat="identity",
           colour = 'black',
           fill="#1C96BA",
           width=0.95) +
  xlab("Principal Component") +
  ylab("Percent Variation") +
  theme_minimal() +
  ggtitle(paste0("Scree Plot of PCA for PhS - ",imp_type))

pca_scree
ggsave(paste0("figure/pca_phs_",imp_type,"_scree.png"), 
        plot=pca_scree, width=4, height=4, dpi=320)

pca.data <- data.frame(SUBJID=as.character(rownames(pca$x)),
                       PC1=pca$x[,1],
                       PC2=pca$x[,2],
                       PC3=pca$x[,3],
                       PC4=pca$x[,4],
                       PC5=pca$x[,5])
#pca.data$Category <- substr(rownames(pca.data), 1, 3) # add category corresponding to each disease
head(pca.data)

write.csv(pca.data,file.path(paste0('./data/pca/pca_phs_',imp_type,'.csv')), row.names = FALSE)

k <- 500
x<-pca.data$PC1
y<-pca.data$PC2

contour_cols <- viridis(k, alpha = 0.5)
get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

dens <- get_density(x, y, k)
def.par<-par()


###
options(repr.plot.width = 2, repr.plot.height = 40, repr.plot.res = 100)
pca_plot <- ggplot(pca.data) +
  geom_point(aes(x=PC1, y=PC2), 
             col = contour_cols[findInterval(dens, seq(0, max(dens), length.out = k))], 
             pch = 16, 
             alpha = .35,
             size=2.5) +
  
  labs(title='GAMLSS',
       # subtitle='Location, scale, and shape\nare modeled as functions of x',
       x = 'PC1', 
       y='PC2')  +
  xlab(paste("PC1 (EV: ", pca.var.per[1,1], "%)", sep="")) +
  ylab(paste("PC2 (EV: ", pca.var.per[2,1], "%)", sep="")) +
  theme_minimal() +
  ggtitle(paste0("PCA for PhS - ",imp_type))

pca_plot

ggsave(paste0("figure/pca_",imp_type,".png"), 
       plot=pca_plot, width=4, height=4, dpi=320)

dim(phs_z_pca)


# DD for phs NM -----------------------------------------------------------

DD <- DD_rf
View(DD)
dim(DD)
rownames(DD) <- phs_qc$SUBJID

#sum(phs_qc$SUBJID %in% phs_z$participant_id)

DD_phs <- DD[which(rownames(DD) %in% phs_z$participant_id),]
#DD_phs <- DD[which(phs_z$participant_i %in% rownames(DD)),]

# Order based of subject ID
DD_phs <- DD_phs[ order(as.numeric(row.names(DD_phs))), ]

dim(DD_phs)
View(DD_phs)

# check rownames

compare_phs <- data.frame(rownames(DD_phs), phs_z$participant_id)
View(compare_phs)



# PCA for DD phs  ---------------------------------------------------------
imp_type <- 'DD_phs'

pca <- prcomp(DD_phs)
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per <- as.data.frame(pca.var.per)
pca.var.per$PC <- rownames(pca.var.per)

# #Plot
pca_scree<- ggplot(data=pca.var.per[1:8,], aes(x = PC, y=pca.var.per)) +
  geom_bar(stat="identity",
           colour = 'black',
           fill="#1C96BA",
           width=0.95) +
  xlab("Principal Component") +
  ylab("Percent Variation") +
  theme_minimal() +
  ggtitle(paste0("Scree Plot of PCA for PhS - ",imp_type))

pca_scree
ggsave(paste0("figure/pca_phs_",imp_type,"_scree.png"), 
       plot=pca_scree, width=4, height=4, dpi=320)




pca.data <- data.frame(SUBJID=as.character(rownames(pca$x)),
                       PC1=pca$x[,1],
                       PC2=pca$x[,2],
                       PC3=pca$x[,3],
                       PC4=pca$x[,4],
                       PC5=pca$x[,5])
#pca.data$Category <- substr(rownames(pca.data), 1, 3) # add category corresponding to each disease
head(pca.data)
write.csv(pca.data,file.path(paste0('./data/pca/pca_phs_',imp_type,'.csv')), row.names = FALSE)

k <- 500
x<-pca.data$PC1
y<-pca.data$PC2

contour_cols <- viridis(k, alpha = 0.5)
get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

dens <- get_density(x, y, k)
def.par<-par()


###
options(repr.plot.width = 2, repr.plot.height = 40, repr.plot.res = 100)
pca_plot <- ggplot(pca.data) +
  geom_point(aes(x=PC1, y=PC2), 
             col = contour_cols[findInterval(dens, seq(0, max(dens), length.out = k))], 
             pch = 16, 
             alpha = .35,
             size=2.5) +
  
  labs(title='GAMLSS',
       # subtitle='Location, scale, and shape\nare modeled as functions of x',
       x = 'PC1', 
       y='PC2')  +
  xlab(paste("PC1 (EV: ", pca.var.per[1,1], "%)", sep="")) +
  ylab(paste("PC2 (EV: ", pca.var.per[2,1], "%)", sep="")) +
  theme_minimal() +
  ggtitle(paste0("PCA for PhS - ",imp_type))

pca_plot

View(pca.data)

# 
ggsave(paste0("figure/pca_phs_",imp_type,".png"), 
      plot=pca_plot, width=4, height=4, dpi=320)
pca_plot_dis <- ggMarginal(pca_plot, type="histogram")

ggsave(paste0("figure/pca_phs_",imp_type,"distribution.png"), 
       plot=pca_plot_dis, width=4, height=4, dpi=320)


pca.data

# Check whether same orders
sum(abs(as.numeric(pca.data$SUBJID) - as.numeric(phs_z$participant_id)))

View(pca.data)
View(phs_z)

DD_phs_pca <- pca.data

write.csv()
write.csv(phs_z,file.path(paste0('../data/phs_R_bigqc_site_Z_df.csv')), row.names = FALSE)
write.csv(phs_z_blr,file.path(paste0('../data/phs_R_bigqc_site_Z_df_blr.csv')), row.names = FALSE)
write.csv(DD_phs_pca,file.path(paste0('../data/phs_R_DD_pca.csv')), row.names = FALSE)


phs_z <- read.csv(paste0('../data/phs_R_bigqc_site_Z_df.csv'))
DD_phs_pca <- read.csv(paste0('../data/phs_R_DD_pca.csv'))


View(phs_z)
View(DD_phs_pca)

# z-score vs PC1 --------------------------------------------------------------------

phs_id <- read.csv('data/phs_id.csv')
phs_z <- read.csv('data/phs_z.csv')
bigqc <- read.csv('../data/mosi_dataset_share_include_qc.csv')

View(phs_id)
View(phs_z)
View(phs)
dim(phs_z)
phs_z$participant_id <-  sub('sub-','',phs_z$participant_id)
length(phs_z$participant_id)
length(phs_z$participant_id %in% unique(phs$SUBJID))

subid_pnc <- bigqc$participant_id[bigqc$site == 'pnc']
length(subid_pnc)
subid_pnc <- sub('sub-','',subid_pnc) 
length(subid_pnc %in% unique(phs$SUBJID))

#
#


# MCA ---------------------------------------------------------------------
# On progress
library("FactoMineR")
DD.mca = MCA(t(DD))#, quanti.sup=19, quali.sup=c(20:36))

res.mca = MCA(tea, quanti.sup=19, quali.sup=c(20:36))


# Table of domains ------------------------------------------------------------------
DD_table <- data.frame()

DD_table <- list('Attention deficit disorder',
                   'Agoraphobia',
                   'Conduct disorder',
                   'Eating disorder',
                   'Generalized anxiety disorder',
                   'Manic',
                   'Depression',                    
                   'Obsessive compulsive disorder',
                   'Oppositional defiant disorder',
                   'Panic disorder',
                   'Specific phobia',
                   'Psychosis',
                   'Post-Traumatic stress',
                   'Separation Anxiety',
                   'Social anxiety',
                   'Suicide')


DD_table <- data.frame(matrix(unlist(DD_table), nrow=length(DD_table), byrow=TRUE))

DD_table <- cbind(DD_table, data.frame(table(substr(colnames(DD_ori),1,3))))
DD_table$Freq2<- data.frame(table(substr(colnames(DD),1,3)))$Freq


colnames(DD_table) <- c('Domains', 'Short','Before QC', 'After QC')
DD_table
write.csv(DD_table,file.path(paste0('./data/mental_domains.csv')), row.names = FALSE)



# Manually PNC QC -----------------------------------------------------------------------

dim(DD_qc) # PNC dataset with all columns
i <- 'PSY'
  
PSY_man <- DD_qc[startsWith(colnames(DD_qc),i)]
dim(PSY_man)
View(PSY_man)


## OCD -------------------------------------------------------------------------------

View(DD_qc)
dim(DD_qc)
domain <- 'OCD'

OCD_man <- DD_qc[startsWith(colnames(DD_qc),domain)]

OCD_man[,apply(OCD_man,2, max, na.rm=T) == 9][OCD_man[,apply(OCD_man,2, max, na.rm=T) == 9] == 9] <- NA

# for (i in colnames(OCD_man)) {
#   if (length(unique(OCD_man)) < 5) {
#     OCD_man[,i][OCD_man[,i] == 9] <- NA
#   }
# }



rownames(OCD_man) <- phs_qc$SUBJID
#View(OCD_man)
OCD_man_qc <- OCD_man[,c(1:17,24,25,32:34)]

#Re categorize
OCD_man_qc$OCD032[OCD_man_qc$OCD032<5] <- 0
OCD_man_qc$OCD032[OCD_man_qc$OCD032>4] <- 1

OCD_man_qc$OCD033[OCD_man_qc$OCD033<5] <- 0
OCD_man_qc$OCD033[OCD_man_qc$OCD033>4] <- 1
#View(OCD_man_qc)
DD <- OCD_man_qc


proportion_na1 <- colMeans(is.na(DD))
prop_missing1 <- ggplot(as.data.frame(proportion_na1), aes(x=proportion_na1)) +
  stat_ecdf(geom = "point",
            colour = '#01A7EC') +
  geom_hline(yintercept=0.20, linetype="dashed", color = "red", size=0.5)+
  xlab('Proportion of NaN')+
  ylab('Empirical Cumulative Density Function')+
  theme_minimal()+
  ggtitle('Empirical Cumulative Density Function plot')
prop_missing1

proportion_na1 <- rowMeans(is.na(DD)) #
missing_plot1 <- ggplot(as.data.frame(proportion_na1) , aes(x=proportion_na1)) +
  geom_histogram(binwidth = 0.02,
                 colour = 'black',
                 fill = 'red',
                 alpha = 0.5)+
  xlab('% of missing data')+
  ylab('Frequency variables')+
  geom_vline(xintercept=0.20, linetype="dashed", color = "red", size=0.5)+
  theme_minimal()+
  ggtitle('Histogram of % missing data')
missing_plot1

# Remove subjet that have max mising
# pm <- 0.80
# DD<-DD[proportion_na1<pm,]
#DD_final <- DD
#DD <- DD_final


replace_index <- function(df,X,Y){
for (i in 1:nrow(df)){
  if (is.na(df[i,Y])) {
    if (all(apply(df[i,X], 2, function(x) is.na(x)))){df[i,Y] = NA}
    else if (all(sapply(df[i,X], identical, 0))){df[i,Y] = 0} 
    else {df[i,Y] = 1}
  }}
  return(df)
  }



DD_OCD <- replace_index(DD,c(1:8),'OCD009') #OCD09
DD_OCD <- replace_index(DD_OCD,c(1:8),'OCD010') #OCD10
DD_OCD <- replace_index(DD_OCD,c(11:17),'OCD024') #OCD10
DD_OCD <- replace_index(DD_OCD,c(11:17),'OCD025') #OCD10
DD_OCD <- replace_index(DD_OCD,c(11:17),'OCD032') #OCD10
DD_OCD <- replace_index(DD_OCD,c(11:17),'OCD033') #OCD10
DD_OCD <- replace_index(DD_OCD,c(11:17),'OCD034') #OCD10
View(DD_OCD)
proportion_na3 <- colMeans(is.na(DD_OCD)) #

missing_plot3 <- ggplot(as.data.frame(proportion_na3) , aes(x=proportion_na3)) +
  geom_histogram(binwidth = 0.02,
                 colour = 'black',
                 fill = 'red',
                 alpha = 0.5)+
  xlab('% of missing data')+
  ylab('Frequency variables')+
  geom_vline(xintercept=0.20, linetype="dashed", color = "red", size=0.5)+
  theme_minimal()+
  ggtitle('Histogram of % missing data')
missing_plot3


ggsave(paste0("domains/misingplot3.png"), 
       plot=missing_plot3, width=4, height=4, dpi=320)

proportion_na4 <- rowMeans(is.na(DD_OCD)) #

missing_plot4 <- ggplot(as.data.frame(proportion_na4) , aes(x=proportion_na4)) +
  geom_histogram(binwidth = 0.02,
                 colour = 'black',
                 fill = 'red',
                 alpha = 0.5)+
  xlab('% of missing data')+
  ylab('Frequency variables')+
  geom_vline(xintercept=0.02, linetype="dashed", color = "red", size=0.5)+
  theme_minimal()+
  ggtitle('Histogram of % missing data')
missing_plot4

ggsave(paste0("domains/misingplot4.png"), 
       plot=missing_plot4, width=4, height=4, dpi=320)

# sum(proportion_na4 > 0.10)
# sum(proportion_na4 == 0.0000000)

DD_OCD_qc <- DD_OCD[proportion_na4 == 0,]
View(DD_OCD_qc)
dim(DD_OCD_qc)

## PCA OCD -------------------------------------------------------------------------


pca <- prcomp(DD_OCD_qc)
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per <- as.data.frame(pca.var.per)
pca.var.per$PC <- rownames(pca.var.per)

# #Plot
pca_scree<- ggplot(data=pca.var.per[1:8,], aes(x = PC, y=pca.var.per)) +
  geom_bar(stat="identity",
           colour = 'black',
           fill="#1C96BA",
           width=0.95) +
  xlab("Principal Component") +
  ylab("Percent Variation") +
  theme_minimal() +
  ggtitle(paste0("Scree Plot of PCA for PhS - ",imp_type))

pca_scree
ggsave(paste0("domains/pca_phs_",domain,"_scree.png"), 
       plot=pca_scree, width=4, height=4, dpi=320)




pca.data <- data.frame(SUBJID=as.character(rownames(pca$x)),
                       PC1=pca$x[,1],
                       PC2=pca$x[,2],
                       PC3=pca$x[,3],
                       PC4=pca$x[,4],
                       PC5=pca$x[,5])
#pca.data$Category <- substr(rownames(pca.data), 1, 3) # add category corresponding to each disease
dim(pca.data)
head(pca.data)



pca.data.saved <- pca.data
pca.data <- pca.data.saved
     

pca.data$OCS <- apply(DD_OCD_qc,1,sum)
k <- 10
pca.data$OCD <- 0
pca.data[pca.data$OCS < k,]$OCD <- 'Normal'
pca.data[pca.data$OCS > (k-1),]$OCD <- 'Mental'
table(pca.data$OCS)
View(pca.data)
mid<- 10
pca_plot <- ggplot(pca.data) +
  geom_point(aes(x=PC1, y=PC2, col = OCS), 
             #col = contour_cols[findInterval(dens, seq(0, max(dens), length.out = k))], 
             #col = colour.pca,
             pch = 16, 
             alpha = 1,
             size=2.5) +
  scale_color_gradient2(midpoint=mid, low="#2CA7D2", mid="#F4F7D2",
                        high="#DC904D", space ="Lab" )+
  #scale_colour_gradientn(colors = my_colors)
  labs(x = 'PC1', 
       y='PC2')  +
  xlab(paste("PC1 (EV: ", pca.var.per[1,1], "%)", sep="")) +
  ylab(paste("PC2 (EV: ", pca.var.per[2,1], "%)", sep="")) +
  theme_minimal() +
  ggtitle(paste0("PCA for ",domain))
pca_plot

ggsave(paste0("domains/",domain,"_PCA.png"), 
       plot=pca_plot, width=4.5, height=4, dpi=320)

k <- 13
pca.data$OCD <- 'empty'
pca.data[pca.data$OCS < k,]$OCD <- 'Normal'
pca.data[pca.data$OCS > (k-1),]$OCD <- 'Mental'


apply(pca.data, 2, function(x) any(is.na(x)))



pca_plot <- ggplot(pca.data) +
  geom_point(aes(x=PC1, y=PC2, col = OCD), 
             #col = contour_cols[findInterval(dens, seq(0, max(dens), length.out = k))], 
             #col = colour.pca,
             pch = 16, 
             alpha = 0.35,
             size=2.5) +
  #scale_color_gradient2(midpoint=mid, low="#2CA7D2", mid="#F4F7D2",
  #                      high="#DC904D", space ="Lab" )+
  #scale_colour_gradientn(colors = my_colors)
  labs(x = 'PC1', 
       y='PC2')  +
  xlab(paste("PC1 (EV: ", pca.var.per[1,1], "%)", sep="")) +
  ylab(paste("PC2 (EV: ", pca.var.per[2,1], "%)", sep="")) +
  theme_minimal() +
  ggtitle(paste0("PCA for ",domain,'_',k))
pca_plot

ggsave(paste0("domains/",domain,'domain_',k,'.png'), 
       plot=pca_plot, width=5, height=4, dpi=320)

#pca.data$SUBJID <- rownames(DD_OCD_qc)
View(pca.data)
write.csv(pca.data,file.path(paste0('./domains/phs_R_DD_pca_',domain,'.csv')), row.names = FALSE)

#write.csv(DD_phs_pca,file.path(paste0('../data/phs_R_DD_pca.csv')), row.names = FALSE)



#PCA

pca.data1 <- head(pca.data,20)
pca.data2 <-pca.data[order(pca.data$PC1, decreasing = TRUE),]
pca.data2 <- head(pca.data2,20)
pca.data3 <- tail(pca.data,20)

DD11 <- DD[which(rownames(DD) %in% pca.data1$SUBJID),]
DD22 <- DD[which(rownames(DD) %in% pca.data2$SUBJID),]
DD33 <- DD[which(rownames(DD) %in% pca.data3$SUBJID),]

sum(DD11)
sum(DD22)
sum(DD33)
