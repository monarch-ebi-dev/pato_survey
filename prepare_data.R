library(data.table)
library(stringr)
library(plyr)
library(DT)

datadir<-"/data/pato_survey_data/"
ontologypath<-"/data/tmp_pato/ontologies/"
ws = "/ws/pato_survey/pato_survey_obo.RData"
p_excluded = c("pato","ncbitaxon")

get_csv<-function(dir,identifier) {
  temp = paste(dir,list.files(dir,pattern=paste("patosurvey_",identifier,"_.*.csv",sep="")),sep="")
  temp = temp[file.size(temp) > 1]
  
  l <- lapply(temp, fread, sep=",",stringsAsFactors = FALSE,header=TRUE)
  l <- mapply(cbind,l,csv=temp,SIMPLIFY = FALSE)
  df <- rbindlist( l ,use.names = TRUE, fill = TRUE)
  
  if(identifier!="corpus") {
    if("o" %in% names(df)) {
      df[,fn:=o]
      df[,o:=gsub(".owl","",fn)]
      df[,corpus:=sapply(strsplit(df$o,"_"), `[`, 1)]
      df[,ftype:=sapply(strsplit(df$o,"_"), `[`, 2)]
      df[,o:=str_replace_all(pattern=paste(corpus,"_",sep=""),replacement = "",o)]
      df[,o:=str_replace_all(pattern=paste(ftype,"_",sep=""),replacement = "",o)]
    }
  } else {
    df[,o:=tolower(o)]
  }
  return(df)
}

successfull_collection<-function(dir,identifier) {
  temp = paste(dir,list.files(dir,pattern=paste("patosurvey_",identifier,"_.*.csv",sep="")),sep="")
  f<-gsub(".owl","",gsub(dir,"",temp))
  f<-gsub(paste("rosurvey_",identifier,"_",sep=""),"",f)
  f<-gsub(".*_","",f)
  f<-gsub(".csv","",f)
  return(unique(f))
}

df_axiomdata<-get_csv(datadir,"axiomdata")
df_allaxiomdata<-get_csv(datadir,"allaxiomdata")
df_expressiondata<-get_csv(datadir,"expressiondata")
df_classdata<-get_csv(datadir,"classdata")
df_label<-get_csv(datadir,"labels")
df_corpus<-get_csv(datadir,"corpus")
df_merge<-get_csv(datadir,"merge")


success_o<-successfull_collection(datadir,"axiomdata")
# Prepare success data
df_merge[,merged_success:=merged_success==1]
df_merge[,remove_success:=remove_success==1]

df_corpus<-merge(df_corpus,df_merge[,.(filepath,merged_success,remove_success)],by=c("filepath"),all.x = TRUE)
df_corpus$merged_success<-ifelse(is.na(df_corpus$merged_success),FALSE,df_corpus$merged_success)
df_corpus$remove_success<-ifelse(is.na(df_corpus$remove_success),FALSE,df_corpus$remove_success)
df_corpus$collectdata_success <- ifelse((df_corpus$o %in% success_o),TRUE,FALSE)
df_corpus$excluded <- ifelse((df_corpus$o %in% p_excluded),TRUE,FALSE)
ct_download_sucess<-count(df_corpus[,.(corpus,download_success,merged_success,collectdata_success,excluded)])

valid_ontologies<-(df_corpus[df_corpus$collectdata_success&!df_corpus$excluded,]$o)

df_allaxiomdata<-df_allaxiomdata[(o %in% valid_ontologies),]
df_axiomdata<-df_axiomdata[(o %in% valid_ontologies),]
df_expressiondata<-df_expressiondata[(o %in% valid_ontologies),]
df_classdata<-df_classdata[(o %in% valid_ontologies),]

df_classdata[,label:=NULL]
df_pato<-df_label



# Prepare property data
df_p<-unique(df_classdata[,.(o,ftype,corpus,iri)])
df_p<-merge(df_p,df_pato,by.x = "iri",by.y = "term",all.x = TRUE)
df_p$corpussize=length(valid_ontologies)

# Prepare axiom data
df_ax<-unique(df_axiomdata[axiomtype!="Declaration",.(o,ftype,corpus,axiomtype,axiomid,iri,pato_only)])
df_ax<-merge(df_ax,df_pato,by.x = "iri",by.y = "term",all.x = TRUE)

# Prepare expressiondata
df_exp<-unique(df_expressiondata[,.(o,ftype,corpus,expressiontype,axiomid,expressionid,iri,p)])
df_expo<-unique(df_expressiondata[,.(o,ftype,corpus,expressiontype,iri)])

df_allaxiomdata<-unique(df_allaxiomdata[,.(o,axiomtype,axiomid,ftype,corpus)])
ct_allaxiomdata<-plyr::count(df_allaxiomdata[,.(o,axiomtype,ftype,corpus)])


df_domainrangedata<-NULL
df_axiomdata<-NULL
df_expressiondata<-NULL
df_property<-NULL
df_allaxiomdata<-NULL
save.image(file=ws)
