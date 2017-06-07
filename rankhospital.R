library(dplyr)
rankhospital<-function(state, outcome, num = "best"){
  readres<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  if (state %in% readres[,which(colnames(readres)=='State')]){
    if (outcome=='heart attack'){
      num_df<-readres[readres$State==state,]
      num_df<-num_df%>%arrange(Hospital.Name)%>%mutate(rank=rank(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),na.last = "keep",ties.method = "first"))
      if(as.numeric(num)<=max(num_df$rank,na.rm = TRUE)|as.character(num)=="best"|as.character(num)=="worst"){
        if (num=='best'){
          hospital_name<-num_df$Hospital.Name[match(1,num_df$rank)]
          return(hospital_name)
        }
        else if(num=='worst'){
          hospital_name<-num_df$Hospital.Name[match(max(num_df$rank,na.rm = TRUE),num_df$rank)]
          return(hospital_name)
        }
        else{
          hospital_name<-num_df$Hospital.Name[match(as.numeric(num),num_df$rank)]
          return(hospital_name)
        }
      }
      else{
        return(NA)
      }
     
    }
    else if(outcome=='heart failure'){
      num_df<-readres[readres$State==state,]
      num_df<-num_df%>%arrange(Hospital.Name)%>%mutate(rank=rank(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),na.last = "keep",ties.method = "first"))
      if(as.numeric(num)<=max(num_df$rank,na.rm = TRUE)|as.character(num)=="best"|as.character(num)=="worst"){
        if (num=='best'){
          hospital_name<-num_df$Hospital.Name[match(1,num_df$rank)]
          return(hospital_name)
        }
        else if(num=='worst'){
          hospital_name<-num_df$Hospital.Name[match(max(num_df$rank,na.rm = TRUE),num_df$rank)]
          return(hospital_name)
        }
        else{
          hospital_name<-num_df$Hospital.Name[match(as.numeric(num),num_df$rank)]
          return(hospital_name)
        }
      }
      else{
        return(NA)
      }
     }
    else if(outcome=='pneumonia'){
      
      num_df<-readres[readres$State==state,]
      num_df<-num_df%>%arrange(Hospital.Name)%>%mutate(rank=rank(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),na.last = "keep",ties.method = "first"))
      if(as.numeric(num)<=max(num_df$rank,na.rm = TRUE)|as.character(num)=="best"|as.character(num)=="worst"){
        if (num=='best'){
          hospital_name<-num_df$Hospital.Name[match(1,num_df$rank)]
          return(hospital_name)
        }
        else if(num=='worst'){
          hospital_name<-num_df$Hospital.Name[match(max(num_df$rank,na.rm = TRUE),num_df$rank)]
          return(hospital_name)
        }
        else{
          hospital_name<-num_df$Hospital.Name[match(as.numeric(num),num_df$rank)]
          return(hospital_name)
        }
      }
      else{
        return(NA)
      }
    }
    else{
      stop(paste("invalid outcome"))
    }
  }
  else{
    stop(paste("invalid state"))
  }
}