library(dplyr)
rankall<-function(outcome,num='best'){
  readres<-read.csv("outcome-of-care-measures.csv",colClasses = 'character')
  if (outcome=='heart attack'){
    tran_df<-readres%>%group_by(State)%>%arrange(Hospital.Name)%>%mutate(rank=rank(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),na.last = 'keep',ties.method = 'first'))
    if (as.numeric(num)<=max(tran_df$rank,na.rm=TRUE)|as.character(num)=='best'|as.character(num)=='worst'){
      if (as.character(num)=='best'){
        ans_df<-data.frame(Hospital=tran_df$Hospital.Name[which(tran_df$rank==1)],State=tran_df$State[which(tran_df$rank==1)])
        return(ans_df%>%arrange(State))
      }
      else if (as.character(num=='worst')){
        ans_df<- tran_df%>%group_by(State)%>%filter(rank==max(rank,na.rm = TRUE))
        return(data.frame(Hospital=ans_df$Hospital.Name,State=ans_df$State)%>%arrange(State))
      }
      else{
        ans_df<-tran_df%>%group_by(State)%>%filter(rank==as.numeric(num))
        return(data.frame(Hospital=ans_df$Hospital.Name,State=ans_df$State)%>%arrange(State))
      }
    }
    else{
      return(NA)
    }
  }
  else if (outcome=='heart failure'){
    tran_df<-readres%>%group_by(State)%>%arrange(Hospital.Name)%>%mutate(rank=rank(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),na.last = 'keep',ties.method = 'first'))
    if (as.numeric(num)<=max(tran_df$rank,na.rm=TRUE)|as.character(num)=='best'|as.character(num)=='worst'){
      if (as.character(num)=='best'){
        ans_df<-data.frame(Hospital=tran_df$Hospital.Name[which(tran_df$rank==1)],State=tran_df$State[which(tran_df$rank==1)])
        return(ans_df%>%arrange(State))
      }
      else if (as.character(num=='worst')){
        ans_df<- tran_df%>%group_by(State)%>%filter(rank==max(rank,na.rm = TRUE))
        return(data.frame(Hospital=ans_df$Hospital.Name,State=ans_df$State)%>%arrange(State))
      }
      else{
        ans_df<-tran_df%>%group_by(State)%>%filter(rank==as.numeric(num))
        return(data.frame(Hospital=ans_df$Hospital.Name,State=ans_df$State)%>%arrange(State))
      }
    }
    else{
      return(NA)
    }
  }
  else if (outcome=='pneumonia'){
    tran_df<-readres%>%group_by(State)%>%arrange(Hospital.Name)%>%mutate(rank=rank(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),na.last = 'keep',ties.method = 'first'))
    if (as.numeric(num)<=max(tran_df$rank,na.rm=TRUE)|as.character(num)=='best'|as.character(num)=='worst'){
      if (as.character(num)=='best'){
        ans_df<-data.frame(Hospital=tran_df$Hospital.Name[which(tran_df$rank==1)],State=tran_df$State[which(tran_df$rank==1)])
        return(ans_df%>%arrange(State))
      }
      else if (as.character(num=='worst')){
        ans_df<- tran_df%>%group_by(State)%>%filter(rank==max(rank,na.rm = TRUE))
        return(data.frame(Hospital=ans_df$Hospital.Name,State=ans_df$State)%>%arrange(State))
      }
      else{
        ans_df<-tran_df%>%group_by(State)%>%filter(rank==as.numeric(num))
        return(data.frame(Hospital=ans_df$Hospital.Name,State=ans_df$State)%>%arrange(State))
      }
    }
    else{
      return(NA)
    }
  }
  else{
    stop(paste('invalid outcome'))
  }
}