rankhospital<-function(state, outcome, num = "best"){
  readres<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  if (state %in% readres[,which(colnames(readres)=='State')]){
    if (outcome=='heart attack'){
      state_index<-grep(pattern = state,x=readres$State)
      num_out<-as.numeric(readres[state_index,match('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',names(readres))])
      num_out_rank<-as.numeric(factor(num_out))
      if(as.numeric(num)<=max(num_out_rank,na.rm = TRUE)|as.character(num)=="best"|as.character(num)=="worst"){
        if (num=='best'){
          rank_index<- match(1,num_out_rank)
        }
        else if(num=='worst'){
          rank_index<-match(max(num_out_rank,na.rm = TRUE),num_out_rank)
        }
        else{
          rank_index<-match(as.numeric(num),num_out_rank)
        }
      }
      else{
        return(NA)
      }
      hospital_index<-state_index[1]+(rank_index-1)
      ans<-readres[hospital_index,match('Hospital.Name',names(readres))]
      ans_final<-sort(ans)
      return(ans_final[1])
    }
    else if(outcome=='heart failure'){
      state_index<-grep(pattern = state,x=readres$State)
      num_out<-as.numeric(readres[state_index,match('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',names(readres))])
      num_out_rank<-as.numeric(factor(num_out))
      if(as.numeric(num)<=max(num_out_rank,na.rm = TRUE)|as.character(num)=="best"|as.character(num)=="worst"){
        if (num=='best'){
          rank_index<- match(1,num_out_rank)
        }
        else if(num=='worst'){
          rank_index<-match(max(num_out_rank,na.rm = TRUE),num_out_rank)
        }
        else{
          rank_index<-match(as.numeric(num),num_out_rank)
        }
      }
      else{
        return(NA)
      }
      hospital_index<-state_index[1]+(rank_index-1)
      ans<-readres[hospital_index,match('Hospital.Name',names(readres))]
      ans_final<-sort(ans)
      return(ans_final[1])
    }
    else if(outcome=='pneumonia'){
      
      state_index<-grep(pattern = state,x=readres$State)
      num_out<-as.numeric(readres[state_index,match('Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia',names(readres))])
      num_out_rank<-as.numeric(factor(num_out))
      if(as.numeric(num)<=max(num_out_rank,na.rm = TRUE)|as.character(num)=="best"|as.character(num)=="worst"){
        if (num=='best'){
          rank_index<- match(1,num_out_rank)
        }
        else if(num=='worst'){
          rank_index<-match(max(num_out_rank,na.rm = TRUE),num_out_rank)
        }
        else{
          rank_index<-match(as.numeric(num),num_out_rank)
        }
      }
      else{
        return(NA)
      }
      hospital_index<-state_index[1]+(rank_index-1)
      ans<-readres[hospital_index,match('Hospital.Name',names(readres))]
      ans_final<-sort(ans)
      return(ans_final[1])
    }
    else{
      stop(paste("invalid outcome"))
    }
  }
  else{
    stop(paste("invalid state"))
  }
}