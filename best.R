best<-function(state,outcome){
  
  readres<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
    if (state %in% readres[,which(colnames(readres)=='State')]){
      
      if (outcome=='heart attack'){
        
        state_index<-grep(pattern = state,x=readres$State)
        num_out<-as.numeric(readres[state_index,match('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',names(readres))])
        min_death<-min(num_out,na.rm = TRUE)
        
        min_death_index<-grep(pattern = min_death,x = num_out)
        hospital_index<-state_index[1]+(min_death_index-1)
        ans<-readres[hospital_index,match('Hospital.Name',names(readres))]
        ans_final<-sort(ans)
        return(ans_final[1])
        
      }
      
      else if(outcome=='heart failure'){
        
        state_index<-grep(pattern = state,x=readres$State)
        num_out<-as.numeric(readres[state_index,match('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',names(readres))])
        min_death<-min(num_out,na.rm = TRUE)
        
        min_death_index<-grep(pattern = min_death,x = num_out)
        hospital_index<-state_index[1]+(min_death_index-1)
        ans<-readres[hospital_index,match('Hospital.Name',names(readres))]
        ans_final<-sort(ans)
        return(ans_final[1])
      
      }
      
      else if(outcome=='pneumonia'){
        
        state_index<-grep(pattern = state,x=readres$State)
        num_out<-as.numeric(readres[state_index,match('Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia',names(readres))])
        min_death<-min(num_out,na.rm = TRUE)
        
        min_death_index<-grep(pattern = min_death,x = num_out)
        hospital_index<-state_index[1]+(min_death_index-1)
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