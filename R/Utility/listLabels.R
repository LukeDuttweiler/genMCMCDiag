listLabels <- function(lst){
  #Which vals are duplicated?
  dupVals <- duplicated(lst)
  
  #Create list to save unique values and vector to save unique labels
  uniqueVals <- vector('list', sum(!dupVals))
  uniqueLabels <- vector('character', sum(!dupVals))
  
  #Create vector of labels for the values of lst
  lstLabels <- vector('character', length(dupVals))
  
  #Index of how many spots in uniqueVals and labels have been filled
  j <- 0
  
  #Loop over list, different action if duplicated vs not
  for(i in 1:length(dupVals)){
    #If duplicated
    if(dupVals[i]){
      #Index of the original value this is a duplicate of
      origIndex <- which(sapply(uniqueVals, identical, y = lst[[i]]))
      
      #Assign label
      lstLabels[i] <- uniqueLabels[origIndex]
    }else{ #If not duplicated, add to uniqueVals and add a new label
      #Iterate j (index of where we are in uniqueVals and labels)
      j <- j+1
      
      #Add value to uniqueVals
      uniqueVals[[j]] <- lst[[i]]
      
      #Add unique label
      uniqueLabels[j] <- as.character(j)
      
      #Assign label to final output
      lstLabels[i] <- as.character(j)
    } #End if/else
  }#End Loop
  
  return(lstLabels)
}