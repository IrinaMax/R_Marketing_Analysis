library(dplyr)


adult <- read.csv('adult_sal.csv')
print(head(adult))
adult <- select(adult, -X)
print(str(adult))
print(summary(adult))
table(adult$type_employer)
##
## Data cleaning
##
## Combine employer type
unemp <- function(job){
  job <- as.character(job)
  if (job == 'Never-worked' | job == 'Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
 }
## Apply to the adult$
adult$type_employer <- sapply(adult$type_employer, unemp)
###
print(table(adult$type_employer))

### Group Self-employed and State and Local
group_emp <- function(job){
  job <- as.character(job)
  if (job == 'Local-gov' | job == 'State-gov'){
    return('SL-gov')
  }else if(job == 'Self-emp-inc' | job == 'Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}
###
adult$type_employer <- sapply(adult$type_employer, group_emp)
print(table(adult$type_employer))

group_marital <- function(mar){
  mar <- as.character(mar)
  
  #for not married
  if (mar=='Separated' |mar =='Divorced' | mar=='Widowed'){
    return('Not married')
    
    # for never married
  }else if(mar=='Never-married'){
    return(mar)
    
    # for Married
  }else{
    return('Married')
  }
}

