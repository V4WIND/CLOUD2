###############################3
####Author: Nalin Singla

entropy_test <- function(variable_cut,variable_target,data_table_name){

FinalData <- data_table_name
cols <- c(variable_cut,variable_target,'overall')
col_name <- variable_cut
FinalData[,overall:=1]
FinalData_sample <- FinalData[,..cols]


min=min(FinalData_sample[,get(variable_cut)])
max=quantile(FinalData_sample[,get(variable_cut)],0.99)
interval = (max-min)/100

test_col <- seq(from = min, to = max, by = interval)

H <- array(0,dim=c(0,length(test_col)))

cnt=1;
#setkey(FinalData_sample,c(variable_cut))
for (i in test_col){
  print(i)
  S1 <- FinalData_sample[get(col_name)>=i]
  S <- nrow(FinalData_sample)
  S2 <- FinalData_sample[get(col_name)<i]
  nrow_S1 <- nrow(S1)
  nrow_S2 <- nrow(S2)
  H1 <- - (sum(S1[get(variable_target)==1,overall])/nrow_S1)*
    log(sum(S1[get(variable_target)==1,overall])/nrow_S1)-
    (sum(S1[get(variable_target)==0,overall])/nrow_S1)*
    log((sum(S1[get(variable_target)==0,overall])/nrow_S1))
  H2 <- - (sum(S2[get(variable_target)==1,overall])/nrow_S2)*
    log(sum(S2[get(variable_target)==1,overall])/nrow_S2)-
    (sum(S2[get(variable_target)==0,overall])/nrow_S2)*
    log(sum(S2[get(variable_target)==0,overall])/nrow_S2)
  print(H1)
  print(H2)
  H_temp <- (nrow_S1*H1 + nrow_S2*H2)/S
  H[cnt] <- H_temp
  cnt <- cnt+1
}

#H <- -H
plot(test_col,H)

file_name <- cbind(test_col,H)

#write.csv(file_name,'D:/Entropy Test/age.csv')
}

entropy_test('age','RFS_PRODUCT_IND1',FinalData)
