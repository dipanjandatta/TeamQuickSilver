test_function = function(data_ts){

index_tr = nrow(data_tr)  
    
data2  <- rbind(data_tr,data_ts)

data2<-data2%>%
  mutate(y=ifelse(Status %in% c("Review Required"), 2, 
                  ifelse(Status %in% c("Normal Transaction"), 1, 0)))
continuous_vars0 <- as.data.frame(data2[,c(21:22,30:40)])

data2<-data2%>%
  mutate(New_or_Old_fl=ifelse(New_or_Old %in% c("New"), 1, 0))

ID <- data2[-c(1:index_tr),][,2]

#continuous_vars0 <- as.data.frame(data2[,c(21:22,30:39)])
##continuous_vars0 <- as.data.frame(data2[,c(21:22,30:40)])

attach(data2)
data_20 <- cbind.data.frame(Region,Sub_region,Frequent.Change.in.address,Nature_of_Claim,Customer.Demography.Segment,No..of.Providers)
detach(data2)

Region <- as.data.frame(as.factor(data_20$Region))
Sub_region <- as.data.frame(as.factor(data_20$Sub_region))
Frequent.Change.in.address <- as.data.frame(as.factor(data_20$Frequent.Change.in.address))
Nature_of_Claim <- as.data.frame(as.factor(data_20$Nature_of_Claim))
Customer.Demography.Segment <- as.data.frame(as.factor(data_20$Customer.Demography.Segment))
No..of.Providers <- as.data.frame(as.factor(data_20$No..of.Providers))

data21 <- cbind.data.frame(Region,Sub_region,Frequent.Change.in.address,Nature_of_Claim,Customer.Demography.Segment,No..of.Providers)



#data_20 <- as.data.frame(lapply(data_20,as.numeric))
names(data21)[1] <- paste("Region")
names(data21)[2] <- paste("Sub_region")
names(data21)[3] <- paste("Frequent.Change.in.address")
names(data21)[4] <- paste("Nature_of_Claim")
names(data21)[5] <- paste("Customer.Demography.Segment")
names(data21)[6] <- paste("No..of.Providers")


dummies <- stats::model.matrix(~Region+Sub_region+Frequent.Change.in.address+Nature_of_Claim+Customer.Demography.Segment+No..of.Providers , data21)

dummies<-as.data.frame(dummies)

data3 <- cbind(data2$y,dummies,continuous_vars0,data2$New_or_Old_fl)
data4 <- data3[,-2]
names(data4)[1] <- paste("y")
names(data4)[34] <- paste("New_or_Old_fl")

data5 <- as.matrix(data4)

#a[[1]] <- xgb.load('xgb.model')
combined_model_data_t <- data5[-c(1:index_tr),] 

model_list <- list()

for(i in 1:nrow(model_selector_2))
{
  model_list[[i]] <- xgb.load('xgb.model')
  j <- subset(model_selector_2,model_selector_2$Series_ID==i)[,1]
  y_pred_0 <- predict( model_list[[i]], combined_model_data_t[,expl_list[[j]]])
  
  if(i==1)
  {
    act_vs_pred_fin <- cbind.data.frame(combined_model_data_t[,1],y_pred_0,j,ID)
    names(act_vs_pred_fin)[1] <- paste("actual")
    names(act_vs_pred_fin)[2] <- paste("prediction")
    names(act_vs_pred_fin)[3] <- paste("varCombNumber")
    names(act_vs_pred_fin)[4] <- paste("Claim_ID")
  } else
  {act_vs_pred_fin_temp <- cbind.data.frame(combined_model_data_t[,1],y_pred_0,j,ID)
  names(act_vs_pred_fin_temp)[1] <- paste("actual")
  names(act_vs_pred_fin_temp)[2] <- paste("prediction")
  names(act_vs_pred_fin_temp)[3] <- paste("varCombNumber")
  names(act_vs_pred_fin_temp)[4] <- paste("Claim_ID")
  act_vs_pred_fin <- rbind(act_vs_pred_fin,act_vs_pred_fin_temp)
  rm(act_vs_pred_fin_temp)
  }
}  


vote_fin <- ddply(act_vs_pred_fin, c("Claim_ID","prediction"),summarise,
                  N=length(varCombNumber))
vote_fin_1 <- vote_fin[order(vote_fin$Claim_ID,-vote_fin$N),]
ID_df <- as.data.frame(ID)
ID_df$Int_ID <- seq.int(nrow(ID_df))


for(l in 1:nrow(ID_df))
{ID_df_l <- subset(ID_df,Int_ID==l)
ID_l <- ID_df_l[1,1]
vote_fin_1_l <-subset(vote_fin_1,vote_fin_1$Claim_ID==ID_l) 

if((vote_fin_1_l[1,3]/sum(vote_fin_1_l[,3])<0.9))
{vote_fin_1_l_f <-vote_fin_1_l[order(vote_fin_1_l$Claim_ID,vote_fin_1_l$N),] } else
{vote_fin_1_l_f <-vote_fin_1_l}

if(l==1)
{vote_fin_2 <- vote_fin_1_l_f} else
{vote_fin_2_temp <- vote_fin_1_l_f
vote_fin_2 <- rbind(vote_fin_2,vote_fin_2_temp)
rm(vote_fin_2_temp)
}

}

vote_unique <- data2[-c(1:index_tr),][,c(2,41)]
names(vote_unique)[1] <- paste("Claim_ID")
vote_prediction <- join(vote_unique,vote_fin_2,by = "Claim_ID",match = "first")
vote_prediction$accuracy_flag <- ifelse(vote_prediction$y==vote_prediction$prediction,1,0)
#final_accuracy <- sum(vote_prediction$accuracy_flag)/nrow(vote_prediction)

final_output_0 <- vote_prediction[,c(1,3)]
final_output_0$`Predicted Status` <- ifelse(final_output_0[,2]==2,"Review Required",ifelse(final_output_0[,2]==1,"Normal Transaction","Document Needed"))

final_output <- final_output_0[,c(1,3)]

return(final_output)

}