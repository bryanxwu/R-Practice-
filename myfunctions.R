function.subset<-function(df,df_small_amt){
  subset(df, is.na(df_small_amt)==FALSE)
}

# subsetting away NA's where df is data frame and df_small_amt is criteria for NA values, representing the called column of the data frame


# df refers to the test dataframe, kvector refers to the list of discount rates, df_large_amt refers to the called large,delayed value of the dataframe, df_delay_amt is the called length of delay column, 
# df_small_amt is the called small, immediate column, and df_choice is the called participants' choice column. Called refers to accessing column with $ sign.

calc.parameter<-function(df,kvector,df_large_amt, df_delay_amt, df_small_amt, df_choice){ # this is the function to call upon
  
  match.percent<-function(df,kvalue,df_large_amt, df_delay_amt, df_small_amt, df_choice){ # function to find match percent for specific kvalue
    predicted.choice<-ifelse((df_large_amt/(1+(kvalue)*df_delay_amt))>df_small_amt,2,1)
    matched.percent<-sum(df_choice==predicted.choice)/length(df_choice)
    return(data.frame(k=kvalue, consistency=matched.percent))
  }
  
  kvalues.df<-data.frame(index=seq(1:length(kvector)),kvalue=kvector) # generate a dataframe to iterate through all kvalues
  
  match.df<-kvalues.df %>% # commands to loop through kvalues and run percent match, returning a dataframe with highest percent match
    group_by(index) %>%
    do(match.percent(df,kvalue=.$kvalue,df_large_amt,df_delay_amt,df_small_amt,df_choice))%>%
    filter(consistency==max(.$consistency))
  
  parameter.df<-data.frame(parameter=prod(match.df$k)^(1/nrow(match.df)),consistency=unique(match.df$consistency))
  return(parameter.df) # finds geometric mean if multiple max percent matches 
}












