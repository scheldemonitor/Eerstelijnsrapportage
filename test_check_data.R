
cleanSMdata2 <- function(df, checklimits = TRUE, removeoutliers = TRUE, ...) 
{
  
  if(!is.data.frame(df)) stop("The input provided is not a dataframe")
  if(is.null(df$valuesign)) stop("There appears to be no column named 'valuesign'")
  if(checklimits) {  
    df <- df %>% 
      mutate(value = case_when(
        valuesign == "=" ~ value, 
        valuesign == "<" ~ 0.5 * value, 
        valuesign == ">" ~ NA_real_))
  }
  if(removeoutliers) {
    # groups <- enquo(outliergroups)
    df <- df %>% group_by(!!! ensyms(...)) %>% mutate(value = remove_outliers(value))
  }
}

df2 <- cleanSMdata2(df, checklimits = T, removeoutliers = TRUE, "parametername", "stationname")

# look at diff
head(df, 1000) %>% anti_join(head(df2, 1000)) %>% View()

