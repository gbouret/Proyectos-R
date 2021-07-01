library(tidyverse)

remove_at <- function(df){
    
    df <- df %>%
        mutate(text_clean2 = gsub("@\\w+", "", df$text));
                   
    return(df)
}


getOutputSchema <- function() {
    return(data.frame(
        user_id = prep_string(),
        text = prep_string(),
        text_clean2 = prep_string()
    ));
}

