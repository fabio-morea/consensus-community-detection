# functions

# Load data 
load_data <- function(path, columns){
    files = list.files(path=path, pattern="dati*", all.files=TRUE, full.names=TRUE)
    nfiles = length(files)
    data <- files[1] %>% 
        read_delim( delim = "|", show_col_types = FALSE) %>%
        select(columns)  

    for (i in 2:nfiles){
        print(paste("Loading file ", files[i]))
        new <- files[i] %>% 
            read_delim(  delim = "|", show_col_types = FALSE) %>%
            select(columns) 

        data <- rbind(data,new) 
        
         
    }
    print(paste(nfiles, "files loaded."))
    return(data)
}