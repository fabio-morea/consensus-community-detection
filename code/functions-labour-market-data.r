# functions

# Load data 
load_data <- function(path, columns, debug = FALSE){
    files = list.files(path=path, pattern="dati*", all.files=TRUE, full.names=TRUE)
    nfiles = length(files)

    if (debug == TRUE) {nfiles <- 2 } #only for debug puropses

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

# make a conversion table to re-identify emplo

  
make.ids.conversion.table <- function(employees){
        
    sorted.employees <- employees %>%
        mutate(data_inizio = min(data_inizio))%>%
        mutate(data_fine = max(data_fine))%>%
        arrange(data_nascita, iso3, genere, data_inizio, id_cittadino)%>%
        group_by(data_nascita,  iso3, genere)%>%
        unique()

    ii<- 1
    ids_conversion_table = tibble(
        idempl=as.integer(0),
        id_cittadino=".")
                
    d_fin_prec = ymd("1900-01-01")
    idprec="x"
    dob_prev=ymd("1900-01-01")
    nat_prev="x"
    gen_prev="x"
    nnnn = nrow(sorted.employees)
 
    for(i in 1:nnnn) {
        idcit = sorted.employees$id_cittadino[i]
        dob = sorted.employees$data_nascita[i]
        nat = sorted.employees$iso3[i]
        gen = sorted.employees$genere[i]
        d_ini = sorted.employees$data_inizio[i]
        interval = time_length(d_ini-d_fin_prec,"years")
        
        if (idcit == idprec) {
            #same id hence same person
            #print(paste("same person", ii,idcit,idprec))
            d_fin_prec=d_ini
        }
        else {
            #different id
            if( (interval >= 0) & (dob==dob_prev)& (nat==nat_prev)& (gen==gen_prev)) {
                #interval ok, it is the same person despite differente id
                d_fin_prec=d_ini
                #print(paste("different id, all the rest ok", ii,idcit,idprec))
        }
        else
        {
            #different id and interval not consistent: it's another person
            #print(paste("different person ", interval))
            ii=ii+1
            d_fin_prec = ymd("1900-01-01")
            #print(paste("different id, wrong interval ", ii,idcit,idprec))
        }
        }
        
        
        idprec = idcit
        dob_prev=dob
        nat_prev=nat
        gen_prev=gen
        ids_conversion_table<-ids_conversion_table%>% add_row(idempl=ii,id_cittadino=idcit )

    }

    return(ids_conversion_table)


}