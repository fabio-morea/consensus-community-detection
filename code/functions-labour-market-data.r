# functions

# Load data 
load_data <- function(path, columns,  echo = FALSE, debug = FALSE){

    if (debug == TRUE) {print("Debug mode: only 2 files will be loaded")} #only for debug puropses
    if (echo == TRUE) {print(paste("Start loading data from ", path) ) }  #only for debug puropses

    
    files = list.files(path=path, pattern="dati*", all.files=TRUE, full.names=TRUE)
    nfiles = length(files)

    if (debug == TRUE) {nfiles <- 2 } #only for debug puropses

    data <- files[1] %>% 
        read_delim( delim = "|", show_col_types = FALSE) %>%
        select(all_of(columns))  

    for (i in 2:nfiles){
        print(paste("Loading file ", files[i]))
        new <- files[i] %>% 
            read_delim(  delim = "|", show_col_types = FALSE) %>%
            select(columns) %>%
            unique()

        data <- rbind(data,new) 
        
         
    }
    if (echo == TRUE) {print(paste(nfiles, "files loaded."))}    #only for debug puropses
    return(data)
}

# make a conversion table to re-identify employees
make.ids.conversion.table <- function(data, echo= FALSE){
    if (echo == TRUE) {print("Start ids conversion table ") }          #only for debug puropses
    sorted.employees <- data %>%
        mutate(data_inizio = min(data_inizio))%>%
        mutate(data_fine = max(data_fine))%>%
        mutate(avvio = ifelse(saldo==1,TRUE,FALSE))%>%
        arrange(data_nascita, iso3, genere, data_inizio, id_cittadino, saldo)%>%
        group_by(data_nascita,  iso3, genere)%>%
        unique()

    ii<- 1
    ids_conversion_table = tibble(
        idempl=as.integer(0),
        id_cittadino=".")
                
    d_fin_prec = ymd("1900-01-01")
    idprec="x"
    dob_prev = ymd("1900-01-01")
    nat_prev ="x"
    gen_prev ="x"
    nnnn = nrow(sorted.employees)
 
    for(i in 1:nnnn) {
        idcit = sorted.employees$id_cittadino[i]
        dob = sorted.employees$data_nascita[i]
        nat = sorted.employees$iso3[i]
        gen = sorted.employees$genere[i]
        d_ini = sorted.employees$data_inizio[i]

        if (sorted.employees$avvio[i]==TRUE){
            interval = time_length(d_ini-d_fin_prec,"years")
        }
        else{
            interval = -1.0
        }
        
        if (idcit == idprec) {
            #same id hence same person
            #print(paste("same person", ii,idcit,idprec))
            d_fin_prec=d_ini
        }
        else {
            #different id
            if( (interval >= 0) & (dob==dob_prev)& (nat==nat_prev)& (gen==gen_prev)) {
                #interval ok, saldo == +1, hence it is the same person despite differente id
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

    if (echo == TRUE) {print("Done.Check output in tmp folder.") }          #only for debug puropses

    return(ids_conversion_table)
}

# mate a table of transitions #################################################
make.transitions.table <- function(contracts, echo= FALSE){
    #debug.examples <-c(1716,1549)
    if (echo == TRUE) {print("Start transition table ") }                   #only for debug puropses

    experience <- contracts %>%
        select(idempl,qualifica_codice,CF,data_inizio,data_fine)%>%
        mutate(dd= replace_na(data_fine, ymd(today())))%>%
        mutate(durat = time_length(dd - data_inizio, 'years'))%>%
        filter(durat>0)%>%
        arrange(idempl,data_inizio)%>%
        unique()
    
    transitions = tibble(
        empl=0, 
        cf1=".", 
        cf2=".", 
        qualif=".",
        date_end1   = ymd("1900-01-01"),
        date_start2 = ymd("1900-01-01"),
        gap=0.0,
        ww=0)

    idcs <- unique(contracts$idempl)  
    for(iii in idcs){
        tmp = experience%>% filter(idempl==iii)
        ncontracts = nrow(tmp)
        nn=ncontracts-1
        if (ncontracts>1){
            for (i in 1:nn){
                if (iii %in% debug.examples ){
                    print(paste("******* Procesing employee ", iii ))
                    print(paste("contract i", i, " from ", tmp$data_inizio[i], " to ", tmp$data_fine[i]))
                    print(paste("contract i+1", i, " from ", tmp$data_inizio[i+1], " to " ,tmp$data_fine[i+1]))
                    print("---")
                }
                empl <- tmp$idempl[i+1]
                cf1  <- tmp$CF[i]
                cf2  <- tmp$CF[i+1]
                qualif = tmp$qualifica_codice[i+1]
                d_start2 <- ymd(tmp$data_inizio[i+1])
                tempdate <- ymd(tmp$data_fine[i])
                if (is.na(tempdate))   # nolint
                    {d_end1 <- d_start2}
                else 
                    {d_end1   <- tempdate}
                gap = round(time_length(d_start2 - d_end1, 'years'), 3)
                ww = round(as.numeric(tmp$durat[i+1]),3) #weight is the duration in the second company
                if (is.na(gap)){gap = -1}
                if (gap < 0 ){
                    d_end1<-d_start2
                    gap<-0
                    #print("Reset gap = 0")
                }
                transitions <- transitions %>% add_row(
                    empl=empl,
                    cf1=cf1, 
                    cf2=cf2,
                    qualif=qualif,
                    date_end1   = d_end1,
                    date_start2 = d_start2,
                    gap=gap,
                    ww=ww)

                if (iii %in% debug.examples ){
                    print(paste("Date End 1",d_end1))
                    print(paste("Date Start 2",d_start2))
                }

             } 
        }
    }
    transitions <- transitions %>% 
        arrange(d_start2,empl)%>%
        mutate(loop = if_else(cf1==cf2, "o", "-"))


    #aggregate transition of same employee with same company
    clean_transitions = transitions%>%head(0)
    cumulate_weight=0
    empl_prec=0
    for(iii in 1:nrow(transitions)){
        tmp = transitions[iii,] #extract one row
        if (tmp$empl != empl_prec){cumulate_weight=0}
        if (tmp$loop == "o") {
            cumulate_weight=cumulate_weight+tmp$ww}
        else{
            cumulate_weight=tmp$ww
            clean_transitions <- clean_transitions%>% 
            add_row(date_start2=tmp$date_start2,
                    date_end1=tmp$date_end1,
                    empl=tmp$empl,
                    cf1=tmp$cf1, 
                    cf2=tmp$cf2,
                    qualif=tmp$qualif,
                    ww=cumulate_weight, 
                    gap=tmp$gap,
                    loop=tmp$loop)
    }

    empl_prec=tmp$empl
    }
    if (echo == TRUE) {print("Done.Check output in tmp folder.") }          #only for debug puropses
    return(clean_transitions)
}


make.organisations.table <- function(data, selected.organisations){
    #print(data$CF)
    org_locations = data %>% 
    select(CF, az_ragione_soc, 
        sede_op_comune,sede_op_indirizzo,sede_op_provincia,
         SLL_nome,sede_op_ateco,)%>%
        unique()%>%
    filter(CF %in%  selected.organisations)

    #add a unique id for each local unit 
    org_locations<- org_locations%>%
        group_by(CF, sede_op_comune, sede_op_indirizzo)%>%
        mutate(local_unit_id = cur_group_id())%>%
        relocate(local_unit_id)%>%
        ungroup()%>%
        unique()
    return(org_locations)
 
}