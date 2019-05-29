#!/usr/bin/Rscript

# Chagement du jeu de données
# aucs_com = read.csv("aucs_com_gLouv.csv", header = TRUE)
# aucs_df = read.csv("aucs_df.csv", header = TRUE)

aucs_com = read.csv("data/florent_com.csv", header = TRUE)
aucs_df = read.csv("data/florent_df.csv", header = TRUE)
init<-function(dataframe,dataframe_comp) {
    aucs_com <- dataframe
    aucs_df <- dataframe_comp
}

count_occurence_in_column <- function(occurence_value,column) {
  occurences<-table(unlist(column))
  as.numeric(occurences[occurence_value])
}

generate_dataframe_aucs_integration <- function() {
    dataframe_aucs_integration <- data.frame (
            id=numeric(),
            actor_source=character(),
            layer_source=character(),
            actor_target=character(),
            layer_target=character(),
            dir=numeric(), 
            layer_count_for_target=numeric(),
            stringsAsFactors=FALSE
        )

    
    for (index in 1:nrow(aucs_df)) {
        row <- aucs_df[index,]

        id_value <- as.numeric(row["id"])
        from_actor_value <- as.character(row["from_actor"][[1]])
        to_actor_value <- as.character(row["to_actor"][[1]])
        from_layer_value <- as.character(row["from_layer"][[1]])
        to_layer_value <- as.character(row["to_layer"][[1]])
        dir_value<- as.numeric(row["dir"][[1]])

        layer_dataframe <- subset(aucs_com, actor == to_actor_value & layer!=from_layer_value)

        single_dataframe <-data.frame( 
            id = id_value,
            actor_source = from_actor_value,
            layer_source= from_layer_value,
            actor_target = to_actor_value,
            layer_target = to_layer_value,
            dir= dir_value,
            layer_count_for_target = nrow(layer_dataframe)
        )
        dataframe_aucs_integration = rbind(dataframe_aucs_integration, single_dataframe)
    }
    dataframe_aucs_integration <- unique(dataframe_aucs_integration)
}

are_same_community <- function(cid_value, actor_source_value,actor_target_value){
    are_same <- FALSE
    actor_1 <- subset(aucs_com, actor == actor_source_value & cid == cid_value)
    actor_2 <- subset(aucs_com, actor == actor_target_value & cid == cid_value)

    if(nrow(actor_1) > 0 & nrow(actor_2) > 0)
        are_same <- TRUE 
    are_same

}

get_dataframe_Pi <- function(cid_value,P){
    Pc <- data.frame (
                actor_source=character(),
                actor_target=character(),
                cid=integer(),
                stringsAsFactors=FALSE
            )

        for (i in 1:nrow(P)){
            row <- P[i,]
            actor_source_value <- as.character(row["actor_source"][[1]])
            actor_target_value <- as.character(row["actor_target"][[1]])
            flag = are_same_community(as.integer(cid_value),actor_source_value,actor_target_value)

            if (flag) {
                    single_dataframe <-data.frame( 
                    actor_source = actor_source_value,
                    actor_target = actor_target_value,
                    cid = cid_value
                )
                Pc = rbind(Pc, single_dataframe)
            }
    }
    Pc <- unique(Pc)
    Pc
}

get_dataframe_Pc <- function(P){
    Pc <- data.frame (
                actor_source=character(),
                actor_target=character(),
                cid=integer(),
                stringsAsFactors=FALSE
            )
    cids <- unique(aucs_com["cid"])
    for(index in 1:nrow(cids)){
        cid_value <- cids[index,]
        for (i in 1:nrow(P)){
            row <- P[i,]
            actor_source_value <- as.character(row["actor_source"][[1]])
            actor_target_value <- as.character(row["actor_target"][[1]])
            flag = are_same_community(as.integer(cid_value),actor_source_value,actor_target_value)

            if (flag) {
                    single_dataframe <-data.frame( 
                    actor_source = actor_source_value,
                    actor_target = actor_target_value,
                    cid = cid_value
                )
                Pc = rbind(Pc, single_dataframe)
            }
        }
        # print(paste(cid_value," ",nrow(Pc)))
    }
    Pc <- unique(Pc)
    Pc
}

get_target <- function(actor_value,list){
    edge <- data.frame (
        neighbor=character(),
        stringsAsFactors=FALSE
    )
    for(i in 1:nrow(list)){
        actor_target = NA
        row = list[i,]
        if(actor_value == row["to_actor"])
            actor_target = as.character(row["from_actor"][[1]])
        else
            actor_target = as.character(row["to_actor"][[1]])

        single_dataframe <-data.frame( 
                neighbor = actor_target
            )

            if(!is.na(actor_target)){
                edge = rbind(edge, single_dataframe)
            }
    }
    edge = unique(edge)
    edge

}

get_target_by_cid_value <- function(cid_value,actor_value,list){
    edge <- data.frame (
        neighbor=character(),
        stringsAsFactors=FALSE
    )
    for(i in 1:nrow(list)){
        actor_target = NA
        row = list[i,]
        if(actor_value == row["to_actor"])
            actor_target = as.character(row["from_actor"][[1]])
        else
            actor_target = as.character(row["to_actor"][[1]])

        count_item = nrow(unique(subset(aucs_com, actor==actor_target & cid ==cid_value)))

        if(!is.na(actor_target) & count_item >0){
            single_dataframe <-data.frame( 
                neighbor = actor_target
            )
                edge = rbind(edge, single_dataframe)
            }
    }
    edge = unique(edge)
    edge

}

get_mcd_cidc <- function (cid_value) {

    edge <- data.frame (
        actor_source=character(),
        actor_target=character(),
        stringsAsFactors=FALSE
    )
    actors <- subset(aucs_com, cid == cid_value)
    actors_col <- unique(actors["actor"])
    
    for(i in 1:nrow(actors_col)){
        actor_value = as.character(actors_col[i,] ) 

        targets = subset(aucs_df, from_actor == actor_value | to_actor == actor_value)
        
        targets = unique(targets)

        target_list = get_target_by_cid_value(cid_value,actor_value,targets)
        
        for(j in 1:nrow(target_list)){

            target_value = as.character(target_list[j,][[1]])
            single_dataframe <-data.frame( 
                    actor_source = actor_value,
                    actor_target = target_value
                )
                edge = rbind(edge, single_dataframe)
        }

        }
        

    for(a in 1:nrow(edge)){
        row_edge = edge[a,]
        source = as.character(row_edge["actor_source"][[1]])
        target = as.character(row_edge["actor_target"][[1]])
        edge$actor_source[edge$actor_source == target & edge$actor_target == source] <- NA
        edge$actor_target[ is.na(edge$actor_source) & edge$actor_target == source] <- NA
    }
    edge<-edge[!(is.na(edge$actor_source) & is.na(edge$actor_target)),]
    df_k = nrow(unique(actors["layer"]))

    edge_count = nrow(unique(edge))
    actors_count_cidi = nrow(unique(actors["actor"]))
    max_edge = ncol(combn(actors_count_cidi,2))

    mcd_cidi = edge_count / (df_k * max_edge)

    mcd_cidi
}

get_triade_cidc <- function (cid_value) {

    triade <- data.frame (
        source =character(),
        neighbor1=character(),
        neighbor2=character(),
        stringsAsFactors=FALSE
    )

    triade_cid = 0
    actors <- subset(aucs_com, cid == cid_value)
    actors_col <- unique(actors["actor"])

    for(i in 1:nrow(actors_col)){
        actor_value = as.character(actors_col[i,] ) 

        targets = subset(aucs_df, from_actor == actor_value | to_actor == actor_value)
        
        targets = unique(targets)

        target_list = get_target_by_cid_value(cid_value,actor_value,targets)
        
        for(j in 1:nrow(target_list)){

            source = as.character(target_list[j,][[1]])

            for(k in 1:nrow(target_list)){
                target = as.character(target_list[k,][[1]])
                count = 0
                
                if(!is.na(target != source))
                if(target != source){
                   
                   count = nrow(
                        subset(
                            aucs_df, 
                            to_actor==source & from_actor == target |
                            to_actor == target & from_actor == source
                        )
                    )
                }
                if(count > 0){
                        single_dataframe <- data.frame (
                            source = actor_value,
                            neighbor1=source,
                            neighbor2=target
                        )
                    triade = rbind(triade,single_dataframe)
                }
            }
            # print(paste("triade avec le noeud ",source," vaut : ",triade_cid))

        }
    }

    triade = unique(triade)
    for(a in 1:nrow(triade)){
        row_triade = triade[a,]
        source = as.character(row_triade["source"][[1]])
        neighbor1 = as.character(row_triade["neighbor1"][[1]])
        neighbor2 = as.character(row_triade["neighbor2"][[1]])

        triade$source[
            triade$source == source & triade$neighbor1 == neighbor2 & triade$neighbor2 == neighbor1 |
            triade$source == neighbor1 & triade$neighbor1 == source & triade$neighbor2 == neighbor2 |
            triade$source == neighbor1 & triade$neighbor1 == neighbor2 & triade$neighbor2 == source |
            triade$source == neighbor2 & triade$neighbor1 == source & triade$neighbor2 == neighbor1 |
            triade$source == neighbor2 & triade$neighbor1 == neighbor1 & triade$neighbor2 == source 

        ]<- NA
    }

    triade<-triade[!(is.na(triade$source)),]
    # triade = unique(subset(triade,!is.na(triade$source)))
    # print(triade)
    triade_cid = nrow(triade)

}

get_rmc_cidc <- function(cid_value){
    rmc_cidi = 0
    com_cidi  <- subset(aucs_com, cid == cid_value)
    layers_cidi <- com_cidi["layer"]
    layers_distint_cidi = unique(layers_cidi)
    ndim = nrow(layers_distint_cidi)
    moyenne = nrow(layers_cidi)/ndim
    nc = nrow(unique(com_cidi["actor"]))

    actor_dk = NA
    for(i in 1:ndim){
        occurence_value = as.character(layers_distint_cidi[i,][[1]])
        dimension_count = count_occurence_in_column(occurence_value,layers_cidi)
        # print(paste("Ocurrence value = ",occurence_value,"dimension_count = ",dimension_count," moyenne = ",moyenne))
        if(moyenne<=dimension_count){
            actor_dk_row <- subset(com_cidi,layer== occurence_value)
            actor_dk <- rbind(na.omit(actor_dk),actor_dk_row)
        }

    }

    node_dk_count = nrow(unique(actor_dk["actor"]))
    rmc_cidi = node_dk_count/(ndim * nc)
    rmc_cidi
}