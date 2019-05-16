#!/usr/bin/Rscript

# Chagement du jeu de données
aucs_com = read.csv("aucs_com_gLouv.csv", header = TRUE)
aucs_df = read.csv("aucs_df.csv", header = TRUE)

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