source('aucs_helper.R')

# Construction du dataframe complet
dataframe_intregrated = generate_dataframe_aucs_integration()
# write.csv(dataframe_intregrated,"dataframe_aucs_integration.csv", row.names = FALSE)

# Détermination du parametre D
D <- unique(aucs_com["layer"])
print(paste("D = ",nrow(D)))

# Détermation de P
P <- dataframe_intregrated[c("actor_source","actor_target")]
print(paste("P = ",nrow(P)))

# Détermination de Pc
Pc <- get_dataframe_Pc(P)
print(paste ("Pc = " ,nrow(unique(Pc)))) 

# Détermination de P2
P2 <- subset(dataframe_intregrated, layer_count_for_target >=2)
print(paste("P2 = ",nrow(P2)))

# Détermination de P2c
P2c <- get_dataframe_Pc(P2)
print(paste ("P2c = " ,nrow(unique(P2c)))) 


