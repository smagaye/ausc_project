source('aucs_helper.R')

# Construction du dataframe complet
dataframe_intregrated = generate_dataframe_aucs_integration()
#write.csv(dataframe_intregrated,"dataframe_aucs_integration.csv", row.names = FALSE)

# Nombre de communautés
cids <- unique(aucs_com["cid"])

# Détermination du parametre D
D <- unique(aucs_com["layer"])
D_count = nrow(D)
print(paste("D = ",D_count))

# Détermation de P
P <- dataframe_intregrated[c("actor_source","actor_target")]
print(paste("P = ",nrow(P)))

# Détermination de Pc
Pc <- get_dataframe_Pc(P)
print(paste ("Pc = " ,nrow(unique(Pc)))) 

# Détermination de P2
P2 <- subset(dataframe_intregrated, layer_count_for_target >=1)
# write.csv(dataframe_intregrated,"dataframe_aucs_integration.csv", row.names = FALSE)

print(paste("P2 = ",nrow(P2)))


# #---------------------------------------------------------------#
# #           Calcul de rho pour la communauté 0                  #
# #---------------------------------------------------------------#
# # Détermination de Pc (avec c = 0)
# Pc0 <- get_dataframe_Pi(0,P)
# Pc0_count =nrow(unique(Pc0))
# print(paste ("Pc0 = " ,Pc0_count)) 

# # Détermination de P0c (avec c = 0)
# P2c0 <- get_dataframe_Pi(1,P2)
# P2c0_count = nrow(unique(P2c0))
# print(paste ("P2c0 = " ,P2c0_count)) 

# # Détermination de rhoc0
# rhoc0 = P2c0_count / (D_count * Pc0_count)
# print(paste ("rhoc0 = " ,rhoc0))


# #---------------------------------------------------------------#
# #           Calcul de rho pour la communauté 1                  #
# #---------------------------------------------------------------#

# # Détermination de Pc (avec c = 1)
# Pc1 <- get_dataframe_Pi(1,P)
# Pc1_count =nrow(unique(Pc1))
# print(paste ("Pc1 = " ,Pc1_count)) 

# # Détermination de P2c (avec c = 1)
# P2c1 <- get_dataframe_Pi(1,P2)
# P2c1_count = nrow(unique(P2c1))
# print(paste ("P2c1 = " ,P2c1_count)) 

# # Détermination de rhoc1
# rhoc1 = P2c1_count / (D_count * Pc1_count)
# print(paste ("rhoc1 = " ,rhoc1))

# #---------------------------------------------------------------#
# #           Calcul de rho pour la communauté 2                  #
# #---------------------------------------------------------------#

# # Détermination de Pc (avec c = 2)
# Pc2 <- get_dataframe_Pi(2,P)
# Pc2_count =nrow(unique(Pc2))
# print(paste ("Pc2 = " ,Pc2_count)) 

# # Détermination de P2c (avec c = 2)
# P2c2 <- get_dataframe_Pi(2,P2)
# P2c2_count = nrow(unique(P2c2))
# print(paste ("P2c2 = " ,P2c2_count)) 

# # Détermination de rhoc2
# rhoc2 = P2c2_count / (D_count * Pc2_count)
# print(paste ("rhoc2 = " ,rhoc2))


# #---------------------------------------------------------------#
# #           Calcul de rho pour la communauté 3                  #
# #---------------------------------------------------------------#

# # Détermination de Pc (avec c = 3)
# Pc3 <- get_dataframe_Pi(3,P)
# Pc3_count =nrow(unique(Pc3))
# print(paste ("Pc3 = " ,Pc3_count)) 

# # Détermination de P2c (avec c = 1)
# P2c3 <- get_dataframe_Pi(3,P2)
# P2c3_count = nrow(unique(P2c3))
# print(paste ("P2c3 = " ,P2c3_count)) 

# # Détermination de rhoc3
# rhoc3 = P2c3_count / (D_count * Pc3_count)
# print(paste ("rhoc3 = " ,rhoc3))

# #---------------------------------------------------------------#
# #           Calcul de rho pour la communauté 4                  #
# #---------------------------------------------------------------#

# # Détermination de Pc (avec c = 4)
# Pc4 <- get_dataframe_Pi(4,P)
# Pc4_count =nrow(unique(Pc4))
# print(paste ("Pc4 = " ,Pc4_count)) 

# # Détermination de P2c (avec c = 4)
# P2c4 <- get_dataframe_Pi(4,P2)
# P2c4_count = nrow(unique(P2c4))
# print(paste ("P2c4 = " ,P2c4_count)) 

# # Détermination de rhoc4
# rhoc4 = P2c4_count / (D_count * Pc4_count)
# print(paste ("rhoc4 = " ,rhoc4))


# ---------------------------------------------------------------#
#           Calcul de la moyenne des rho                        #
# ---------------------------------------------------------------#
max_communaute = nrow(cids)
rho = 0
for (i in 1:max_communaute) {

    Pci <- get_dataframe_Pi(i-1,P)
    Pci_count =nrow(unique(Pci))

    # Détermination de P2c (avec c = i)
    P2ci <- get_dataframe_Pi(i-1,P2)
    P2ci_count <- nrow(unique(P2ci))

    # Détermination de rhoci
    rhoci <- P2ci_count / (D_count * Pci_count)
    print(paste ("rho ",i-1, " = " ,rhoci))

    rho <-  rho + (rhoci/max_communaute)
}
print(paste ("rho = " ,rho))


#---------------------------------------------------------------#
#           MCDense (Multi-community Density)                   #
#---------------------------------------------------------------#
mcd = 0
for(i in 1:nrow(cids)){
    mcd_cidi = get_mcd_cidc(i-1)
    mcd = mcd + (mcd_cidi / nrow(cids))
    print(paste ("mcd_cid",i-1," = " ,mcd_cidi))
}
print(paste ("mcd = " ,mcd))


#---------------------------------------------------------------#
#           Triade (Multi-community Density)                    #
#---------------------------------------------------------------#

triade = 0
for(i in 1:nrow(cids)){
    actors <- subset(aucs_com, cid == i-1)
    actors_col <- unique(actors["actor"])
    nc = nrow(actors_col)
    ndim =nrow(unique(actors["layer"]))
    triade_cidi = get_triade_cidc(i-1)

    denominateur = ndim * ncol(combn(nc,3))
    delta_c = triade_cidi /denominateur
    print(paste("Triade_cid ",i-1,"  = ",delta_c))
    triade = triade + (delta_c)/nrow(cids)
    
}

print(paste("Triade = ",triade))

#---------------------------------------------------------------#
#           RMC (Multi-community Density)                       #
#---------------------------------------------------------------#
rmc = 0
for(i in 1:nrow(cids)){
    rmc_cdi = get_rmc_cidc(i-1)
    rmc = rmc + rmc_cdi/nrow(cids)
    print(paste("rmc_cdi ",i-1,"  = ",rmc_cdi))

}

print(paste("RMC Dense = ",rmc))




