source("R_files/merge_data.R")
vec_of_attr <- c("Total_Cmp%", 
                 "Short_Cmp%", 
                 "Medium_Cmp%", 
                 "Long_Cmp%",
                 "Performance_PK",
                 "Performance_PKatt",
                 "Tackles_TklW",
                 "Pressures_%",
                 "Clr",
                 "Blocks_Blocks",
                 "Blocks_Sh",
                 "Blocks_ShSv",
                 "Blocks_Pass",
                 "Gls",
                 "Standard_SoT%",
                 "Standard_G/Sh")

pos <- c("DF","MF","FW")


cor_tourn_merge <- PLAYER_tourn_non_goal
colnames(cor_tourn_merge) <- gsub(" ","_", colnames(cor_tourn_merge))

## Took average of all 90s columns
cor_tourn_merge$`90s_avg` <- rowMeans(cor_tourn_merge %>% select("90s","90s.x","90s.y")) * 90
cor_tourn_merge$`90s_avg` <- ifelse(cor_tourn_merge$`90s_avg` < 0.1,0,cor_tourn_merge$`90s_avg`)
cor_tourn_merge <- filter(cor_tourn_merge,`90s_avg` > 0)
cor_tourn_merge[vec_of_attr] <- cor_tourn_merge[vec_of_attr]/cor_tourn_merge$`90s_avg`

# Replaced all negative values with 0
for (c in colnames(cor_tourn_merge))
    cor_tourn_merge[[c]] <- replace(cor_tourn_merge[[c]],which(cor_tourn_merge[[c]] <0) ,0) 


# Replace all NA with 0
for (c in colnames(cor_tourn_merge))
    cor_tourn_merge[[c]] <- replace(cor_tourn_merge[[c]],which(is.na(cor_tourn_merge[[c]])) ,0) 


# table(subset(cor_tourn_merge,is.na(`Standard_G/Sh`))$Pos)


# for (i in colnames(cor_tourn_merge)) {
#     for (j in colnames(cor_tourn_merge)) {
#         if (i == j) {
#             next
#         } else {
#             if (all(!is.na(cor_tourn_merge[[i]])) & all(!is.na(cor_tourn_merge[[j]])) & sum(cor_tourn_merge[[i]] == cor_tourn_merge[[j]])/nrow(cor_tourn_merge) > 0.9) {
#                 print(paste(i,j))
#             }
#         }
#     }
# }

cor_tourn_merge<- cor_tourn_merge %>% select(-c("90s","90s.x","90s.y"))

cor_tourn_merge$Annualized_Salary <- 0
cor_tourn_merge$Squad <- NA

colnames(cor_tourn_merge)
keep <- c('Age','Tackles_Tkl','Vs_Dribbles_Att','Pressures_%','Blocks_Sh',
          'Int','Clr','Total_Cmp%','xA','Standard_SoT/90',
          'Standard_Sh/90','Standard_G/SoT','Standard_Dist','Standard_FK',
          'Performance_PK','Expected_xG','Annualized_Salary','90s_avg',
          "Player", "Nation", "Pos_new", "League", "Squad")

cor_tourn_merge <- cor_tourn_merge[,keep]


#### Remove Data except the following #####
# rm(list= ls()[! (ls() %in% c('cor_tourn_merge'))])
# save(cor_tourn_merge,file = "data/tourn_merge.RData")


