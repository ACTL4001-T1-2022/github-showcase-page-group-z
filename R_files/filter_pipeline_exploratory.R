source("R_files/merge_data.R")
library(dplyr)
library(corrplot)
library(ggplot2)

df <- PLAYER_league_non_goal_salary

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

nonRFL <- filter(df,League != "RFL")
RFL <- filter(df,League == "RFL")

keep_player <- c('Age','Tackles_Tkl','Vs_Dribbles_Att','Pressures_%','Blocks_Sh',
                 'Int','Clr','Total_Cmp%','xA','Standard_SoT/90',
                 'Standard_Sh/90','Standard_G/SoT','Standard_Dist','Standard_FK',
                 'Performance_PK','Expected_xG','Annualized_Salary','90s_avg')

keep_goal <- c("Player","Nation","Pos_new","League",'Age','Playing_Time_90s','Performance_GA90','Performance_Saves',
               'Performance_Save%','W','D','L','Performance_CS','Performance_CS%','Penalty_Kicks_PKA',
               'Penalty_Kicks_PKsv','Penalty_Kicks_PKm','Penalty_Kicks_Save%',
               'Annualized_Salary')


#Player cormat
temp_df <- df%>% select(-c("Player","Nation","Pos_new","League","Squad"))
cor_df <- df[,c('Age','Tackles_Tkl','Vs_Dribbles_Att','Pressures_%','Blocks_Sh','Int','Clr','Total_Cmp%','xA','Standard_SoT/90','Standard_Sh/90','Standard_G/SoT','Standard_Dist','Standard_FK','Performance_PK','Expected_xG','Annualized_Salary','90s_avg')]
stopifnot(dim(cor_df) == c(5500,18))

gk_df <- PLAYER_league_goal_salary[,c('Age','Playing_Time_90s','Performance_GA90','Performance_Saves','Performance_Save%','W','D','L','Performance_CS','Performance_CS%','Penalty_Kicks_PKA','Penalty_Kicks_PKsv','Penalty_Kicks_PKm','Penalty_Kicks_Save%','Annualized_Salary','Player','Nation','League','Squad','Country')]
stopifnot(dim(gk_df) == c(413,20))

# player_tourn_df <- PLAYER_tourn_non_goal[,keep_player[!keep_player == 'Annualized_Salary']]
gk_tourn_df <- PLAYER_tourn_goal[,c('Player','Nation','Pos_new','League','Age','Playing_Time_90s','Performance_GA90','Performance_Saves','Performance_Save%','W','D','L','Performance_CS','Performance_CS%','Penalty_Kicks_PKA','Penalty_Kicks_PKsv','Penalty_Kicks_PKm','Penalty_Kicks_Save%')]
stopifnot(dim(gk_tourn_df)== c(129,18))



rm(list= ls()[! (ls() %in% c('df',
                             'nonRFL',
                             'RFL',
                             'cor_df',
                             'gk_df',
                             'gk_tourn_df',
                             'cor_tourn_merge'))])
