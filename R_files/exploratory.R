source("R_files/merge_data.R")

library(dplyr)
library(corrplot)
library(ggplot2)

df <- PLAYER_league_non_goal_salary

#Edit in a new column that takes first two characters from position


# Alex --------------------------------------------------------------------


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





nat <- df %>%
    group_by(Nation) %>%
    summarise(across(vec_of_attr,~mean(.x,na.rm=T)))


l <- df %>%
    group_by(League) %>%
    summarise(across(vec_of_attr,~mean(.x,na.rm=T)))





nonRFL <- filter(df,League != "RFL")
RFL <- filter(df,League == "RFL")

###### SAVING INTO MODEL.R
# save(RFL,nonRFL,file = "data/model.RData")



# Aidan Code --------------------------------------------------------------
#Separate players by position
pos_coef_list <- list()
for (i in seq_along(pos)) {
    pos_df <- df %>%
        filter(Pos_new == pos[3])%>%
        filter(League != 'RFL')
    
    
    mod <- glm(Annualized_Salary ~ .-Player-Nation-Squad-League-Year, data = pos_df[,-71],
               family = )
    s <- summary(mod)
    
    coeff_table <- s$coefficients
    colnames(coeff_table)[4] <- "p_value"
    coeff_table[coeff_table[,"p_value"]<0.05,]

    
}

mod <- glm(Annualized_Salary ~ .-Player-Nation-Pos_new, data = df)

s <- summary(mod)

coeff_table <- s$coefficients
colnames(coeff_table)[4] <- "p_value"
coeff_table[coeff_table[,"p_value"]<0.05,] 

## Correlation heatmap
# cormat <- cor(temp_df, method = "pearson")
# remove <- c('Total_Att','')

colnames(df)
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

cormat <- cor(temp_df[c(names(temp_df)[1:25],"Annualized_Salary")], method = "pearson")
corrplot(cormat, method = "number")
cormat <- cor(temp_df[c(names(temp_df)[26:51],"Annualized_Salary")], method = "pearson")
corrplot(cormat, method = "number")
cormat <- cor(temp_df[48:67], method = "pearson")
corrplot(cormat, method = "number")

cor_df <- df[,keep_player]
# save(cor_df, file = "data/cor_df.RData")
cormat <- cor(cor_df, method = "pearson")
corrplot(cormat, method = "number")


pca <- prcomp(temp_df,scale = T)
sd <- pca$sdev/sum(pca$sdev)
autoplot(pca,loadings = T)

#Goalkeeper corrmat
temp_df <- PLAYER_league_goal_salary%>% select(-c("Player","Nation","League","Squad","Pos_new",
                                                  "Year","Country"))

cormat <- cor(temp_df, method = "pearson")
corrplot(cormat, method = "number")

gk_df <- PLAYER_league_goal_salary[,c(keep_goal,"Player","Nation","League","Squad","Country")]
# save(gk_df, file = "data/gk_df.RData")
##Distribution of salary (including RFL)
ggplot(df)+
    geom_histogram(aes(x = Annualized_Salary, y = ..density..), color = "black", fill="#33AFFF")+
    labs(x = "Annualised Salary", y = "Density", title = "Distribution of Annualised Salary")+
    theme_bw() +
    theme(axis.text=element_text(size=9.5), axis.title=element_text(size=13, face = "bold"), plot.title = element_text(size=16, face = "bold"), plot.subtitle=element_text(size=13))


# save(df,file = "data/model2.RData")

#Check expected goals per position
for (level in pos) {
    test <- nonRFL[,c(keep_player,'Pos_new')] %>% filter(Pos_new == level)
    print(level)
    print(mean(test$Expected_xG))
}

#Box plot showing distribution of Expected_xG
boxplot(`Standard_Sh/90` ~ Pos_new, data = df)


#Make Player_tourn_non_goal same columns as cor_df
player_tourn_df <- PLAYER_tourn_non_goal[,keep_player[!keep_player == 'Annualized_Salary']]
# save(player_tourn_df, file = "data/player_tourn_df.RData")


#Make Player_tourn_goal same columns as goal_df
gk_tourn_df <- PLAYER_tourn_goal[keep_goal[!keep_goal == 'Annualized_Salary']]
# save(gk_tourn_df, file = "data/gk_tourn_df.RData")


# Metric exploration ------------------------------------------------------
path <- 'Graphs'
width <- 600
height <- 500
#Box plot on xG xA
player.traits <- c('Expected_xG','xA','Tackles_Tkl','Int','Standard_Sh/90')

for (trait in player.traits) {
    
    temp.df <- df%>% filter(Pos_new != 'GK')
    
    png(filename = paste0(path,'/',trait,'boxplot.png'), width = width, height = height)
    boxplot(eval(parse(text = trait)) ~ Pos_new, data = temp.df, main = paste0(trait,' Box plot'),
            ylab = trait)
    dev.off()
    
}

boxplot(`Standard_Sh/90` ~ Pos_new, data = temp.df)
