source("R_files/merge_data.R")


library(dplyr)
library(corrplot)
df <- PLAYER_league_non_goal_salary

#Edit in a new column that takes first two characters from position
df['Pos_new'] <- substr(df$Pos,1,2)


colnames(df) <- gsub(" ","_", colnames(df))

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

## Took average of all 90s columns
df$`90s_avg` <- rowMeans(df %>% select("90s","90s.x","90s.y")) * 90
df$`90s_avg` <- ifelse(df$`90s_avg` < 0.1,0,df$`90s_avg`)
df <- filter(df,`90s_avg` > 0)
df[vec_of_attr] <- df[vec_of_attr]/df$`90s_avg`



nat <- df %>%
    group_by(Nation) %>%
    summarise(across(vec_of_attr,~mean(.x,na.rm=T)))


l <- df %>%
    group_by(League) %>%
    summarise(across(vec_of_attr,~mean(.x,na.rm=T)))


## Clean Missing Data from each column

summary(df)

# Replaced all negative values with 0
for (c in colnames(df))
    df[[c]] <- replace(df[[c]],which(df[[c]] <0) ,0) 


# Replace all NA with 0
for (c in colnames(df))
    df[[c]] <- replace(df[[c]],which(is.na(df[[c]])) ,0) 


summary(df)



table(subset(df,is.na(`Standard_G/Sh`))$Pos)


for (i in colnames(df)) {
    for (j in colnames(df)) {
        if (i == j) {
            next
        } else {
            if (all(!is.na(df[[i]])) & all(!is.na(df[[j]])) & sum(df[[i]] == df[[j]])/nrow(df) > 0.9) {
                print(paste(i,j))
            }
        }
    }
}

df<- df %>% select(-c("90s","90s.x","90s.y"))

df<- df %>% select(-c("Position", "Country","Pos"))

nonRFL <- filter(df,League != "RFL")

mod <- glm(Annualized_Salary ~ .-Player-Nation, data = nonRFL)



#Separate players by position
pos_coef_list <- list()
for (i in seq_along(pos)) {
    pos_df <- df %>%
        filter(Pos_new == pos[1])%>%
        filter(League != 'RFL')
    
    
    mod <- glm(Annualized_Salary ~ .-Player-Nation, data = pos_df[,-71])
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
temp_df <- df%>% select(-c("Player","Nation","Pos_new","League","Squad"))
cormat <- cor(temp_df[1:20], method = "pearson")
cormat <- corrplot(cormat, method = "number")
cormat <- cor(temp_df[21:40], method = "pearson")
cormat <- corrplot(cormat, method = "number")
cormat <- cor(temp_df[41:60], method = "pearson")
cormat <- corrplot(cormat, method = "number")
cormat <- cor(temp_df[61:67], method = "pearson")
cormat <- corrplot(cormat, method = "number")

min(df$Annualized_Salary)
max(df$Annualized_Salary)

ggplot(data = df, mapping = aes(x = League, y = Annualized_Salary)) +
    geom_point() +
    ylim(100, 10000000)

ggplot(data = df, mapping = aes(x = Age, y = Annualized_Salary)) +
    geom_point(mapping = aes(color = League)) +
    ylim(100, 10000000)

ggplot(data = df, mapping = aes(x = Age, y = Annualized_Salary)) +
    geom_point(mapping = aes(color = Pos)) +
    ylim(100, 10000000)

ggplot(data = df, mapping = aes(x = Pos, y = Annualized_Salary)) +
    geom_point() +
    ylim(100, 30000000)


