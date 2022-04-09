source("R_files/convert_excel_to_r.R")


##### Load Packages #####
library(dplyr)



##### Pre-cleaning Examination #####
dats <- c("PLAYER_league_def","PLAYER_league_pass","PLAYER_league_shoot")
vec = c("Player","Nation","Pos", "Squad","Age") # for some reason "Born" does not cover the whole set like "Age"
mat = matrix(0,
             nrow = length(vec),
             ncol = length(dats),
             dimnames = list(vec,dats))


# Count how many uniques in each combination of player profile
for (j in (1:length(dats))) {
    for (i in (1:length(vec))) {
        tmp <- vec[1:i]
        mat[i,j] = as.numeric(count(unique(select(eval(parse(text = dats[j])),tmp))))
    }
}
## Alex's Comment: It is possible they are all the same player combinations

# Checking if the sets are different - they are the exact same.
setdiff(unique(select(eval(parse(text= dats[1])),vec[1:5])),unique(select(eval(parse(text = dats[2])), vec[1:5])))
setdiff(unique(select(eval(parse(text= dats[1])),vec[1:5])),unique(select(eval(parse(text = dats[3])), vec[1:5])))

### Merging Data ###
overlapping_cols <- c("Player","Nation","Pos", "Squad","Age", "Year", "Born", "League")
PLAYER_league_non_goal <- merge(x = eval(parse(text = dats[1])),
                                y = eval(parse(text = dats[2])), 
                                by = overlapping_cols,
                                all = T)
PLAYER_league_non_goal <- merge(x = PLAYER_league_non_goal,
                                y = eval(parse(text = dats[3])),
                                by = overlapping_cols,
                                all = T)

# A check to see if the merged data rows are increased
stopifnot(count(PLAYER_league_non_goal) == count(PLAYER_league_def))

Year <- rep(2021,count(PLAYER_salary2021))
a <- cbind(Year,PLAYER_salary2021)
Year <- rep(2020,count(PLAYER_salary2020))
PLAYER_salary <- rbind(a,cbind(Year,PLAYER_salary2020))
names(PLAYER_salary)[names(PLAYER_salary) == "Player Name"] <- "Player"


# setdiff(unique(select(PLAYER_salary, c("Player Name","Year","League"))),
overlapping_cols2 <- c("Player","Year","Squad")
setdiff(unique(select(PLAYER_salary, overlapping_cols2)),
        unique(select(PLAYER_league_non_goal, overlapping_cols2)))

PLAYER_league_non_goal_salary <- left_join(
    x = PLAYER_league_non_goal,
    y = PLAYER_salary,
    by = c(overlapping_cols2,"League")
)


PLAYER_league_goal_salary <- left_join(
    x = PLAYER_league_goal,
    y = PLAYER_salary,
    by = overlapping_cols2
)

PLAYER_league_goal_salary$League.x <- NULL
colnames(PLAYER_league_goal_salary)[which(colnames(PLAYER_league_goal_salary)=="League.y")] <- "League"

# GET RID OF A ZERO SALARY DUDE (Y. Meyer) - ALEX 
PLAYER_league_non_goal_salary <- filter(PLAYER_league_non_goal_salary,!is.na(`Annualized Salary`))


PLAYER_league_non_goal_salary$Position <- NULL
PLAYER_league_non_goal_salary$Country <- NULL

PLAYER_league_goal_salary$Position <- NULL



dats2 <- c("PLAYER_tourn_def","PLAYER_tourn_pass","PLAYER_tourn_shoot_2021")
vec = c("Player","Year","League", "Age", "Born")#,"Nation","Pos") # for some reason "Born" does not cover the whole set like "Age"
mat = matrix(0,
             nrow = length(vec),
             ncol = length(dats2),
             dimnames = list(vec,dats2))


# Count how many uniques in each combination of player profile
for (j in (1:length(dats2))) {
    for (i in (1:length(vec))) {
        tmp <- vec[1:i]
        mat[i,j] = as.numeric(count(unique(select(eval(parse(text = dats2[j])),tmp))))
    }
}
## Alex's Comment: It is possible they are all the same player combinations

# Checking if the sets are different - they are the exact same.
setdiff(unique(select(eval(parse(text= dats2[1])),vec[1])),unique(select(eval(parse(text = dats2[2])), vec[1])))
setdiff(unique(select(eval(parse(text= dats2[1])),vec[1])),unique(select(eval(parse(text = dats2[3])), vec[1])))

##### Merging Data #####

overlapping_cols2 = c(vec)
PLAYER_tourn_non_goal <- merge(x = eval(parse(text = dats2[1])),
                                y = eval(parse(text = dats2[2])), 
                                by = overlapping_cols2,
                                all = T)
PLAYER_tourn_non_goal <- merge(x = PLAYER_tourn_non_goal,
                                y = eval(parse(text = dats2[3])),
                                by = overlapping_cols2,
                                all = T)

# A check to see if the merged data rows are increased
stopifnot(count(PLAYER_tourn_non_goal) == count(PLAYER_tourn_def))



PLAYER_tourn_res_2020["Year"] <- 2020
colnames(PLAYER_tourn_res_2020)[c(1,2)] <- c("Places","Nation")


PLAYER_tourn_res_2021["Year"] <- 2021
colnames(PLAYER_tourn_res_2021)[c(1,2)] <- c("Places","Nation")

PLAYER_tourn_res_all <- rbind(PLAYER_tourn_res_2020,PLAYER_tourn_res_2021)

PLAYER_tourn_non_goal <- left_join(x = PLAYER_tourn_non_goal,
                                    y = PLAYER_tourn_res_all,
                                    by = c("Year","Nation"))
PLAYER_tourn_goal <- left_join(x = PLAYER_tourn_goal,
                               y = PLAYER_tourn_res_all,
                               by = c("Year","Nation"))


#### Data Preprocessing ####


preprocessing <- function(df,ninetysec = F, position = F, knn = F) {
    colnames(df) <- gsub(" ","_", colnames(df))
    
    if (length(unique(df$Pos)) > 1) {
        df['Pos_new'] <- substr(df$Pos,1,2)
        if(position) {
            df<- df %>% select(-c("Pos","Position"))
        } else {
            df<- df %>% select(-c("Pos"))
        }
    }
    
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
    
    if (ninetysec) {
        df$`90s_avg` <- rowMeans(df %>% select("90s","90s.x","90s.y")) * 90
        df$`90s_avg` <- ifelse(df$`90s_avg` < 0.1,0,df$`90s_avg`)
        df <- filter(df,`90s_avg` > 0) # got rid of all negative 90s ppl
        df[vec_of_attr] <- df[vec_of_attr]/df$`90s_avg`
    }
    
    # KNN Imputation
    if (knn) {
        df <- impute.knn(as.matrix(df))
    }
    # Replaced all negative values with 0
    for (c in colnames(df))
        df[[c]] <- replace(df[[c]],which(df[[c]] <0) ,0) 
    
    
    # Replace all NA with 0
    for (c in colnames(df))
        df[[c]] <- replace(df[[c]],which(is.na(df[[c]])) ,0) 
    
    
    
    return(df)
    
}



PLAYER_tourn_goal <- preprocessing(PLAYER_tourn_goal)
PLAYER_tourn_non_goal <- preprocessing(PLAYER_tourn_non_goal,ninetysec=T)
PLAYER_league_non_goal_salary <- preprocessing(PLAYER_league_non_goal_salary,ninetysec=T,position = F)
PLAYER_league_goal_salary <- preprocessing(PLAYER_league_goal_salary,position = F)


PLAYER_league_non_goal_salary <- PLAYER_league_non_goal_salary %>% select(-c("90s","90s.x","90s.y"))

stopifnot(nrow(PLAYER_league_non_goal_salary) == 5500 && 
              ncol(PLAYER_league_non_goal_salary) == 72)




ori_col_PLNGS <- c('Player','Year','League','Squad','Nation','Age','Born','Tackles_Tkl','Tackles_TklW','Tackles_Def_3rd','Tackles_Mid_3rd','Tackles_Att_3rd','Vs_Dribbles_Tkl','Vs_Dribbles_Att','Vs_Dribbles_Tkl%','Vs_Dribbles_Past','Pressures_Press','Pressures_Succ','Pressures_%','Pressures_Def_3rd','Pressures_Mid_3rd','Pressures_Att_3rd','Blocks_Blocks','Blocks_Sh','Blocks_ShSv','Blocks_Pass','Int','Tkl+Int','Clr','Err','Total_Cmp','Total_Att','Total_Cmp%','Total_TotDist','Total_PrgDist','Short_Cmp','Short_Att','Short_Cmp%','Medium_Cmp','Medium_Att','Medium_Cmp%','Long_Cmp','Long_Att','Long_Cmp%','Ast','xA','A-xA','KP','1/3','PPA','CrsPA','Prog','Gls','Standard_Sh','Standard_SoT','Standard_SoT%','Standard_Sh/90','Standard_SoT/90','Standard_G/Sh','Standard_G/SoT','Standard_Dist','Standard_FK','Performance_PK','Performance_PKatt','Expected_xG','Expected_npxG','Expected_npxG/Sh','Expected_G-xG','Expected_np:G-xG','Annualized_Salary','Pos_new','90s_avg')
stopifnot(sort(colnames(PLAYER_league_non_goal_salary)) == sort(ori_col_PLNGS))

PLAYER_league_non_goal_salary <- PLAYER_league_non_goal_salary %>% select(all_of(ori_col_PLNGS))

ori_col_CTM <-c('Age','Tackles_Tkl','Vs_Dribbles_Att','Pressures_%','Blocks_Sh','Int','Clr','Total_Cmp%','xA','Standard_SoT/90','Standard_Sh/90','Standard_G/SoT','Standard_Dist','Standard_FK','Performance_PK','Expected_xG','Annualized_Salary','90s_avg','Player','Nation','Pos_new','League','Squad')




#### Remove Data except the following #####
rm(list= ls()[! (ls() %in% c('PLAYER_tourn_goal',
                             'PLAYER_tourn_non_goal',
                             'PLAYER_league_non_goal_salary',
                             'PLAYER_league_goal_salary',
                             'cor_tourn_merge'))])

# save(PLAYER_tourn_goal,PLAYER_tourn_non_goal, file = "data/merged_tourn.RData")
# save(PLAYER_league_non_goal_salary,PLAYER_league_goal_salary, file = "data/merged_league.RData")




