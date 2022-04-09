source("R_files/merge_data.R")

library(ggplot2)
library(dplyr)
library(qcc)

#Analysing other team compositions
df <- PLAYER_tourn_non_goal
#df_2020 <- df %>% filter(Year == 2020)
#df_2021 <- df %>% filter(Year == 2021)

pos <- c("DF","MF","FW",'GK')
winning_teams <- c('Sobianitedrucy',
"People's Land of Maneau",
                  'Nganion',
                  'Mico',
                  'Quewenia',
                  'Southern Ristan',
                  'Galamily',
                  'Bernepamar',
                  'Dosqaly',
                  'Giumle Lizeibon',
                  'Greri Landmoslands',
                  'Xikong',
                  'Manlisgamncent',
                  'Esia',
                  'Byasier Pujan',
                  'Djipines',
                  'Leoneku Guidisia',
                  'Ledian',
                  'Eastern Sleboube',
                  'New Uwi',
                  'Ngoque Blicri',
                  'Nkasland Cronestan',
                  'Eastern Niasland',
                  'Varijitri Isles')


df %>%
    filter(League == "RFL")

attach(df)    
#Linear regression analaysis
pos_df <- df %>%
    filter(Pos_new == "DF")

pos_df <- pos_df%>% select(-c("Player","Nation","Pos_new","League","Squad"))
        
lm <-lm(Annualized_Salary ~ ., data = pos_df)
c <- summary(lm)
coeff_table <- c$coefficients
colnames(coeff_table)[4] <- "p_value"
coeff_table[coeff_table[,"p_value"]<0.05,]

pos_df <- df %>%
    filter(Pos_new == "MF")

pos_df <- pos_df%>% select(-c("Player","Nation","Pos_new","League","Squad"))
        
lm <-lm(Annualized_Salary ~ ., data = pos_df)
c <- summary(lm)
coeff_table <- c$coefficients
colnames(coeff_table)[4] <- "p_value"
coeff_table[coeff_table[,"p_value"]<0.05,]

pos_df <- df %>%
    filter(Pos_new == "FW")

pos_df <- pos_df%>% select(-c("Player","Nation","Pos_new","League","Squad"))
        
lm <-lm(Annualized_Salary ~ ., data = pos_df)
c <- summary(lm)
coeff_table <- c$coefficients
colnames(coeff_table)[4] <- "p_value"
coeff_table[coeff_table[,"p_value"]<0.05,]

temp <- df %>% filter(Nation == 'Rarita') %>% summarise(average = median(Annualized_Salary))

#KPI analysis
fw.kpi <- c('Standard_Sh','Standard_SoT','Standard_G/Sh','Expected_xG')
mf.kpi <- c('CrsPA','KP','Total_Cmp','PPA','1/3')
df.kpi <- c('Int','Clr','Tackles_TklW')
gk.kpi <- c('Performance_GA90')

#Histogram plots

make.histogram <- function(kpis, position, data, path) {
    
    for (kpi in kpis) {
        position.df <- PLAYER_league_non_goal_salary %>%
            filter(Pos_new == position)%>%
            filter(!(abs(!!as.name(kpi) - median(!!as.name(kpi))) > 2*sd(!!as.name(kpi))))
        png(filename = paste(path,kpi,position,"histogram.png"))
        p<- ggplot(position.df, aes(x = !!as.name(kpi))) +
            geom_histogram(stat = "count")+
            labs(title = paste(position, "histogram"))
        print(p)
        dev.off()
    }
}

get.cutoff <- function(kpis, position, data, percentile) {
    for (kpi in kpis) {
        position.df <- PLAYER_league_non_goal_salary %>%
            filter(Pos_new == position)%>%
            filter(!(abs(!!as.name(kpi) - median(!!as.name(kpi))) > 2*sd(!!as.name(kpi))))%>%
            filter(quantile(!!as.name(kpi), percentile) < !!as.name(kpi))
        print(kpi)
        print(min(position.df[kpi]))
    }
    
}

ggplot(PLAYER_league_non_goal_salary, aes(x = PLAYER_league_non_goal_salary$League, 
                                          y = PLAYER_league_non_goal_salary$Annualized_Salary))+
    geom_boxplot()+
    theme_bw()+
    labs(x = "League", y = "Annualised Salary (∂)", title = "Box Plot of Annualized Salary per League")+
    theme(axis.text=element_text(size=9.5), axis.title=element_text(size=13, face = "bold"), plot.title = element_text(size=14, face = "bold"))+
    scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))

make.histogram(fw.kpi, 'FW',PLAYER_league_non_goal_salary, graph.path)
make.histogram(df.kpi, 'DF',PLAYER_league_non_goal_salary, graph.path)
make.histogram(mf.kpi, 'MF',PLAYER_league_non_goal_salary, graph.path)
make.histogram(gk.kpi, 'GK',PLAYER_league_goal_salary, graph.path)

percentile <- 0.9

get.cutoff(fw.kpi, 'FW',PLAYER_league_non_goal_salary, percentile)
get.cutoff(fw.kpi, 'FW',PLAYER_league_non_goal_salary, percentile)


# Archived ----------------------------------------------------------------


position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'FW')
qplot(position.df$Standard_Sh, geom = "histogram")
qplot(position.df$Standard_SoT, geom = "histogram")
qplot(position.df$`Standard_G/Sh`, geom = "histogram", bins = "")
qplot(position.df$Expected_xG, geom = "histogram")

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'MF')
qplot(position.df$CrsPA, geom = "histogram")
qplot(position.df$KP, geom = "histogram")
qplot(position.df$`Total_Cmp%`, geom = "histogram", bins = "200")
qplot(position.df$PPA, geom = "histogram")
qplot(position.df$`1/3`, geom = "histogram")

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'DF')
qplot(position.df$Int, geom = "histogram")
qplot(position.df$Clr, geom = "histogram", bins = "200")
qplot(position.df$Tackles_TklW, geom = "histogram", bins = "200")

position.df <- PLAYER_league_goal_salary %>%
    filter(Pos_new == 'GK')
qplot(position.df$`Performance_Save%`, geom = "histogram")
qplot(position.df$Performance_GA90, geom = "histogram")

#filter(!(abs(value - median(value)) > 2*sd(value)))

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'FW')%>%
    filter(quantile(Standard_Sh, percentile) < Standard_Sh)%>%
    filter(!(abs(Standard_Sh - median(Standard_Sh)) > 2*sd(Standard_Sh)))
min(position.df$Standard_Sh)

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'FW')%>%
    filter(quantile(Standard_SoT, percentile) < Standard_SoT)
min(position.df$Standard_SoT)

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'FW')%>%
    filter(quantile(`Standard_G/Sh`, percentile) < `Standard_G/Sh`)
min(position.df$`Standard_G/Sh`)

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'FW')%>%
    filter(quantile(Expected_xG, percentile) < Expected_xG)
min(position.df$Expected_xG)


position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'MF')%>%
    filter(quantile(CrsPA, percentile) < CrsPA)
min(position.df$CrsPA)

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'MF')%>%
    filter(quantile(KP, percentile) < KP)
min(position.df$KP)

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'MF')%>%
    filter(quantile(`Total_Cmp%`, percentile) < `Total_Cmp%`)
min(position.df$`Total_Cmp%`)

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'MF')%>%
    filter(quantile(PPA, percentile) < PPA)
min(position.df$PPA)

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'MF')%>%
    filter(quantile(`1/3`, percentile) < `1/3`)
min(position.df$`1/3`)


position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'DF')%>%
    filter(quantile(Int, percentile) < Int)
min(position.df$Int)

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'DF')%>%
    filter(quantile(Clr, percentile) < Clr)
min(position.df$Clr)

position.df <- PLAYER_league_non_goal_salary %>%
    filter(Pos_new == 'DF')%>%
    filter(quantile(Tackles_TklW, percentile) < Tackles_TklW)
min(position.df$Tackles_TklW)

position.df <- PLAYER_league_goal_salary %>%
    filter(Pos_new == 'GK')%>%
    filter(quantile(`Performance_Save%`, percentile) < `Performance_Save%`)
min(position.df$`Performance_Save%`)

position.df <- PLAYER_league_goal_salary %>%
    filter(Pos_new == 'GK')%>%
    filter(quantile(Performance_GA90, percentile) < Performance_GA90)
min(position.df$Performance_GA90)