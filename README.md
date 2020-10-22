# Analysis-of-English-Premier-League-2019
Analysis of English Premier League 2019 Using R Programming. 

---
title: "Project 4"
output:
  pdf_document: default
  html_document: default
  word_document: default
theme: cerulean
---


### The data used for this project concerns the 2019-2020 football season of the English Premier League. This contains six datasets which include goalkeepers, players, scorefixture, standing, teamgoalkeeping, and teamstats.  

### Importing all the six datasets using read_csv().
```{r echo=FALSE}
library(tidyverse)
library(dm)
standings<- read_csv("standings-1920.csv")
teamstats<- read_csv("teamstats-1920.csv")
teamgoalkeeping<- read_csv("teamgoalkeeping-1920.csv")
players<- read_csv("players-1920.csv")
goalkeepers<- read_csv("goalkeepers-1920.csv")
scoresfixtures<- read_csv("scoresfixtures-1920.csv")
```

```{r}
## 1 
### a. How many teams are there? 
teamstats2<- as_tibble(teamstats)
total_teams<- length(teamstats2[[1]])
print(paste("The total number of teams there are", total_teams), quote = FALSE)

### b. How many players are there?
players2<- as_tibble(players)
total_players<- length(players2$Player)
print(paste("The total number of players are", total_players), quote = FALSE)

### c. Total number of goals scored from teamstats and players dataset.
### From players dataset
players3<- as_tibble(players)
total_goals<- sum(players3$Gls)
### From teamstats
total.goals<- sum(teamstats$Gls)
print(paste("The total number of goals scored from teamstats dataset are", total.goals), quote = FALSE)
print(paste("The total number of goals scored from players dataset are", total_goals), quote = FALSE)

### d. Average attendance (spectators) at the games. 
total.attendance<-mean(scoresfixtures$Attendance, na.rm = TRUE)
print(paste(" The average attendance at the games are", total.attendance), quote = FALSE)
```


```{r}
## 2.
### Identifying the primary keys in each data set and keeping those value other than 1. 

### For goalkeepers

goalkeepers %>% count(Player) %>% filter(n>1)
#### The are no instances where there are two or more instances of a player hence Player is a primary key for goalkeepers. 

### For scoresfixtures.
scoresfixtures %>% count(Day, Date, Time, Home) %>% filter(n>1)
### The combination of day, date, time, and home turns to be a primary key for the scorefixtures data. 

### For Standings
standings %>% count(Squad) %>% filter(n>1)
### Squad (Teams) is a primary key for the standing data set as there are no instances where the each team  appeared twice.

### For teamgoalkeeping.
teamgoalkeeping %>% count(Squad) %>% filter(n>1)
### Squad (Teams) is a primary key for the goalkeeping data set as there are no instances where the each team(squad)  appeared twice.

### For teamstats.
teamstats %>% count(Squad) %>% filter(n>1)
###Squad (Teams) is a primary key for the teamstats data set as there are no instances where the each team(squad)  appeared twice.

### For players. 
players %>% count(Player) %>% filter(n>1)
### Player is not a primary key for the players data set as there are 7 intances where each player names appeared twice. 
```


```{r}
## 3. 
### Identifying the table with foreign keys. 
###   Table          Foreign Key         Associated table
###   Gaolkeepers     Player             Players
###                   Squad              Players

###   Scorefixtures    Attendance        Standing. 
```


```{r}
### 4
### Separating the players names  in the "Player" variable into two and keeping ONLY the first version of the name.
players %>% separate(Player, into = c("Full_Player_Name", "Full_player_Name2"), sep = "\\\\") %>% select(-Full_player_Name2)

### Separating the nationalities of the players in the "Nation" variable into two ( and keeping both of them).
players %>% separate(Nation, into = c("Nationality_Small_Caps", "Nationality_Big_Caps"), sep = " ")

### Assigning the new data frame to a new names different from the original ones. 
Player_Nationality <- players %>% separate(Nation, into = c("Nationality_Small_Caps", "Nationality_Big_Caps"), sep = " ", convert = TRUE)
Player_Nationality
```


```{r}
### 5a. 
###  Finding the mean player age, the age of the youngest player and the age of the oldest player. 
player1 <- players %>% separate(Player, into = c("Full_Player_Name", "Full_player_Name2"), sep = "\\\\") %>% select(-Full_player_Name2)
player1 %>% group_by(Squad) %>% summarise(maximum.age=max(Age, na.rm=TRUE), minimum.age=min(Age, na.rm=TRUE), mean.age=mean(Age, na.rm=TRUE))

### Finding the name of the player, team name and minimum age,  
player1 %>% group_by(Squad) %>% summarise(minimum_age=min(Age, na.rm=TRUE), Player_Name = Full_Player_Name[which(Age == min(Age, na.rm=TRUE))])
```



```{r}
### Add the team performance numbers (specifically, Possession, Assists, Penalty Kicks scored, Save percentage, and Clean sheet percentage) to the standings table. Add also the age statistics found in Q5 above to the standings table. Then make the following plots (A vs B means A is on the y axis):
### (a) Points scored vs Possession
###(b) Points scored vs Save percentage
###(c) Points scored vs Goal against
### (d) Clean sheet percentage vs Save percentage
new_columns %>% left_join(teamstats, by='Squad', c("Poss", "Ast", "PK"))
new_columns %>% left_join(teamstats, by='Squad', c("Poss", "Ast", "PK"))
new_columns <- goalkeepers %>% group_by(Squad) %>% summarise(`Save%` = mean(`Save%`, na.rm = TRUE), `CS%` = mean(`CS%`, na.rm = TRUE))
new_columns <- new_columns %>% left_join(teamstats, by='Squad', c("Poss", "Ast", "PK"))
new_standings <- standings %>% left_join(new_columns, by = 'Squad')
new_standings1 <- select(new_standings, Squad, Poss, Ast, PK, `Save%`, `CS%`)
Latest.standing<- standings %>% left_join(new_standings1)
plot(Latest.standing$Pts, Latest.standing$Poss)
plot(Latest.standing$Pts, Latest.standing$`Save%`)
plot(Latest.standing$Pts, Latest.standing$GA)

```



```{r}
### 7
### Identifying the names of the top 3 referees that refereed most games. Showing the columns for the week, date the games were played, names of the home and away teams and the names of the referees.
scoresfixtures %>% mutate(Referee=fct_lump(Referee, n=3)) %>% count(Referee, Wk, Date, Home, Away,  sort = TRUE)
```

```{r}
### 8
### Obtaining the list of the games played by the top scoring teams (the 3 top teams that scored the most goals.). Showing the weeks, date, home and away teams and the results. 
scoresfixtures %>% mutate(Score=fct_lump(Score, n=3)) %>% count(Wk, Date, Home, Away,Score, sort = TRUE)
```

```{r}
### 9.
### Finding the 3 days with the most games played.
scoresfixtures %>% mutate(Day, fct_lump(Day)) %>% count(Day, sort = TRUE)
```

```{r}
### 10 Finding the 3 days with the most games played.
scoresfixtures %>% mutate(Day, fct_lump(Day, n=3)) %>% count(Day, sort = TRUE)
```

