rm(list=ls())
library(nhlscrapr)
source("Patch for nhlscrapr.r")

fgd <- full.game.database(extra.seasons = 2)

year <- "20162017"
game_ids <- subset(fgd, season == year)

# Look these up on nhl.com
first_gameid <- 20001
last_gameid <- 20195

game_ids <- subset(game_ids, first_gameid <= gcode & last_gameid >= gcode)

#download the game info from nhl.com
dummy <- download.games(games = game_ids, wait=1)

# parse the game info
process.games(games=game_ids, override.download=FALSE)

# garbage collection
gc()

compile.all.games(new.game.table = game_ids)

temp <- load("source-data\\nhlscrapr-20162017.RData")
events <- get(temp)

temp <- load("source-data\\nhlscrapr-core.Rdata")
roster <- get(temp)

events_at_5v5 <- subset(events, (home.skaters == 6 & away.skaters == 6))
eventsNEN <- subset(events, home.G != 1, away.G != 1)
events_at_5v5NEN <- subset(eventsNEN, (home.skaters == 6 & away.skaters == 6))

roster_name <- aggregate_roster_by_name(roster)

ps <- player.summary(events, roster_name)
ps5v5NEN <- player.summary(events_at_5v5NEN, roster_name)
ps5v5 <- player.summary(events_at_5v5, roster_name)
psNEN <- player.summary(eventsNEN, roster_name)

ps <- as.data.frame(ps[,,1])
ps5v5NEN <- as.data.frame(ps5v5NEN[,,1])
ps5v5 <- as.data.frame(ps5v5[,,1])
psNEN <- as.data.frame(psNEN[,,1])

#TOP SHOTS
ps[ order(-ps[,2]), ]
ps5v5[ order(-ps5v5[,2]), ]
ps5v5NEN [ order(-ps5v5NEN[,2]), ]
#TOP GOALS
ps[ order(-ps[,3]), ]
ps5v5[ order(-ps5v5[,3]), ]

#TOP SOG
ps[ order(-(ps[,2] + ps[,3])), ]
ps5v5[ order(-(ps5v5[,2] + ps5v5[,3])), ]



