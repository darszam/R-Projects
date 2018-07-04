mlbPlayer = read.table(file=file.choose(),
                       header = T, sep = " ", quote = "",
                       na.strings = "`",
                       stringsAsFactors = F)
playerData = mlbPlayer[, c("RBI", "AVG")]

png(file = "player_rbi_avg.png")
plot(x = playerData$RBI, y = playerData$AVG,
     xlab = "RBI", ylab = "AVG", main = "RBIs and Average")
dev.off()

