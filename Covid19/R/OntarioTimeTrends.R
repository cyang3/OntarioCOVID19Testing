###############
# COVID19 Testing in Ontario 
# March 23, 2020
#############

dat.file <- list.files(path = "./Data/", pattern = paste0("*",Sys.Date(),".csv"), full.names = TRUE)
test.dat <- read.csv (dat.file, stringsAsFactors = FALSE, header = 1)
plot.dat <- test.dat[!duplicated(test.dat$Date),]
plot.dat$Date <- as.Date(plot.dat$Date, format = "%d-%m-%y")
plot.dat$Days.from.start <- plot.dat$Date-plot.dat$Date[1]
plot.dat$Percent.results.pending <- 100 * plot.dat$Currently.Under.Investigation/plot.dat$Total.approved.for.COVID.testing.to.date
plot.dat$Percent.tested.negative <- 100 * plot.dat$Negative/plot.dat$Total.approved.for.COVID.testing.to.date
plot.dat$Percent.tested.positive <- 100 - (plot.dat$Percent.tested.negative + plot.dat$Percent.results.pending)

##########
# Data cleanup
#########

barplot.dat <- 
  rbind (
    data.frame (
        Days.from.start = seq (0,max(plot.dat$Days.from.start)) [!(seq (0,max(plot.dat$Days.from.start)) %in% plot.dat$Days.from.start)],
        Percent.results.pending = NA,
        Percent.tested.negative = NA,
        Percent.tested.positive = NA,
        Negative = NA,
        Currently.Under.Investigation= NA,
        Confirmed.positive= NA,
        Resolved= NA,
        Deceased= NA,
        Total.approved.for.COVID.testing.to.date = NA,
        stringsAsFactors = FALSE
        ),
    plot.dat[,c(12:15, 3:8)]
    )
barplot.dat <- barplot.dat[order(barplot.dat[,1]),]
barplot.dat$Date <- seq.Date(from = min (plot.dat$Date),to = max (plot.dat$Date),by = 1)
barplot.dat$mp <- barplot (t(barplot.dat[,rev(c(2:4))]),beside = FALSE, plot = FALSE)
barplot.dat$num.test.increase.from.previous <- c(barplot.dat$Total.approved.for.COVID.testing.to.date[1],tail(barplot.dat$Total.approved.for.COVID.testing.to.date, n = nrow(barplot.dat)-1) - 
                                                head(barplot.dat$Total.approved.for.COVID.testing.to.date, n = nrow(barplot.dat)-1))

barplot.dat$num.test.completed <- barplot.dat$Total.approved.for.COVID.testing.to.date-barplot.dat$Currently.Under.Investigation
barplot.dat$num.test.completed.from.previous <- c( barplot.dat$num.test.completed[1], tail(barplot.dat$num.test.completed, n = nrow(barplot.dat)-1) - 
                                                   head(barplot.dat$num.test.completed, n = nrow(barplot.dat)-1))

##########
# Plot cumulative tests in Ontario
#########
pdf(paste0("./Plots/Ontario_Testing_Total_Cases_Over_Time_",
           Sys.Date(), ".pdf"), width = 7, height = 5)
par(xpd = T, mar = par()$mar + c(7,0,0,0))

ycat <- "Total.approved.for.COVID.testing.to.date"
plot.form <- as.formula (paste(ycat,"mp",sep="~"))

plot (plot.form, barplot.dat, cex.axis = 0.6,
      ylab = "Number of individuals",
      xlab = "", xaxt = "n",
      las = 2, pch = 16, col = "black")
axis (1, at = barplot.dat$mp, labels = barplot.dat$Date, cex.axis = 0.60, las = 2)
ycat <- "Currently.Under.Investigation"
plot.form <- as.formula (paste(ycat,"mp",sep="~"))
points (plot.form, barplot.dat, 
      ylab = "Number of individuals",
      las = 2, pch = 16, col = "red")
legend("topleft", c ("Total Tested", "Under Investigation"), 
       title = paste0("(Updated: ", Sys.Date(),")"), bty = "n",
       col = c("black", "red"), pch = c(16,16), cex = 0.8)

dev.off()

##########
# Plot daily tests administered and completed
##########
pdf(paste0("./Plots/Ontario_Testing_Capacity_Over_Time_",
           Sys.Date(), ".pdf"), width = 7, height = 5)
par(xpd = T, mar = par()$mar + c(7,0,0,0))

ycat <- "num.test.increase.from.previous"
plot.form <- as.formula (paste(ycat,"mp",sep="~"))

plot (plot.form, barplot.dat, cex.axis = 0.6,
      ylab = "Number of Tests",
      xlab = "", xaxt = "n",
      las = 2, pch = 16, col = "black", type = "b")
axis (1, at = barplot.dat$mp, labels = barplot.dat$Date, cex.axis = 0.60, las = 2)
ycat <- "num.test.completed.from.previous"
plot.form <- as.formula (paste(ycat,"mp",sep="~"))
points (plot.form, barplot.dat, 
        ylab = "Number of individuals",
        las = 2, pch = 16, col = "blue", type = "b")
legend("topleft", c ("New tests submitted", "Tests completed"), 
       title = paste0("(Updated: ", Sys.Date(),")"), bty = "n",
       col = c("black", "blue"), pch = c(16,16), cex = 0.8)
dev.off()

##########
# Plot daily histogram of test status (in percent)
##########
pdf(paste0("./Plots/Ontario_Testing_Percent_Cases_Over_Time_",
           Sys.Date(), ".pdf"), width = 7, height = 5)
par(xpd = T, mar = par()$mar + c(7,0,0,0))
barplot (t(barplot.dat[,rev(c(2:4))]),
         beside = FALSE, las = 2, cex.names = 0.8,
         names.arg = barplot.dat$Date,
         ylab = "% of individuals tested",
         col = c("red", "blue", "grey"))
legend(15, -80,
       c("Positive", "Negative", "Under Investigation"),
       fill = c("red", "blue", "gray"),
       cex = 0.8)
legend (-0.5, -65, legend = paste0("(Updated: ", Sys.Date(),")"),
      cex = 0.70, bty = "n")
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()

########
# Visualize how far behind are the backlog
########
pdf(paste0("./Plots/Ontario_Testing_Backlog_Over_Time_",
           Sys.Date(), ".pdf"), width = 6.6, height = 3)
par(xpd = T, mar = par()$mar + c(0,0.5,0,5))
# Plot main barplot
xmax <- signif(max(barplot.dat$Total.approved.for.COVID.testing.to.date, na.rm = TRUE), 2)
barplot (cbind(barplot.dat$num.test.increase.from.previous,
               barplot.dat$num.test.completed.from.previous)[!is.na(barplot.dat$num.test.increase.from.previous),],
         horiz = TRUE, col = rainbow(20), las = 1,
         names.arg = c("submitted", "completed"),
         xlab = "Cumulative number of tests",
         xlim = c(0,xmax), 
         cex.names = 0.8, cex.axis = 0.8)
legend (xmax+3000, 3.5, 
        fill = rainbow(20), 
        title = "Date",
        legend = barplot.dat$Date[!is.na(barplot.dat$num.test.increase.from.previous)],
        cex = 0.60)
# Plot cumulative annotation
segments(x0 = sum(barplot.dat$num.test.completed.from.previous, na.rm = TRUE),
         y0 = 0 , y1 = 3, lwd = 2.5, lty = 2,
         col = "black")
text (x = sum(barplot.dat$num.test.completed.from.previous, na.rm = TRUE),
      y = 3.2, col = "red", cex = 0.90,
      labels = barplot.dat$Date[!is.na(barplot.dat$num.test.increase.from.previous)][
        sum(cumsum(barplot.dat$num.test.increase.from.previous[!is.na(barplot.dat$num.test.increase.from.previous)]) < sum(barplot.dat$num.test.completed.from.previous,na.rm = TRUE))+1])
segments(x0 = sum(barplot.dat$num.test.increase.from.previous, na.rm = TRUE),
         y0 = 0 , y1 = 3, lwd = 2.5, lty = 2,
         col = "black")
text (x = sum(barplot.dat$num.test.increase.from.previous, na.rm = TRUE),
      y = 3.2, labels = tail(barplot.dat$Date,n=1), cex = 0.80)
backlog <- tail(plot.dat$Currently.Under.Investigation,n=1)
text (x = sum(barplot.dat$num.test.completed.from.previous, na.rm = TRUE)+ (backlog/2),
      y = 1.9, labels = paste(backlog, "pending"), cex = 0.80)
text (x = 0, y = 3, 
      labels = paste0("(Updated: ", Sys.Date(),")"), cex = 0.70)
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()
