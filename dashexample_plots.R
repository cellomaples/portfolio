

library(ggplot2)
library(dplyr)


# Read in df
df <- read.csv("fake_data1.csv", stringsAsFactors = FALSE)
dfdepression <- read.csv("fake_data2.csv", stringsAsFactors = FALSE)
dfdepression$`Cantril Scale` <- NA
for(i in 1:nrow(dfdepression)){
  if(dfdepression$cantril_ladder_steps[i] >= 7 & dfdepression$cantril_5years[i] >= 8){
    dfdepression$`Cantril Scale`[i] <- "Thriving"
  } else
  if(dfdepression$cantril_ladder_steps[i] <= 4 & dfdepression$cantril_ladder_steps[i] <= 4){
    dfdepression$`Cantril Scale`[i] <- "Suffering"
  }
}

for(i in 1:nrow(dfdepression)){
  if(is.na(dfdepression$`Cantril Scale`[i])){
    dfdepression$`Cantril Scale`[i] <- "Struggling"
  }
}

names(dfdepression)[names(dfdepression) == "little_interest_pleasure"] <- "Little Interest/Pleasure in Things"
names(dfdepression)[names(dfdepression) == "feeling_down_depressed"] <- "Feeling Down Depressed"

dfdepression$`Little Interest/Pleasure in Things` <- as.character(dfdepression$`Little Interest/Pleasure in Things`)
for(i in 1:nrow(dfdepression)){
  if(dfdepression$`Little Interest/Pleasure in Things`[i] == 0){
    dfdepression$`Little Interest/Pleasure in Things`[i] <- "Not at all"
  }
  if(dfdepression$`Little Interest/Pleasure in Things`[i] == 1){
    dfdepression$`Little Interest/Pleasure in Things`[i] <- "Several days"
  }
  if(dfdepression$`Little Interest/Pleasure in Things`[i] == 2){
    dfdepression$`Little Interest/Pleasure in Things`[i] <- "> half the days"
  }
  if(dfdepression$`Little Interest/Pleasure in Things`[i] == 3){
    dfdepression$`Little Interest/Pleasure in Things`[i] <- "Nearly every day"
  }
}

dfdepression$`Feeling Down Depressed` <- as.character(dfdepression$`Feeling Down Depressed`)
for(i in 1:nrow(dfdepression)){
  if(dfdepression$`Feeling Down Depressed`[i] == 0){
    dfdepression$`Feeling Down Depressed`[i] <- "Not at all" 
  }
  if(dfdepression$`Feeling Down Depressed`[i] == 1){
    dfdepression$`Feeling Down Depressed`[i] <- "Several days" 
  }
  if(dfdepression$`Feeling Down Depressed`[i] == 2){
    dfdepression$`Feeling Down Depressed`[i] <- "> half the days" 
  }
  if(dfdepression$`Feeling Down Depressed`[i] == 3){
    dfdepression$`Feeling Down Depressed`[i] <- "Nearly every day" 
  }
}
# Rename columns

# Demographics
names(df)[names(df) == "AGE_ASOF"] <- "Age"
names(df)[names(df) == "SEX"] <- "Sex"
names(df)[names(df) == "TOT_SAL"] <- "Salary"

# Health risk profile
names(df)[names(df) == "HEALTHRISKCAT"] <- "Risk Profile"

# Healthy culture
names(df)[names(df) == "SUPERVISOR_SUPPORT"] <- "Supervisor Support"
names(df)[names(df) == "PHYSICAL_ENVIRONMENT"] <- "Physical Environment"

# Health behaviors
names(df)[names(df) == "LOWFRUITVEGE_CAT"] <- "Low Fruit Veg"
names(df)[names(df) == "LACKEXERCISE_CAT"] <- "Lack Exercise"
names(df)[names(df) == "SMOKING_CAT"] <- "Smoking"
names(df)[names(df) == "LACKSLEEP_CAT"] <- "Lack Sleep"
names(df)[names(df) == "HIGHBMI_CAT"] <- "High BMI"
names(df)[names(df) == "UNMANSTRESS_CAT"] <- "Unmanaged Stress"

# Productivity barriers
names(df)[names(df) == "PROD_HEALTH"] <- "Health"
names(df)[names(df) == "PROD_CAREGIV"] <- "Caregiving"
names(df)[names(df) == "PROD_LACKRES"] <- "Lack Resources"
names(df)[names(df) == "PROD_LACKTIME"] <- "Lack Time"
names(df)[names(df) == "PROD_FIN"] <- "Financial"
names(df)[names(df) == "PROD_LACKTRAIN"] <- "Lack Training"

# Summary Stats Tables

agetable <- function(year = "2018"){
  if(is.null(year)){
    err <- "Please select one or more years."
    stop(err)
  }
  suppressPackageStartupMessages(library(scales))
  rowsage <- c()
  yearsage <- strsplit(year, split = " ")
  for(i in 1:length(yearsage)){
    rowsagetemp <- which(df$PHAYR == yearsage[[i]])
    rowsage <- c(rowsage, rowsagetemp)
  }
  dfage <- subset(df[rowsage,], select = c("Age", "PHAYR"))
  dfage <- group_by(dfage, PHAYR)
  dfage <- summarize(dfage, Year = unique(PHAYR), `Mean Age` = round(mean(Age),1), 
                     `Median Age` = median(Age))
  dfage$PHAYR <- NULL
  dfage$`Mean Age` <- as.character(dfage$`Mean Age`)
  dfage$`Median Age` <- as.character(dfage$`Median Age`)
  
  print(dfage)
}

salarytable <- function(year = "2018"){
  if(is.null(year)){
    err <- "Please select one or more years."
    stop(err)
  }
  suppressPackageStartupMessages(library(scales))
  rowsalary <- c()
  yearsalary <- strsplit(year, split = " ")
  for(i in 1:length(yearsalary)){
    rowsalarytemp <- which(df$PHAYR == yearsalary[[i]])
    rowsalary <- c(rowsalary, rowsalarytemp)
  }
  dfsalary <- subset(df[rowsalary,], select = c("Salary", "PHAYR"))
  dfsalary <- group_by(dfsalary, PHAYR)
  dfsalary <- summarize(dfsalary, Year = unique(PHAYR), `Mean Salary` = dollar(round(mean(Salary),0)),
                        `Median Salary` = dollar(median(Salary)))
  dfsalary$PHAYR <- NULL
  dfsalary$`Mean Salary` <- as.character(dfsalary$`Mean Salary`)
  dfsalary$`Median Salary` <- as.character(dfsalary$`Median Salary`)
  
  print(dfsalary)
}


# Plot functions

mypalette <- c("deepskyblue", "blue", "blue4")

barrisk <- function(profile = "Risk Profile", year = "2018"){
  if(is.null(year)){
    err <- "Please select one or more years."
    stop(err)
  }
  if(profile == "Risk Profile"){
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(scales))
  rowsrisk <- c()
  yearsrisk <- strsplit(year, split = " ")
  for(i in 1:length(yearsrisk)){
    rowsrisktemp <- which(df$PHAYR == yearsrisk[[i]])
    rowsrisk <- c(rowsrisk, rowsrisktemp)
  }
  dftemprisk <- subset(df[rowsrisk,], select = c("Risk Profile", "PHAYR"))
  dftemprisk[,1] <- factor(dftemprisk[,1], levels = c("Low", "Mod", "High"))
  p <- ggplot(dftemprisk, aes(dftemprisk[,1], group = dftemprisk[,2]))
  p <- p + geom_bar(aes(y = ..prop.., fill = factor(dftemprisk[,2])), stat = "count")
  p <- p + geom_text(aes(label = paste0(round(..prop..*100,1),"%"),
                         y = ..prop..), stat = "count", vjust = -.3, color = "black", size = 5)
  p <- p + scale_y_continuous(labels = function(x) paste0(x*100, "%"))
  p <- p + xlab("Health Risk Profile")
  p <- p + facet_grid(cols = vars(dftemprisk[,2]))
  p <- p + theme(legend.position = "none",
                 axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, color = "black"),
                 axis.title.y = element_blank(),
                 plot.title = element_text(size = 20),
                 axis.title = element_text(size = 18, color = "black"),
                 axis.text = element_text(size = 14, color = "black"),
                 strip.text.x = element_text(size = 14, color = "black"),
                 axis.ticks = element_line(color = "black"),
                 strip.background.x = element_rect(fill = "snow"),
                 panel.background = element_rect(fill = "snow"),
                 plot.background = element_rect(fill = "snow"),
                 panel.grid = element_blank())
  p <- p + scale_fill_manual(values = mypalette)
  print(p)
}
  if(profile == "Cantril Scale"){
    suppressPackageStartupMessages(library(ggplot2))
    suppressPackageStartupMessages(library(scales))
    rowsrisk <- c()
    yearsrisk <- strsplit(year, split = " ")
    for(i in 1:length(yearsrisk)){
      rowsrisktemp <- which(dfdepression$PHAYR == yearsrisk[[i]])
      rowsrisk <- c(rowsrisk, rowsrisktemp)
    }
    dftemprisk <- subset(dfdepression[rowsrisk,], select = c("Cantril Scale", "PHAYR"))
    dftemprisk[,1] <- factor(dftemprisk[,1], levels = c("Suffering", "Struggling", "Thriving"))
    p <- ggplot(dftemprisk, aes(dftemprisk[,1], group = dftemprisk[,2]))
    p <- p + geom_bar(aes(y = ..prop.., fill = factor(dftemprisk[,2])), stat = "count")
    p <- p + geom_text(aes(label = paste0(round(..prop..*100,1),"%"),
                           y = ..prop..), stat = "count", vjust = -.3, color = "black", size = 5)
    p <- p + scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0,.5))
    p <- p + xlab("Cantril Scale")
    p <- p + facet_grid(cols = vars(dftemprisk[,2]))
    p <- p + theme(legend.position = "none",
                   axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, color = "black"),
                   axis.title.y = element_blank(),
                   plot.title = element_text(size = 20),
                   axis.title = element_text(size = 18, color = "black"),
                   axis.text = element_text(size = 13, color = "black"),
                   strip.text.x = element_text(size = 14, color = "black"),
                   axis.ticks = element_line(color = "black"),
                   strip.background.x = element_rect(fill = "snow"),
                   panel.background = element_rect(fill = "snow"),
                   plot.background = element_rect(fill = "snow"),
                   panel.grid = element_blank())
    p <- p + scale_fill_manual(values = mypalette)
    print(p)
  }
  if(profile == "Little Interest/Pleasure in Things"){
    suppressPackageStartupMessages(library(ggplot2))
    suppressPackageStartupMessages(library(scales))
    rowsrisk <- c()
    yearsrisk <- strsplit(year, split = " ")
    for(i in 1:length(yearsrisk)){
      rowsrisktemp <- which(dfdepression$PHAYR == yearsrisk[[i]])
      rowsrisk <- c(rowsrisk, rowsrisktemp)
    }
    dftemprisk <- subset(dfdepression[rowsrisk,], select = c("Little Interest/Pleasure in Things", "PHAYR"))
    dftemprisk[,1] <- factor(dftemprisk[,1], levels = c("Not at all", "Several days",
                                                        "> half the days", "Nearly every day"))
    p <- ggplot(dftemprisk, aes(dftemprisk[,1], group = dftemprisk[,2]))
    p <- p + geom_bar(aes(y = ..prop.., fill = factor(dftemprisk[,2])), stat = "count")
    p <- p + geom_text(aes(label = paste0(round(..prop..*100,1),"%"),
                           y = ..prop..), stat = "count", vjust = -.3, color = "black", size = 5)
    p <- p + scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0,.3))
    p <- p + xlab("Over the Last Two Weeks")
    p <- p + facet_grid(cols = vars(dftemprisk[,2]))
    p <- p + theme(legend.position = "none",
                   axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, color = "black"),
                   axis.title.y = element_blank(),
                   plot.title = element_text(size = 20),
                   axis.title = element_text(size = 18, color = "black"),
                   axis.text = element_text(size = 13, color = "black"),
                   strip.text.x = element_text(size = 14, color = "black"),
                   axis.ticks = element_line(color = "black"),
                   strip.background.x = element_rect(fill = "snow"),
                   panel.background = element_rect(fill = "snow"),
                   plot.background = element_rect(fill = "snow"),
                   panel.grid = element_blank())
    p <- p + scale_fill_manual(values = mypalette)
    print(p)
  }
  if(profile == "Feeling Down Depressed"){
    suppressPackageStartupMessages(library(ggplot2))
    suppressPackageStartupMessages(library(scales))
    rowsrisk <- c()
    yearsrisk <- strsplit(year, split = " ")
    for(i in 1:length(yearsrisk)){
      rowsrisktemp <- which(dfdepression$PHAYR == yearsrisk[[i]])
      rowsrisk <- c(rowsrisk, rowsrisktemp)
    }
    dftemprisk <- subset(dfdepression[rowsrisk,], select = c("Feeling Down Depressed", "PHAYR"))
    dftemprisk[,1] <- factor(dftemprisk[,1], levels = c("Not at all", "Several days",
                                                        "> half the days", "Nearly every day"))
    p <- ggplot(dftemprisk, aes(dftemprisk[,1], group = dftemprisk[,2]))
    p <- p + geom_bar(aes(y = ..prop.., fill = factor(dftemprisk[,2])), stat = "count")
    p <- p + geom_text(aes(label = paste0(round(..prop..*100,1),"%"),
                           y = ..prop..), stat = "count", vjust = -.3, color = "black", size = 5)
    p <- p + scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0,.3))
    p <- p + xlab("Over the Last Two Weeks")
    p <- p + facet_grid(cols = vars(dftemprisk[,2]))
    p <- p + theme(legend.position = "none",
                   axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, color = "black"),
                   axis.title.y = element_blank(),
                   plot.title = element_text(size = 20),
                   axis.title = element_text(size = 18, color = "black"),
                   axis.text = element_text(size = 13, color = "black"),
                   strip.text.x = element_text(size = 14, color = "black"),
                   axis.ticks = element_line(color = "black"),
                   strip.background.x = element_rect(fill = "snow"),
                   panel.background = element_rect(fill = "snow"),
                   plot.background = element_rect(fill = "snow"),
                   panel.grid = element_blank())
    p <- p + scale_fill_manual(values = mypalette)
    print(p)
  }
}

bardem <- function(demographic = "Age", year = "2018"){
  if(is.null(year)){
    err <- "Please select one or more years."
    stop(err)
  }
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(scales))
  rowsdem <- c()
  yearsdem <- strsplit(year, split = " ")
  for(i in 1:length(yearsdem)){
    rowsdemtemp <- which(df$PHAYR == yearsdem[[i]])
    rowsdem <- c(rowsdem, rowsdemtemp)
  }
  if(demographic == grep(demographic, colnames(df), value = TRUE)){
    dftempdem <- subset(df[rowsdem,], select = c(demographic, "PHAYR"))
    if(demographic == "Sex"){
      p <- ggplot(dftempdem, aes(dftempdem[,1], group = dftempdem[,2]))
      p <- p + geom_bar(aes(y = ..prop.., fill = factor(dftempdem[,2])), stat = "count")
      p <- p + geom_text(aes(label = paste0(round(..prop..*100,1),"%"),
                             y = ..prop..), stat = "count", vjust = -.3, color = "black", size = 5)
      p <- p + scale_y_continuous(labels = function(x) paste0(x*100, "%"), breaks = c(0,.2,.4,.6,.8,1))
      p <- p + xlab("Sex")
      p <- p + facet_grid(cols = vars(dftempdem[,2]))
      p <- p + theme(legend.position = "none",
                     axis.title.y = element_blank(),
                     plot.title = element_text(size = 20),
                     axis.title = element_text(size = 18, color = "black"),
                     axis.text = element_text(size = 14, color = "black"),
                     strip.text.x = element_text(size = 14, color = "black"),
                     axis.ticks = element_line(color = "black"),
                     strip.background.x = element_rect(fill = "snow"),
                     panel.background = element_rect(fill = "snow"),
                     plot.background = element_rect(fill = "snow"),
                     panel.grid = element_blank())
      p <- p + scale_fill_manual(values = mypalette)
      print(p)
    }
    if(demographic == "Age"){
      p <- ggplot(dftempdem, aes(dftempdem[,1], group = dftempdem[,2]))
      p <- p + geom_histogram(aes(fill = factor(dftempdem[,2])), stat = "count")
      p <- p + xlab("Age")
      p <- p + ylab("Count")
      p <- p + facet_grid(cols = vars(dftempdem[,2]))
      p <- p + theme(legend.position = "none",
                     plot.title = element_text(size = 20),
                     axis.title = element_text(size = 18, color = "black"),
                     axis.text = element_text(size = 14, color = "black"),
                     strip.text.x = element_text(size = 14, color = "black"),
                     axis.ticks = element_line(color = "black"),
                     strip.background.x = element_rect(fill = "snow"),
                     panel.background = element_rect(fill = "snow"),
                     plot.background = element_rect(fill = "snow"),
                     panel.grid = element_blank())
      p <- p + scale_fill_manual(values = mypalette)
      print(p)
    }
    if(demographic == "Salary"){
      p <- ggplot(dftempdem, aes(dftempdem[,1], group = dftempdem[,2]))
      p <- p + geom_histogram(aes(x=dftempdem[,1], fill = factor(dftempdem[,2])), bins = 40)
      p <- p + scale_x_continuous(labels = dollar, breaks = c(25000*c(1:5)))
      p <- p + scale_y_continuous(breaks = c(100*c(1:7)))
      p <- p + coord_cartesian(xlim = c(min(dftempdem[,1]), 150000))
      p <- p + xlab("Salary")
      p <- p + ylab("Count")
      p <- p + facet_grid(cols = vars(dftempdem[,2]))
      p <- p + theme(legend.position = "none",
                     axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1),
                     plot.title = element_text(size = 20),
                     axis.title = element_text(size = 18, color = "black"),
                     axis.text = element_text(size = 14, color = "black"),
                     strip.text.x = element_text(size = 14, color = "black"),
                     axis.ticks = element_line(color = "black"),
                     strip.background.x = element_rect(fill = "snow"),
                     panel.background = element_rect(fill = "snow"),
                     plot.background = element_rect(fill = "snow"),
                     panel.grid = element_blank())
      p <- p + scale_fill_manual(values = mypalette)
      print(p)
    }
  }
}

barcult <- function(culture = "Supervisor Support", year = "2018"){
  if(is.null(year)){
    err <- "Please select one or more years."
    stop(err)
  }
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(scales))
  rowscult <- c()
  yearscult <- strsplit(year, split = " ")
  for(i in 1:length(yearscult)){
    rowsculttemp <- which(df$PHAYR == yearscult[[i]])
    rowscult <- c(rowscult, rowsculttemp)
  }
  if(culture == grep(culture, colnames(df), value = TRUE)){
    dftempcult <- subset(df[rowscult,], select = c(culture, "PHAYR"))
    dftempcult[,1] <- factor(dftempcult[,1], levels = c("Strongly Agree", "Agree", "Disagree"))
    p <- ggplot(dftempcult, aes(dftempcult[,1], group = dftempcult[,2]))
    p <- p + geom_bar(aes(y = ..prop.., fill = factor(dftempcult[,2])), stat = "count")
    p <- p + geom_text(aes(label = paste0(round(..prop..*100,1),"%"),
                           y = ..prop..), stat = "count", vjust = -.3, color = "black", size = 5)
    p <- p + scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0,.4))
    p <- p + xlab("Response")
    p <- p + facet_grid(cols = vars(dftempcult[,2]))
    p <- p + theme(legend.position = "none",
                   axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1),
                   axis.title.y = element_blank(),
                   plot.title = element_text(size = 20),
                   axis.title = element_text(size = 18, color = "black"),
                   axis.text = element_text(size = 13, color = "black"),
                   strip.text.x = element_text(size = 14, color = "black"),
                   axis.ticks = element_line(color = "black"),
                   strip.background.x = element_rect(fill = "snow"),
                   panel.background = element_rect(fill = "snow"),
                   plot.background = element_rect(fill = "snow"),
                   panel.grid = element_blank())
    p <- p + scale_fill_manual(values = mypalette)
    print(p)
  }
}

barhealth <- function(behavior = "Low Fruit Veg", year = "2018"){
  if(is.null(year)){
    err <- "Please select one or more years."
    stop(err)
  }
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(scales))
  rowshealth <- c()
  yearshealth <- strsplit(year, split = " ")
  for(i in 1:length(yearshealth)){
    rowshealthtemp <- which(df$PHAYR == yearshealth[[i]])
    rowshealth <- c(rowshealth, rowshealthtemp)
  }
  if(behavior == grep(behavior, colnames(df), value = TRUE)){
    dftemphealth <- subset(df[rowshealth,], select = c(behavior, "PHAYR"))
    p <- ggplot(dftemphealth, aes(dftemphealth[,1], group = dftemphealth[,2]))
    p <- p + geom_bar(aes(y = ..prop.., fill = factor(dftemphealth[,2])), stat = "count")
    p <- p + geom_text(aes(label = paste0(round(..prop..*100,1),"%"),
                           y = ..prop..), stat = "count", vjust = -.3, color = "black", size = 5)
    p <- p + scale_y_continuous(labels = function(x) paste0(x*100, "%"), breaks = c(0,.2,.4,.6,.8,1))
    p <- p + xlab("Response")
    p <- p + facet_grid(cols = vars(dftemphealth[,2]))
    p <- p + theme(legend.position = "none",
                   axis.title.y = element_blank(),
                   plot.title = element_text(size = 20),
                   axis.title = element_text(size = 18, color = "black"),
                   axis.text = element_text(size = 14, color = "black"),
                   strip.text.x = element_text(size = 14, color = "black"),
                   axis.ticks = element_line(color = "black"),
                   strip.background.x = element_rect(fill = "snow"),
                   panel.background = element_rect(fill = "snow"),
                   plot.background = element_rect(fill = "snow"),
                   panel.grid = element_blank())
    p <- p + scale_fill_manual(values = mypalette)
    print(p)
  }
}

barprod <- function(productivity = "Health", year = "2018"){
  if(is.null(year)){
    err <- "Please select one or more years."
    stop(err)
  }
  suppressPackageStartupMessages(library(ggplot2))
  suppressPackageStartupMessages(library(scales))
  rowsprod <- c()
  yearsprod <- strsplit(year, split = " ")
  for(i in 1:length(yearsprod)){
    rowsprodtemp <- which(df$PHAYR == yearsprod[[i]])
    rowsprod <- c(rowsprod, rowsprodtemp)
  }
  if(productivity == grep(productivity, colnames(df), value = TRUE)){
    dftempprod <- subset(df[rowsprod,], select = c(productivity, "PHAYR"))
    p <- ggplot(dftempprod, aes(dftempprod[,1], group = dftempprod[,2]))
    p <- p + geom_bar(aes(y = ..prop.., fill = factor(dftempprod[,2])), stat = "count")
    p <- p + geom_text(aes(label = paste0(round(..prop..*100,1),"%"),
                           y = ..prop..), stat = "count", vjust = -.3, color = "black", size = 5)
    p <- p + scale_y_continuous(labels = function(x) paste0(x*100, "%"))
    p <- p + xlab("Response")
    p <- p + facet_grid(cols = vars(dftempprod[,2]))
    p <- p + theme(legend.position = "none",
                   axis.title.y = element_blank(),
                   plot.title = element_text(size = 20),
                   axis.title = element_text(size = 18, color = "black"),
                   axis.text = element_text(size = 14, color = "black"),
                   strip.text.x = element_text(size = 14, color = "black"),
                   axis.ticks = element_line(color = "black"),
                   strip.background.x = element_rect(fill = "snow"),
                   panel.background = element_rect(fill = "snow"),
                   plot.background = element_rect(fill = "snow"),
                   panel.grid = element_blank())
    p <- p + scale_fill_manual(values = mypalette)
    print(p)
  }
}

