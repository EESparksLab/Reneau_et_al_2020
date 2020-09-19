#Reneau et al., 2020 
#Full complement of R scripts used for data analysis in the manuscript:
# <ADD CITATION>

#Erin Sparks
#March 11, 2020

#R Version 3.6.3 for Mac
setwd("~/Dropbox/UD/Writing/Reneau2020/Reneau_et_al_2020/RInputFiles")

#Version 3.3.0
library("ggplot2")
#Version 0.8.5
library("dplyr")
#Version 1.4.3
library("reshape2")
#Version 0.2.5
library("ggpubr")
#Version 0.1.7
library("png")
#Version 1.0.0
library("cowplot")
#Version 0.9.22
library("rptR")
#Version 3.0.8
library("car")
#Version 2.3.25
library("rcompanion")



#input data
#Data used for Figure S1 & S2.  Contains slopes extracted from the three different methods.
data1 <-read.csv ("RepeatTesting_Displacement.csv")
head(data1)
#Data used for ANOVA related to Figure 2. File contains only the time of day data (103d) and brace root intact measurements. 
data2 <- read.csv("CML258_Aonly103.csv")
head(data2)
#Data used for Figure 2. Contains all flexural rigidity measurements with brace roots intact for VT and R5.
data3 <- read.csv("CML258_Aonly.csv")
head(data3)
#Data use for Figure S3 and to calculate repeatability
datarep <- read.csv("CML258_repeatability.csv")
head(datarep)
#Data used for Figure 3A.
data4 <- read.csv("BRnumber.csv")
head(data4)
#Data used for Figure 3B/C.
data5 <- read.csv("CML258_BR.csv")
head(data5)
#Data used for Figure 3D, S4, 4A, 4C, 4D
data6 <- read.csv ("CML258_BRrelative.csv")
head(data6)
#Data used for Figure 4B
data7 <- read.csv("CML258_BReachwhorl.csv")
head(data7)
#Data used for Figure 5
data8 <- read.csv("TusonBRoot_032020.csv")
head(data8)
#Data used for Figure S5
data9 <- read.csv("Sparks_set_in_Teixeira_2015.csv")
head(data9)
#Data line averages used for Figure 5D
data10 <- read.csv("LineAverages.csv")
head(data10) 
#Data used to determine if the change in beam length alters the brace root contribution
data11 <- read.csv("LengthCorrection.csv")
head(data11)



#------------------------------
#Figure S1. Comparison of Slope Extraction Methods

head(data1)
#Correlation between flexural stiffness calculations from different slope extraction methods
cor.test (data1$line_raw_slope..N.m., data1$line_push_slope..N.m., use = "everything", method = c("pearson"))
cor.test (data1$line_raw_slope..N.m., data1$ransac_slope.N.m., use = "everything", method = c("pearson"))
cor.test (data1$line_push_slope..N.m., data1$ransac_slope.N.m., use = "everything", method = c("pearson"))

#Plot correlations

image <- readPNG("FirstSet_2019-9-2-12-55-34.png")
S2A <- ggplot() + 
  background_image(image) 

S2B <- 
  ggplot (data1, aes(line_raw_slope..N.m., ransac_slope.N.m.))+
  geom_point(size = 1, colour = "black") +
  geom_smooth(method="glm", se = TRUE)+
  theme(axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), 
        plot.title=element_text(size=10, vjust=3), 
        axis.text=element_text(size=10), 
        axis.title = element_text(size=10), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 10, colour = "blue"),
        strip.text.y = element_text(size = 10, colour = "black")) +
  xlab("Force-Deflection Slope Load + Unload (N/m)") + 
  ylab("Force-Deflection Slope Loading RANSAC (N/m)") +
    xlim(0,450)+
    ylim(0,450)

S2C <- ggplot (data1, aes(line_push_slope..N.m., ransac_slope.N.m.))+
  geom_point(size = 1, colour = "black") +
  geom_smooth(method="glm", se = TRUE)+
  theme(axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), 
        plot.title=element_text(size=10, vjust=3), 
        axis.text=element_text(size=10), 
        axis.title = element_text(size=10), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 10, colour = "blue"),
        strip.text.y = element_text(size = 10, colour = "black")) +
  xlab("Force-Deflection Slope Loading (N/m)") + 
  ylab("Force-Deflection Slope Loading RANSAC (Nm^2)") +
  ylim(0,450) +
  xlim(0,450)

S2D <- ggplot (data1, aes(line_raw_slope..N.m., line_push_slope..N.m.))+
  geom_point(size = 1, colour = "black") +
  geom_smooth(method="glm", se = TRUE)+
  theme(axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), 
        plot.title=element_text(size=10, vjust=3), 
        axis.text=element_text(size=10), 
        axis.title = element_text(size=10), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 10, colour = "blue"),
        strip.text.y = element_text(size = 10, colour = "black")) +
  xlab("Force-Deflection Slope Load + Unload (N/m)") + 
  ylab("Force-Deflection Slope Loading (N/m)") +
  ylim(0,450) +
  xlim(0,450)


#Top Left = 0, 0.5. 
#Top Right = 0.5, 0.5
#Bottom Right = 0.5, 0
#Bottom Left = 0, 0

#Save as pdf:  8.5in Wide x 8in= High
ggdraw() +
  draw_plot(S2A, x = 0, y = 0.5, width = .5, height = .5) +
  draw_plot(S2B, x = 0.5, y = 0.5, width = .5, height = .5) +
  draw_plot(S2C, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(S2D, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), 
                  size = 15,
                  x = c(0, 0.5, 0, 0.5), 
                  y = c(1, 1, 0.5, 0.5))




#------------------------------
#Figure 1.Methods

image2 <- readPNG("BraceRootsFlexProcess-01.png")
F1A <- ggplot() + 
  background_image(image2) 

#Save as pdf: 4 Wide x 3 High
ggdraw(xlim = c(0, 1), ylim = c(0, 0.8)) +
  draw_plot(F1A, x = 0, y = 0, width = 1, height = 0.75) +
  draw_plot_label(label = c("A", "B", "C"), 
                  size = 20,
                  x = c(.15, 0.48, 0.78), 
                  y = c(0.8, 0.8, 0.8))



#------------------------------
#Figure 2.Flexural Rigidity Changes by Day Time and Developmental Stage

#Keep order of data for time
data3$time = factor(data3$time, levels = unique(data3$time))

head(data3)
#Plot time of day by EI and separate by plant and plot

data3 %>%
  mutate(flag = ifelse(data3$time == 'R5 - 12PM', T, F)) %>%
ggplot (aes(time, line_raw_slope.N.m.))+
  geom_boxplot(size = 0.25, fill = "grey", color = "black") + 
  geom_point(size = 1, color = data3$test) +
  facet_grid(plot ~ plant)+
  theme(axis.text.x = element_text(size=12, angle = 90), 
        axis.text.y = element_text(size=12), 
        plot.title=element_text(size=12, vjust=3), 
        axis.text=element_text(size=12), 
        axis.title = element_text(size=12), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 12, colour = "blue"),
        strip.text.y = element_text(size = 12, colour = "black")) +
  xlab("") + 
  ylab("Force-Deflection Slope (N/m)") 

#Save as pdf: 8.5 Wide x 6 High


#ANOVA analyses of effect for time, plot and test on only the 103day data
shapiro.test(data2$line_raw_slope.N.m.)
ggdensity(data2$line_raw_slope.N.m., 
          main = "Density plot",
          xlab = "Slope")
qqPlot(data2$line_raw_slope.N.m.,
       ylab = "Slope")

#Data is not normally distributed, so a Tukey's Ladder of Powers was applied to transform the data.
data2$tukey <- transformTukey(
  data2$line_raw_slope.N.m.,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)

res.aov <- aov(tukey ~ time + plot +test + time:plot, data = data2)
summary(res.aov)

res.aov <- aov(tukey ~ time, data = data2)
summary(res.aov)
tukey.test <- TukeyHSD(res.aov)
tukey.test


#------------------------------
#Figure S2. Determine the repeatability of tests.

head(datarep)
rep <- 
  rpt(line_raw_slope.N.m. ~ (1 | uniqueID), grname="uniqueID", data = datarep, nboot = 1000, npermut = 0, datatype="Gaussian")
summary(rep)
plot(rep, cex.main=1, main="")

#Save as pdf: 4 Wide x 4 High


#------------------------------
#Figure 3. Brace roots significantly contribute to flexural rigidity

#3A. Number of BR nodes


F3A <- 
  ggplot (data4, aes(x=Brnumber, color=plot))+
  geom_histogram(bins = 3, binwidth = 1, position=position_dodge(width=0.7), fill="white")+
  theme(axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=17), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 15, colour = "blue"),
        strip.text.y = element_text(size = 15, colour = "black"),
        legend.title=element_blank()) +
  xlab("Number of Brace Root Whorls in Soil") + 
  ylab("Number of Plants") +
  xlim(0,4) +
  scale_y_continuous(breaks=seq(0,10,2))


res.aov <- aov(Brnumber ~ plot, data = data4)
summary(res.aov)

shapiro.test(data4$line_raw_slope..N.m.)
data4$tukey <- transformTukey(
  data4$line_raw_slope..N.m.,
  start = -10,
  end = 10,
  int = 0.025,
  plotit = TRUE,
  verbose = FALSE,
  quiet = FALSE,
  statistic = 1,
  returnLambda = FALSE
)

res.aov <- aov(line_raw_slope..N.m. ~ plot, data = data4)
summary(res.aov)

#3B. EI versus test number

F3B <- 
  ggplot (data5, aes(whirl_id, line_raw_slope..N.m.))+
  geom_violin(fill = "grey")+
  geom_point(size = 1, colour = data5$color) +
  theme(axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=15), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.5,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 15, colour = "blue"),
        strip.text.y = element_text(size = 15, colour = "black")) +
  xlab("Brace Roots Removed ------>") + 
  ylab("Force-Deflection Slope (N/m)")+
  ylim(0,450)

res.aov <- aov(line_raw_slope..N.m. ~ whirl_id, data = data5)
summary(res.aov)
tukey.test <- TukeyHSD(res.aov)
tukey.test

#3C Individual plants
F3C <- 
  ggplot (data5, aes(whirl_id, line_raw_slope..N.m.))+
  geom_point(size = 1, colour = "black") +
  facet_grid(plot ~ plant)+
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=15), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.1,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 15, colour = "blue"),
        strip.text.y = element_text(size = 15, colour = "black")) +
  xlab("Brace Roots Removed ------>") + 
  ylab("Force-Deflection Slope (N/m)")+
  ylim(0,450)

#3D Ratio and Difference

F3D1 <- 
  ggplot (data6, aes(operator, Ratio))+
  geom_violin(fill = "grey") +
  geom_point(size = 2, colour = "black") +
  theme(axis.text.x = element_text(size=0), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=15), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        axis.ticks.x = element_blank(),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  ylab("FDwo / FDw") + 
  xlab("") +
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,0.25))

F3D2 <- 
  ggplot (data6, aes(operator, Difference))+
  geom_violin(fill = "grey") +
  geom_point(size = 2, colour = "black") +
  theme(axis.text.x = element_text(size=0), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=15), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
       axis.ticks.length.x = unit(.2,"cm"),
        axis.ticks.x = element_blank(),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  ylab("FDw - FDwo") + 
  xlab("") +
  ylim(0,250)

cor.test (data6$Ratio, data6$Difference, use = "everything", method = c("pearson"))

shapiro.test(data6$Difference)
res.aov <- aov(Difference ~ plot, data = data6)
summary(res.aov)

shapiro.test(data6$Ratio)
res.aov <- aov(Ratio ~ plot, data = data6)
summary(res.aov)

#Save as pdf:  10 x 8.5
ggdraw() +
  draw_plot(F3A, x = 0.01, y = 0.52, width = .46, height = .44) +
  draw_plot(F3B, x = 0.52, y = 0.52, width = .46, height = .44) +
  draw_plot(F3C, x = 0.01, y = 0.01, width = 0.66, height = 0.44) +
  draw_plot(F3D1, x = 0.7, y = 0, width = 0.13, height = 0.44) +
  draw_plot(F3D2, x = 0.85, y = 0, width = 0.13, height = 0.44) +
  draw_plot_label(label = c("A", "B", "C", "D"), 
                  size = 15,
                  x = c(0, 0.51, 0, 0.7),
                  y = c(.98, .98, 0.47, 0.47))


#------------------------------
#Figure S3. There is Little or No Correlation Between Brace Root Contribution and Initial Flexural Rigidity. 

head(data6)
S3A <- ggplot (data6, aes(line_raw_slope.N.m., Difference))+
  geom_point(size = 2, colour = "black") +
  geom_smooth(method="glm",se=TRUE)+
  theme(axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=15), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  ylab("Difference (N/m)") + 
  xlab("Force-Deflection Slope (N/m)") +
  xlim (0,450)

cor.test (data6$line_raw_slope.N.m., data6$Difference, use = "everything", method = c("pearson"))


S3B <- 
  ggplot (data6, aes(line_raw_slope.N.m., Ratio))+
  geom_point(size = 2, colour = "black") +
  geom_smooth(method="glm",se=TRUE)+
  theme(axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=15), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  ylab("Ratio") + 
  xlab("Force-Deflection Slope (N/m)") +
  xlim (0,450) +
  ylim (0,1)

cor.test (data6$line_raw_slope.N.m., data6$Ratio, use = "everything", method = c("pearson"))

ggdraw(xlim = c(0, 1.1), ylim = c(0.5, 1)) +
  draw_plot(S3A, x = 0.01, y = 0.5, width = .5, height = .5) +
  draw_plot(S3B, x = 0.55, y = 0.5, width = .5, height = .5) +
  draw_plot_label(label = c("A", "B"), 
                  size = 15,
                  x = c(0, 0.55), 
                  y = c(1, 1))
#Save as pdf:  8.5 x 4.5



#------------------------------
#Figure 4.Brace Root Whorls Have Differential Contribution to Flexural Rigidity.

#4A. Total ratio by Number of Whorls
F4A <- 
  ggplot (data6, aes(Brnumber, Ratio))+
  geom_point(size = 2, colour = "black") +
  geom_smooth(method="glm",se=TRUE)+
  theme(axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=15), 
        axis.title = element_text(size=15), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  ylab("Ratio All WRs") + 
  xlab("Brace Root WRs in Soil") +
  xlim(1,4) +
  ylim(0,1)


cor.test (data6$Brnumber, data6$Ratio, use = "everything", method = c("pearson"))

#4B. Individual Whorl Contributions


F4B <-
ggplot (data7, aes(Whorl, Ratio))+
  geom_violin(fill = "grey") +
  geom_point(size = 2, colour = "black") +
  theme(axis.text.x = element_text(size=15, angle=90), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=15), 
        axis.title = element_text(size=15), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  ylab("Ratio Indiv. WRs") + 
  xlab("Soil ------>")  +
  ylim(0,1)

shapiro.test(data7$Ratio)
res.aov <- aov(Ratio ~ Whorl, data = data7)
summary(res.aov)
tukey.test <- TukeyHSD(res.aov)
tukey.test


#4C. Total v. WR1 contribution

F4C <- 
  ggplot (data6, aes(Ratio, WR1))+
  geom_point(size = 2, colour = data6$Color) +
  geom_smooth(method="glm",se=TRUE)+
  theme(axis.text.x = element_text(size=15, angle=90), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15), 
        axis.text=element_text(size=15), 
        axis.title = element_text(size=15), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  ylab("Ratio All WRs") + 
  xlab("Ratio WR1") +
  xlim(0,1) +
  ylim(0,1)
  
cor.test (data6$Brnumber, data6$line_raw_slope.N.m., use = "everything", method = c("pearson"))

cor.test (data6$Ratio, data6$WR1, use = "everything", method = c("pearson"))
  cor.test (data6$Ratio, data6$WR2, use = "everything", method = c("pearson"))
  cor.test (data6$Ratio, data6$WR3, use = "everything", method = c("pearson"))
  
  
  #4D EI by Brace Root Whorls
 
  cor.test (data6$Brnumber, data6$EI_Complete, use = "everything", method = c("pearson"))
  
  F4D<-
    ggplot (data6, aes(Brnumber, line_raw_slope.N.m.))+
    geom_point(size = 2, colour = "black") +
      geom_smooth(method="glm",se=TRUE)+
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15), 
          axis.text=element_text(size=15), 
          axis.title = element_text(size=15), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_blank(),
          strip.text.y = element_blank()) +
    ylab("Force-Deflection Slope (N/m)") + 
    xlab("Brace Root WRs in Soil") +
    xlim(1,4)+
    ylim(0,450)
  
  
  
  
  ggdraw(xlim = c(0, 3.05), ylim = c(1.5, 3.05)) +
    draw_plot(F4A, x = 0.01, y = 1.55, width = .9, height = 1.45) +
    draw_plot(F4B, x = 1.02, y = 1.55, width = .9, height = 1.45) +
    draw_plot(F4C, x = 2.03, y = 1.55, width = .9, height = 1.45) +
    draw_plot_label(label = c("A", "B", "C"), 
                    size = 15,
                    x = c(0, 1, 2), 
                    y = c(3, 3, 3))
#Save as pdf: 8.5 x 4.5
    
  
  
#------------------------------
#Figure 5.Selection for Early Flowering (Silking) Does Not Select for the Number of Brace Roots in the Soil or the Contribution of Brace Roots to Flexural Rigidity.

  head(data8)  
  
#5A Flexural rigidity
  F5A <-
  ggplot (data8, aes(SelectionYear, RawSlope, group=SelectionYear))+
    geom_violin(fill = "grey")+
    geom_point(size = 2, colour = "black") +
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15, vjust=3), 
          axis.text=element_text(size=17), 
          axis.title = element_text(size=15), 
          axis.title.y= element_text(vjust=2.5), 
          axis.title.x= element_text(vjust=-1.4), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_text(size = 15, colour = "blue"),
          strip.text.y = element_text(size = 15, colour = "black"),
          legend.title=element_blank()) +
    xlab("Generation") + 
    ylab("Force-Deflection Slope (N/m)")+
    scale_x_continuous(breaks=seq(0,10,2))
  
  shapiro.test(data8$RawSlope)
  res.aov <- aov(RawSlope ~ as.factor(SelectionYear), data = data8)
  summary(res.aov)
  tukey.test <- TukeyHSD(res.aov)
  tukey.test
  
  
#5B number of brace roots in the soil  

F5B <- 
ggplot (data8, aes(SelectionYear, Brnumber, group=SelectionYear))+
  geom_violin(fill = "grey")+
  geom_jitter(size = 2, colour = "black", width=0.5, height=0) +
  theme(axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=15), 
        plot.title=element_text(size=15, vjust=3), 
        axis.text=element_text(size=17), 
        axis.title = element_text(size=15), 
        axis.title.y= element_text(vjust=2.5), 
        axis.title.x= element_text(vjust=-1.4), 
        axis.ticks.length = unit(.2,"cm"),
        strip.background = element_rect(fill="grey"),
        strip.text.x = element_text(size = 15, colour = "blue"),
        strip.text.y = element_text(size = 15, colour = "black"),
        legend.title=element_blank()) +
  xlab("Generation") + 
  ylab("Brace Root WRs in Soil")+
  ylim(0,4) +
  scale_x_continuous(breaks=seq(0,10,2))

  shapiro.test(data8$Brnumber)
  res.aov <- aov(Brnumber ~ as.factor(SelectionYear), data = data8)
  summary(res.aov)
  res.aov <- aov(Brnumber ~ as.factor(SelectionYear):Accession, data = data8)
  summary(res.aov)
  
  
  F5C <- 
    ggplot (data8, aes(SelectionYear, Ratio, group=SelectionYear))+
    geom_violin(fill = "grey")+
    geom_point(size = 2, colour = "black") +
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15, vjust=3), 
          axis.text=element_text(size=17), 
          axis.title = element_text(size=15), 
          axis.title.y= element_text(vjust=2.5), 
          axis.title.x= element_text(vjust=-1.4), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_text(size = 15, colour = "blue"),
          strip.text.y = element_text(size = 15, colour = "black"),
          legend.title=element_blank()) +
    xlab("Generation") + 
    ylab("Ratio All WRs")+
    ylim(0,1)+
    scale_x_continuous(breaks=seq(0,10,2))

  head(data10)  

 F5D<-
    ggplot (data10, aes(Average.of.tsw, Average.of.Slope))+
    geom_point(size = 2, colour = "black") +
    geom_smooth(method="glm",se=TRUE)+
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15), 
          axis.text=element_text(size=15), 
          axis.title = element_text(size=15), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_blank(),
          strip.text.y = element_blank()) +
    ylab("FD (N/m)") + 
    xlab("Tassel Weight") +
    ylim(0,250)
  
  F5E<-
    ggplot (data10, aes(Average.of.peh, Average.of.Slope))+
    geom_point(size = 2, colour = "black") +
    geom_smooth(method="glm",se=TRUE)+
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15), 
          axis.text=element_text(size=15), 
          axis.title = element_text(size=15), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_blank(),
          strip.text.y = element_blank()) +
    ylab("FD (N/m)") + 
    xlab("Ear-to-Tassel Length") +
    ylim(0,250)
  
  cor.test (data10$Average.of.Slope, data10$Average.of.dta, use= "everything", method = c("pearson"))
  cor.test (data10$Average.of.Slope, data10$Average.of.dts, use= "everything", method = c("pearson"))
  cor.test (data10$Average.of.Slope, data10$Average.of.asi, use= "everything", method = c("pearson"))
  cor.test (data10$Average.of.Slope, data10$Average.of.plh, use= "everything", method = c("pearson"))
  cor.test (data10$Average.of.Slope, data10$Average.of.erh, use= "everything", method = c("pearson"))
  cor.test (data10$Average.of.Slope, data10$Average.of.peh, use= "everything", method = c("pearson"))
  cor.test (data10$Average.of.Slope, data10$Average.of.tsw, use= "everything", method = c("pearson"))
  cor.test (data10$Average.of.Slope, data10$Average.of.spl, use= "everything", method = c("pearson"))
  
  
  
  
  shapiro.test(data8$Ratio)
  res.aov <- aov(Ratio ~ as.factor(SelectionYear), data = data8)
  summary(res.aov)
  res.aov <- aov(Ratio ~ as.factor(SelectionYear):Accession, data = data8)
  summary(res.aov)
  
  ggdraw(xlim = c(0, 1.55), ylim = c(0.45, 1.05)) +
    draw_plot(F5B, x = 0.01, y = 0.5, width = .45, height = .5) +
    draw_plot(F5C, x = 0.55, y = 0.5, width = .45, height = .5) +
    draw_plot(F5A, x = 1.05, y = 0.5, width = .45, height = .5) +
    draw_plot_label(label = c("A", "B", "C"), 
                    size = 15,
                    x = c(0, .55, 1.05), 
                    y = c(1.02, 1.02, 1.02))
  
  #Save as pdf:  8.5 x 4.5
  
#------------------------------
#Figure 5S
  
S5A <-
    ggplot (data9, aes(gen_slope, dts, group=gen_slope))+
    geom_violin(fill = "grey")+
    geom_point(size = 2, colour = "black") +
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15, vjust=3), 
          axis.text=element_text(size=17), 
          axis.title = element_text(size=15), 
          axis.title.y= element_text(vjust=2.5), 
          axis.title.x= element_text(vjust=-1.4), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_text(size = 15, colour = "blue"),
          strip.text.y = element_text(size = 15, colour = "black"),
          legend.title=element_blank()) +
    xlab("Generation") + 
    ylab("Days to silking") +
    scale_x_continuous(breaks=seq(0,10,2))
  
  S5B <-
    ggplot (data9, aes(gen_slope, dta, group=gen_slope))+
    geom_violin(fill = "grey")+
    geom_point(size = 2, colour = "black") +
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15, vjust=3), 
          axis.text=element_text(size=17), 
          axis.title = element_text(size=15), 
          axis.title.y= element_text(vjust=2.5), 
          axis.title.x= element_text(vjust=-1.4), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_text(size = 15, colour = "blue"),
          strip.text.y = element_text(size = 15, colour = "black"),
          legend.title=element_blank()) +
    xlab("Generation") + 
    ylab("Days to anthesis") +
    scale_x_continuous(breaks=seq(0,10,2))
  
  S5C <-
    ggplot (data9, aes(gen_slope, asi, group=gen_slope))+
    geom_violin(fill = "grey")+
    geom_point(size = 2, colour = "black") +
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15, vjust=3), 
          axis.text=element_text(size=17), 
          axis.title = element_text(size=15), 
          axis.title.y= element_text(vjust=2.5), 
          axis.title.x= element_text(vjust=-1.4), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_text(size = 15, colour = "blue"),
          strip.text.y = element_text(size = 15, colour = "black"),
          legend.title=element_blank()) +
    xlab("Generation") + 
    ylab("Anthesis-silking interval") +
    scale_x_continuous(breaks=seq(0,10,2))
  
  S5D <-
    ggplot (data9, aes(gen_slope, plh, group=gen_slope))+
    geom_violin(fill = "grey")+
    geom_point(size = 2, colour = "black") +
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15, vjust=3), 
          axis.text=element_text(size=17), 
          axis.title = element_text(size=15), 
          axis.title.y= element_text(vjust=2.5), 
          axis.title.x= element_text(vjust=-1.4), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_text(size = 15, colour = "blue"),
          strip.text.y = element_text(size = 15, colour = "black"),
          legend.title=element_blank()) +
    xlab("Generation") + 
    ylab("Plant height") +
    scale_x_continuous(breaks=seq(0,10,2))
  
  
  S5E <-
    ggplot (data9, aes(gen_slope, erh, group=gen_slope))+
    geom_violin(fill = "grey")+
    geom_point(size = 2, colour = "black") +
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15, vjust=3), 
          axis.text=element_text(size=17), 
          axis.title = element_text(size=15), 
          axis.title.y= element_text(vjust=2.5), 
          axis.title.x= element_text(vjust=-1.4), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_text(size = 15, colour = "blue"),
          strip.text.y = element_text(size = 15, colour = "black"),
          legend.title=element_blank()) +
    xlab("Generation") + 
    ylab("Ear height") +
    scale_x_continuous(breaks=seq(0,10,2))
  
  S5F <-
    ggplot (data9, aes(gen_slope, peh, group=gen_slope))+
    geom_violin(fill = "grey")+
    geom_point(size = 2, colour = "black") +
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15, vjust=3), 
          axis.text=element_text(size=17), 
          axis.title = element_text(size=15), 
          axis.title.y= element_text(vjust=2.5), 
          axis.title.x= element_text(vjust=-1.4), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_text(size = 15, colour = "blue"),
          strip.text.y = element_text(size = 15, colour = "black"),
          legend.title=element_blank()) +
    xlab("Generation") + 
    ylab("Ear-to-tassel length") +
    scale_x_continuous(breaks=seq(0,10,2))
  
  
  S5G <-
    ggplot (data9, aes(gen_slope, tsw, group=gen_slope))+
    geom_violin(fill = "grey")+
    geom_point(size = 2, colour = "black") +
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15, vjust=3), 
          axis.text=element_text(size=17), 
          axis.title = element_text(size=15), 
          axis.title.y= element_text(vjust=2.5), 
          axis.title.x= element_text(vjust=-1.4), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_text(size = 15, colour = "blue"),
          strip.text.y = element_text(size = 15, colour = "black"),
          legend.title=element_blank()) +
    xlab("Generation") + 
    ylab("Tassel weight") +
    scale_x_continuous(breaks=seq(0,10,2))
  
  
  S5H <-
    ggplot (data9, aes(gen_slope, spl, group=gen_slope))+
    geom_violin(fill = "grey")+
    geom_point(size = 2, colour = "black") +
    theme(axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15, vjust=3), 
          axis.text=element_text(size=17), 
          axis.title = element_text(size=15), 
          axis.title.y= element_text(vjust=2.5), 
          axis.title.x= element_text(vjust=-1.4), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_text(size = 15, colour = "blue"),
          strip.text.y = element_text(size = 15, colour = "black"),
          legend.title=element_blank()) +
    xlab("Generation") + 
    ylab("Tassel spike length") +
    scale_x_continuous(breaks=seq(0,10,2))
  
 
  
  ggdraw(xlim = c(0, 1.05), ylim = c(0.45, 2.55)) +
    draw_plot(S5H, x = 0.01, y = 0.5, width = .5, height = .5) +
    draw_plot(S5G, x = 0.55, y = 0.5, width = .5, height = .5) +
    draw_plot(S5E, x = 0.01, y = 1.0, width = .5, height = .5) +
    draw_plot(S5F, x = 0.55, y = 1.0, width = .5, height = .5) +
    draw_plot(S5D, x = 0.01, y = 1.5, width = .5, height = .5) +
    draw_plot(S5C, x = 0.55, y = 1.5, width = .5, height = .5) +
    draw_plot(S5B, x = 0.01, y = 2.0, width = .5, height = .5) +
    draw_plot(S5A, x = 0.55, y = 2.0, width = .5, height = .5) +
    draw_plot_label(label = c("A", "B", "C", "D","E","F","G","H"), 
                    size = 15,
                    x = c(0, .55, 0, .55,0, .55,0, .55), 
                    y = c(2.55,2.55, 2.05, 2.05, 1.55, 1.55, 1.05, 1.05))


  #Save as pdf:  8.5 x 10


  
  
  #------------------------------
  #Figure 5S
  
  data11$Plot <- as.factor(data11$Plot)
  data11$Plant <- as.factor(data11$Plant)
  data11$Slope.with <- as.factor(data11$Slope.with)
  data11$Slope.without <- as.factor(data11$Slope.without)
  data11$FSwith <- as.factor(data11$FSwith)
  data11$Fswithout <- as.factor(data11$Fswithout)
  data11$Height.attachment <- as.factor(data11$Height.attachment)
  data11$Height.attachment.m. <- as.factor(data11$Height.attachment.m.)
  
  
  
  
  data11a <-melt(data11)
  head(data11a)
  
  ggplot (data11a, aes(variable, value))+
    geom_violin()+
    geom_point(size = 2, colour = "black") +
    theme(axis.text.x = element_text(size=15, angle = 90, vjust=0.5), 
          axis.text.y = element_text(size=15), 
          plot.title=element_text(size=15, vjust=3), 
          axis.text=element_text(size=17), 
          axis.title = element_text(size=15), 
          axis.title.y= element_text(vjust=2.5), 
          axis.title.x= element_text(vjust=-1.4), 
          axis.ticks.length = unit(.2,"cm"),
          strip.background = element_rect(fill="grey"),
          strip.text.x = element_text(size = 15, colour = "blue"),
          strip.text.y = element_text(size = 15, colour = "black"),
          legend.title=element_blank()) +
    xlab("") + 
    ylab("Ratio") +
    ylim(0,1)
  
  #Save as pdf:  4 x 6