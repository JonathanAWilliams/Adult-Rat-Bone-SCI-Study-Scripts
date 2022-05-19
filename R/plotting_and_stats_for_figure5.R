library(tidyverse)
library(ggpubr)
library(car)
library(gmodels)
library(grid)
library(gridExtra)

DATA <- read.table("EPI TRAB Morph data for bar chart plot.txt", header = TRUE)



p1 <- ggbarplot(DATA, x = "Time", y = "BVTV", ylim=c(0, 55),fill ="Surgery",
             add = "mean_sd", add.params = list(size = 1.5, color = "gray26"), position=position_dodge(),
             ylab = "BV/TV, %") +
  labs(tag = "A)") +
  theme(axis.text=element_text(size=14,face="bold"),
                axis.title.y = element_text(size=14, face="bold"),
                axis.title.x = element_blank(),
                axis.line = element_line(colour = 'black', size = 1.25),
                axis.ticks = element_line(colour = "black", size = 1.25),
                legend.position = c(0.5, 0.99),
                legend.direction = "horizontal",
                plot.tag.position = c(0.02, 0.98)
  )+
  geom_jitter(data=DATA, position=position_dodge(width = 1), cex=2, aes(shape = Surgery, fill = Surgery))+
  annotate(geom="text", x=4, y=44, label="**", size = 10)+
  geom_segment(aes(x = 3.75, y = 42.6, xend = 4.25, yend = 42.6),size = 1.3) +
  scale_y_continuous(breaks = seq(0, 50, by = 10))

p2 <- ggbarplot(DATA, x = "Time", y = "vBMD", ylim=c(0, 0.75),fill ="Surgery",
                add = "mean_sd", add.params = list(size = 1.5,color = "gray26"), position=position_dodge(),
                ylab =  expression(bold(paste("vBMD, g cm"^"-3")))) +
  labs(tag = "B)") +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = 'black', size = 1.25),
        axis.ticks = element_line(colour = "black", size = 1.25),
        legend.position = c(0.5, 0.99),
        legend.direction = "horizontal",
        plot.tag.position = c(0.02, 0.98)
  )+
  geom_jitter(data=DATA, position=position_dodge( width = 1), cex=2, aes(shape = Surgery, fill = Surgery))+
  annotate(geom="text", x=4, y=0.61, label="**", size = 10)+
  geom_segment(aes(x = 3.75, y = 0.6, xend = 4.25, yend = 0.6),size = 1.3)

p3 <- ggbarplot(DATA, x = "Time", y = "TbTh", ylim=c(0.0, 0.12),fill ="Surgery",
                add = "mean_sd", add.params = list(size = 1.5, color = "gray26"),  position=position_dodge(),
                ylab = "Tb.Th, mm") +
  labs(tag = "C)") +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = 'black', size = 1.25),
        axis.ticks = element_line(colour = "black", size = 1.25),
        legend.position = "none",
        plot.tag.position = c(0.02, 0.98)
  )+
  geom_jitter(data=DATA, position=position_dodge( width = 1), cex=2, aes(shape = Surgery, fill = Surgery))

p4 <- ggbarplot(DATA, x = "Time", y = "TbN", ylim=c(0,4),fill ="Surgery",
                add = "mean_sd", add.params = list(size = 1.5, color = "gray26"), position=position_dodge(),
                ylab =  expression(bold(paste("Tb.N, mm"^"-1")))) +
  labs(tag = "D)") +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = 'black', size = 1.25),
        axis.ticks = element_line(colour = "black", size = 1.25),
        legend.position = "none",
        plot.tag.position = c(0.02, 0.98)
  )+
  geom_jitter(data=DATA, position=position_dodge( width = 1), cex=2, aes(shape = Surgery, fill = Surgery))+
  annotate(geom="text", x=4, y=3.85, label="**", size = 10)+
  geom_segment(aes(x = 3.75, y = 3.8, xend = 4.25, yend = 3.8),size = 1.3)

p5 <- ggbarplot(DATA, x = "Time", y = "TbSp", ylim=c(0,0.35),fill ="Surgery",
                add = "mean_sd", add.params = list(size = 1.5, color = "gray26"), position=position_dodge(),
                ylab = "Tb.Sp, mm") +
  labs(tag = "E)") +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = 'black', size = 1.25),
        axis.ticks = element_line(colour = "black", size = 1.25),
        legend.position = "none",
        plot.tag.position = c(0.02, 0.98)
  )+
  geom_jitter(data=DATA, position=position_dodge( width = 1), cex=2, aes(shape = Surgery, fill = Surgery))+
  annotate(geom="text", x=4, y=0.34, label="*", size = 10)+
  geom_segment(aes(x = 3.75, y = 0.335, xend = 4.25, yend = 0.335),size = 1.3)

p6 <- ggbarplot(DATA, x = "Time", y = "ConnD", ylim=c(0,175),fill ="Surgery",
                add = "mean_sd", add.params = list(size = 1.5, color = "gray26"), position=position_dodge(),
                ylab =  expression(bold(paste("Conn.D, mm"^"-3"))))+
  labs(tag = "F)") +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = 'black', size = 1.25),
        axis.ticks = element_line(colour = "black", size = 1.25),
        legend.position = "none",
        plot.tag.position = c(0.02, 0.98)
  )+
  geom_jitter(data=DATA, position=position_dodge( width = 1), cex=2, aes(shape = Surgery, fill = Surgery))

p7 <- ggbarplot(DATA, x = "Time", y = "BSBV", ylim=c(0,50),fill ="Surgery",
                add = "mean_sd", add.params = list(size = 1.5, color = "gray26"),  position=position_dodge(),
                ylab =  expression(bold(paste("BS/BV, mm"^"-1"))))+
  labs(tag = "G)") +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = 'black', size = 1.25),
        axis.ticks = element_line(colour = "black", size = 1.25),
        legend.position = "none",
        plot.tag.position = c(0.02, 0.98)
  )+
  geom_jitter(data=DATA, position=position_dodge( width = 1), cex=2, aes(shape = Surgery, fill = Surgery))+
  annotate(geom="text", x=4, y=45, label="*", size = 10)+
  geom_segment(aes(x = 3.75, y = 44, xend = 4.25, yend = 44),size = 1.3)

p8 <- ggbarplot(DATA, x = "Time", y = "TbPf", ylim=c(0,8),fill ="Surgery",
                add = "mean_sd", add.params = list(size = 1.5, color = "gray26"),  position=position_dodge(),
                ylab =  expression(bold(paste("Tb.Pf, mm"^"-1"))))+
  labs(tag = "H)") +
  theme(axis.text=element_text(size=14,face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.title.x = element_blank(),
        axis.line = element_line(colour = 'black', size = 1.25),
        axis.ticks = element_line(colour = "black", size = 1.25),
        legend.position = "none",
        plot.tag.position = c(0.02, 0.98)
  )+
  geom_jitter(data=DATA, position=position_dodge( width = 1), cex=2, aes(shape = Surgery, fill = Surgery))+
  annotate(geom="text", x=4, y=7.7, label="**", size = 10)+
  geom_segment(aes(x = 3.75, y = 7.5, xend = 4.25, yend = 7.5),size = 1.3)


grid.arrange(p1, p2, p3, p4, p5, p6,p7, p8,  nrow = 4)


