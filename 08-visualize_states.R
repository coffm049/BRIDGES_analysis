library(tidyverse)
library(reshape2)

#library(ggcorrplot)
dir = "/home/christian/Research/Brain_stuff/BRIDGES_GIG_ICA/GIG-ICA-5-updated-GIFT/thresh50/"
df <- data.frame()
for (i in 1:7) {
  temp = read.csv(paste0(dir, "S", i, ".csv"),
                           header = FALSE)
  temp$State =i
  df <- rbind(df, temp)
}
colnames(df) <- c(1:41, "State")

# Create ROI names list
nm = c(rep("Control C", 3),
       "Default A",
       "Dor Attn B",
       rep("Limbic A", 9),
       rep("Limbic B", 2),
       rep("Right Cerebellum", 2),
       rep("Sal/VenAttn A", 4),
       rep("SomMotor A", 5),
       rep("SomMotor B", 4),
       rep("Temp Par", 2),
       rep("Visual A", 6),
       rep("Visual B", 2))
# alter thelist so it lines up nicely on the axes
nm2 <- rep("", 41)
nm2[c(2,4,5,10,15,18,21,25,29,33,36,41)] <- nm[c(2,4,5,10,16,18,21,25,29,33,36,41)]

coords = c(0,cumsum(rle(nm)$lengths))

names <- rle(nm)
nm2 <- append(nm2, c(""))
nm2 <- nm2[-c(1)]
# corABt<- as.data.frame(corABt)
df1<- df %>% dplyr::select(-State)

dfs <- list(rep(matrix(nrow=41, ncol = 41), 3))
# 1
dft <- df1[1:41, 1:41]
dfb <- df1[42:82, 1:41]
dfs[[1]] <- matrix(nrow = 41, ncol = 41)
dfs[[1]][upper.tri(dft)] <- dft[upper.tri(dft)]
dfs[[1]][lower.tri(dfb)] <- dfb[lower.tri(dfb)]
dfs[[1]] <- cbind(1:41, dfs[[1]])
dfs[[1]] <- cbind(1, dfs[[1]])
# 2
dft <- df1[83:123, 1:41]
dfb <- df1[124:164, 1:41]
dfs[[2]] <- matrix(nrow = 41, ncol = 41)
dfs[[2]][upper.tri(dft)] <- dft[upper.tri(dft)]
dfs[[2]][lower.tri(dfb)] <- dfb[lower.tri(dfb)]
dfs[[2]] <- cbind(1:41, dfs[[2]])
dfs[[2]] <- cbind(2, dfs[[2]])
# 3
dft <- df1[165:205, 1:41]
dfb <- df1[206:246, 1:41]
dfs[[3]] <- matrix(nrow = 41, ncol = 41)
dfs[[3]][upper.tri(dft)] <- dft[upper.tri(dft)]
dfs[[3]][lower.tri(dfb)] <- dfb[lower.tri(dfb)]
dfs[[3]] <- cbind(1:41, dfs[[3]])
dfs[[3]] <- cbind(3, dfs[[3]])
# 4
dft <- df1[247:287, 1:41]
dfb <- 0
dfs[[4]] <- matrix(nrow = 41, ncol = 41)
dfs[[4]][upper.tri(dft)] <- dft[upper.tri(dft)]
dfs[[4]][lower.tri(dfb)] <- dfb[lower.tri(dfb)]
dfs[[4]] <- cbind(1:41, dfs[[4]])
dfs[[4]] <- cbind(4, dfs[[4]])

dftot <- matrix(nrow = 41*4, ncol= 43)
dftot[1:41, 1:43] <- dfs[[1]]
dftot[42:82, 1:43] <- dfs[[2]]
dftot[83:123, 1:43] <- dfs[[3]]
dftot[124:164, 1:43] <- dfs[[4]]
dftot[is.na(dftot)] <- 0

colnames(dftot) <- c("State", "IC1", 1:41)

places <- data.frame(mins = coords[-13], maxs = coords[-1])
places <- rbind(places, c(41,42))

State_labs <- c("States 1 (Top) and 2 (Bottom)",
                "States 3 (Top) and 4 (Bottom)",
                "States 5 (Top) and 6 (Bottom)",
                "State 7 (Top)")
names(State_labs) <- c("1", "2", "3", "4")

melt(as.data.frame(dftot), id.vars = c("State", "IC1"), value.name="Correlation", variable.name = "IC2") %>% 
  mutate(IC1 = factor(IC1)) %>% 
  mutate(Correlation = ifelse(Correlation >1, 1, Correlation)) %>%
  ggplot(aes(x= IC1, y= IC2, fill = Correlation)) +
  geom_tile() +
  facet_wrap(~State, labeller = labeller(State = State_labs))+
  geom_hline(yintercept = coords[-13][-1]- 0.5, linetype = "dotted") + 
  geom_vline(xintercept = coords[-13][-1] - 0.5, linetype = "dotted") +
  scale_x_discrete(name = "", labels = nm2)  +
  scale_y_discrete(name = "", labels = nm2)  + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, size = 8, color = "black", face="bold"),
        axis.text.y=element_text(vjust = 0, angle =0, size = 8,  color = "black", face="bold"),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"
        )) + 
  scale_fill_gradient2(low = "blue", mid = "white", midpoint = 0, high = "red", limits= c(-1,1),
                       guide = guide_colorbar(direction="horizontal", title.position = "top")) +
  theme(legend.position = c(0.9, 0.1)) +
  geom_rect(xmin = places[1,1]-0.5, xmax=places[1,2]-0.5, ymin=places[1,1]-0.5, ymax=places[1,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[2,1]-0.5, xmax=places[2,2]-0.5, ymin=places[2,1]-0.5, ymax=places[2,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[3,1]-0.5, xmax=places[3,2]-0.5, ymin=places[3,1]-0.5, ymax=places[3,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[4,1]-0.5, xmax=places[4,2]-0.5, ymin=places[4,1]-0.5, ymax=places[4,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[5,1]-0.5, xmax=places[5,2]-0.5, ymin=places[5,1]-0.5, ymax=places[5,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[6,1]-0.5, xmax=places[6,2]-0.5, ymin=places[6,1]-0.5, ymax=places[6,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[7,1]-0.5, xmax=places[7,2]-0.5, ymin=places[7,1]-0.5, ymax=places[7,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[8,1]-0.5, xmax=places[8,2]-0.5, ymin=places[8,1]-0.5, ymax=places[8,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[9,1]-0.5, xmax=places[9,2]-0.5, ymin=places[9,1]-0.5, ymax=places[9,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[10,1]-0.5, xmax=places[10,2]-0.5, ymin=places[10,1]-0.5, ymax=places[10,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[11,1]-0.5, xmax=places[11,2]-0.5, ymin=places[11,1]-0.5, ymax=places[11,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[12,1]-0.5, xmax=places[12,2]-0.5, ymin=places[12,1]-0.5, ymax=places[12,2]-0.5, col = "black",fill= "red", alpha =0) +
  geom_rect(xmin = places[13,1]-0.5, xmax=places[13,2]-0.5, ymin=places[13,1]-0.5, ymax=places[13,2]-0.5, col = "black",fill= "red", alpha =0)
  


print("stop")


strip.text.x = element_text(
  size = 12, color = "red", face = "bold.italic"
)





as.data.frame(dftot) %>%
  ggplot(aes(x= `1`, y = `2`)) +
  geom_point() +
  facet_wrap(~State)














# Meng's example
states <- 1:7
title = sprintf("States %g (Upper) and %g (Lower)",states[1],states[2])
corABt = corAB
nm=colnames(corABt)
stnm = which(!duplicated(nm))
gp = c(diff(which(!duplicated(nm))),length(nm)-stnm[length(stnm)]+1)
colnames(corABt)=rownames(corABt)=NULL
mAB = melt(corABt)
gfig=  ggplot(mAB,aes(x=Var1,y=Var2,fill=value)) + 
  geom_tile() +scale_fill_gradient2(low="blue",mid='white',high="red",limits=c(-1,1)) + 
  coord_equal() +
  scale_x_continuous("",breaks=stnm+(gp-1)/2,
                     labels=nm[which(!duplicated(nm))])+
  scale_y_continuous("",breaks=stnm+(gp-1)/2,
                     labels=nm[which(!duplicated(nm))])+
  theme(axis.text.x = element_text(angle =90),
        panel.background = element_rect(fill='white'),
        axis.ticks = element_blank()) +
  labs(title = title)
for (i in 1:length(stnm)){
  xmin = stnm[i]
  xmax = stnm[i]+gp[i]-1
  gfig=gfig+
    annotation_custom(grob = linesGrob(),  xmin = xmin, xmax = xmax, ymin = -.5, ymax =-.5)+
    annotation_custom(grob = linesGrob(),  xmin = xmin, xmax = xmin, ymin = -.5, ymax =0)+
    annotation_custom(grob = linesGrob(),  xmin = xmax, xmax = xmax, ymin = -.5, ymax =0)+
    annotation_custom(grob = linesGrob(),  xmin = -.5, xmax = -.5, ymin = xmin, ymax =xmax)+
    annotation_custom(grob = linesGrob(),  xmin = -.5, xmax = 0, ymin = xmin, ymax =xmin)+
    annotation_custom(grob = linesGrob(),  xmin = -.5, xmax = 0, ymin = xmax, ymax =xmax)
};
coords = c(0,cumsum(rle(as.numeric(labels))$lengths))
for(i in 1:(length(coords)-1)){
  gfig = gfig + 
    geom_rect(xmin = coords[i]+.5, xmax = coords[i+1]+.5, ymin = coords[i]+.5, ymax = coords[i+1]+.5,
              size = 0.75,
              color='black',
              fill=NA)
}
return(gfig)
