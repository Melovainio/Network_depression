######################################################################################################
################################## DATA FROM  Finnish Health 2000 - 2011 Mean FINAL ##################
######################################################################################################
library(ggthemes)
library(dplyr)
library(qgraph)
library(bootnet)
library(stringr)
library(EstimateGroupNetwork)
library(ggplot2)
library(gridExtra)
####################################################################################################
############################### Network Analyses ###################################################
###############stringr##############################################################################
setwd("/Users/melovain/Documents/Kone/terveys2000/Uudet2017")
library(foreign)
# ---------------------------------------------------------------------------------------
### reading data
ma <- read.dta("/Users/melovain/Documents/Kone/terveys2000/Uudet2017/t20h.dta") #Year 2000 -11 mean / MDD-
ma0 <- ma[,2:14] # Select only depression variables
maa0 <- na.omit(ma0) # Listvise deletion
data1<-as.data.frame(maa0)
mb <- read.dta("/Users/melovain/Documents/Kone/terveys2000/Uudet2017/t20m.dta") #Year 2000 -11 mean / MDD+
mb0 <- mb[,2:14] # Select only depression variables
mbb0 <- na.omit(mb0) # Listvise deletion
data3<-as.data.frame(mbb0)
nrow(data1)          # 5998
nrow(data3)          #  595

#changing names  
library(gdata)#Renaming variables 
data1 <- rename.vars(data1, from = c("b1_m","b2_m","b3_m","b4_m",
                                       "b5_m","b6_m","b7_m","b8_m","b9_m","b10_m","b11_m","b12_m","b13_m"), to = c("b1","b2","b3","b4",
                                                                                                                   "b5","b6","b7","b8","b9","b10","b11","b12","b13"))
data3 <- rename.vars(data3, from =  c("b1_m","b2_m","b3_m","b4_m",
                                        "b5_m","b6_m","b7_m","b8_m","b9_m","b10_m","b11_m","b12_m","b13_m"), to = c("b1","b2","b3","b4",
                                                                                                                    "b5","b6","b7","b8","b9","b10","b11","b12","b13"))
### FIGURE 1
# 3 cliques
net <- matrix(0,9,9)
net[1:3,1:3] <- .99999
net[4:6,4:6] <- .99999
net[7:9,7:9] <- .99999
diag(net) <- 0
sum(net)/2 # connectivity ~ 9

# 1 clique
net1 <- matrix(0,9,9)
net1[1:9,1:9] <- .25
diag(net1) <- 0
sum(net1)/2 # connectivity ~ 9

# plot
library(qgraph)

layout(t(1:2))
nw <- qgraph(net, details=F, title="Network with 3 fully connected communities", theme ="colorblind")
nw1 <- qgraph(net1, layout=nw$layout, details=F, maximum=1, title="Network with 1 weakly connected community", theme ="colorblind")

### SUPPLEMENT FIGURE 1 
##### MEANDS / SDs VERSION TWOE##################
library(reshape2)
means1 <- colMeans(data1, na.rm=TRUE)
sds1 <- as.vector(sapply(data1, sd, na.rm=TRUE))
mean(sds1)
means3 <- colMeans(data3, na.rm=TRUE)
sds3 <- as.vector(sapply(data3, sd, na.rm=TRUE))
mean(sds3)
means <- as.data.frame(cbind(means1, means3))
means <- mutate(means, id = rownames(means))
colnames(means)<-c("1", "2", "Symptoms")
means_n <- melt(means, id="Symptoms")
names(means_n)[2] <- "Datasets"
type <- c("Mean")
means_n <- cbind(means_n, type)
sds <- as.data.frame(cbind(sds1,sds3))
sds <- mutate(sds, id = rownames(sds))
colnames(sds)<-c("1", "2","Symptoms")
sds_n <- melt(sds, id="Symptoms")
sds_n$Symptoms <- as.numeric(sds_n$Symptoms)
names(sds_n)[2] <- "Datasets"
type <- c("SD")
sds_n <- cbind(sds_n, type)
mean(sds_n$value)
sds_n$value <- -sds_n$value
Names2 <- c("Sadness " ,"Worthlessness","Loss of energy", "Tiredness or fatigue", "Change in appetite ",  "Pessimism" ,"Past failure",
            "Loss of pleasure " ,"Guilty feelings", "Self dislike", "Suicidal thoughts", "Loss of interest", "Indecisiveness")
p1<-ggplot() +
  geom_line(aes(x=Symptoms, y=value, colour=Datasets, group=Datasets), lty=1, data=means_n) +
  geom_point(aes(x=Symptoms, y=value, colour=Datasets, group=Datasets), data=means_n, shape = 21, fill = "white", size = 1.5, stroke = 1) +
  geom_line(aes(x=Symptoms, y=value, colour=Datasets, group=Datasets), lty=2, data=sds_n) +
  geom_point(  aes(x=Symptoms, y=value, colour=Datasets, group=Datasets), data=sds_n, shape = 21, fill = "white", size = 1.5, stroke = 1) +
  xlab(" depressive symptoms ") + ylab("Means /SDs") +
  scale_y_continuous(limits = c(-1, 1)) + 
  scale_x_discrete(label = Names2) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)); p1
p2<-p1+scale_color_manual(values=c("#4682B4", "#B4464B"),
                          name="Datasets (Means solid/ SDs dotted)",
                          breaks=c("1", "2"),
                          labels=c("Without MDD","With MDD"));p2 
p3<-p2+ ggtitle("Supplement figure 1");p3

#--------------Same in bar plots-----------------# -------- Final supplement Figure 1

Names3 <- c("Sadness " ,  "Pessimism" ,"Past failure", "Loss of pleasure " ,"Guilty feelings", "Self dislike", "Suicidal thoughts", "Loss of interest", 
            "Indecisiveness", "Worthlessness","Loss of energy", "Tiredness or fatigue", "Change in appetite ")

data1["dep"]<-0
data3["dep"]<-1

df_data<-rbind(data1, data3)

df_data$dep<- factor(df_data$dep, labels=c("No", "Yes"))
g<-df_data
library(reshape2)            # for melt(...)
ggg <- melt(g,id="dep")
ggg<-na.omit(ggg)
library(ggthemes)
library(ggpubr)
pp1<-ggplot(ggg, aes(x=variable, y=value, fill=factor(dep))) + 
  stat_summary(fun.y=mean, geom="bar",position=position_dodge(1),  colour="black", # Use black outlines,
               size=.3) +      # Thinner lines) + 
  stat_compare_means(aes(group = dep), method="anova", label = "p.signif", label.y = 1, color="black")+
  stat_summary(fun.data=mean_cl_normal,geom="errorbar",
               color="grey40",position=position_dodge(1), width=.2) +
  scale_fill_discrete(name="Group", breaks =c("No", "Yes"), labels=c("DD-","DD+"))+
  ylim(0,1)+
  xlab("") +
  ylab("Means, scale from 0 to 1") +
 scale_x_discrete(label = Names3) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)); pp1
pp2<-pp1+ scale_fill_manual( name="Group", values = c('red', 'blue'), breaks =c("No", "Yes"), labels=c("DD-","DD+"));pp2
pp3<-pp2+ scale_fill_brewer(name="Group", breaks =c("No", "Yes"), labels=c("DD-","DD+"), palette="Set1"); pp3  


data1<- data1 %>% select(-dep)
data3<- data3 %>% select(-dep)

### Supplement figure 2
#Correlations
library(GGally)
c1pp<-ggcorr(data1, geom = "text", nbreaks = 3, palette = "RdYlBu", hjust = 1, label = TRUE, label_alpha = 0.5)+guides(colour = FALSE, alpha = FALSE)+ ggtitle("Correlations between items, between individuals DD-")
c3pp<-ggcorr(data3, geom = "text", nbreaks = 3, palette = "RdYlBu", hjust = 1, label = TRUE, label_alpha = 0.5)+guides(colour = FALSE, alpha = FALSE)+ ggtitle("Correlations between items, between individuals DD+")
gridExtra::grid.arrange(c1pp, c3pp, ncol = 2)

# Associations between Spearman and polychoric .99 and .98
cor(as.vector(cor(data1, method="spearman")[upper.tri(cor(data1))]),as.vector(cor_auto(data1)[upper.tri(cor_auto(data1))]))
cor(as.vector(cor(data3, method="spearman")[upper.tri(cor(data3))]),as.vector(cor_auto(data3)[upper.tri(cor_auto(data3))]))

# Correlations between means and SDs
cor(means1,sds1, method="spearman")  
cor(means3,sds3, method="spearman") 

## significant mean differnces 
data1sum <- data1; data1sum$s <- rowMeans(data1) 
data3sum <- data3; data3sum$s <- rowMeans(data3) 
t.test(data1sum$s, data3sum$s) 
tt<-t.test(data1sum$s, data3sum$s) 
psycho::analyze(tt)
t.test(data1sum$s, data3sum$s) %>% 
  psycho::analyze() %>% 
  summary()
t.test(data1$b1, data3$b1)
tt<-t.test(data1$b1, data3$b1) 
psycho::analyze(tt)
t.test(data1$b1, data3$b1) %>% 
  psycho::analyze() %>% 
  summary()
t.test(data1$b5, data3$b5)
ttt<-t.test(data1$b5, data3$b5) 
psycho::analyze(ttt)
t.test(data1$b1, data3$b1) %>% 
  psycho::analyze() %>% 
  summary()
tttt<-t.test(data1$b13, data3$b13) 
psycho::analyze(tttt)
t.test(data1$b13, data3$b13) %>% 
  psycho::analyze() %>% 
  summary()
ttt6<-t.test(data1$b6, data3$b6) 
psycho::analyze(ttt6)
t.test(data1$b6, data3$b6) %>% 
  psycho::analyze() %>% 
  summary()
ttt11<-t.test(data1$b11, data3$b11) 
psycho::analyze(ttt11)
t.test(data1$b11, data3$b11) %>% 
  psycho::analyze() %>% 
  summary()

# Internal consistency; 
a1 <- psych::alpha(cor_auto(data1)); a1$total$std.alpha #.84
a3 <- psych::alpha(cor_auto(data3)); a3$total$std.alpha #.89

# Correlation between symptom means 
cor(means1,means3, method="spearman") #0.80

#############################################
### Network analyses ########################

Groups <- c(rep("Affective",2),rep("Cognitive",1), rep("Affective",1), rep("Cognitive",3),rep("Affective",1), rep("Somatic",1),rep("Cognitive",1),rep("Somatic",3))
Names <- scan("/Users/melovain/Documents/Kone/terveys2000/Uudet2017/Depression_ch/Becktext.txt",what = "character", sep = "\n")

### Correlations for network analyses
data1cor <- cor_auto(data1)
data3cor <- cor_auto(data3)

library(EstimateGroupNetwork) 
netg1 <- EstimateGroupNetwork(list(data1,data3), inputType = "list.of.dataframes", method="crossvalidation", criterion = "ebic", gamma=0.5,
                              simplifyOutput = FALSE, seed=100)
netg1$network

nn1 <- getWmat(qgraph(netg1$network[[1]], sampleSize = nrow(data1), DoNotPlot = TRUE))
nn3 <- getWmat(qgraph(netg1$network[[2]], sampleSize = nrow(data3), DoNotPlot = TRUE))
Max <- max(c(nn1, nn3))
Max
L <- averageLayout(nn1,nn3)
#preidictability networks
library(mgm)
# networks - mgm 
set.seed(1)
fit1 <- mgm(data = data1, type = rep('g',13), level = rep(1, 13),lambdaSel = 'CV',ruleReg = 'OR')
fit3 <- mgm(data = data3, type = rep('g',13), level = rep(1, 13),lambdaSel = 'CV',ruleReg = 'OR')
pred1 <- predict(object = fit1,data = data1, errorCon = 'R2')
pred3 <- predict(object = fit3,data = data3, errorCon = 'R2')
list(pred1$error$R2)
list(pred3$error$R2)
# Average node predictability 
mean(pred1$error$R2) 
mean(pred3$error$R2) 

# plotting networks version 2 
par(mfrow=c(1,2))
gr1 <- qgraph(netg1$network[[1]], layout = L, title = "DD- group", maximum=Max, theme="colorblind", 
              pieColor = "#5885A2", pie=pred1$error$R2,
              border.width=2, vsize=10, label.cex=2, tuning=0.25) 
gr3 <- qgraph(netg1$network[[2]], layout = L, title = "DD+ group", maximum=Max, theme="colorblind", 
              pieColor = "#5885A2", pie=pred3$error$R2,
              border.width=2, vsize=10, label.cex=2, tuning=0.25)

# Correlations joint networks with each other
cor(getWmat(gr1)[lower.tri(getWmat(gr1))], getWmat(gr3)[lower.tri(getWmat(gr3))],	method="spearman") # 0.65

mean(getWmat(gr1))
mean(getWmat(gr3))

# network comparison 
library(NetworkComparisonTest)
library(bootnet)
#same using NCT_bootnet, used in them manuscript
q1<-estimateNetwork(data1,"EBICglasso", corMethod="cor_auto")
q3<-estimateNetwork(data3,"EBICglasso", corMethod="cor_auto")  
plot(q1)
plot(q3)
nct_13b$glstrinv.real
#nct_13b <- NCT_bootnet(q1, q3, it = 1000,  progressbar = TRUE, test.edges=TRUE, edges='all')
nct_13b <- NCT(q1, q3, it = 1000,  progressbar = TRUE, test.edges=TRUE, edges='all')
res_nctb <- matrix(c(1, 3, 0, 0, 0, 0, 0, 0, 
                     2, 4, 0, 0, 0, 0, 0, 0), ncol = 8, byrow = TRUE)
colnames(res_nctb) <- c("Dataset A", "Dataset B", "Difference in global strangth", "p-value of global strength", "Max.diff invariance", 
                        "p-value of invar.", "Global strength  Measure1","Global strength  Measure1" )
res_nctb[1,3:8] <- c(nct_13b$glstrinv.real, nct_13b$glstrinv.pval, nct_13b$nwinv.real, nct_13b$nwinv.pval, nct_13b$glstrinv.sep)
res_nctb
plot(nct_13b, what="network")
 # Plot results of global strength invariance test (not reliable with only 10 permutations!):
plot(nct_13b, what="strength")


######### Figure 3 community struture 
par(mfrow=c(1,2))
library(igraph)
graph.g1<-as.igraph(gr1)
wc1<- walktrap.community(graph.g1)
n.dim1 <- max(wc1$membership)
plot.ega1 <- qgraph(gr1, layout = "spring", vsize = 10, groups = as.factor(wc1$membership), overlay=TRUE,
                    label.cex=2, labels = colnames(data1), maximum=Max)

graph.g3<-as.igraph(gr3)
wc3<- walktrap.community(graph.g3)
n.dim3 <- max(wc3$membership)
plot.ega3 <- qgraph(gr3, layout = "spring", vsize = 10, groups = as.factor(wc3$membership), overlay=TRUE,
                    label.cex=2, labels = colnames(data3), maximum=Max)

library(NetworkToolbox)
#check walktrap robustness

rand<-.Random.seed
walkch<-matrix(0,nrow=13,ncol=10)
for(i in 1:10)
{
  set.seed(rand[i])
  walkch[,i]<-walktrap.community(graph.g1)$membership
}
walkch

for(i in 1:10)
{
  set.seed(rand[i])
  walkch[,i]<-walktrap.community(graph.g3)$membership
}
walkch

####Figure 4
#Minimum spanning trees
library(igraph)
library(NetworkToolbox)
md1<-MaST(data1, normal = TRUE, na.data = "none", depend = FALSE)
a<-qgraph(md1, layout="spring", DoNotPlot = TRUE)
gmst1<-as.igraph(a)
V(gmst1)$name<-Names
scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
vSizes <- (scale01(apply(data1, 1, mean)) + 1.0) * 10
edgeweights <- gmst1$weight * 2.0
#Plot the tree object
plot(
  gmst1,
  layout=layout.fruchterman.reingold,
  edge.curved=FALSE,
  vertex.size=vSizes,
  vertex.label.dist=-0.2,
  vertex.label.color="black",
  vertex.color="white",
  asp=FALSE,
  # vertex.label=V(gmst1)$name, 
  vertex.label.cex=1.6,
  edge.width=edgeweights,
  edge.arrow.mode=0,
  main="Healthy between"
)
md3<-MaST(data3, normal = TRUE, na.data = "none", depend = FALSE)
a3<-qgraph(md3, labels = colnames(data3), DoNotPlot = TRUE)
gmst3<-as.igraph(a3)
scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
vSizes3 <- (scale01(apply(data3, 1, mean)) + 1.0) * 10
edgeweights3 <- gmst3$weight * 2.0
#Plot the tree object
plot(
  gmst3,
  layout=layout.fruchterman.reingold,
  edge.curved=FALSE,
  vertex.size=vSizes3,
  vertex.label.dist=-0.2,
  vertex.label.color="black",
  asp=FALSE,
  edge.label.family="Times",
  edge.label.font=1,
  vertex.label.cex=1.6,
  #vertex.label=V(gmst1)$name,
  edge.width=edgeweights3,
  edge.arrow.mode=0,
  main="MDD between "
)
#version two
qgraph(md1, layout="spring", labels = TRUE, edge.labels = TRUE, label.cex=2, edge.label.cex=1.5, vsize = 7, esize = 10, label.color = "black", theme ="colorblind" , borders = TRUE, title ="DD- group")
qgraph(md3, layout="spring", labels = TRUE, edge.labels = TRUE, label.cex=2, edge.label.cex=1.5, vsize = 7, esize = 10, label.color = "black", theme ="colorblind" , borders = TRUE, title ="DD+ group ")

#### Figure 5
### Estimate and plot centrality
#VERSION TWO
library("igraph")
library(networktools)
gr1i<-as.igraph(gr1)
gr3i<-as.igraph(gr3)
ef1<-expectedInf(gr1i, step = "both", directed = FALSE)
ef3<-expectedInf(gr3i, step = "both", directed = FALSE)
ep1<-plot(ef1)
ep3<-plot(ef3)
uu <- c("b1", "b2", "b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13")
de1 <-unclass(ef1$step2)
de3 <-unclass(ef3$step2)
kadate <- data.frame(uu, de1,de3)
cen_l <- melt(kadate, id="uu")
names(cen_l)[2] <- "Datasets"
dss_ib<-dss_i %>% filter(Datasets=="d1" | Datasets=="d3")

library(NetworkToolbox)
i1<-getWmat(qgraph(netg1$network[[1]]))
i3<-getWmat(qgraph(netg1$network[[2]]))
wpc1 <- participation(i1, comm = "walktrap")
wpc3 <- participation(i3, comm = "walktrap")
uu <- c("b1", "b2", "b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13")
di1 <-unclass(wpc1$overall)
di3 <-unclass(wpc3$overall)
kadati <- data.frame(uu, di1,di3 )
cen_i <- melt(kadati, id="uu")
names(cen_i)[2] <- "Datasets"

uu <- c("b1", "b2", "b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13")
ds1<-centralityPlot(GGM =list(Healthy_between = netg1$network[[1]], Depressed_between = netg1$network[[2]]),include = c("Strength"))

#dss_ib<-dss_i %>% filter(Datasets=="d1" | Datasets=="d3")
dss<-ds1$data
dss$id = numeric(nrow(dss))
for (i in 1:nrow(dss)){
  dat_temp <- dss[1:i,]
  dss[i,]$id <- nrow(dat_temp[dat_temp$type == dss[i,]$type,])
}
dss <-dcast(dss, id~type, value = 'value')
library(gdata)#Renaming variables 
dss <- rename.vars(dss, from = c("Depressed_between", "Depressed_within", "Healthy_between", "Healthy_within"), 
                   to = c("d3", "d4", "d1", "d2")) 
dss <- data.frame(uu, dss)
dss<-subset(dss, select =c(-id))
#dss<-dss[c("uu", "d1","d2")]
dss_i <- melt(dss, id="uu")
names(dss_i)[2] <- "Datasets"


cor(scale(centrality(gr1)$InDegree), scale(centrality(gr3)$InDegree)) #0.86
cor(scale(centrality(gr1)$InExpectedInfluence), scale(centrality(gr3)$InExpectedInfluence)) #0.91
cor(scale(de1), scale(de3)) # 0.94
cor(scale(di1), scale(di3)) # 0.67


#STRENGHT
strength1b <- ggplot(data=dss_i, aes(x=uu, y=value, group=Datasets, colour=Datasets)) +
  geom_line(size=1.2) +
  geom_point(shape = 21, fill = "white", size = 1.5, stroke = 1) +
  xlab(" ") + ylab("Strength") +
  scale_y_continuous(limits = c(-2, 2)) + 
  scale_x_discrete(labels=Names2) +
  theme_bw() +
  coord_flip()+
  theme(panel.grid.minor=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) + 
  scale_color_manual(values=c("#4682B4", "#B4464B"),
                     labels=c("MDD-","MDD+"));strength1b 

#EXPECTED INFLUENCE 2
cen_lb <- cen_l %>% filter(Datasets == "de1" | Datasets == "de3")
expe1b <- ggplot(data=cen_lb, aes(x=uu, y=value, group=Datasets, colour=Datasets)) +
  geom_line(size=1.2) +
  geom_point(shape = 21, fill = "white", size = 1.5, stroke = 1) +
  xlab(" ") + ylab("Expected influence") +
  scale_y_continuous(limits = c(0, 3)) + 
  scale_x_discrete(labels=Names2) +
  theme_bw() +
  coord_flip()+
  theme(panel.grid.minor=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) + 
  scale_color_manual(values=c("#4682B4", "#B4464B"),
                     labels=c("DD-","DD+")); expe1b
expe2b   <-expe1b+ theme(axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.y=element_blank())


#PARTICIPATION COEFFICIENT 
cen_ib <- cen_i %>% filter(Datasets == "di1" | Datasets == "di3")
part1b <- ggplot(data=cen_i, aes(x=uu, y=value, group=Datasets, colour=Datasets)) +
  geom_line(size=1.2) +
  geom_point(shape = 21, fill = "white", size = 1.5, stroke = 1) +
  xlab(" ") + ylab("Participation coefficient") +
  scale_y_continuous(limits = c(0, 1.5)) + 
  scale_x_discrete(labels=Names2) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) + 
  coord_flip()+
  scale_color_manual(values=c("#4682B4", "#B4464B"),
                     labels=c("DD-","DD+")); part1b 
part2b<-part1b +theme(axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())

library(ggpubr)
ggarrange(strength1b,expe2b ,part2b, widths = c(1, 0.7, 0.7),  
          labels = c("", "", ""), align = "h",
          ncol = 3, nrow = 1, common.legend = TRUE, legend = "right")

#unstandardized strength

cents1 <- as.data.frame(cbind(scale(centrality(netg1$network[[1]])$InDegree),
                              scale(centrality(netg1$network[[2]])$InDegree)))


centsa <- as.data.frame(centrality(netg1$network[[1]])$InDegree)
names(centsa)[1] <- "v1"
centsb <- as.data.frame(centrality(netg1$network[[2]])$InDegree)
names(centsb)[1] <- "v2"
centsu_long<-cbind(centsa, centsb)
centsu <- mutate(centsu_long, id = rownames(centsu_long))
colnames(centsu)<-c("1", "2", "Symptoms")
cents_long <- melt(centsu, id="Symptoms")
# cents_long$Symptoms <- as.numeric(cents_long$Symptoms)
names(cents_long)[2] <- "Datasets"

#STRENGHT
strength1c <- ggplot(data=cents_long, aes(x=Symptoms, y=value, group=Datasets, colour=Datasets)) +
  geom_line(size=1.2) +
  geom_point(shape = 21, fill = "white", size = 1.5, stroke = 1) +
  xlab(" ") + ylab("Strength") +
  scale_y_continuous(limits = c(-2, 2)) + 
  scale_x_discrete(labels=Names2) +
  theme_bw() +
  coord_flip()+
  theme(panel.grid.minor=element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) + 
  scale_color_manual(values=c("#4682B4", "#B4464B"),
                     labels=c("DD-","DD+"));strength1c 


library(ggpubr)
ggarrange(strength1c,expe2b ,part2b, widths = c(1, 0.7, 0.7),  
          labels = c("", "", ""), align = "h",
          ncol = 3, nrow = 1, common.legend = TRUE, legend = "right")


# stagility estimates
library(bootnet)
# making the test again in individual networks 
network1b <- estimateNetwork(data1, default="EBICglasso")
network3b <- estimateNetwork(data3, default="EBICglasso")
plot(network1b)
plot(network3b)
kboot1a <- bootnet(network1b, nBoots = 1000)
kboot1b <- bootnet(network1b, nBoots = 1000, type = "case")

kboot3a <- bootnet(network3b, nBoots = 1000)
kboot3b <- bootnet(network3b, nBoots = 1000, type = "case")

### Plot edge weight CI
plot(kboot1a, labels = FALSE, order = "sample") 
plot(kboot3a, labels = FALSE, order = "sample") 

### Plot centrality stability
plot(kboot1b)
plot(kboot3b)

### Centrality stability coefficient
cs1 <- corStability(kboot1b)  
cs3 <- corStability(kboot3b) 
cs <- matrix(NA,2,2)
cs[1,1:2] <- round(cs1, digits=2)
cs[2,1:2] <- round(cs3, digits=2)
colnames(cs) <- c("edge", "stregnth")
rownames(cs) <- c("MDD-", "MDD+")
cs

### Edge weights diff test
plot(kboot1a, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(kboot3a, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")

### Centrality diff test
plot(kboot1a, "strength", order="sample", labels=TRUE) 
plot(kboot3a, "strength", order="sample", labels=TRUE) 

####------------- Reviewer asked bootstrapping centrality correlations -----------------####

cor(scale(centrality(gr1)$InDegree), scale(centrality(gr3)$InDegree)) #0.86
cor(scale(centrality(gr1)$InDegree), scale(centrality(gr3)$InDegree)) #0.86
ccc1 <-centrality_auto(gr1)
ccc3 <-centrality_auto(gr3)
rew1<-cor(ccc1$node.centrality$Strength,ccc3$node.centrality$Strength)
rew1 #0.86

rew2<-cor(ccc1$node.centrality$ExpectedInfluence,ccc3$node.centrality$ExpectedInfluence)
rew2 #0.91

tic()
library(purrr)
library(parallel)
boots <- 1000
set.seed(33, sample.kind = "Rounding")

data_rew1 <- data1

data_rew3 <- data3

rew_1_c <- map(1:boots,
                             ~ sample.int(nrow(data_rew1), replace = TRUE))
rew_3_c <- map(1:boots,
                           ~ sample.int(nrow(data_rew3), replace = TRUE))
toc()

get_centrality_ggm <- function(i, df){
  df_boot_res_rew <- df[i,]
  results <- estimateNetwork(df_boot_res_rew, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5, verbose = FALSE)
  centrality_auto(results)$node.centrality$Strength
}


study_1_centrality_res_rew <- mclapply(rew_1_c, get_centrality_ggm, df = data_rew1,
                                           mc.cores = detectCores(logical = FALSE) - 1L)
study_2_centrality_res_rew <- mclapply(rew_3_c, get_centrality_ggm, df = data_rew3,
                                           mc.cores = detectCores(logical = FALSE) - 1L)

## Find all  correlations (1,000,000) between strength centrality

a <- expand.grid(x = 1:1000, y = 1:1000)
cross_sec_res_strength_across_samples_cor_rew <- mclapply(1:1e6, function(i){
  x <- a$x[i]; y <- a$y[i]
  cor(study_1_centrality_res_rew[[x]], study_2_centrality_res_rew[[y]])
}) 


#create dataframe of bootstrapped correlations
boot.cor_cross_sec_res_strength_rew <- as.data.frame(do.call(rbind, cross_sec_res_strength_across_samples_cor_rew))
names(boot.cor_cross_sec_res_strength_rew) <- "r"

mean(boot.cor_cross_sec_res_strength_rew$r)
round(quantile(boot.cor_cross_sec_res_strength_rew$r, c(.025)), 2)
round(quantile(boot.cor_cross_sec_res_strength_rew$r, c(.975)), 2)

## the Median Ranks in Centrality for Symptoms Throughout the Replicates?
## Look at median centrality rank of symptoms in the cross-sectional network

study_1_centrality_res_rew_num <- as.data.frame(do.call(rbind, study_1_centrality_res_rew))
names(study_1_centrality_res_rew_num) <- Names3

study_1_centrality_res_rew_num_rank <- t(apply(study_1_centrality_res_rew_num, 1, function(x) rank(-x) ))

psych::describe(study_1_centrality_res_rew_num_rank)

## Look at median centrality rank of symptoms in the change over time network 
## (Ranks: No symptom ranked 1, Self-Dislike and Sadness = 2, Effort to Work = 3, Anhedonia = 8)

study_2_centrality_res_rew_num <- as.data.frame(do.call(rbind, study_2_centrality_res_rew))
names(study_2_centrality_res_rew_num) <- Names3

study_2_centrality_res_rew_num_rank <- t(apply(study_2_centrality_res_rew_num, 1, function(x) rank(-x) ))

psych::describe(study_2_centrality_res_rew_num_rank)

# --------------- Same for expected influence --------------------#

get_centrality_ggm2 <- function(i, df){
  df_boot_res_rew <- df[i,]
  results <- estimateNetwork(df_boot_res_rew, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5, verbose = FALSE)
  centrality_auto(results)$node.centrality$ExpectedInfluence
}

study_1_centrality_res_rew2 <- mclapply(rew_1_c, get_centrality_ggm2, df = data_rew1,
                                       mc.cores = detectCores(logical = FALSE) - 1L)
study_2_centrality_res_rew2 <- mclapply(rew_3_c, get_centrality_ggm2, df = data_rew3,
                                       mc.cores = detectCores(logical = FALSE) - 1L)

## correlations (1,000,000) 
a <- expand.grid(x = 1:1000, y = 1:1000)
cross_sec_res_strength_across_samples_cor_rew2 <- mclapply(1:1e6, function(i){
  x <- a$x[i]; y <- a$y[i]
  cor(study_1_centrality_res_rew2[[x]], study_2_centrality_res_rew2[[y]])
}) 

#create dataframe of bootstrapped correlations
boot.cor_cross_sec_res_strength_rew2 <- as.data.frame(do.call(rbind, cross_sec_res_strength_across_samples_cor_rew2))
names(boot.cor_cross_sec_res_strength_rew2) <- "r"

mean(boot.cor_cross_sec_res_strength_rew2$r)
round(quantile(boot.cor_cross_sec_res_strength_rew2$r, c(.025)), 2)
round(quantile(boot.cor_cross_sec_res_strength_rew2$r, c(.975)), 2)

## Look at median centrality rank of symptoms in the cross-sectional network

study_1_centrality_res_rew_num2 <- as.data.frame(do.call(rbind, study_1_centrality_res_rew2))
names(study_1_centrality_res_rew_num2) <- Names3

study_1_centrality_res_rew_num_rank2 <- t(apply(study_1_centrality_res_rew_num2, 1, function(x) rank(-x) ))

psych::describe(study_1_centrality_res_rew_num_rank2)

study_2_centrality_res_rew_num2 <- as.data.frame(do.call(rbind, study_2_centrality_res_rew2))
names(study_2_centrality_res_rew_num2) <- Names3

study_2_centrality_res_rew_num_rank2 <- t(apply(study_2_centrality_res_rew_num2, 1, function(x) rank(-x) ))

psych::describe(study_2_centrality_res_rew_num_rank2)


## Identifying redundant nodes in the MDD_ and MDD+ network - None suggested
goldbricker(data1, p = 0.05, method = "hittner2003", threshold = 0.25,
            corMin = 0.5, progressbar = TRUE)

goldbricker(data3, p = 0.05, method = "hittner2003", threshold = 0.25,
            corMin = 0.5, progressbar = TRUE)

##Poorly informative items
des1<-psych::describe(data1)
psych::describe(des1$sd) 
des1$sd[4] 
mean(des1$sd)-2.5*sd(des1$sd) 

# correlation between centrality and sd
Study1_rew_sd_centrality<-cor(psych::describe(data1)$sd,ccc1$node.centrality$Strength)
Study1_rew_sd_centrality

## bootstrap 
boots <- 1000
set.seed(33, sample.kind = "Rounding")
row_ids_study_1_cent <- map(1:boots,
                            ~ sample.int(nrow(data1), replace = TRUE))

set.seed(33, sample.kind = "Rounding")
row_ids_study_3_cent <- map(1:boots,
                            ~ sample.int(nrow(data3), replace = TRUE))



get_cor_cent_sd_rew_within <- function(i, df){
  df_boot_study_1_within <- df[i,]
  results_study_1_within <- estimateNetwork(df_boot_study_1_within, default = "EBICglasso", corMethod = "cor_auto", tuning = 0.5, verbose = FALSE)
  cent_rank_study_1_within <- centrality_auto(results_study_1_within)$node.centrality$Strength
  sd_college_study_1_within <- psych::describe(df_boot_study_1_within)$sd
  cor(cent_rank_study_1_within,sd_college_study_1_within)
  
}


cent_sd_cor <- mclapply(row_ids_study_1_cent, get_cor_cent_sd_rew_within, df = data1,
                        mc.cores = detectCores(logical = FALSE) - 1L)

cent_sd_cor3 <- mclapply(row_ids_study_3_cent, get_cor_cent_sd_rew_within, df = data3,
                        mc.cores = detectCores(logical = FALSE) - 1L)


#bootstrapped correlations
boot.cor_cross_sec_cent_with_sd_study_1 <- as.data.frame(do.call(rbind, cent_sd_cor))
names(boot.cor_cross_sec_cent_with_sd_study_1) <- "r"
mean(boot.cor_cross_sec_cent_with_sd_study_1$r)
round(quantile(boot.cor_cross_sec_cent_with_sd_study_1$r, c(.025)), 2)
round(quantile(boot.cor_cross_sec_cent_with_sd_study_1$r, c(.975)), 2)

boot.cor_cross_sec_cent_with_sd_study_3 <- as.data.frame(do.call(rbind, cent_sd_cor3))
names(boot.cor_cross_sec_cent_with_sd_study_3) <- "r"
mean(boot.cor_cross_sec_cent_with_sd_study_3$r)
round(quantile(boot.cor_cross_sec_cent_with_sd_study_3$r, c(.025)), 2)
round(quantile(boot.cor_cross_sec_cent_with_sd_study_3$r, c(.975)), 2)

######### Eikos bootstrapped spinglass  analyses #############

### Iterated community detection via spinglass
#library("IComDet") Function from the Eikos mail. 
library("naniar")
library("pheatmap")
library("ggcorrplot")

# In the following, we perform IComDetSpin for network gr1 MDD-

icd1A <- IComDetSpin(gr1, gr1$graphAttributes$Graph$nNodes, numberEstimations = 5000)

# Frequency of unique solutions
icd1A$freqOfResult # 24.14%, 19.24%

# Save membership vector of the two most frequent unique solutions
icdgr1 <- as.factor(icd1A$uniqueMembershipVectors[,1])
icdgr1.2 <- as.factor(icd1A$uniqueMembershipVectors[,2])

# Most common solution:
icdgr1 <- list('1: First'=c(1,2,4), '2: Second'=c(3, 5, 6, 10),
               '3: Third'=c(7), '4: Forth'=c(8), '5: Fifth'=c(9, 11:13))    #you need to adapt this to your network

# Second most common solution:
icdgr1.2 <- list('1: First'=c(1:6), '2: Second'=c(7),
                 '3: Third'=c(8), '4: Forth'=c(9, 11:13),'5: Fifth'=c(10))    #you need to adapt this to your network


# Plot
#pdf(paste0(figs, "S.Common_communities.pdf"))
par(mfrow=c(1,2))
icdg1 <- qgraph(gr1, layout=gr1$layout, groups=icdgr1, legend=FALSE, vsize=9, legend=FALSE, title= paste("GGM IComDet ", as.character(icd1A$freqOfResult[1]*100), "%", sep = ""))
icdg1.2 <- qgraph(gr1, layout=gr1$layout, groups=icdgr1.2, legend=FALSE, vsize=9, legend=FALSE, title = paste("GGM IComDet ", as.character(icd1A$freqOfResult[2]*100), "%", sep = ""))
#dev.off()  # this plots the most common solutions!

# Plot node coappearance matrices and respective networks
pdf(paste0(figs, "S.Heatmap_PhobiaNetwork.pdf"))
par(mfrow=c(1,1))
icd1A$heatmapClustMembership
dev.off()

#Same for the MDD+ network (gr3)   

icd2A <- IComDetSpin(gr3, gr3$graphAttributes$Graph$nNodes, numberEstimations = 5000)

# Frequency of unique solutions
icd2A$freqOfResult # 65.28, 11.38

# Save membership vector of the two most frequent unique solutions
icdgr2 <- as.factor(icd2A$uniqueMembershipVectors[,1])
icdgr2.2 <- as.factor(icd2A$uniqueMembershipVectors[,2])

# Most common solution:
icdgr2 <- list('1: First'=c(1,2,4), '2: Second'=c(3, 5:7,10),
               '3: Third'=c(8,9,11:13))    #you need to adapt this to your network

# Second most common solution:
icdgr2.2 <- list('1: First'=c(1,2,4), '2: Second'=c(3, 5:7,10),
                 '3: Third'=c(8,9,11,12), '4: Forth'=c(13))    #you need to adapt this to your network


# Plot
#pdf(paste0(figs, "S.Common_communities.pdf"))
par(mfrow=c(1,2))
icdg2 <- qgraph(gr3, layout=gr3$layout, groups=icdgr2, legend=FALSE, vsize=9, legend=FALSE, title= paste("GGM IComDet ", as.character(icd2A$freqOfResult[1]*100), "%", sep = ""))
icdg22 <- qgraph(gr3, layout=gr3$layout, groups=icdgr2.2, legend=FALSE, vsize=9, legend=FALSE, title = paste("GGM IComDet ", as.character(icd2A$freqOfResult[2]*100), "%", sep = ""))
#dev.off()  # this plots the most common solutions!

# Plot node coappearance matrices and respective networks
#pdf(paste0(figs, "S.Heatmap_PhobiaNetwork.pdf"))
icd2A$heatmapClustMembership
#dev.off()
ichm1 <- icd1A$heatmapClustMembership
ichm2 <- icd2A$heatmapClustMembership

#library(patchwork)
#p3 + pp3 

# Plots 

library(tidyr)
library(ggridges)
p1 <- df_data %>%
  ggplot(aes(x = dep, fill = dep)) +
  geom_bar(alpha = 0.8) +
  scale_fill_tableau() +
  guides(fill = FALSE) +
  xlab("Depression") + ylab("Number of cases")
p2 <- df_data %>%
  gather(x, y, b1:b13) %>%
  ggplot(aes(x = y, y = dep, color = dep, fill = dep)) +
  facet_wrap( ~ x, scale = "free", ncol = 3) +
  scale_fill_tableau() +
  scale_color_tableau() +
  geom_density_ridges(alpha = 0.8) +
  guides(fill = FALSE, color = FALSE) + 
  xlab("") + ylab("")
grid.arrange(p1, p2, ncol = 2, widths = c(0.3, 0.7))
plot1a<-grid.arrange(p1, p2, ncol = 2, widths = c(0.3, 0.7))

table(df_data$dep)

#------------Resampling----------------#

library(dplyr)
newd1 <-sample_n(data1, 600)
newd2 <-sample_n(data1, 600)
newd3 <-sample_n(data1, 600)
newd4 <-sample_n(data1, 600)
newd5 <-sample_n(data1, 600)

newd1cor <- cor_auto(newd1)
newd2cor <- cor_auto(newd2)
newd3cor <- cor_auto(newd3)
newd4cor <- cor_auto(newd4) 
newd5cor <- cor_auto(newd5) 

# determine maximum value
n1 <- getWmat(qgraph(newd1cor, graph = "glasso", sampleSize = nrow(newd1), DoNotPlot = TRUE))
n2 <- getWmat(qgraph(newd2cor, graph = "glasso", sampleSize = nrow(newd2), DoNotPlot = TRUE))
n3 <- getWmat(qgraph(newd3cor, graph = "glasso", sampleSize = nrow(newd3), DoNotPlot = TRUE))
n4 <- getWmat(qgraph(newd4cor, graph = "glasso", sampleSize = nrow(newd4), DoNotPlot = TRUE))
n5 <- getWmat(qgraph(newd5cor, graph = "glasso", sampleSize = nrow(newd5), DoNotPlot = TRUE))

Max <- max(c(n1, n2, n3, n4, n5))
Max

gg1 <- qgraph(newd1cor, graph = "glasso", layout = "spring",  sampleSize = nrow(newd1), maximum = 0.44, tuning = 0.25 )
gg2 <- qgraph(newd2cor, graph = "glasso", layout = "spring",  sampleSize = nrow(newd2), maximum = 0.44, tuning = 0.25 )
gg3 <- qgraph(newd3cor, graph = "glasso", layout = "spring",  sampleSize = nrow(newd3), maximum = 0.44, tuning = 0.25 )
gg4 <- qgraph(newd4cor, graph = "glasso", layout = "spring",  sampleSize = nrow(newd4), maximum = 0.44, tuning = 0.25 )
gg5 <- qgraph(newd5cor, graph = "glasso", layout = "spring",  sampleSize = nrow(newd5), maximum = 0.44, tuning = 0.25 )


icdn1 <- IComDetSpin(gg1, gg1$graphAttributes$Graph$nNodes, numberEstimations = 5000)
icdn2 <- IComDetSpin(gg2, gg1$graphAttributes$Graph$nNodes, numberEstimations = 5000)
icdn3 <- IComDetSpin(gg3, gg1$graphAttributes$Graph$nNodes, numberEstimations = 5000)
icdn4 <- IComDetSpin(gg4, gg1$graphAttributes$Graph$nNodes, numberEstimations = 5000)
icdn5 <- IComDetSpin(gg5, gg1$graphAttributes$Graph$nNodes, numberEstimations = 5000)

icdn1$heatmapClustMembership
icdn2$heatmapClustMembership
icdn3$heatmapClustMembership
icdn4$heatmapClustMembership
icdn5$heatmapClustMembership

icdn1$freqOfResult
icdn2$freqOfResult
icdn3$freqOfResult
icdn4$freqOfResult
icdn5$freqOfResult


icdgrn1 <- as.factor(icdn1$uniqueMembershipVectors[,1])
as.factor(icdn1$uniqueMembershipVectors[,1])
as.factor(icdn2$uniqueMembershipVectors[,1])
as.factor(icdn3$uniqueMembershipVectors[,1])
as.factor(icdn4$uniqueMembershipVectors[,1])
as.factor(icdn5$uniqueMembershipVectors[,1])




