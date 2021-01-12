# Study 2 For presentation
library(tidyverse)
library(Ben)
library(lm.beta)
library(magrittr)
theme_sl = function(){theme_ang()+theme(text = element_text(family = "Helvetica"))}
penncol = function(){scale_color_manual(values = c("#990000","#011F5b"))}


pr = read_csv("Data/s2 210107-prolific.csv") %>% mutate(source = "Prolific")
sm = read_csv("Data/s2 210107-socialmedia.csv")
d = bind_rows(pr,sm)

dc = d %>% filter(DistributionChannel != "preview",
             source != "test",
             jobhours > 4,
             jobstatus == 1,
             jobdesc != "delete",
             att1 == 3,
             !is.na(jobdesc)) %>% 
  mutate(gender = ifelse(gender == 3, NA, gender))

vars = readxl::read_excel("Data/s2 codebook.xlsx")
keys = readxl::read_excel("Data/s2 codebook.xlsx",sheet = 2)
extract = function(var){vars %>% filter(!is.na(!!var)) %>% select(!!var)  %>% pull(!!var)}

# Alphas ####
dc %>% 
  # select(extract(quo(Grit))) %>% 
  # select(extract(quo(Passion))) %>% 
  # select(extract(quo(Perseverance))) %>%
  # select(extract(quo(External))) %>%
  # select(extract(quo(Introjected))) %>% 
  # select(extract(quo(Identified))) %>%
  # select(extract(quo(Intrinsic))) %>% 
   select(extract(quo(RAI))) %>% 
  # select(extract(quo(BO))) %>% 
  psych::alpha(keys = keys$RAI)

alpha = numeric()
scores = data.frame(id = 1:97)

for(i in 1:ncol(vars)){
  items = (vars[i] %>% pull(.))[!is.na((vars[i] %>% pull(.)))]
  key = (keys[i])[!is.na(keys[i])]
  a = dc %>% select(items) %>% 
    psych::alpha(x = ., keys = key)
  alpha = append(alpha, a$total$std.alpha)
  scores = cbind(scores,a$scores)
}
alpha %>% enframe %>% 
  mutate(Variable = colnames(vars)) %>% 
  rename(alpha = value) %>% 
  ggplot(aes(Variable,alpha))+
  geom_col()+
  geom_text(aes(label = numformat(alpha)),hjust=-.1)+
  ylab(expression(alpha))+
  geom_hline(yintercept=.6)+
  coord_flip(ylim = c(.5,1))+
  theme_sl()+
  labs(title = "Reliability")
ggsave("Plots/Alpha.pdf",height = 4,width = 4)

dc %>% select(g1:b5,-att1) %>% 
  gather(Item,Response) %>% 
  mutate(Variable = case_when(str_detect(Item,"g") ~ "Grit",
                              str_detect(Item,"m") ~ "Self-concordance",
                              str_detect(Item,"b") ~ "Burnout")) %>% 
  ggplot(aes(Response, fill = Variable))+
  geom_bar(alpha = .6)+
  facet_wrap(~Item)+
  theme_sl()+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#000000","#990000","#011F5b"))
ggsave("Plots/Item distributions.pdf",height = 6,width = 12)


names(scores) = c("id",colnames(vars))
scores = dc %>% select(age,gender,education,jobhours,jobpay,jobdesc,source) %>% 
  select_if(is.numeric) %>% 
  cbind(scores %>% select(-id)) %>% 
  rename(Burnout = BO,
         Age = age,
         Gender = gender,
         Education = education,
         Hours = jobhours,
         Pay = jobpay
         ) 

scores %>% select(Grit:Burnout) %>% 
  gather(factor_key = T) %>% 
  mutate(fam = case_when(key %in% c("Grit","Passion","Perseverance") ~ "Grit",
                         key %in% c("RAI","External","Introjected","Identified","Intrinsic") ~ "RAI",
                         key == "Burnout" ~ "Burnout")) %>% 
  ggplot(aes(value,fill = fam))+
  geom_density(col = "white",alpha = .6)+
  facet_wrap(~key)+
  theme_sl()+
  scale_fill_manual(values = c("#000000","#990000","#011F5b"))+
  theme(legend.position = "none")
ggsave("Plots/Hist.pdf",height = 4,width = 4)

scores %>% 
  select(Grit:Burnout) %>% 
  HARcor() %T>% 
  write.clip()

# Regressions ####
m1 = scores %>% 
  select(Grit,RAI,Burnout) %>% 
  mutate(Grit = scale(Grit,scale = F),
         RAI = scale(RAI,scale = F)) %>% 
  lm(Burnout ~ Grit+RAI,data = .)
m2 = scores %>% 
  select(Grit,RAI,Burnout) %>% 
  mutate(Grit = scale(Grit,scale = F),
         RAI = scale(RAI,scale = F)) %>% 
  lm(Burnout ~ Grit*RAI,data = .)

m2 %>% lm.beta %>% summary
anova(m1,m2)

#install.packages("stargazer")
library(stargazer)
stargazer(m1,m2,type = "html") %>% write.clip

library(interactions)
interact_plot(model = lm(Burnout ~ Grit*RAI,data = scores),pred = Grit,modx = RAI,
              plot.points = T,
              colors = c("#990000","#000000","#011F5b"),vary.lty = F,jitter = T,point.alpha = .2,interval = T)+
  theme_gray()+
  theme_sl()
ggsave("Plots/2.3.pdf",width = 6,height = 4)


# Regressions Contorl ####
m0 = scores %>% 
  select(Burnout,Age,Gender,Education,Hours,Pay) %>% 
  lm(Burnout ~ .,data = .)
m1 = scores %>% 
  select(Burnout,Age,Gender,Education,Hours,Pay,Grit,RAI) %>% 
  mutate(Grit = scale(Grit,scale = F),
         RAI = scale(RAI,scale = F)) %>% 
  lm(Burnout ~ .,data = .)
m2 = scores %>% 
  select(Burnout,Age,Gender,Education,Hours,Pay,Grit,RAI) %>% 
  mutate(Grit = scale(Grit,scale = F),
         RAI = scale(RAI,scale = F)) %>%
  lm(Burnout ~ Age+Gender+Education+Hours+Pay+Grit*RAI,data = .)

lms = scores %>% 
  gather(`Grit Facet`,`Grit Score`,-c(Age,Gender,Education,Hours,Pay,External:RAI),-Burnout) %>% 
  gather(`SC Facet`,`Self Concordance Score`,-c(Age,Gender,Education,Hours,Pay,Burnout,`Grit Facet`,`Grit Score`)) %>% 
  group_by(`Grit Facet`,`SC Facet`) %>% 
  nest() %>% 
  mutate(lm = map(data,~lm(Burnout ~ Age+Gender+Education+Hours+Pay+scale(`Grit Score`)*scale(`Self Concordance Score`),data = .)),
         tidy = map(lm,broom::tidy)) %>% 
  unnest(tidy) %>% 
  select(-data,lm) %>% 
  filter(term %in% c('scale(`Grit Score`)','scale(`Self Concordance Score`)','scale(`Grit Score`):scale(`Self Concordance Score`)')) %>% 
  mutate(term = case_when(term == 'scale(`Grit Score`)' ~'Grit',
                          term == 'scale(`Self Concordance Score`)' ~ 'Self Concordance',
                          term == 'scale(`Grit Score`):scale(`Self Concordance Score`)' ~ 'Interaction'))
  
#write_rds(lms,"Data/lms.rds")
#write_rds(scores,"Data/scores.rds")

anova(m0,m1,m2)

m0 %>% lm.beta
m1 %>% lm.beta
m2 %>% lm.beta


library(stargazer)
stargazer(m0,m1,m2,type = "html",
          star.char = c("†", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001)) %>% write.clip


library(interactions)
interact_plot(model = lm(Burnout~Grit*RAI+Age+Gender+Education+Hours+Pay,data=scores),pred = Grit,modx = RAI,
              plot.points = T,
              colors = c("#990000","#000000","#011F5b"),vary.lty = F,jitter = T,point.alpha = .2,interval = T)+
  theme_gray()+
  theme_sl()
ggsave("Plots/2.3c.pdf",width = 6,height = 4)


# Mediation ####
library(lavaan)
med = sem(model = "Burnout ~ c*Grit + b*RAI
             RAI ~ a*Grit
             ab := a*b
             cp := c+ab
             pm := ab/cp",
    data= scores)
summary(med,standardized = T, fit.measures=T)

med2 = sem(model = "Burnout ~ b*Grit + c*RAI
             Grit ~ a*RAI
          ab := a*b
          cp := c+ab
          pm := ab/cp",
          data= scores)
summary(med2,standardized = T, fit.measures=T)
psych::describe(scores)
# Just correlation plot
scores %>% 
  ggplot(aes(Grit,RAI))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  theme_sl()+
  coord_cartesian(ylim = c(1,5),xlim = c(1,5))
ggsave("Plots/2.4.1.pdf",width = 6,height = 4)

  
scores %>% 
  select(Passion,Perseverance,RAI) %>% 
  gather(Facet,Grit,-RAI) %>%
  ggplot(aes(Grit,RAI,color = Facet))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  theme_sl()+
  coord_cartesian(ylim = c(1,5),xlim = c(1,5))+
  penncol()+
  theme(legend.position = "none")
ggsave("Plots/2.4.2.pdf",width = 6,height = 4)

#Clustering
cd = dc %>% 
  select(starts_with(c("g","m"))) %>% 
  select(-gender,-grit)

# Clustering ####
library(factoextra)

set.seed(123)

#Clusterizable?
clustertend::hopkins(cd,n=ncol(cd),byrow = F) #Debe ser menor que .50
fviz_dist(dist(cd), show_labels = FALSE) #Inspección gráfica

#Cuantos clusters
c1 = fviz_nbclust(cd, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+labs(subtitle = "Gap Statistic Method")
c2 = fviz_nbclust(cd, kmeans, nstart = 25, method = "wss", nboot = 50)+labs(subtitle = "Within cluster Sum of Squares")+geom_vline(xintercept = 4, linetype = 2,color="#4184B9")
c3 = fviz_nbclust(cd, kmeans, nstart = 25, method = "silhouette", nboot = 50)+labs(subtitle = "Silhouette Method")
c4 = NbClust::NbClust(cd, distance = "euclidean", min.nc = 3,max.nc = 5, method = "kmeans") %>% fviz_nbclust()+labs(title = "Optimal Number of Clusters",subtitle = "All fviz_nbclust() methods")
cowplot::plot_grid(c1,c2,c3,c4,nrow =1)
ggsave("Plots/2.howmany.pdf",width = 13,height = 3)


C1 = eclust(cd,FUNcluster = "kmeans",k = 1,nstart=100)
C2 = eclust(cd,FUNcluster = "kmeans",k = 2,nstart=100)
C3 = eclust(cd,FUNcluster = "kmeans",k = 3,nstart=100)
C4 = eclust(cd,FUNcluster = "kmeans",k = 4,nstart=100)
C5 = eclust(cd,FUNcluster = "kmeans",k = 5,nstart=100)
C6 = eclust(cd,FUNcluster = "kmeans",k = 6,nstart=100)
C7 = eclust(cd,FUNcluster = "kmeans",k = 7,nstart=100)

#Cluster Viz
fviz_cluster(C3,geom = "point",star.plot=T)+theme(legend.position = "bottom")
ggsave("Plots/2.cluster centers.pdf",width = 4,height = 3)
fviz_silhouette(C3)+coord_cartesian(ylim = c(-.05,.7))+theme(legend.position = "none")+labs(title = "Cluster Silhouette Plot")
ggsave("Plots/2.silhouette.pdf",width = 4,height = 3)

#Variance Explained
lm(as.matrix(cd)~C4$cluster %>% as.factor()) %>% summary() %>% map_dbl("r.squared") %>% round(2) %>% enframe() %>% separate(name,c("tr","item"),sep = " ") %>% select(-tr)

#Number of Misclassified obs
sum(C1$silinfo$widths$sil_width<0)
sum(C2$silinfo$widths$sil_width<0)
sum(C3$silinfo$widths$sil_width<0)
sum(C4$silinfo$widths$sil_width<0)

#Cluster Centers
scores %>% 
  mutate(Cluster = C7$cluster %>% as.factor()) %>% 
  group_by(Cluster) %>% 
  select(Grit,`Self-concordance` = RAI, Burnout) %>% 
  summarize_all(mean) %>%
  gather(Variable, Mean,-Cluster,factor_key = T) %>% 
  ggplot(aes(Variable,Mean,group=Cluster,color=Cluster))+geom_line(stat="identity")+
  theme(legend.position = "none")+labs(title = "Cluster Centers")
ggsave("Plots/2.centers.pdf",width = 4,height = 3)

C4$centers  %>% as_tibble(rownames = "Cluster") %>% ggradar::ggradar(grid.min = -1.5,grid.mid = 0,grid.max = 1.6)

#Plots
clusters = list(C1,C2,C3,C4,C5,C6,C7)
for (i in 1:7){
  a = fviz_cluster(clusters[[i]],geom = "point",star.plot=T)+theme(legend.position = "bottom")+theme_sl()
  b = scores %>% 
    mutate(Cluster = clusters[[i]]$cluster %>% as.factor()) %>% 
    group_by(Cluster) %>% 
    select(Grit,`Self-concordance` = RAI, Burnout) %>% 
    summarize_all(mean) %>%
    gather(Variable, Mean,-Cluster,factor_key = T) %>% 
    ggplot(aes(Variable,Mean,group=Cluster,color=Cluster))+geom_line(stat="identity")+
    theme(legend.position = "none")+labs(title = "Cluster Centers")+theme_sl()
  cowplot::plot_grid(a,b,nrow =1)
  ggsave(paste("Plots/2.cluster centers",i,".pdf"),width = 6,height = 4)
}

