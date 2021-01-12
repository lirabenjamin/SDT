# Study 1. For presentation

library(tidyverse)
# devtools::install_github("lirabenjamin/Ben")
library(Ben)
theme_sl = function(){theme_ang()+theme(text = element_text(family = "Helvetica"))}
penncol = function(){scale_color_manual(values = c("#990000","#011F5b"))}

gr = read_csv('Data/cleaned_grit rubric.csv')

p1 = gr  %>% 
  select(Grit,`Self-concordant goals` = scg) %>% 
  ggplot(aes(Grit,`Self-concordant goals`))+
  geom_jitter() + 
  geom_smooth(method= "lm")+theme_sl()+
  labs(title = "Grit and self-concordance")+
  coord_cartesian(xlim = c(2,5),ylim = c(2,7))
ggsave("Plots/1.1.pdf",plot = p1,width = 6,height = 4)
p1
p1 + geom_hline(yintercept = 4)+
  geom_vline(xintercept = 3)+
  geom_hline(yintercept = 4.832910, linetype=2,color ="red")+
  geom_vline(xintercept = 3.673401, linetype=2,color ="red")+
  annotate("rect",xmin = 3.673401,xmax = 5,ymin = 2,ymax = 4.832910,fill = "red", alpha = .1)+
  annotate("rect",xmin = 3,xmax = 5,ymin = 2,ymax = 4,fill = "black", alpha = .3)+
  coord_cartesian(xlim = c(2,5),ylim = c(2,7))+theme_sl()
ggsave("Plots/1.2.pdf",width = 6,height = 4)

gr %>% 
  select(Passion = grit_pass, Perseverance = grit_pers,scg) %>% 
  gather(Facet,Grit,-scg) %>% 
  rename(`Self Concordance` = scg) %>% 
  ggplot(aes(x = Grit,`Self Concordance`, color = Facet))+
  geom_jitter()+
  geom_smooth(method = "lm")+
  theme_sl()+
  penncol()+
  labs(title = "Passion correlates more strongly than perseverance with self concordance")+
  annotate("text",x = 2, y = 7, label = "rs = .42*** and .20**")+
  theme(legend.position = "none")
ggsave("Plots/1.3.pdf",width = 6,height = 4)


gr %>% 
  select(Grit,starts_with("scg2")) %>% 
  select(1:4) %>% 
  clnR2::cv_bake(new = Introjection, quos(scg2_goal1,scg2_goal2,scg2_goal3)) %T>% 
  Ben::HARcor() %>% 
  ggplot(aes(Grit,Introjection))+geom_jitter() + geom_smooth(method= "lm")+
  labs(subtitle = "r = -.31",
       title = "Grit and introjection are negatively related")+theme_sl()


