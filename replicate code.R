
theme_set(
  theme_bw()+
    theme(
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = rel(1.1)),
      legend.position = "none")
)

pacman::p_load(ggsci,scales,paletteer,cowplot,geomtextpath)


#Fig.3a-----------

timeHM_formatter <- function(x){
  h <- floor(x/4)
  m <- round(15*(x %% 4))
  lab <- sprintf("%02d:%02d",h,m)
  return(lab)
}

read_xlsx("data_files/Fig.3a.xlsx") %>% 
  mutate(across(c(coef,LB,UB, coef2,LB2,UB2), ~.x/0.03024347)) %>% 
  mutate(bin = 1:96, mean = mean(coef2)) %>% 
  ggplot(aes(x=bin)) + 
  geom_line(aes(y=coef),color="grey80",linetype="longdash",size=0.5)+ 
  geom_ribbon(aes(ymin=LB,ymax=UB),alpha=0.1,fill='grey40')+
  geom_line(aes(y=coef2),color="#7fc97f",size=0.5)+      #did02
  geom_ribbon(aes(ymin=LB2,ymax=UB2),alpha=0.3,fill='#74a9cf')+
  geom_hline(yintercept = 0,color="grey50",size=0.5,
             linetype="dashed",alpha=0.7) +
  
  annotate("segment",x=2,xend=8,y=0.25,yend = 0.25,
           linetype="longdash",color="grey80")+
  annotate("text",x=30,y=0.25,label="General knowledge intervention") +
  annotate("segment",x=2,xend=8,y=0.2,yend = 0.2,color="#7fc97f")+
  annotate("text",x=29,y=0.2,label="Specific guidance intervention") +
  labs(x=NULL, y='Electricity conservation effects (%)')+
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = seq(0,96,16), labels = timeHM_formatter) +
  scale_y_percent(limits = c(-0.7,0.3))+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(size = rel(1)))


#Fig.3b-----------

showvar <- c('income:\nmiddle','income:\nmiddle_high','income:\nhigh','education:\nmiddle','education:\nhigh','child &\nno elderly','elderly &\nno child','child &\nelderly')
#the order that we desire to beautify the figure
varlevels <- c('income:\nmiddle','income:\nmiddle_high','income:\nhigh','education:\nmiddle','education:\nhigh','elderly &\nno child','child &\nno elderly','child &\nelderly')

colorset <- c("#af8dc3","#f1a340","#4DBBD5FF")

read_xlsx("Fig.3b.xlsx") %>% 
  gather(ends_with("...1."),key="var",value="coef") %>% 
  mutate(var = str_remove_all(var, "\\...1."),
         var=str_replace(var,"edu",""),
         type = case_when(
           var %in% c('inc_lev2','inc_lev3','inc_lev4') ~ 'income',
           var %in% c('ISCED3','ISCED4_8') ~ 'education',
           var %in% c('child','old','child_old') ~ 'HHstructure')) %>%  
  mutate(var2 = rep(showvar, each=96),
         var2 = factor(var2, levels = varlevels)) %>%
  mutate(coef = ifelse(var=='child_old',coef-0.02, coef),
         coef = coef/mean(regdata_inter$add_ele_wh)) %>% 
  
  ggplot(aes(x=var2,y=coef,fill= type))+
  geom_jitter(aes(color=type), alpha=0.2, width = 0.2) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  stat_summary(fun='mean', shape=21, fill='white',size=0.4) +
  geom_hline(yintercept = 0,color="grey50",size=0.5,
             linetype="dashed",alpha=0.7) +
  scale_y_percent(limits=c(-1.2,1.2), 
                  breaks = c(seq(-0.8, 0.8, 0.4)),
                  label = c('-80%','-40%','0','40%','80%')) +
  labs(x=NULL, y='Electricity conservation effects (%)') +
  theme(axis.text = element_text(size = rel(0.8)),
        axis.title.y = element_text(size = rel(1)),
        legend.position = 'none')


#Fig.4b-----------

df <- read_xlsx("Fig.4b.xlsx") 

scen_levels <- c("SSP1 RCP2.6","SSP2 RCP6.0","SSP5 RCP8.5")
scen_cols   <- c("SSP1 RCP2.6" = "#54A24B",   # 绿
                 "SSP2 RCP6.0" = "#4C78A8",   # 蓝
                 "SSP5 RCP8.5" = "#F58518")   # 橙

offset_map <- c("SSP1 RCP2.6" = 0.25, "SSP2 RCP6.0" = 0, "SSP5 RCP8.5" = +0.5)

seg_end <- df %>%
  dplyr::filter(year == 2030, !is.na(ELE_inter2)) %>%
  mutate(
    scenario = factor(scenario, scen_levels),
    x    = year + unname(offset_map[as.character(scenario)]),
    y0   = ele,            
    y1   = ELE_inter2,     
    ymid = (y0 + y1) / 2
  )

df %>% 
  dplyr::filter(year >= 2021) %>%
  ggplot() +
  geom_line(aes(year, ele, colour = scenario, group = scenario, linetype = "Baseline"),
            linewidth = 1) +
  geom_line(aes(year, ELE_inter2, colour = scenario, group = scenario, 
                linetype = "Intervention"),
            linewidth = 1) +
  geom_point(aes(year, ele, colour = scenario), shape = 1, stroke = 1, size = 2) +
  geom_point(aes(year, ELE_inter2, colour = scenario), shape = 1, stroke = 1, size = 2) +
  
  geom_line(data = df %>% 
              dplyr::filter(year <= 2021) %>% distinct(year, ele), 
            aes(year, ele, group = 1), colour = "#377EB8", linewidth = 1.4) +
  geom_point(data = df %>% 
               dplyr::filter(year <= 2021) %>% distinct(year, ele), 
             aes(year, ele), colour = "#377EB8", size = 2.2, stroke = 0) +
  
  geom_errorbar(data = seg_end,
                aes(x = x, ymin = y1, ymax = y0, colour = scenario),
                width = 0.35, linewidth = 0.9) +
  #只给中间情景加注释，避免重复
  geom_text(data = seg_end %>% dplyr::filter(scenario == "SSP2 RCP6.0"),
            aes(x = x + 0.6, y = ymid, label = "Intervention"),
            angle=270,hjust = 0.8, vjust = -1, size = 4, colour = "#222222") +
  
  scale_color_manual(values = scen_cols, name = "Scenario") +
  scale_linetype_manual(values = c(Baseline = "dashed",Intervention = "longdash"),name ="") +
  scale_x_continuous(limits = c(2012, 2032), expand = c(0, 0),
                     breaks = seq(2015, 2030, 5)) +
  labs(x = NULL, y = "Cooling consumption (TWh)") +
  theme(legend.position    = c(0.3, 0.8),  
        legend.box         = "horizontal",
        legend.text = element_text(size = rel(1.1)))


#Fig.4c-----------

df_top10 <- read_xlsx("Fig.4c.xlsx")

lab_df <- data.frame(
  province = factor('Sichuan', levels = levels(df_top10$province)), 
  y       = seq(20,80,20),
  lab     = seq(20,80,20)  
)

df_top10 %>%
  pivot_longer(c(Baseline, Intervention), names_to = "type", values_to = "ELE") %>%
  ggplot(aes(x=province,y=ELE,fill=type))+
  geom_col(width = 0.9,
           position = position_dodge2(preserve = "single", padding = 0.05)) +
  coord_curvedpolar(theta = "x", start = -pi/2, direction = 1) +
  scale_y_continuous(limits = c(0,80),expand = c(0,0),breaks = seq(0,80,20),
                     labels = NULL)+
  geom_text(data = lab_df,
            aes(x = province, y = y, label = lab),
            inherit.aes = FALSE,
            colour = "grey30", size = 4,
            angle = 90,      # 旋转 90°
            hjust = 0.5,  vjust = 0) +  
  scale_fill_manual(values = c(Baseline = "#FDB863", Intervention = "#74ADD1")) +
  labs(x=NULL, y = "Cooling consumption (TWh)", fill=NULL)+
  guides(fill=guide_legend(nrow = 1))+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        plot.margin = margin(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = rel(1.4)),
        legend.position = 'bottom',
        legend.text = element_text(size = rel(1.1)),
        legend.box.spacing = unit(2, "pt"),      
        legend.margin      = margin(0, 0, 0, 0), 
        legend.box.margin  = margin(t = -4))     


#Fig.4d-----------

pal <- c(
  "2021"        = "#777777",   # 深灰
  "2030"        = "#4D4D4D",   # 中灰
  "Education"   = "#009E73",   # 绿
  "Composition" = "#8DA0CB",   # 淡紫蓝
  "Income"      = "#E69F00",   # 橙
  "Others"      = "#56B4E9",   # 蓝
  "Final"       = "#777777"    # 终值：中灰
)

read_xlsx("Fig.4d") %>% 
  mutate(
    type = factor(type,levels = c("e2021","e2030","edu","hhstr","income","others","final"),
                  labels = c("2021","2030","Education","Composition","Income","Others","Final")),
    x0 = as.integer(type),
    x1 = x0 + 0.8,
    xm = x0 + 0.4) %>% 
  ggplot() +
  geom_rect(aes(xmin=x0, xmax = x1, ymin=lower, ymax=upper,fill = factor(type)),
            colour = "#E5E5E5", size = 0.25, alpha = 0.95) +
  geom_errorbar(aes(x=xm, ymin=lb, ymax=ub), 
                width = 0.12, size = 0.5, colour = "#2b2b2b")+
  annotate('text', x=7.55, y=770, label='SSP2 RCP6.0',
           hjust=1, family='arial',face='bold',size=4) +
  
  scale_fill_manual(values = pal, guide = "none") +
  scale_y_continuous(expand = c(0,0),limits=c(0,810), breaks = seq(0,800,200))+
  scale_x_continuous(expand=c(0.01,0.01), limits = c(1,8),breaks=seq(1.4,7.4,1),
                     labels=c('2021','2030\nbaseline','Education','Composition','Income','Others','2030 after\nintervention')) +
  labs(x=NULL, y="Cooling electricity demand (TWh)") +
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(.1,.1,.1,.1),'cm'),
        legend.position = 'none',
        axis.text.x = element_text(size = rel(1.2)))



