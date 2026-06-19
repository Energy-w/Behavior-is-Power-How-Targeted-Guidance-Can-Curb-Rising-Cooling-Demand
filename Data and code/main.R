install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("ggsci")
install.packages("scales")
install.packages("cowplot")
install.packages("geomtextpath")
install.packages("ggridges")
install.packages("magrittr")
install.packages("ggpubr")
#install.packages("rstatix")
install.packages("caret")

library("dplyr")
library("ggplot2")
library("readxl")
library("ggsci")
library("scales")
library("cowplot")
library("geomtextpath")
library("ggridges")
library("magrittr")
library("ggpubr")
#library("rstatix")
library("caret")

theme_set(
  theme_bw()+
    theme(
      #panel.border = element_blank(),
      #axis.line = element_line(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = rel(1.1)),
      legend.position = "none")
)


#Figure 1 is the framework; Figs. 2a-f are plots made in Excel.

#Fig.2-----------
m <- mean(read_xlsx("../data/Fig.2a.xlsx")$mean_ele, na.rm=TRUE)
p_month <- read_xlsx("../data/Fig.2a.xlsx") %>% 
  ggplot(aes(x = month, y = mean_ele)) +
  geom_col(aes(alpha = summer), fill = "steelblue", color='grey30', size =0.2, width = 1) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.3, size=0.4, color="grey50") +
  geom_hline(yintercept = m, color="grey50", size=0.5,
             linetype="dashed", alpha=0.7) +
  annotate("text", x = 10, y = m +20, hjust = 0,
           label = paste0("Mean = ", round(m, 0), " kWh"), size = 2) +
  scale_alpha_manual(values = c(`FALSE` = 0.7, `TRUE` = 1), guide = "none") +
  scale_y_continuous(expand=c(0, 0), limits = c(0,420)) +
  scale_x_discrete(breaks = c('Jan','Apr','Jul','Oct')) +
  labs(x="Month in 2021", y = "Total electricity consumption (kWh/HH)") +
  theme(axis.title = element_text(size = rel(0.7)),
        axis.text = element_text(size = rel(0.7)))
p_month


p_annual <- read_xlsx("../data/Fig.2b.xlsx") %>% 
  ggplot(aes(x = mid, y = prop)) +
  geom_col(aes(width = width), fill = "steelblue", color='grey30', size=0.2) +
  scale_x_continuous(expand=c(0.02, 0.02),
                     breaks = c(200, 800, 1600, 2200, 3000, 4000)) +
  #scale_y_percent(expand=c(0, 0), limits = c(0,0.31)) +
  labs(x = "Annual cooling consumption (kWh/HH)",
       y = "Share of households") +
  theme(axis.title = element_text(size = rel(0.7)),
        axis.text = element_text(size = rel(0.7)))
p_annual


lab_df <- data.frame(
  province = factor('SX', levels = levels(read_xlsx("../data/Fig.2c.xlsx")$abbname)), 
  y       = seq(600, 1000, 300),
  lab     = seq(600, 1000, 300)   # paste0(seq(20,80,20), " TWh")
)
p_prov <- read_xlsx("../data/Fig.2c.xlsx") %>%
  ggplot(aes(x=abbname, y= mean_ele, fill=region2))+
  geom_col(width = 0.9,
           position = position_dodge2(preserve = "single", padding = 0.05)) +
  coord_curvedpolar(theta = "x", start = -pi/2, direction = 1) +
  scale_y_continuous(expand = c(0.02,0.02),limits = c(0,1100),
                     breaks = seq(0,1050,300), labels = NULL) +
  geom_text(data = lab_df,
            aes(x = province, y = y, label = lab),
            inherit.aes = FALSE,
            colour = "grey30", size = 2,
            hjust = 0.5,  vjust = 0) +
  scale_fill_manual(values = c(North = "#FDB863", South = "#74ADD1")) +
  labs(x=NULL, y = "Household annual cooling consumption (kWh)", fill=NULL)+
  guides(fill=guide_legend(nrow = 1)) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = rel(0.7)),
        axis.title = element_text(size = rel(0.7)),
        legend.position = "bottom",
        legend.text = element_text(size = rel(0.7)),
        legend.key.size = unit(0.3, "cm"),
        legend.box.spacing = unit(2, "pt"),      
        legend.margin      = margin(0, 0, 0, 0), 
        legend.box.margin  = margin(t = -4))     
p_prov


label_formatter <- function(avg){
  avg = paste0(avg,' kWh')
  return(avg)
}
p_inc <- read_xlsx("../data/Fig.2d_inc.xlsx") %>%   
  dplyr::filter(!is.na(inclev_cal)) %>% 
  group_by(inclev_cal) %>% 
  mutate(avg = round(mean(cool_hh_ele),0),
         avg = ifelse(row_number()==1,avg, NA)) %>%
  ungroup() %>% 
  ggplot(aes(x=cool_hh_ele, y=inclev_cal)) + 
  ggridges::geom_density_ridges(scale=0.7, fill="#fb8072", alpha=0.5, color='grey30', size=0.2) +
  geom_text(aes(x= avg +1500, label=label_formatter(avg)),vjust=-1, size=2) +
  annotate("text", x = 1500, y = 4.4, label = "Income", fontface = "plain", size = 3, hjust =0) +
  scale_y_discrete(expand = c(0.02, 0.02)) +
  scale_x_continuous(expand = c(0.02, 0.02), breaks = seq(0,3000,1000),
                     labels = c(0,"",2000,""))+
  labs(x=NULL, y= NULL)+
  theme(axis.text.x = element_text(size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.6), angle = 90, hjust = -0.3),
        plot.margin = margin(t = 6, r = 1, b = 16, l = 1))
p_inc 

p_edu <- read_xlsx("../data/Fig.2d_edu.xlsx") %>%   
  dplyr::filter(!is.na(edu_huzhu)) %>% 
  group_by(edu_huzhu) %>% 
  mutate(avg = round(mean(cool_hh_ele),0),
         avg = ifelse(row_number()==1,avg, NA)) %>%
  ungroup() %>% 
  ggplot(aes(x=cool_hh_ele, y=edu_huzhu)) +  
  ggridges::geom_density_ridges(scale=0.7, fill="#4DBBD5FF", alpha=0.5, color='grey30', size=0.2) +
  geom_text(aes(x= avg +1500, label=label_formatter(avg)),vjust=-1, size=2) +
  annotate("text", x = 1500, y = 3.45, label = "Education", fontface = "plain", size = 3, hjust =0) +
  scale_y_discrete(expand = c(0.02, 0.02)) +
  scale_x_continuous(expand = c(0.02, 0.02), breaks = seq(0,3000,1000),
                     labels = c(0,"",2000,""))+
  labs(x=NULL, y= NULL)+
  theme(axis.text.x = element_text(size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.6), angle = 90, hjust = -0.3),
        plot.margin = margin(t = 6, r = 1, b = 16, l = 1))
p_edu

p_hhstr <- read_xlsx("../data/Fig.2d_composition.xlsx") %>%   
  group_by(str) %>% 
  mutate(avg = round(mean(cool_hh_ele),0),
         avg = ifelse(row_number()==1,avg, NA)) %>%
  ungroup() %>% 
  ggplot(aes(x=cool_hh_ele, y=str)) +  
  ggridges::geom_density_ridges(scale=0.7, fill="#f1a340", alpha=0.5, color='grey30', size=0.2) +
  geom_text(aes(x= avg +1500, label=label_formatter(avg)),vjust=-1, size=2) +
  annotate("text", x = 1500, y = 4.5, label = "Composition", fontface = "plain", size = 3, hjust =0) +
  scale_y_discrete(expand = c(0.02, 0.02)) +
  scale_x_continuous(expand = c(0.02, 0.02), breaks = seq(0,3000,1000),
                     labels = c(0,"",2000,""))+
  labs(x=NULL, y= NULL)+
  theme(axis.text.x = element_text(size = rel(0.7)),
        axis.text.y = element_text(size = rel(0.6), angle = 90, hjust = -0.3),
        plot.margin = margin(t = 6, r = 1, b = 16, l = 1))
p_hhstr

p_row_raw <- plot_grid(
  p_inc, p_edu, p_hhstr,
  ncol = 3, nrow = 1, align = "hv", axis = "tb"
)
p_hetero <- ggdraw(p_row_raw) +
  draw_label(
    "Household annual cooling consumption (kWh)",
    x = 0.5, y = 0.04, hjust = 0.5, vjust = 0,
    size = 8
  ) +
  theme(plot.margin = margin(t = 0, r = 8, b = 0, l = 10, unit = "pt"))
p_hetero 


timeHM_formatter <- function(x){
  h <- floor(x/4)
  m <- round(15*(x %% 4))
  lab <- sprintf("%02d:%02d",h,m)
  return(lab)
}
p_time <- read_xlsx("../data/Fig.2e.xlsx") %>% 
  ggplot()+
  geom_line(aes(x=bin, y=ele), size =1.2, color="#80b1d3")+
  geom_line(aes(x=bin, y=rescale(temp,c(0.04, 0.06))),
            color="#fb8072", size=0.5, linetype="longdash", alpha=0.7)+
  scale_y_continuous(
    expand = c(0,0), limits = c(0,0.062), breaks = seq(0,0.06,0.02),
    sec.axis = sec_axis(~rescale(., c(0,30)),name = "Temperature (℃)")
  )+
  scale_x_continuous(breaks = seq(0,96,32),expand = c(0.02,0.02),
                     labels = timeHM_formatter)+
  labs(x=NULL, y="15 min cooling consumption (kWh/HH)")+
  theme(axis.text = element_text(size=rel(0.7)),
        axis.title = element_text(size=rel(0.7)))
p_time


# stat.test <- read_xlsx("../data/Fig.2f.xlsx") %>% 
#   group_by(type) %>% 
#   rstatix::t_test(add_ele_wh ~ group) %>%    
#   rstatix::add_significance("p.adj")  %>%   
#   rstatix::add_xy_position(x="type",scales = 'free',fun = 'max') %>% 
#   mutate(y.position = y.position/10,
#          y.position = c(5,5.4,5.8,3,3.4,3.8),
#          p.adj.signif = ifelse(p.adj.signif=='ns',"", p.adj.signif),
#          label = paste0(round(p.adj, 3), p.adj.signif)) 

p_ttest <- read_xlsx("../data/Fig.2f.xlsx") %>% 
  ggplot(aes(x=type,y=mean)) + 
  geom_col(aes(fill=group), position = position_dodge(width=0.8))+
  geom_errorbar(aes(ymax = mean + 1.96*se, ymin = mean - 1.96*se, fill=group),
                position = position_dodge(width=0.8),
                width=0.3, size=0.4, color="grey50")+
  #stat_pvalue_manual(stat.test, label = "label",label.size = 2)+
  scale_y_continuous(expand = c(0,0), limits = c(0,6.1), breaks = seq(0,6,2)) +
  labs(x=NULL, y="Daily cooling consumption (kWh/HH)", fill=NULL)+
  scale_fill_manual(
    values = c('g0' = '#fed9a6','g1'='#decbe4','g2'='#b3cde3'),
    labels = c("g0", "g1", "g2")
  )+
  theme(axis.text = element_text(size=rel(0.7)),
        axis.title = element_text(size=rel(0.7)),
        legend.position = c(0.9,0.85),
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.y = unit(0.1, "cm"),
        #legend.text = ggtext::element_markdown(size = rel(0.6))
        )
p_ttest

#combine
row1 <- plot_grid(
  p_month, p_annual,
  ncol = 2, rel_widths = c(1, 1)
)
row2 <- plot_grid(
  p_prov, p_hetero,
  ncol = 2, rel_widths = c(1, 2)
)
row3 <- plot_grid(
  p_time, p_ttest, 
  ncol = 2, rel_widths = c(1, 1)
)
p_row <- plot_grid(
  row1, row2, row3,
  ncol = 1, rel_heights = c(1, 1, 1)
)
p_final <- ggdraw(p_row) +
  draw_plot_label(
    label = letters[1:6],
    x = c(0.01, 0.51,  0.01, 0.35,  0.01, 0.51),
    y = c(0.996, 0.996,  0.67, 0.67,  0.34, 0.34),
    size = 11, fontface = "bold",
    hjust = 0, vjust = 1
  )
p_final

ggsave("../results/Fig.2.pdf", width = 8,height = 8,dpi = 600,device = cairo_pdf)



#Fig.3-----------
timeHM_formatter <- function(x){
  h <- floor(x/4)
  m <- round(15*(x %% 4))
  lab <- sprintf("%02d:%02d",h,m)
  return(lab)
}

p_g1 <- read_xlsx("../data/Fig.3ab.xlsx") %>%
  ggplot(aes(x=bin)) +
  geom_line(aes(y=coef),color="grey60",linetype="longdash",size=0.5)+
  geom_ribbon(aes(ymin=LB,ymax=UB),alpha=0.1,fill='grey30')+
  geom_hline(yintercept = 0,color="grey50",size=0.5,
             linetype="dashed",alpha=0.7) +
  geom_hline(yintercept = mean(read_xlsx("../data/Fig.3ab.xlsx")$coef), 
             color="grey80",size=0.5, alpha=0.7) +
  annotate("text", x = 45, y = 0.33,
           label = "Knowledgeable information (g1)", size = 3, fontface = "plain") +
  annotate("text",x=21,y=-0.3,label="ATE: -1.6%\n (95% CI= -2.8%, -0.4%)",size=2) +
  labs(x=NULL, y='Treatment effects')+
  scale_x_continuous(expand = c(0.02,0.02), limits = c(0,97),
                     breaks = seq(0,96,32), labels = timeHM_formatter) +
  #scale_y_percent(expand = c(0, 0),limits = c(-0.7,0.4), breaks = seq(-0.5,0.25,0.25))+
  theme(axis.title = element_text(size = rel(0.7)),
        axis.text = element_text(size = rel(0.7)))
p_g1

p_g2 <- read_xlsx("../data/Fig.3ab.xlsx") %>%
  ggplot(aes(x=bin)) +
  geom_line(aes(y=coef2),color="#7fc97f",size=0.6)+      
  geom_ribbon(aes(ymin=LB2, ymax=UB2),alpha=0.3,fill='#74a9cf')+
  geom_hline(yintercept = 0,color="grey50",size=0.5,
             linetype="dashed",alpha=0.7) +
  geom_hline(yintercept = mean(read_xlsx("../data/Fig.3ab.xlsx")$coef2), 
             color="#7fc97f",size=0.5, alpha=0.7) +
  annotate("text", x = 33, y = 0.33,
           label = "Specific guidance (g2)", size = 3, fontface = "plain") +
  annotate("text",x=23,y=-0.3,label="ATE: -13.7%\n (95% CI= -15.0%, -12.5%)",size=2) +
  labs(x=NULL, y='Treatment effects')+
  scale_x_continuous(expand = c(0.02,0.02), limits = c(0,97),
                     breaks = seq(0,96,32), labels = timeHM_formatter) +
  #scale_y_percent(expand = c(0, 0),limits = c(-0.7,0.4), breaks = seq(-0.5,0.25,0.25))+
  theme(axis.title = element_text(size = rel(0.7)),
        axis.text = element_text(size = rel(0.7))) 
p_g2


p_g3 <- read_xlsx("../data/Fig.3c.xlsx") %>% 
  ggplot(aes(difft_bin, y=estimate))+
  geom_hline(yintercept = 0, linetype="dashed", colour="grey50", size=0.5, alpha=0.7) +
  geom_vline(xintercept = 0, linetype="dashed", colour="grey50", size=0.5, alpha=0.7)+
  geom_errorbar(aes(ymin=lb, ymax=ub), width=2, size=0.4, color="grey50") +
  geom_point(shape=22, size = 3, stroke = 0.2, colour = "grey50", fill="#af8dc3",alpha=0.8) +
  scale_x_continuous(expand = c(0.02,0.02), limits=c(-51,44),breaks = scales::pretty_breaks(n = 5)) +
  #scale_y_percent(expand = c(0,0), limits = c(-0.5,0.5), breaks = scales::pretty_breaks(n = 5)) +
  labs(x="Days relative to intervention", y="Treatment effects") +
  theme(axis.title = element_text(size = rel(0.7)),
        axis.text = element_text(size = rel(0.7))) 
p_g3


p_hours <- read_xlsx("../data/Fig.3d.xlsx") %>% 
  ggplot(aes(x=group, y=estimate)) +
  geom_hline(yintercept = 0, linetype="dashed", colour="grey50", size=0.5, alpha=0.7) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), 
                width=0.1, size=0.5, color="grey50") +
  geom_point(aes(fill=group), shape=22, size = 4, stroke = 0.2, colour = "grey80") +
  geom_text(aes(label = lab), hjust = -0.4, size= 2.5) +
  annotate("text", x = 1, y = 0.03, label = "Operating hours",    
           fontface = "plain", size = 3) +
  coord_cartesian(clip = "off") +
  # scale_x_discrete(labels=c("g1×post" = "g<sub>1</sub>×post",
  #                          "g2×post" = "g<sub>2</sub>×post")) +
  #scale_y_percent(expand = c(0.005,0.005),limits = c(-0.035,0.03), 
  #                   breaks = seq(-0.03,0.03,0.02)) +
  scale_fill_manual(values = c( "g1×post" = "#f1a340",
                                "g2×post" = "#4DBBD5FF"), 
                    guide = "none") +
  labs(x="Information intervention groups", y="Mediated effects") +
  theme(axis.title = element_text(size = rel(0.7)),
        # axis.text.x = ggtext::element_markdown(size = rel(0.7)),  
        axis.text.y = element_text(size = rel(0.7)))
p_hours


p_thresh <- read_xlsx("../data/Fig.3e.xlsx") %>% 
  ggplot(aes(x=group, y=estimate)) +
  geom_hline(yintercept = 0, linetype="dashed", colour="grey50", size=0.5, alpha=0.7) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), 
                width=0.1, size=0.5, color="grey50") +
  geom_point(aes(fill=group), shape=22, size = 4, stroke = 0.2, colour = "grey80") +
  geom_text(aes(label = lab), hjust = -0.3, size= 2.5) +
  annotate("text", x = 1.15, y = 0.01, label = "Temperature threshold", 
           fontface = "plain", size = 3) +
  coord_cartesian(clip = "off") +
  # scale_x_discrete(labels=c("g1×post" = "g<sub>1</sub>×post",
  #                          "g2×post" = "g<sub>2</sub>×post")) +
  #scale_y_percent(expand = c(0.003,0.003),limits = c(-0.02,0.01), 
  #                breaks = seq(-0.02,0.01,0.01)) +
  scale_fill_manual(values = c( "g1×post" = "#f1a340",
                                "g2×post" = "#4DBBD5FF"), 
                    guide = "none") +
  labs(x="Information intervention groups", y="Mediated effects") +
  theme(axis.title = element_text(size = rel(0.7)),
        # axis.text.x = ggtext::element_markdown(size = rel(0.7)),  
        axis.text.y = element_text(size = rel(0.7)))
p_thresh


p_cycles <- read_xlsx("../data/Fig.3f.xlsx") %>% 
  ggplot(aes(x=group, y=estimate)) +
  geom_hline(yintercept = 0, linetype="dashed", colour="grey50", size=0.5, alpha=0.7) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), 
                width=0.1, size=0.5, color="grey50") +
  geom_point(aes(fill=group), shape=22, size = 4, stroke = 0.2, colour = "grey80") +
  geom_text(aes(label = lab), hjust = -0.3, size= 2.5) +
  annotate("text", x = 1, y = 0.01, label = "AC on/off cycles", 
           fontface = "plain", size = 3) +
  coord_cartesian(clip = "off") +
  # scale_x_discrete(labels=c("g1×post" = "g<sub>1</sub>×post",
  #                          "g2×post" = "g<sub>2</sub>×post")) +
  #scale_y_percent(expand = c(0.003,0.003),limits = c(-0.02,0.01), 
  #                breaks = seq(-0.02,0.01,0.01)) +
  scale_fill_manual(values = c( "g1×post" = "#f1a340",
                                "g2×post" = "#4DBBD5FF"), 
                    guide = "none") +
  labs(x="Information intervention groups", y="Mediated effects") +
  theme(axis.title = element_text(size = rel(0.7)),
        # axis.text.x = ggtext::element_markdown(size = rel(0.7)),  
        axis.text.y = element_text(size = rel(0.7)))
p_cycles


#combine
p_row <- plot_grid(
  p_g1, p_g2, p_g3, p_hours, p_thresh, p_cycles, 
  ncol = 3, nrow = 2
)
p_final <- ggdraw(p_row) +   
  draw_plot_label(
    label = letters[1:6],
    x = c(0.01, 0.343, 0.676, 0.01, 0.343, 0.676),
    y = c(0.99, 0.99, 0.99, 0.49, 0.49, 0.49),
    size = 10, fontface = "bold",
    hjust = 0, vjust = 1
  )
p_final

ggsave("../results/Fig.3.pdf", width = 8,height = 8,dpi = 600,device = cairo_pdf)



#Fig.4-----------
heterodf <- read_xlsx("../data/Fig.4.xlsx")

cols <- c(
  'income' = "#fb8072",
  'education' = "#4DBBD5FF",
  'hh_structure' = "#f1a340",
  'hh_size' = "#af8dc3"
)

panel_plot <- function(dat, tp, label){
  dat %>% 
    dplyr::filter(type == tp) %>% 
    ggplot(aes(x=var2, y=coef))+
    geom_jitter(color = cols[[tp]], alpha=0.2, width = 0.2) +
    geom_boxplot(fill = cols[[tp]], outlier.shape = NA, width=0.4, size = 0.4) +
    geom_hline(yintercept = 0,color="grey50",size=0.5, linetype="dashed",alpha=0.7) +
    annotate("text", x = 0.5, y = 1, label = label, fontface = "plain", size = 4, hjust =0) +
    coord_cartesian(clip = "off") +
    scale_y_continuous(expand = c(0,0), limits=c(-1.2,1.2), 
                       breaks = c(seq(-0.8, 0.8, 0.4)),
                       label = c('-80%','-40%','0','40%','80%')) +
    scale_fill_manual(values = cols, drop = FALSE) +
    scale_color_manual(values = col, drop = FALSE) +
    labs(x=NULL, y = "Change in AC electricity use (%)") +
    theme(axis.title = element_text(size = rel(0.7)),
          axis.text = element_text(size = rel(0.7))) 
}

p_income <- panel_plot(heterodf, "income", "Income")
p_education <- panel_plot(heterodf, "education", "Education")
p_hhstruc <- panel_plot(heterodf, "hh_structure", "Composition")
p_hhsize <- panel_plot(heterodf, "hh_size", "HH size")

#combine
p_row <- plot_grid(
  p_income, p_education, p_hhstruc, p_hhsize,
  ncol = 2, nrow = 2
)
p_final <- ggdraw(p_row) +   
  draw_plot_label(
    label = letters[1:4],
    x = c(0.01, 0.51, 0.01,0.51),
    y = c(0.99, 0.99, 0.49, 0.49),
    size = 11, fontface = "bold",
    hjust = 0, vjust = 1
  )
p_final

ggsave("../results/Fig.4.pdf", width = 8,height = 8,dpi = 600,device = cairo_pdf)



#Fig.5-----------
scen_cols   <- c("SSP1 RCP2.6" = "#54A24B",   
                 "SSP2 RCP6.0 (BAU)" = "#fb8072",  
                 "SSP5 RCP8.5" = "#4C78A8")   
p_annual_ele <- read_xlsx("../data/Fig.5a-fut.xlsx") %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = ELE_billion_LB, ymax = ELE_billion_UB, fill = scenario),
              alpha = 0.2, colour = NA) +
  geom_line(aes(y = ELE_billion, colour = scenario, group = scenario),
            linetype= "longdash", size = 0.5) +
  geom_point(aes(y = ELE_billion, colour = scenario),
             shape = 1, stroke = 0.9, size = 2) +
  geom_line(data = read_xlsx("../data/Fig.5a-his.xlsx"), aes(y = ELE_billion, group = 1),
            colour = "grey30", size = 0.5) +
  geom_point(data = read_xlsx("../data/Fig.5a-his.xlsx"), aes(y = ELE_billion),
             colour = "grey30", size = 2) +
  scale_color_manual(values = scen_cols, name = "Scenario") +
  scale_fill_manual(values = scen_cols, guide = "none") +
  scale_y_continuous(expand = c(0,0), limits = c(0,620), breaks = seq(200,600,200))+
  labs(x = NULL, y = "Cooling electricity demand (TWh)") +
  theme(axis.title = element_text(size = rel(0.7)),
        axis.text = element_text(size = rel(0.7)),
        legend.position    = c(0.21, 0.8),  
        legend.box         = "horizontal",
        legend.key.size = unit(0.3, "cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.text = element_text(size = rel(0.7)),
        legend.title = element_text(size = rel(0.7)))
p_annual_ele

ggsave("../results/Fig.5a.pdf", width = 8,height = 8,dpi = 600,device = cairo_pdf)


scen_cols2 <- c(
  'BAU' = "#fb8072",
  'FD' = "#4DBBD5FF",
  'MD' = "#f1a340",
  'ND' = "#af8dc3"
)
p_conserv <- read_xlsx("../data/Fig.5d.xlsx") %>% 
  ggplot(aes(x = year, y = MEAN)) +
  geom_errorbar(aes(ymin = LB, ymax = UB, group = scenario),
                position = position_dodge(width = 0.7), 
                width=0.3, size=0.4, color="grey40") +
  geom_col(aes(fill = scenario, color = scenario, group = scenario),
           position = position_dodge(width = 0.7), width = 0.7, alpha = 0.5)+
  scale_fill_manual(values = c(BAU=scen_cols2[["BAU"]], FD="transparent",
                               MD="transparent",ND="transparent")) +
  scale_color_manual(values = scen_cols2, name = "Conservation scenario") +
  scale_y_continuous(expand = c(0,0), limits = c(0,620), breaks = seq(200,600,200))+
  scale_x_discrete(expand = c(0.2, 0.2)) +
  labs(x = NULL, y = "Cooling electricity demand (TWh)") +
  guides(
    fill = "none", 
    color = guide_legend(nrow = 2, byrow = TRUE,
                         override.aes = list(fill=c(scen_cols2["BAU"],
                                                    rep("transparent",3)),
                                             linewidth =0.5))
  ) +
  theme(axis.title = element_text(size = rel(0.7)),
        axis.text = element_text(size = rel(0.7)),
        legend.position    = c(0.22, 0.84),  
        legend.box         = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key.size = unit(0.3, "cm"),
        legend.key.height = unit(0.5,"cm"),
        legend.title = element_text(size = rel(0.7)),
        legend.text = element_text(size = rel(0.7)))
p_conserv

ggsave("../results/Fig.5d.pdf", width = 8,height = 8,dpi = 600,device = cairo_pdf)



# ## Regression replicate code------
# # The raw data for household AC electricity consumption from RCT trial is not available. While code availability is below:
# 
# ## Average treatment effects
# fe_hwh_1 <- feols(add_ele_wh ~ did01+did02,       
#                   data = regdata_inter, vcov = ~HHID)
# 
# fe_hwh_2 <- feols(add_ele_wh ~ did01+did02+t+
#                     weekday+stageh+month | HHID,       
#                   data = regdata_inter, vcov = ~HHID)
# 
# fe_hwh_3 <- feols(add_ele_wh ~ did01+did02+t+TEMP+PRCP+WDSP+
#                     weekday+stageh+month | HHID,       
#                   data = regdata_inter, vcov = ~HHID)
# 
# fe_hwh4 <- feols(add_ele_wh ~ did01+did02+t+TEMP+PRCP+WDSP+g2+g3+
#                    inc_lev2+inc_lev3+inc_lev4+ eduISCED3+eduISCED4_8+
#                    child+old+child_old+
#                    weekday+stageh+month | city,       
#                  data = regdata_inter, vcov = ~HHID)
# 
# fe_hwh5 <- feols(add_ele_wh ~ did01+did02+t+TEMP+PRCP+WDSP+g2+g3+
#                    inc_lev2+inc_lev3+inc_lev4+ eduISCED3+eduISCED4_8+
#                    child+old+child_old+  hhsize+area+houseAge+
#                    weekday+stageh+month | city,
#                  data = regdata_inter, vcov = ~HHID) 
# 
# #hourly
# reghour <- regdata_inter %>% 
#   group_by(HHID,date,stageh) %>% 
#   mutate(add_ele_wh = sum(add_ele_wh)) %>%ungroup() %>% 
#   distinct(HHID,date,stageh, .keep_all = TRUE) 
# #daily
# regdaily <- regdata_inter %>% 
#   group_by(HHID,date) %>% 
#   mutate(add_ele_wh = sum(add_ele_wh),
#          TEMP = mean(TEMP),PRCP = mean(PRCP),WDSP=mean(WDSP)) %>% ungroup() %>%
#   distinct(HHID,date, .keep_all = TRUE)
# #in Wh
# regdata_wh <- regdata_inter %>% 
#   mutate(add_ele_wh2 = add_ele_wh*1000)
# 
# 
# fe_cwh_h <- feols(add_ele_wh ~ did01+did02+t+TEMP+PRCP+WDSP+g2+g3+
#                     inc_lev2+inc_lev3+inc_lev4+ eduISCED3+eduISCED4_8+
#                     child+old+child_old+ hhsize+area+houseAge+ 
#                     weekday+month  | city,     
#                   data = reghour, vcov = ~HHID)
# fe_cwh_d <- feols(add_ele_wh ~ did01+did02+t+TEMP+PRCP+WDSP+g2+g3+
#                     inc_lev2+inc_lev3+inc_lev4+ eduISCED3+eduISCED4_8+
#                     child+old+child_old+ hhsize+area+houseAge+ 
#                     weekday+month  | city,     
#                   data = regdaily, vcov = ~HHID)
# fe_wh <- feols(add_ele_wh2 ~ did01+did02+t+TEMP+PRCP+WDSP+g2+g3+
#                  inc_lev2+inc_lev3+inc_lev4+ eduISCED3+eduISCED4_8+
#                  child+old+child_old+  hhsize+area+houseAge+
#                  weekday+stageh+month | city,
#                data = regdata_wh, vcov = ~HHID) 
# 
# 
# modelsummary(list(fe_hwh_1,fe_hwh_2,fe_hwh_3,fe_hwh4,fe_hwh5,
#                   fe_cwh_h,fe_cwh_d,fe_wh),
#              coef_omit = "weekend[1]|weekday[2-7]|stageh[1-9][0-9]?|month[1-9][0-9]?",
#              output = "SuppTable8.docx", stars = TRUE)
# 
# 
# 
# ## Dynamic effects
# ptdf <- regdata_inter %>% 
#   group_by(HHID, date) %>%                 #sum to daily level
#   mutate(add_ele_wh = sum(add_ele_wh)) %>% ungroup() %>% 
#   select(-time) %>% distinct(HHID, date, .keep_all = TRUE) %>% 
#   mutate(difft = as.integer(difftime(date,"2021-09-15",units = "days")),
#          difft_bin = 7 * floor(difft / 7),
#          post = as.integer(difft >=0)) %>% 
#   dplyr::filter(difft > (-50))             
# 
# 
# fe_bin <- feols(add_ele_wh ~ i(difft_bin, g2, ref=-7) + TEMP+PRCP+WDSP +inc_level2+inc_level3+inc_level4 + eduISCED3+eduISCED4_8 + child+old+child_old + hhsize+area+houseAge | city+ date,
#                 data = ptdf, vcov = ~HHID)
# iplot(fe_bin, ref.line = 0)
# 
# 
# 
# ## Mediation effects
# fe_cwh_d <- feols(add_ele_wh ~ did01+did02 + t+TEMP+PRCP+WDSP+g2+g3+
#                     inc_level2+inc_level3+inc_level4+ eduISCED3+eduISCED4_8+
#                     child+old+child_old+ hhsize+area+houseAge+ 
#                     weekday+month  | city,    
#                   data = mediation, vcov = ~HHID)
# omit_vars <- paste0("^(","TEMP|PRCP|WDSP|","g2|g3|","inc_level2|inc_level3|inc_level4|","eduISCED3|eduISCED4_8|", "child|old|child_old|","hhsize|area|houseAge|","weekday[2-7]|","month([1-9]|1[0-2])",")$") 
# 
# #(1) operating hours
# fe_hour1_1 <- feols(ac_use_hours ~ did01+did02 + t+TEMP+PRCP+WDSP+
#                       weekday+month  | HHID,   
#                     data = mediation_cycle, vcov = ~HHID)
# fe_hour1_2 <- feols(add_ele_wh ~ did01+did02+ac_use_hours + t+TEMP+PRCP+WDSP+
#                       weekday+month  | HHID,   
#                     data = mediation_cycle, vcov = ~HHID)
# 
# #(2) temperature threshold
# fe_thresh_1 <- feols(thresh ~ did01+did02 + t+TEMP+PRCP+WDSP+g2+g3+
#                        inc_level2+inc_level3+inc_level4+ eduISCED3+eduISCED4_8+
#                        child+old+child_old+ hhsize+area+houseAge+ 
#                        weekday+month  | city,   
#                      data = mediation, vcov = ~HHID)
# fe_thresh_2 <- feols(add_ele_wh ~ did01+did02+thresh + t+TEMP+PRCP+WDSP+g2+g3+
#                        inc_level2+inc_level3+inc_level4+ eduISCED3+eduISCED4_8+
#                        child+old+child_old+ hhsize+area+houseAge+ 
#                        weekday+month  | city,   
#                      data = mediation, vcov = ~HHID)
# 
# #(3) on/off cycling
# fe_cycle_1 <- feols(ac_cycles ~ did01+did02 +t+ TEMP+PRCP+WDSP+
#                       weekday+month  | HHID,   
#                     data = mediation_cycle, vcov = ~HHID)  
# fe_cycle_2 <- feols(add_ele_wh ~ did01+did02+ac_cycles + t+TEMP+PRCP+WDSP+
#                       weekday+month  | HHID,   
#                     data = mediation_cycle, vcov = ~HHID)
# 
# modelsummary(list(fe_cwh_d, fe_hour1_1, fe_hour1_2, fe_thresh_1, fe_thresh_2, fe_cycle_1, fe_cycle_2),
#              coef_omit = omit_vars,
#              output = "SuppTable9.docx", stars = TRUE)
# 
# 
# 
# ## Heterogeneous treatment effects
# fe_cwh <- feols(add_ele_wh ~ did01+did02+t+TEMP+PRCP+WDSP+g2+g3+
#                   inc_lev2+inc_lev3+inc_lev4+ eduISCED3+eduISCED4_8+
#                   child+old+child_old+ hhsize_lev2+hhsize_lev3p +
#                   area+houseAge+
#                   weekday+stageh+month  | city,    
#                 data = regdata_inter, vcov = ~HHID) 
# 
# fe_inter1 <- feols(add_ele_wh ~ did01+did02+t+TEMP+PRCP+WDSP+g2+g3+
#                      inc_lev2+inc_lev3+inc_lev4+ eduISCED3+eduISCED4_8+
#                      child+old+child_old+ hhsize_lev2+hhsize_lev3p +
#                      area+houseAge + 
#                      did02:inc_lev2 + did02:inc_lev3 + did02:inc_lev4 +
#                      weekday+stageh+month  | city, 
#                    data = regdata_inter, vcov = ~HHID) 
# 
# fe_inter2 <- feols(add_ele_wh ~ did01+did02+t+TEMP+PRCP+WDSP+g2+g3+
#                      inc_lev2+inc_lev3+inc_lev4+ eduISCED3+eduISCED4_8+
#                      child+old+child_old+ hhsize_lev2+hhsize_lev3p +
#                      area+houseAge + 
#                      did02:inc_lev2 + did02:inc_lev3 + did02:inc_lev4 +
#                      did02:eduISCED3 + did02:eduISCED4_8 +
#                      weekday+stageh+month  | city, 
#                    data = regdata_inter, vcov = ~HHID) 
# 
# fe_inter3 <- feols(add_ele_wh ~ did01+did02+t+TEMP+PRCP+WDSP+g2+g3+
#                      inc_lev2+inc_lev3+inc_lev4+ eduISCED3+eduISCED4_8+
#                      child+old+child_old+ hhsize_lev2+hhsize_lev3p +
#                      area+houseAge + 
#                      did02:inc_lev2 + did02:inc_lev3 + did02:inc_lev4 +
#                      did02:eduISCED3 + did02:eduISCED4_8 +
#                      did02:child + did02:old + did02:child_old +
#                      weekday+stageh+month  | city, 
#                    data = regdata_inter, vcov = ~HHID) 
# 
# fe_inter4 <- feols(add_ele_wh ~ did01+did02+t+TEMP+PRCP+WDSP+g2+g3+
#                      inc_lev2+inc_lev3+inc_lev4+ eduISCED3+eduISCED4_8+
#                      child+old+child_old+ hhsize_lev2+hhsize_lev3p +
#                      area+houseAge + 
#                      did02:inc_lev2 + did02:inc_lev3 + did02:inc_lev4 +
#                      did02:eduISCED3 + did02:eduISCED4_8 +
#                      did02:child + did02:old + did02:child_old +
#                      did02:hhsize_lev2 + did02:hhsize_lev3p +
#                      weekday+stageh+month  | city, 
#                    data = regdata_inter, vcov = ~HHID) 
# 
# keepvar <- c("did01", "did02",
#              "did02:inc_lev2","did02:inc_lev3","did02:inc_lev4",
#              "did02:eduISCED3","did02:eduISCED4_8",
#              "did02:child","did02:old","did02:child_old",
#              "did02:hhsize_lev2", "did02:hhsize_lev3p")
# dispvar <- c("g1×post","g2×post",
#              "g2×post×inc_middle","g2×post×inc_middle-high","g2×post×inc_high",
#              "g2×post×edu_secondary","g2×post×edu_higher",
#              "g2×post×child","g2×post×elderly","g2×post×child_elderly",
#              "g2×post×hhsize_middle", "g2×post×hhsize_high")
# coef_map <- setNames(dispvar, keepvar)
# 
# modelsummary(list("City_FE_X" = fe_cwh,
#                   "City_FE_inter1" = fe_inter1,
#                   "City_FE_inter2" = fe_inter2,
#                   "City_FE_inter3" = fe_inter3,
#                   "City_FE_inter4" = fe_inter4), 
#              coef_map = coef_map,
#              output = "SuppTable10.docx",
#              stars = TRUE)

