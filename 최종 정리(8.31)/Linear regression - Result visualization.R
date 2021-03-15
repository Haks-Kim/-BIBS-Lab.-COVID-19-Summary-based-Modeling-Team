#rm(list=ls())
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(ggrepel)

# Path
path <- "C:/Users/김학용/Desktop/코로나 연구 인턴/주차별 작업사항/최종 정리(8.31)/"
data_path <- paste0(path, "data/")
result_path <- paste0(path, "result/")
plot_path <- paste0(path, "plot/SLR/")

#### Bar Plot pf p_value ####
param_name = c("a", "b", "c")
model_name = c("Logistic", "Gompertz")
title_box = c(
  expression("P-Value of "~theta[1]~" of"~bold(alpha) ~"~ National Factors(Logistic) "),
  expression("P-Value of "~theta[1]~" of"~bold(beta) ~"~ National Factors(Logistic) "),
  expression("P-Value of "~theta[1]~" of"~bold(gamma) ~"~ National Factors(Logistic) "),
  expression("P-Value of "~theta[1]~" of"~bold(alpha) ~"~ National Factors(Gompertz) "),
  expression("P-Value of "~theta[1]~" of"~bold(beta) ~"~ National Factors(Gompertz) "),
  expression("P-Value of "~theta[1]~" of"~bold(gamma) ~"~ National Factors(Gompertz) ")
)

for (i in 1:3){
  for(j in 1:2){
  ii = i+3 + ifelse(j==2,3,0)
  jj = j
  if(j==1){
  df_model <- data.frame(
    National_Factors = factor(unique(df_sum1$Explanatory)),
    coefficient = c(df_sum1[,ii][which((1:nrow(df_sum1))%%3==1)], df_sum2[,ii][which((1:nrow(df_sum2))%%3==1)]),
    p.value_scaled = -log(c(df_sum1[,ii][which((1:nrow(df_sum1))%%3==0)], df_sum2[,ii][which((1:nrow(df_sum2))%%3==0)])),
    segment = factor(rep(1:2, each=length(unique(df_sum1$Explanatory))))
    # category = factor(c(7,7,1,1,7,5,4,4,7,3,3,6,5,5,2,1,1,1,2,2,2,6,7,7,7,7,7,7,7,2,5))
    # Territory&population : 1 / Age : 2 / Trade : 3 / Environment : 4 / 
    # Education&Culture&Science : 5 / National Accounts : 6 / 
    # Health&Sociecy&Welfare : 7
  ) %>% arrange(National_Factors) 
  }else{
    df_model <- data.frame(
      National_Factors = factor(unique(df_sum1$Explanatory)),
      coefficient = c(df_sum1[,ii][which((1:nrow(df_sum1))%%3==1)], df_sum2[,ii][which((1:nrow(df_sum2))%%3==1)]),
      p.value_scaled = -log(c(df_sum1[,ii][which((1:nrow(df_sum1))%%3==0)], df_sum2[,ii][which((1:nrow(df_sum2))%%3==0)])),
      segment = factor(rep(1:2, each=length(unique(df_sum1$Explanatory))))
      # category = factor(c(7,7,1,1,7,5,4,4,7,3,3,6,5,5,2,1,1,1,2,2,2,1,7,7,7,7,7,7,7,2,5))
    ) %>% arrange(National_Factors,segment)
  }

  #bar plot
  bar_plot = ggplot(df_model %>%
                   mutate(National_Factors = fct_reorder(National_Factors,desc(National_Factors))),
                   aes(x=National_Factors, y=p.value_scaled, fill=fct_rev(segment))) +
    geom_bar(stat="identity", position="dodge") +
    labs(fill="Segment") +
    guides(fill = guide_legend(reverse=TRUE))+
    geom_hline(yintercept=-log(0.05), color = 2, size=0.5, linetype=8) +
    labs(title=title_box[ii-3],subtitle = "", y="-log(p.value)" ) +
    coord_flip() +
    scale_fill_manual(values=c("#56B4E9","#E69F00")) +
    theme_classic() +
    theme(plot.title=element_text(size=15, hjust=0.5, face='bold.italic',vjust=2),
          legend.title = element_text(face = "bold.italic"),
          legend.text = element_text(face = "italic"))
  

    ggsave(bar_plot, filename = paste0(model_name[jj],"_",param_name[i], ".jpeg"), path = paste0(plot_path,"barplot_p.value/"), dpi = 600, width = 8, height = 8.5)
  }
}


#### Bar Plot pf coefficient ####
param_name = c("a","b","c")
model_name = c("Logistic", "Gompertz")
seg_name =  c("Segment 1", "Segment 2")
title_box = c(
  expression(theta[1]~" of"~bold(alpha) ~"~ National Factors(Segment1, Logistic) "),
  expression(theta[1]~" of"~bold(alpha) ~"~ National Factors(Segment1, Gompertz) "),
  expression(theta[1]~" of"~bold(alpha) ~"~ National Factors(Segment2, Logistic) "),
  expression(theta[1]~" of"~bold(alpha) ~"~ National Factors(Segment2, Gompertz) "),
  expression(theta[1]~" of"~bold(beta) ~"~ National Factors(Segment1, Logistic) "),
  expression(theta[1]~" of"~bold(beta) ~"~ National Factors(Segment1, Gompertz) "),
  expression(theta[1]~" of"~bold(beta) ~"~ National Factors(Segment2, Logistic) "),
  expression(theta[1]~" of"~bold(beta) ~"~ National Factors(Segment2, Gompertz) "),
  expression(theta[1]~" of"~bold(gamma) ~"~ National Factors(Segment1, Logistic) "),
  expression(theta[1]~" of"~bold(gamma) ~"~ National Factors(Segment1, Gompertz) "),
  expression(theta[1]~" of"~bold(gamma) ~"~ National Factors(Segment2, Logistic) "),
  expression(theta[1]~" of"~bold(gamma) ~"~ National Factors(Segment2, Gompertz) ")
)

loop_count = 0
for (i in 1:3){ # parameter
  for(j in 1:2){ # segment
    for(k in 1:2){ # model
      
      ii = i+3+ifelse(k==2,3,0)
      
      if(j==1){
        df_model <- data.frame(
          National_Factors = factor(unique(df_sum1$Explanatory)),
          coefficient = c(df_sum1[,ii][which((1:nrow(df_sum1))%%3==1)]),
          category = factor(c(7,7,1,1,7,5,4,4,7,3,3,6,5,5,2,1,1,1,2,2,2,1,7,7,7,7,7,7,7,2,5)),
          significant_var = factor( ifelse(as.numeric(df_sum1[,ii][which((1:nrow(df_sum1))%%3==0)]<0.05)+rep(0,31)==1,
                                           "significant","non-significant"))
        ) %>% arrange(National_Factors) 
      }else{
        df_model <- data.frame(
          National_Factors = factor(unique(df_sum1$Explanatory)),
          coefficient = c(df_sum2[,ii][which((1:nrow(df_sum2))%%3==1)]),
          category = factor(c(7,7,1,1,7,5,4,4,7,3,3,6,5,5,2,1,1,1,2,2,2,1,7,7,7,7,7,7,7,2,5)),
          significant_var = factor( ifelse(as.numeric(df_sum2[,ii][which((1:nrow(df_sum2))%%3==0)]<0.05)+rep(0,31)==1,
                                           "significant","non-significant"))
        ) %>% arrange(National_Factors) 
      }
  
      x_max = max(abs(df_model$coefficient))
      loop_count = loop_count + 1
      
      # beta1 of c ~ Time-Independent-Variable
      #bar plot
      bar_plot = ggplot(df_model %>%
                          mutate(National_Factors = fct_reorder(National_Factors, desc(National_Factors))),
                        aes(x=National_Factors, y=coefficient, fill=significant_var)) +
        geom_bar(stat="identity", position="dodge") +
        labs(fill="Significance",face="bold") +
        guides(fill = guide_legend(reverse=TRUE))+
        scale_y_continuous(limits=c(-x_max,x_max),oob = rescale_none)+
        labs(title = title_box[loop_count],subtitle = "") +
        coord_flip() +
        scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
        theme_classic() +
        theme(legend.title=element_text(size=10,face="bold"),plot.title=element_text(size=15, hjust=0.5, face="bold", vjust=2))
      
      
       ggsave(bar_plot, filename = paste0(model_name[k],"_",param_name[i],"_",seg_name[j], ".jpeg"), path = paste0(plot_path,"barplot_coefficient/"), dpi = 600, width = 8, height = 8.5)
    }
  }
}

