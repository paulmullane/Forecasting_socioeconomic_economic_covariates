#packages----
library(dplyr)
library(tidyr)
library(purrr)
library(tsutils)

# Load and reshape data----
data_dir <- "C:/Users/20229798/OneDrive - University of Limerick/Desktop/Covariate Forecasting/Post Hoc statistical tests"
csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

read_variable <- function(f){
  df <- read.csv(f, stringsAsFactors = FALSE)
  variable_name <- tools::file_path_sans_ext(basename(f))
  df %>%
    mutate(Variable = variable_name) %>%
    select(Variable, Country, Model, Horizon_Full_MAPE)
}

all_data <- map_dfr(csv_files, read_variable)

n_variables <- length(unique(all_data$Variable))
n_countries <- length(unique(all_data$Country))
n_models    <- length(unique(all_data$Model))


#Pooled Friedman test. blocks are variable-country pairs (N=169)----
pooled_wide <- all_data %>%
  mutate(Block = paste(Variable, Country, sep = "__")) %>%
  select(Block, Model, Horizon_Full_MAPE) %>%
  pivot_wider(names_from = Model, values_from = Horizon_Full_MAPE)

pooled_matrix <- as.matrix(pooled_wide %>% select(-Block))
rownames(pooled_matrix) <- pooled_wide$Block

friedman_pooled <- friedman.test(pooled_matrix)

#results of freidman test----
print(friedman_pooled) #significant p-value here says not all models are equally good


cat("\nAverage ranks (pooled):\n")
print(round(sort(nem_pooled$means), 2)) #this ranks the models

cat(sprintf("Critical difference (pooled, alpha = 0.05): %.3f\n", nem_pooled$cd)) #this is the value which shows how much the ranks
                                                                                  #need to differ by to be statistically significant

cat(sprintf("Friedman p-value (via nemenyi, should match friedman.test above): %.3e\n", nem_pooled$fpval))


#making the plot to demonstrate findings----
library(ggplot2)
plot_cd_diagram <- function(avg_ranks, cd, title = "", subtitle = NULL){
  
  ord <- order(avg_ranks)
  ranks_sorted <- avg_ranks[ord]
  models_sorted <- names(avg_ranks)[ord]
  k <- length(ranks_sorted)
  
  #find intervals of consecutive (sorted) models whose max-min rank 
  #difference is <= cd, keeping only maximal ones
  cliques <- list()
  for (i in seq_len(k)) {
    j <- i
    while (j < k && (ranks_sorted[j + 1] - ranks_sorted[i]) <= cd) j <- j + 1
    if (j > i) cliques[[length(cliques) + 1]] <- c(i, j)
  }
  #drop cliques fully contained in another
  keep <- rep(TRUE, length(cliques))
  for (a in seq_along(cliques)) {
    for (b in seq_along(cliques)) {
      if (a != b && cliques[[a]][1] >= cliques[[b]][1] && cliques[[a]][2] <= cliques[[b]][2] &&
          !(cliques[[a]][1] == cliques[[b]][1] && cliques[[a]][2] == cliques[[b]][2])) {
        keep[a] <- FALSE
      }
    }
  }
  cliques <- cliques[keep]
  
  #stagger label height when points are too close together to fit labels
  #side by side without collision
  label_gap_threshold <- diff(range(avg_ranks)) * 0.16
  level <- numeric(k)
  for (i in 2:k) {
    if ((ranks_sorted[i] - ranks_sorted[i - 1]) < label_gap_threshold) {
      level[i] <- level[i - 1] + 1
    } else {
      level[i] <- 0
    }
  }
  
  points_df <- data.frame(
    model = factor(models_sorted, levels = models_sorted),
    rank = ranks_sorted,
    y = 1,
    level = level
  )
  
  clique_df <- do.call(rbind, lapply(seq_along(cliques), function(idx) {
    g <- cliques[[idx]]
    data.frame(
      x = ranks_sorted[g[1]],
      xend = ranks_sorted[g[2]],
      y = 1 - 0.10 * idx,
      yend = 1 - 0.10 * idx
    )
  }))
  
  best_rank <- min(ranks_sorted)
  x_min <- floor(min(ranks_sorted) - 0.3)
  x_max <- ceiling(max(ranks_sorted) + 0.3)
  
  max_level <- max(points_df$level)
  tick_step <- 0.14
  
  ggplot()+geom_hline(yintercept=1, colour="grey40", linewidth=0.4)
  geom_segment(data=points_df, aes(x=rank, xend=rank, y=1, 
                                   yend=1+tick_step*(level+1)), colour="grey50", 
               linewidth=0.4)+
    geom_point(data=points_df, aes(x=rank, y=1, colour=rank==best_rank), size=3)+
    scale_colour_manual(values=c(`TRUE`="#b5461b", `FALSE`="grey30"), guide="none")+
    geom_text(data=points_df, aes(x=rank, y=1+tick_step*(level+1)+0.02,
                                  label=sprintf("%s (%.2f)", model, rank)),
              size=3.4, vjust=0, hjust=0.5, fontface="plain")+
    geom_segment(data=clique_df, aes(x=x, xend=xend, y=y, yend=yend), 
                 colour="#2b6a99", linewidth=2.2, lineend="round")+
    scale_x_continuous(limits=c(x_min, x_max), breaks=seq(ceiling(x_min), 
                                                          floor(x_max), by=1))+
    scale_y_continuous(limits=c(1-0.10*(length(cliques)+1), 
                                1+tick_step*(max_level+2)+0.15))+
    labs(title=title, subtitle=subtitle, x="Average rank", y=NULL,
         caption=sprintf("Critical difference = %.3f  (Nemenyi, \u03b1 = 0.05)", cd))+
    theme_minimal(base_size=12)+
    theme(panel.grid=element_blank(), axis.text.y=element_blank(), 
          axis.ticks.y=element_blank(), 
          axis.title.x=element_text(margin=margin(t=10), colour="grey20"),
          axis.text.x=element_text(colour="grey30"), 
          plot.title=element_text(face="bold", size=13),
          plot.subtitle=element_text(colour="grey40", size=10),
          plot.caption=element_text(colour="grey40", hjust=0, size=9, 
                                    margin=margin(t=8)),
          plot.margin=margin(15, 20, 10, 20))
}

#reproduce pooled result ----
avg_ranks <- c(Theta = 3.24, ETS = 4.14, ARIMA = 4.27, GP = 4.29,
               NNAR = 4.74, GAM = 4.79, LSTM = 5.13, ARFIMA = 5.39)
cd <- 0.808

p <- plot_cd_diagram(avg_ranks, cd)

ggsave("cd_diagram_pooled.pdf", p, width = 8.5, height = 4.2, dpi = 300, bg = "white")
