make_corr_plot <- function(current_df){
  
  ALPHA <- .05
  cols <- rev(colorRampPalette(c("red", "white", "blue"))(100))
  
  clean_df <- current_df %>%
    select_if(is.numeric) 
  
  corr_mat <- cor(clean_df, 
                  use = "pairwise.complete.obs")
  
  p.mat <- corrplot::cor.mtest(clean_df, 
                               conf.level = (1-ALPHA),  
                               use = "pairwise.complete.obs")$p
  
  corrplot::corrplot(corr_mat, method = "color",  col = cols,
                     order = "original", number.cex = .7,
                     addCoef.col = "black", 
                     p.mat = p.mat, sig.level = ALPHA, insig = "blank", 
                     tl.col = "black", tl.srt = 90,
                     diag = FALSE)
  
}