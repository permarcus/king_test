#' Plot original data
#' 
#' @param dat a data containing all data of interest
#' 
#' @export
plots_king <- function(dat){
  checkmate::assert_list(dat)
  
  data_set <- dplyr::bind_rows(dat$train, dat$test)
  
  # Table distribution of conversion each data set
  data_conv <- dplyr::group_by(data_set, playerid)
  data_conv <- dplyr::slice(data_conv, 1)
  data_conv$conversion[data_conv$conversion == 1] <- "Conversion"
  data_conv$conversion[data_conv$conversion == 0] <- "Non Conversion"
  df_conv <- as.data.frame(c(table(data_conv$abtest_group, data_conv$conversion)[1], 
                             table(data_conv$abtest_group, data_conv$conversion)[2]))
  df_non_conv <- as.data.frame(c(table(data_conv$abtest_group, data_conv$conversion)[3], 
                                 table(data_conv$abtest_group, data_conv$conversion)[4]))
  names(df_conv) <- "Value"; names(df_non_conv) <- "Value"
  df_conv$type <- "Conversion"
  df_non_conv$type <- "Non Conversion"
  df <- dplyr::bind_rows(df_conv, df_non_conv)
  df$Group <- c("A", "B", "A", "B")
  
  # Grouped plot for number of conversion in each group
  conversion_in_group_1 <- ggplot(data = df, aes(fill=type, y=Value, x=Group)) + 
                                  geom_bar(position="dodge", stat="identity") +
                                  ggtitle("BarChart: Amount of converions in groups") +
                                  xlab("Amount of conversion in group") + 
                                  ylab("Group") 
  
  # Grouped plot for number of conversion in each group
  conversion_in_group_2 <- ggplot(data = df, aes(fill=type, y=Value, x=Group)) + 
                                  geom_bar(position="fill", stat="identity") + 
                                  ggtitle("BarChart: Proportion of converions in groups") +
                                  xlab("Proportion of conversion in group") + 
                                  ylab("Group") 
  
  # Scatterplot based on Game ends pachases and total amount of purchases
  scatter_total <- ggplot(data_set, aes(x=gameends, y=purchases)) + 
                          geom_point(aes(color = tot_purchases)) + 
                          ggtitle("Game ends, purchases and total amount of purchases for both groups") +
                          xlab("Game ends") + 
                          ylab("Purchases") 
  
  # Plotting same for group a
  data_a <- dplyr::filter(data_set, abtest_group == "A")
  scatter_a <- ggplot(data_a, aes(x=gameends, y=purchases)) + 
                      geom_point(aes(color = tot_purchases)) +
                      ggtitle("Game ends, purchases and total amount of purchases for Group A") +
                      xlab("Game ends") + 
                      ylab("Purchases") 
  
  
  # Plotting same for group b
  data_b <- dplyr::filter(data_set, abtest_group == "B")
  scatter_b <- ggplot(data_b, aes(x=gameends, y=purchases)) + 
                      geom_point(aes(color = tot_purchases)) +
                      ggtitle("Game ends purchases and total amount of purchases for Group B") +
                      xlab("Game ends") + 
                      ylab("Purchases") 
  
  plots <- list()
  plots$conversion_in_group_1 <- conversion_in_group_1
  plots$conversion_in_group_2 <- conversion_in_group_2
  plots$scatter_total <- scatter_total
  plots$scatter_a <- scatter_a  
  plots$scatter_b <- scatter_b  
  
  return(plots)
}