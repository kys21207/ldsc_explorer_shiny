#general_fxns.R
#' Enable table downloads
#' @param data_use A dataframe to display
downloadable_table <- function(data_use) {
  datatable(data = data_use,
            rownames = FALSE,
            filter = list(position = 'top', clear = FALSE),
            selection = 'single',
            extensions = c('Buttons', 'ColReorder', 'RowReorder'),
            options = list(
              scrollX = TRUE,
              dom = 'Bfrtipl',
              pageLength = 10,
              lengthMenu = list(c(10, 25, 50, 100,-1), c(10, 25, 50, 100, "All")),
              info = FALSE,
              colReorder = TRUE,
              rowReorder = TRUE,
              buttons = list(list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf', 'print'),
                text = 'Download')
            )))
}

#' Clean a string typed by user
#' @param caption (character) trait keywords 
text2statment <- function(caption){
  tmp <-as.character(caption)
  tmp <- unlist(strsplit(tmp,","))
  tmp <- gsub("[[:punct:]]|[' ']", "", tmp)
  tmp[tmp==""]<-NA
  text <- na.omit(tmp)
#  sql_statment <- paste0("analysis like '%",tmp,"%'",collapse=" OR ")
  return(text)
}

#' General function to return the classification of traits with average rg
#' @param group (character) MeSH term or disease category to query
#' @param rg (numeric) rg to query
#' @param p (numeric) p-value to query
#' @param dsn (character) DSN to connect to
#' @param db (character) Database to make a gtx connection to (must be gtx compliant)
query_rg_selected_class <- function(group,trait,cate,dsn,db){
  conn <- get_connection(dsn = dsn, db = db)
  if(group=='mesh'){
    showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>MeSH <br>trait: ",trait,"<br> category: ",cate)),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT p1,p2,rg,p,p1_phenotype,p2_phenotype,p2_ratio FROM ",db,".ldsc_rg_gsk
                WHERE p1 = '",trait,"' AND
                      p2_phenotype_vocab_id = '",cate,"'")
    analyses_rg <- dbGetQuery(conn,sql) 
    if(nrow(analyses_rg) == 0){
      analyses_rg <- data.frame(matrix(nrow = 0, ncol = 7)) 
      colnames(analyses_rg) <- c("p1","p2","rg","p","p1_phenotype","p2_phenotype","p2_ratio")
    }else{
      analyses_rg <- analyses_rg %>% arrange(p) 
    }
    removeModal()
  }else if(group=='mesh_label'){
    showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>MeSH label <br>trait: ",trait,"<br> category: ",cate)),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT p1,p2,rg,p,p1_phenotype,p2_phenotype,p2_ratio FROM ",db,".ldsc_rg_gsk
                WHERE p1 = '",trait,"' AND
                      p2_phenotype_vocab_label = '",cate,"'")
    analyses_rg <- dbGetQuery(conn,sql)  
    if(nrow(analyses_rg) == 0){
      analyses_rg <- data.frame(matrix(nrow = 0, ncol = 7)) 
      colnames(analyses_rg) <- c("p1","p2","rg","p","p1_phenotype","p2_phenotype","p2_ratio")
    }else{
      analyses_rg <- analyses_rg %>% arrange(p) 
    }
    removeModal()
  }else if(group=='disease'){
    showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>disease_category <br>trait: ",trait,"<br> category: ",cate)),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT p1,p2,rg,p,p1_phenotype,p2_phenotype,p2_ratio FROM ",db,".ldsc_rg_gsk
                WHERE p1 = '",trait,"' AND
                      p2_disease_category = '",cate,"'")
    analyses_rg <- dbGetQuery(conn,sql) 
    if(nrow(analyses_rg) == 0){
      analyses_rg <- data.frame(matrix(nrow = 0, ncol = 7)) 
      colnames(analyses_rg) <- c("p1","p2","rg","p","p1_phenotype","p2_phenotype","p2_ratio")
    }else{
      analyses_rg <- analyses_rg %>% arrange(p) 
    }
    removeModal()
  }else if(group=='all'){
    showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>All other traits <br>trait: ",trait,"<br> category: ",cate)),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT p1,p2,rg,p,p1_phenotype,p2_phenotype,p2_ratio FROM ",db,".ldsc_rg_gsk
                WHERE p1 = '",trait,"' AND
                      p2 != '",trait,"'")
    analyses_rg <- dbGetQuery(conn,sql) 
    if(nrow(analyses_rg) == 0){
      analyses_rg <- data.frame(matrix(nrow = 0, ncol = 7)) 
      colnames(analyses_rg) <- c("p1","p2","rg","p","p1_phenotype","p2_phenotype","p2_ratio")
    }else{
      analyses_rg <- analyses_rg %>% arrange(p) 
    }
    removeModal()
  }
  return(analyses_rg)
}

#' General function to return the list of sub-groups in h2 table
#' @param group (character) MeSH term or disease category to query
#' @param dsn (character) DSN to connect to
#' @param db (character) Database to make a gtx connection to (must be gtx compliant)
query_rg_groups <-function(group,dsn,db){
  conn <- get_connection(dsn = dsn, db = db)
  if(group=='mesh'){
   # showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>MeSH")),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT DISTINCT p2_phenotype_vocab_id FROM ",db,".ldsc_rg_gsk")
    cate <- dbGetQuery(conn,sql)  
    if(nrow(cate) == 0){
      cate <- data.frame(matrix(nrow = 0, ncol = 1)) 
      colnames(cate) <- c("group")
    }else{
      cate <- cate %>% 
        rename(group=p2_phenotype_vocab_id)
    }
  #  removeModal()
  }else if(group=='disease'){
  #  showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>disease_category")),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT DISTINCT p2_disease_category FROM ",db,".ldsc_rg_gsk")
    cate <- dbGetQuery(conn,sql)  
    if(nrow(cate) == 0){
      cate <- data.frame(matrix(nrow = 0, ncol = 1)) 
      colnames(cate) <- c("group")
    }else{
      cate <- cate %>% 
        rename(group=p2_disease_category)
    }
  #  removeModal()
    
  }else if(group=='mesh_label'){
  #  showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>MeSH label")),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT DISTINCT p2_phenotype_vocab_label  FROM ",db,".ldsc_rg_gsk")
    cate <- dbGetQuery(conn,sql)  
    if(nrow(cate) == 0){
      cate <- data.frame(matrix(nrow = 0, ncol = 1)) 
      colnames(cate) <- c("group")
    }else{
      cate <- cate %>% 
        rename(group=p2_phenotype_vocab_label)
    }
  #  removeModal()
    
  }else if(group=='phenotype'){
 #   showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>phenotype")),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT DISTINCT p2_phenotype FROM ",db,".ldsc_rg_gsk")
    cate <- dbGetQuery(conn,sql)  
    if(nrow(cate) == 0){
      cate <- data.frame(matrix(nrow = 0, ncol = 1)) 
      colnames(cate) <- c("group")
    }else{
      cate <- cate %>% 
        rename(group=p2_phenotype)
    }
  #  removeModal()
    
  }
  return(cate)
}


#' General function to return the list of sub-groups in h2 table
#' @param group (character) MeSH term or disease category to query
#' @param dsn (character) DSN to connect to
#' @param db (character) Database to make a gtx connection to (must be gtx compliant)
query_h2_groups <-function(group,dsn,db){
  conn <- get_connection(dsn = dsn, db = db)
  if(group=='mesh'){
    showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>MeSH")),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT DISTINCT phenotype_vocab_id FROM ",db,".ldsc_h2_gsk")
    cate <- dbGetQuery(conn,sql)  
    if(nrow(cate) == 0){
      cate <- data.frame(matrix(nrow = 0, ncol = 1)) 
      colnames(cate) <- c("group")
    }else{
      cate <- cate %>% 
        rename(group=phenotype_vocab_id)
    }
    removeModal()
  }else if(group=='disease'){
   showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>disease_category")),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT DISTINCT disease_category FROM ",db,".ldsc_h2_gsk")
     cate <- dbGetQuery(conn,sql)  
    if(nrow(cate) == 0){
      cate <- data.frame(matrix(nrow = 0, ncol = 1)) 
      colnames(cate) <- c("group")
    }else{
      cate <- cate %>% 
        rename(group=disease_category)
    }
    removeModal()
    
  }else if(group=='mesh_label'){
    showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>MeSH label")),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT DISTINCT phenotype_vocab_label  FROM ",db,".ldsc_h2_gsk")
    cate <- dbGetQuery(conn,sql)  
    if(nrow(cate) == 0){
      cate <- data.frame(matrix(nrow = 0, ncol = 1)) 
      colnames(cate) <- c("group")
    }else{
      cate <- cate %>% 
        rename(group=phenotype_vocab_label)
    }
    removeModal()
    
  }else if(group=='phenotype'){
    showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>phenotype")),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT DISTINCT phenotype FROM ",db,".ldsc_h2_gsk")
    cate <- dbGetQuery(conn,sql)  
    if(nrow(cate) == 0){
      cate <- data.frame(matrix(nrow = 0, ncol = 1)) 
      colnames(cate) <- c("group")
    }else{
      cate <- cate %>% 
        rename(group=phenotype)
    }
    removeModal()
    
  }
  return(cate)
}

#' General function to return the list of traits in h2 table
#' @param select (character) a specific sub-group to query
#' @param group (character) MeSH term or disease category to query
#' @param dsn (character) DSN to connect to
#' @param db (character) Database to make a gtx connection to (must be gtx compliant)
query_h2_selected <-function(group,select,zh2,dsn,db){
  conn <- get_connection(dsn = dsn, db = db)
  if(group=='mesh'){
    showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>MeSH: ",select)),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT analysis,h2,h2_se,ratio,ratio_se,lambda_gc,mean_chi2,phenotype,is_disease,ncase,ncohort FROM ",db,".ldsc_h2_gsk
                   WHERE phenotype_vocab_id = '",select,"' AND
                   z_h2 >= ",zh2) 
                   
    cate <- dbGetQuery(conn,sql) %>% mutate(n=n(),ratio_th=ifelse(is.null(ncase) | is.na(ncase) | ncase=="",0.45,0.2),p=ncase/ncohort)
    if(nrow(cate) == 0){
      cate <- data.frame(matrix(nrow = 0, ncol = 11)) 
      colnames(cate) <- c("analysis","h2","h2_se","ratio","ratio_se","lambda_gc","mean_chi2","phenotype","is_disease","n","ratio_th")
    }
    removeModal()
  }else if(group=='disease'){
    showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>disease_category: ",select)),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT analysis,h2,h2_se,ratio,ratio_se,lambda_gc,mean_chi2,phenotype,is_disease,ncase,ncohort FROM ",db,".ldsc_h2_gsk
                   WHERE disease_category = '",select,"' AND
                   z_h2 >= ",zh2)
    cate <- dbGetQuery(conn,sql) %>% mutate(n=n(),ratio_th=ifelse(is.null(ncase) | is.na(ncase) | ncase=="",0.45,0.2),p=ncase/ncohort)
    if(nrow(cate) == 0){
      cate <- data.frame(matrix(nrow = 0, ncol = 11)) 
      colnames(cate) <- c("analysis","h2","h2_se","ratio","ratio_se","lambda_gc","mean_chi2","phenotype","is_disease","n","ratio_th")
    }
    removeModal()
    
  }else if(group=='mesh_label'){
    showModal(modalDialog(HTML(paste0("<b>Getting analyses for:</b>  <br>MeSH label: ",select)),footer = modalButton("Dismiss"), easyClose = T))
    sql <- paste0("SELECT analysis,h2,h2_se,ratio,ratio_se,lambda_gc,mean_chi2,phenotype,is_disease,ncase,ncohort FROM ",db,".ldsc_h2_gsk
                   WHERE phenotype_vocab_label = '",select,"' AND
                   z_h2 >= ",zh2)
    cate <- dbGetQuery(conn,sql) %>% mutate(n=n(),ratio_th=ifelse(is.null(ncase) | is.na(ncase) | ncase=="",0.45,0.2),p=ncase/ncohort)
    if(nrow(cate) == 0){
      cate <- data.frame(matrix(nrow = 0, ncol = 11)) 
      colnames(cate) <- c("analysis","h2","h2_se","ratio","ratio_se","lambda_gc","mean_chi2","phenotype","is_disease","n","ratio_th")
    }
    removeModal()
    
  }
  return(cate)
}
#######################################################
# read the ldsc results log files from the terminal
# for i in `ls analyses/output/*.log`; 
# do grep -A 2 "Summary of Genetic Correlation Results" $i | tail -1; 
# done > analyses/output/ldsc_all_results.txt
#######################################################

gen_barplot_h2 <- function(dta){
  
barplot <- ggplot(dta) +
    geom_bar( aes(x=analysis, y=h2), stat="identity", fill="skyblue", alpha=0.5) +
    geom_errorbar( aes(x=analysis, ymin=h2-h2_se, ymax=h2+h2_se), width=0.4, colour="orange", alpha=0.9, size=1.3) +
    coord_flip()+theme_bw() 
  return(barplot)
}

gen_heatmap_aver_rg_all <- function(dta){

  if(nrow(dta) > 0 & !is.null(dta)){
# read in original list of analyses
  summary.tbl <- dta %>% mutate_if(is.factor, as.character) %>%
    rename(short1=p1,short2=p2,corr=rg) %>%
    mutate(corr=ifelse(corr>1,1,corr)) %>%
    select(short1,short2,corr)

#add the symmetric rows (for reconstructing full correlation matrix)
#sym.tbl <- summary.tbl %>% 
#  rename(shorta=short1,shortb=short2,ga=g1,gb=g2) %>%
#  rename(short2=shorta,short1=shortb,g2=ga,g1=gb) %>% 
#  subset(short2!=short1)

#summary.tbl <- bind_rows(summary.tbl,sym.tbl) %>% distinct()
#summary.tbl <- sym.tbl %>% distinct()
#print(nrow(summary.tbl)) #should be 48*48=2304


# Create the heatmap
heatmap_plot <- ggplot(summary.tbl, aes(x = short1, y = short2, fill = corr)) + 
  geom_tile(color='black')+
  scale_fill_gradient2(low='blue',high='red',mid='white',
                       midpoint=0,limit=c(-1,1),space='Lab',
                       name='LDScoreCorr')+xlab("")+ylab("") +
  theme_minimal() + theme_bw() +
  theme(
    axis.text.x = element_markdown(angle = 45, hjust = 1)
  ) 
  }else{
  heatmap_plot <- NULL
  }
  return(heatmap_plot)
}

gen_heatmap_rg_all <- function(dta){
  if(nrow(dta) > 0 & !is.null(dta)){
  # read in original list of analyses
  summary.tbl <- dta %>% mutate_if(is.factor, as.character) %>%
    rename(short1=p1,short2=p2,corr=rg,g1=group1,g2=group2) %>%
    mutate(corr=ifelse(corr>1,1,corr)) %>%
    mutate(sig_p = ifelse(p < .05, T, F), p_if_sig = ifelse(p <.05, p, NA), r_if_sig = ifelse(p <.05, corr, NA)) %>%
    select(short1,short2,corr,g1,g2,r_if_sig)
    
  #add the symmetric rows (for reconstructing full correlation matrix)
  #sym.tbl <- summary.tbl %>% 
  #  rename(shorta=short1,shortb=short2,ga=g1,gb=g2) %>%
  #  rename(short2=shorta,short1=shortb,g2=ga,g1=gb) %>% 
  #  subset(short2!=short1)
  
  #summary.tbl <- bind_rows(summary.tbl,sym.tbl) %>% distinct()
  #summary.tbl <- sym.tbl %>% distinct()
  #print(nrow(summary.tbl)) #should be 48*48=2304
  
  # Generate qualitative color palettes
  qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual', ]
  col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  # Combine unique elements from g1 and g2
  unique_elements <- unique(c(as.character(summary.tbl$g1), as.character(summary.tbl$g2)))
  
  # Assign colors to these unique elements
  combined_colors <- setNames(col_vector[1:length(unique_elements)], unique_elements)
  
  # Assign colors to the unique levels of g1 and g2
  g1_colors <- combined_colors[as.character(summary.tbl$g1)]
  g2_colors <- combined_colors[as.character(summary.tbl$g2)]
  
  # Combine g1 and g2 colors for the legend
  combined_labels <- c(names(combined_colors))
  
  # Create a new data frame for the legend
  legend_data <- data.frame(
    label = combined_labels,
    color = combined_colors
  )
  
  unique_x <- unique(summary.tbl$short1)
  unique_y <- unique(summary.tbl$short2)
  associated_g1 <- as.character(summary.tbl$g1)[match(unique_x, summary.tbl$short1)]
  associated_g2 <- as.character(summary.tbl$g2)[match(unique_y, summary.tbl$short2)]
  
  # Assign colors
  x_colors <- g1_colors[associated_g1]
  y_colors <- g2_colors[associated_g2]
  
  # Create the heatmap
  heatmap_plot <- ggplot(summary.tbl, aes(x = short1, y = short2, fill = corr, label=round(r_if_sig,2))) + 
    geom_tile()+
    scale_fill_gradient2(low='blue',high='red',mid='white',
                         midpoint=0,limit=c(-1,1),space='Lab',
                         name='LDScoreCorr')+xlab("")+ylab("") +
    theme_minimal() + theme_bw()+
    theme(
      axis.text.x = element_markdown(angle = 45, hjust = 1, colour = x_colors),
      axis.text.y = element_markdown(colour = y_colors)
    ) 
  
  
  # Updated number of rows and columns for the legend based on unique elements
  num_rows <- ceiling(length(unique_elements) / 3)
  
  # Update legend_plot
  legend_plot <- ggplot(legend_data, aes(x = 1, y = label, color = color)) +
    geom_point(size = 5) +
    scale_color_identity("", labels = combined_labels, breaks = combined_colors, guide = guide_legend(ncol = 3, nrow = num_rows)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      #    legend.title = element_markdown("Groups"),
      legend.key.size = unit(1, "cm")
    )
  
  # Combine heatmap and legend plots (you may need to adjust the widths)
  legend <- get_legend(legend_plot)
  
  # Combine heatmap and legend
  unclusted_heatmap <- ggdraw(plot_grid(heatmap_plot, legend, ncol = 1, nrow = 2, rel_heights = c(9,1)))
  }else{
    unclusted_heatmap <- NULL
  }
  return(unclusted_heatmap)
}

gen_histo_density_h2_cluster <- function(dta){
  if(nrow(dta) > 0 & !is.null(dta)){
  histodensity<- ggplot(dta, aes(x=h2)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.02,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
#  geom_vline(xintercept = 0.1,linetype = "dashed",color="red") +
  theme_bw()+ # Overlay with transparent density plot
  theme(axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20))
  }else{
    histodensity <- NULL
  }
  return(histodensity)
}

gen_histo_density_highlight_h2_cluster <- function(dta){
  if(nrow(dta) > 0 & !is.null(dta)){
    histodensity<- ggplot(dta, aes(x=h2)) + 
      geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                     binwidth=.02,
                     colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666") +
      geom_vline(xintercept = 0.1,linetype = "dashed",color="red") +
      theme_bw()+ # Overlay with transparent density plot
      theme(axis.text.x=element_text(size=20),
            axis.text.y=element_text(size=20))
  }else{
    histodensity <- NULL
  }
  return(histodensity)
}

gen_heatmap_rg_cluster <- function(dta){
  # read in original list of analyses
    summary.tbl <- dta %>% mutate_if(is.factor, as.character) %>%
      rename(short1=p1,short2=p2,corr=rg) %>%
      mutate(corr=ifelse(corr>1,1,corr)) %>%
      select(short1,short2,corr,p)
 
  #add the symmetric rows (for reconstructing full correlation matrix)
  sym.tbl <- summary.tbl %>% 
    rename(shorta=short1,shortb=short2) %>%
    rename(short2=shorta,short1=shortb) %>% 
    subset(short2!=short1)
  
  summary.tbl <- bind_rows(summary.tbl,sym.tbl) %>% distinct()
 # print(nrow(summary.tbl)) #should be 48*48=2304
  
  #write.table(summary.tbl,"analyses/LDSC_all_results_tall.txt",quote=F,row.names=F,sep='\t')
  
   pval.lim <- 2*0.05/nrow(summary.tbl) #bonferroni correction
  
  #only cluster significant corr
 # summary.tbl <- summary.tbl %>%
 #    mutate(corr=ifelse(p > pval.lim,0,corr))
  
  cormat <- as.matrix(dcast(summary.tbl,short1~short2,sum,value.var='corr'))
  temp_rownames <- cormat[,1]
  cormat <- cormat[,2:ncol(cormat)]
  cormat <- apply(cormat,2,as.numeric)
  rownames(cormat) <- temp_rownames
  
  #matrix format of LDSC p values
  pmat <- as.matrix(dcast(summary.tbl,short1~short2,sum,value.var='p'))
  temp_rownames <- pmat[,1]
  pmat <- pmat[,2:ncol(pmat)]
  pmat <- apply(pmat,2,as.numeric)
  rownames(pmat) <- temp_rownames  
  
  # actual clustering
  dd <- dist(cormat,method='euclidean')
  hc <- hclust(dd)
  dend1 <- as.dendrogram(hc)
  
  sig_pc <- ifelse(pmat<=pval.lim,round(cormat,3),"")  
  
  #colors of the heatmap
  colors <- rev(colorRampPalette(brewer.pal(8,'RdBu'))(20))
  colors <- c(colors[1:10],"#FFFFFF",colors[11:20])
  
  heatmap_by_groups <- heatmap.2(cormat,trace='none',dendrogram='both',na.rm=T,
            Rowv=dend1,Colv=dend1,
            symkey=F,symm=F,symbreaks=T,scale='none',
            cellnote=sig_pc,notecex=1,notecol='#000000',
            col=colors,
            na.color='#FFFFFF',
            key=T,key.xlab='LDSC',key.title='NA',
            density.info='none',
            cexRow=1, cexCol=1,
            margin=c(15,15),srtCol = 45
#,lwid=c(0.8,4),lhei=c(0.8,5)
  )
  return(heatmap_by_groups)
}

gen_dendrogram_rg_cluster <- function(dta){
  # read in original list of analyses
  summary.tbl <- dta %>% mutate_if(is.factor, as.character) %>%
    rename(short1=p1,short2=p2,corr=rg) %>%
    mutate(corr=ifelse(corr>1,1,corr)) %>%
    select(short1,short2,corr,p)
  
  #add the symmetric rows (for reconstructing full correlation matrix)
  sym.tbl <- summary.tbl %>% 
    rename(shorta=short1,shortb=short2) %>%
    rename(short2=shorta,short1=shortb) %>% 
    subset(short2!=short1)
  
  summary.tbl <- bind_rows(summary.tbl,sym.tbl) %>% distinct()
  # print(nrow(summary.tbl)) #should be 48*48=2304
  
  #write.table(summary.tbl,"analyses/LDSC_all_results_tall.txt",quote=F,row.names=F,sep='\t')
  
  pval.lim <- 2*0.05/nrow(summary.tbl) #bonferroni correction
  
  #only cluster significant corr
  # summary.tbl <- summary.tbl %>%
  #    mutate(corr=ifelse(p > pval.lim,0,corr))
  
  cormat <- as.matrix(dcast(summary.tbl,short1~short2,sum,value.var='corr'))
  temp_rownames <- cormat[,1]
  cormat <- cormat[,2:ncol(cormat)]
  cormat <- apply(cormat,2,as.numeric)
  rownames(cormat) <- temp_rownames
  
  #matrix format of LDSC p values
  pmat <- as.matrix(dcast(summary.tbl,short1~short2,sum,value.var='p'))
  temp_rownames <- pmat[,1]
  pmat <- pmat[,2:ncol(pmat)]
  pmat <- apply(pmat,2,as.numeric)
  rownames(pmat) <- temp_rownames  
  
  # actual clustering
  dd <- dist(cormat,method='euclidean')
  hc <- hclust(dd)
  dend1 <- as.dendrogram(hc)
  
  dendrogram <- ggdendrogram(hc, rotate = FALSE, size = 2)
  return(dendrogram)
}

#' Get DBConnection / Refresh existing one
#' @param dsn (character) DSN to connect to
#' @param db (character) Database to make a gtx connection to (must be gtx compliant)
#' @param force (logical) Should a connection be forced if one already exists?
#' @param cache (logical) Should tables like entities be cached in memory?
#' Get DBConnection / Refresh existing one
#'
get_connection <- function(dsn = "IPSCC", db = "gene_gwas_hg38_use", force = FALSE, cache = T) {
  
  dbc <- getOption("gtx.dbConnection", NULL)
  
  # Restart connection if >1 hour
  t1      <- Sys.time()
  t0      <- getOption("db_time_start", Sys.time())
  elapsed <- as.numeric(difftime(t1, t0, units="hours"))
  
  if (is.null(dbc) || elapsed >= 1 || !dbIsValid(dbc) || force == TRUE) {
    message("Restarting connection.")
    
    # Reauthenticate with Kerberos
    #user   <- Sys.getenv("USER")
    #system(glue("kinit {user}@WMSERVICE.CORPNET1.COM -k -t ~/{user}.keytab"))
    #system(glue("kinit rc710711@WMSERVICE.CORPNET1.COM -k -t /app/build/R/access.keytab"))
    user <- "hf885044"
    keytab <- "hg_tools.keytab"
    system(glue("kinit {user}@WMSERVICE.CORPNET1.COM -k -t {keytab}"))
    
 #   if (!is.null(dbc)) {
#      dbDisconnect(dbc)
#    }
    
    # Try 25x to establish connection
    dbc <- NULL
    attempt <- 1
    while( is.null(dbc) && attempt <= 25 ) {
      message(glue("Attempt {attempt} to connect"))
      attempt <- attempt + 1
      tryCatch({
        gtxconnect(dbc = dbConnect(odbc::odbc(), dsn = dsn),
                   use_database = db,
                   cache = cache)
        dbc <- getOption("gtx.dbConnection", NULL)
        
        #Check connection is valid
        if(dbIsValid(dbc)){
          message("Valid connection")
        }else{
          message("Invalid connection, retrying connection")
          dbc <- NULL
          gtxconnect(dbc = dbConnect(odbc::odbc(), dsn = dsn),
                     use_database = db,
                     cache = cache)
          dbc <- getOption("gtx.dbConnection", NULL)
        }
        
        #Run a test to ensure we have a connection
        # sql <- paste0("SELECT * FROM gene_gwas_hg38_use.gwas_results LIMIT 10")
        # test <- try(dbGetQuery(dbc, sql))
        # if(nrow(test) == 10 & !"character" %in% class(test)){
        #   message("Connection stable!!!")
        # }else{
        #   dbc <- NULL
        #   message("Connection is unstable, re-attempting to connect")
        # }
      }, error = function(e) {
        message(e)
        Sys.sleep(60)
      })
    }
    
    options("db_time_start" = Sys.time())
  }
  
  if (is.null(dbc)) {
    stop("Could not establish a database connection")
  }
  
  return(dbc)
}