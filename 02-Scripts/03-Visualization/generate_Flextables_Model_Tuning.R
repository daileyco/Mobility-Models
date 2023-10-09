# script to format model summary tables and save to word docs


## load data
load("./03-Output/01-Tables/table_gravity_dt_tuning.rds")
load("./03-Output/01-Tables/table_uo_ab_tuning.rds")


## packages
library(dplyr)
library(flextable)
library(officer)



tables.gravity.tuning <- c(list(full=table.gravity.distance.thresholds, 
                                basic=table.gravity.distance.thresholds %>%
                                  filter(Period=="2011-2020" & 
                                           `Model Variation`=="base*distance_threshold" & 
                                           `Objective Scale for Tuning`=="log")), 
                           table.gravity.distance.thresholds %>% 
                             split(., f = .$Period)) %>%
  setNames(., nm = paste0("gravity_tuning_", names(.))) %>%
  lapply(., 
         (\(x) {
           x[which(duplicated(x[,1:3])),3] <- NA
           x[which(duplicated(x[,1:2])),2] <- NA
           x[which(duplicated(x[,1])),1] <- NA
           return(x)
         }))
  

tables.uo.tuning <- c(list(full=table.uo.params, 
                           basic=table.uo.params%>%
                             filter(Period=="2011-2020" & 
                                      `Surrounding Population, s_{ij}`=="Comprehensive, Census Data" & 
                                      `Total Commuters, T_{i}`=="Observed" & 
                                      `Objective Scale for Tuning`=="log")), 
                      table.uo.params %>% 
                        split(., f = .$Period)) %>%
  setNames(., nm = paste0("uo_tuning_", names(.))) %>%
  lapply(., 
         (\(x) {
           x[which(duplicated(x[,1:4])),4] <- NA
           x[which(duplicated(x[,1:3])),3] <- NA
           x[which(duplicated(x[,1:2])),2] <- NA
           x[which(duplicated(x[,1])),1] <- NA
           return(x)
         }))




t.list <- c(tables.gravity.tuning, 
            tables.uo.tuning)



## create titles/captions for tables
caps <- paste0(rep(c("Gravity Model Tuning of Distance Thresholds", "Universal Opportunity Model Tuning"), each = 5),
               c("", "", ", 2011-2015", ", 2011-2020", ", 2016-2020"))



## flextable formatting
ft.list <- lapply(1:length(t.list), 
                   function(fti){
                     
                     
                     ft <- t.list[[fti]]
                     ftc <- caps[fti]
                     
                     
                     ft %>%
                       flextable() %>%
                       
                       ### set font and fontsize
                       font(part = "all", fontname = "Arial") %>% 
                       fontsize(part = "header", size = 10) %>%
                       fontsize(part = "body", size = 9) %>% 
                       
                       ### alignment
                       align(part = "header", align = "center") %>%
                       #### text left
                       align(part = "body", j=1:ifelse(grepl("Gravity", ftc), 4, 5), align = "left") %>%
                       #### numbers right
                       align(part = "body", j=ifelse(grepl("Gravity", ftc), 4+1, 5+1):ncol(ft), align = "right") %>% 
                       
                       ### bold top row giving column names
                       bold(part = "header") %>% 
                       
                       ### remove default borders
                       border_remove() %>%
                       #### add one below column names and one below body of table
                       border(part = "header", border.bottom = fp_border_default(color = "black", width = 2)) %>% 
                       border(part = "body", i=nrow(ft), border.bottom = fp_border_default(color = "black", width = 1)) %>% 
                       
                       ### set widths of columns to make content all fit on a single row
                       #### dim_pretty() determines this based off of string width(?)
                       width(., width = dim_pretty(.)$widths) %>% 
                       
                       ### set heights of the rows
                       #### set to the point size for size 13 font 
                       ##### (little extra space since table text is size 9)
                       #### dim_pretty is not constant among rows
                       # height_all(., height = max(dim_pretty(.)$heights)) %>%
                       height_all(., height = 13/72) %>%
                       
                       #### helpful to add dummy row below border below headings to prevent weird looking first row
                       add_body_row(values = NA) %>%
                       height(part = "body", i = 1, height = 2/72) %>%
                       
                       #### this forces word to adhere to the set heights
                       ##### useful for determining the necessary page size
                       hrule(part = "all", rule = "exact") %>%
                       
                       #### remove before and after and left and right extra line spacing
                       padding(part = "all", padding = 0) %>%
                       
                       
                       ### add title/caption
                       
                       set_caption(caption = as_paragraph(as_chunk(ftc, 
                                                                   props = fp_text(color = "black", 
                                                                                   font.size = 11, 
                                                                                   font.family = "Arial"))),
                                   #### this may overwrite the word style settings (?)
                                   fp_p = fp_par(text.align = "left",
                                                 # padding = 0,
                                                 line_spacing = 1,
                                                 padding.bottom = 5, 
                                                 padding.top = 0, 
                                                 padding.left = 0, 
                                                 padding.right = 0,
                                                 border = fp_border(width = 0),
                                                 word_style = "Normal"),
                                   
                                   #### table is centered on page, 
                                   ##### this will force it to use paragraph formatting specified above
                                   align_with_table = FALSE) %>%
                       
                       return()
                     
                   }) %>% 
  ### add names back lost in lapply() since used index to loop
  setNames(., 
           nm = names(t.list))




### figure out minimum page sizes to save as word doc
#### does not work well, have to add
#### flextable_dim() does the same thing
ft.dims <- lapply(ft.list,
                   function(ft){
                     ft %>% 
                       dim() %>%
                       sapply(sum) %>%
                       return() #gives width, height of whole table
                   })




## determine extra size needed for page size to fit table on a single page

### specify page margins in inches
the.page.margins <- list(bottom = 1, left = 1, top = 1, right = 1, 
                         header = 0.5, footer = 0.5, 
                         gutter = 0.5)

### calculate extra height and width needed 
#### based on margins and extra lines at 
#### top (1 for title/caption) and 
#### bottom (2 for description 
##### [may remove later depending if built in footnotes are okay])

#### unit conversions for points (pts, aka font sizes) to inches
##### one inch = 72 points
##### font size is in pt

##### overwrote default styles to avoid this nightmare
###### word defaults 
####### line height for single line spacing is 1.15*font size
####### Cambria size 12
####### 5pt spacing before and after
####### 0.07" indent left and right

extra.width <- (0
                + sum(unlist(the.page.margins[which(names(the.page.margins)%in%c("left", "right", "gutter"))]))
)
extra.height <- (0 
                 # for extra line at top for title/caption
                 + 2*11/72
                 # for the border below column names and extra row added beneath it
                 + 2*2/72
                 # for the bottom border
                 + 1/72
                 # for two lines after
                 + 2*10/72
                 # for page margins
                 + sum(unlist(the.page.margins[which(names(the.page.margins)%in%c("top", "bottom"))])) 
)


## write tables to word docs

for(i in 1:length(ft.list)){
  
  ### determine page size to fit table on a single page

  the.page.sizes <- list(width = ft.dims[[i]][1]+extra.width, 
                         height = ft.dims[[i]][2]+extra.height) %>%
    c(., 
      #### seems finicky if you don't specify based on page dimensions
      list(orient = ifelse(.[["width"]]<.[["height"]], 
                           "portrait", 
                           "landscape")))
  
  ### create word doc
  temp <- read_docx()
  temp <- set_doc_properties(temp, 
                             creator = "Cody Dailey")
  
  temp <- cursor_begin(temp)
  
  #### add the flextable with prespecified formatting
  temp <- body_add_flextable(temp, 
                             ft.list[[i]], 
                             align = "center", 
                             pos = "after", 
                             split = FALSE, 
                             topcaption = TRUE)
  
  #### manually specify/modify word doc styles
  temp <- docx_set_paragraph_style(temp, 
                                   style_id = "MyNormal", 
                                   style_name = "MyNormal", 
                                   base_on = "Normal", 
                                   fp_p = fp_par(
                                     text.align = "left",
                                     padding = 0,
                                     line_spacing = 1,
                                     border = fp_border(width = 0),
                                     # padding.bottom,
                                     # padding.top,
                                     # padding.left,
                                     # padding.right,
                                     # border.bottom,
                                     # border.left,
                                     # border.top,
                                     # border.right,
                                     shading.color = "transparent",
                                     keep_with_next = FALSE,
                                     word_style = "Normal"
                                   ), 
                                   fp_t = fp_text(
                                     color = "black",
                                     font.size = 10,
                                     bold = FALSE,
                                     italic = FALSE,
                                     underlined = FALSE,
                                     font.family = "Arial",
                                     cs.family = NULL,
                                     eastasia.family = NULL,
                                     hansi.family = NULL,
                                     vertical.align = "baseline",
                                     shading.color = "transparent"
                                   ))
  ##### Table Caption style may not be needed as specified within flextable
  # temp <- docx_set_paragraph_style(temp, 
  #                                  style_id = "TableCaption", 
  #                                  style_name = "Table Caption", 
  #                                  base_on = "Normal", 
  #                                  fp_p = fp_par(
  #                                    text.align = "left",
  #                                    # padding = 0,
  #                                    line_spacing = 2,
  #                                    border = fp_border(width = 0),
  #                                    padding.bottom = 0,
  #                                    padding.top = 0,
  #                                    padding.left = 0,
  #                                    padding.right = 0,
  #                                    # border.bottom,
  #                                    # border.left,
  #                                    # border.top,
  #                                    # border.right,
  #                                    shading.color = "transparent",
  #                                    keep_with_next = FALSE,
  #                                    word_style = "Normal"
  #                                  ), 
  #                                  fp_t = fp_text(
  #                                    color = "black",
  #                                    font.size = 11,
  #                                    bold = FALSE,
  #                                    italic = FALSE,
  #                                    underlined = FALSE,
  #                                    font.family = "Arial",
  #                                    cs.family = NULL,
  #                                    eastasia.family = NULL,
  #                                    hansi.family = NULL,
  #                                    vertical.align = "baseline",
  #                                    shading.color = "transparent"
  #                                  ))
  
  
  #### change the page size and layout
  ##### suggested somewhere in documentation to be after table added
  temp <- body_set_default_section(temp, 
                                   prop_section(page_size = do.call("page_size", 
                                                                    args = the.page.sizes), 
                                                page_margins = do.call("page_mar", 
                                                                       args = the.page.margins)))
  
  
  ### save the word doc to a file
  print(temp, 
        target = paste0("./03-Output/01-Tables/table_", 
                        names(ft.list)[i], 
                        ".docx"))
  
  
  
  ### even without specifying title for flextable, save_as_docx adds a blank line before table
  #### frustrating because the first line will be Word defaults
  ##### messing up page size calculations
  ### above sequence is alternative to this function
  # save_as_docx(values = list(sft.list[[i]]) %>% setNames(., nm = caps[i]), 
  #              # sft.list[[i]], 
  #              path = paste0("./03-Output/01-Tables/summary_table_", 
  #                            names(sft.list)[i], 
  #                            ".docx"), 
  #              pr_section = prop_section(page_size = do.call("page_size", 
  #                                                            args = the.page.sizes), 
  #                                        page_margins = do.call("page_mar", 
  #                                                               args = the.page.margins)))
  

}





## save
save(ft.list, 
     file = "./03-Output/01-Tables/flextables_model_tuning.rds")


## clean environment
rm(list=ls())
gc()




