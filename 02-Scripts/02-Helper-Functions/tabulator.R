# functions to create summary tables 



create.Summary.stats <- function(the.variable, the.strata, the.data, the.method){
  
  # load packages
  my.packages <- c("dplyr", "magrittr", "purrr", "flextable")
  lapply(my.packages, require, character.only=T)

  
  the.variable.data <- the.data[,the.variable]
  
  if(is.factor(the.data[,the.strata])){
    the.levels <- levels(unlist(the.data[,the.strata]))
  }else{
    the.levels <- levels(factor(unlist(the.data[,the.strata])))
  }
  
  if(the.method == "means"){
    the.summary <- rbind(
                    cbind(
                      Levels = NA,
                      t(
                      tapply(
                        unlist(the.data[,the.variable]), 
                        unlist(the.data[,the.strata]), 
                        function(x){
                          paste0(round(mean(x, na.rm=T), 5), " (", round(sd(x, na.rm=T), 5), ")")
                          }
                            )
                     )
                     ),
                    cbind(
                      Levels = "Missing",
                      
                      t(
                        sapply(the.levels, 
                               function(.this.level){
                                 paste0(
                                   sum(is.na(unlist(the.data[which(the.data[,the.strata]==.this.level),the.variable]))), 
                                   " (", round(sum(is.na(unlist(the.data[which(the.data[,the.strata]==.this.level),the.variable])))/ nrow(the.data[which(the.data[,the.strata]==.this.level),])*100, 1), ")"
                                   )
                                 }
                               )
                      )
                    )
                   )
    the.test <- if(length(the.levels)==2){
                  t(
                    unlist(
                      t.test(
                        the.data[which(the.data[,the.strata]==the.levels[1]),the.variable], 
                        the.data[which(the.data[,the.strata]==the.levels[2]),the.variable]
                      )
                    )
                  ) %>% 
                  as.data.frame() %>% 
                  select(
                    Statistic = statistic.t, 
                    Parameter = parameter.df, 
                    p = p.value, 
                    Test = method
                    ) %>% 
                  mutate(
                    Statistic = round(as.numeric(Statistic), 2), 
                    Parameter = round(as.numeric(Parameter), 2),
                    p = as.character(ifelse(round(as.numeric(p), 2)==0, "<0.001", round(as.numeric(p), 2)))
                  )
    }else{
                  if(length(the.levels)>2){
                    aov(as.formula(paste0(the.variable, "~", the.strata)), data = the.data) %>%
                      summary() %>% 
                      unlist() %>% 
                      t() %>% 
                      as.data.frame() %>% 
                      mutate(
                        Parameter = paste0(Df1, ", ", Df2), 
                        Statistic = `F value1`, 
                        p = as.character(ifelse(round(as.numeric(`Pr(>F)1`), 2)==0, "<0.001", round(as.numeric(`Pr(>F)1`), 2))), 
                        Test = "AoV"
                      ) %>% 
                      select(
                        Statistic, 
                        Parameter, 
                        p, 
                        Test
                      )
                  }
                }
  }
  
  if(the.method == "medians"){
    the.summary <- rbind(
                    cbind(
                      Levels = NA,
                      t(
                      tapply(
                        unlist(the.data[,the.variable]), 
                        unlist(the.data[,the.strata]), 
                        function(x){
                          paste0(round(median(x, na.rm=T), 2), " [", paste0(quantile(x, probs = c(0.25, 0.75), na.rm=T), collapse = ", "), "]")
                        }
                      )
                    )
                    ), 
                    cbind(
                      Levels = "Missing",
                      t(
                        sapply(the.levels, 
                             function(.this.level){
                               paste0(
                                 sum(is.na(unlist(the.data[which(the.data[,the.strata]==.this.level),the.variable]))), 
                                 " (", round(sum(is.na(unlist(the.data[which(the.data[,the.strata]==.this.level),the.variable])))/ nrow(the.data[which(the.data[,the.strata]==.this.level),])*100, 1), ")"
                               )
                             }
                        )
                      )
                    )
                  )
    
    the.test <- if(length(the.levels)==2){
      t(
        unlist(
          wilcox.test(
            unlist(the.data[which(the.data[,the.strata]==the.levels[1]),the.variable]), 
            unlist(the.data[which(the.data[,the.strata]==the.levels[2]),the.variable])
          )
        )
      ) %>% 
        as.data.frame() %>% 
        select(
          Statistic = statistic.W,
          Parameter = NULL, 
          p = p.value, 
          Test = method
        ) %>% 
        mutate(
          Statistic = round(as.numeric(Statistic), 0),
          p = as.character(ifelse(round(as.numeric(p), 2)==0, "<0.001", round(as.numeric(p), 2)))
        )
    }else{
      if(length(the.levels)>2){
        kruskal.test(as.formula(paste0(the.variable, "~", the.strata)), data = the.data) %>%
          # summary() %>% 
          unlist() %>% 
          t() %>% 
          as.data.frame() %>% 
          mutate(
            Statistic = as.numeric(`statistic.Kruskal-Wallis chi-squared`), 
            Parameter = parameter.df, 
            p = as.character(ifelse(round(as.numeric(p.value), 2)==0, "<0.001", round(as.numeric(p.value), 2))), 
            Test = method
          ) %>% 
          select(
            Statistic, 
            Parameter, 
            p, 
            Test
          )
      }
    }
    
    
    
  }
  
  if(the.method == "frequencies"){
    
    the.summary <- matrix(
                    paste0(
                      table(unlist(the.data[,the.variable]), unlist(the.data[,the.strata]), useNA = 'always'), 
                      " (", round(prop.table(table(unlist(the.data[,the.variable]), unlist(the.data[,the.strata]), useNA = 'always'), margin = 2)*100, 1), ")"
                      ), 
                    ncol = 3
                    ) %>% 
                    set_colnames(
                      ., 
                      c(the.levels, "Missing")
                      ) %>% 
                    set_rownames(
                      ., 
                      c(levels(factor(unlist(the.data[,the.variable]))), "Missing")
                      ) %>% 
                    as.data.frame() %>% 
                    tibble::rownames_to_column("Levels")
    
    the.test <- t(
                  unlist(
                    chisq.test(
                      table(
                        unlist(the.data[,the.variable]), 
                        unlist(the.data[,the.strata])
                        )
                      )
                    )
                  ) %>% 
                  as.data.frame() %>% 
                  select(
                    Statistic = "statistic.X-squared", 
                    Parameter = parameter.df, 
                    p = p.value, 
                    Test = method
                    ) %>% 
                  mutate(
                    Statistic = round(as.numeric(Statistic), 2), 
                    Parameter = round(as.numeric(Parameter), 2),
                    p = as.character(ifelse(round(as.numeric(p), 2)==0, "<0.001", round(as.numeric(p), 2)))
                  )
    
  }
  
  
  table.entry <- cbind(
                  Variable = c(the.variable, rep(NA, nrow(the.summary)-1)), 
                  the.summary, 
                  rbind(the.test, matrix(rep(rep(NA, ncol(the.test)), nrow(the.summary)-1), ncol = ncol(the.test))%>%set_colnames(names(the.test)))
                  )
  
  return(table.entry)
}













create.Table.1 <- function(the.vars, the.strata, the.data, the.methods=NULL){
  
  # load packages
  my.packages <- c("dplyr", "magrittr", "purrr", "flextable")
  lapply(my.packages, require, character.only=T)
  
  
  if(is.null(the.methods)){
    the.methods <- ifelse(sapply(the.data[,the.vars], class)=="numeric", "means", ifelse(sapply(the.data[,the.vars], class)=="integer", "medians", "frequencies"))
  }
  
  
  
  the.table <- lapply(1:length(the.vars), function(.index){
    create.Summary.stats(the.vars[.index], the.strata, the.data, the.methods[.index])
  })
  
  the.levels <- levels(as.factor(unlist(the.data[,the.strata])))
  
  the.table %<>% bind_rows() %>% select(Variable, Levels, all_of(the.levels), Statistic, Parameter, p, Test)
  
  the.table %<>% add_row(.before = 1)
  the.table[1,the.levels] <- cbind(
                                                      t(
                                                        sapply(the.levels, 
                                                               function(.this.level){
                                                                 paste0(
                                                                   "n = ",
                                                                   nrow(the.data[which(the.data[,the.strata]==.this.level),]), 
                                                                   " (", round(nrow(the.data[which(the.data[,the.strata]==.this.level),])/ nrow(the.data)*100, 1), ")"
                                                                 )
                                                               }
                                                        )
                                                      )
                                                    )
  return(the.table)
}


