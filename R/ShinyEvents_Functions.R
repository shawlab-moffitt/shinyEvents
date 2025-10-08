



timelinePlot_lyprep <- function(data = NULL, event_type_col = NULL, start_col = NULL, stop_col = NULL, highlight_col = NULL, text_col = "text") {
  
  if (is.null(data)) stop("Must provide event data")
  data <- as.data.frame(data)
  
  coi <- grep(event_type_col,colnames(data), invert = T, value = T)
  coi <- coi[which(!coi%in%c("index",highlight_col))]
  data[,text_col] <- NA
  for (r in seq(nrow(data))) {
    text_vec <- c()
    for (c in coi) {
      if (!is.na(data[r,c])) {
        if (c == start_col | c == stop_col) {
          if (data[r,start_col] != data[r,stop_col]) {
            text_vec <- c(text_vec,
                          paste0("\\<br\\>\\<b\\>", c, ":\\</b\\> ",data[r,c]))
          } else {
            text_vec <- c(text_vec,
                          paste0("\\<br\\>\\<b\\>EventTime:\\</b\\> ",data[r,c]))
          }
        } else {
          data_text <- ifelse(grepl("_Summary$",c),paste0("\n",data[r,c]),data[r,c])
          text_vec <- c(text_vec,
                        paste0("\\<br\\>\\<b\\>", c, ":\\</b\\> ",data_text))
        }
      }
    }
    text_vec <- unique(text_vec)
    text = paste(text_vec,collapse = '')
    text = gsub("\\\\", '', text)
    data[,text_col][r] = text
  }
  return(data)
  
}

timelinePlot <- function(data = NULL,event_col = NULL, event_type_col = NULL, start_col = NULL, stop_col = NULL, unit = "years",
                         plotly = FALSE, hover_text = "text", title = "Timeline Plot", title_font = 16, x_font = 14, y_font = 14, na.rm = TRUE,
                         svg_name = "plot", svg_height = 8, svg_width = 8, dot_size = 3, highlight_col = NULL, col_pal = NULL) {
  
  if (is.null(data)) stop("Must provide event data")
  data <- as.data.frame(data)
  if (is.null(event_col)) event_col <- colnames(data)[1]
  if (is.null(event_type_col)) event_type_col <- colnames(data)[2]
  if (is.null(start_col)) start_col <- colnames(data)[3]
  if (is.null(stop_col)) stop_col <- colnames(data)[4]
  if (!is.numeric(data[,start_col])) stop("Event start time must be numeric")
  if (!is.numeric(data[,stop_col])) stop("Event stop time must be numeric")
  
  data[which(is.na(data[,start_col])),start_col] <- data[which(is.na(data[,start_col])),stop_col]
  data[which(is.na(data[,stop_col])),stop_col] <- data[which(is.na(data[,stop_col])),start_col]
  if (na.rm) {
    data <- data[which(!is.na(data[,start_col]) & !is.na(data[,stop_col])),] # remove data with incomplete time
  }
  data$index <- as.numeric(factor(data[,event_col], levels=unique(data[,event_col])))
  
  # Get x axis min and max
  allAges <- c(data[,start_col],data[,stop_col])
  
  uniq_cols_needed <- length(unique(data[,event_type_col]))
  if (!is.null(col_pal)) {
    if (col_pal %in% palette.pals()) {
      if (uniq_cols_needed > length(palette.colors(palette = col_pal))) {
        new_cols <- palette.colors(n = uniq_cols_needed, palette = col_pal, recycle = TRUE)
      } else {
        new_cols <- palette.colors(n = uniq_cols_needed, palette = col_pal)
      }
    } else {
      new_cols <- hcl.colors(n = uniq_cols_needed, palette = col_pal)
    }
  }
  
  
  if (plotly) {
    if (!hover_text %in% colnames(data)) {
      data <- timelinePlot_lyprep(data = data, event_type_col = event_type_col, start_col = start_col, stop_col = stop_col,
                                  highlight_col = highlight_col, text_col = hover_text)
    }
    plot2 <- ggplot2::ggplot(data, ggplot2::aes(x=!!ggplot2::sym(start_col), y=index, label = !!ggplot2::sym(event_col),
                                                color = !!ggplot2::sym(event_type_col), text = !!ggplot2::sym(hover_text)))
  } else {
    plot2 <- ggplot2::ggplot(data, ggplot2::aes(x=!!ggplot2::sym(start_col), y=index, label = !!ggplot2::sym(event_col),
                                                color = !!ggplot2::sym(event_type_col)))
  }
  if (!is.null(col_pal)) {
    plot2 <- plot2 +
      scale_color_manual(values = new_cols)
  }
  
  g <- ggplot2::ggplot_build(plot2)
  YlabCols <- g[["data"]][[1]][["colour"]]
  yLabs <- g[["data"]][[1]][["label"]]
  newCols_df <- data.frame(Labs = yLabs,
                           cols = YlabCols)
  newCols_df_uniq <- unique(newCols_df)
  
  if (!is.null(highlight_col)) {
    if (highlight_col %in% colnames(data)) {
      highligh_data <- merge(data[which(data[,highlight_col] == TRUE),],newCols_df_uniq, by.x = event_col, by.y = "Labs", all.x = T)
      plot2 <- plot2 +
        geom_vline(xintercept = highligh_data[,start_col],
                   color = highligh_data[,"cols"])
    } else if (is.numeric(highlight_col)) {
      plot2 <- plot2 +
        geom_vline(xintercept = highlight_col)
    }
  }
  
  plot2 <- plot2 +
    ## Add end time points
    ggplot2::geom_point(ggplot2::aes(x=!!ggplot2::sym(start_col), y=index),size=dot_size)+
    ## draw lines - inherits start time point x,y
    ggplot2::geom_segment(ggplot2::aes(xend = !!ggplot2::sym(stop_col), yend = index), linewidth = 2, lineend = "butt") +
    ## x and y labels
    ggplot2::xlab(unit) +
    ggplot2::ylab('') +
    ggplot2::theme_minimal() +
    ## adjust x & y axis to accommodate the event lables
    ggplot2::scale_y_continuous(breaks=unique(data$index), # each line break from the index column
                                labels = stringr::str_wrap(unique(data[,event_col]), width = 100), # event label
                                trans = "reverse") +
    ggplot2::scale_x_continuous(limits=c(min(allAges, na.rm = T), max(allAges, na.rm = T)))
  
  
  plot2 <- plot2 +
    ## adjust font size and face and legend position
    ggplot2::theme(plot.title = ggplot2::element_text(size=title_font,face="bold"),
                   legend.position = "none",
                   axis.text.x = ggplot2::element_text(size=x_font),
                   axis.text.y = ggtext::element_markdown(size=y_font, colour = newCols_df_uniq$cols),
                   axis.title.x = ggplot2::element_text(size=x_font,face="bold"),
                   axis.title.y = ggplot2::element_text(size=y_font,face="bold"),
                   plot.margin = ggplot2::margin(0, 2, 0, 0, "cm")
    ) +
    ## add title
    ggplot2::ggtitle(title)
  if (plotly) {
    plot3 <- plotly::ggplotly(plot2,tooltip = hover_text)
    plot3[["x"]][["layout"]][["yaxis"]][["ticktext"]] <- paste0("<span style='color:",newCols_df_uniq$cols,"'>",plot3[["x"]][["layout"]][["yaxis"]][["ticktext"]],"</span>")
    plot3 <- plot3 %>%
      config(
        toImageButtonOptions = list(
          format = "svg",
          height = svg_height,
          width = svg_width,
          filename = svg_name
        )
      )
    return(plot3)
  } else {
    return(plot2)
  }
  
}


areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = "text", na = c("NA","","N/A")))
  x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

getMed <- function(x) {
  x[which(suppressWarnings(as.numeric(x) > 0))] <- x[2]
  x[which(suppressWarnings(as.numeric(x) == 0))] <- NA
  return(x)
}

event_count_df <- function(event_data = NULL, event_type_col = "EventType", event_start_col = "EventStart", event_end_col = "EventEnd") {
  event_data_yor <- event_data %>%
    group_by(!!sym(colnames(event_data)[1])) %>%
    mutate(`Time On Record` = round((max(!!sym(event_end_col))-min(!!sym(event_start_col))),3)) %>%
    #mutate(`Years On Record` = round((max(!!sym(event_end_col))-min(!!sym(event_start_col))),3)) %>%
    group_by(!!sym(colnames(event_data)[1]),!!sym(event_type_col)) %>%
    mutate(Time_Points = length(!!sym(event_type_col)),
           row_index = row_number()) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = c(!!sym(colnames(event_data)[1]), `Time On Record`,row_index),
      names_from = !!sym(event_type_col),
      values_from = Time_Points
    ) %>%
    arrange(!!sym(colnames(event_data)[1]), row_index) %>%
    select(-row_index) %>%
    arrange(rowSums(is.na(.))) %>%
    distinct(!!sym(colnames(event_data)[1]), .keep_all = TRUE) %>%
    relocate(any_of(grep("Summary$",names(.),value = T)), .after = last_col()) %>%
    rename_at(vars(-c(!!sym(colnames(event_data)[1]),`Time On Record`)), ~ paste0(.," Events")) %>%
    as.data.frame()
  return(event_data_yor)
}



feat_breakdown <- function(df = NULL,id_col = NULL,ref_feat = NULL,feats = NULL, shiny = FALSE) {
  if (is.null(df)) stop("Please provide data frame.")
  if (is.null(id_col)) id_col <- colnames(df)[1]
  
  df2 <- unique(df[,which(colnames(df) %in% c(id_col,ref_feat,feats))])
  df2 <- df2[which(!is.na(df2[,id_col])),]
  
  if (!is.null(ref_feat)) {
    feats_tabs <- as.data.frame(data.table::rbindlist(lapply(feats, function(x) {
      if (suppressWarnings(length(which(is.na(as.numeric(df2[,x])))) > (length(df2[,x])*0.8)) | any(is.logical(df2[,x]))) {
        #if (suppressWarnings(all(is.na(as.numeric(df2[,x])))) | any(is.logical(df2[,x]))) {
        df3 <- df2 %>%
          select(!!sym(id_col),!!sym(ref_feat),!!sym(x)) %>%
          unique() %>%
          group_by(!!sym(ref_feat),!!sym(x)) %>%
          summarize(count = n(), .groups = "drop")
        num_feat <- FALSE
      } else {
        df3 <- df2 %>%
          select(!!sym(id_col),!!sym(ref_feat),!!sym(x)) %>%
          unique() %>%
          group_by(!!sym(ref_feat)) %>%
          summarize(Min = round(min(as.numeric(!!sym(x)), na.rm = T),2),
                    Max = round(max(as.numeric(!!sym(x)), na.rm = T),2),
                    Median = round(median(as.numeric(!!sym(x)), na.rm = T),2),
                    Mean = round(mean(as.numeric(!!sym(x)), na.rm = T),2)) %>%
          pivot_longer(!(!!sym(ref_feat)),
                       names_to = x,
                       values_to = "Count")
        num_feat <- TRUE
      }
      
      #df3[is.na(df3[,2]),2] <- "NA"
      colnames(df3) <- c("Ref_Feature","Feature","Count")
      df3_cast <- reshape2::dcast(df3,Feature ~ Ref_Feature, value.var = "Count")
      if (!num_feat) {
        df3_cast <- rbind(c(Feature = "Total (non-NA)",apply(df3_cast[which(!is.na(df3_cast[,1])),-1],2,function(x) {
          sum(x,na.rm = T)
        })),df3_cast)
      }
      df3_cast2 <- rbind(c(paste0("<strong>",x,"</strong>"),rep("---",(ncol(df3_cast)-1))),
                         df3_cast)
      return(df3_cast2)
    })))
  } else {
    feats_tabs <- as.data.frame(data.table::rbindlist(lapply(feats,function(x) {
      df2 <- unique(df2[,c(id_col,x)])
      vec <- df2[,x]
      if (suppressWarnings(length(which(is.na(as.numeric(vec)))) > (length(vec)*0.8)) | any(is.logical(df2[,x]))) {
        #if (suppressWarnings(all(is.na(as.numeric(vec)))) | any(is.logical(df2[,x]))) {
        x_df <- as.data.frame(table(vec, useNA = "ifany"))
        x_df[,1] <- as.character(x_df[,1])
        x_df <- rbind(c("Total (non-NA)",sum(x_df[which(!is.na(x_df[,1])),2])),
                      x_df)
        x_df[is.na(x_df)] <- "NA"
        colnames(x_df) <- c("Feature","Count")
        x_df <- rbind(data.frame(Feature = paste0("<strong>",x,"</strong>"),
                                 Count = "---"),
                      x_df)
        return(x_df)
      } else {
        vec <- suppressWarnings(as.numeric(vec))
        x_df <- data.frame(Feature = c("Min","Max","Median","Mean"),
                           Count = round(c(min(vec, na.rm = T),
                                           max(vec, na.rm = T),
                                           median(vec, na.rm = T),
                                           mean(vec, na.rm = T)),2))
        x_df <- rbind(data.frame(Feature = paste0("<strong>",x,"</strong>"),
                                 Count = "---"),
                      x_df)
        return(x_df)
      }
    })))
  }
  if (!shiny) {
    feats_tabs[,1] <- gsub("<strong>|<\\/strong>","",feats_tabs[,1])
  }
  feats_tabs[which(is.na(feats_tabs[,1])),1] <- "NA"
  feats_tabs[is.na(feats_tabs)] <- 0
  #colnames(feats_tabs)[-1] <- paste0(id_col," ",colnames(feats_tabs)[-1])
  if (!is.null(ref_feat)) {
    colnames(feats_tabs)[-1] <- paste0(ref_feat," ",colnames(feats_tabs)[-1])
  } else {
    colnames(feats_tabs)[-1] <- paste0(id_col," ",colnames(feats_tabs)[-1])
  }
  return(feats_tabs)
}

perform_fishers_test <- function(df, reference_col) {
  feat_cols <- colnames(df)[-1]
  feats_noRef <- feat_cols[which(feat_cols != reference_col)]
  # Identify feature groups
  group_indices <- which(df[, 2] == "---")
  df$FeatureGroup <- NA  # Column to store grouping information
  
  
  for (i in seq_along(group_indices)) {
    start_idx <- group_indices[i]
    end_idx <- ifelse(i < length(group_indices), group_indices[i + 1] - 1, nrow(df))
    df$FeatureGroup[start_idx:end_idx] <- df$Feature[start_idx]
  }
  put_back <- NULL
  if (all(c("Min","Max","Median","Mean") %in% df$Feature)) {
    remove_groups <- unique(df[which(df$Feature %in% c("Min","Max","Median","Mean")),"FeatureGroup"])
    put_back <- df[which(df$FeatureGroup %in% remove_groups),]
    df <- df[which(!df$FeatureGroup %in% remove_groups),]
  }
  
  # Convert count columns to numeric, replacing "---" with NA
  numeric_cols <- names(df)[2:(ncol(df) - 1)]
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) as.numeric(replace(x, x == "---", NA)))
  
  # Initialize result columns
  #df$OddsRatio <- NA
  #df$PValue <- NA
  
  # Check if reference column exists
  if (!(reference_col %in% colnames(df))) {
    stop("Reference column not found in dataframe.")
  }
  
  # Perform Fisher's exact test within each feature group
  for (group in unique(df$FeatureGroup[!is.na(df$FeatureGroup)])) {
    group_data <- df[df$FeatureGroup == group & df[, 2] != "---", ]  # Exclude header row
    #group_data <- df[df$FeatureGroup == group, ]  # Exclude header row
    
    if (nrow(group_data) > 2) {
      for (i in seq_len(nrow(group_data))) {
        feature_row <- group_data[i, ]
        
        # Ensure reference column is not NA
        if (is.na(feature_row[[reference_col]])) next
        
        # Contingency table (2x2)
        cont_table <- matrix(c(
          feature_row[[reference_col]], sum(feature_row[which(names(feature_row) %in% feats_noRef)], na.rm = TRUE),
          sum(group_data[[reference_col]], na.rm = TRUE) - feature_row[[reference_col]],
          sum(group_data[which(names(group_data) %in% feats_noRef)], na.rm = TRUE) - sum(feature_row[which(names(feature_row) %in% feats_noRef)], na.rm = TRUE)
        ), nrow = 2, byrow = TRUE)
        #cont_table <- matrix(c(
        #  feature_row[[reference_col]], sum(feature_row[-which(names(feature_row) %in% c("Feature", "FeatureGroup", "OddsRatio", "PValue", reference_col))], na.rm = TRUE),
        #  sum(group_data[[reference_col]], na.rm = TRUE) - feature_row[[reference_col]],
        #  sum(group_data[-which(names(group_data) %in% c("Feature", "FeatureGroup", "OddsRatio", "PValue", reference_col))], na.rm = TRUE) - sum(feature_row[-which(names(feature_row) %in% c("Feature", "FeatureGroup", "OddsRatio", "PValue", reference_col))], na.rm = TRUE)
        #), nrow = 2, byrow = TRUE)
        
        # Perform Fisher's Exact Test if valid
        if (all(cont_table >= 0) && all(rowSums(cont_table) > 0) && all(colSums(cont_table) > 0)) {
          test <- fisher.test(cont_table)
          df[df$Feature == feature_row$Feature, paste0(reference_col," OddsRatio")] <- round(test$estimate,4)
          df[df$Feature == feature_row$Feature, paste0(reference_col," FishersTest Pvalue")] <- round(test$p.value,4)
          #df[df$Feature == feature_row$Feature, "OddsRatio"] <- test$estimate
          #df[df$Feature == feature_row$Feature, "PValue"] <- test$p.value
        }
      }
    }
    
  }
  
  if (!is.null(put_back)) {
    df <- as.data.frame(data.table::rbindlist(list(df,put_back), fill = T))
  }
  df <- df[,-which(colnames(df) == "FeatureGroup")]
  df[group_indices,-1] <- "---"
  
  return(df)
}


# Function to convert spelled-out numbers to numeric values
word_to_number <- function(word) {
  word_map <- c(
    "first" = 1, "second" = 2, "third" = 3, "fourth" = 4, "fifth" = 5,
    "sixth" = 6, "seventh" = 7, "eighth" = 8, "ninth" = 9, "tenth" = 10,
    "eleventh" = 11
  )
  
  # Convert the word to lowercase and find the corresponding number
  word <- tolower(word)
  if (word %in% names(word_map)) {
    return(word_map[word])
  }
  return(NA)  # Return NA for non-matching words
}

# Function to sort the input vector
sort_spelled_numbers <- function(input) {
  # Extract the numbers from the input strings
  numbers <- sapply(input, function(x) {
    # Match the first word in the string that is a spelled-out number
    x <- tolower(x)
    match <- str_extract(x, "(first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth|eleventh)")
    return(word_to_number(match))
  })
  
  # Separate numeric and non-numeric items
  numeric_items <- input[!is.na(numbers)]
  non_numeric_items <- input[is.na(numbers)]
  
  # Order the numeric items based on their numeric values
  sorted_numeric_items <- numeric_items[order(numbers[!is.na(numbers)])]
  
  # Return the ordered items, with non-numeric items at the end
  return(c(sorted_numeric_items, non_numeric_items))
}


assign_clusters <- function(sub_df, col1 = "EventStart", cluster_window = 1) {
  cluster_window <- cluster_window / 12
  starts <- sub_df[[col1]]
  diffs <- c(0, diff(starts))
  cluster <- cumsum(diffs > cluster_window) + 1
  return(cluster)
}



apply_find_clusters_to_patients <- function(event_data, event_summary = "Treatment",
                                            name_col = "Name", event_col = "Event", eventtype_col = "EventType",
                                            eventtab_col = "EventTab", eventcolumn_col = "EventColumn",
                                            eventstart_col = "EventStart", eventend_col = "EventEnd",
                                            cluster_window = 1, verbose = TRUE) {
  event_data_sub <- event_data
  # Split data by patient
  patient_groups <- split(event_data_sub, event_data_sub[[1]])
  num_patients <- length(patient_groups)
  if (verbose) {
    pb <- txtProgressBar(min = 0, max = num_patients, style = 3)
  }
  patient_groups_cat_out <- lapply(seq_along(patient_groups),function(x) {
    # subset patient data
    patient_data <- patient_groups[[x]]
    patient_data <- patient_data %>%
      arrange(!!sym(name_col),!!sym(eventstart_col),!!sym(eventend_col)) %>%
      as.data.frame()
    if (all(patient_data$Event == patient_data$EventType)) {
      # Get overall treatment/response event summary
      overall_cls <- assign_clusters(sub_df = patient_data3, col1 = eventstart_col, cluster_window = cluster_window)
      patient_data4 <- patient_data3 %>%
        # format treatment/response event summary column
        mutate(event_col_save = !!sym(event_col),
               !!sym(event_col) := paste0(event_summary," Summary Cluster ",overall_cls),
               !!sym(eventtype_col) := paste0("Full ",event_summary," Summary")) %>%
        # reformat event event data
        mutate(EventSummary = paste0(unique(event_col_save), collapse = "\n"),
               !!sym(eventtab_col) := NA,
               !!sym(eventcolumn_col) := NA,
               !!sym(eventstart_col) := min(!!sym(eventstart_col),na.rm = T),
               !!sym(eventend_col) := max(!!sym(eventend_col),na.rm = T), .by = !!sym(event_col)) %>%
        select(-event_col_save) %>%
        distinct() %>%
        as.data.frame()
      patient_data5 <- patient_data4
      if (verbose) setTxtProgressBar(pb, x)
      return(patient_data5)
    } else {
      patient_groups_cat <- split(patient_data,patient_data[,eventtype_col])
      # get patient clusters
      pat_cls <- lapply(seq_along(patient_groups_cat),function(y) {
        df <- patient_groups_cat[[y]]
        cls <- assign_clusters(sub_df = df, col1 = eventstart_col, cluster_window = cluster_window)
        df$EventCluster <- cls
        return(df)
      })
      patient_data2 <- as.data.frame(rbindlist(pat_cls))
      patient_data2$EventCluster <- paste0(patient_data2[,eventtype_col]," Cluster ",patient_data2$EventCluster)
      # Format event summary column
      patient_data3 <- patient_data2 %>%
        # remove event type from event name - simplify event name
        mutate(!!sym(event_col) := gsub(unique(paste0(!!sym(eventtype_col),": ")),"",!!sym(event_col)), .by = !!sym(eventtype_col)) %>%
        # Format event summary column of events in cluster
        mutate(EventSummary = paste0(c(unique(!!sym(eventtype_col)),paste0("  ",sort(unique(!!sym(event_col))), collapse = "\n")), collapse = "\n"),
               !!sym(eventtab_col) := NA,
               !!sym(eventcolumn_col) := NA,
               !!sym(eventstart_col) := min(!!sym(eventstart_col),na.rm = T),
               !!sym(eventend_col) := max(!!sym(eventend_col),na.rm = T), .by = EventCluster) %>%
        # replace event name with event cluster name
        mutate(!!sym(event_col) := EventCluster,
               !!sym(eventtype_col) := paste0(!!sym(eventtype_col)," Summary")) %>%
        select(-EventCluster) %>%
        distinct() %>%
        arrange(!!sym(name_col),!!sym(eventstart_col),!!sym(eventend_col)) %>%
        as.data.frame()
      # Get overall treatment/response event summary
      overall_cls <- assign_clusters(sub_df = patient_data3, col1 = eventstart_col, cluster_window = cluster_window)
      patient_data4 <- patient_data3 %>%
        # format treatment/response event summary column
        mutate(!!sym(event_col) := paste0(event_summary," Summary Cluster ",overall_cls),
               !!sym(eventtype_col) := paste0("Full ",event_summary," Summary")) %>%
        # reformat event event data
        mutate(EventSummary = paste0(unique(EventSummary), collapse = "\n"),
               !!sym(eventstart_col) := min(!!sym(eventstart_col),na.rm = T),
               !!sym(eventend_col) := max(!!sym(eventend_col),na.rm = T), .by = !!sym(event_col)) %>%
        distinct() %>%
        as.data.frame()
      patient_data5 <- as.data.frame(rbindlist(list(patient_data4,patient_data3)))
      if (verbose) setTxtProgressBar(pb, x)
      return(patient_data5)
    }
  })
  if (verbose) close(pb)
  clustered_data <- do.call(rbind,patient_groups_cat_out)
  return(clustered_data)
}



eventDataSummary <- function(event_data = NULL, event_summary = "Treatment",
                             event_col = "Event", eventtype_col = "EventType",
                             eventstart_col = "EventStart", eventend_col = "EventEnd",
                             cluster_window = 1, verbose = TRUE) {
  if (nrow(event_data) > 0) {
    event_data_tr_cls <- apply_find_clusters_to_patients(event_data, event_summary = event_summary, verbose = verbose, cluster_window = cluster_window)
  } else {
    event_data_tr_cls <- NULL
  }
  return(event_data_tr_cls)
}

convert_time_units <- function(x, unit_in, unit_out) {
  unit_in <- tolower(unit_in)
  unit_out <- tolower(unit_out)
  to_days <- c(hour = 1/24, days = 1, months = 30.44, years = 365.25)
  valid_units <- names(to_days)
  if (!(unit_in %in% valid_units)) stop("Invalid 'unit_in'. Must be one of: hours, days, months, years.")
  if (!(unit_out %in% valid_units)) stop("Invalid 'unit_out'. Must be one of: hours, days, months, years.")
  if (!is.numeric(x)) stop("'x' must be a numeric vector.")
  numbers_in_days <- x * to_days[unit_in]
  converted_numbers <- numbers_in_days / to_days[unit_out]
  return(converted_numbers)
}

# Added optional sort, saved 0.02 secs
data_to_event <- function(df = NULL,
                          table_name = NULL,
                          event_name = NULL,
                          col_defined_event = FALSE,
                          event_category = NULL,
                          event_start_col = NULL,
                          event_end_col = NULL,
                          event_start_units = NULL,
                          event_end_units = NULL,
                          sort = FALSE) {
  if (length(event_name) > 1) {
    event_name_splt <- event_name
  } else {
    event_name_splt <- strsplit(event_name,",")[[1]]
  }
  if (col_defined_event) {
    if (length(event_name_splt) > 1) {
      event_col <- apply(df[,event_name_splt],1,function(x){paste0(x,collapse = "-")})
    } else {
      event_col <- df[,event_name]
    }
  } else {
    event_col <- event_name
  }
  df_event_data <- unique(data.frame(Name = df[,1],
                                     Event = event_col,
                                     EventType = event_category,
                                     EventTab = table_name,
                                     EventStart = suppressWarnings(as.numeric(df[,event_start_col])),
                                     EventEnd = suppressWarnings(as.numeric(df[,event_end_col])),
                                     EventColumn = event_name))
  df_event_data <- df_event_data[which(!is.na(df_event_data$Event)),]
  if (col_defined_event) {
    if (nrow(df_event_data) > 0) {
      df_event_data$Event <- paste0(df_event_data$EventType,": ",df_event_data$Event)
    }
  }
  df_event_data[,"EventStart"] <- convert_time_units(df_event_data[,"EventStart"],event_start_units,"years")
  df_event_data[,"EventEnd"] <- convert_time_units(df_event_data[,"EventEnd"],event_end_units,"years")
  if (sort) {
    df_event_data <- df_event_data %>%
      arrange(EventStart,EventEnd)
  }
  return(df_event_data)
}




getEventData <- function(param = NULL,data = NULL, summary = TRUE, read_files = FALSE, cluster_window = 1, verbose = TRUE) {
  if (is.null(param)) stop("Must provide file parameter data.")
  if (is.null(data) & !read_files) stop("Must provide list of data frames.")
  param_cols <- c("Data Table Name","Data File","Event Name","Column Defined Event","Event Category","Event Start Column",
                  "Event End Column","Treatment","Response","Event Start Time Units","Event End Time Units")
  if (!all(colnames(param) == param_cols)) {
    stop("Please check parameter file header names or column order.")
  }
  param <- param[which(!is.na(param[,"Event Name"])),]
  param[,"Event End Column"] <- ifelse(is.na(param[,"Event End Column"]),param[,"Event Start Column"],param[,"Event End Column"])
  param[,"Event Start Time Units"] <- ifelse(is.na(param[,"Event Start Time Units"]),"years",param[,"Event Start Time Units"])
  param[,"Event End Time Units"] <- ifelse(is.na(param[,"Event End Time Units"]),"years",param[,"Event End Time Units"])
  if (read_files & is.null(data)) {
    if (verbose) message("Reading in data files")
    data_files <- unique(param[,c(1,2)])
    if (nrow(data_files) > 1) {
      data <- lapply(data_files[,2], function(x) {
        return(as.data.frame(fread(x,na.strings = c("","NA"))))
      })
      names(data) <- data_files[,1]
    }
  }
  if (verbose) message("Formatting Event Data")
  event_data_list <- lapply(seq_along(data),function(data_tab) {
    table_name <- names(data)[data_tab]
    df <- data[[data_tab]]
    if (table_name %in% param[,1]) {
      param_sub <- param[which(param[,1] == table_name),]
      event_data_list_sub <- apply(param_sub,1,function(row) {
        data_to_event(df, table_name, event_name = row[[3]], col_defined_event = row[[4]],
                      event_category = row[[5]], event_start_col = row[[6]], event_end_col = row[[7]],
                      event_start_units = row[[10]], event_end_units = row[[11]])
        
      })
      event_data_list_tab <- unique(as.data.frame(data.table::rbindlist(event_data_list_sub)))
      event_data_list_tab$EventEnd <- ifelse(is.na(event_data_list_tab$EventEnd),event_data_list_tab$EventStart,event_data_list_tab$EventEnd)
      event_data_list_tab$EventStart <- ifelse(is.na(event_data_list_tab$EventStart),event_data_list_tab$EventEnd,event_data_list_tab$EventStart)
      event_data_list_tab <- event_data_list_tab[complete.cases(event_data_list_tab),]
      return(event_data_list_tab)
    }
  })
  event_data <- unique(as.data.frame(data.table::rbindlist(event_data_list)))
  event_data <- event_data[complete.cases(event_data),]
  if (!summary) {
    event_data <- event_data[order(event_data[,1]),]
    return(event_data)
  } else {
    if (verbose) message("Summarizing Event Data")
    treatment_events <- unique(param[which(param$Treatment == TRUE),])
    treatment_events <- ifelse(treatment_events$`Column Defined Event` == FALSE,treatment_events$`Event Name`,
                               paste0(treatment_events$`Event Category`,": "))
    response_events <- unique(param[which(param$Response == TRUE),])
    response_events <- ifelse(response_events$`Column Defined Event` == FALSE,response_events$`Event Name`,
                              paste0(response_events$`Event Category`,": "))
    event_data_tr <- event_data[grepl(paste(treatment_events,collapse = "|"),event_data$Event),]
    event_data_re <- event_data[grepl(paste(response_events,collapse = "|"),event_data$Event),]
    if (verbose) message("Summarizing Treatment Event Data")
    event_data_tr_cls <- eventDataSummary(event_data_tr, event_summary = "Treatment", verbose = verbose, cluster_window = cluster_window)
    if (verbose) message("Summarizing Response Event Data")
    event_data_re_cls <- eventDataSummary(event_data_re, event_summary = "Response", verbose = verbose, cluster_window = cluster_window)
    event_data_cls <- as.data.frame(rbindlist(list(event_data_tr_cls,event_data_re_cls)))
    event_data_cls$Event <- gsub("Cluster \\d+$","Cluster",event_data_cls$Event)
    event_data_cls <- event_data_cls %>%
      group_by(Name) %>%
      arrange(!EventType %in% c("Full Treatment Summary","Full Response Summary"), .by_group = TRUE)
    event_data_cls_all <- data.table::rbindlist(list(event_data_cls,event_data), fill = T)
    event_data_cls_all <- event_data_cls_all[order(event_data_cls_all[,1]),]
    event_data_cls_all <- as.data.frame(event_data_cls_all)
    return(event_data_cls_all)
  }
}


ColorPalSelect_UI <- function(id, default_col = "Standard colors") {
  ns <- NS(id)
  qual_cols <- palette.pals()
  qual_cols <- qual_cols[qual_cols!="R3"]
  names(qual_cols) <- sapply(qual_cols, function(pal) { paste0(pal," (",length(palette.colors(palette = pal)),")")})
  seq_cols <- hcl.pals("sequential")
  div_cols <- hcl.pals("diverging")
  ColorOptList <- list("Standard colors",Qualitative = qual_cols,Sequential = seq_cols, Diverging = div_cols)
  fluidRow(
    column(6,
           shiny::selectInput(ns("PalSelect"),
                              label = tooltip(
                                trigger = list(
                                  "Select Color Palette:",
                                  bsicons::bs_icon("info-circle")
                                ),
                                "If finite, number of possible colors in palette shown in parentheses."
                              ),
                              choices = ColorOptList, selected = default_col)
    ),
    column(6,
           shiny::actionButton(ns("PalSelect_mod"),"Available Palettes", icon = icon("palette"), width = "100%",
                               style = "background-color: #2c3e50; border-color: #2c3e50; margin-top:30px")
    )
  )
}



ColorPalSelect_server <- function(id){
  shiny::moduleServer(id, 
                      function(input, output, session){
                        observeEvent(input$PalSelect_mod, {
                          showModal(modalDialog(
                            title = "Available Color Palettes",
                            size = "l",
                            easyClose = TRUE,
                            tabsetPanel(id = "ColorPalPanels",
                                        tabPanel("Qualitative",
                                                 tags$img(src = "QualitativePaletteColors_R.png", height = "450px", width = "100%")
                                        ),
                                        tabPanel("Sequential",
                                                 tags$img(src = "SequentialHCLColors_R.png", height = "1000px", width = "100%")
                                        ),
                                        tabPanel("Diverging",
                                                 tags$img(src = "DivergingHCLColors_R.png", height = "800px", width = "100%")
                                        )
                            )
                          ))
                        })
                        return(reactive(input$PalSelect))
                      }
  )
}


min_max_breaks <- function(...) {
  function(x) {
    rng <- range(x, na.rm = TRUE)
    c(min(rng),max(rng))
  }
}


margin_adjust <- function(top,bot,lite = FALSE,top_new = NULL, bot_new = NULL) {
  top_new <- ifelse(is.null(top_new),(top+10),top_new)
  bot_new <- ifelse(is.null(bot_new),(bot+10),bot_new)
  
  if (lite) {
    top <- ifelse(!is.na(top) & top<0,top_new,top)
    bot <- ifelse(!is.na(bot) & bot<0,bot_new,bot)
  }
  top_p <- ifelse(!is.na(top),paste0("margin-top:",top,"px"),NA)
  bot_p <- ifelse(!is.na(bot),paste0("margin-bottom:",bot,"px"),NA)
  out_p <- c(top_p,bot_p)
  out_p <- out_p[!is.na(out_p)]
  return(paste0(out_p,collapse = ";"))
}


