preprocessExp1File <- function() {
  
  
  df <- read.csv("data/df_full_bc.csv", header = TRUE, stringsAsFactors = FALSE)
  
  df$ppid_full[which(df$ppid_full == "p029j_neg2")] <- "p029_neg2"
  df$ppid_full[which(df$ppid_full == "1_neg3")]     <- "p001_neg3"
  
  
  
  for (waterspeed in c(-2, -3)) {
    sdf <- df[which(df$speed_label == waterspeed),]
    
    labelling <- list('neg0.3'='L30',
         'neg0.6'='L60',
         'p0.3'='R30',
         'p0.6'='R60')

    for (label in names(labelling)) {
      idx <- which(sdf$target_x_label == label)
      sdf$target_x_label[idx] <- labelling[[label]]
    }
    
    ws_df <- NA
    bl_df <- NA
    
    # recalculate: min_x_delta_bc
    participants <- unique(sdf$ppid_full)
    
    for (id in participants) {
      # data for the participant:
      psdf <- sdf[which(sdf$ppid_full == id),]
      # baseline corrected minimum x delta (min_x_delta_bc) goes in this column:
      psdf$min_x_delta_bc <- NA
      
      # identify baseline trial indices:
      bidx <- psdf$trial_num[which(psdf$phase == "baseline")]
      # bidx <- bidx[which(abs(psdf$min_x_delta[bidx]) < 50)]
      # get a dataframe with the baseline trials for this participant:
      bdat <- psdf[which(psdf$trial_num %in% bidx),]
      # remove baseline trials with large errors (abs(min_x_delta) > 50 cm):
      bdat <- bdat[which(abs(bdat$min_x_delta) < 50),]
      
      # average min_x_delta for each target in the baseline phase:
      baseline <- aggregate(min_x_delta ~ target_x_label, data = bdat, FUN = mean)
      # loop through targets:
      for (target in unique(baseline$target_x_label)) {
        # get the baseline value for this target:
        bval <- baseline$min_x_delta[which(baseline$target_x_label == target)]
        # print(bval)
        # identify the indices for all trials this target:
        idx <- which(psdf$target_x_label == target)
        # baseline correct the min_x_delta for these trials:
        psdf$min_x_delta_bc[idx] <- psdf$min_x_delta[idx] - bval
      }
      # append the participant's dataframe to the waterspeed dataframe:
      if (is.data.frame(ws_df)) {
        ws_df <- rbind(ws_df, psdf)
      } else {
        ws_df <- psdf
      }
      
      baseline$ppid_full <- id
      if (is.data.frame(bl_df)) {
        bl_df <- rbind(bl_df, baseline)
      } else {
        bl_df <- baseline
      }
    }
    
    write.csv(bl_df, paste0("data/df_ttt_", waterspeed, "_bl.csv"), row.names = FALSE)
    
    removed_trials <- 0
    # go through individual trials and exclude trials outside the 3 SD criterion per trial:
    for (trial in unique(ws_df$trial_num)) {
      tr_idx <- which(ws_df$trial_num == trial)
      trdat <- ws_df[tr_idx,]
      avg <- mean(trdat$min_x_delta_bc, na.rm = TRUE)
      std <- sd(trdat$min_x_delta_bc, na.rm = TRUE)
      idx <- which(ws_df$trial_num == trial & abs(ws_df$min_x_delta_bc - avg) > (3 * std))
      if (length(idx) > 0) {
        ws_df <- ws_df[-idx,]
      }
      removed_trials <- removed_trials + length(idx) 
    }
    
    write.csv(ws_df, paste0("data/df_", waterspeed, "_bc.csv"), row.names = FALSE)
    cat(sprintf('Speed %d: removed %d / %d trials\n', waterspeed, removed_trials, removed_trials + nrow(ws_df)))
  }
  
}


preprocessExp2file <- function() {
  
  sdf <- read.csv('data/Copy of PCA_error_TD_2.0M_spatial_generalization_TU_2.0M_spatial_generalization_speed_neg2_0_0_0.csv', header = TRUE, stringsAsFactors = FALSE)
  waterspeed = -2
  
  ws_df <- NA
  
  bl_df <- NA
  
  # recalculate: min_x_delta_bc
  participants <- unique(sdf$ppid_full)
  
  for (id in participants) {
    # data for the participant:
    psdf <- sdf[which(sdf$ppid_full == id),]
    # baseline corrected minimum x delta (min_x_delta_bc) goes in this column:
    psdf$min_x_delta_bc <- NA
    
    # identify baseline trial indices:
    bidx <- psdf$trial_num[which(psdf$phase == "baseline")]
    # bidx <- bidx[which(abs(psdf$min_x_delta[bidx]) < 50)]
    # get a dataframe with the baseline trials for this participant:
    bdat <- psdf[which(psdf$trial_num %in% bidx),]
    # remove baseline trials with large errors (abs(min_x_delta) > 50 cm):
    bdat <- bdat[which(abs(bdat$min_x_delta) < 50),]
    
    # average min_x_delta for each target in the baseline phase:
    baseline <- aggregate(min_x_delta ~ target_x_label, data = bdat, FUN = mean)
    # loop through targets:
    for (target in unique(baseline$target_x_label)) {
      # get the baseline value for this target:
      bval <- baseline$min_x_delta[which(baseline$target_x_label == target)]
      # print(bval)
      # identify the indices for all trials this target:
      idx <- which(psdf$target_x_label == target)
      # baseline correct the min_x_delta for these trials:
      psdf$min_x_delta_bc[idx] <- psdf$min_x_delta[idx] - bval
    }
    # append the participant's dataframe to the waterspeed dataframe:
    if (is.data.frame(ws_df)) {
      ws_df <- rbind(ws_df, psdf)
    } else {
      ws_df <- psdf
    }
    
    baseline$ppid_full <- id
    if (is.data.frame(bl_df)) {
      bl_df <- rbind(bl_df, baseline)
    } else {
      bl_df <- baseline
    }
    
  }
  
  write.csv(bl_df, paste0("data/df_stt_", waterspeed, "_bl.csv"), row.names = FALSE)
  
}

getDeepikasData <- function() {
  
  # for each target get the baseline
  # for each target that exists in training 1 and training 2, get the first and last 4 occurrence
  # for each target that exists in washout 1 and washout 2, get the first 4 occurrences
  
  # determine the group for each participant:
  # in exp 1:
  # - group 1: trained on -60 and 30 cm targets
  # - group 2: trained on -30 and 60 cm targets
  # in exp 2:
  # - group 1: trained on -60 cm target
  # - group 2: trained on 60 cm target
  
  data_collection <- list(
    'experiment' = c(),
    'ID' = c(),
    'group' = c(),
    'baseline_L30' = c(),
    'baseline_L60' = c(),
    'baseline_R30' = c(),
    'baseline_R60' = c(),
    'training_1_early_L60' = c(),
    'training_1_early_L30' = c(),
    'training_1_early_R30' = c(),
    'training_1_early_R60' = c(),
    'training_1_late_L60' = c(),
    'training_1_late_L30' = c(),
    'training_1_late_R30' = c(),
    'training_1_late_R60' = c(),
    'washout_1_L60' = c(),
    'washout_1_L30' = c(),
    'washout_1_R30' = c(),
    'washout_1_R60' = c(),
    'training_2_early_L60' = c(),
    'training_2_early_L30' = c(),
    'training_2_early_R30' = c(),
    'training_2_early_R60' = c(),
    'training_2_late_L60' = c(),
    'training_2_late_L30' = c(),
    'training_2_late_R30' = c(),
    'training_2_late_R60' = c(),
    'washout_2_L60' = c(),
    'washout_2_L30' = c(),
    'washout_2_R30' = c(),
    'washout_2_R60' = c()
  )
  
  phase_parts <- data.frame(
    phase = c('baseline', 'training_1', 'training_1', 'washout_1', 'training_2', 'training_2', 'washout_2'),
    part  = c(   'early',      'early',       'late',     'early',      'early',       'late',     'early')
  )
  targets <- c('L30', 'L60', 'R30', 'R60')
  
  for (exp in c(1, 2)) {
    if (exp == 1) {
      df <- read.csv("data/df_-2_bc.csv", header = TRUE, stringsAsFactors = FALSE)
      bl <- read.csv("data/df_ttt_-2_bl.csv", header = TRUE, stringsAsFactors = FALSE)
      exp_label <- 'ttt'
    } else if (exp == 2) {
      df <- read.csv("data/Copy of PCA_error_TD_2.0M_spatial_generalization_TU_2.0M_spatial_generalization_speed_neg2_0_0_0.csv", header = TRUE, stringsAsFactors = FALSE)
      bl <- read.csv("data/df_stt_-2_bl.csv", header = TRUE, stringsAsFactors = FALSE)
      exp_label <- 'stt'
    }
    df$ppid_full <- sprintf("%s_%s", exp_label, df$ppid_full)
    bl$ppid_full <- sprintf("%s_%s", exp_label, bl$ppid_full)
    
    participants <- unique(df$ppid_full)
    
    for (id in participants) {
      pdat <- df[which(df$ppid_full == id),]
      pbl <- bl[which(bl$ppid_full == id),]

      tr1_targets <- unique(pdat$target_x_label[which(pdat$phase == 'training_1')])
      
      # determine group:
      if (exp == 1) {
        if (all(c('L60', 'R30') %in% tr1_targets)) {
          group <- 'ttt_L60R30'
        } else if (all(c('L30', 'R60') %in% tr1_targets)) {
          group <- 'ttt_L30R60'
        } else {
          group <- NA
        }
      } else if (exp == 2) {
        if ('L60' %in% tr1_targets) {
          group <- 'stt_L60'
        } else if ('R60' %in% tr1_targets) {
          group <- 'stt_R60'
        } else {
          group <- NA
        }
      }
      
      data_collection$experiment <- c(data_collection$experiment, exp_label)
      data_collection$ID <- c(data_collection$ID, id)
      data_collection$group <- c(data_collection$group, group)
      
      for (partno in c(1:nrow(phase_parts))) {
        phase <- phase_parts$phase[partno]
        part <- phase_parts$part[partno]
        
        # print(c(phase, part))
        
        for (target in targets) {
          if (phase == 'baseline') {
            value <- pbl$min_x_delta[which(pbl$target_x_label == target)]
            col_name <- paste(phase, target, sep = '_')
          } else if (grepl('washout', phase)) {
            tdat <- pdat[which(pdat$phase == phase & pdat$target_x_label == target),]
            if (nrow(tdat) > 0) {
              value <- mean(tdat$min_x_delta_bc[1:min(4, nrow(tdat))], na.rm = TRUE)
            } else {
              value <- NA
            }
            col_name <- paste(phase, target, sep = '_')
          } else {
            tdat <- pdat[which(pdat$phase == phase & pdat$target_x_label == target),]
            if (nrow(tdat) > 0) {
              if (part == 'early') {
                value <- mean(tdat$min_x_delta_bc[1:min(4, nrow(tdat))], na.rm = TRUE)
              } else if (part == 'late') {
                value <- mean(tdat$min_x_delta_bc[max(1, nrow(tdat)-3):nrow(tdat)], na.rm = TRUE)
              }
            } else {
              value <- NA
            }
            col_name <- paste(phase, part, target, sep = '_')
          }
          
          
          data_collection[[col_name]] <- c(data_collection[[col_name]], value)
        }
      }
    }
  }
  # print(data_collection)
  data_df <- as.data.frame(data_collection)
  write.csv(data_df, "data/df_deepika.csv", row.names = FALSE)
  
}