
getColors <- function() {
  
  # cols.op <- c(# rgb(249, 222,  55, 255, max = 255),
  #              # rgb(255, 147,  41, 255, max = 255), # orange:  21, 255, 148
  #              
  #              rgb(114,  11,  29,  255, max = 255), # red:    229,  22,  54 YORK
  #              #rgb(207,   0, 216, 255, max = 255), # pink:   211, 255, 108
  #              rgb(255,  64, 216, 255, max = 255), # violet: 195, 255, 108
  #              rgb(  0, 220, 140, 255, max = 255), # green:   0, 255, 140
  #              rgb(  0,  19, 136, 255, max = 255)) # blue:   164, 255, 68
  
  # cols.tr <- c(# rgb(249, 222,  55, 32, max = 255),
  #   # rgb(255, 147, 41,  32,  max = 255), # orange:  21, 255, 148
  #   
  #   rgb(114,  11,  29,  32,  max = 255), # red:    248, 210, 126
  #   # rgb(207, 0,   216, 32,  max = 255), # pink:   211, 255, 108
  #   rgb(255,  64, 216, 32,  max = 255), # violet: 195, 255, 108 VIOLET: 127, 0,   216
  #   rgb(  0, 220, 140, 32, max = 255), # green:   0, 255, 140
  #   rgb(0,   19,  136, 32,  max = 255)) # blue:   164, 255, 68
  
  cols.op <- c(
              
               # rgb(t(col2rgb("hotpink")), max=255),
               # rgb(t(col2rgb("turquoise")), max=255),
               rgb(t(col2rgb("magenta")), max=255),
               rgb(t(col2rgb("dodgerblue")), max=255),
               rgb(t(col2rgb("navyblue")), max=255),
               rgb(t(col2rgb("darkred")), max=255)
              )
  
  # print(cols.op)
  
  cols.tr <- c(
    Reach::colorAlpha(col=cols.op[1], alpha=32),
    Reach::colorAlpha(col=cols.op[2], alpha=32),
    Reach::colorAlpha(col=cols.op[3], alpha=32),
    Reach::colorAlpha(col=cols.op[4], alpha=32)
  )
  
  cols <- list()
  cols$op <- cols.op
  cols$tr <- cols.tr
  
  return(cols)
  
}

# Exp 1 -----

## exploratory plots ----

plotExp1participant <- function(df, participant) {
  
  block0 <- getZeroBlocks(df)
  
  psdf <- df[which(df$ppid_full == participant),]
  
  plot(NULL,NULL,
       xlim=c(20,497), ylim=c(-100,100),
       main=participant, xlab='trial', ylab='min_x_delta_bc (cm)',
       axes=FALSE, bty='n')
  
  for (br in 1:nrow(block0)) {
    rect(block0$start[br], -100, block0$end[br], 100, col=rgb(0,0,0,24,max=255), border=NA)
  }
  lines(x=c(20,497), y=c(0,0), col='black', lty=2)
  colors <- getColors()
  
  targ_idx <- list('-0.6'=1, '-0.3'=2, '0.3'=3, '0.6'=4)
  
  for (phase in c('baseline','rest')) {
    if (phase == 'baseline') {
      phdf <- psdf[which(psdf$phase == 'baseline'),]
    } else {
      phdf <- psdf[which(psdf$phase != 'baseline'),]
    }
    
    for (target_pos in names(targ_idx)) {
      
      col.idx <- targ_idx[[target_pos]]
      
      # t_idx <- which(psdf$target_position_x == as.numeric(target_pos))
      # tdat <- psdf[t_idx,]
      # lines(tdat$trial_num, tdat$min_x_delta_bc, col=colors$op[col.idx], lwd=2)

      t_idx <- which(phdf$target_position_x == as.numeric(target_pos))
      tdat <- phdf[t_idx,]
      lines(tdat$trial_num, tdat$min_x_delta_bc, col=colors$op[col.idx], lwd=1)
      
    }
  }
  
  legend(x=56, y=100, legend=names(targ_idx), col=colors$op[c(1,2,3,4)], lwd=2, bty='n')
  
  axis(1, at=seq(20,496,28))
  axis(2, at=seq(-100,100,50))
  
  
}

getZeroBlocks <- function(df) {
  zero.idx <- sort(unique(df$trial_num[which(df$water_speed_m_s == 0)]))
  lds <- which(diff(zero.idx) > 1)
  
  B.ends <- c(zero.idx[lds], zero.idx[length(zero.idx)])
  B.starts <- zero.idx[c(1, lds + 1)]
  
  blocks <- data.frame(start=B.starts-0.5, end=B.ends+0.5)
  return(blocks)
}

allParticiantPlotsExp1 <- function() {
  
  for (waterspeed in c(-2, -3)) {
    df <- read.csv(paste0("data/df_", waterspeed, "_bc.csv"))
    participants <- unique(df$ppid_full)
    
    pdf(paste0("doc/exp1_participants_", waterspeed, ".pdf"), width=8.5, height=11)
    layout(mat=matrix(data = c(1:3), ncol=1))    
    for (participant in participants) {
      plotExp1participant(df, participant)
    }
    dev.off()
  }
  
}

## group average plots -----

groupAveragesExp1 <- function(target='inline') {
  
  if (target=='pdf') {
    pdf("doc/exp1_averages.pdf", width=11, height=8.5)
  }
  colors <- getColors()
  
  # block0 <- getZeroBlocks(df)
  
  layout(mat=matrix(data = c(1,2), ncol=1))
  
  for (waterspeed in c(-2, -3)) {
    df <- read.csv(paste0("data/df_", waterspeed, "_bc.csv"))
    
    block0 <- getZeroBlocks(df)
    
    # get the average min_x_delta_bc for each trial and target position:
    avg_df <- aggregate(min_x_delta_bc ~ trial_num + target_position_x, data = df, FUN = mean)
    CI_df  <- aggregate(min_x_delta_bc ~ trial_num + target_position_x, data = df, FUN = Reach::getConfidenceInterval, method='b')
    
    # plot the average min_x_delta_bc for each trial and target position:
    plot(NULL,NULL,
         xlim=c(20,497), ylim=c(-70,70),
         main=paste0("Water speed: ", waterspeed, " m/s"), xlab='trial', ylab='average min_x_delta_bc (cm)',
         axes=FALSE, bty='n')
    
    for (br in 1:nrow(block0)) {
      rect(block0$start[br], -100, block0$end[br], 100, col=rgb(0,0,0,24,max=255), border=NA)
    }
    lines(x=c(20,497), y=c(0,0), col='black', lty=2)
    
    targ_idx <- list('-0.6'=1, '-0.3'=2, '0.3'=3, '0.6'=4)
    
    for (target_pos in names(targ_idx)) {
      
      col.idx <- targ_idx[[target_pos]]
      
      t_idx <- which(CI_df$target_position_x == as.numeric(target_pos))
      tdat <- CI_df[t_idx,]
      # print(str(tdat))
      x <- c(tdat$trial_num, rev(tdat$trial_num))
      # print(length(x))
      y <- c(tdat$min_x_delta_bc[,1], rev(tdat$min_x_delta_bc[,2]))
      polygon(x=x, 
              y=y, 
              col=colors$tr[col.idx], border=NA)
      
      t_idx <- which(avg_df$target_position_x == as.numeric(target_pos))
      tdat <- avg_df[t_idx,]
      lines(tdat$trial_num, tdat$min_x_delta_bc, col=colors$op[col.idx], lwd=1)
      
    }
    
    if (waterspeed == -2) {
      legend(x=56, y=67, legend=names(targ_idx), col=colors$op[c(1,2,3,4)], lwd=2, bty='n', ncol=2)
    }
    # legend(x=56, y=100, legend=names(targ_idx), col=colors$op[c(1,2,3,4)], lwd=2, bty='n')
    
    axis(1, at=seq(20,496,28))
    axis(2, at=seq(-60,60,30))
    
  }
  
  if (target %in% c('pdf')) {
    dev.off()
  }
  
}