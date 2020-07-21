## interval options need to be monotonically increasing
## either lower bound or upper bound needs to be further adjusted, here we adjust upper bound in list
calculateBucket <- function(min_val,max_val,max_bin=10,interval=10,interval_options=seq(10,500,10),center=100,floor_at=NULL,ceil_at=NULL){
  min_val_round <- plyr::round_any(min_val,interval,f=floor)
  max_val_round <- plyr::round_any(max_val,interval,f=ceiling)
  
  if (min_val_round == min_val){
    min_val_round <- min_val_round - interval # reduce by one group as the cut function takes (x1,x2]
  }
  
  if ((min_val_round == 0) & (max_val_round == 0)){
    delta <- max_val_round - min_val_round
    num_bin <- c(1)
    res_idex <- 1
    interval_plot <- 0
    breaks <- c(-interval,0,0,interval)
    
  }else if (min_val_round * max_val_round >= 0){
    ## when both are negative/positive/invovles 0
    delta <- max_val_round - min_val_round
    
    num_bin <- ceiling(delta / interval_options)
    res_idx <- which(num_bin < max_bin)[1]
    interval_plot <- interval_options[res_idx]
    
    # the range (max_val_round - min_val_round) needs to be adjusted to be a multiple of interval
    # then we do a shift to get the adjusted upper bound: adjusted range + min
    breaks <- seq(min_val_round,
                  plyr::round_any(max_val_round - min_val_round,interval,f=ceiling) + min_val_round,
                  interval_plot)
  }else{
    ## when one is positive and one is negative
    ## need to make sure 0 is always in the breaks
    delta_positive <- max_val_round
    delta_negative <- abs(min_val_round)
    
    num_bin <- ceiling(delta_positive / interval_options) + ceiling(delta_negative / interval_options)
    res_idx <- which(num_bin < max_bin)[1]
    interval_plot <- interval_options[res_idx]
    
    breaks <- c(seq(plyr::round_any(min_val_round,interval_plot,f=floor), 0, interval_plot), 
                seq(0,plyr::round_any(max_val_round,interval_plot,f=ceiling), interval_plot)[-1] # remove 0 so no duplicate 0
    )
  }
  
  breaks_adjusted_for_label <- breaks
  if (!is.null(floor_at)){
    if (min(breaks) < floor_at){
      breaks_adjusted_for_label <- c(max(floor_at,breaks_adjusted_for_label[1]), # e.g., floor the break to start from 0 for the original value
                                     breaks_adjusted_for_label[breaks_adjusted_for_label > floor_at])
    }
  }
  
  if (!is.null(ceil_at)){
    if (max(breaks) > ceil_at){
      breaks_adjusted_for_label <- c(breaks_adjusted_for_label[breaks_adjusted_for_label < ceil_at], # e.g., ceil the break to end by 100 for the original value
                                     min(ceil_at,breaks_adjusted_for_label[length(breaks_adjusted_for_label)]))
    }
  }
  
  return(list(num_bin=num_bin[res_idx],
              interval=interval_plot,
              breaks=breaks,
              breaks_label=sapply(1:length(breaks_adjusted_for_label), 
                                  function(x) if (x>1) paste0(plyr::round_any(breaks_adjusted_for_label[x-1]+center,interval),
                                                              ' - ',
                                                              plyr::round_any(breaks_adjusted_for_label[x]+center,interval)) else '')[-1]))
}


inferColor <- function(color_bucket,color_below='#e34a33',color_above='#31a354',color_center='white',interval=10,center=100){
  # if (center > 0){
  #   breaks <- breaks[breaks>=-center] # original value should be floored at 0
  # }
  
  breaks <- color_bucket$breaks
  
  breaks_above <- breaks[breaks>=0]
  breaks_below <- breaks[breaks<=0]
  
  max_bin_oneside <- max(length(breaks[breaks<=0])-1, length(breaks[breaks>=0])-1)
  
  if (abs(sum(sign(breaks))) >= length(breaks)-1){ # -1 to take 0 into consideration
    
    # +1 so that the group around 0 gets slightly different color than white
    # then remove the white color
    color_val <- if (length(breaks_below)==0) {
      colorRampPalette(colors = c(color_center, color_above), space = "Lab")(max_bin_oneside+1)[-1]
    }else{
      colorRampPalette(colors = c(color_below, color_center), space = "Lab")(max_bin_oneside+1)[-(max_bin_oneside+1)]
    }
    
    return(data.frame(Color_Value = color_val,
                      Color_Label = color_bucket$breaks_label,
                      stringsAsFactors = F))
    
  }else{
    if (length(breaks[breaks==0])>1){
      ## when there are two 0 in breaks (only one region), we want to create a unique color group 100-100 that's white
      ## also max_bin_oneside gets two 0 counted, need to take 1 out
      ## in the end we end up with 3 colors: slightly above, at center, slightly below
      color_val <- c(colorRampPalette(colors = c(color_below, color_center), space = "Lab")(max_bin_oneside), 
                     colorRampPalette(colors = c(color_center, color_above), space = "Lab")(max_bin_oneside)[-1])
    }else{
      # create symmetric colors with max_bin_oneside+1 (considering white color) then take white out
      # then for each side take the effective ones (drop the ones not needed)
      
      # negative: (length(breaks_below)-1) is number of effective color groups for negative ones; if it's 1 we want to take the last color
      color_val_negative <- colorRampPalette(colors = c(color_below, color_center), space = "Lab")(max_bin_oneside+1)[-(max_bin_oneside+1)][(max_bin_oneside - (length(breaks_below)-1) +1):max_bin_oneside]
      # positive: (length(breaks_above)-1) is number of effective color groups for positive ones; if it's 1 we want to take the first color
      color_val_positive <- colorRampPalette(colors = c(color_center, color_above), space = "Lab")(max_bin_oneside+1)[-1][1:(length(breaks_above)-1)]
      
      color_val <- c(color_val_negative, color_val_positive)
    }
    
    return(data.frame(Color_Value = color_val,
                      Color_Label = color_bucket$breaks_label,
                      stringsAsFactors = F))
  }
}