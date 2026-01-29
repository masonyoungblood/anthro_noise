
# ANALYSIS PREP -----------------------------------------------------------

#load library
library(lme4)
library(flextable)
library(dplyr)
library(ftExtra)
library(officer)
library(ggplot2)
library(rphylopic)
library(ggeffects)
library(rphylopic)

#set workspace
setwd("/Users/masonyoungblood/Documents/Work/Fall 2024/Anthropogenic Noise/anthro_noise")

#create function for table construction
table_construction <- function(model, vars, intercept = FALSE, digits = 3){
  if(class(model)[1] == "lmerMod"){
    fixefs <- as.numeric(fixef(model))
    confints <- confint(model)[-c(1:2), ]
    if(intercept){
      temp <- data.frame(estimate = fixefs, lower = confints[, 1], upper = confints[, 2])
    } else{
      temp <- data.frame(estimate = fixefs, lower = confints[, 1], upper = confints[, 2])[-1, ]
    }
    temp$sig <- sapply(1:nrow(temp), function(x){ifelse(temp$lower[x]*temp$upper[x] > 0, "*", "")})
  }
  if(class(model)[1] == "lm"){
    fixefs <- as.numeric(coef(model))
    confints <- confint(model)
    if(intercept){
      temp <- data.frame(estimate = fixefs, lower = confints[, 1], upper = confints[, 2])
    } else{
      temp <- data.frame(estimate = fixefs, lower = confints[, 1], upper = confints[, 2])[-1, ]
    }
    temp$sig <- sapply(1:nrow(temp), function(x){ifelse(temp$lower[x]*temp$upper[x] > 0, "*", "")})
  }
  temp[, 1] <- round(temp[, 1], digits)
  temp[, 2] <- round(temp[, 2], digits)
  temp[, 3] <- round(temp[, 3], digits)
  rownames(temp) <- vars
  temp <- cbind(rownames(temp), temp)
  colnames(temp)[1] <- " "
  return(temp)
}

#load data
wc_data <- read.csv("data/white_crowned_sparrow/white_crowned_sparrow_note_lengths_ambient_noise.csv")
wc_ur_data <- read.csv("data/white_crowned_sparrow/white_crowned_sparrow_note_lengths_urban_rural.csv")
pt_data <- readxl::read_xlsx("data/pied_tamarin/Pied tamarin acoustic features linguistic laws x noise.xlsx")
cf_data <- readxl::read_xlsx("data/chaffinch/chaffinch_trills.xlsx", sheet = 2)
cr_data_a <- readxl::read_xlsx("data/field_cricket/Tick intervals.xlsx")
cr_data_b <- readxl::read_xlsx("data/field_cricket/Tick intervals 2.xlsx")
sp_data <- read.csv("data/jumping_spider/Habronattus_formosus_courtship_IOIs.csv")
tf_data <- readxl::read_xlsx("data/tungara_frog/TungaraFrog_Call duration data.xlsx")
gt_data <- readxl::read_xlsx("data/great_tit/GreatTit_data_annotated.xlsx")

#restructure pied tamarin data
pt_data <- pt_data[-which(pt_data$`sound level (dbC)` == "NA"), ] #remove data without noise levels
pt_data <- do.call(rbind, lapply(1:nrow(pt_data), function(x){
  temp <- as.numeric(unlist(pt_data[x, 12:21]))
  temp <- temp[-which(is.na(temp))]
  data.frame(duration = temp, length = length(temp), seq_id = x, noise = as.numeric(pt_data$`sound level (dbC)`[x]))
}))
pt_data <- pt_data[-which(pt_data$seq_id %in% c(52, 71, 388)), ] #remove sequences with impossible durations

#restructure white-crowned sparrow data
wc_song_lengths <- as.data.frame(table(wc_data$song))
urban_rural <- wc_ur_data$area[match(wc_data$song, wc_ur_data$song)]
urban_rural <- as.numeric(sapply(urban_rural, function(x){ifelse(x == "rural", 0, 1)}))
wc_data <- data.frame(duration = wc_data$length, length = wc_song_lengths$Freq[match(wc_data$song, wc_song_lengths$Var1)], seq_id = as.numeric(factor(wc_data$song)), noise = wc_data$LAF90, urban = urban_rural)

#restructure chaffinch data
cf_data <- data.frame(duration = cf_data$duration/cf_data$syllable_count, length = cf_data$syllable_count, seq_id = 1:nrow(cf_data), noise = as.numeric(cf_data$ambient_noise), urban = ifelse(cf_data$habitat == "Rural", 0, 1))

#restructure cricket data
cr_data_a <- cr_data_a[, -c(1, which(colnames(cr_data_a) %in% colnames(cr_data_a)[grep("Interval", colnames(cr_data_a))]))]
cr_data_a <- do.call(rbind, lapply(1:ncol(cr_data_a), function(x){
  noise <- as.character(cr_data_a[1, x])
  times <- as.numeric(cr_data_a[-1, x][[1]])
  times <- times[-which(is.na(times))]
  dividers <- c(0, which(diff(times) >= 750))
  split_times <- lapply(2:length(dividers), function(y){times[(dividers[y-1]+1):dividers[y]]})
  output <- do.call(rbind, lapply(1:length(split_times), function(y){
    seq_times <- diff(split_times[[y]])
    if(length(seq_times) > 0){
      output <- data.frame(duration = seq_times, length = length(seq_times), seq_id = paste0(x, "-", y), noise = noise)
    } else{
      output <- NA
    }
    return(output)
  }))
  output <- output[-which(is.na(output$duration)), ]
  return(output)
}))
cr_data_b <- cr_data_b[, -c(1, which(colnames(cr_data_b) %in% colnames(cr_data_b)[grep("Interval", colnames(cr_data_b))]))]
cr_data_b <- do.call(rbind, lapply(1:ncol(cr_data_b), function(x){
  noise <- as.character(cr_data_b[1, x])
  times <- as.numeric(cr_data_b[-1, x][[1]])
  times <- times[-which(is.na(times))]
  dividers <- c(0, which(diff(times) >= 750))
  split_times <- lapply(2:length(dividers), function(y){times[(dividers[y-1]+1):dividers[y]]})
  output <- do.call(rbind, lapply(1:length(split_times), function(y){
    seq_times <- diff(split_times[[y]])
    if(length(seq_times) > 0){
      output <- data.frame(duration = seq_times, length = length(seq_times), seq_id = paste0(x, "-", y), noise = noise)
    } else{
      output <- NA
    }
    return(output)
  }))
  output <- output[-which(is.na(output$duration)), ]
  return(output)
}))
cr_data_a$seq_id <- paste0(cr_data_a$seq_id, "-a")
cr_data_b$seq_id <- paste0(cr_data_b$seq_id, "-b")
cr_data <- rbind(cr_data_a, cr_data_b)
cr_data$noise <- ifelse(cr_data$noise == "Ambient", 0, 1)
cr_data$seq_id <- as.numeric(factor(cr_data$seq_id))
cr_data$duration <- cr_data$duration/1000

#restructure spider data
#drop percussive thumps from data
sp_data <- sp_data[-which(sp_data$Vib_type == "pc"), ]
sp_data <- data.frame(duration = sp_data$IOI..s., length = sp_data$No_syllables, seq_id = sp_data$Seq_ID+1, noise = ifelse(sp_data$Condition == "silence", 0, 1))
sp_data <- sp_data[-which(is.na(sp_data$duration)), ]
sp_data$length <- sp_data$length-1

#restructure tungara frog data
#drop rows with NA values, missing data distributed randomly according to halfwerk
tf_data <- tf_data[-which(is.na(tf_data$`Call complexity (# of chucks added after whine)`)), ]
tf_data$`Call complexity (# of chucks added after whine)` <- tf_data$`Call complexity (# of chucks added after whine)` + 1 #add 1, since all calls start with a whine
tf_data <- data.frame(duration = tf_data$`call_length(s)`/tf_data$`Call complexity (# of chucks added after whine)`, length = tf_data$`Call complexity (# of chucks added after whine)`, seq_id = 1:nrow(tf_data), noise = tf_data$`noise level`)

#restructure great tit data
gt_og_data <- gt_data
gt_og_data$syllable_id <- paste0(gt_og_data$`Song name`, gt_og_data$`Syllable Number`)
gt_data <- data.frame(duration = gt_og_data$Length/1000, #elements in syllables
                      length = sapply(1:nrow(gt_og_data), function(x){length(which(gt_og_data$syllable_id == gt_og_data$syllable_id[x]))}),
                      seq_id = as.numeric(factor(gt_og_data$syllable_id)),
                      noise = ifelse(gt_og_data$`Before/during/after noise exposure` == "D", 1, 0))
# gt_data <- do.call(rbind, lapply(1:length(unique(gt_og_data$syllable_id)), function(x){ #syllables in songs
#   target_inds <- which(gt_og_data$syllable_id == unique(gt_og_data$syllable_id)[x])
#   data.frame(duration = (gt_og_data$`End time`[target_inds[length(target_inds)]] - gt_og_data$`Start time`[target_inds[1]])/1000,
#              length = max(gt_og_data$`Syllable Number`[which(gt_og_data$`Song name` == gt_og_data$`Song name`[which(gt_og_data$syllable_id == unique(gt_og_data$syllable_id)[x])][1])]),
#              seq_id = as.numeric(factor(gt_og_data$`Song name`)[which(gt_og_data$syllable_id == unique(gt_og_data$syllable_id)[x])][1]),
#              noise = ifelse(gt_og_data$`Before/during/after noise exposure`[which(gt_og_data$syllable_id == unique(gt_og_data$syllable_id)[x])][1] == "D", 1, 0))
# }))

#remove sequences with length of one
if(length(which(pt_data$length == 1)) > 0){pt_data <- pt_data[-which(pt_data$length == 1), ]}
if(length(which(wc_data$length == 1)) > 0){wc_data <- wc_data[-which(wc_data$length == 1), ]}
if(length(which(cf_data$length == 1)) > 0){cf_data <- cf_data[-which(cf_data$length == 1), ]}
if(length(which(cr_data$length == 1)) > 0){cr_data <- cr_data[-which(cr_data$length == 1), ]}
if(length(which(sp_data$length == 1)) > 0){sp_data <- sp_data[-which(sp_data$length == 1), ]}
if(length(which(tf_data$length == 1)) > 0){tf_data <- tf_data[-which(tf_data$length == 1), ]}
if(length(which(gt_data$length == 1)) > 0){gt_data <- gt_data[-which(gt_data$length == 1), ]}

# STATISTICAL MODELS ------------------------------------------------------

#run models at the token level (with sequence ID as varying intercept, where applicable)
token_effects <- list(
  pied_tamarin = list(
    base = lmer(scale(log(duration)) ~ scale(log(length)) + (1|seq_id), data = pt_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    noise = lmer(scale(log(duration)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise) + (1|seq_id), data = pt_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    simple_duration = lmer(scale(duration) ~ scale(noise) + (1|seq_id), data = pt_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    simple_length = lm(scale(length) ~ scale(noise), data = aggregate(length ~ noise + seq_id, pt_data, FUN = mean))
  ),
  white_crowned_sparrow = list(
    base = lmer(scale(log(duration)) ~ scale(log(length)) + (1|seq_id), data = wc_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    noise = lmer(scale(log(duration)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise) + (1|seq_id), data = wc_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    urban = lmer(scale(log(duration)) ~ scale(log(length)) + urban + scale(log(length)):urban + (1|seq_id), data = wc_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    simple_duration = lmer(scale(duration) ~ scale(noise) + (1|seq_id), data = wc_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    simple_length = lm(scale(length) ~ scale(noise), data = aggregate(length ~ noise + seq_id, wc_data, FUN = mean))
  ),
  chaffinch = NA,
  field_cricket = list(
    base = lmer(scale(log(duration + 1)) ~ scale(log(length)) + (1|seq_id), data = cr_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    noise = lmer(scale(log(duration + 1)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise) + (1|seq_id), data = cr_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    simple_duration = lmer(scale(duration + 1) ~ scale(noise) + (1|seq_id), data = cr_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    simple_length = lm(scale(length) ~ scale(noise), data = aggregate(length ~ noise + seq_id, cr_data, FUN = mean))
  ),
  jumping_spider = list(
    base = lmer(scale(log(duration)) ~ scale(log(length)) + (1|seq_id), data = sp_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    noise = lmer(scale(log(duration)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise) + (1|seq_id), data = sp_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    simple_duration = lmer(scale(duration) ~ scale(noise) + (1|seq_id), data = sp_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    simple_length = lm(scale(length) ~ scale(noise), data = aggregate(length ~ noise + seq_id, sp_data, FUN = mean))
  ),
  tungara_frog = NA,
  great_tit = list(
    base = lmer(scale(log(duration)) ~ scale(log(length)) + (1|seq_id), data = gt_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    noise = lmer(scale(log(duration)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise) + (1|seq_id), data = gt_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    simple_duration = lmer(scale(duration) ~ scale(noise) + (1|seq_id), data = gt_data, REML = FALSE, control = lmerControl(optimizer = "bobyqa")), 
    simple_length = lm(scale(length) ~ scale(noise), data = aggregate(length ~ noise + seq_id, gt_data, FUN = mean))
  )
)

#run models at the sequence level (with duration and noise averaged within sequences)
sequence_effects <- list(
  pied_tamarin = list(
    base = lm(scale(log(duration)) ~ scale(log(length)), data = aggregate(duration ~ length + noise + seq_id, pt_data, FUN = mean)), 
    noise = lm(scale(log(duration)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise), data = aggregate(duration ~ length + noise + seq_id, pt_data, FUN = mean)), 
    simple_duration = lm(scale(duration) ~ scale(noise), data = aggregate(duration ~ noise + seq_id, pt_data, FUN = mean)), 
    simple_length = lm(scale(length) ~ scale(noise), data = aggregate(length ~ noise + seq_id, pt_data, FUN = mean))
  ),
  white_crowned_sparrow = list(
    base = lm(scale(log(duration)) ~ scale(log(length)), data = aggregate(duration ~ length + noise + seq_id, wc_data, FUN = mean)), 
    noise = lm(scale(log(duration)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise), data = aggregate(duration ~ length + noise + seq_id, wc_data, FUN = mean)), 
    urban = lm(scale(log(duration)) ~ scale(log(length)) + urban + scale(log(length)):urban, data = aggregate(duration ~ length + urban + seq_id, wc_data, FUN = mean)), 
    simple_duration = lm(scale(duration) ~ scale(noise), data = aggregate(duration ~ noise + seq_id, wc_data, FUN = mean)), 
    simple_length = lm(scale(length) ~ scale(noise), data = aggregate(length ~ noise + seq_id, wc_data, FUN = mean))
  ),
  chaffinch = list(
    base = lm(scale(log(duration)) ~ scale(log(length)), data = cf_data),
    noise = lm(scale(log(duration)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise), data = cf_data[which(!is.na(cf_data$noise)), ]),
    urban = lm(scale(log(duration)) ~ scale(log(length)) + urban + scale(log(length)):urban, data = cf_data),
    simple_duration = lm(scale(duration) ~ scale(noise), data = cf_data),
    simple_length = lm(scale(length) ~ scale(noise), data = cf_data)
  ),
  field_cricket = list(
    base = lm(scale(log(duration + 1)) ~ scale(log(length)), data = aggregate(duration ~ length + noise + seq_id, cr_data, FUN = mean)), 
    noise = lm(scale(log(duration + 1)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise), data = aggregate(duration ~ length + noise + seq_id, cr_data, FUN = mean)), 
    simple_duration = lm(scale(duration + 1) ~ scale(noise), data = aggregate(duration ~ noise + seq_id, cr_data, FUN = mean)), 
    simple_length = lm(scale(length) ~ scale(noise), data = aggregate(length ~ noise + seq_id, cr_data, FUN = mean))
  ),
  jumping_spider = list(
    base = lm(scale(log(duration)) ~ scale(log(length)), data = aggregate(duration ~ length + noise + seq_id, sp_data, FUN = mean)), 
    noise = lm(scale(log(duration)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise), data = aggregate(duration ~ length + noise + seq_id, sp_data, FUN = mean)), 
    simple_duration = lm(scale(duration) ~ scale(noise), data = aggregate(duration ~ noise + seq_id, sp_data, FUN = mean)), 
    simple_length = lm(scale(length) ~ scale(noise), data = aggregate(length ~ noise + seq_id, sp_data, FUN = mean))
  ),
  tungara_frog = list(
    base = lm(scale(log(duration)) ~ scale(log(length)), data = tf_data),
    noise = lm(scale(log(duration)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise), data = tf_data),
    simple_duration = lm(scale(duration) ~ scale(noise), data = tf_data),
    simple_length = lm(scale(length) ~ scale(noise), data = tf_data)
  ),
  great_tit = list(
    base = lm(scale(log(duration)) ~ scale(log(length)), data = aggregate(duration ~ length + noise + seq_id, gt_data, FUN = mean)), 
    noise = lm(scale(log(duration)) ~ scale(log(length)) + scale(noise) + scale(log(length)):scale(noise), data = aggregate(duration ~ length + noise + seq_id, gt_data, FUN = mean)), 
    simple_duration = lm(scale(duration) ~ scale(noise), data = aggregate(duration ~ noise + seq_id, gt_data, FUN = mean)), 
    simple_length = lm(scale(length) ~ scale(noise), data = aggregate(length ~ noise + seq_id, gt_data, FUN = mean))
  )
)

#construct tables for main models, run at the token and type level
pt_token_noise <- table_construction(token_effects$pied_tamarin$noise, vars = c("length", "noise", "length:noise"))
pt_sequence_noise <- table_construction(sequence_effects$pied_tamarin$noise, vars = c("length", "noise", "length:noise"))
wc_token_noise <- table_construction(token_effects$white_crowned_sparrow$noise, vars = c("length", "noise", "length:noise"))
wc_sequence_noise <- table_construction(sequence_effects$white_crowned_sparrow$noise, vars = c("length", "noise", "length:noise"))
wc_token_urban <- table_construction(token_effects$white_crowned_sparrow$urban, vars = c("length", "urban", "length:urban"))
wc_sequence_urban <- table_construction(sequence_effects$white_crowned_sparrow$urban, vars = c("length", "urban", "length:urban"))
cf_sequence_noise <- table_construction(sequence_effects$chaffinch$noise, vars = c("length", "noise", "length:noise"))
cf_sequence_urban <- table_construction(sequence_effects$chaffinch$urban, vars = c("length", "urban", "length:urban"))
cr_token_noise <- table_construction(token_effects$field_cricket$noise, vars = c("length", "noise", "length:noise"))
cr_sequence_noise <- table_construction(sequence_effects$field_cricket$noise, vars = c("length", "noise", "length:noise"))
sp_token_noise <- table_construction(token_effects$jumping_spider$noise, vars = c("length", "noise", "length:noise"))
sp_sequence_noise <- table_construction(sequence_effects$jumping_spider$noise, vars = c("length", "noise", "length:noise"))
tf_sequence_noise <- table_construction(sequence_effects$tungara_frog$noise, vars = c("length", "noise", "length:noise"))
gt_token_noise <- table_construction(sequence_effects$great_tit$noise, vars = c("length", "noise", "length:noise"))
gt_sequence_noise <- table_construction(token_effects$great_tit$noise, vars = c("length", "noise", "length:noise"))

#combine and save table
combined_table <- cbind(
  species = rep(
    c("Pied Tamarin", "White-Crowned Sparrow", "Chaffinch", "Great Tit", "Jumping Spider", "Field Cricket", "Túngara Frog"),
    each = 3
  ),
  rbind(
    pt_sequence_noise, 
    wc_sequence_noise,
    cf_sequence_noise,
    gt_sequence_noise,
    sp_sequence_noise,
    cr_sequence_noise,
    tf_sequence_noise
  )
)
colnames(combined_table) <- c("Species", "Parameter", "Estimate", "L-95%", "U-95%", "Sig.")
combined_table$Parameter[which(combined_table$Parameter == "length")] <- "Length"
combined_table$Parameter[which(combined_table$Parameter == "noise")] <- "Noise"
combined_table$Parameter[which(combined_table$Parameter == "length:noise")] <- "Length : Noise"
flextable(combined_table) %>% 
  set_caption("CAPTION", autonum = run_autonum(seq_id = "tab", bkm = "table-datasets")) %>%
  merge_v(j = "Species") %>%
  theme_vanilla() %>% set_table_properties(layout = "autofit") %>% save_as_docx(path = "output/table.docx")

# PLOTS -------------------------------------------------------------------

#img_coords: x, y, height
menz_plot <- function(data, intercept, effect, bounds, color = "black", alpha = 1, effects_panel = 0.2, img_height, img_name, already_averaged = FALSE, title = NULL, ylab = NULL, xlab = "Sequence Length"){
  #get image from phylopic
  uuid <- get_uuid(name = img_name)
  img <- get_phylopic(uuid = uuid)
  
  #restructure data to average within sequences
  if(!already_averaged){
    data <- data.frame(seq_id = unique(data$seq_id),
                       duration = aggregate(duration ~ seq_id, data, mean)$duration,
                       length = aggregate(length ~ seq_id, data, mean)$length)
  }
  
  #get original scaling from data
  og_scaled_x <- scale(log(data$length))
  og_scaled_y <- scale(log(data$duration))
  
  #function that predicts durations from lengths, based on intercepts, effects, and means and sds from scaling
  pred_durations <- function(intercept, effect, length, mean_log_length, sd_log_length, mean_log_dur, sd_log_dur){
    return(exp((intercept + effect*((log(length) - mean_log_length)/sd_log_length))*sd_log_dur + mean_log_dur))
  }
  
  #compute best fit line for plotting
  best_fit_data <- data.frame(x = seq(from = min(data$length), to = max(data$length), length.out = 1000))
  best_fit_data$y <- pred_durations(intercept, effect, length = best_fit_data$x,
                                    mean_log_length = attr(og_scaled_x, "scaled:center"), sd_log_length = attr(og_scaled_x, "scaled:scale"),
                                    mean_log_dur = attr(og_scaled_y, "scaled:center"), sd_log_dur = attr(og_scaled_y, "scaled:scale"))
  
  #construct base plot
  base_plot <- ggplot(data, aes(x = length, y = duration)) + 
    geom_point(colour = color, size = 1, stroke = 0) + 
    geom_line(data = best_fit_data, aes(x = x, y = y), colour = color, linewidth = 0.5) +
    theme_linedraw(base_size = 8, base_family = "Avenir") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) + 
    ggtitle(title) + 
    theme(plot.title = element_text(face = "bold")) + 
    xlab(xlab) + ylab(ylab)
  x_range <- range(ggplot_build(base_plot)$layout$panel_params[[1]]$x.range)
  y_range <- range(ggplot_build(base_plot)$layout$panel_params[[1]]$y.range)
  x_pos <- x_range[1] + 0.9 * diff(x_range)
  y_pos <- y_range[1] + 0.8 * diff(y_range)
  pic_size <- img_height * diff(y_range)
  base_plot <- base_plot + add_phylopic(img = img, x = x_pos, y = y_pos, height = pic_size, fill = color)
  
  #construct effects plot
  effects_plot <- ggplot() + 
    geom_point(aes(x = 1, y = effect), stroke = 1, colour = color) + 
    geom_linerange(aes(x = 1, ymin = bounds[1], ymax = bounds[2]), colour = color) + 
    theme_linedraw(base_size = 8, base_family = "Avenir") + 
    geom_hline(yintercept = 0, lty = "dashed") + 
    scale_y_continuous(name = "Slope", position = "right", labels = scales::number_format(accuracy = 0.01), breaks = scales::pretty_breaks(n = 6)) + 
    scale_x_continuous(name = "...", breaks = c(1), labels = c("A"), limits = c(0.5, 1.5)) + 
    ggtitle("   ") + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
          axis.ticks.x = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"))
  
  #combine and return
  return(cowplot::plot_grid(base_plot, effects_plot, rel_widths = c(1, effects_panel)))
}

#pied tamarin plot
pt_plot <- menz_plot(data = pt_data, 
                     intercept = coefficients(summary(sequence_effects$pied_tamarin$base))[1, 1],
                     effect = coefficients(summary(sequence_effects$pied_tamarin$base))[2, 1],
                     bounds = confint(sequence_effects$pied_tamarin$base, parm = "scale(log(length))", method = "Wald"),
                     img_height = 0.3, img_name = "Saguinus bicolor", color = "#0072B2",
                     title = "Pied Tamarin", ylab = "Mean ED (s)")

#white crowned plot
wc_plot <- menz_plot(data = wc_data, 
                     intercept = coefficients(summary(sequence_effects$white_crowned_sparrow$base))[1, 1],
                     effect = coefficients(summary(sequence_effects$white_crowned_sparrow$base))[2, 1],
                     bounds = confint(sequence_effects$white_crowned_sparrow$base, parm = "scale(log(length))", method = "Wald"),
                     img_height = 0.23, img_name = "Zonotrichia leucophrys", color = "#D55E00",
                     title = "White-Crowned Sparrow", ylab = "Mean ED (s)")

#chaffinch plot
cf_plot <- menz_plot(data = cf_data, 
                     intercept = coefficients(summary(sequence_effects$chaffinch$base))[1, 1],
                     effect = coefficients(summary(sequence_effects$chaffinch$base))[2, 1],
                     bounds = confint(sequence_effects$chaffinch$base, parm = "scale(log(length))", method = "Wald"),
                     img_height = 0.2, img_name = "Fringilla coelebs", color = "#009E73", already_averaged = TRUE,
                     title = "Chaffinch", ylab = "Mean ED (s)")

#field cricket plot
cr_plot <- menz_plot(data = cr_data, 
                     intercept = coefficients(summary(sequence_effects$field_cricket$base))[1, 1],
                     effect = coefficients(summary(sequence_effects$field_cricket$base))[2, 1],
                     bounds = confint(sequence_effects$field_cricket$base, parm = "scale(log(length))", method = "Wald"),
                     img_height = 0.3, img_name = "Gryllus", color = "#CC79A7",
                     title = "Field Cricket", ylab = "Mean IOI (s)")

#jumping spider plot
sp_plot <- menz_plot(data = sp_data, 
                     intercept = coefficients(summary(sequence_effects$jumping_spider$base))[1, 1],
                     effect = coefficients(summary(sequence_effects$jumping_spider$base))[2, 1],
                     bounds = confint(sequence_effects$jumping_spider$base, parm = "scale(log(length))", method = "Wald"),
                     img_height = 0.2, img_name = "Habronattus", color = "#BDB10F",
                     title = "Jumping Spider", ylab = "Mean IOI (s)")

#tungara frog plot
tf_plot <- menz_plot(data = tf_data, 
                     intercept = coefficients(summary(sequence_effects$tungara_frog$base))[1, 1],
                     effect = coefficients(summary(sequence_effects$tungara_frog$base))[2, 1],
                     bounds = confint(sequence_effects$tungara_frog$base, parm = "scale(log(length))", method = "Wald"),
                     img_height = 0.2, img_name = "Engystomops pustulosus", color = "#38A6E5",
                     title = "Túngara Frog", ylab = "Mean IOI (s)")

#great tit plot
gt_plot <- menz_plot(data = gt_data, 
                     intercept = coefficients(summary(sequence_effects$great_tit$base))[1, 1],
                     effect = coefficients(summary(sequence_effects$great_tit$base))[2, 1],
                     bounds = confint(sequence_effects$great_tit$base, parm = "scale(log(length))", method = "Wald"),
                     img_height = 0.2, img_name = "Parus atricapillus", color = "#E09900",
                     title = "Great Tit", ylab = "Mean ED (s)")

#export combined plot
svg("output/combined_plot.svg", width = 8, height = 7); cowplot::plot_grid(pt_plot, wc_plot, cf_plot, gt_plot, sp_plot, cr_plot, tf_plot, nrow = 4, align = "hv"); dev.off()
png("output/combined_plot.png", width = 8, height = 7, units = "in", res = 600); cowplot::plot_grid(pt_plot, wc_plot, cf_plot, gt_plot, sp_plot, cr_plot, tf_plot, nrow = 4, align = "hv"); dev.off()

#function for plotting predicted interactions
pred_plot <- function(model, data, img_height, img_name, title, ylab){
  uuid <- get_uuid(name = img_name)
  img <- get_phylopic(uuid = uuid)
  colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#BDB10F", "#38A6E5", "#E09900")
  pred <- ggpredict(model, c("length", "noise"), back_transform = FALSE)
  pred$predicted <- exp((pred$predicted*sd(log(data$duration))) + mean(log(data$duration)))
  pred$conf.low <- exp((pred$conf.low*sd(log(data$duration))) + mean(log(data$duration)))
  pred$conf.high <- exp((pred$conf.high*sd(log(data$duration))) + mean(log(data$duration)))
  to_remove <- which(is.infinite(pred$predicted) | is.nan(pred$predicted) | is.na(pred$predicted))
  if(length(to_remove) > 0){pred <- pred[-to_remove,]}
  output <- pred %>% plot(line_size = 0.5)
  output <- output + 
    ggtitle(title) + 
    scale_colour_manual(values = colors[1:length(levels(pred$group))]) + 
    scale_fill_manual(values = colors[1:length(levels(pred$group))]) + 
    labs(colour = "Noise") + xlab("Sequence Length") + ylab(ylab) + 
    theme_linedraw(base_size = 8, base_family = "Avenir")
  x_range <- range(ggplot_build(output)$layout$panel_params[[1]]$x.range)
  y_range <- range(ggplot_build(output)$layout$panel_params[[1]]$y.range)
  x_pos <- x_range[1] + 0.9 * diff(x_range)
  y_pos <- y_range[1] + 0.8 * diff(y_range)
  pic_size <- img_height * diff(y_range)
  output <- output + add_phylopic(img = img, x = x_pos, y = y_pos, height = pic_size, fill = "black")
  output$layers[[2]]$show.legend <- FALSE
  return(output)
}

#create plot for each species
pt_plot <- pred_plot(sequence_effects$pied_tamarin$noise, aggregate(duration ~ length + noise + seq_id, pt_data, FUN = mean), img_height = 0.3, img_name = "Saguinus bicolor", "Pied Tamarin", "Mean ED (s)")
wc_plot <- pred_plot(sequence_effects$white_crowned_sparrow$noise, aggregate(duration ~ length + noise + seq_id, wc_data, FUN = mean), img_height = 0.23, img_name = "Zonotrichia leucophrys", "White-Crowned Sparrow", "Mean ED (s)")
cf_plot <- pred_plot(sequence_effects$chaffinch$noise, cf_data[which(!is.na(cf_data$noise)), ], img_height = 0.2, img_name = "Fringilla coelebs", "Chaffinch", "Mean ED (s)")
gt_plot <- pred_plot(sequence_effects$great_tit$noise, aggregate(duration ~ length + noise + seq_id, gt_data, FUN = mean), img_height = 0.2, img_name = "Parus atricapillus", "Great Tit", "Mean ED (s)")
sp_plot <- pred_plot(sequence_effects$jumping_spider$noise, aggregate(duration ~ length + noise + seq_id, sp_data, FUN = mean), img_height = 0.2, img_name = "Habronattus", "Jumping Spider", "Mean IOI (s)")
cr_plot <- pred_plot(sequence_effects$field_cricket$noise, aggregate(duration ~ length + noise + seq_id, cr_data, FUN = mean), img_height = 0.3, img_name = "Gryllus", "Field Cricket", "Mean IOI (s)")
tf_plot <- pred_plot(sequence_effects$tungara_frog$noise, tf_data, img_height = 0.2, img_name = "Engystomops pustulosus", "Túngara Frog", "Mean IOI (s)")

#combine and save
#svg("output/interact_plot.svg", width = 8, height = 7); cowplot::plot_grid(pt_plot, wc_plot, cf_plot, gt_plot, sp_plot, cr_plot, tf_plot, nrow = 4, align = "hv"); dev.off()
