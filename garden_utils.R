# an R script for all key functions.
# This is the full list of functions

## 1. Generate initial solution
## 2. Functions to make the solution viable:
### 2.1. Check number of unique plants
### 2.2. Make the water feature visible
### 2.3. Check height restriction for all plants:
#### 2.3.1. Order the plants from low to high
#### 2.3.2. Ensure all plants are visible
### 2.4. Combine these into a viable solution
## 3. Cost functions for garden aesthetic
### 3.1. Colour Harmony score
### 3.2. Balance and Symmetry:
#### 3.2.1. Colour similarity
#### 3.2.2. Height similarity
#### 3.2.3. Category similarity
#### 3.2.4. Combine these three into one score
### 3.3. Seasonal variation
### 3.4. Combine into total aesthetic score

######################### All functions ##########################################
#### 1. Generate initial solution ####

generate_initial_sol <- function(an_empty_grid){
  # find the available position
  initial_sol <- an_empty_grid
  initial_sol[which(is.na(an_empty_grid), arr.ind = T)] <- sample(1:50, sum(is.na(initial_sol)), replace = T)
  
  return(initial_sol)
}

#### 2. Functions to make the solution viable: ####
##### 2.1. Check number of unique plants #####

correct_amount_unique_plants <- function(candidate_grid){
  # get number of unique plants (minus one for "U")
  unique_plants <- length(unique(as.vector(candidate_grid))) - 1
  
  # set condition to loop over
  ## if 15 <= unique_plants <= 25, this will be TRUE, FALSE otherwise
  correct_amount <- (unique_plants<=25 & unique_plants>=15)
  
  # while we don't have the correct amount
  while (!correct_amount) {
    # if there are not enough unique plants:
    if(unique_plants<15){
      # select how many new unique plants
      amount_new_plants <- 15 - unique_plants
      # index of plants not yet in garden:
      available_plants <- setdiff(c(1:50),        
                                  unique(as.vector(candidate_grid)))
      
      # get most frequent plants
      freq_table <- table(candidate_grid[candidate_grid != "U"])
      sorted_freq <- sort(freq_table, decreasing = TRUE)
      # replace some of the most frequent plants
      most_frequent_to_replace <- names(sorted_freq)[1:amount_new_plants]
      # store their frequencies
      frequencies <- as.numeric(sorted_freq)[1:amount_new_plants]
      
      # replace those:
      for(i in 1:amount_new_plants){
        # get row/col indices of all occurrences of the plant
        positions <- which(candidate_grid == most_frequent_to_replace[i],
                           arr.ind = TRUE)
        # choose two random positions to replace
        chosen_positions <- positions[sample(1:nrow(positions), 2), ,
                                      drop = FALSE]
        # pick a new plant
        new_plant <- sample(available_plants, 1)
        # Replace in the matrix
        candidate_grid[chosen_positions] <- new_plant
        
      }
      # update the number of unique plants:
      unique_plants <- length(unique(as.vector(candidate_grid))) - 1
      # update the condition:
      correct_amount <- (unique_plants<=25 & unique_plants>=15)
    }
    # otherwise, if there are too many unique plants:
    else if(unique_plants>25){
      # find plants already in garden:
      existing_plants <- unique(as.vector(
        candidate_grid[candidate_grid!="U"]))
      # how many to remove:
      remove_amount <- unique_plants - 25
      # randomly choose 'remove_amount' of plants to remove
      plant_species_to_remove <- sample(existing_plants, 
                                        remove_amount, replace = F) # without replacement
      
      # make list of plants that they can be replaced with:
      replace_with <- setdiff(existing_plants,
                              plant_species_to_remove)
      
      # iterate over all plants from these species and replace
      # with plants from this list
      for(i in 1:remove_amount){
        # current plant we want to replace
        cur_plant <- plant_species_to_remove[i]
        # find all these plants and randomly replace with
        # 'replace_with' plants
        
        amount_to_replace <- nrow(which(candidate_grid == cur_plant,
                                        arr.ind = TRUE))
        # replace them
        candidate_grid[which(candidate_grid == cur_plant,
                             arr.ind = TRUE)] <-
          sample(replace_with, 
                 amount_to_replace, replace = TRUE) # sample from plants in this list
      }
      # update the number of unique plants:
      unique_plants <- length(unique(as.vector(candidate_grid))) - 1
      # update the condition:
      correct_amount <- (unique_plants <=25 & unique_plants >=15)
    }
  }
  return(candidate_grid)
}

##### 2.2. Make the water feature visible #####

water_feature_visible <- function(candidate_grid){
  # find the plant indexes in the block around the water feature
  all_plants <- candidate_grid[5, 2:5] # the row above the water feature
  all_plants <- c(all_plants, candidate_grid[8, 2:5]) # the row below the water feature
  all_plants <- c(all_plants, candidate_grid[6:7, 2]) # the left side
  all_plants <- c(all_plants, candidate_grid[6:7, 5]) # the right side
  
  # find the heights of these plants
  ## again this works because the plant numbers are their index
  heights <- plant_data$Maturity_Height[as.numeric(all_plants)]
  
  # for all plants with heights >90, random sample from those with heights <=90:
  all_plants[heights>90] <- sample(which(plant_data$Maturity_Height<=90),
                                   length(heights[heights>90]), replace = TRUE)
  
  # now put these back into the grid:
  candidate_grid[5, 2:5] <- all_plants[1:4]
  candidate_grid[8, 2:5] <- all_plants[5:8]
  candidate_grid[6:7, 2] <- all_plants[9:10]
  candidate_grid[6:7, 5] <- all_plants[11:12]
  
  return(candidate_grid)
}

##### 2.3. Check height restriction for all plants #####
###### 2.3.1. Order the plants from low to high ######

sort_plants <- function(plant_vector, sort_decreasing = T){
  # convert to numeric
  plant_vector <- as.numeric(plant_vector)
  # get plant heights
  heights <- plant_data$Maturity_Height[plant_vector]
  # order from shortest to tallest
  plant_vector <- plant_vector[order(heights, decreasing = sort_decreasing)]
  return(plant_vector)
}

###### 2.3.2. Ensure all plants are visible ######

all_plants_visible <- function(candidate_grid){
  
  # apply function to each section
  # top: apply to the columns of this section
  T_b1 <- apply(candidate_grid[1:2, 1:5], 2, sort_plants)
  T_b2 <- apply(candidate_grid[1:3, 6:12], 2,
                sort_plants)
  T_b3 <- apply(candidate_grid[1:2, 13:16], 2,
                sort_plants)
  
  # right: apply to the rows of this section
  # for right section, flip direction of sorting
  R_b1 <- t(apply(candidate_grid[6:8, 13:16], 1,
                  sort_plants, sort_decreasing = FALSE))
  R_b2 <- t(apply(candidate_grid[3:5, 15:16], 1,
                  sort_plants, sort_decreasing = FALSE))
  
  # left: apply to the rows of these sections
  L_b1 <- matrix(sort_plants(candidate_grid[5, 1:5]),
                 nrow = 1)
  L_b2 <- t(apply(candidate_grid[6:7, c(1,2,5)], 1,
                  sort_plants))
  L_b3 <- matrix(sort_plants(candidate_grid[8, 1:5]),
                 nrow = 1)
  
  # put these back:
  # top section:
  candidate_grid[1:2, 1:5] <- T_b1
  candidate_grid[1:3, 6:12] <- T_b2
  candidate_grid[1:2, 13:16] <- T_b3
  
  # right section:
  candidate_grid[6:8, 13:16] <- R_b1
  candidate_grid[3:5, 15:16] <- R_b2
  
  # left section:
  candidate_grid[5, 1:5] <- L_b1
  candidate_grid[6:7, c(1,2,5)] <- L_b2
  candidate_grid[8, 1:5] <- L_b3
  
  return(candidate_grid)
}


##### 2.4. Combine these into a viable solution #####

make_viable_solution <- function(candidate_grid){
  # step 1: fix number of unique plants
  candidate_grid <- correct_amount_unique_plants(
    candidate_grid)
  # step 2: ensure water feature visible
  candidate_grid <- water_feature_visible(candidate_grid)
  # step 3: sort plants by height
  candidate_grid <- all_plants_visible(candidate_grid)
  
  # return the viable solution
  return(candidate_grid)
}

#### 3. Cost functions for garden aesthetic ####
##### 3.1. Colour Harmony score #####

colour_harmony_score <- function(candidate_grid) {
  # extract plant IDs from the grid
  plant_ids <- as.numeric(candidate_grid[candidate_grid!="U"])
  
  # get hues from plant_data
  plant_hues <- plant_data$Flower_Colour_1_Hue[plant_ids]
  
  # filter out non-flowering (NA) plants
  plant_hues <- plant_hues[!is.na(plant_hues)]
  
  # cast as numeric:
  plant_hues <- as.numeric(plant_hues)
  
  # if fewer than 2 hues, no pairwise distances to evaluate
  if (length(plant_hues) < 2) return(0)
  
  # circular hue distance function
  hue_dist <- function(h1, h2) {
    d <- abs(h1 - h2) # distance between the heus
    pmin(d, 360 - d) # convert to circular for colours at top of colour wheel (ex. 1 and 359)
  }
  
  # get all pairwise distances
  pairwise_dists <- combn(plant_hues, 2, function(x) hue_dist(x[1], x[2])) # take all 2-way combinations of the hue_dist functions
  
  # scoring: count analogous, complementary and discordant
  analogous <- length(which(pairwise_dists <= 30))
  complementary <- length(which(pairwise_dists >= 160 & pairwise_dists <= 200))
  discordant <- length(which(pairwise_dists > 30 & pairwise_dists < 160 | pairwise_dists > 200))
  
  # total colour harmony score
  score <- 2 * analogous + 1 * complementary - discordant
  
  # scale the score by number of pairs of plants
  n <- length(plant_hues)
  n_pairs <- choose(n, 2)
  
  scaled_score <- score/ n_pairs
  
  return(scaled_score)
}
##### 3.2. Balance and Symmetry #####
###### 3.2.1 Colour Similarity ######

colour_similarity <- function(h1, h2) {
  # if either are NA, return 0
  if (is.na(h1) || is.na(h2)) return(0)
  # otherwise, calculate the distance between their colours
  d <- abs(h1 - h2)
  # since the distance are circular, correct these:
  circular_dist <- pmin(d, 360 - d)
  # convert distance (0–180) to similarity (1 to 0)
  return(1 - circular_dist / 180)
}

###### 3.2.2 Height Similarity ######
# height
height_similarity <- function(h1, h2) {
  # again, if either are NA, return 0
  if (is.na(h1) || is.na(h2)) return(0)
  # otherwise, return 1- (heigh difference)/(max height)
  return(1 - abs(h1 - h2) / max(h1, h2))
}

###### 3.2.3 Category Similarity ######
# category- this will be binary TRUE/ FALSE if they are the same category
category_similarity <- function(c1, c2) {
  # again, if either is NA, return 0
  if (is.na(c1) || is.na(c2)) return(0)
  # otherwise, return 1 if same category, 0 otherwise
  return(as.numeric(c1 == c2))
}

###### 3.2.4. Combine these three scores ######
symmetry_score <- function(candidate_grid) {
  n_rows <- nrow(candidate_grid)
  n_cols <- ncol(candidate_grid)
  mid <- floor(n_cols / 2) # find middle point of grid
  
  total_score <- 0
  count <- 0
  
  # for each row of the grid
  for (i in 1:n_rows) {
    # compare the left (j) and right (mirror_j) side 
    for (j in 1:mid) {
      mirror_j <- n_cols - j + 1 # find the mirrored block
      
      # find plant ids for both
      left_id <- candidate_grid[i, j] 
      right_id <- candidate_grid[i, mirror_j]
      
      # if both are plants and not "U":
      if (left_id != "U" && right_id != "U") {
        left_id <- as.numeric(left_id)
        right_id <- as.numeric(right_id)
        
        # get all the data for these 2 plants:
        left <- plant_data[left_id, ] 
        right <- plant_data[right_id, ]
        
        # calculate the scores
        ## colour
        cs <- colour_similarity(left$Flower_Colour_1_Hue,
                                right$Flower_Colour_1_Hue)
        ## height
        hs <- height_similarity(left$Maturity_Height,
                                right$Maturity_Height)
        ## category
        cats <- category_similarity(left$Category, right$Category)
        
        # score for these blocks: 
        ## weighted average of similarities
        score <- (cs + hs + cats) / 3 
        
        # add to total score
        total_score <- total_score + score
        # count the number of blocks compared
        count <- count + 1
      }
    }
  }
  
  # if all were "U":
  if (count == 0) return(0)  # avoid division by zero
  return(total_score / count)  # average similarity over mirrored pairs
}

##### 3.3. Seasonal variation #####

seasonal_variation_score <- function(candidate_grid) {
  # get plant ids from grid
  plant_ids <- as.numeric(candidate_grid[candidate_grid != "U"])
  plants <- plant_data[plant_ids, ]
  
  # extract flowering months (named January_Flowering as example)
  flower_months <- plants[, grep("_Flowering$", names(plants))]
  
  # total flowering plants per month
  monthly_counts <- colSums(flower_months)
  
  # Score 1: Evenness of flowering (inverse of std dev)
  flower_sd <- sd(monthly_counts)
  if (flower_sd == 0) {
    flower_evenness_score <- 1  # perfect even flowering
  } else {
    flower_evenness_score <- 1 / flower_sd  # lower SD = higher score
  }
  
  # Score 2: non-flowering visual interest
  # give a small reward for each non-flowering plant
  non_flowering <- rowSums(flower_months) == 0
  evergreen_bonus_total <- sum(non_flowering)/ length(which(candidate_grid!="U")) *0.3 # proportion of non_flowering * 0.5
  
  # Combine scores
  total_score <- flower_evenness_score + evergreen_bonus_total
  
  return(total_score)
}


##### 3.4. Combined score function #####

total_aesthetic_score <- function(candidate_grid){
  col_score <- colour_harmony_score(candidate_grid)
  sym_score <- symmetry_score(candidate_grid)
  seasonal_var_score <- seasonal_variation_score(
    candidate_grid)
  
  total <- col_score + sym_score + seasonal_var_score
  return(total)
}
