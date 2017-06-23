library(tidyverse)

#read in data with 5000 obs
products <- read_csv("products.csv")
orders <- read_csv("orders5000.csv")
orders_train <- read_csv("orders_train5000.csv")
orders_prior <- read_csv("orders_prior5000.csv")


### define function to calculate f1 score ###
f1 <- function (actual, predicted)
{
  act <- unique(actual)
  pred <- unique(predicted)
  
  tp <- length(intersect(act,pred))
  fp <- length(setdiff(pred,act))
  fn <- length(setdiff(act,pred))
  
  precision <- ifelse ((tp==0 & fp==0), 0, tp/(tp+fp))
  recall <- ifelse ((tp==0 & fn==0), 0, tp/(tp+fn))
  
  score <- ifelse ((precision==0 & recall==0), 0, 2*precision*recall/(precision+recall))
  score
}


##### predict next basket using naive model (match last order) #####
n <- max(orders$user_id)
prediction_matrix <- tibble(id = numeric(n), 
                            order_id = numeric(n),
                            predicted_purchase = numeric(n),
                            actual_purchase = numeric(n), 
                            f1score = numeric(n))

tme <- Sys.time()
# create new datasets for each user
for (i in 1:n){   
  id <- i
  
  #filter orders to use only current userid
  userX_orders<- filter(orders, user_id==id)
  
  #find all products ordered in all previous orders for a single user
  userX_prior <- userX_orders %>% 
    left_join(orders_prior, by="order_id") %>% 
    filter(eval_set=='prior') %>% 
    left_join(products, by="product_id")
  
  #find products ordered in training data
  userX_train_products <- userX_orders %>% 
    left_join(orders_train, by="order_id") %>% 
    filter(eval_set=='train') %>% 
    left_join(products, by="product_id") %>% 
    select(product_name, product_id)
  
  #calculate information about number of products purchased in previous orders
  userX_order_summary <- userX_prior %>%
    group_by(order_number) %>%
    summarize(item_count = n()) %>%
    right_join(userX_orders, by='order_number') %>%
    select(-c(user_id, eval_set))
  
  
  ### generate predictions using last order
  (order_id <- userX_order_summary[["order_id"]][max(userX_order_summary$order_number)] )
  predicted_items <- userX_prior %>%
    filter(order_number == max(order_number)) %>%
    select(product_id)
  
  (predicted_purchase <- as.numeric(data.matrix(predicted_items)))
  (actual_purchase <- as.numeric(data.matrix(userX_train_products[,2])))
  (f1score <- f1(actual_purchase, predicted_purchase))
  
  #include data from observation i into prediction matrix
  prediction_matrix[i,] <- c(id,
                             order_id,
                             paste(predicted_purchase, collapse = " "),
                             paste(actual_purchase, collapse = " "), 
                             f1score)
  
  print(paste("iteration", i, "complete"))
}
Sys.time() - tme

#calculate average F1 score for baseline
localscores_naive <- prediction_matrix %>% 
  filter(actual_purchase!=0)  %>% 
  filter(actual_purchase!="") %>% 
  filter(predicted_purchase!="")
mean(as.numeric(localscores_naive$f1score)) #0.2668645



##### predict next basket using Personalized Markov chains #####
n <- max(orders$user_id)
prediction_matrix <- tibble(id = numeric(n), 
                            order_id = numeric(n),
                            predicted_purchase = numeric(n),
                            actual_purchase = numeric(n), 
                            f1score = numeric(n))

tme <- Sys.time()
# create new datasets for each user
for (i in 1:n){   
  id <- i
  
  #filter orders to use only current userid
  userX_orders<- filter(orders, user_id==id)
  
  #find all products ordered in all previous orders for a single user
  userX_prior <- userX_orders %>% 
    left_join(orders_prior, by="order_id") %>% 
    filter(eval_set=='prior') %>% 
    left_join(products, by="product_id")
  
  #find products ordered in training data
  userX_train_products <- userX_orders %>% 
    left_join(orders_train, by="order_id") %>% 
    filter(eval_set=='train') %>% 
    left_join(products, by="product_id") %>% 
    select(product_name, product_id)
  
  #summarize information about products purchased by user among all prior orders
  userX_product_summary <- userX_prior %>% 
    group_by(product_name) %>% 
    summarize(purchase_count = n(), 
              most_recent_order = max(order_number), 
              avg_cart_order = mean(add_to_cart_order)) %>% 
    mutate(purchase_count_scaled = purchase_count/max(userX_prior$order_number)) %>% 
    mutate(recent_order_scaled = most_recent_order/max(most_recent_order)) %>% 
    mutate(cart_order_scaled = 1/(avg_cart_order)) %>% 
    mutate(score = 2*purchase_count_scaled+recent_order_scaled+cart_order_scaled) %>% 
    arrange(desc(most_recent_order), desc(purchase_count)) %>% 
    left_join(products, by = "product_name") %>% 
    select(product_id, product_name, score, everything()) %>% 
    arrange(desc(score))
  
  #calculate information about number of products purchased in previous order for use in forecasting  
  userX_order_summary <- userX_prior %>% 
    group_by(order_number) %>% 
    summarize(item_count = n()) %>% 
    right_join(userX_orders, by='order_number') %>% 
    select(-c(user_id, eval_set))
  
  
  ###calculate transition probabilities and create transition matrix (PMC specific calculations)
  
  #find all unique products that a given user has purchased in the past
  (allproducts <- unique(arrange(userX_prior, product_id)$product_id))
  
  #count number of products and number of orders
  num_products <- length(allproducts)
  num_orders <- max(userX_prior$order_number)
  
  #find products from each order and add them to a list
  orderproducts_list <- list()
  for (order in 1:num_orders){
    orderproducts_list[[order]] <- unique(filter(userX_prior, order_number == order) %>% arrange(product_id))$product_id
  } 
  
  #create matrix with products on x axis and orders on y axis
  ordermatrix <- matrix(data=NA, nrow=num_products, ncol=num_orders)
  rownames(ordermatrix) <- allproducts
  
  for (order in 1:num_orders){
    ordermatrix[,order] <- (as.numeric(allproducts %in% orderproducts_list[[order]]))
  }
  denominators <- rowSums(ordermatrix[,1:num_orders-1, drop=FALSE])+.00001
  
  #create transition matrix
  raw_transitionmatrix <- matrix(data=NA, nrow=num_products, ncol=num_products)
  rowsum_matrix <- matrix(data=NA, nrow=num_products, ncol=num_orders-1)
  order <- 1
  product <- 1
  for (product in 1:num_products){
    for (order in 1:num_orders-1){
      rowsum_matrix[,order] <- (ordermatrix[product,order] * ordermatrix[,(order+1)])
      raw_transitionmatrix[,product] <- rowSums(rowsum_matrix)
    }  
  }
  
  #create transposed transition matricies
  rendle_probabilities <- t(raw_transitionmatrix)/denominators
  rendle_transpose <- t(rendle_probabilities)
  
  #calculate probabilities based on previous basket
  calc_probs <- matrix(data=NA, nrow=num_products, ncol=num_products)
  for (product in 1:num_products){
    calc_probs[,product] <- (ordermatrix[product,num_orders])*rendle_transpose[,product]
  }
  
  next_order_probs <- rowSums(calc_probs, na.rm = TRUE)/sum(ordermatrix[, num_orders])
  productprobs <- as_tibble(cbind(allproducts, next_order_probs))
  productprobs <- arrange(productprobs, desc(next_order_probs))
  
  
  # generate predictions based on Personalized Markov chains
  numpredicted <- round(mean(userX_order_summary$item_count, na.rm=TRUE))
  (order_id <- userX_order_summary[["order_id"]][max(userX_order_summary$order_number)] )

  (predicted_purchase <- as.numeric(data.matrix(productprobs[1:numpredicted, 1])))
  (actual_purchase <- as.numeric(data.matrix(userX_train_products[,2])))
  (f1score <- f1(actual_purchase, predicted_purchase))
  
  #include data from observation i into prediction matrix
  prediction_matrix[i,] <- c(id,
                             order_id,
                             paste(predicted_purchase, collapse = " "),
                             paste(actual_purchase, collapse = " "), 
                             f1score)
  
  print(paste("iteration", i, "complete"))
}
Sys.time() - tme

#calculate average F1 score for Personalized Markov Chains
localscores_PMC <- prediction_matrix %>% 
  filter(actual_purchase!=0)  %>% 
  filter(actual_purchase!="") %>% 
  filter(predicted_purchase!="")
mean(as.numeric(localscores_PMC$f1score)) #0.2672749



















