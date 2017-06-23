library(tidyverse)

### Personalized Markov Chains example using a simple dataset ###

#define orders
order1products <- c('apples','bananas','carrots')
order2products <- c('bananas', 'carrots')
order3products <- c('apples', 'bananas')
order4products <- c('carrots')
orderproducts_list <- list(order1products, order2products, order3products, order4products)
allproducts <- c('apples','bananas','carrots')

#count number of products and number of orders
num_products <- length(allproducts)
num_orders <- length(orderproducts_list)

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

for (product in 1:num_products){
  for (order in 1:num_orders-1){
    rowsum_matrix[,order] <- (ordermatrix[product,order] * ordermatrix[,(order+1)])
    raw_transitionmatrix[,product] <- rowSums(rowsum_matrix)
  }  
}

#create transposed transition matricies
rendle_probabilities <- t(raw_transitionmatrix)/denominators
rendle_transpose <- t(rendle_probabilities)

#calculate probabilities for a new order
calc_probs <- matrix(data=NA, nrow=num_products, ncol=num_products)
for (product in 1:num_products){
  calc_probs[,product] <- (ordermatrix[product,num_orders])*rendle_transpose[,product]
}

next_order_prob <- rowSums(calc_probs, na.rm = TRUE)/sum(ordermatrix[, num_orders])
productprobs <- as_tibble(cbind(allproducts, next_order_prob)) %>% 
  rename(product = allproducts) %>% 
  arrange(desc(next_order_prob))

productprobs


  