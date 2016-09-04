###############################################
### Appendix H  Analysis of bee health Healthy B
### version Sept 5 2016
###############################################

## Figure H1

## Function to generate artifical data
generate.data <- function(n = 50, noise = 2){
x1 <- rexp(n,2); xx1 <- scale(x1) 
x2 <- rnorm(n); xx2 <- scale(x2)
x3 <- rnorm(n); xx2 <- scale(x2)
y1 <- x1 + 10*x2 + x2*x1 + x3 + 5*x3^2*x2 + rnorm(n,0,noise); yy1 <- scale(y1)
y2 <- x1 - 10*x3  + rnorm(n,0,noise); yy2 <- scale(y2)
y3 <- -x1*x3*x2 + rnorm(n,0,noise); yy3 <- scale(y3)
y4 <- -y3 + x1 + rnorm(n,0,noise); yy4 <- scale(y4)
y5 <- x3 + x1 * y2 + rnorm(n,0,noise); yy5 <- scale(y5)
y6 <- y1 + y2 + y3*y4 + 2*y5
y7 <- y5 + y6
predictors=cbind(x1,x2,x3)
colnames(predictors) <- c('Management','RPU','Environmental drivers')
response = cbind(y1,y2,y3,y4,y5)
colnames(response) <- c('Queen','Disease','Products','Behaviour','Demography')
outputs = cbind(y6,y7)
colnames(outputs) = c('Pollination service','Harvested honey')
bee <- list(predictors=predictors,response = response,outputs=outputs)
return(bee)
}

## load packages
library('plsdepot')
library('ggbiplot')
library('devtools')
library('plspm')


## Generate data
bee <- generate.data(n = 50, noise = 2)

## Run a PCA on the response variables
bee_response = prcomp(bee$response)
## Run a PCA on the predictor variables
bee_drivers = prcomp(bee$predictors)
## Run a PLS on the response and predictor variables
bee.pls <- plsreg2(bee$predictors,bee$response,comps=2,crosval=TRUE)

## Make plots of attributes (response variables)
g <- ggbiplot(bee_response, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

### make plots of the predictors
h <- ggbiplot(bee_drivers, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              circle = TRUE)
h <- h + scale_color_discrete(name = '')
h <- h + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(h)

## Make plot of the PLS
plot(bee.pls,what='variables',col.xarrows="#5592e3",lwd=1,
     col.yarrows="#fe9429",xlab='HSI 1-dim',ylab='HSI 2-dim',
     main='')

########################
## PLS path analysis


#Identify blocks in the data. Here we use DRIVERS and INDICATORS as blocks. 

# Define a path model matrix showing how the latent variables for the blocks are influencing each other
DRIVERS = c(0,0,0)
ATTRIBUTES = c(1,0,0)
OUTPUTS = c(0,1,0)
bee_path = rbind(DRIVERS,ATTRIBUTES,OUTPUTS)
colnames(bee_path) = rownames(bee_path)

# The value show if a column is affecting that row. Here the DRIVERS is affecting the INDICATORS.
bee_path
# Plot the path model matrix 
innerplot(bee_path)
# Specify a list of blocks for the outer model, i.e. which variables are associated to which block
bee_blocks = list(
  colnames(bee$predictors),
  colnames(bee$response),
  colnames(bee$outputs)
)

# Scale each variable
# "nom","ord","raw"
bee_scaling = list(
  rep("raw",length(bee_blocks[[1]])),
  rep("raw",length(bee_blocks[[2]])),
  rep("raw",length(bee_blocks[[3]]))
)

# Fit the PLS path analysis
beedata <- cbind(bee$predictors,bee$response,bee$outputs)
bee_pls = plspm(beedata,bee_path,bee_blocks,scaling=bee_scaling)

# study the output
bee_pls
summary(bee_pls)
bee_pls$path_coefs

# plot the interaction between latent components
plot(bee_pls)

# plot the interaction within latent components
# use  zoom if it is difficult to see the plot
plot(bee_pls,what = "loadings", arr.width = 0.2)

plot(bee_pls, what = "weights", arr.width = 0.2)


