x = c(20,22,24,26,28)
# calculate the sample variance of a
(x_samp_var = 1/(length(x) - 1) * sum( (x - mean(x))^2) )
#> [1] 10
# xcheck
var(x)
# [1] 10

y = c(10,11,0,13,14)
z = c(-20,-22,-7,-30,-22)

M = cbind(x, y, z)

# Subtract the mean from each feature/column
M_Centered = apply(M,2,function(X) X - mean(X))
# Get the covariance matrix
( covar = 1/(nrow(M)-1) * t(M_Centered) %*% M_Centered )
#    a     b     c
# a 10   5.0  -6.0
# b  5  31.3 -42.6
# c -6 -42.6  69.2

#calculate the correlation matrix
diag_c = diag(diag(covar)^(-1/2))
( cor = diag_c %*% covar %*% diag_c )
#            [,1]       [,2]       [,3]
# [1,]  1.0000000  0.2826167 -0.2280858
# [2,]  0.2826167  1.0000000 -0.9153441
# [3,] -0.2280858 -0.9153441  1.0000000

#xcheck
cor(M)
#            a          b          c
# a  1.0000000  0.2826167 -0.2280858
# b  0.2826167  1.0000000 -0.9153441
# c -0.2280858 -0.9153441  1.0000000