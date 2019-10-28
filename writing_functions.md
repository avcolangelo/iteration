Writing Functions
================
Alexis
10/28/2019

## Get started

We’re going to write some functions.

Z-scores

``` r
x = rnorm(n = 30, mean = 4, sd = 2.3)
x_again = rnorm(n = 30, mean = 6, sd = .3)
y = rnorm(n = 30, mean = 24, sd = 2.3)

(x - mean(x)) / sd(x)
```

    ##  [1] -0.3140192 -1.6350565 -0.7696545  0.3342366 -0.5526366 -0.9030387
    ##  [7] -0.2122820 -0.8077893  0.4512972 -1.2938035  0.2115149  0.9856593
    ## [13] -0.7270493 -0.3370725  0.6615875  2.4551631  0.1316096 -1.2514333
    ## [19]  2.4056633  0.4741518  1.0554965 -0.5506119  1.0279529  0.1109214
    ## [25]  0.8565408 -1.0412652 -0.1478329  0.4769739  0.1434506 -1.2386741

``` r
(x_again - mean(x_again)) / sd(x_again)
```

    ##  [1]  0.6607998  0.2966306  0.2095765 -1.6068599 -1.9998986 -0.9348292
    ##  [7]  1.3955972  1.0212020 -1.8740385  0.3730844 -1.0448877  0.8990551
    ## [13] -1.0621657  1.3506372 -0.1839726  0.4162264  0.5573816 -0.6807776
    ## [19]  0.4488487  0.3718977 -1.1514649 -0.2370535  1.1612836  0.5214244
    ## [25]  0.1499360  1.5585404  0.4653074  0.5492178 -1.4706641 -0.1600341

Now a function

``` r
z_score = function(x_arg) {
  
  if (!is.numeric(x_arg)) {
    stop("x should be numeric")
  } else if (length(x_arg) < 3) {
    stop("x should be longer than 3")
  }
  
  (x_arg - mean(x_arg)) / sd(x_arg)
}
```

Try out the
    function.

``` r
z_score(x_arg = y)
```

    ##  [1]  0.127209823  0.060659945 -0.890627624  0.663610054 -0.773029039
    ##  [6] -2.879382021 -1.506600625  1.051134638  0.631947058  0.523863261
    ## [11] -0.227375899 -0.005714910  0.585453726  0.010992838  0.499009233
    ## [16]  0.380767965  1.368848228  1.517540389 -0.684373747  1.201672425
    ## [21] -1.473152173  0.886258671  0.194629656 -0.716078162 -0.636405227
    ## [26]  0.006818575  0.132170699  1.303882857 -1.509552107  0.155821492

``` r
#these should not work given the function:
#z_score(x_arg = 3)
#z_score(x_arg = "my name is jeff")
#z_score(x_arg = c(TRUE, TRUE, FALSE, TRUE))
#z_score(x_arg = iris)
```

## Multiple outputs

``` r
mean_and_sd = function(input_x) {
  
   if (!is.numeric(input_x)) {
    stop("x should be numeric")
  } else if (length(input_x) < 3) {
    stop("x should be longer than 3")
  }

tibble(
  mean_input = mean(input_x),
  sd_input = sd(input_x)  
)

}
```

test this function

``` r
mean_and_sd(input_x = y)
```

    ## # A tibble: 1 x 2
    ##   mean_input sd_input
    ##        <dbl>    <dbl>
    ## 1       23.8     2.17

using list

``` r
mean_and_sd = function(input_x) {
  
   if (!is.numeric(input_x)) {
    stop("x should be numeric")
  } else if (length(input_x) < 3) {
    stop("x should be longer than 3")
  }
  
  list(
    mean_input = mean(input_x),
    sd_input = sd(input_x),
    z_score = (input_x - mean(input_x)) / sd(input_x)
)
}
```

``` r
mean_and_sd(input_x = y)
```

    ## $mean_input
    ## [1] 23.79207
    ## 
    ## $sd_input
    ## [1] 2.171615
    ## 
    ## $z_score
    ##  [1]  0.127209823  0.060659945 -0.890627624  0.663610054 -0.773029039
    ##  [6] -2.879382021 -1.506600625  1.051134638  0.631947058  0.523863261
    ## [11] -0.227375899 -0.005714910  0.585453726  0.010992838  0.499009233
    ## [16]  0.380767965  1.368848228  1.517540389 -0.684373747  1.201672425
    ## [21] -1.473152173  0.886258671  0.194629656 -0.716078162 -0.636405227
    ## [26]  0.006818575  0.132170699  1.303882857 -1.509552107  0.155821492

## Multiple inputs
