IPCC <- read.csv("IPCCfinal.csv", sep=",")

swpcalcdata <- read.csv("swpcalcdata.csv", sep=",")

names(IPCC) <- c("1a", "1b", "2a", "2b", "3", "4", "5", "6", "7")
IPCC <- cbind(Years = 1991:2020, IPCC)

paste("Var", names(IPCC))
names(IPCC)[1] <- "Years"

a <- 5
b <- 10
x <- 2
y <- a*x + b

stb <- "~waiting, ~eruptions"
faithful %>%
  ggvis(as.list(stb)) %>%
  layer_smooths()

vart <- "NSP.Exports"
data.frame(Years = 1900:2020, vart = 1900:2020)



# Use a small value of n for these examples
mtcars %>% compute_model_prediction(mpg ~ wt, n = 10)
mtcars %>% compute_model_prediction(mpg ~ wt, n = 10, se = TRUE)
mtcars %>% group_by(cyl) %>% compute_model_prediction(mpg ~ wt, n = 10)

# compute_smooth defaults to loess
mtcars %>% compute_smooth(mpg ~ wt)

# Override model to suppress message or change approach
mtcars %>% compute_model_prediction(mpg ~ wt, n = 10, model = "loess")
mtcars %>% compute_model_prediction(mpg ~ wt, n = 10, model = "lm")

# Set the domain manually
strg <- "mpg~wt"
as.formula(get(strg))
mtcars %>%
  compute_model_prediction(as.formula(strg), n = 20, model = "lm", domain = c(0, 8))

vard <- "NSP.Exports"
form <- as.formula(paste(vard, "Years", sep="~"))
swpcalcdata %>%
  ggvis(as.formula("~Years"), ~NSP.Exports) %>%
  compute_model_prediction(form, n = 11, model = "lm", domain = c(2013,2023)) %>%
  ggvis(~pred_, ~resp_) %>%
  layer_lines()

# Plot the results
mtcars %>%
  compute_smooth(mpg ~ wt,
                           span = sliderInput("slid",
                                              "Span Slider",
                                              min = 0.1,
                                              max = 2, 
                                              value = .5)) %>%
  ggvis(~pred_, ~resp_) %>%
  layer_paths()
mtcars %>% ggvis() %>%
  compute_model_prediction(mpg ~ wt) %>%
  layer_paths(~pred_, ~resp_)

mtcars %>%
  ggvis(~mpg, ~wt) %>%
  layer_points() %>%
  layer_smooths(span = input_slider(.2,2), domain = c(10.4, 50))
  
mtcars %>%
  ggvis(~mpg, ~wt) %>%
  layer_points() %>%
  layer_model_predictions(model = "lm", domain = c(10.4, 50))

mtcars %>%
  ggvis(~mpg, ~wt) %>%
  layer_points() %>%
  layer_model_predictions(model = "loess", domain = c(10.4, 50))