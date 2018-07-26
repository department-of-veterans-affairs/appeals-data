library(tidyverse)
library(simmer)
library(simmer.plot)

set.seed(42)

env <- simmer("AMA")

update_capacity <- function(env) {
  env %>%
    set_capacity("direct review", )
}

direct_review <- trajectory("direct review trajectory") %>%
  update_capacity %>%
  seize("direct review", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("direct review", 1)

env %>%
  add_resource("direct review") %>%
  add_generator("direct reviews", direct_review, function() rnorm(1, 1, .1))

run(env, 365)
get_mon_resources(env)

