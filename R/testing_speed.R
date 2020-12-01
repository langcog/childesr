library(tidyverse)
library(magrittr)
source("R/childesr.R")
source("R/updated_childesr.R")

updated <-  get_contexts_u(corpus = "Brown",
                           role = "target_child",
                           target_child = "Adam",
                           token = c("dog", "ball"))

regular <- get_contexts(corpus = "Brown",
                            role = "target_child",
                            target_child = "Adam",
                            token = c("dog", "ball"))

