library(reticulate)
library(tidyverse)

use_python("/usr/local/bin/python")
gensim <- import("zstd")