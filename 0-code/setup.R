# Set up document (ggplot defaults, knitr hooks, etc.)

knitr::opts_chunk$set(
  echo=FALSE,
  cache=TRUE,
  warning=FALSE,
  dpi=600,
  warning=FALSE,
  tidy=TRUE,
  dev = c("pdf", "png")
)

library(knitr)
library(tidyverse)
library(janitor)
library(haven)
library(ggpubr)
library(english)
library(readxl)
library(DescTools)
library(grid)
library(gridExtra)
library(tools)
library(ddpcr)
library(pdftools)
library(kableExtra)

# Directory for cache.
opts_chunk$set(cache.path = '.index_latex_cache/')

# Plot theme.
theme <- theme_set(theme_light())
theme <- theme_update(
  plot.title=element_text(colour="gray25", hjust=0.5, size=8),
  axis.text.x=element_text(colour="gray25", size=7),
  axis.text.y=element_text(colour="gray25", size=7),
  axis.title.x=element_text(colour="gray25", size=8),
  axis.title.y=element_text(colour="gray25", size=8),
  legend.position="bottom",
  legend.title = element_blank(),
  legend.text=element_text(colour="gray25", size=8),
  panel.border=element_blank(),
  legend.background=element_rect(colour = "transparent", fill=NA),
  legend.key.width = unit(0.7,"cm"),
  strip.background=element_rect(fill="white"),
  strip.text=element_text(colour="grey25", size=7, face="bold")
)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Hook for figures
knit_hooks$set(plot = function(x, options) {
      return(paste("\n\\begin{figure}", ifelse(!is.null(options$fig.placement), paste("[", options$fig.placement, "!", "]", sep = ''), ""), "\n",
                   "\\centering",
                   "\n\\includegraphics", ifelse(isTRUE(options$fig.manualwidth), paste("[width=", options$fig.width, "\\linewidth]", sep=""), "[width=\\linewidth]"), "{", opts_knit$get("base.url"), paste(x, collapse = "."), "}\n",
                   "\n\\caption{", options$fig.cap, "}\n", "\\label{fig:", options$label, "}\n",
                   ifelse(!is.null(options$fig.note), paste("\\justify\\footnotesize\\textit{Note}. ", options$fig.note), ""),
                   "\n\\end{figure}\n",
                   sep = '')
    )
  }
)

# Suppress NA in Kable tables.
options(knitr.kable.NA = '')
