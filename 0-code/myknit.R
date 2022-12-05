myknit <- function (inputFile, encoding) {
  rmarkdown::render(
    inputFile,
    clean = TRUE,
    encoding = encoding,
    output_format = "all"
  )

  if (file.exists('index.pdf')) file.rename('index.pdf', 'oeconomia.pdf')
  if (file.exists('index.html')) file.rename('index.html', 'oeconomia.html')
  if (file.exists('index.docx')) file.rename('index.docx', 'oeconomia.docx')

  for(fend in c("snm", "aux", "out", "bcf", "nav", "run.xml", "toc", "log", "ptc", "blg", "bbl")){
    system(paste("rm *.", fend, sep=""), ignore.stderr=TRUE)
  }
}