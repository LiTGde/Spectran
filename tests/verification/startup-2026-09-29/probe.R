args <- commandArgs(trailingOnly=TRUE)
.libPaths(c('/Users/zauner/Library/Caches/org.R-project.R/R/renv/library/Spectran-fd34ab45/macos/R-4.6/aarch64-apple-darwin23',.libPaths()))
options(sass.cache='/private/tmp/spectran-sass-cache')
clock <- function() unname(proc.time()[['elapsed']])
out <- args[[2]]
Rprof(paste0(out,'-load.Rprof'), interval=.005)
t <- clock()
pkgload::load_all(args[[1]], quiet=TRUE, export_all=TRUE)
load <- clock()-t
Rprof(NULL)
Rprof(paste0(out,'-ui.Rprof'), interval=.005)
t <- clock()
app <- Spectran('Deutsch')
construct <- clock()-t
t <- clock()
html <- app$httpHandler(list(REQUEST_METHOD='GET', PATH_INFO='/', HTTP_USER_AGENT='Spectran startup benchmark'))
render <- clock()-t
Rprof(NULL)
write.csv(data.frame(load_seconds=load, construct_seconds=construct, render_seconds=render, html_bytes=nchar(html$content,type='bytes')),paste0(out,'-times.csv'),row.names=FALSE)
saveRDS(html,paste0(out,'-response.rds'))
for (phase in c('load','ui')) {
  profile <- summaryRprof(paste0(out,'-',phase,'.Rprof'))
  write.csv(profile$by.total, paste0(out,'-',phase,'-total.csv'))
  write.csv(profile$by.self, paste0(out,'-',phase,'-self.csv'))
}
writeLines(capture.output(sessionInfo()), paste0(out,'-sessionInfo.txt'))
