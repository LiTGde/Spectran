a <- read.csv('/private/tmp/spectran-review-oct03/M4-04/manifest.csv')
b <- read.csv('/private/tmp/spectran-review-oct03/M4-05/manifest.csv')
m <- merge(a,b,by='file',all=TRUE,suffixes=c('_before','_after'))
print(m$file[is.na(m$md5_before) | is.na(m$md5_after) | m$md5_before != m$md5_after])
root <- '/private/tmp/spectran-review-oct03/M4-05/source'
bytes <- function(path) {
  f <- list.files(path,recursive=TRUE,full.names=TRUE)
  sum(file.info(f)$size,na.rm=TRUE)
}
web <- bytes(file.path(root,'inst/app/www'))
entries <- utils::untar('/private/tmp/spectran-review-oct03/M4-05/Spectran_1.0.6.tar.gz', list=TRUE)
manual_entries <- entries[grepl('^Spectran/man/', entries) & !endsWith(entries, '/')]
manual_paths <- sub('^Spectran/', paste0(root, '/'), manual_entries)
manual <- sum(file.info(manual_paths)$size)
stopifnot(!any(grepl('man/figures/English|data-raw|tests/verification', entries)))
source <- file.info('/private/tmp/spectran-review-oct03/M4-05/Spectran_1.0.6.tar.gz')$size
values <- data.frame(item=c('source_tar_gz','all_web_assets','shipped_manual_source','web_plus_shipped_manual_source'),bytes=c(source,web,manual,web+manual))
print(values)
write.csv(values,'/private/tmp/spectran-review-oct03/M4-05/size.csv',row.names=FALSE)
stopifnot(source < 1e7, web+manual < 5e6)
