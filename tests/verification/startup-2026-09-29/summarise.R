# Summarize paired startup measurements and sampled R call-stack costs.
root <- '/private/tmp/spectran-startup-2026-09-29'
files <- list.files(root, '-session-.*-browser.csv$', full.names=TRUE)
browser <- do.call(rbind, lapply(files, function(f) {x <- read.csv(f); x$file<-basename(f); x$recorded_at <- as.character(file.info(f)$mtime); x}))
browser <- browser[order(browser$recorded_at),]
browser$run <- ave(seq_len(nrow(browser)),browser$label,FUN=seq_along)
browser$phase <- ifelse(browser$run == 1,'first_session','repeat')
write.csv(browser,file.path(root,'browser-measurements.csv'),row.names=FALSE)
warm <- browser[browser$phase=='repeat',]
summary <- do.call(rbind,lapply(split(warm,warm$label),function(x) data.frame(label=x$label[[1]],n=nrow(x),ready_median_ms=median(x$ready_ms),ready_min_ms=min(x$ready_ms),ready_max_ms=max(x$ready_ms),connected_median_ms=median(x$connected_ms),after_connect_median_ms=median(x$ready_ms-x$connected_ms),resource_count_median=median(x$resource_count),resource_transfer_bytes_median=median(x$resource_transfer_bytes),dom_nodes_median=median(x$dom_nodes))))
write.csv(summary,file.path(root,'browser-summary.csv'),row.names=FALSE)
print(summary,row.names=FALSE)
files <- list.files(root, '-process-[1-5]-times.csv$', full.names=TRUE)
process <- do.call(rbind,lapply(files,function(f){x<-read.csv(f);x$label<-sub('-process-.*','',basename(f)); x$run<-as.integer(sub('.*-process-([1-5])-times.csv','\\1',f)); x}))
process_summary <- do.call(rbind,lapply(split(process,process$label),function(x) data.frame(label=x$label[[1]],n=nrow(x),load_median_s=median(x$load_seconds),construct_median_s=median(x$construct_seconds),render_median_s=median(x$render_seconds),html_bytes=median(x$html_bytes))))
write.csv(process,file.path(root,'process-measurements.csv'),row.names=FALSE)
write.csv(process_summary,file.path(root,'process-summary.csv'),row.names=FALSE)
print(process_summary,row.names=FALSE)
# Inclusive times overlap across nested functions and must not be summed.
profiles <- do.call(rbind,lapply(seq_len(nrow(warm)),function(i){f<-sub('-browser.csv$','-total.csv',file.path(root,warm$file[[i]]));x<-read.csv(f);names(x)[[1]]<-'function_name';x$label<-warm$label[[i]];x$run<-warm$run[[i]];x}))
write.csv(profiles,file.path(root,'profile-measurements.csv'),row.names=FALSE)
keep <- grepl('transmissionServer|transmissionHistoryServer|transmissionApplyServer|transmission_info_tooltip_server|importServer|analysisServer|exportServer|prepare_transmission|material_strings|filtered_catalogue|sysdata|readRDS',profiles$function_name)
key <- profiles[keep,]
key_summary <- aggregate(cbind(total.time,self.time)~label+function_name,key,median)
write.csv(key_summary,file.path(root,'profile-summary.csv'),row.names=FALSE)
print(key_summary[key_summary$label=='b08',c('function_name','total.time','self.time')],row.names=FALSE)
orig <- summary$ready_median_ms[summary$label=='original']; mat<-summary$ready_median_ms[summary$label=='b08']; absent<-summary$ready_median_ms[summary$label=='b08_no_material_server']
cat('B08 added milliseconds:',mat-orig,'; relative increase %:',100*(mat/orig-1),'\n')
cat('Removing the backend recovers % of added median time:',100*(mat-absent)/(mat-orig),'\n')
