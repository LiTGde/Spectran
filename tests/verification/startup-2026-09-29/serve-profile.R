# Measure package/UI startup and real browser sessions without changing the app sources.
args <- commandArgs(trailingOnly=TRUE)
.libPaths(c('/Users/zauner/Library/Caches/org.R-project.R/R/renv/library/Spectran-fd34ab45/macos/R-4.6/aarch64-apple-darwin23',.libPaths()))
options(sass.cache='/private/tmp/spectran-sass-cache')
source_path <- args[[1]]
label <- args[[2]]
port <- as.integer(args[[3]])
output_dir <- '/private/tmp/spectran-startup-2026-09-29'
clock <- function() unname(proc.time()[['elapsed']])
t <- clock()
pkgload::load_all(source_path, quiet=TRUE)
package_seconds <- clock()-t
if (identical(label, 'b08_no_material_server')) {
  assignInNamespace('transmissionServer', function(...) list(history=shiny::reactive(NULL), promotion_event=shiny::reactive(NULL), restore_event=shiny::reactive(NULL)), ns='Spectran')
}
t <- clock()
app <- Spectran('Deutsch')
ui_seconds <- clock()-t
write.csv(data.frame(label, package_seconds, ui_seconds),file.path(output_dir,paste0(label,'-server-start.csv')),row.names=FALSE)
server <- app$serverFuncSource()
app$serverFuncSource <- function() function(input, output, session) {
  token <- session$token
  prefix <- file.path(output_dir,paste0(label,'-session-',token))
  started <- clock()
  Rprof(paste0(prefix,'.Rprof'), interval=.005)
  session$onFlushed(function() {
    cat(sprintf('FLUSH %s %.4f\n', token, clock()-started))
  }, once=FALSE)
  shiny::observe({
    values <- input$startup_bench
    shiny::req(values)
    Rprof(NULL)
    values$label <- label
    values$server_since_start_seconds <- clock()-started
    values$session <- token
    write.csv(as.data.frame(values),paste0(prefix,'-browser.csv'),row.names=FALSE)
    profile <- summaryRprof(paste0(prefix,'.Rprof'))
    write.csv(profile$by.total,paste0(prefix,'-total.csv'))
    write.csv(profile$by.self,paste0(prefix,'-self.csv'))
    cat('BENCHMARK_READY ',token,'\n')
  }) |> shiny::bindEvent(input$startup_bench)
  server(input, output, session)
}
handler <- app$httpHandler
app$httpHandler <- function(req) {
  result <- handler(req)
  if (!is.null(result) && identical(req$PATH_INFO,'/') && is.character(result$content)) {
    script <- '<script>(function(){let connected=0, firstIdle=0, lastIdle=0, sent=false, timer;function finish(){if(sent||!connected)return;sent=true;let nav=performance.getEntriesByType("navigation")[0];let resources=performance.getEntriesByType("resource");let data={connected_ms:connected,first_idle_ms:firstIdle,ready_ms:lastIdle,dom_content_loaded_ms:nav.domContentLoadedEventEnd,load_ms:nav.loadEventEnd,resource_count:resources.length,resource_transfer_bytes:resources.reduce((a,x)=>a+x.transferSize,0),resource_encoded_bytes:resources.reduce((a,x)=>a+x.encodedBodySize,0),dom_nodes:document.querySelectorAll("*").length};Shiny.setInputValue("startup_bench",data,{priority:"event"});let badge=document.createElement("div");badge.id="startup-benchmark-ready";badge.textContent="Startup benchmark complete: "+lastIdle.toFixed(0)+" ms";badge.style="position:fixed;bottom:0;right:0;z-index:99999;background:white;color:black;padding:5px";document.body.appendChild(badge);}document.addEventListener("DOMContentLoaded",()=>{ $(document).on("shiny:connected",()=>{connected=performance.now();});$(document).on("shiny:busy",()=>{clearTimeout(timer);});$(document).on("shiny:idle",()=>{lastIdle=performance.now();if(!firstIdle)firstIdle=lastIdle;clearTimeout(timer);timer=setTimeout(finish,750);});});})();</script>'
    result$content <- sub('</head>',paste0(script,'</head>'),result$content,fixed=TRUE)
  }
  result
}
shiny::runApp(app, host='127.0.0.1', port=port, launch.browser=FALSE)
