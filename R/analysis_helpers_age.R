#Function to calculate the spectral age-dependent transmission of the eye, according to the DIN/TS 5031-100
prerecep_filter <- function(wavelength, age) {
  #Density
  Dichte <- 
    (0.15 + 0.000031*age^2)*
    (400/wavelength)^4+151.5492*exp(-((0.057*(wavelength-273))^2)) +
    (1.05-0.000063*age^2)*2.13*exp(-((0.029*(wavelength-370))^2)) +
    (0.059 + 0.000186*age^2)*11.95*exp(-((0.021*(wavelength-325))^2)) +
    (0.016 + 0.000132*age^2)*1.43*exp(-((0.008*(wavelength-325))^2))+0.06
  #Return value
  10^(-Dichte)
}

#Prereceptoral Filtering for a 32yr-old reference observer
Tau32 <- tibble::tibble(Wellenlaenge = 380:780,
                        Tau = prerecep_filter(Wellenlaenge, 32))

#Function to calculate the age dependent relative transmission
Tau_rel_fun <- function(age){
  WL <- 380:780
  prerecep_filter(WL, age)/prerecep_filter(WL, 32)
}

#Function to calculate the age dependent transmission (Scalar)
k_trans_fun <- function(age, mel_wt_Spectrum){
  #Lightspectrum*melanopicActionSpectrum(=mel_wt_Spectrum)*rel_Transmission
  WL <- 380:780
  Tau_rel <- Tau_rel_fun(age)
  zahler <- sum(
    mel_wt_Spectrum * Tau_rel
  )
  nenner <- sum(mel_wt_Spectrum)
  zahler / nenner
  
}

#Function to calculate the age-dependent reduction in pupil size
k_pup_fun <- function(age){
  c <- 0.00612
  (1-c*(age-32))^2
}

#Function to calculate the total age-dependent correction factor
k_mel <- function(age, mel_wt_Spectrum) {
  k_trans_fun(age, mel_wt_Spectrum) * k_pup_fun(age)
}

# #Plot zur Darstellung des relativen Pupillendurchmessers im Alter
# plot_alter_pup <- function(size_font = 15){
#   k <- k_pup_fun(input$Alter)
#   p1 <- tau_alter$Korrekturfaktoren %>%
#     ggplot(aes(x=Alter, y = k_pup))+
#     geom_line(lwd = 1)+
#     labs(title = lang$server(92))+
#     theme_cowplot(font_size = size_font, font_family = "sans")+
#     annotate("point", x= 32, y = k_pup_fun(32), col = "grey60", size = 5)+
#     annotate("segment", x= 32, xend = 32, y = -1, yend = k_pup_fun(32), col = "grey60", lty = 5)+
#     annotate("segment", x= -5, xend = 32, y = k_pup_fun(32), yend = k_pup_fun(32), col = "grey60", lty = 5)+
#     annotate("point", x= input$Alter, y = k_pup_fun(input$Alter), col = "red", size = 5)+
#     annotate("segment", x= input$Alter, xend = input$Alter, y = -1, yend = k, col = "red", lty = 5)+
#     annotate("segment", x= -5, xend = input$Alter, y = k, yend = k, col = "red", lty = 5)+
#     annotate("label", x= 0, y = k, col = "red", label = paste0(round(k*100,0),"%"), size = 4/15*size_font)+
#     annotate("label", x= input$Alter, y = 0, col = "red", label = input$Alter, size = 4/15*size_font)+
#     ylab(label = lang$server(93)) +
#     scale_y_continuous(labels = scales::percent_format(scale = 100))+
#     scale_x_continuous(breaks=c(0, 25, 32, 50, 75, 100))+
#     xlab(label = lang_ui[116])+
#     coord_cartesian(xlim = c(0, 100), ylim = c(0, 1.43))
#   p1
# }
# 
# 
# #Plot mit der Darstellung aller altersabhängigen Korrekturen
# 
# plot_alter_ges <- function(size_font = 15){
#   k_pup <- k_pup_fun(input$Alter)
#   Typ2 <- Rezeptorkurven[["Melanopsin"]]
#   Tau_werte <- tau_alter$Transmission %>% filter(Alter == input$Alter)
#   p1 <- plot_alter_basis(size_font)+
#     labs(subtitle = paste0(lang$server(95), input$Alter, lang$server(96)))+
#     {if(input$Hintergrund3){
#       geom_bar(stat = "identity", aes(col = Wellenlaenge, fill = Wellenlaenge, y = Bestrahlungsstaerke*1000), alpha = 0.03, lwd = 1)}}+
#     {if(input$Hintergrund3){
#       geom_bar(stat = "identity", aes(col = Wellenlaenge, fill = Wellenlaenge, y = Bestrahlungsstaerke*1000*Typ2), alpha = 0.2, lwd = 1)}}+
#     {if(input$Hintergrund3){
#       geom_bar(stat = "identity", aes(col = Wellenlaenge, fill = Wellenlaenge, y = Tau_werte$Tau_rel*Bestrahlungsstaerke*1000*Typ2), alpha = 0.4, lwd = 1)}}+
#     geom_ridgeline_gradient(aes(y = 0, height =Tau_werte$Tau_rel*Bestrahlungsstaerke*1000*Typ2*k_pup, fill = Wellenlaenge))+
#     geom_path(aes(y = Tau_werte$Tau_rel*Bestrahlungsstaerke*1000*Typ2*k_pup, lty = "1"), lwd = 1,  show.legend=FALSE)+
#     geom_path(aes(y = Bestrahlungsstaerke*1000*Typ2, lty = "5"), lwd = 0.75,  show.legend=FALSE)+
#     geom_path(aes(y = Tau_werte$Tau_rel*Bestrahlungsstaerke*1000*Typ2, lty = "2"), lwd = 1,  show.legend=FALSE)+
#     geom_path(aes(lty = "3"), lwd = 0.75)+
#     {if(input$Alter_mel){
#       geom_path(data = Rezeptorkurven2 %>% filter(Typ %in% "Melanopsin"),
#                 aes(x=Wellenlaenge, y= rel_Empfindlichkeit*Tau_werte$Tau_rel*spectrum$max_E*k_pup), col = "#1D63DC", lwd = 0.75)}}+
#     scale_linetype_manual(name = lang$server(82), values = c("3" =3,"5" = 5, "2" = 4, "1" = 1), labels = c(lang$server(83), lang$server(84), paste0(lang$server(85), input$Alter, lang$server(86)), paste0(lang$server(94), input$Alter, lang$server(86))))+
#     {if(input$Alter_mel){
#       geom_label_repel(data = Bewertung() %>% filter(Bezeichnung %in% "Melanopsin"),
#                        aes(x=Peak, y= spectrum$max_E*Tau_werte$Tau_rel[111]*k_pup, label = Bezeichnung), col = "#1D63DC",alpha = 0.85,
#                        min.segment.length = 0, ylim = c(spectrum$max_E, spectrum$max_E*1.17), parse = TRUE, size = 4/15*size_font)}}
#   p1
# }
# 
# table_age <- function(laenge = 1:5, Breite = 100){
#   tau_alter$Tabelle %>%
#     select(!"Formelzeichen") %>%
#     slice(laenge) %>%
#     gt(rowname_col = "Größe") %>%
#     fmt(columns = "Zeichen", rows = 1, fns = HTML) %>%
#     fmt(columns = "Zeichen", rows = 2, fns = HTML) %>%
#     fmt(columns = "Zeichen", rows = 3, fns = HTML) %>%
#     fmt(columns = "Zeichen", rows = 4, fns = HTML) %>%
#     fmt(columns = "Zeichen", rows = 5, fns = HTML) %>%
#     tab_footnote(
#       footnote =  HTML(lang$server(97)),
#       locations = cells_stub(rows = starts_with("MEDI"))
#     ) %>%
#     tab_source_note(source_note = Quelle_Alter) %>%
#     tab_source_note(source_note = withMathJax("")) %>%
#     tab_header(title = strong(spec_name()),
#                subtitle = table_subtitle(lang$server(98))) %>%
#     tab_style(style = cell_fill(color = "grey95"), locations = list(cells_body(columns = everything(), rows = 4:5),
#                                                                     cells_stub(rows = 4:5))) %>%
#     opt_align_table_header(align = "left") %>%
#     Number_formatting_tables() %>% 
#     tab_options(column_labels.hidden = TRUE, table.width = pct(Breite))
# }
# 
# #Erstellt einen Basisplot für spektrale Elemente abhängig des Alters
# plot_alter_basis <- function(size_font = 15){
#   Tau_werte <- tau_alter$Transmission %>% filter(Alter == input$Alter)
#   Typ2 <- Rezeptorkurven[["Melanopsin"]]
#   p1 <- Plot_01(size_font)+
#     theme(legend.position = c(1, 0.07), legend.key.width=unit(1,"cm"), legend.text.align = 0, legend.justification = c(1,0), legend.background = element_rect(fill = "#FFFFFFDD"))+
#     coord_cartesian(xlim = c(380, 780), ylim = c(0, spectrum$max_E*1.1*input$plot_multiplier))
#   p1
# }
# 
# 
# 
# #Erstellt einen Plot, der die Transmission der Augenmedien mit dem Alter darstellt.
# #Teil 1 des Plots
# plot_alter_trans_u1 <- function(size_font = 15) {
#   Tau_werte <- tau_alter$Transmission %>% filter(Alter == input$Alter)
#   Typ2 <- Rezeptorkurven[["Melanopsin"]]
#   p1 <- plot_alter_basis(size_font)+
#     labs(subtitle = paste0(lang$server(81)))+
#     {if(input$Hintergrund3){
#       geom_bar(stat = "identity", aes(col = Wellenlaenge, fill = Wellenlaenge, y = Bestrahlungsstaerke*1000), alpha = 0.03, lwd = 1)}}+
#     {if(input$Hintergrund3){
#       geom_bar(stat = "identity", aes(col = Wellenlaenge, fill = Wellenlaenge, y = Bestrahlungsstaerke*1000*Typ2), alpha = 0.2, lwd = 1)}}+
#     geom_ridgeline_gradient(aes(y = 0, height =Tau_werte$Tau_rel*Bestrahlungsstaerke*1000*Typ2, fill = Wellenlaenge), lwd = 1)+
#     {if(input$Alter_mel){
#       geom_path(data = Rezeptorkurven2 %>% filter(Typ %in% "Melanopsin"),
#                 aes(x=Wellenlaenge, y= rel_Empfindlichkeit*Tau_werte$Tau_rel*spectrum$max_E), col = "#1D63DC", lwd = 0.75)}}+
#     geom_path(aes(y = Bestrahlungsstaerke*1000*Typ2, lty = "5"), lwd = 0.75,  show.legend=FALSE)+
#     geom_path(aes(lty = "3"), lwd = 0.75)+
#     scale_linetype_manual(name = lang$server(82), values = c("3" =3,"5" = 5,  "2" = 1), labels = c(lang$server(83), lang$server(84), paste0(lang$server(85), input$Alter, lang$server(86))))+
#     {if(input$Alter_rel){
#       geom_path(aes(y=Tau_werte$Tau_rel*spectrum$max_E), col = "red", lwd = 0.75)
#     }}+
#     {if(input$Alter_rel){
#       geom_label(data = alters_skala(),aes(x = x, y = y, label = label), hjust = 0, col = "red", size = 4/15*size_font)
#     }}+
#     {if(input$Alter_mel){
#       geom_label_repel(data = Bewertung() %>% filter(Bezeichnung %in% "Melanopsin"),
#                        aes(x=Peak, y= spectrum$max_E*Tau_werte$Tau_rel[111], label = Bezeichnung), col = "#1D63DC",alpha = 0.85,
#                        min.segment.length = 0, ylim = c(spectrum$max_E, spectrum$max_E*1.17), parse = TRUE, size = 4/15*size_font)}}+
#     if(input$Alter_rel){
#       annotate("text", y = max_E()*0.575*input$plot_multiplier, x=364, label = lang$server(87), vjust = 1, col = "red", size = 5/15*size_font, angle = 90)
#     }
#   p1
# }
# #Teil 2 des Plots
# plot_alter_trans_u2 <- function(size_font = 15) {
#   p2 <- ggplot(data = tau_alter$Transmission %>% filter(Alter == input$Alter), aes(Wellenlaenge, y = Tau))+
#     geom_path(data = tau_alter$Transmission %>% filter(Alter == 32), aes(y=Tau), lty = 5)+
#     geom_path()+
#     labs(x = lang$server(100))+
#     ylab(bquote(.(lang$server(99))~paste(tau)[paste(lambda)]))+
#     scale_y_continuous(labels = scales::percent_format(scale = 100))+
#     coord_cartesian(ylim = c(0, 1))+
#     theme_cowplot(font_size = 8/15*size_font)+
#     theme(legend.position = "none", plot.background = element_rect(fill = "#FFFFFF90"))+
#     if(input$Alter_mel){
#       geom_density(stat = "identity", data = Rezeptorkurven2 %>% filter(Typ %in% "Melanopsin"),
#                    aes(x=Wellenlaenge, y= rel_Empfindlichkeit), fill = "#1D63DC", alpha = 0.3, col = NA, lwd = 0.75)}
#   p2
# }
# 
# #Zusammensetzen des Plots (für die App)
# plot_alter_trans_shiny <- function(size_font = 15) {
#   plot_alter_trans_u1(size_font) +
#     if(input$Alter_inset) {
#       inset_element(plot_alter_trans_u2(size_font), left = 0.75, bottom = 0.65, right = 0.98, top = 0.98, align_to = "full")
#     }
# }
