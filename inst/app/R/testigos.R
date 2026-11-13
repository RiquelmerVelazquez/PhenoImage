# Presentación y controles compartidos. No altera los valores de las evaluaciones.
eda_campo_equivalente <- function(d, campo) {
  if (!length(campo) || is.na(campo) || !nzchar(campo)) return(NULL)
  if (campo %in% names(d)) return(campo)
  clave <- function(n) tolower(ib_atributo(n))
  k <- names(d)[vapply(names(d), clave, character(1)) == clave(campo)]
  if (length(k)) k[1] else NULL
}
eda_genotipo_campo <- function(d) {
  k <- names(d)[tolower(sub('^Parcela_', '', names(d))) %in% c('original_i','original_id','genotipo','genotype')]
  k<-k[vapply(k,function(n){v<-as.character(d[[n]]);any(!is.na(v)&nzchar(trimws(v)))},logical(1))]
  if (length(k)) k[1] else 'id'
}
eda_meta_puntos <- function(d) {
  g <- eda_genotipo_campo(d)
  r <- names(d)[tolower(sub('^Parcela_', '', names(d))) %in% c('rep','repeticion','repetition','replicate')]
  list(genotipo = if (g!='id' && g %in% names(d)) as.character(d[[g]]) else rep(NA_character_,nrow(d)),
       repeticion = if(length(r)) as.character(d[[r[1]]]) else rep(NA_character_,nrow(d)))
}
eda_marcas <- function(d, estado = NULL) {
  k <- if (is.null(estado)) NULL else eda_campo_equivalente(d,estado$campo)
  elegidos <- if(is.null(estado)) character() else estado$valores
  color <- if(!is.null(estado$color) && grepl('^#[[:xdigit:]]{6}$',estado$color)) estado$color else '#D18A20'
  marcado <- if (is.null(k)) rep(FALSE,nrow(d)) else !is.na(d[[k]]) & trimws(as.character(d[[k]])) %in% elegidos
  list(testigo=marcado,color=color,campo=k,elegidos=elegidos)
}
eda_tipo_grupo <- function(flags) {
  if (length(flags) && all(flags)) 'Testigo' else if(any(flags)) 'Grupo mixto' else 'Experimental'
}
eda_colores <- function(tipo,color='#D18A20') ifelse(tipo=='Testigo',color,ifelse(tipo=='Grupo mixto','#467F95','#24704A'))
eda_simbolos <- function(tipo) ifelse(tipo=='Testigo',23L,ifelse(tipo=='Grupo mixto',22L,21L))
eda_leyenda <- function(tipo,color) {
  clases <- intersect(c('Experimental','Testigo','Grupo mixto'),unique(tipo))
  graphics::legend('topright',inset=c(0,-.13),legend=c(clases,'Media general'),
    pch=c(eda_simbolos(clases),NA),pt.bg=c(eda_colores(clases,color),NA),col=c(eda_colores(clases,color),'#718079'),
    lty=c(rep(NA,length(clases)),2),lwd=c(rep(NA,length(clases)),1.2),bty='n',cex=.78,xpd=TRUE, horiz=TRUE,pt.cex=1.05)
}
eda_ficha <- function(fila,hover,campos,titulo='Datos del punto') {
  if (is.null(hover) || is.null(fila) || !nrow(fila)) return(NULL)
  f <- fila[1,,drop=FALSE]
  fmt <- function(x) if (!length(x) || is.na(x[1]) || identical(x[1],'')) '—' else if(is.numeric(x)) formatC(x[1],digits=4,format='fg') else as.character(x[1])
  cx <- hover$coords_css$x;cy <- hover$coords_css$y
  if (is.null(cx) || is.null(cy) || !is.finite(cx) || !is.finite(cy)) return(NULL)
  rx <- if(!is.null(hover$img_css_ratio$x) && is.finite(hover$img_css_ratio$x)) hover$img_css_ratio$x else 1
  ancho <- if(!is.null(hover$range$right)) hover$range$right/rx else Inf
  # La ficha se mantiene dentro del panel incluso junto a su borde derecho.
  derecha <- is.finite(ancho) && cx+285>ancho
  ry<-if(!is.null(hover$img_css_ratio$y)&&is.finite(hover$img_css_ratio$y))hover$img_css_ratio$y else 1
  bajo<-if(!is.null(hover$range$bottom))hover$range$bottom/ry else Inf
  arriba<-is.finite(bajo)&&cy+320>bajo&&cy>280
  transformacion<-if(derecha||arriba)paste0('transform:translate(',if(derecha)'-100%' else '0',',',if(arriba)'-100%' else '0',');') else '' 
  shiny::div(class='eda-hover-card',role='tooltip',style=paste0('left:',round(cx+if(derecha)-12 else 12),'px;top:',max(4,round(cy+if(arriba)-12 else 12)),'px;',transformacion),
    shiny::tags$strong(titulo),
    shiny::tags$dl(lapply(names(campos),function(n) shiny::tagList(shiny::tags$dt(n),shiny::tags$dd(fmt(f[[campos[[n]]]]))))))
}
eda_registrar_hover <- function(input,output,id_hover,id_ficha,puntos,campos,titulo='Datos del punto') {
  output[[id_ficha]] <- shiny::renderUI({
    h <- input[[id_hover]]; d <- puntos()
    if(is.null(h) || is.null(d) || !nrow(d)) return(NULL)
    p <- shiny::nearPoints(d,h,xvar='x',yvar='y',threshold=18,maxpoints=1)
    eda_ficha(p,h,campos,titulo)
  })
}
eda_plot_interactivo <- function(id,hover,ficha,height='570px') {
  shiny::div(class='eda-graph-scroll',shiny::div(class='eda-interactive-plot',shiny::plotOutput(id,height=height,hover=shiny::hoverOpts(hover,delay=70,delayType='debounce',nullOutside=TRUE)),shiny::uiOutput(ficha)))
}
registrar_testigos <- function(input,output,session,datos) {
  estado <- shiny::reactiveVal(list(campo='id',valores=character(),color='#D18A20'))
  opciones <- shiny::reactive({
    d<-datos()$tabla; ks<-unique(c(unname(datos()$grupos),'id'));ks<-intersect(ks,names(d))
    stats::setNames(ks,ifelse(ks=='id','ID de parcela',sub('^Parcela_','',ks)))
  })
  valores <- function(campo) {
    d<-datos()$tabla;k<-eda_campo_equivalente(d,campo)
    if(is.null(k)) return(character())
    v<-trimws(as.character(d[[k]])); sort(unique(v[!is.na(v)&nzchar(v)]))
  }
  shiny::observeEvent(datos(),{
    e<-estado();ks<-unname(opciones()); campo<-eda_campo_equivalente(datos()$tabla,e$campo)
    if(is.null(campo) || !campo %in% ks || (campo=='id' && !length(e$valores))) campo<-eda_genotipo_campo(datos()$tabla)
    if(!campo %in% ks) campo<-ks[1]
    nuevo<-list(campo=campo,valores=intersect(e$valores,valores(campo)),color=e$color)
    if(!identical(e,nuevo)) estado(nuevo)
  },ignoreNULL=TRUE,priority=20)
  for(contexto in c('resultados','eda')) local({
    ctx<-contexto;pref<-paste0('tg_',ctx)
    output[[paste0(pref,'_ui')]]<-shiny::renderUI({
      ks<-opciones();e<-shiny::isolate(estado());campo<-eda_campo_equivalente(datos()$tabla,e$campo)
      if(is.null(campo)||!campo %in% unname(ks)) campo<-eda_genotipo_campo(datos()$tabla)
      shiny::div(class='control-section testigo-controls',
        shiny::div(class='tool-heading',shiny::icon('diamond'),'Testigos del ensayo'),
        shiny::selectInput(paste0(pref,'_campo'),'Campo que identifica los testigos',ks,selected=campo),
        shiny::selectizeInput(paste0(pref,'_valores'),'Seleccionar testigos',choices=valores(campo),selected=intersect(e$valores,valores(campo)),multiple=TRUE,options=list(placeholder='Selecciona uno o varios')),
        shiny::div(class='testigo-color-row',shiny::tags$label(`for`=paste0(pref,'_color'),'Color de los testigos'),
          shiny::tags$input(id=paste0(pref,'_color'),type='color',value=e$color,class='testigo-color',onchange=paste0("Shiny.setInputValue('",pref,"_color', this.value, {priority:'event'});"))),
        shiny::uiOutput(paste0(pref,'_nota')))
    })
    output[[paste0(pref,'_nota')]]<-shiny::renderUI({
      e<-estado();m<-eda_marcas(datos()$tabla,e)
      shiny::tags$p(class='ib-note',if(!length(e$valores)) 'Sin testigos seleccionados.' else paste(length(e$valores),'testigos seleccionados ·',sum(m$testigo),'parcelas identificadas.'),
        if(length(e$valores)) shiny::tags$span(' Se distinguen con un rombo y el color elegido.'))
    })
    shiny::observeEvent(input[[paste0(pref,'_campo')]],{
      campo<-input[[paste0(pref,'_campo')]]; e<-estado()
      if(!campo %in% unname(opciones()) || identical(campo,e$campo)) return(NULL)
      e$campo<-campo;e$valores<-intersect(e$valores,valores(campo));estado(e)
    },ignoreNULL=TRUE)
    shiny::observeEvent(input[[paste0(pref,'_valores')]],{
      elegido<-input[[paste0(pref,'_valores')]];e<-estado()
      # Solo acepta la selección del control que ya usa el campo actual.
      if(!identical(input[[paste0(pref,'_campo')]],e$campo)) return(NULL)
      e$valores<-intersect(as.character(elegido),valores(e$campo))
      if(!identical(e,estado())) estado(e)
    },ignoreNULL=FALSE,ignoreInit=TRUE)
    shiny::observeEvent(input[[paste0(pref,'_color')]],{
      co<-input[[paste0(pref,'_color')]]
      if(length(co)==1 && grepl('^#[[:xdigit:]]{6}$',co)){e<-estado();e$color<-co;if(!identical(e,estado()))estado(e)}
    },ignoreNULL=TRUE)
    shiny::observeEvent(estado(),{
      e<-estado();shiny::updateSelectInput(session,paste0(pref,'_campo'),selected=e$campo)
      shiny::updateSelectizeInput(session,paste0(pref,'_valores'),choices=valores(e$campo),selected=e$valores,server=FALSE)
      # El color HTML no es una dependencia externa ni un paquete adicional.
      session$sendCustomMessage('cipat-testigo-color',list(id=paste0(pref,'_color'),value=e$color))
    },ignoreNULL=TRUE)
  })
  estado
}

eda_base_integrada <- function(base) {
  if(is.null(base)) stop('Primero prepara una base en Resultados → Exportar base integrada.')
  d<-base$tabla;metricas<-base$diccionario$Columna
  d$id<-as.character(d$Plot);d$Plot<-NULL
  attrs<-setdiff(names(d),c(metricas,'id','Lote','Fecha'))
  names(d)[match(attrs,names(d))]<-paste0('Parcela_',attrs)
  grupos<-names(d)[startsWith(names(d),'Parcela_')]
  grupos<-grupos[vapply(grupos,function(k){v<-as.character(d[[k]]);any(!is.na(v)&nzchar(trimws(v)))},logical(1))]
  list(tabla=d,grupos=stats::setNames(grupos,sub('^Parcela_','',grupos)),aviso=paste(base$coincidencias,'parcelas con ambos sensores;',base$solo_sentera,'solo Sentera;',base$solo_rgb,'solo RGB. La relación X–Y usa únicamente pares con datos.'))
}
eda_unir_sesion <- function(actuales) {
  raw<-lapply(actuales,ib_actual)
  if(is.null(raw$sentera)||is.null(raw$phantom)) stop('Necesitas resultados de Sentera y RGB en esta sesión. Puedes abrir los dos Excel originales desde Resultados, cambiando de módulo, o preparar una base importada para exportación.')
  prep<-function(x,sensor) {
    ids<-intersect(c('id','Plot'),names(x$tabla))
    if(!length(ids))stop('No se encontró el ID de parcela en ',sensor,'.')
    ib_preparar(x,sensor,ids[1],x$lote,x$fecha)
  }
  ib_integrar(prep(raw$sentera,'Sentera'),prep(raw$phantom,'RGB'))
}
