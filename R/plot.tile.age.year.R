plot.tile.age.year <- function(mod, type="selAA", do.tex = FALSE, do.png = FALSE, fontfam="", od){
  dat <- mod$env$data
  rep <- mod$rep
  years <- mod$years
  n_years <- length(years)
  n_ages <- dat$n_ages
  ages <- 1:n_ages
  ages.lab <- 1:n_ages
  if(!is.null(mod$ages.lab)) ages.lab <- mod$ages.lab

  # selAA for all blocks using facet_wrap
  if(type=="selAA"){ 
    n_selblocks <- length(rep$selAA)
    sel_mod <- c("age-specific","logistic","double-logistic","decreasing-logistic")[dat$selblock_models]
    sel_re <- c("no","IID","AR1","AR1_y","2D AR1")[dat$selblock_models_re]
    df.selAA <- data.frame(matrix(NA, nrow=0, ncol=n_ages+2))
    colnames(df.selAA) <- c(paste0("Age_",1:n_ages),"Year","Block")
    block.names <- paste0("Block ",1:n_selblocks,": ", sel_mod,"\n(",sel_re," random effects)")
    block.fleets.indices <- lapply(1:n_selblocks, function(x){
      y <- dat$selblock_pointer_fleets
      z <- matrix(as.integer(y == x), NROW(y), NCOL(y))
      fleet_ind <- apply(z,2,any)
      out <- mod$input$fleet_names[which(fleet_ind)]
      y <- dat$selblock_pointer_indices
      z <- matrix(as.integer(y == x), NROW(y), NCOL(y))
      index_ind <- apply(z,2,any)
      out <- c(out, mod$input$index_names[which(index_ind)])
    })
    include.selblock <- sapply(block.fleets.indices, length) > 0
    for(i in 1:n_selblocks) if(include.selblock[i]){
      block.names[i] <- paste0(block.names[i], "\n", paste(block.fleets.indices[[i]], collapse = ", "))
    }
    for(i in 1:n_selblocks) if(include.selblock[i]){
      years_ind <- apply(dat$selblock_pointer_fleets, 1, \(x) sum(x == i)) + 
        apply(dat$selblock_pointer_indices, 1, \(x) sum(x == i))
      tmp <- rep$selAA[[i]]
      tmp[which(years_ind == 0),] <- NA
      tmp <- as.data.frame(tmp)
      colnames(tmp) <- c(paste0("Age_",1:n_ages))
      tmp$Year = years
      tmp$Block <- block.names[i]
      df.selAA <- rbind(df.selAA, tmp)
    }
    df.plot <- df.selAA |> tidyr::pivot_longer(-c(Year,Block),
              names_to = "Age", 
              names_prefix = "Age_",
              names_ptypes = list(Age = character()),
              values_to = "Selectivity")
    df.plot$Age <- as.factor(as.integer(df.plot$Age))
    levels(df.plot$Age) <- ages.lab
    df.plot$Block <- factor(as.character(df.plot$Block), levels=block.names[include.selblock])
    fn <- "SelAA_tile"
#     if(do.tex) cairo_pdf(file.path(od, paste0(fn, ".pdf")), family = fontfam, height = 10, width = 10)
#     if(do.png) png(filename = file.path(od, paste0(fn, ".png")), width = 10*144, height = 10*144, res = 144, pointsize = 12, family = fontfam)
#       print(ggplot2::ggplot(df.plot, ggplot2::aes(x=Year, y=Age, fill=Selectivity)) + 
#         ggplot2::geom_tile() +
#         ggplot2::scale_x_continuous(expand=c(0,0)) +
#         ggplot2::scale_y_discrete(expand=c(0,0)) + #, breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +        
# #        ggplot2::scale_y_continuous(expand=c(0,0), breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +        
#         ggplot2::theme_bw() + 
#         ggplot2::facet_wrap(~Block, dir="v") +
#         viridis::scale_fill_viridis())
#     if(do.tex | do.png) dev.off()
  }
  return(df.plot)
}  
