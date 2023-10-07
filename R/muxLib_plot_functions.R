###################################################################
## PLOT FUNCTIONS
###################################################################
requireNamespace("graphics", quietly = TRUE)
requireNamespace("grid", quietly = TRUE)
requireNamespace("rgl", quietly = TRUE)
requireNamespace("ggplot2", quietly = TRUE)

#' Multiplot
#'
#' @param ... plots to to be arranged in the multiplot, alternatively provide a list \code{plotlist}
#' @param plotlist list of plots to be arranged
#' @param file a file
#' @param cols number of columns
#' @param layout an igraph layout
#' @return a plot
#' @importFrom grid grid.newpage grid.layout pushViewport viewport
#' @export
multiplot <-
  function(...,
           plotlist = NULL,
           file,
           cols = 1,
           layout = NULL) {

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]],
              vp = viewport(
                layout.pos.row = matchidx$row,
                layout.pos.col = matchidx$col
              ))
      }
    }
  }

#' @describeIn multiplot Multiplot
multiplot.col <-
  function(...,
           plotlist = NULL,
           file,
           cols = 1,
           layout = NULL) {
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]],
              vp = viewport(
                layout.pos.row = matchidx$row,
                layout.pos.col = matchidx$col
              ))
      }
    }
  }
#' @describeIn multiplot Multiplot
multiplot.row <-
  function(...,
           plotlist = NULL,
           file,
           cols = 1,
           layout = NULL) {
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    cols = numPlots
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]],
              vp = viewport(
                layout.pos.row = matchidx$row,
                layout.pos.col = matchidx$col
              ))
      }
    }
  }

#' layoutMultiplex
#'
#' @param g.list list of networks
#' @param layout layout, as for \link[igraph]{layout_}. 
#'  Default "fr" for \link[igraph]{layout_with_fr}; other options are: 
#'  "drl", "auto", "kk", "comp", "dh".
#' @param ggplot.format default FALSE
#' @param box default TRUE
#' @return a layout, i.e. a matrix of coordinates.
#' @export
layoutMultiplex <-
  function(g.list,
           layout = "fr",
           ggplot.format = F,
           box = T) {
    lay <- NULL
    if (layout == "fr") {
      lay <-
        igraph::layout_with_fr(igraph::graph_from_adjacency_matrix(GetAggregateMatrixFromNetworkList(g.list)))
    } else if (layout == "drl") {
      lay <-
        igraph::layout_with_drl(igraph::graph_from_adjacency_matrix(GetAggregateMatrixFromNetworkList(g.list)))
    } else if (layout == "auto") {
      lay <-
        igraph::layout_nicely(igraph::graph_from_adjacency_matrix(GetAggregateMatrixFromNetworkList(g.list)))
    } else if (layout == "kk") {
      lay <-
        igraph::layout_with_kk(igraph::graph_from_adjacency_matrix(GetAggregateMatrixFromNetworkList(g.list)))
    } else if (layout == "comp") {
      lay <-
        igraph::layout_components(igraph::graph_from_adjacency_matrix(GetAggregateMatrixFromNetworkList(g.list)))
    } else if (layout == "dh") {
      lay <-
        igraph::layout_with_dh(igraph::graph_from_adjacency_matrix(GetAggregateMatrixFromNetworkList(g.list)))
    } else {
      stop("Not a valid layout algorithm!")
    }
    
    if (box) {
      lay[, 1] <-
        2 * (lay[, 1] - min(lay[, 1])) / (max(lay[, 1]) - min(lay[, 1])) - 1
      lay[, 2] <-
        2 * (lay[, 2] - min(lay[, 2])) / (max(lay[, 2]) - min(lay[, 2])) - 1
    }
    
    if (ggplot.format) {
      layout.mux <- list()
      for (l in 1:length(g.list)) {
        #fictitious, we need it just to quickly format the data frame
        layout.df <-
          ggraph::create_layout(g.list[[l]], layout = 'circle')
        layout.df$x <- lay[, 1]
        layout.df$y <- lay[, 2]
        layout.mux[[l]] <- layout.df
      }
      return(layout.mux)
    } else {
      return(lay)
    }
  }


#' Plot a multiplex
#'
#' @param g.list list of networks (representing the layers)
#' @param layer.colors list of colors for the each layer
#' @param edge.colors edge colors. If not provided, use the same color of the layer.
#' @param node.colors node colors. If not provided, use the same color of the layer.
#' @param node.size.values default 0.5,
#' @param node.alpha default 1,
#' @param edge.alpha default 1,
#' @param layout default "fr", see \link[igraph]{layout_with_fr} for other options.
#' @param show.legend default TRUE
#' @return a plot
#' @importFrom ggplot2 ggplot aes theme theme_void ggtitle element_text scale_color_manual
#' @export
plot_multiplex <-
  function(g.list,
           layer.colors,
           edge.colors = "auto",
           node.colors = "auto",
           node.size.values = 0.5,
           node.alpha = 1,
           edge.alpha = 1,
           layout = "fr",
           show.legend = TRUE
           ) {
    # Generate the coordinates for layouting our networks.
    mypal <- layer.colors
    Layers <- length(g.list)
    
    lay <- layoutMultiplex(g.list, layout = layout)
    p <- list()
    
    for (l in 1:Layers) {
      if (node.size.values == "auto") {
        igraph::V(g.list[[l]])$size <- sqrt(igraph::strength(g.list[[l]]))
      } else {
        igraph::V(g.list[[l]])$size <- node.size.values
      }
      
      igraph::V(g.list[[l]])$color <- layer.colors[l]
      
      # fictitious, we use it just to build the dataframe required for plotting
      layout <- ggraph::create_layout(g.list[[l]], layout = 'drl')
      layout$x <- lay[, 1]
      layout$y <- lay[, 2]
      
      if (node.colors == "auto") {
        node.col <- layer.colors[l]
      } else {
        node.col <- node.colors
      }
      
      if (edge.colors == "auto") {
        edge.col <- layer.colors[l]
      } else {
        edge.col <- edge.colors
      }
      
      p[[l]] <- ggraph::ggraph(layout) + 
        theme_void() +
        ggraph::geom_edge_link(colour = edge.col, show.legend = FALSE, alpha = edge.alpha) +
        ggraph::geom_node_point(aes(size = size), colour = node.col, alpha = node.alpha) +
        theme(
          legend.position = "bottom",
          plot.title = element_text(
            size = 12,
            hjust = 0.5,
            face = "bold",
            colour = layer.colors[l],
            vjust = -1
          )
        ) +
        ggtitle(paste("Layer", l)) +
        #guides(size=guide_legend(title="MuxPR"), color=F, alpha=F) +
        scale_color_manual(values = c("orange", "grey80"))
      
      if (!show.legend) {
        p[[l]] <- p[[l]] + theme(legend.position = "none")
      }
    }
    
    return(do.call(multiplot.col, args = p))
  }

#' @describeIn plot_multiplex Old name
#' @usage plot.multiplex(g.list, layer.colors, edge.colors = "auto", node.colors = "auto",
#'    node.size.values = 0.5, node.alpha = 1, edge.alpha = 1, layout = "fr", show.legend = T)
#' @export plot.multiplex 
plot.multiplex <- function(g.list,
                           layer.colors,
                           edge.colors = "auto",
                           node.colors = "auto",
                           node.size.values = 0.5,
                           node.alpha = 1,
                           edge.alpha = 1,
                           layout = "fr",
                           show.legend = T
                           ) {
  .Deprecated("plot_multiplex")
  return(
    plot_multiplex(g.list, layer.colors, edge.colors, node.colors, node.size.values, 
                   node.alpha, edge.alpha, layout,show.legend)
    )
}

#' 3D plot of a multiplex
#'
#' @param g.list list of networks (representing the layers)
#' @param layer.colors colors of the layers (mandatory)
#' @param as.undirected default TRUE
#' @param layer.layout default "auto",
#' @param layer.labels default "auto",
#' @param layer.labels.cex default 2,
#' @param edge.colors default "auto",
#' @param edge.normalize default F,
#' @param edge.size.scale default 1,
#' @param node.colors default "auto",
#' @param node.size.values default 0.5,
#' @param node.size.scale default 1,
#' @param node.alpha default 1,
#' @param edge.alpha default 1,
#' @param layer.alpha default "auto",
#' @param layout default "fr", see \link[igraph]{layout_with_fr} for other options.
#' @param show.nodeLabels default F,
#' @param show.aggregate default F,
#' @param aggr.alpha default "auto",
#' @param aggr.color default to hex-color "#dadada",
#' @param node.colors.aggr default to hex-color "#dadada",
#' @param layer.scale default 2,
#' @param layer.shift.x default 0,
#' @param layer.shift.y default 0,
#' @param layer.space default 1.5,
#' @param FOV default 30
#' @return a plot
#' @importFrom rgl rgl.clear bg3d quads3d text3d par3d
#' @export 
plot_multiplex3D <-
  function(g.list,
           layer.colors,
           as.undirected = T,
           layer.layout = "auto",
           layer.labels = "auto",
           layer.labels.cex = 2,
           edge.colors = "auto",
           edge.normalize = F,
           edge.size.scale = 1,
           node.colors = "auto",
           node.size.values = 0.5,
           node.size.scale = 1,
           node.alpha = 1,
           edge.alpha = 1,
           layer.alpha = "auto",
           layout = "fr",
           show.nodeLabels = F,
           show.aggregate = F,
           aggr.alpha = "auto",
           aggr.color = "#dadada",
           node.colors.aggr = "#dadada",
           layer.scale = 2,
           layer.shift.x = 0,
           layer.shift.y = 0,
           layer.space = 1.5,
           FOV = 30) {
    # Generate a 3D visualization of the multiplex network
    
    # Arguments can be either "auto" or NA in most cases
    
    #If node.colors is a matrix Nodes x Layers, the color of each node can be assigned
    #If node.size.scale is a vector of size Layers, each layer will be scaled independently
    #If edge.size.scale is a vector of size Layers, each layer will be scaled independently
    #If show.aggregate is true, then node.colors.aggr could be set as well
    
    
    mypal <- layer.colors
    
    Layers <- length(g.list)
    Nodes <- igraph::vcount(g.list[[1]])
    
    if (!is.matrix(layer.layout) && layer.layout == "auto") {
      lay <-
        layoutMultiplex(g.list,
                        layout = layout,
                        ggplot.format = F,
                        box = T)
    } else {
      lay <- layer.layout
    }
    
    if (layer.alpha == "auto") {
      layer.alpha <- rep(0.5, Layers)
    }
    if (any(is.na(layer.labels) | sapply(layer.labels, function(x) is.null(x)))) {
      layer.labels <- NA
    } else {
      if (layer.labels == "auto" || length(layer.labels) != Layers) {
        layer.labels <- paste("Layer", 1:Layers)
      }
      if (show.aggregate &&
          (!is.na(layer.labels) && !is.null(layer.labels))) {
        layer.labels <- c(layer.labels, "Aggregate")
      }
    }
    
    if (length(node.size.scale) == 1) {
      node.size.scale <- rep(node.size.scale, Layers)
    }
    
    if (length(edge.size.scale) == 1) {
      edge.size.scale <- rep(edge.size.scale, Layers)
    }
    
    LAYER_SCALE <- layer.scale
    LAYER_SHIFT_X <- layer.shift.x
    LAYER_SHIFT_Y <- layer.shift.y
    LAYER_SPACE <- layer.space
    
    PLOT_FOV <- FOV
    d <- 0
    
    rgl.clear()
    bg3d(col = "white")
    
    for (l in 1:Layers) {
      if (as.undirected) {
        g.list[[l]] <- igraph::as.undirected(g.list[[l]])
      }
      
      if (node.size.values == "auto") {
        igraph::V(g.list[[l]])$size <-
          3 * node.size.scale[l] * sqrt(igraph::strength(g.list[[l]]))
      } else {
        igraph::V(g.list[[l]])$size <- node.size.values * node.size.scale[l]
      }
      
      if (!is.matrix(node.colors)) {
        if (node.colors == "auto") {
          node.col <- layer.colors[l]
        } else {
          node.col <- node.colors
        }
        igraph::V(g.list[[l]])$color <- node.col
      } else {
        igraph::V(g.list[[l]])$color <- node.colors[, l]
      }
      
      if (show.nodeLabels) {
        igraph::V(g.list[[l]])$label <- 1:igraph::gorder(g.list[[l]])
      } else {
        igraph::V(g.list[[l]])$label <- NA
      }
      
      if (edge.colors == "auto") {
        edge.col <- layer.colors[l]
      } else {
        edge.col <- edge.colors
      }
      igraph::E(g.list[[l]])$color <- edge.col
      
      if (!is.null(igraph::E(g.list[[l]])$weight)) {
        igraph::E(g.list[[l]])$width <- igraph::E(g.list[[l]])$weight
      } else {
        igraph::E(g.list[[l]])$width <- 1
      }
      
      if (edge.normalize) {
        igraph::E(g.list[[l]])$width <-
          edge.size.scale[l] * log(1 + igraph::E(g.list[[l]])$width) / max(log(1 + igraph::E(g.list[[l]])$width))
      }
      
      if (show.aggregate) {
        d <- -1 + LAYER_SCALE * LAYER_SPACE * l / (Layers + 1)
      } else {
        d <- -1 + LAYER_SCALE * LAYER_SPACE * l / Layers
      }
      #print(d)
      
      layout.layer <- matrix(0, nrow = Nodes, ncol = 3)
      layout.layer[, 1] <- lay[, 1] + (l - 1) * LAYER_SHIFT_X
      layout.layer[, 2] <- lay[, 2] + (l - 1) * LAYER_SHIFT_Y
      layout.layer[, 3] <- d
      
      x <-
        c(-1, -1, -1 + LAYER_SCALE, -1 + LAYER_SCALE) + (l - 1) * LAYER_SHIFT_X
      y <-
        c(-1 + LAYER_SCALE, -1, -1, -1 + LAYER_SCALE) + (l - 1) * LAYER_SHIFT_Y
      z <- c(d, d, d, d)
      quads3d(x,
              y,
              z,
              alpha = layer.alpha[[l]],
              col = layer.colors[[l]],
              add = T)
    
      igraph::rglplot(g.list[[l]], layout = layout.layer,
              rescale = F)
      
      if (!is.na(layer.labels) && !is.null(layer.labels)) {
        text3d(
          -1 + (l - 1) * LAYER_SHIFT_X,
          -1 + (l - 1) * LAYER_SHIFT_Y,
          d + 0.1,
          text = layer.labels[l],
          adj = 0.2,
          color = "black",
          family = "sans",
          cex = layer.labels.cex
        )
      }
    }
    
    if (show.aggregate) {
      g.aggr <- GetAggregateNetworkFromNetworkList(g.list)
      
      if (node.size.values == "auto") {
        igraph::V(g.aggr)$size <- 3 * node.size.scale[l] * sqrt(igraph::strength(g.aggr))
      } else {
        igraph::V(g.aggr)$size <- node.size.values * node.size.scale[l]
      }
      
      igraph::V(g.aggr)$color <- node.colors.aggr
      
      if (show.nodeLabels) {
        igraph::V(g.aggr)$label <- 1:igraph::gorder(g.aggr)
      } else {
        igraph::V(g.aggr)$label <- NA
      }
      
      igraph::E(g.aggr)$color <- aggr.color
      
      if (!is.null(igraph::E(g.aggr)$weight)) {
        igraph::E(g.aggr)$width <- igraph::E(g.aggr)$weight
      } else {
        igraph::E(g.aggr)$width <- 1
      }
      
      l <- Layers + 1
      d <- -1 + LAYER_SCALE * LAYER_SPACE * l / (Layers + 1)
      layout.layer <- matrix(0, nrow = Nodes, ncol = 3)
      layout.layer[, 1] <- lay[, 1] + (l - 1) * LAYER_SHIFT_X
      layout.layer[, 2] <- lay[, 2] + (l - 1) * LAYER_SHIFT_Y
      layout.layer[, 3] <- d
      
      x <-
        c(-1, -1, -1 + LAYER_SCALE, -1 + LAYER_SCALE) + (l - 1) * LAYER_SHIFT_X
      y <-
        c(-1 + LAYER_SCALE, -1, -1, -1 + LAYER_SCALE) + (l - 1) * LAYER_SHIFT_Y
      z <- c(d, d, d, d)
      
      if (aggr.alpha == "auto") {
        quads3d(x,
                y,
                z,
                alpha = 0.5,
                col = aggr.color,
                add = T)
      } else {
        quads3d(x,
                y,
                z,
                alpha = aggr.alpha,
                col = aggr.color,
                add = T)
      }
      
      igraph::rglplot(g.aggr, layout = layout.layer,
              rescale = F)
      
      if (!is.na(layer.labels) && !is.null(layer.labels)) {
        text3d(
          -1 + (l - 1) * LAYER_SHIFT_X,
          -1 + (l - 1) * LAYER_SHIFT_Y,
          d + 0.1,
          text = "Aggregate",
          adj = 0.2,
          color = "black",
          family = "sans",
          cex = layer.labels.cex
        )
      }
      
    }
    
    
    M <- matrix(0, ncol = 4, nrow = 4)
    M[1, ] <- c(0.54, 0, 0.84, 0)
    M[2, ] <- c(0.33, 0.92, -0.22, 0)
    M[3, ] <- c(-0.77, 0.39, 0.5, 0)
    M[4, ] <- c(0, 0, 0, 1)
    
    par3d(FOV = PLOT_FOV, userMatrix = M)
  }

#' @describeIn plot_multiplex3D Old name
#' @usage plot.multiplex3D(g.list, layer.colors, as.undirected, layer.layout, layer.labels, 
#'                         layer.labels.cex, edge.colors, edge.normalize, edge.size.scale, 
#'                         node.colors, node.size.values, node.size.scale, node.alpha, 
#'                         edge.alpha, layer.alpha, layout, show.nodeLabels, show.aggregate, 
#'                         aggr.alpha, aggr.color, node.colors.aggr, layer.scale, 
#'                         layer.shift.x, layer.shift.y, layer.space, FOV)
#' @export plot.multiplex3D
plot.multiplex3D <-
  function(g.list,
           layer.colors,
           as.undirected = T,
           layer.layout = "auto",
           layer.labels = "auto",
           layer.labels.cex = 2,
           edge.colors = "auto",
           edge.normalize = F,
           edge.size.scale = 1,
           node.colors = "auto",
           node.size.values = 0.5,
           node.size.scale = 1,
           node.alpha = 1,
           edge.alpha = 1,
           layer.alpha = "auto",
           layout = "fr",
           show.nodeLabels = F,
           show.aggregate = F,
           aggr.alpha = "auto",
           aggr.color = "#dadada",
           node.colors.aggr = "#dadada",
           layer.scale = 2,
           layer.shift.x = 0,
           layer.shift.y = 0,
           layer.space = 1.5,
           FOV = 30) {
    .Deprecated("plot_multiplex3D")
    return(
      plot_multiplex3D(g.list, layer.colors, as.undirected, layer.layout, layer.labels, 
                       layer.labels.cex, edge.colors, edge.normalize, edge.size.scale, 
                       node.colors, node.size.values, node.size.scale, node.alpha, edge.alpha, 
                       layer.alpha, layout, show.nodeLabels, show.aggregate, aggr.alpha, 
                       aggr.color, node.colors.aggr, layer.scale, layer.shift.x, layer.shift.y, 
                       layer.space, FOV)
      )
  }

#' @title Plot multilayer motifs
#' 
#' Plot a motif from a motifs table returned by \code{\link{GetMultilayerMotifs}}.
#' 
#' @param motifsTable a table as returned from \code{\link{GetMultilayerMotifs}}
#' @param motifID ID of the motif following the classification of \code{\link{GetMultilayerMotifs}}
#' @param layer.colors colors for edges/layers
#' @param edge.colors default "auto",
#' @param node.colors default "auto",
#' @param node.size default 5
#' @return an igraph plotting object
#' @importFrom graphics par
#' @export
plot_multimotif <- function(
                     motifsTable,
                     motifID,
                     layer.colors,
                     edge.colors = "auto",
                     node.colors = "auto",
                     node.size = 5
                    ) {
    
    if (node.colors == "auto") {
      node.col <- "#A0A0A0"
    } else {
      node.col <- node.colors
    }
    
    if (edge.colors == "auto") {
      edge.col <- layer.colors
    } else {
      edge.col <- edge.colors
    }
    
    r <- which(motifsTable$ID == motifID)
    motif_name <- motifsTable[r, ]$Adj.Matrix
    
    cat(paste("Plotting motif ID", motifID, "defined by A =", motif_name, "\n"))
    
    g.motif <-
      igraph::graph.adjacency(t(matrix(
        as.numeric(strsplit(motif_name, "")[[1]]), ncol = sqrt(stringr::str_length(motif_name))
      )))
    igraph::E(g.motif)$color <- 1
    g.motif <-
      igraph::simplify(g.motif, edge.attr.comb = list(color = "sum"))
    g.layout <- igraph::layout.circle(g.motif)
    g.layout[, 1] <-
      0.95 * (g.layout[, 1] - min(g.layout[, 1])) / (max(g.layout[, 1]) - min(g.layout[, 1])) - 0.95 /
      2
    g.layout[, 2] <-
      0.95 * (g.layout[, 2] - min(g.layout[, 2])) / (max(g.layout[, 2]) - min(g.layout[, 2])) - 0.95 /
      2
    
    par(mar = c(0, 0, 0, 0),
        xaxs = 'i',
        yaxs = 'i')
    par(oma = c(0, 0, 0, 0))
    plot(
      x = NULL,
      y = NULL,
      type = "n",
      xlim = c(-1, 1),
      ylim = c(-1, 1)
    )
    
    igraph::plot.igraph(
      g.motif,
      layout = g.layout,
      vertex.label = "",
      vertex.size = node.size,
      vertex.color = node.col,
      vertex.frame.color = NA,
      edge.color = edge.col[igraph::E(g.motif)$color],
      edge.arrow.size = 1,
      edge.arrow.width = 1.5,
      edge.width = 3,
      rescale = F
    )
    
}

#' @describeIn plot_multimotif Old name
#' @usage plot.multimotif(motifsTable, motifID, layer.colors, edge.colors = "auto", 
#'          node.colors = "auto", node.size = 5)
#' @export plot.multimotif
plot.multimotif <- function(
  motifsTable,
  motifID,
  layer.colors,
  edge.colors = "auto",
  node.colors = "auto",
  node.size = 5
  ) {
  .Deprecated("plot_multimotif")
  return(
    plot_multimotif(motifsTable, motifID, layer.colors, edge.colors, node.colors, node.size)
    )
}

#' Plot modules 
#' 
#' Plot modules obtained from 
#' GetMultiplexCommunities_Infomap/GetMultilayerCommunities_Infomap
#'
#'
#' @param communityList list of modules
#' @param module.colors String
#' @param show.aggregate logical, default TRUE
#' @return a ggplot object
#' @importFrom ggplot2 ggplot aes geom_tile labs guides guide_legend theme theme_minimal ggtitle 
#'   scale_fill_viridis_d scale_fill_manual scale_color_manual element_blank
#' @importFrom rgl rgl.clear bg3d quads3d text3d par3d
#' @export 
plot_multimodules <-
  function(communityList,
           module.colors = "auto",
           show.aggregate = TRUE) {
    
    if (length(module.colors) == 1 && module.colors == "auto") {
      pp <-
        ggplot(communityList$membership.multi,
               aes(node, as.factor(layer), fill = as.factor(module))) +
        theme_minimal() + 
        geom_tile() +
        scale_fill_viridis_d(name = "Module") +
        theme(panel.grid = element_blank(), legend.position = "bottom") +
        labs(x = "Node", y = "Layer") +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))
      
      if (show.aggregate) {
        communityList$membership.aggr$layer <- 0
        pp.agg <-
          ggplot(communityList$membership.aggr,
                 aes(node, as.factor(layer), fill = as.factor(module))) +
          theme_minimal() + geom_tile() +
          scale_fill_viridis_d(name = "Module") +
          theme(panel.grid = element_blank(), legend.position = "bottom") +
          labs(x = "Node", y = "Aggregate") +
          guides(fill = guide_legend(nrow = 2, byrow = TRUE))
      }
    } else {
      pp <-
        ggplot(communityList$membership.multi,
               aes(node, as.factor(layer), fill = as.factor(module))) +
        theme_minimal() + 
        geom_tile() +
        scale_fill_manual(name = "Module", values = module.colors) +
        theme(panel.grid = element_blank(), legend.position = "bottom") +
        labs(x = "Node", y = "Layer") +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))
      
      if (show.aggregate) {
        communityList$membership.aggr$layer <- 0
        pp.agg <-
          ggplot(communityList$membership.aggr,
                 aes(node, as.factor(layer), fill = as.factor(module))) +
          theme_minimal() + 
          geom_tile() +
          scale_fill_manual(name = "Module", values = module.colors) +
          theme(panel.grid = element_blank(), legend.position = "bottom") +
          labs(x = "Node", y = "Aggregate") +
          guides(fill = guide_legend(nrow = 2, byrow = TRUE))
      }
    }
    
    if (show.aggregate) {
      return(multiplot(pp, pp.agg, cols = 1))
    } else {
      return(pp)
    }
  }

#' @describeIn plot_multimodules Old name
#' @usage plot.multimodules(communityList, module.colors = "auto", show.aggregate = TRUE)
#' @export plot.multimodules
plot.multimodules <- function(communityList,
                              module.colors = "auto",
                              show.aggregate = TRUE) {
  .Deprecated("plot_multimodules")
  return(plot_multimodules(communityList, module.colors, show.aggregate))
}
  
