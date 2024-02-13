


# modified function: added definition of all crossed area + 0 don't appear on the graph
draw.quint.venn.selma<- function (vector1, vector2, vector3, vector4
                                  , vector5, 
                                  category = rep("", 5), 
                                  lwd = rep(2, 5), 
                                  lty = rep("solid", 5), 
                                  col = rep("black", 5), 
                                  fill = NULL, alpha = rep(0.5, 5), 
                                  label.col = rep("black", 31), 
                                  cex = rep(1, 31), 
                                  fontface = rep("plain", 31), 
                                  fontfamily = rep("serif", 31),
                                  cat.pos = c(0, 287.5, 215, 145, 70), 
                                  cat.dist = rep(0.2, 5), 
                                  cat.col = rep("black",5), 
                                  cat.cex = rep(1, 5), 
                                  cat.fontface = rep("plain",5), 
                                  cat.fontfamily = rep("serif", 5), 
                                  cat.just = rep(list(c(0.5,0.5)), 5), 
                                  shift,
                                  rotation.degree = 0, 
                                  rotation.centre = c(0.5, 0.5), ind = TRUE, 
                                  cex.prop = NULL, print.mode = "raw", 
                                  sigdigs = 3, direct.area = FALSE, area.vector = 0, ...) 
{
  if (length(category) == 1) {
    cat <- rep(category, 5)
  }
  else if (length(category) != 5) {
    flog.error("Unexpected parameter length for 'category'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'category'")
  }
  if (length(lwd) == 1) {
    lwd <- rep(lwd, 5)
  }
  else if (length(lwd) != 5) {
    flog.error("Unexpected parameter length for 'lwd'", name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'lwd'")
  }
  if (length(lty) == 1) {
    lty <- rep(lty, 5)
  }
  else if (length(lty) != 5) {
    flog.error("Unexpected parameter length for 'lty'", name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'lty'")
  }
  if (length(col) == 1) {
    col <- rep(col, 5)
  }
  else if (length(col) != 5) {
    flog.error("Unexpected parameter length for 'col'", name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'col'")
  }
  if (length(label.col) == 1) {
    label.col <- rep(label.col, 31)
  }
  else if (length(label.col) != 31) {
    flog.error("Unexpected parameter length for 'label.col'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'label.col'")
  }
  if (length(cex) == 1) {
    cex <- rep(cex, 31)
  }
  else if (length(cex) != 31) {
    flog.error("Unexpected parameter length for 'cex'", name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cex'")
  }
  if (length(fontface) == 1) {
    fontface <- rep(fontface, 31)
  }
  else if (length(fontface) != 31) {
    flog.error("Unexpected parameter length for 'fontface'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'fontface'")
  }
  if (length(fontfamily) == 1) {
    fontfamily <- rep(fontfamily, 31)
  }
  else if (length(fontfamily) != 31) {
    flog.error("Unexpected parameter length for 'fontfamily'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'fontfamily'")
  }
  if (length(fill) == 1) {
    fill <- rep(fill, 5)
  }
  else if (length(fill) != 5 & length(fill) != 0) {
    flog.error("Unexpected parameter length for 'fill'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'fill'")
  }
  if (length(alpha) == 1) {
    alpha <- rep(alpha, 5)
  }
  else if (length(alpha) != 5 & length(alpha) != 0) {
    flog.error("Unexpected parameter length for 'alpha'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'alpha'")
  }
  if (length(cat.pos) == 1) {
    cat.pos <- rep(cat.pos, 5)
  }
  else if (length(cat.pos) != 5) {
    flog.error("Unexpected parameter length for 'cat.pos'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.pos'")
  }
  if (length(cat.dist) == 1) {
    cat.dist <- rep(cat.dist, 5)
  }
  else if (length(cat.dist) != 5) {
    flog.error("Unexpected parameter length for 'cat.dist'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.dist'")
  }
  if (length(cat.col) == 1) {
    cat.col <- rep(cat.col, 5)
  }
  else if (length(cat.col) != 5) {
    flog.error("Unexpected parameter length for 'cat.col'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.col'")
  }
  if (length(cat.cex) == 1) {
    cat.cex <- rep(cat.cex, 5)
  }
  else if (length(cat.cex) != 5) {
    flog.error("Unexpected parameter length for 'cat.cex'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.cex'")
  }
  if (length(cat.fontface) == 1) {
    cat.fontface <- rep(cat.fontface, 5)
  }
  else if (length(cat.fontface) != 5) {
    flog.error("Unexpected parameter length for 'cat.fontface'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.fontface'")
  }
  if (length(cat.fontfamily) == 1) {
    cat.fontfamily <- rep(cat.fontfamily, 5)
  }
  else if (length(cat.fontfamily) != 5) {
    flog.error("Unexpected parameter length for 'cat.fontfamily'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.fontfamily'")
  }
  if (!(class(cat.just) == "list" & length(cat.just) == 5 & 
        length(cat.just[[1]]) == 2 & length(cat.just[[2]]) == 
        2 & length(cat.just[[3]]) == 2 & length(cat.just[[4]]) == 
        2 & length(cat.just[[5]]) == 2)) {
    flog.error("Unexpected parameter format for 'cat.just'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter format for 'cat.just'")
  }
  cat.pos <- cat.pos + rotation.degree
  if (direct.area) {
    areas <- area.vector
    for (i in 1:31) {
      assign(paste("a", i, sep = ""), area.vector[i])
    }
  }
  else {
    area1<-length(vector1)
    area2<-length(vector2)
    area3<-length(vector3)
    area4<-length(vector4)
    area5<-length(vector5)
    
    n12345 <-length(intersect(intersect(intersect(intersect(vector1,vector2),vector3),vector4),vector5))     
    n1234 <-length(intersect(intersect(intersect(vector1,vector2),vector3),vector4))
    n1235 <- length(intersect(intersect(intersect(vector1,vector2),vector3),vector5))
    n1245 <-length(intersect(intersect(intersect(vector1,vector2),vector4),vector5))
    n1345 <-length(intersect(intersect(intersect(vector1,vector3),vector4),vector5))
    n2345 <- length(intersect(intersect(intersect(vector2,vector3),vector4),vector5))
    
    n123 <-length(intersect(intersect(vector1,vector2),vector3))
    n124 <-length(intersect(intersect(vector1,vector2),vector4))
    n125 <-length(intersect(intersect(vector1,vector2),vector5))
    n134 <-length(intersect(intersect(vector1,vector3),vector4))
    n135 <-length(intersect(intersect(vector1,vector3),vector5))
    n145 <-length(intersect(intersect(vector1,vector4),vector5))
    
    n234 <- length(intersect(intersect(vector2,vector3),vector4))
    n235 <- length(intersect(intersect(vector2,vector3),vector5))
    n245<- length(intersect(intersect(vector2,vector4),vector5))
    n345 <- length(intersect(intersect(vector3,vector4),vector5))
    
    n12<- length(intersect(vector1, vector2))
    n13<- length(intersect(vector1, vector3))
    n14 <- length(intersect(vector1, vector4))
    n15 <- length(intersect(vector1,vector5))
    n23 <- length(intersect(vector2, vector3))
    n24 <- length(intersect(vector2,vector4))
    n25 <- length(intersect(vector2,vector5))
    n34 <- length(intersect(vector3,vector4))
    n35 <- length(intersect(vector3,vector5))
    n45 <- length(intersect(vector4,vector5))
    
    a31 <- n12345
    a30 <- n1234 - a31
    a29 <- n1235 - a31
    a28 <- n1245 - a31
    a27 <- n1345 - a31
    a26 <- n2345 - a31
    a25 <- n245 - a26 - a28 - a31
    a24 <- n234 - a26 - a30 - a31
    a23 <- n134 - a27 - a30 - a31
    a22 <- n123 - a29 - a30 - a31
    a21 <- n235 - a26 - a29 - a31
    a20 <- n125 - a28 - a29 - a31
    a19 <- n124 - a28 - a30 - a31
    a18 <- n145 - a27 - a28 - a31
    a17 <- n135 - a27 - a29 - a31
    a16 <- n345 - a26 - a27 - a31
    a15 <- n45 - a18 - a25 - a16 - a28 - a27 - a26 - a31
    a14 <- n24 - a19 - a24 - a25 - a30 - a28 - a26 - a31
    a13 <- n34 - a16 - a23 - a24 - a26 - a27 - a30 - a31
    a12 <- n13 - a17 - a22 - a23 - a27 - a29 - a30 - a31
    a11 <- n23 - a21 - a22 - a24 - a26 - a29 - a30 - a31
    a10 <- n25 - a20 - a21 - a25 - a26 - a28 - a29 - a31
    a9 <- n12 - a19 - a20 - a22 - a28 - a29 - a30 - a31
    a8 <- n14 - a18 - a19 - a23 - a27 - a28 - a30 - a31
    a7 <- n15 - a17 - a18 - a20 - a27 - a28 - a29 - a31
    a6 <- n35 - a16 - a17 - a21 - a26 - a27 - a29 - a31
    a5 <- area5 - a6 - a7 - a15 - a16 - a17 - a18 - a25 - 
      a26 - a27 - a28 - a31 - a20 - a29 - a21 - a10
    a4 <- area4 - a13 - a14 - a15 - a16 - a23 - a24 - a25 - 
      a26 - a27 - a28 - a31 - a18 - a19 - a8 - a30
    a3 <- area3 - a21 - a11 - a12 - a13 - a29 - a22 - a23 - 
      a24 - a30 - a31 - a26 - a27 - a16 - a6 - a17
    a2 <- area2 - a9 - a10 - a19 - a20 - a21 - a11 - a28 - 
      a29 - a31 - a22 - a30 - a26 - a25 - a24 - a14
    a1 <- area1 - a7 - a8 - a18 - a17 - a19 - a9 - a27 - 
      a28 - a31 - a20 - a30 - a29 - a22 - a23 - a12
    areas <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, 
               a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, 
               a22, a23, a24, a25, a26, a27, a28, a29, a30, a31)
  }
  areas.error <- c("a1 <- area1 - a7 - a8 - a18 - a17 - a19 - a9 - a27 - a28 - a31 - a20 - a30 - a29 - a22 - a23 - a12", 
                   "a2 <- area2 - a9 - a10 - a19 - a20 - a21 - a11 - a28 - a29 - a31 - a22 - a30 - a26 - a25 - a24 - a14", 
                   "a3 <- area3 - a21 - a11 - a12 - a13 - a29 - a22 - a23 - a24 - a30 - a31 - a26 - a27 - a16 - a6 - a17", 
                   "a4 <- area4 - a13 - a14 - a15 - a16 - a23 - a24 - a25 - a26 - a27 - a28 - a31 - a18 - a19 - a8 - a30", 
                   "a5 <- area5 - a6 - a7 - a15 - a16 - a17 - a18 - a25 - a26 - a27 - a28 - a31 - a20 - a29 - a21 - a10", 
                   "a6 <- n35 - a16 - a17 - a21 - a26 - a27 - a29 - a31", 
                   "a7 <- n15 - a17 - a18 - a20 - a27 - a28 - a29 - a31", 
                   "a8 <- n14 - a18 - a19 - a23 - a27 - a28 - a30 - a31", 
                   "a9 <- n12 - a19 - a20 - a22 - a28 - a29 - a30 - a31", 
                   "a10 <- n25 - a20 - a21 - a25 - a26 - a28 - a29 - a31", 
                   "a11 <- n23 - a21 - a22 - a24 - a26 - a29 - a30 - a31", 
                   "a12 <- n13 - a17 - a22 - a23 - a27 - a29 - a30 - a31", 
                   "a13 <- n34 - a16 - a23 - a24 - a26 - a27 - a30 - a31", 
                   "a14 <- n24 - a19 - a24 - a25 - a30 - a28 - a26 - a31", 
                   "a15 <- n45 - a18 - a25 - a16 - a28 - a27 - a26 - a31", 
                   "a16 <- n345 - a26 - a27 - a31", "a17 <- n135 - a27 - a29 - a31", 
                   "a18 <- n145 - a27 - a28 - a31", "a19 <- n124 - a28 - a30 - a31", 
                   "a20 <- n125 - a28 - a29 - a31", "a21 <- n235 - a26 - a29 - a31", 
                   "a22 <- n123 - a29 - a30 - a31", "a23 <- n134 - a27 - a30 - a31", 
                   "a24 <- n234 - a26 - a30 - a31", "a25 <- n245 - a26 - a28 - a31", 
                   "a26 <- n2345 - a31", "a27 <- n1345 - a31", "a28 <- n1245 - a31", 
                   "a29 <- n1235 - a31", "a30 <- n1234 - a31", "a31 <- n12345")
  for (i in 1:length(areas)) {
    if (areas[i] < 0) {
      flog.error(paste("Impossible:", areas.error[i], "produces negative area"), 
                 name = "VennDiagramLogger")
      stop(paste("Impossible:", areas.error[i], "produces negative area"))
    }
  }
  if (length(cex.prop) > 0) {
    if (length(cex.prop) != 1) 
      flog.error("Value passed to cex.prop is not length 1", 
                 name = "VennDiagramLogger")
    stop("Value passed to cex.prop is not length 1")
    func = cex.prop
    if (class(cex.prop) != "function") {
      if (cex.prop == "lin") {
        func = function(x) x
      }
      else if (cex.prop == "log10") {
        func = log10
      }
      else flog.error(paste0("Unknown value passed to cex.prop: ", 
                             cex.prop), name = "VennDiagramLogger")
      stop(paste0("Unknown value passed to cex.prop: ", 
                  cex.prop))
    }
    maxArea = max(areas)
    for (i in 1:length(areas)) {
      cex[i] = cex[i] * func(areas[i])/func(maxArea)
      if (cex[i] <= 0) 
        stop(paste0("Error in rescaling of area labels: the label of area ", 
                    i, " is less than or equal to zero"))
    }
  }
  grob.list <- gList()
  dist <- 0.13
  a <- 0.24
  b <- 0.46
  init.angle <- -20
  ellipse.positions <- matrix(nrow = 5, ncol = 3)
  colnames(ellipse.positions) <- c("x", "y", "rotation")
  ellipse.positions[1, ] <- c(0.5 + dist * sin(init.angle * 
                                                 pi/180), 0.5 + dist * cos(init.angle * pi/180), 0)
  ellipse.positions[2, ] <- c(0.5 - dist * cos((288 + init.angle - 
                                                  270) * pi/180), 0.5 + dist * sin((288 + init.angle - 
                                                                                      270) * pi/180), -110)
  ellipse.positions[3, ] <- c(0.5 - dist * sin((216 + init.angle - 
                                                  180) * pi/180), 0.5 - dist * cos((216 + init.angle - 
                                                                                      180) * pi/180), 145)
  ellipse.positions[4, ] <- c(0.5 + dist * sin((180 - 144 - 
                                                  init.angle) * pi/180), 0.5 - dist * cos((180 - 144 - 
                                                                                             init.angle) * pi/180), 35)
  ellipse.positions[5, ] <- c(0.5 +  dist * cos((init.angle + 
                                                   72 - 90) * pi/180), 0.5 - dist * sin((init.angle + 72 - 
                                                                                           90) * pi/180), -72.5)
  for (i in 1:5) {
    grob.list <- gList(grob.list, VennDiagram::ellipse(x = ellipse.positions[i, 
                                                                             "x"], y = ellipse.positions[i, "y"], a = a, b = b, 
                                                       rotation = ellipse.positions[i, "rotation"], gp = gpar(lty = 0, 
                                                                                                              fill = fill[i], alpha = alpha[i])))
  }
  for (i in 1:5) {
    grob.list <- gList(grob.list, VennDiagram::ellipse(x = ellipse.positions[i, 
                                                                             "x"], y = ellipse.positions[i, "y"], a = a, b = b, 
                                                       rotation = ellipse.positions[i, "rotation"], gp = gpar(lwd = lwd[i], 
                                                                                                              lty = lty[i], col = col[i], fill = "transparent")))
  }
  label.matrix <- matrix(nrow = 31, ncol = 3)
  colnames(label.matrix) <- c("label", "x", "y")
  label.matrix[1, ] <- c(a1, 0.4555 + shift, 0.9322)
  label.matrix[2, ] <- c(a2, 0.08 + shift, 0.6)
  label.matrix[3, ] <- c(a3, 0.3 + shift, 0.1)
  label.matrix[4, ] <- c(a4, 0.79 + shift, 0.17)
  label.matrix[5, ] <- c(a5, 0.9 + shift, 0.68)
  label.matrix[6, ] <- c(a6, 0.74 + shift, 0.695)
  label.matrix[7, ] <- c(a7, 0.63 + shift, 0.805)
  label.matrix[8, ] <- c(a8, 0.4 + shift, 0.795)
  label.matrix[9, ] <- c(a9, 0.255 + shift, 0.715)
  label.matrix[10, ] <- c(a10, 0.193 + shift, 0.48)
  label.matrix[11, ] <- c(a11, 0.225 + shift, 0.333)
  label.matrix[12, ] <- c(a12, 0.42 + shift, 0.205)
  label.matrix[13, ] <- c(a13, 0.572 + shift, 0.18)
  label.matrix[14, ] <- c(a14, 0.753 + shift, 0.32)
  label.matrix[15, ] <- c(a15, 0.823 + shift, 0.47)
  label.matrix[16, ] <- c(a16, 0.747 + shift, 0.582)
  label.matrix[17, ] <- c(a17, 0.662 + shift, 0.75)
  label.matrix[18, ] <- c(a18, 0.488 + shift, 0.761)
  label.matrix[19, ] <- c(a19, 0.323 + shift, 0.737)
  label.matrix[20, ] <- c(a20, 0.253 + shift, 0.573)
  label.matrix[21, ] <- c(a21, 0.225 + shift, 0.395)
  label.matrix[22, ] <- c(a22, 0.355 + shift, 0.29)
  label.matrix[23, ] <- c(a23, 0.515 + shift, 0.205)
  label.matrix[24, ] <- c(a24, 0.655 + shift, 0.29)
  label.matrix[25, ] <- c(a25, 0.783 + shift, 0.42)
  label.matrix[26, ] <- c(a26, 0.72 + shift, 0.445)
  label.matrix[27, ] <- c(a27, 0.605 + shift, 0.701)
  label.matrix[28, ] <- c(a28, 0.342 + shift, 0.668)
  label.matrix[29, ] <- c(a29, 0.294 + shift, 0.41)
  label.matrix[30, ] <- c(a30, 0.522 + shift, 0.273)
  label.matrix[31, ] <- c(a31, 0.5 + shift, 0.5)
  processedLabels <- rep("", length(label.matrix[, "label"]))
  if (print.mode[1] == "percent") {
    processedLabels <- paste(signif(label.matrix[, "label"]/sum(label.matrix[, 
                                                                             "label"]) * 100, digits = sigdigs), "%", sep = "")
    if (isTRUE(print.mode[2] == "raw")) {
      processedLabels <- paste(processedLabels, "\n(", 
                               label.matrix[, "label"], ")", sep = "")
    }
  }
  if (print.mode[1] == "raw") {
    processedLabels <- label.matrix[, "label"]
    if (isTRUE(print.mode[2] == "percent")) {
      processedLabels <- paste(processedLabels, "\n(", 
                               paste(signif(label.matrix[, "label"]/sum(label.matrix[, 
                                                                                     "label"]) * 100, digits = sigdigs), "%)", sep = ""), 
                               sep = "")
    }
  }
  
  
  for (i in 1:nrow(label.matrix)) {
    if (label.matrix[i,1]>0) {
      tmp <- textGrob(label = processedLabels[i], x = label.matrix[i, 
                                                                   "x"], y = label.matrix[i, "y"], gp = gpar(col = label.col[i], 
                                                                                                             cex = cex[i], fontface = fontface[i], fontfamily = fontfamily[i]))
      grob.list <- gList(grob.list, tmp)
    }
  }
  cat.pos.x <- c(0.4555, 0.08, 0.3, 0.79, 0.9)
  cat.pos.y <- c(0.9322, 0.6, 0.1, 0.17, 0.68)
  for (i in 1:5) {
    this.cat.pos <- find.cat.pos(x = cat.pos.x[i], y = cat.pos.y[i], 
                                 pos = cat.pos[i], dist = cat.dist[i])
    grob.list <- gList(grob.list, textGrob(label = category[i], 
                                           x = this.cat.pos$x, y = this.cat.pos$y, just = cat.just[[i]], 
                                           gp = gpar(col = cat.col[i], cex = cat.cex[i], fontface = cat.fontface[i], 
                                                     fontfamily = cat.fontfamily[i])))
  }
  grob.list <- VennDiagram::adjust.venn(VennDiagram::rotate.venn.degrees(grob.list, 
                                                                         rotation.degree, rotation.centre[1], rotation.centre[2]), 
                                        ...)
  if (ind) {
    grid.draw(grob.list)
  }
  return(grob.list)
}










# venn quadr diagram function modified 
draw.quad.venn.selma<-function (vector1, vector2, vector3, vector4, 
                                category = rep("", 4), 
                                lwd = rep(2, 4), 
                                lty = rep("solid", 4), 
                                col = rep("black", 4), 
                                fill = NULL, alpha = rep(0.5, 4), 
                                label.col = rep("black",15), 
                                cex = rep(1, 15), 
                                fontface = rep("plain", 15), 
                                fontfamily = rep("serif", 15),
                                shift,
                                cat.pos = c(-15, 15, 0, 0), 
                                cat.dist = c(0.22, 0.22, 0.11, 0.11),
                                cat.col = rep("black", 4), 
                                cat.cex = rep(1,4), 
                                cat.fontface = rep("plain", 4), 
                                cat.fontfamily = rep("serif", 4), 
                                cat.just = rep(list(c(0.5, 0.5)), 4),
                                rotation.degree = 0, 
                                rotation.centre = c(0.5, 0.5), 
                                ind = TRUE, cex.prop = NULL, 
                                print.mode = "raw", sigdigs = 3, 
                                direct.area = FALSE, 
                                area.vector = 0, ...) 
{
  if (length(category) == 1) {
    cat <- rep(category, 4)
  }
  else if (length(category) != 4) {
    flog.error("Unexpected parameter length for 'category'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'category'")
  }
  if (length(lwd) == 1) {
    lwd <- rep(lwd, 4)
  }
  else if (length(lwd) != 4) {
    flog.error("Unexpected parameter length for 'lwd'", name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'lwd'")
  }
  if (length(lty) == 1) {
    lty <- rep(lty, 4)
  }
  else if (length(lty) != 4) {
    flog.error("Unexpected parameter length for 'lty'", name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'lty'")
  }
  if (length(col) == 1) {
    col <- rep(col, 4)
  }
  else if (length(col) != 4) {
    flog.error("Unexpected parameter length for 'col'", name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'col'")
  }
  if (length(label.col) == 1) {
    label.col <- rep(label.col, 15)
  }
  else if (length(label.col) != 15) {
    flog.error("Unexpected parameter length for 'label.col'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'label.col'")
  }
  if (length(cex) == 1) {
    cex <- rep(cex, 15)
  }
  else if (length(cex) != 15) {
    flog.error("Unexpected parameter length for 'cex'", name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cex'")
  }
  if (length(fontface) == 1) {
    fontface <- rep(fontface, 15)
  }
  else if (length(fontface) != 15) {
    flog.error("Unexpected parameter length for 'fontface'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'fontface'")
  }
  if (length(fontfamily) == 1) {
    fontfamily <- rep(fontfamily, 15)
  }
  else if (length(fontfamily) != 15) {
    flog.error("Unexpected parameter length for 'fontfamily'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'fontfamily'")
  }
  if (length(fill) == 1) {
    fill <- rep(fill, 4)
  }
  else if (length(fill) != 4 & length(fill) != 0) {
    flog.error("Unexpected parameter length for 'fill'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'fill'")
  }
  if (length(alpha) == 1) {
    alpha <- rep(alpha, 4)
  }
  else if (length(alpha) != 4 & length(alpha) != 0) {
    flog.error("Unexpected parameter length for 'alpha'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'alpha'")
  }
  if (length(cat.pos) == 1) {
    cat.pos <- rep(cat.pos, 4)
  }
  else if (length(cat.pos) != 4) {
    flog.error("Unexpected parameter length for 'cat.pos'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.pos'")
  }
  if (length(cat.dist) == 1) {
    cat.dist <- rep(cat.dist, 4)
  }
  else if (length(cat.dist) != 4) {
    flog.error("Unexpected parameter length for 'cat.dist'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.dist'")
  }
  if (length(cat.col) == 1) {
    cat.col <- rep(cat.col, 4)
  }
  else if (length(cat.col) != 4) {
    flog.error("Unexpected parameter length for 'cat.col'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.col'")
  }
  if (length(cat.cex) == 1) {
    cat.cex <- rep(cat.cex, 4)
  }
  else if (length(cat.cex) != 4) {
    flog.error("Unexpected parameter length for 'cat.cex'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.cex'")
  }
  if (length(cat.fontface) == 1) {
    cat.fontface <- rep(cat.fontface, 4)
  }
  else if (length(cat.fontface) != 4) {
    flog.error("Unexpected parameter length for 'cat.fontface'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.fontface'")
  }
  if (length(cat.fontfamily) == 1) {
    cat.fontfamily <- rep(cat.fontfamily, 4)
  }
  else if (length(cat.fontfamily) != 4) {
    flog.error("Unexpected parameter length for 'cat.fontfamily'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for 'cat.fontfamily'")
  }
  if (!(class(cat.just) == "list" & length(cat.just) == 4 & 
        length(cat.just[[1]]) == 2 & length(cat.just[[2]]) == 
        2 & length(cat.just[[3]]) == 2 & length(cat.just[[4]]) == 
        2)) {
    flog.error("Unexpected parameter format for 'cat.just'", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter format for 'cat.just'")
  }
  cat.pos <- cat.pos + rotation.degree
  if (direct.area) {
    areas <- area.vector
    for (i in 1:15) {
      assign(paste("a", i, sep = ""), area.vector[i])
    }
  }
  else {
    area1<-length(vector1)
    area2<-length(vector2)
    area3<-length(vector3)
    area4<-length(vector4)
    n1234 <- length(intersect(intersect(intersect(vector1,vector2),vector3),vector4))
    n123 <-length(intersect(intersect(vector1,vector2),vector3))
    n124 <-length(intersect(intersect(vector1,vector2),vector4))
    n134 <-length(intersect(intersect(vector1,vector3),vector4))
    n234 <-length(intersect(intersect(vector2,vector3),vector4))
    n12 <-length(intersect(vector1,vector2))
    n13 <-length(intersect(vector1,vector3))
    n14 <-length(intersect(vector1,vector4))
    n23 <-length(intersect(vector2,vector3))
    n24 <-length(intersect(vector2,vector4))
    n34 <-length(intersect(vector3,vector4))
    a6 <- n1234
    a12 <- n123 - a6
    a11 <- n124 - a6
    a5 <- n134 - a6
    a7 <- n234 - a6
    a15 <- n12 - a6 - a11 - a12
    a4 <- n13 - a6 - a5 - a12
    a10 <- n14 - a6 - a5 - a11
    a13 <- n23 - a6 - a7 - a12
    a8 <- n24 - a6 - a7 - a11
    a2 <- n34 - a6 - a5 - a7
    a9 <- area1 - a4 - a5 - a6 - a10 - a11 - a12 - a15
    a14 <- area2 - a6 - a7 - a8 - a11 - a12 - a13 - a15
    a1 <- area3 - a2 - a4 - a5 - a6 - a7 - a12 - a13
    a3 <- area4 - a2 - a5 - a6 - a7 - a8 - a10 - a11
    areas <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
  }
  areas.error <- c("a1  <- area3 - a2 - a4 - a5 - a6 - a7 - a12 - a13", 
                   "a2  <- n34 - a6 - a5 - a7", "a3  <- area4 - a2 - a5 - a6 - a7 - a8 - a10 - a11", 
                   "a4  <- n13 - a6 - a5 - a12", "a5  <- n134 - a6", "a6  <- n1234", 
                   "a7  <- n234 - a6", "a8  <- n24 - a6 - a7 - a11", "a9  <- area1 - a4 - a5 - a6 - a10 - a11 - a12 - a15", 
                   "a10 <- n14 - a6 - a5 - a11", "a11 <- n124 - a6", "a12 <- n123 - a6", 
                   "a15 <- n12 - a6 - a11 - a12", "a13 <- n23 - a6 - a7 - a12", 
                   "a14 <- area2 - a6 - a7 - a8 - a11 - a12 - a13 - a15")
  for (i in 1:length(areas)) {
    if (areas[i] < 0) {
      flog.error(paste("Impossible:", areas.error[i], "produces negative area"), 
                 name = "VennDiagramLogger")
      stop(paste("Impossible:", areas.error[i], "produces negative area"))
    }
  }
  if (length(cex.prop) > 0) {
    if (length(cex.prop) != 1) 
      flog.error("Value passed to cex.prop is not length 1", 
                 name = "VennDiagramLogger")
    stop("Value passed to cex.prop is not length 1")
    func = cex.prop
    if (class(cex.prop) != "function") {
      if (cex.prop == "lin") {
        func = function(x) x
      }
      else if (cex.prop == "log10") {
        func = log10
      }
      else flog.error(paste0("Unknown value passed to cex.prop: ", 
                             cex.prop), name = "VennDiagramLogger")
      stop(paste0("Unknown value passed to cex.prop: ", 
                  cex.prop))
    }
    maxArea = max(areas)
    for (i in 1:length(areas)) {
      cex[i] = cex[i] * func(areas[i])/func(maxArea)
      if (cex[i] <= 0) 
        stop(paste0("Error in rescaling of area labels: the label of area ", 
                    i, " is less than or equal to zero"))
    }
  }
  grob.list <- gList()
  ellipse.positions <- matrix(nrow = 4, ncol = 7)
  colnames(ellipse.positions) <- c("x", "y", "a", "b", "rotation", 
                                   "fill.mapping", "line.mapping")
  ellipse.positions[1, ] <- c(0.65, 0.47, 0.35, 0.2, 45, 2, 
                              4)
  ellipse.positions[2, ] <- c(0.35, 0.47, 0.35, 0.2, 135, 1, 
                              1)
  ellipse.positions[3, ] <- c(0.5, 0.57, 0.33, 0.15, 45, 4, 
                              3)
  ellipse.positions[4, ] <- c(0.5, 0.57, 0.35, 0.15, 135, 3, 
                              2)
  for (i in 1:4) {
    grob.list <- gList(grob.list, VennDiagram::ellipse(x = ellipse.positions[i, 
                                                                             "x"], y = ellipse.positions[i, "y"], a = ellipse.positions[i, 
                                                                                                                                        "a"], b = ellipse.positions[i, "b"], rotation = ellipse.positions[i, 
                                                                                                                                                                                                          "rotation"], gp = gpar(lty = 0, fill = fill[ellipse.positions[i, 
                                                                                                                                                                                                                                                                        "fill.mapping"]], alpha = alpha[ellipse.positions[i, 
                                                                                                                                                                                                                                                                                                                          "fill.mapping"]])))
  }
  for (i in 1:4) {
    grob.list <- gList(grob.list, ellipse(x = ellipse.positions[i, "x"], 
                                          y = ellipse.positions[i, "y"], 
                                          a = ellipse.positions[i,"a"], 
                                          b = ellipse.positions[i, "b"], 
                                          rotation = ellipse.positions[i, "rotation"], 
                                          gp = gpar(lwd = lwd[ellipse.positions[i,                                                                                                                                                                                  "line.mapping"]], lty = lty[ellipse.positions[i, 
                                                                                                                                                                                                                                                                                                                  "line.mapping"]], col = col[ellipse.positions[i, 
                                                                                                                                                                                                                                                                                                                                                                "line.mapping"]], fill = "transparent")))
  }
  label.matrix <- matrix(nrow = 15, ncol = 3)
  colnames(label.matrix) <- c("label", "x", "y")
  label.matrix[1, ] <- c(a1, 0.35 + shift, 0.77)
  label.matrix[2, ] <- c(a2, 0.5 + shift, 0.69)
  label.matrix[3, ] <- c(a3, 0.65 + shift, 0.77)
  label.matrix[4, ] <- c(a4, 0.31 + shift, 0.67)
  label.matrix[5, ] <- c(a5, 0.4 + shift, 0.58)
  label.matrix[6, ] <- c(a6, 0.5 + shift, 0.47)
  label.matrix[7, ] <- c(a7, 0.6 + shift, 0.58)
  label.matrix[8, ] <- c(a8, 0.69 + shift, 0.67)
  label.matrix[9, ] <- c(a9, 0.18 + shift, 0.58)
  label.matrix[10, ] <- c(a10, 0.32 + shift, 0.42)
  label.matrix[11, ] <- c(a11, 0.425 + shift, 0.38)
  label.matrix[12, ] <- c(a12, 0.575 + shift, 0.38)
  label.matrix[13, ] <- c(a13, 0.68 + shift, 0.42)
  label.matrix[14, ] <- c(a14, 0.82 + shift, 0.58)
  label.matrix[15, ] <- c(a15, 0.5 + shift, 0.28)
  processedLabels <- rep("", length(label.matrix[, "label"]))
  if (print.mode[1] == "percent") {
    processedLabels <- paste(signif(label.matrix[, "label"]/sum(label.matrix[, 
                                                                             "label"]) * 100, digits = sigdigs), "%", sep = "")
    if (isTRUE(print.mode[2] == "raw")) {
      processedLabels <- paste(processedLabels, "\n(", 
                               label.matrix[, "label"], ")", sep = "")
    }
  }
  if (print.mode[1] == "raw") {
    processedLabels <- label.matrix[, "label"]
    if (isTRUE(print.mode[2] == "percent")) {
      processedLabels <- paste(processedLabels, "\n(", 
                               paste(signif(label.matrix[, "label"]/sum(label.matrix[, 
                                                                                     "label"]) * 100, digits = sigdigs), "%)", sep = ""), 
                               sep = "")
    }
  }
  
  
  for (i in 1:nrow(label.matrix)) {
    if (label.matrix[i,1]>0) {
      grob.list <- gList(grob.list, textGrob(label = processedLabels[i], 
                                             x = label.matrix[i, "x"], y = label.matrix[i, "y"], 
                                             gp = gpar(col = label.col[i], cex = cex[i], fontface = fontface[i], 
                                                       fontfamily = fontfamily[i])))
    }
  }
  cat.pos.x <- c(0.18, 0.82, 0.35, 0.65)
  cat.pos.y <- c(0.58, 0.58, 0.77, 0.77)
  for (i in 1:4) {
    this.cat.pos <- find.cat.pos(x = cat.pos.x[i], y = cat.pos.y[i], 
                                 pos = cat.pos[i], dist = cat.dist[i])
    grob.list <- gList(grob.list, textGrob(label = category[i], 
                                           x = this.cat.pos$x, y = this.cat.pos$y, just = cat.just[[i]], 
                                           gp = gpar(col = cat.col[i], cex = cat.cex[i], fontface = cat.fontface[i], 
                                                     fontfamily = cat.fontfamily[i])))
  }
  grob.list <- VennDiagram::adjust.venn(VennDiagram::rotate.venn.degrees(grob.list, 
                                                                         rotation.degree, rotation.centre[1], rotation.centre[2]), 
                                        ...)
  if (ind) {
    grid.draw(grob.list)
  }
  return(grob.list)
}

