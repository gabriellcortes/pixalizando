
pixel <- function(imagem) {
  qsplit <- function(im){
    imsplit(im,"x",2) %>% map(~ imsplit(.,"y",2)) %>%
      flatten
  }
  qunsplit <- function(l){
    list(l[1:2],l[3:4]) %>% map(~ imappend(.,"y")) %>%
      imappend("x")
  }
  imsd <- function(im){
    imsplit(im,"c") %>% map_dbl(sd) %>% max
  }

  refine <- function(l){
    if (is.cimg(l)){
      qs <- qsplit(l)
      if (any(dim(l)[1:2] <= 4)){
        qs$sds <- rep(0,4) }else{
          qs$sds <- map_dbl(qs,imsd)}
      qs}else{
        indm <- which.max(l$sds)
        l[[indm]] <- refine(l[[indm]])
        l$sds[indm] <- max(l[[indm]]$sds)
        l
      }
  }


  rebuild <- function(l,borders=FALSE){
    map(l[-5],~ if (is.cimg(.)) meanim(.,borders=borders)
        else rebuild(.,borders=borders)) %>% qunsplit}
  # Produz uma imagem que Ã© uma mÃ©dia das imagens
  meanim <- function(im,borders=FALSE){
    im <- imsplit(im,"c") %>% map(~ 0*. + mean(.)) %>%  imappend("c")
    if (borders){
      im[px.borders(im)] <- 0
    }
    im
  }

  iter.refine <- function(im,nIter){
    for (i in seq_len(nIter)){
      im <- refine(im) };
    im
  }
  resposta<-iter.refine(imagem,1000) %>% rebuild(borders=F) %>% plot
  return(resposta)

}
