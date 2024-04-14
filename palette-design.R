edge_pal <- imgpalr::image_pal(
    file = "https://www.theanalyticaledge.com/images/tae.gif",
    n = 5,
    type = "qual",
    plot = TRUE
)

edge_pal

new_edge_pal <- list(
    light = "#C0D1DE",
    dark = "#6A6B6E",
    neutral = ,
    catch = "#00AEEF"
)

library(tinter)
tinter_plot <- function(x) {
    grid <- c(length(x), 1)
    width <- 0.9 / (max(grid) + 1)
    gap <- 1 / (max(grid) + 1)
    centres <- lapply(grid, function(i) {
        gap * ((max(grid) -
                    i) / 2 + seq_len(i))
    })
    centres <- as.matrix(expand.grid(centres))
    oldPars <- graphics::par(mai = c(0, 0, 0, 0), bg = "white")
    on.exit(graphics::par(oldPars))
    devSize <- grDevices::dev.size()
    devRatio <- devSize[2] / devSize[1]
    graphics::plot(NA, NA,
                   xlim = c(-0.1, 1.1), ylim = 0.5 + c(-1, 1) *
                       devRatio * 0.6, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
                   bty = "n", asp = 1
    )
    graphics::rect(centres[, 1] - width / 2, rev(centres[, 2]) - width / 2,
                   centres[, 1] + width / 2, rev(centres[, 2]) + width / 2,
                   col = x, border = "white", lwd = 0.2
    )
}

eye_pals <- tinter("#00AEEF", 9)
tinter_plot(eye_pals)

tinter("#00AEEF", 3) %>% tinter_plot()

library(dplyr)
library(colorspace)
new_edge_pal <- c(
    light = eye_pals[1],
    neutral = eye_pals[3],
    catch = median(eye_pals),
    dark = eye_pals[15]
)

tinter_plot(new_edge_pal)

edge_pals <- c(new_edge_pal,
               desaturate(new_edge_pal))

swatchplot(new_edge_pal,
           border = "transparent")

tae_bw_pal <- desaturate(new_edge_pal)
tae_col_pal <- new_edge_pal

tae_col_pal
names(tae_bw_pal) <- names(tae_col_pal)
tae_bw_pal

swatchplot(desaturate(new_edge_pal),
           border = "transparent")
