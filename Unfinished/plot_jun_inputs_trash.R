
png(filename=file.path('C:/Users/ChristopherDory/Pictures/JunLocs.png'),
    width=8, height=10, units="in", res=1000)
plot(st_geometry(ext),
     axes = T)
HRU_jun <- df_juniper$HRU_ID[df_juniper$juniper_here == TRUE]
grid <- st_transform(grid, 4326)
plot(st_geometry(grid[grid$HRU_ID %in% HRU_jun,]), col = 'red', border = 'red', add = T)
dev.off()






inds <- as.numeric(round(nhru_file$covden_sum*10000,0))
inds[inds == 0] <- 1

png(filename=file.path('C:/Users/ChristopherDory/Pictures/Covden_Sum.png'),
    width=8, height=10, units="in", res=1000)
nf <- layout(matrix(c(1,2), ncol = 2),
             widths = c(1,0.2))
plot(st_geometry(ext),
     axes = T,
     main = 'covdensum')

pal <- rev(sequential_hcl(n = 10000, palette = 'greens'))
plot(st_geometry(grid[order(grid$HRU_ID), ]), col = pal[inds],
     border = pal[inds], add = T)
par(mar = c(5.1,0,4.1,0))
plot(c(1:25),
     xaxt = 'n',
     yaxt = 'n',
     xlab = '',
     ylab = '',
     col = 'white')
box(col = 'white')
Custom_Color_Bar(colors = pal,
                 xleft = 0,
                 xright = 10,
                 ybot = 0,
                 ytop = par('usr')[4],
                 labels_TF = T,
                 labels_at = round(seq(from = 0,
                                 to = length(pal),
                                 length.out = length(pal)/1000),0),
                 labels_text = round(seq(from = min(nhru_file$covden_sum),
                                       to = max(nhru_file$covden_sum),
                                       length.out = length(pal)/1000),3))
dev.off()







png(filename=file.path('C:/Users/ChristopherDory/Pictures/Snow_Intcp.png'),
    width=8, height=10, units="in", res=1000)
nf <- layout(matrix(c(1,2), ncol = 2),
             widths = c(1,0.2))
plot(st_geometry(ext),
     axes = T,
     main = 'snowintcp')

pal <- rev(sequential_hcl(n = 1000, palette = 'blues'))
plot(st_geometry(grid[order(grid$HRU_ID), ]), col = pal[round(nhru_file$snow_intcp*10000,0)],
     border = pal[round(nhru_file$snow_intcp*10000,0)], add = T)
par(mar = c(5.1,0,4.1,0))
plot(c(1:25),
     xaxt = 'n',
     yaxt = 'n',
     xlab = '',
     ylab = '',
     col = 'white')
box(col = 'white')
Custom_Color_Bar(colors = pal,
                 xleft = 0,
                 xright = 10,
                 ybot = 0,
                 ytop = par('usr')[4],
                 labels_TF = T,
                 labels_at = round(seq(from = 0,
                                       to = length(pal),
                                       length.out = length(pal)/100),0),
                 labels_text = round(seq(from = min(nhru_file$snow_intcp),
                                         to = max(nhru_file$snow_intcp),
                                         length.out = length(pal)/100),3))
dev.off()








inds <- as.numeric(round(nhru_file$covden_win*10000,0))
inds[inds == 0] <- 1

png(filename=file.path('C:/Users/ChristopherDory/Pictures/Covdenwin.png'),
    width=8, height=10, units="in", res=1000)
nf <- layout(matrix(c(1,2), ncol = 2),
             widths = c(1,0.2))
plot(st_geometry(ext),
     axes = T,
     main = 'covdenwin')

pal <- rev(sequential_hcl(n = 6000, palette = 'purples'))
plot(st_geometry(grid[order(grid$HRU_ID), ]), col = pal[inds],
     border = pal[inds], add = T)
par(mar = c(5.1,0,4.1,0))
plot(c(1:25),
     xaxt = 'n',
     yaxt = 'n',
     xlab = '',
     ylab = '',
     col = 'white')
box(col = 'white')
Custom_Color_Bar(colors = pal,
                 xleft = 0,
                 xright = 10,
                 ybot = 0,
                 ytop = par('usr')[4],
                 labels_TF = T,
                 labels_at = round(seq(from = 0,
                                       to = length(pal),
                                       length.out = length(pal)/1000),0),
                 labels_text = round(seq(from = min(nhru_file$covden_win),
                                         to = max(nhru_file$covden_win),
                                         length.out = length(pal)/1000),3))
dev.off()










inds <- as.numeric(round(nhru_file$srain_intcp*10000,0))
inds[inds == 0] <- 1

png(filename=file.path('C:/Users/ChristopherDory/Pictures/srainintcp.png'),
    width=8, height=10, units="in", res=1000)
nf <- layout(matrix(c(1,2), ncol = 2),
             widths = c(1,0.2))
plot(st_geometry(ext),
     axes = T,
     main = 'srain_intcp')

pal <- rev(sequential_hcl(n = 4000, palette = 'blues'))
plot(st_geometry(grid[order(grid$HRU_ID), ]), col = pal[inds],
     border = pal[inds], add = T)
par(mar = c(5.1,0,4.1,0))
plot(c(1:25),
     xaxt = 'n',
     yaxt = 'n',
     xlab = '',
     ylab = '',
     col = 'white')
box(col = 'white')
Custom_Color_Bar(colors = pal,
                 xleft = 0,
                 xright = 10,
                 ybot = 0,
                 ytop = par('usr')[4],
                 labels_TF = T,
                 labels_at = round(seq(from = 0,
                                       to = length(pal),
                                       length.out = length(pal)/1000),0),
                 labels_text = round(seq(from = min(nhru_file$srain_intcp),
                                         to = max(nhru_file$srain_intcp),
                                         length.out = length(pal)/1000),3))
dev.off()









inds <- as.numeric(round(nhru_file$wrain_intcp*10000,0))
inds[inds == 0] <- 1

png(filename=file.path('C:/Users/ChristopherDory/Pictures/wrainintcp.png'),
    width=8, height=10, units="in", res=1000)
nf <- layout(matrix(c(1,2), ncol = 2),
             widths = c(1,0.2))
plot(st_geometry(ext),
     axes = T,
     main = 'wrain_intcp')

pal <- rev(sequential_hcl(n = 1000, palette = 'blues'))
plot(st_geometry(grid[order(grid$HRU_ID), ]), col = pal[inds],
     border = pal[inds], add = T)
par(mar = c(5.1,0,4.1,0))
plot(c(1:25),
     xaxt = 'n',
     yaxt = 'n',
     xlab = '',
     ylab = '',
     col = 'white')
box(col = 'white')
Custom_Color_Bar(colors = pal,
                 xleft = 0,
                 xright = 10,
                 ybot = 0,
                 ytop = par('usr')[4],
                 labels_TF = T,
                 labels_at = round(seq(from = 0,
                                       to = length(pal),
                                       length.out = length(pal)/100),0),
                 labels_text = round(seq(from = min(nhru_file$wrain_intcp),
                                         to = max(nhru_file$wrain_intcp),
                                         length.out = length(pal)/100),3))
dev.off()







inds <- as.numeric(round(tbl_snow_params$rad_trncf*10000,0))
inds[inds == 0] <- 1

png(filename=file.path('C:/Users/ChristopherDory/Pictures/radtrncf.png'),
    width=8, height=10, units="in", res=1000)
nf <- layout(matrix(c(1,2), ncol = 2),
             widths = c(1,0.2))
plot(st_geometry(ext),
     axes = T,
     main = 'radtrncf')

pal <- rev(sequential_hcl(n = 6000, palette = 'blues'))
plot(st_geometry(grid[order(grid$HRU_ID), ]), col = pal[inds],
     border = pal[inds], add = T)
par(mar = c(5.1,0,4.1,0))
plot(c(1:25),
     xaxt = 'n',
     yaxt = 'n',
     xlab = '',
     ylab = '',
     col = 'white')
box(col = 'white')
Custom_Color_Bar(colors = pal,
                 xleft = 0,
                 xright = 10,
                 ybot = 0,
                 ytop = par('usr')[4],
                 labels_TF = T,
                 labels_at = round(seq(from = 0,
                                       to = length(pal),
                                       length.out = length(pal)/1000),0),
                 labels_text = round(seq(from = min(tbl_snow_params$rad_trncf),
                                         to = max(tbl_snow_params$rad_trncf),
                                         length.out = length(pal)/1000),3))
dev.off()










inds <- round(tbl_crop_coef$Cover...14,0)
inds[inds == 0] <- 1

png(filename=file.path('C:/Users/ChristopherDory/Pictures/covtype.png'),
    width=8, height=10, units="in", res=1000)
nf <- layout(matrix(c(1,2), ncol = 2),
             widths = c(1,0.2))
plot(st_geometry(ext),
     axes = T,
     main = 'covtype')

pal <- c('bisque2','springgreen3','darkolivegreen','darkgreen','forestgreen')
plot(st_geometry(grid[order(grid$HRU_ID), ]), col = pal[inds],
     border = pal[inds], add = T)
par(mar = c(5.1,0,4.1,0))
plot(c(1:25))
box(col = 'white',
    xaxt = 'n',
    yaxt = 'n',
    xlab = '',
    ylab = '')
Custom_Color_Bar(colors = pal,
                 xleft = 0,
                 xright = 10,
                 ybot = 0,
                 ytop = par('usr')[4],
                 labels_TF = T,
                 labels_at = c(1:5),
                 labels_text = c('Bare','Grass','Shrub','Deciduous','Conifer'),
                 middle = TRUE)
dev.off()
