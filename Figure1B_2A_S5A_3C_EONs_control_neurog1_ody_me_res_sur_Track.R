#Tracks EON_ Figure 1B, Figure 2A, Fig S5A
##Read Data EONs
timewtmengod<- read.csv2("wtmengod_EONSkin.csv", header=TRUE)
View(timewtmengod)
subtabletEON = rbind( timewtmengod[grep( "EON", timewtmengod[, 1] ), ])
View(subtabletEON)
## Read data Control
WTEON_Dis = rbind( subtabletEON[grep( "wt", subtabletEON[, 1] ), ])
View(WTEON_Dis)

## Transform values for A, C & E, and store all values for col 3 in sym
sym = list();
for( n in c( "1wt_A1EON", "1wt_A2EON", "2wt_A1EON", "2wt_A2EON", "3wt_A1EON", "3wt_A2EON", "1wt_C1EON", "1wt_C2EON", "2wt_C1EON", "2wt_C2EON", "3wt_C1EON", "3wt_C2EON", "1wt_E1EON", "1wt_E2EON", "2wt_E1EON", "2wt_E2EON", "3wt_E1EON", "3wt_E2EON" ) ){
	sym[[n]] <- WTEON_Dis [WTEON_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1wt_B1EON", "1wt_B2EON", "2wt_B1EON", "2wt_B2EON", "3wt_B1EON", "3wt_B2EON", "1wt_D1EON", "1wt_D2EON", "2wt_D1EON", "2wt_D2EON", "3wt_D1EON", "3wt_D2EON", "1wt_F1EON", "1wt_F2EON", "2wt_F1EON", "2wt_F2EON", "3wt_F1EON", "3wt_F2EON" ) ){
	sym[[n]] <- WTEON_Dis [WTEON_Dis$Track.name == n, 3];
}

## Define plot function
addPoints = function( tabToPlot, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 1, col2 = 8 ){
	points( tabToPlot[varToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
	points( tabToPlot[varToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot1 : Anterior Plot WT - Fig 1B
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1wt_A1EON", "1wt_A2EON", "1wt_B1EON", "1wt_B2EON", "2wt_A1EON", "2wt_A2EON", "2wt_B1EON", "2wt_B2EON", "3wt_A1EON", "3wt_A2EON", "3wt_B1EON", "3wt_B2EON" ) ){
	addPoints( tabToPlot = cbind( sym[[n]], WTEON_Dis[WTEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot2 : Middel WT- Fig 1B
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1wt_C1EON", "1wt_C2EON", "1wt_D1EON", "1wt_D2EON", "2wt_C1EON", "2wt_C2EON", "2wt_D1EON", "2wt_D2EON", "3wt_C1EON", "3wt_C2EON", "3wt_D1EON", "3wt_D2EON" ) ){
	addPoints( tabToPlot = cbind( sym[[n]], WTEON_Dis[WTEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot3 : Posterior WT- Fig 1B
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1wt_E1EON", "1wt_E2EON", "1wt_F1EON", "1wt_F2EON", "2wt_E1EON", "2wt_E2EON", "2wt_F1EON", "2wt_F2EON", "3wt_E1EON", "3wt_E2EON", "3wt_F1EON", "3wt_F2EON" ) ){
	addPoints( tabToPlot = cbind( sym[[n]], WTEON_Dis[WTEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

#### Read data neurog1mut
ngEON_Dis = rbind( subtabletEON[grep( "ng", subtabletEON[, 1] ), ])
View(ngEON_Dis)

## Transform values for A, C & E, and store all values for col 3 in ngsym
ngsym = list();
for( n in c( "1ng_A1EON", "2ng_A1EON", "2ng_A2EON", "3ng_A1EON", "3ng_A2EON", "1ng_C1EON", "2ng_C1EON", "2ng_C2EON", "3ng_C1EON", "3ng_C2EON", "1ng_E1EON", "2ng_E1EON", "2ng_E2EON","2ng_F1EON", "2ng_F2EON" ) ){
  ngsym[[n]] <- ngEON_Dis [ngEON_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1ng_B1EON", "1ng_A2EON", "1ng_B2EON", "2ng_B1EON", "2ng_B2EON", "3ng_B1EON", "3ng_B2EON", "1ng_C2EON", "1ng_D1EON", "1ng_D2EON", "2ng_D1EON", "2ng_D2EON", "3ng_D1EON", "3ng_D2EON", "1ng_E2EON", "1ng_F1EON", "1ng_F2EON",  "3ng_E1EON", "3ng_E2EON","3ng_F1EON", "3ng_F2EON" ) ){
  ngsym[[n]] <- ngEON_Dis [ngEON_Dis$Track.name == n, 3];
}

## Define plot function
addPoints = function( tabngToPlot, varngToSelect = ngEON_Dis[ngEON_Dis$Track.name == "1ng_A1EON", 2], Thr = 37, col1 = 6, col2 = 8 ){
  points( tabngToPlot[varngToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabngToPlot[varngToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot1 : Anterior Plot ng- Fig 1B
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1ng_A1EON", "1ng_A2EON", "1ng_B1EON", "1ng_B2EON", "2ng_A1EON", "2ng_A2EON", "2ng_B1EON", "2ng_B2EON", "3ng_A1EON", "3ng_A2EON", "3ng_B1EON", "3ng_B2EON" ) ){
  addPoints( tabngToPlot = cbind( ngsym[[n]], ngEON_Dis[ngEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot2 : Middel ng- Fig 1B
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1ng_C1EON", "1ng_C2EON", "1ng_D1EON", "1ng_D2EON", "2ng_C1EON", "2ng_C2EON", "2ng_D1EON", "2ng_D2EON", "3ng_C1EON", "3ng_C2EON", "3ng_D1EON", "3ng_D2EON" ) ){
  addPoints( tabngToPlot = cbind( ngsym[[n]], ngEON_Dis[ngEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot3 : Posterior ng- Fig 1B
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1ng_E1EON", "1ng_E2EON", "1ng_F1EON", "1ng_F2EON", "2ng_E1EON", "2ng_E2EON", "2ng_F1EON", "2ng_F2EON", "3ng_E1EON", "3ng_E2EON", "3ng_F1EON", "3ng_F2EON" ) ){
  addPoints( tabngToPlot = cbind( ngsym[[n]], ngEON_Dis[ngEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

###Figure 2A & Figure S5A
#### Read data ody mutant
odEON_Dis = rbind( subtabletEON[grep( "od", subtabletEON[, 1] ), ])
View(odEON_Dis)

## Transform values and store all values for col 3 in odsym
odsym = list();
for( n in c( "1od_A1EON", "1od_A2EON", "2od_A1EON", "2od_A2EON","2od_B1EON", "3od_A1EON", "3od_A2EON", "1od_C1EON","1od_C2EON", "1od_D1EON",  "2od_C1EON", "2od_C2EON", "3od_C1EON", "3od_C2EON", "1od_E1EON","1od_E2EON",  "2od_E1EON", "2od_E2EON","3od_E1EON", "3od_E2EON" ) ){
  odsym[[n]] <- odEON_Dis [odEON_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1od_B1EON", "1od_B2EON",  "2od_B2EON", "3od_B1EON", "3od_B2EON", "1od_D2EON", "2od_D1EON", "2od_D2EON", "3od_D1EON", "3od_D2EON", "1od_F1EON", "1od_F2EON", "2od_F1EON", "2od_F2EON", "3od_F1EON", "3od_F2EON" ) ){
  odsym[[n]] <- odEON_Dis [odEON_Dis$Track.name == n, 3];
}

## Define plot function
addPoints = function( tabodToPlot, varodToSelect = odEON_Dis[odEON_Dis$Track.name == "1od_A1EON", 2], Thr = 37, col1 = 4, col2 = 8 ){
  points( tabodToPlot[varodToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabodToPlot[varodToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot1 : Anterior Plot od - Figure 2A
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1od_A1EON", "1od_A2EON", "1od_B1EON", "1od_B2EON", "2od_A1EON", "2od_A2EON", "2od_B1EON", "2od_B2EON", "3od_A1EON", "3od_A2EON", "3od_B1EON", "3od_B2EON" ) ){
  addPoints( tabodToPlot = cbind( odsym[[n]], odEON_Dis[odEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );


## Plot2 : Middel od - Fig S5A
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1od_C1EON", "1od_C2EON", "1od_D1EON", "1od_D2EON", "2od_C1EON", "2od_C2EON", "2od_D1EON", "2od_D2EON", "3od_C1EON", "3od_C2EON", "3od_D1EON", "3od_D2EON" ) ){
  addPoints( tabodToPlot = cbind( odsym[[n]], odEON_Dis[odEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );


## Plot3 : Posterior od - Fig S5A
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1od_E1EON", "1od_E2EON", "1od_F1EON", "1od_F2EON", "2od_E1EON", "2od_E2EON", "2od_F1EON", "2od_F2EON", "3od_E1EON", "3od_E2EON", "3od_F1EON", "3od_F2EON" ) ){
  addPoints( tabodToPlot = cbind( odsym[[n]], odEON_Dis[odEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

#### Read data medusa (me) mutant
meEON_Dis = rbind( subtabletEON[grep( "me", subtabletEON[, 1] ), ])
View(meEON_Dis)

## Transform values and store all values for col 3 in mesym
mesym = list();
for( n in c(  "1me_B1EON", "1me_B2EON", "2me_B2EON", "2me_B1EON", "3me_B1EON", "3me_B2EON", "2me_D1EON", "2me_D2EON", "3me_D1EON", "3me_D2EON", "1me_E1EON","1me_E2EON","1me_F2EON","2me_F1EON", "2me_F2EON", "3me_F1EON", "3me_F2EON" ) ){
  mesym[[n]] <- meEON_Dis [meEON_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1me_A1EON", "1me_A2EON", "2me_A1EON", "2me_A2EON","3me_A1EON", "3me_A2EON", "1me_C1EON","1me_C2EON", "1me_D1EON", "1me_D2EON", "2me_C1EON", "2me_C2EON", "3me_C1EON", "3me_C2EON", "1me_F1EON",  "2me_E1EON", "2me_E2EON", "3me_E1EON", "3me_E2EON" ) ){
  mesym[[n]] <- meEON_Dis [meEON_Dis$Track.name == n, 3];
}

## Define plot function
addPoints = function( tabmeToPlot, varmeToSelect = meEON_Dis[meEON_Dis$Track.name == "1me_A1EON", 2], Thr = 37, col1 = 3, col2 = 8 ){
  points( tabmeToPlot[varmeToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabmeToPlot[varmeToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot1 : Anterior Plot me- Figure 2A

plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1me_A1EON", "1me_A2EON", "1me_B1EON", "1me_B2EON", "2me_A1EON", "2me_A2EON", "2me_B1EON", "2me_B2EON", "3me_A1EON", "3me_A2EON", "3me_B1EON", "3me_B2EON" ) ){
  addPoints( tabmeToPlot = cbind( mesym[[n]], meEON_Dis[meEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot2 : Middel me - Fig S5A
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1me_C1EON", "1me_C2EON", "1me_D1EON", "1me_D2EON", "2me_C1EON", "2me_C2EON", "2me_D1EON", "2me_D2EON", "3me_C1EON", "3me_C2EON", "3me_D1EON", "3me_D2EON" ) ){
  addPoints( tabmeToPlot = cbind( mesym[[n]], meEON_Dis[meEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Plot3 : Posterior me - Fig S5A
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1me_E1EON", "1me_E2EON", "1me_F1EON", "1me_F2EON", "2me_E1EON", "2me_E2EON", "2me_F1EON", "2me_F2EON", "3me_E1EON", "3me_E2EON", "3me_F1EON", "3me_F2EON" ) ){
  addPoints( tabmeToPlot = cbind( mesym[[n]], meEON_Dis[meEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

######@### Figure 3C
## Define plot function WT Ant EON
addPoints = function( tabToPlot, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 1, col2 = 8 ){
  points( tabToPlot[varToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabToPlot[varToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot1 : Anterior Plot WT - Fig 1B
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1wt_A1EON", "1wt_A2EON", "1wt_B1EON", "1wt_B2EON", "2wt_A1EON", "2wt_A2EON", "2wt_B1EON", "2wt_B2EON", "3wt_A1EON", "3wt_A2EON", "3wt_B1EON", "3wt_B2EON" ) ){
  addPoints( tabToPlot = cbind( sym[[n]], WTEON_Dis[WTEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

## Define plot function neurog1 Ant EON
addPoints = function( tabngToPlot, varngToSelect = ngEON_Dis[ngEON_Dis$Track.name == "1ng_A1EON", 2], Thr = 37, col1 = 6, col2 = 8 ){
  points( tabngToPlot[varngToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabngToPlot[varngToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot2 : Anterior Plot ng- Fig 3C
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1ng_A1EON", "1ng_A2EON", "1ng_B1EON", "1ng_B2EON", "2ng_A1EON", "2ng_A2EON", "2ng_B1EON", "2ng_B2EON", "3ng_A1EON", "3ng_A2EON", "3ng_B1EON", "3ng_B2EON" ) ){
  addPoints( tabngToPlot = cbind( ngsym[[n]], ngEON_Dis[ngEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

# Read data neurog1 mutant + cxcr4b = rescue 
resEON_Dis <-read.csv2("res_EON_Dis.csv", header=TRUE )
View(resEON_Dis)

## Transform values and store all values for col 3 in ressym
ressym = list();
for( n in c( "1res_B1EON", "1res_B3EON" ) ){
  ressym[[n]] <- resEON_Dis [resEON_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1res_A1EON", "1res_A4EON", "1res_A5EON", "1res_B2EON", "2res_A1EON", "2res_A2EON","3res_B1EON", "4res_B1EON", "4res_B2EON", "4res_B3EON") ){
  ressym[[n]] <- resEON_Dis [resEON_Dis$Track.name == n, 3];
}

## Define plot function rescue ANT EON
addPoints = function( tabresToPlot, varresToSelect = resEON_Dis[resEON_Dis$Track.name == "1res_A1EON", 2], Thr = 37, col1 = 5, col2 = 8 ){
  points( tabresToPlot[varresToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabresToPlot[varresToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot3 : Anterior Plot Res - Figure 3C
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1res_A1EON", "1res_A4EON", "1res_A5EON","1res_B1EON", "1res_B2EON", "1res_B3EON" , "2res_A1EON", "2res_A2EON","3res_B1EON", "4res_B1EON", "4res_B2EON", "4res_B3EON") ){
  addPoints( tabresToPlot = cbind( ressym[[n]], resEON_Dis[resEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

######Read data cxcr4b surexpression
surEON_Dis <-read.csv2("sur_EON_Dis.csv", header=TRUE )
View(surEON_Dis)

## Transform value and store all values in sursym
sursym = list();
for( n in c( "1sur_B1EON", "1sur_B2EON", "1sur_B3EON", "3sur_A1EON","3sur_A2EON", "3sur_A3EON", "3sur_A4EON" ) ){
  sursym[[n]] <- surEON_Dis [surEON_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1sur_A1EON", "2sur_A1EON", "2sur_A2EON", "2sur_A4EON",  "4sur_B1EON", "4sur_B2EON" ) ){
  sursym[[n]] <- surEON_Dis [surEON_Dis$Track.name == n, 3];
}

## Define plot function overexpression plot 4 EON
addPoints = function( tabsurToPlot, varsurToSelect = surEON_Dis[surEON_Dis$Track.name == "1sur_A1EON", 2], Thr = 37, col1 = "lightblue", col2 = 8 ){
  points( tabsurToPlot[varsurToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabsurToPlot[varsurToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Plot4 : Anterior Plot sur - Figure 3C
plot( 1, type = "n", xlim = c( -100, 100 ), ylim = c( -100, 120 ), main = "", xlab = "", ylab = "" );
for( n in c( "1sur_A1EON", "1sur_B1EON", "1sur_B2EON", "1sur_B3EON", "2sur_A1EON", "2sur_A2EON", "2sur_A4EON", "3sur_A1EON", "3sur_A2EON", "3sur_A3EON", "3sur_A4EON",  "4sur_B1EON", "4sur_B2EON" ) ){
  addPoints( tabsurToPlot = cbind( sursym[[n]], surEON_Dis[surEON_Dis$Track.name == n, 4] ) );
}
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );
####
