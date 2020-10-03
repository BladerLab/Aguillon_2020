#Mean EONs Tracks : Figure 1C, Figure 2C, Figure 3D & Figure S5B
##Read Data EONs
timewtmengod<- read.csv2("wtmengod_EONSkin.csv", header=TRUE)
View(timewtmengod)
subtabletEON = rbind( timewtmengod[grep( "EON", timewtmengod[, 1] ), ])
View(subtabletEON)
## Read data Control
WTEON_Dis = rbind( subtabletEON[grep( "wt", subtabletEON[, 1] ), ])
View(WTEON_Dis)
## Transform values for A, C & E, and store all values for col 3 in symWT
symWT = list();
for( n in c( "1wt_A1EON", "1wt_A2EON", "2wt_A1EON", "2wt_A2EON", "3wt_A1EON", "3wt_A2EON", "1wt_C1EON", "1wt_C2EON", "2wt_C1EON", "2wt_C2EON", "3wt_C1EON", "3wt_C2EON", "1wt_E1EON", "1wt_E2EON", "2wt_E1EON", "2wt_E2EON", "3wt_E1EON", "3wt_E2EON" ) ){
  symWT[[n]] <- WTEON_Dis [WTEON_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1wt_B1EON", "1wt_B2EON", "2wt_B1EON", "2wt_B2EON", "3wt_B1EON", "3wt_B2EON", "1wt_D1EON", "1wt_D2EON", "2wt_D1EON", "2wt_D2EON", "3wt_D1EON", "3wt_D2EON", "1wt_F1EON", "1wt_F2EON", "2wt_F1EON", "2wt_F2EON", "3wt_F1EON", "3wt_F2EON" ) ){
  symWT[[n]] <- WTEON_Dis [WTEON_Dis$Track.name == n, 3];
}

## Read data Neurog1 mut : ng
ngEON_Dis = rbind( subtabletEON[grep( "ng", subtabletEON[, 1] ), ])
View(ngEON_Dis)

## Transform values for A, C & E, and store all values for col 3 in symNG
symNG = list();
for( n in c( "1ng_A1EON", "2ng_A1EON", "2ng_A2EON", "3ng_A1EON", "3ng_A2EON", 
             "1ng_C1EON", "2ng_C1EON", "2ng_C2EON", "3ng_C1EON", "3ng_C2EON", 
             "1ng_E1EON", "2ng_E1EON", "2ng_E2EON", "2ng_F1EON", "2ng_F2EON" ) ){
  symNG[[n]] <- ngEON_Dis [ngEON_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1ng_A2EON", 
             "1ng_B1EON", "1ng_B2EON", "2ng_B1EON", "2ng_B2EON", "3ng_B1EON", "3ng_B2EON",
             "1ng_C2EON",
             "1ng_D1EON", "1ng_D2EON", "2ng_D1EON", "2ng_D2EON", "3ng_D1EON", "3ng_D2EON",
             "1ng_E2EON", "3ng_E1EON", "3ng_E2EON", 
             "1ng_F1EON", "1ng_F2EON", "3ng_F1EON", "3ng_F2EON" ) ){
  symNG[[n]] <- ngEON_Dis [ngEON_Dis$Track.name == n, 3];
}

#### Read data ody mutant
odEON_Dis = rbind( subtabletEON[grep( "od", subtabletEON[, 1] ), ])
View(odEON_Dis)

## Transform values for A, C & E, and store all values for col 3 in odsym
odsym = list();
for( n in c( "1od_A1EON", "1od_A2EON", "2od_A1EON", "2od_A2EON","2od_B1EON", "3od_A1EON", "3od_A2EON", "1od_C1EON","1od_C2EON", "1od_D1EON",  "2od_C1EON", "2od_C2EON", "3od_C1EON", "3od_C2EON", "1od_E1EON","1od_E2EON",  "2od_E1EON", "2od_E2EON","3od_E1EON", "3od_E2EON" ) ){
  odsym[[n]] <- odEON_Dis [odEON_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1od_B1EON", "1od_B2EON",  "2od_B2EON", "3od_B1EON", "3od_B2EON", "1od_D2EON", "2od_D1EON", "2od_D2EON", "3od_D1EON", "3od_D2EON", "1od_F1EON", "1od_F2EON", "2od_F1EON", "2od_F2EON", "3od_F1EON", "3od_F2EON" ) ){
  odsym[[n]] <- odEON_Dis [odEON_Dis$Track.name == n, 3];
}

#### Read data medusa (me) mutant
meEON_Dis = rbind( subtabletEON[grep( "me", subtabletEON[, 1] ), ])
View(meEON_Dis)

## Transform values for A, C & E, and store all values for col 3 in mesym
mesym = list();
for( n in c(  "1me_B1EON", "1me_B2EON", "2me_B2EON", "2me_B1EON", "3me_B1EON", "3me_B2EON", "2me_D1EON", "2me_D2EON", "3me_D1EON", "3me_D2EON", "1me_E1EON","1me_E2EON","1me_F2EON","2me_F1EON", "2me_F2EON", "3me_F1EON", "3me_F2EON" ) ){
  mesym[[n]] <- meEON_Dis [meEON_Dis$Track.name == n, 3]*-1;
}
for( n in c( "1me_A1EON", "1me_A2EON", "2me_A1EON", "2me_A2EON","3me_A1EON", "3me_A2EON", "1me_C1EON","1me_C2EON", "1me_D1EON", "1me_D2EON", "2me_C1EON", "2me_C2EON", "3me_C1EON", "3me_C2EON", "1me_F1EON",  "2me_E1EON", "2me_E2EON", "3me_E1EON", "3me_E2EON" ) ){
  mesym[[n]] <- meEON_Dis [meEON_Dis$Track.name == n, 3];
}

## Define plot function
addPoints = function( tabToPlot, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 1, col2 = 8 ){
  points( tabToPlot[varToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabToPlot[varToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}
###Ant Mean WT & neurog1 - Fig 1C
plot( 1, type = "n", xlim = c( -10, 60 ), ylim = c( -80, 80 ),lwd = 3, main = "", xlab = "", ylab = "" );
addPoints( tabToPlot = toPlot_AB_WT, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 1, col2 = 8);
addPoints( tabToPlot = toPlot_AB_NG, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 6, col2 = 8);
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

#Mean WT, neurog, ody, me - Fig 2B
col1 = col2 = NULL;
for( n in c( "1wt_A1EON", "1wt_A2EON", "1wt_B1EON", "1wt_B2EON", "2wt_A1EON", "2wt_A2EON", "2wt_B1EON", "2wt_B2EON", "3wt_A1EON", "3wt_A2EON", "3wt_B1EON", "3wt_B2EON" ) ){
  col1 = cbind( col1, symWT[[n]] );
  col2 = cbind( col2, WTEON_Dis[WTEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_AB_WT = cbind( rowMeans( col1 ), rowMeans( col2 ) )
col1 = col2 = NULL;
for( n in c( "1ng_A1EON", "1ng_A2EON", "1ng_B1EON", "1ng_B2EON", "2ng_A1EON", "2ng_A2EON", "2ng_B1EON", "2ng_B2EON", "3ng_A1EON", "3ng_A2EON", "3ng_B1EON", "3ng_B2EON" ) ){
  col1 = cbind( col1, symNG[[n]] );
  col2 = cbind( col2, ngEON_Dis[ngEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_AB_NG = cbind( rowMeans( col1 ), rowMeans( col2 ) )
col1 = col2 = NULL;
for( n in c( "1od_A1EON", "1od_A2EON", "1od_B1EON", "1od_B2EON", "2od_A1EON", "2od_A2EON", "2od_B1EON", "2od_B2EON", "3od_A1EON", "3od_A2EON", "3od_B1EON", "3od_B2EON" ) ){
  col1 = cbind( col1, odsym[[n]] );
  col2 = cbind( col2, odEON_Dis[odEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_AB_od = cbind( rowMeans( col1 ), rowMeans( col2 ) )
col1 = col2 = NULL;
for( n in c( "1me_A1EON", "1me_A2EON", "1me_B1EON", "1me_B2EON", "2me_A1EON", "2me_A2EON", "2me_B1EON", "2me_B2EON", "3me_A1EON", "3me_A2EON", "3me_B1EON", "3me_B2EON" ) ){
  col1 = cbind( col1, mesym[[n]] );
  col2 = cbind( col2, meEON_Dis[meEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_AB_me = cbind( rowMeans( col1 ), rowMeans( col2 ) )

plot( 1, type = "n", xlim = c( -10, 60 ), ylim = c( -80, 80 ),lwd = 3, main = "", xlab = "", ylab = "" );
addPoints( tabToPlot = toPlot_AB_WT, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 1, col2 = 8);
addPoints( tabToPlot = toPlot_AB_NG, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 6, col2 = 8);
addPoints( tabToPlot = toPlot_AB_od, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 4, col2 = 8);
addPoints( tabToPlot = toPlot_AB_me, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 3, col2 = 8);
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

###Ant Mean WT,ody, me - Fig S5B
plot( 1, type = "n", xlim = c( -10, 60 ), ylim = c( -80, 80 ),lwd = 3, main = "", xlab = "", ylab = "" );
addPoints( tabToPlot = toPlot_AB_WT, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 1, col2 = 8);
#addPoints( tabToPlot = toPlot_AB_NG, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 6, col2 = 8);
addPoints( tabToPlot = toPlot_AB_od, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 4, col2 = 8);
addPoints( tabToPlot = toPlot_AB_me, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 3, col2 = 8);
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

###Middel Mean WT,ody, me - Fig S5B
col1 = col2 = NULL;
for( n in c( "1wt_C1EON", "1wt_C2EON", "1wt_D1EON", "1wt_D2EON", "2wt_C1EON", "2wt_C2EON", "2wt_D1EON", "2wt_D2EON", "3wt_C1EON", "3wt_C2EON", "3wt_D1EON", "3wt_D2EON" ) ){
  col1 = cbind( col1, symWT[[n]] );
  col2 = cbind( col2, WTEON_Dis[WTEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_CD_WT = cbind( rowMeans( col1 ), rowMeans( col2 ) );
col1 = col2 = NULL;
for( n in c( "1ng_C1EON", "1ng_C2EON", "1ng_D1EON", "1ng_D2EON", "2ng_C1EON", "2ng_C2EON", "2ng_D1EON", "2ng_D2EON", "3ng_C1EON", "3ng_C2EON", "3ng_D1EON", "3ng_D2EON" ) ){
  col1 = cbind( col1, symNG[[n]] );
  col2 = cbind( col2, ngEON_Dis[ngEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_CD_NG = cbind( rowMeans( col1 ), rowMeans( col2 ) );
col1 = col2 = NULL;
for( n in c( "1od_C1EON", "1od_C2EON", "1od_D1EON", "1od_D2EON", "2od_C1EON", "2od_C2EON", "2od_D1EON", "2od_D2EON", "3od_C1EON", "3od_C2EON", "3od_D1EON", "3od_D2EON" ) ){
  col1 = cbind( col1, odsym[[n]] );
  col2 = cbind( col2, odEON_Dis[odEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_CD_od = cbind( rowMeans( col1 ), rowMeans( col2 ) );

col1 = col2 = NULL;
for( n in c( "1me_C1EON", "1me_C2EON", "1me_D1EON", "1me_D2EON", "2me_C1EON", "2me_C2EON", "2me_D1EON", "2me_D2EON", "3me_C1EON", "3me_C2EON", "3me_D1EON", "3me_D2EON" ) ){
  col1 = cbind( col1, mesym[[n]] );
  col2 = cbind( col2, meEON_Dis[meEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_CD_me = cbind( rowMeans( col1 ), rowMeans( col2 ) );

plot( 1, type = "n", xlim = c( -10, 60 ), ylim = c( -80, 80 ),lwd = 3, main = "", xlab = "", ylab = "" );
addPoints( tabToPlot = toPlot_CD_WT, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 1, col2 = 8);
#addPoints( tabToPlot = toPlot_CD_NG, varToSelect = ngEON_Dis[ngEON_Dis$Track.name == "1ng_A1EON", 2], Thr = 37, col1 = 6, col2 = 8);
addPoints( tabToPlot = toPlot_CD_od, varToSelect =odEON_Dis[odEON_Dis$Track.name == "1od_A1EON", 2], Thr = 37, col1 = 4, col2 = 8);
addPoints( tabToPlot = toPlot_CD_me, varToSelect = meEON_Dis[meEON_Dis$Track.name == "1me_A1EON", 2], Thr = 37, col1 = 3, col2 = 8);
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );

##Posterior Mean WT,ody, me - Fig S5B
col1 = col2 = NULL;
for( n in c( "1wt_E1EON", "1wt_E2EON", "1wt_F1EON", "1wt_F2EON", "2wt_E1EON", "2wt_E2EON", "2wt_F1EON", "2wt_F2EON", "3wt_E1EON", "3wt_E2EON", "3wt_F1EON", "3wt_F2EON" ) ){
  col1 = cbind( col1, symWT[[n]] );
  col2 = cbind( col2, WTEON_Dis[WTEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_EF_WT = cbind( rowMeans( col1 ), rowMeans( col2 ) );

col1 = col2 = NULL;
for( n in c( "1ng_E1EON", "1ng_E2EON", "1ng_F1EON", "1ng_F2EON", "2ng_E1EON", "2ng_E2EON", "2ng_F1EON", "2ng_F2EON", "3ng_E1EON", "3ng_E2EON", "3ng_F1EON", "3ng_F2EON" ) ){
  col1 = cbind( col1, symNG[[n]] );
  col2 = cbind( col2, ngEON_Dis[ngEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_EF_NG = cbind( rowMeans( col1 ), rowMeans( col2 ) )

col1 = col2 = NULL;
for( n in c( "1od_E1EON", "1od_E2EON", "1od_F1EON", "1od_F2EON", "2od_E1EON", "2od_E2EON", "2od_F1EON", "2od_F2EON", "3od_E1EON", "3od_E2EON", "3od_F1EON", "3od_F2EON" ) ){
  col1 = cbind( col1, odsym[[n]] );
  col2 = cbind( col2, odEON_Dis[odEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_EF_od = cbind( rowMeans( col1 ), rowMeans( col2 ) )

col1 = col2 = NULL;
for( n in c( "1me_E1EON", "1me_E2EON", "1me_F1EON", "1me_F2EON", "2me_E1EON", "2me_E2EON", "2me_F1EON", "2me_F2EON", "3me_E1EON", "3me_E2EON", "3me_F1EON", "3me_F2EON" ) ){
  col1 = cbind( col1, mesym[[n]] );
  col2 = cbind( col2, meEON_Dis[meEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_EF_me = cbind( rowMeans( col1 ), rowMeans( col2 ) )

plot( 1, type = "n", xlim = c( -10, 60 ), ylim = c( -80, 80 ), lwd =3, main = "", xlab = "", ylab = "" );
addPoints( tabToPlot = toPlot_EF_WT, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 1, col2 = 8 );
#addPoints( tabToPlot = toPlot_EF_NG, varToSelect = ngEON_Dis[ngEON_Dis$Track.name == "1ng_A1EON", 2], Thr = 37, col1 = 6, col2 = 8 );
addPoints( tabToPlot = toPlot_EF_od, varToSelect = odEON_Dis[odEON_Dis$Track.name == "1od_A1EON", 2], Thr = 37, col1 = 4, col2 = 8);
addPoints( tabToPlot = toPlot_EF_me, varToSelect = meEON_Dis[meEON_Dis$Track.name == "1me_A1EON", 2], Thr = 37, col1 = 3, col2 = 8);
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );


## FIG3D 
###Read data neurog1 mutant + cxcr4b = rescue 
resEON_Dis <-read.csv2("res_EON_Dis.csv", header = TRUE );
View( resEON_Dis );

## Transform values and store all values for col 3 in ressym
ressym = list();
for( n in c( "1res_B1EON", "1res_B3EON" ) ){
  ressym[[n]] <- resEON_Dis [resEON_Dis$Track.name == n, 3] * -1;
}
for( n in c( "1res_A1EON", "1res_A4EON", "1res_A5EON", "1res_B2EON", "2res_A1EON", "2res_A2EON","3res_B1EON", "4res_B1EON", "4res_B2EON", "4res_B3EON" ) ){
  ressym[[n]] <- resEON_Dis [resEON_Dis$Track.name == n, 3];
}

########### Read data cxcr4b surexpression
surEON_Dis <-read.csv2("sur_EON_Dis.csv", header = TRUE );
View( surEON_Dis );

## Transform value and store all values in sursym
sursym = list();
for( n in c( "1sur_B1EON", "1sur_B2EON", "1sur_B3EON", "3sur_A1EON","3sur_A2EON", "3sur_A3EON", "3sur_A4EON" ) ){
  sursym[[n]] <- surEON_Dis [surEON_Dis$Track.name == n, 3] * -1;
}
for( n in c( "1sur_A1EON", "2sur_A1EON", "2sur_A2EON", "2sur_A4EON",  "4sur_B1EON", "4sur_B2EON" ) ){
  sursym[[n]] <- surEON_Dis [surEON_Dis$Track.name == n, 3];
}

## Define plot function
addPoints = function( tabToPlot, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 1, col2 = 8 ){
  points( tabToPlot[varToSelect < Thr, 1:2], col = col1, lwd = 3,  type = 'l' );
  points( tabToPlot[varToSelect > Thr - 1, 1:2], col = col2, lwd = 3,  type = 'l' );
}

## Mean WT, neurog1mut, sur, res
col1 = col2 = NULL;
for( n in c( "1res_A1EON", "1res_A4EON", "1res_A5EON", "1res_B1EON","1res_B2EON","1res_B3EON", "2res_A1EON", "2res_A2EON","3res_B1EON", "4res_B1EON", "4res_B2EON", "4res_B3EON" ) ){
  col1 = cbind( col1, ressym[[n]] );
  col2 = cbind( col2, resEON_Dis[resEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_AB_res = cbind( rowMeans( col1 ), rowMeans( col2 ) );

col1 = col2 = NULL;
for( n in c( "1sur_A1EON", "1sur_B1EON", "1sur_B2EON", "1sur_B3EON","2sur_A1EON", "2sur_A2EON", "2sur_A4EON", "3sur_A1EON", "3sur_A2EON", "3sur_A3EON", "3sur_A4EON", "4sur_B1EON", "4sur_B2EON") ){
  col1 = cbind( col1, sursym[[n]] );
  col2 = cbind( col2, surEON_Dis[surEON_Dis[, "Track.name"] == n, 4] );
}
toPlot_AB_sur = cbind( rowMeans( col1 ), rowMeans( col2 ) );

##########Figure 3D
plot( 1, type = "n", xlim = c( -10, 60 ), ylim = c( -80, 80 ),lwd = 3, main = "", xlab = "", ylab = "" );
addPoints( tabToPlot = toPlot_AB_WT, varToSelect = WTEON_Dis[WTEON_Dis$Track.name == "1wt_A1EON", 2], Thr = 37, col1 = 1, col2 = 8 );
addPoints( tabToPlot = toPlot_AB_NG, varToSelect = ngEON_Dis[ngEON_Dis$Track.name == "1ng_A1EON", 2], Thr = 37, col1 = 6, col2 = 8 );
addPoints( tabToPlot = toPlot_AB_res, varToSelect = resEON_Dis[resEON_Dis$Track.name == "1res_A1EON", 2], Thr = 37, col1 = 5, col2 = 8 );
addPoints( tabToPlot = toPlot_AB_sur, varToSelect = surEON_Dis[surEON_Dis$Track.name == "1sur_A1EON", 2], Thr = 37, col1 = "lightblue", col2 = 8 );
abline( h = 0, col = 1 );
abline( v = 0, col = 1 );
