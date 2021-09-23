*LCOG HBShop MC MODEL - Year 2004
*----------------------------------------------------------
*  (note: added Auto Toll Cost Coefficient, 1/97)
*  (modify intras 8/00)
*  Change: share costs, long bike param, prem transit 9/04
*  Change: transit fare coeff, KNR model, recalibrate 4/05
*
*Share Cost Options:
1 Share Auto Oper Costs    ->NO
2 Share Parking Cost       ->NO
3 Share Toll Cost          ->NO
*
*Person Trip Matrix Location:
1  Input Matrix            ->mf75
*
*Number of Major Markets:
*(This should be set to 4 for HBW,HBNW and 1 for the NHB purpose)
1  Major Markets           -> 4
*
*Auto Ownership Percentage Matrix Locations:
*(This should be coordinated with the number of Major Markets and
*should be omitted if Markets = 1)
1  Auto Ownership 0        ->mo10
2  Auto Ownership 1        ->mo11
3  Auto Ownership 2        ->mo12
4  Auto Ownership 3+       ->mo13
*
*Matrix Locations for Utilities and/or Trips:
1  Drive Alone             ->mf81
2  Auto 2  Person          ->mf82
3  Auto 3+ Person          ->mf83
4  Walk - Local            ->mf84
5  Walk - Premium          ->mf85
6  PNR Trips               ->mf86
7  KNR Trips               ->mf87
8  Walk Trips              ->mf88
9  Bicycle Trips           ->mf89
*
*Network Skim Matrix Locations:
1  Auto Time               ->mf102
2  Auto Distance           ->mf04
3  Auto 2 Person Time      ->mf102
4  Auto 2 Person Distance  ->mf04
5  Auto 3+ Person Time     ->mf102
6  Auto 3+ Person Distance ->mf04
7  Auto Toll Cost          ->mf60
8  WA Local IVTT           ->mf107
9  WA Local walk time      ->mf108
10 WA Local 1st wait       ->mf109
11 WA Local total wait     ->mf190
12 WA Local boardings      ->mf218
13 WA Local fare           ->mf10
14 WA Prem. IVTT           ->mf228
15 WA Prem. walk time      ->mf229
16 WA Prem. 1st wait       ->mf230
17 WA Prem. total wait     ->mf231
18 WA Prem. boardings      ->mf232
19 WA Prem. fare           ->mf10
20 PNR IVTT                ->mf233
21 PNR walk time           ->mf234
22 PNR 1st wait            ->mf235
23 PNR total wait          ->mf236
24 PNR boardings           ->mf237
25 PNR fare                ->mf10
26 PNR drive access time   ->mf238
27 PNR formal indicator    ->ms51
28 KNR IVTT                ->mf242
29 KNR walk time           ->mf243
30 KNR 1st wait            ->mf246
31 KNR total wait          ->mf247
32 KNR boardings           ->mf248
33 KNR fare                ->mf10
34 KNR drive access time   ->mf249
35 Walk Travel Time        ->mf250
36 Bike Travel Time        ->mf258
37 Walk Skim(2)            ->ms51
38 Walk Skim(3)            ->ms51
39 Bike Skim(2)            ->ms51
40 Bike Skim(3)            ->ms51
*
*Zonal Data Matrix Locations:
1  Transit Access Percent  ->mo34
2  Transit Egress Percent  ->md34
3  Residential Density     ->mo30
4  Daily Parking Costs     ->md04
5  Dest.Terminal Time      ->md02
6  Area Type for Zone      ->mo28
*
*Utility Coefficients:
1  In-Vehicle Time         -> -0.01500
2  Wait 1 less than X min. -> -0.04000
3  Wait 1 over X min.      -> -0.01600
4  Walk Time               -> -0.04000
5  Transfer Time           -> -0.04000
6  Number of Transfers     -> -0.36000
7  Transit Fare (all)      -> -0.01600 
8  Drive to Transit Time   -> -0.04000
9  Parking Cost (all)      -> -0.04
10 Highway Operating Cost  -> -0.01600
11 Auto Toll Cost          -> -0.04
12 HOV Time Savings        ->  0.0    
13 Formal Lot Indicator    ->  0.0
14 Residential Density     ->  0.20000
15 Short Walk Travel Time  -> -0.04000
16 Long Walk  Travel Time  -> -0.12000
17 Short Bike Travel Time  -> -0.04000
18 Long Bike Travel Time   -> -0.12000
19 Walk Coeff(2)           ->  0.0
20 Walk Coeff(3)           ->  0.0
21 Bike Coeff(2)           ->  0.0
22 Bike Coeff(3)           ->  0.0
*
*Logsum Coefficients:
1  Down 1 Level            -> 0.75
2  Down 2 Levels           -> 0.65
*
*Choice Level Constants:
*                      --------------Auto Ownership------------
*Choice                   0          1          2          3+
1  Drive Alone      ->  -6.345      0.0        0.0        0.0
2  Auto 2 Person    ->   0.0        0.0        0.0        0.0  
3  Auto 3+ Person   ->   0.169     -0.871     -0.536     -0.497  
4  Walk/Local       ->   0.0        0.0        0.0        0.0  
5  Walk/Premium     ->   0.0        0.0        0.0        0.0                              
6  Drive/PNR        ->   0.0        0.0        0.0        0.0                                   
7  Drive/KNR        ->   3.060      0.517      0.0        3.346  
8  Non-Motor/Walk   ->   0.0        0.0        0.0        0.0                               
9  Non-Motor/Bike   ->  -4.783     -4.468     -3.936     -4.833  
*
*Choice Level Undifferentiated Constants:
*Choice
1  Drive Alone      ->   0.0
2  Auto 2 Person    ->   0.0
3  Auto 3+ Person   ->   0.0
4  Walk/Local       ->   0.0
5  Walk/Premium     ->   0.308
6  Drive/PNR        ->   0.0
7  Drive/KNR        ->   0.0
8  Non-Motor/Walk   ->   0.0
9  Non-Motor/Bike   ->   0.0
*                                       
*Nest Level Constants:                                  
*                      --------------Auto Ownership------------                    
*Choice                   0          1          2          3+
1  Auto             ->  -4.027      0.0        0.0        0.0
2  Shared Ride      ->   0.0       -0.223     -0.174     -0.107  
3  Transit          ->  -2.138     -2.378     -4.559     -3.066  
4  Walk Access      ->   0.0        0.0        0.0        0.0
5  Drive Access     ->  -7.256     -3.775      0.000     -7.812  
6  Non-Motorized    ->   0.0       -0.789     -1.437     -2.779  
*
*User Variables:
1  Auto operating cost     -> 12.5
2  First wait break point  ->  7.0
3  Residential dummy value ->  7.5
4  3+ Person auto occ.     ->  3.15
5  2  Person pickup time   ->  1.0
6  3+ Person pickup time   ->  2.5
7  Drive speed (mpm)       ->  0.25
8  Long walk break (min)   -> 20.0
9  Long bike break (min)   -> 20.0
10 HOV Time Savings (min)  ->  5.0
*
*Intra-Zonal Percentages:
*Area Type                      1     2     3
1  Drive Alone             ->  0.29  0.17  0.24
2  Auto 2  Person          ->  0.27  0.07  0.22
3  Auto 3+ Person          ->  0.17  0.01  0.12
4  Walk - Local            ->  0.0   0.0   0.0
5  Walk - Premium          ->  0.0   0.0   0.0
6  Drive - PNR             ->  0.0   0.0   0.0
7  Drive - KNR             ->  0.0   0.0   0.0
8  Walk Trips              ->  0.24  0.70  0.37
9  Bicycle Trips           ->  0.03  0.05  0.05
*
*Run Mode (1,2, or 3):
*Mode 1 - calcululate utilities only and save them in emme2ban
*Mode 2 - read utilites from emme2ban and calculate mode shares
*Mode 3 - calculate utilities and mode shares in one pass
*
1  Run Mode                ->3
*
*Save Matrices Created? (YES or NO):
1  Save Matrices           ->YES 
*
* Matrix Destination? (EMME/2 or BINARY):
1  Matrix Destination      ->EMME/2
*
* User Benefit file headers (no more than 6 chars)
1 Purpose                  ->SHP
2 Period                   ->ALL
*
*Auto Ownership Class Trip Matrices
*Specify an auto ownership class to output trip matrices for (1,2,3,4)
1  Auto Own Class          ->1
*
*Trip Matrix Locations for Auto Ownership Class
1  Drive Alone             ->mf121
2  Auto 2  Person          ->mf122
3  Auto 3+ Person          ->mf123
4  Walk - Local            ->mf124
5  Walk - Premium          ->mf125
6  PNR Trips               ->mf126
7  KNR Trips               ->mf127
8  Walk Trips              ->mf128
9  Bicycle Trips           ->mf129	   
*
*Output Selected Zone Pairs? (YES or NO):
*(Choose up to 25 zone pairs to show trace calculations)
select zones               ->NO
1  select                  ->430 250
*
