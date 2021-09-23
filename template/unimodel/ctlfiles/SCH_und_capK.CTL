*LCOG HBSchool MC Model - Year 2004
*----------------------------------------------------------
*  (note: added Auto Toll Cost Coefficient, 1/97)
*  ms52 youth fare ('95$) = 36
*
*  Change: share costs, long bike param, prem transit 8/04, pickup
*  Change: transit fare coeff, KNR model, recalibrate 4/05 
*  Change: made full matrix for school trips with 0 fare for trips from
*          zones 213, 393, 396-405 
*
*Share Cost Options:
1 Share Auto Oper Cost     ->No
2 Share Parking Cost       ->No
3 Share Toll Cost          ->No
*
*Person Trip Matrix Location:
1  Input Matrix            ->mf79
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
1  Auto Time               ->mf19
2  Auto Distance           ->mf04
3  Auto 2 Person Time      ->mf19
4  Auto 2 Person Distance  ->mf04
5  Auto 3+ Person Time     ->mf19
6  Auto 3+ Person Distance ->mf04
7  Auto Toll Cost          ->mf60
8  WA Local IVTT           ->mf20
9  WA Local walk time      ->mf21
10 WA Local 1st wait       ->mf23
11 WA Local total wait     ->mf22
12 WA Local boardings      ->mf24
13 WA Local fare           ->mf216
14 WA Prem. IVTT           ->mf25
15 WA Prem. walk time      ->mf26
16 WA Prem. 1st wait       ->mf28
17 WA Prem. total wait     ->mf27
18 WA Prem. boardings      ->mf29
19 WA Prem. fare           ->mf216
20 PNR IVTT                ->mf30
21 PNR walk time           ->mf31
22 PNR 1st wait            ->mf33
23 PNR total wait          ->mf32
24 PNR boardings           ->mf34
25 PNR fare                ->mf216
26 PNR drive access time   ->mf40
27 PNR formal indicator    ->ms51
28 KNR IVTT                ->mf35
29 KNR walk time           ->mf36
30 KNR 1st wait            ->mf38
31 KNR total wait          ->mf37
32 KNR boardings           ->mf39
33 KNR fare                ->mf216
34 KNR drive access time   ->mf41
35 Walk Travel Time        ->mf12
36 Bike Travel Time        ->mf17
37 Walk Skim(2)            ->ms51
38 Walk Skim(3)            ->ms51
39 Bike Skim(2)            ->ms51
40 Bike Skim(3)            ->ms51
*
*Zonal Data Matrix Locations:
1  Transit Access Percent  ->mo34
2  Transit Egress Percent  ->md34
3  Residential Density     ->mo30
4  Daily Parking Costs     ->md03
5  Dest.Terminal Time      ->md02
6  Area Type for Zone      ->mo28
*
*Utility Coefficients:
1  In-Vehicle Time         -> -0.00800
2  Wait 1 less than 7 min. -> -0.02000
3  Wait 1 over 7 min.      -> -0.00800
4  Walk Time               -> -0.02000
5  Transfer Time           -> -0.02000
6  Number of Transfers     -> -0.06000
7  Transit Fare (all)      -> -0.008 
8  Drive to Transit Time   -> -0.02000
9  Parking Cost (all)      -> -0.02   
10 Highway Operating Cost  -> -0.008 
11 Auto Toll Cost          -> -0.02
12 HOV Time Savings        ->  0.00560
13 Formal Lot Indicator    ->  0.0
14 Residential Density     ->  0.1
15 Short Walk Travel Time  -> -0.02000
16 Long Walk  Travel Time  -> -0.06000
17 Short Bike Travel Time  -> -0.02000
18 Long Bike Travel Time   -> -0.06000
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
1  Drive Alone      ->   0.0        0.0        0.0        0.0  
2  Auto 2 Person    ->   0.0        0.0        0.0        0.0  
3  Auto 3+ Person   ->   1.666     -0.383      0.70      -0.2470  
4  Walk/Local       ->   0.0        0.0        0.0        0.0  
5  Walk/Premium     ->   0.0        0.0        0.0        0.0
6  Drive/PNR        ->   0.0        0.0        0.0        0.0  
7  Drive/KNR        ->   3.142      2.979      3.021      3.174  
8  Non-Motor/Walk   ->   0.0        0.0        0.0        0.0  
9  Non-Motor/Bike   ->  -1.387     -5.134     -3.464     -1.450  
*
*Choice Level Undifferentiated Constants:
*Choice
1  Drive Alone      ->   0.0
2  Auto 2 Person    ->   0.0
3  Auto 3+ Person   ->   0.0
4  Walk/Local       ->   0.0
5  Walk/Premium     ->   0.164
6  Drive/PNR        ->   0.0
7  Drive/KNR        ->   0.0
8  Non-Motor/Walk   ->   0.0
9  Non-Motor/Bike   ->   0.0
*
*Nest Level Constants:
*                      --------------Auto Ownership------------
*Choice                   0          1          2          3+
1  Auto             ->  -5.236      0.0        0.0        0.0
2  Shared Ride      ->   0.0        1.672      1.568      0.420  
3  Transit          ->  -0.620      0.949     -0.276     -0.405  
4  Walk Access      ->   0.0        0.0        0.0        0.0  
5  Drive Access     ->  -6.136     -5.644     -5.605     -5.264  
6  Non-Motorized    ->   0.0        2.480      1.760     -0.031  
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
*
*Intra-Zonal Percentages:
*Area Type                      1       2       3
1  Drive Alone             ->  0.02    0.02    0.02
2  Auto 2  Person          ->  0.175   0.175   0.175
3  Auto 3+ Person          ->  0.180   0.180   0.180
4  Walk - Local            ->  0.005   0.005   0.005
5  Walk - Premium          ->  0.0     0.0     0.0
6  Drive - PNR             ->  0.0     0.0     0.0
7  Drive - KNR             ->  0.0     0.0     0.0
8  Walk Trips              ->  0.54    0.54    0.054
9  Bicycle Trips           ->  0.08    0.08    0.08
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
1 Purpose                  ->SCH
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
1  select                  ->430 409
*
