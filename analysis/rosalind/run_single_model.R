model <- '
DATA: FILE = "sdf.dat";
VARIABLE: 
NAMES = pid age female gad02 gad03 gad04 gad05 gad06 gad07 gad08 gad09 gad10 gad11
     gad12 gad13 gad14 gad15 gad16 gad17 gad18 gad19 gad20 gad21 gad22 gad23 gad24
     gad25 gad26 gad27 gad28 gad29 gad30 gad31 gad32 gad33; 
MISSING=.;
USEVARIABLES = gad02-gad33;
ANALYSIS:
!TYPE = RANDOM;
ALGORITHM = INTEGRATION;
INTEGRATION = MONTECARLO;
PROCESSORS = 24;
ESTIMATOR = MLR;
MODEL:
i s q c | gad02@1 gad03@2 gad04@3 gad05@4 gad06@5 gad07@6
		gad08@7 gad09@8 gad10@9 gad11@10 gad12@11 gad13@12
		gad14@13 gad15@14 gad16@15 gad17@16 gad18@17 gad19@18
		gad20@19 gad21@20 gad22@21 gad23@22 gad24@23 gad25@24
		gad26@25 gad27@26 gad28@27 gad29@28 gad30@29 gad31@30
		gad32@31 gad33@32; 
c@0; 
OUTPUT:
TECH1 TECH4;
PLOT:
TYPE IS PLOT1 PLOT2 PLOT3;
SERIES IS gad02-gad32 (*);
'

f <- tempfile()
writeLines(model, f)
system(paste("/bin/mplus", f))
