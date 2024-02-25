# double boot, indirect only

    Code
      boot.result$t0
    Output
      [1] 0.4721302

---

    Code
      boot.ci(boot.result, index = 1, type = "perc")
    Output
      BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
      Based on 100 bootstrap replicates
      
      CALL : 
      boot.ci(boot.out = boot.result, type = "perc", index = 1)
      
      Intervals : 
      Level     Percentile     
      95%   ( 0.3564,  0.5881 )  
      Calculations and Intervals on Original Scale
      Some percentile intervals may be unstable

# double boot, all

    Code
      boot.result$t0
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.093216534 -0.097030372  0.611850845  0.220992327  0.610417713  0.679294070 
           re.SySm     re.SmXSm     re.SyMSm     re.SyXSm      re.SmSy      re.SySy 
       0.056841871  0.017842843  0.009498816 -0.006990675  0.056841871  0.270142249 
          re.SmXSy     re.SyMSy     re.SyXSy     re.SmSmX     re.SySmX    re.SmXSmX 
       0.011784875 -0.004323056 -0.018316454  0.017842843  0.011784875  0.120091124 
         re.SyMSmX    re.SyXSmX     re.SmSyM     re.SySyM    re.SmXSyM    re.SyMSyM 
       0.098645577 -0.021301243  0.009498816 -0.004323056  0.098645577  0.111982257 
         re.SyXSyM     re.SmSyX     re.SySyX    re.SmXSyX    re.SyMSyX    re.SyXSyX 
       0.005319217 -0.006990675 -0.018316454 -0.021301243  0.005319217  0.032541578 

---

    Code
      boot.ci(boot.result, index = 1, type = "perc")
    Output
      BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
      Based on 100 bootstrap replicates
      
      CALL : 
      boot.ci(boot.out = boot.result, type = "perc", index = 1)
      
      Intervals : 
      Level     Percentile     
      95%   (-0.1065,  0.2785 )  
      Calculations and Intervals on Original Scale
      Some percentile intervals may be unstable

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "indirect", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.3643302 0.5975923 
      
      $est
      [1] 0.4721302
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "a", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5238559 0.7030133 
      
      $est
      [1] 0.6118508
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "b", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.4539183 0.7308635 
      
      $est
      [1] 0.6104177
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "cprime", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.1100087 0.3677252 
      
      $est
      [1] 0.2209923
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "covab", ci.conf = 0.95)
    Output
      $CI
            2.5%      97.5% 
      0.05556644 0.17012157 
      
      $est
      [1] 0.09864558
      

# level 2 boot, indirect only

    Code
      boot.result$t0
    Output
      [1] 0.4721302

---

    Code
      boot.ci(boot.result, index = 1, type = "perc")
    Output
      BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
      Based on 100 bootstrap replicates
      
      CALL : 
      boot.ci(boot.out = boot.result, type = "perc", index = 1)
      
      Intervals : 
      Level     Percentile     
      95%   ( 0.3591,  0.5717 )  
      Calculations and Intervals on Original Scale
      Some percentile intervals may be unstable

# level 2 boot, all

    Code
      boot.result$t0
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.093216534 -0.097030372  0.611850845  0.220992327  0.610417713  0.679294070 
           re.SySm     re.SmXSm     re.SyMSm     re.SyXSm      re.SmSy      re.SySy 
       0.056841871  0.017842843  0.009498816 -0.006990675  0.056841871  0.270142249 
          re.SmXSy     re.SyMSy     re.SyXSy     re.SmSmX     re.SySmX    re.SmXSmX 
       0.011784875 -0.004323056 -0.018316454  0.017842843  0.011784875  0.120091124 
         re.SyMSmX    re.SyXSmX     re.SmSyM     re.SySyM    re.SmXSyM    re.SyMSyM 
       0.098645577 -0.021301243  0.009498816 -0.004323056  0.098645577  0.111982257 
         re.SyXSyM     re.SmSyX     re.SySyX    re.SmXSyX    re.SyMSyX    re.SyXSyX 
       0.005319217 -0.006990675 -0.018316454 -0.021301243  0.005319217  0.032541578 

---

    Code
      boot.ci(boot.result, index = 1, type = "perc")
    Output
      BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
      Based on 100 bootstrap replicates
      
      CALL : 
      boot.ci(boot.out = boot.result, type = "perc", index = 1)
      
      Intervals : 
      Level     Percentile     
      95%   (-0.1227,  0.2334 )  
      Calculations and Intervals on Original Scale
      Some percentile intervals may be unstable

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "indirect", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.3655156 0.5572424 
      
      $est
      [1] 0.4721302
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "a", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5368761 0.7001004 
      
      $est
      [1] 0.6118508
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "b", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5258939 0.6829505 
      
      $est
      [1] 0.6104177
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "cprime", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.1248937 0.3040597 
      
      $est
      [1] 0.2209923
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "covab", ci.conf = 0.95)
    Output
      $CI
            2.5%      97.5% 
      0.04834917 0.13732887 
      
      $est
      [1] 0.09864558
      

# bootresid

    Code
      extract.boot.modmed.mlm(bootresid, type = "indirect")
    Output
      $CI
           2.5%     97.5% 
      0.3866029 0.5801863 
      
      $est
      [1] 0.4721302
      

