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
      95%   ( 0.3234,  0.5907 )  
      Calculations and Intervals on Original Scale
      Some percentile intervals may be unstable

# double boot, all

    Code
      boot.result$t0
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.093216534 -0.097030372  0.611850845  0.220992327  0.610417713  0.679294071 
           re.SySm     re.SmXSm     re.SyMSm     re.SyXSm      re.SmSy      re.SySy 
       0.056841871  0.017842843  0.009498816 -0.006990675  0.056841871  0.270142249 
          re.SmXSy     re.SyMSy     re.SyXSy     re.SmSmX     re.SySmX    re.SmXSmX 
       0.011784875 -0.004323056 -0.018316454  0.017842843  0.011784875  0.120091126 
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
      95%   (-0.1133,  0.2874 )  
      Calculations and Intervals on Original Scale
      Some percentile intervals may be unstable

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "indirect", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.3656735 0.5790592 
      
      $est
      [1] 0.4721302
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "a", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5259069 0.6915684 
      
      $est
      [1] 0.6118508
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "b", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.4657587 0.7423931 
      
      $est
      [1] 0.6104177
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "cprime", ci.conf = 0.95)
    Output
      $CI
            2.5%      97.5% 
      0.09310891 0.32521519 
      
      $est
      [1] 0.2209923
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "covab", ci.conf = 0.95)
    Output
      $CI
            2.5%      97.5% 
      0.04835209 0.17420484 
      
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
      95%   ( 0.3690,  0.5971 )  
      Calculations and Intervals on Original Scale
      Some percentile intervals may be unstable

# level 2 boot, all

    Code
      boot.result$t0
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.093216534 -0.097030372  0.611850845  0.220992327  0.610417713  0.679294071 
           re.SySm     re.SmXSm     re.SyMSm     re.SyXSm      re.SmSy      re.SySy 
       0.056841871  0.017842843  0.009498816 -0.006990675  0.056841871  0.270142249 
          re.SmXSy     re.SyMSy     re.SyXSy     re.SmSmX     re.SySmX    re.SmXSmX 
       0.011784875 -0.004323056 -0.018316454  0.017842843  0.011784875  0.120091126 
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
      95%   (-0.1063,  0.3030 )  
      Calculations and Intervals on Original Scale
      Some percentile intervals may be unstable

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "indirect", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.3792086 0.5801355 
      
      $est
      [1] 0.4721302
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "a", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5334494 0.6976009 
      
      $est
      [1] 0.6118508
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "b", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5137218 0.7075524 
      
      $est
      [1] 0.6104177
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "cprime", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.1341611 0.3120920 
      
      $est
      [1] 0.2209923
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "covab", ci.conf = 0.95)
    Output
      $CI
            2.5%      97.5% 
      0.05343939 0.15434969 
      
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
      

