# custom double boot

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
      extract.boot.modmed.mlm(boot.result, type = "indirect", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.3741537 0.5497128 
      
      $est
      [1] 0.4721302
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "a", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5297676 0.6958566 
      
      $est
      [1] 0.6118508
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "b", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.4789898 0.6983879 
      
      $est
      [1] 0.6104177
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "cprime", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.1033340 0.3011657 
      
      $est
      [1] 0.2209923
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "covab", ci.conf = 0.95)
    Output
      $CI
            2.5%      97.5% 
      0.04497169 0.17101320 
      
      $est
      [1] 0.09864558
      

# custom level 2 boot

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
      extract.boot.modmed.mlm(boot.result, type = "indirect", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.3714792 0.5270503 
      
      $est
      [1] 0.4721302
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "a", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5343788 0.6758379 
      
      $est
      [1] 0.6118508
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "b", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5179698 0.6727246 
      
      $est
      [1] 0.6104177
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "cprime", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.1550536 0.3071533 
      
      $est
      [1] 0.2209923
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "covab", ci.conf = 0.95)
    Output
      $CI
            2.5%      97.5% 
      0.05056144 0.12996191 
      
      $est
      [1] 0.09864558
      

# custom resid boot

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
      extract.boot.modmed.mlm(boot.result, type = "indirect", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.3709168 0.5644604 
      
      $est
      [1] 0.4721302
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "a", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5276175 0.7043573 
      
      $est
      [1] 0.6118508
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "b", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5098324 0.7107582 
      
      $est
      [1] 0.6104177
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "cprime", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.1339179 0.2968167 
      
      $est
      [1] 0.2209923
      

---

    Code
      extract.boot.modmed.mlm(boot.result, type = "covab", ci.conf = 0.95)
    Output
      $CI
            2.5%      97.5% 
      0.06009195 0.13673317 
      
      $est
      [1] 0.09864558
      

# custom resid boot, glmmTMB

    Code
      boot_resid_glmmTMB$t0
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.093215043 -0.096851776  0.611856524  0.220812370  0.610562559  0.679433836 
           re.SySm     re.SmXSm     re.SyMSm     re.SyXSm      re.SmSy      re.SySy 
       0.056812562  0.018161130  0.009321789 -0.006673870  0.056812562  0.270283401 
          re.SmXSy     re.SyMSy     re.SyXSy     re.SmSmX     re.SySmX    re.SmXSmX 
       0.011879520 -0.004283427 -0.018275861  0.018161130  0.011879520  0.120482848 
         re.SyMSmX    re.SyXSmX     re.SmSyM     re.SySyM    re.SmXSyM    re.SyMSyM 
       0.098954703 -0.021495654  0.009321789 -0.004283427  0.098954703  0.111872467 
         re.SyXSyM     re.SmSyX     re.SySyX    re.SmXSyX    re.SyMSyX    re.SyXSyX 
       0.005421549 -0.006673870 -0.018275861 -0.021495654  0.005421549  0.032437443 

---

    Code
      extract.boot.modmed.mlm(boot_resid_glmmTMB, type = "indirect", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.3713589 0.5654975 
      
      $est
      [1] 0.4725314
      

---

    Code
      extract.boot.modmed.mlm(boot_resid_glmmTMB, type = "a", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5273825 0.7046756 
      
      $est
      [1] 0.6118565
      

---

    Code
      extract.boot.modmed.mlm(boot_resid_glmmTMB, type = "b", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.5099178 0.7112169 
      
      $est
      [1] 0.6105626
      

---

    Code
      extract.boot.modmed.mlm(boot_resid_glmmTMB, type = "cprime", ci.conf = 0.95)
    Output
      $CI
           2.5%     97.5% 
      0.1336557 0.2956653 
      
      $est
      [1] 0.2208124
      

---

    Code
      extract.boot.modmed.mlm(boot_resid_glmmTMB, type = "covab", ci.conf = 0.95)
    Output
      $CI
            2.5%      97.5% 
      0.06059154 0.13656235 
      
      $est
      [1] 0.0989547
      

