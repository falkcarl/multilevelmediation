# BPG rand a rand b

    Code
      extract.modmed.mlm(fit)
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.096021427 -0.093769548  0.612020474  0.244975872  0.588036977  0.698025159 
           re.SySm     re.SmXSm     re.SyMSm      re.SmSy      re.SySy     re.SmXSy 
       0.050961663  0.017420359  0.008279007  0.050961663  0.276503183  0.012243207 
          re.SyMSy     re.SmSmX     re.SySmX    re.SmXSmX    re.SyMSmX     re.SmSyM 
      -0.013316133  0.017420359  0.012243207  0.120764868  0.092594649  0.008279007 
          re.SySyM    re.SmXSyM    re.SyMSyM 
      -0.013316133  0.092594649  0.134697001 

---

    Code
      extract.modmed.mlm(fit, type = "indirect")
    Output
      [1] 0.4524853

---

    Code
      extract.modmed.mlm(fit, type = "a")
    Output
      [1] 0.6120205

---

    Code
      extract.modmed.mlm(fit, type = "b")
    Output
      [1] 0.588037

---

    Code
      extract.modmed.mlm(fit, type = "covab")
    Output
      [1] 0.09259465

---

    Code
      extract.modmed.mlm(fit, type = "cprime")
    Output
      [1] 0.2449759

---

    Code
      extract.modmed.mlm(fit, type = "fixef")
    Output
               Sm          Sy         SmX         SyX         SyM 
       0.09602143 -0.09376955  0.61202047  0.24497587  0.58803698 

---

    Code
      extract.modmed.mlm(fit, type = "recov")
    Output
                   Sm          Sy        SmX          SyM
      Sm  0.698025159  0.05096166 0.01742036  0.008279007
      Sy  0.050961663  0.27650318 0.01224321 -0.013316133
      SmX 0.017420359  0.01224321 0.12076487  0.092594649
      SyM 0.008279007 -0.01331613 0.09259465  0.134697001

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
           re.SmSm      re.SySm     re.SmXSm     re.SyMSm      re.SmSy      re.SySy 
       0.698025159  0.050961663  0.017420359  0.008279007  0.050961663  0.276503183 
          re.SmXSy     re.SyMSy     re.SmSmX     re.SySmX    re.SmXSmX    re.SyMSmX 
       0.012243207 -0.013316133  0.017420359  0.012243207  0.120764868  0.092594649 
          re.SmSyM     re.SySyM    re.SmXSyM    re.SyMSyM 
       0.008279007 -0.013316133  0.092594649  0.134697001 

# BPG rand a, rand b, rand c

    Code
      extract.modmed.mlm(fit)
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.092678686 -0.097199099  0.611892573  0.219354446  0.611767037  0.677715701 
           re.SySm     re.SmXSm     re.SyMSm     re.SyXSm      re.SmSy      re.SySy 
       0.056177784  0.018544187  0.009296541 -0.006039702  0.056177784  0.271355771 
          re.SmXSy     re.SyMSy     re.SyXSy     re.SmSmX     re.SySmX    re.SmXSmX 
       0.012275780 -0.004325420 -0.017470816  0.018544187  0.012275780  0.120099536 
         re.SyMSmX    re.SyXSmX     re.SmSyM     re.SySyM    re.SmXSyM    re.SyMSyM 
       0.099910030 -0.022761504  0.009296541 -0.004325420  0.099910030  0.110315717 
         re.SyXSyM     re.SmSyX     re.SySyX    re.SmXSyX    re.SyMSyX    re.SyXSyX 
       0.007020152 -0.006039702 -0.017470816 -0.022761504  0.007020152  0.030512998 

---

    Code
      extract.modmed.mlm(fit, type = "indirect")
    Output
      [1] 0.4742457

---

    Code
      extract.modmed.mlm(fit, type = "a")
    Output
      [1] 0.6118926

---

    Code
      extract.modmed.mlm(fit, type = "b")
    Output
      [1] 0.611767

---

    Code
      extract.modmed.mlm(fit, type = "covab")
    Output
      [1] 0.09991003

---

    Code
      extract.modmed.mlm(fit, type = "cprime")
    Output
      [1] 0.2193544

---

    Code
      extract.modmed.mlm(fit, type = "fixef")
    Output
               Sm          Sy         SmX         SyX         SyM 
       0.09267869 -0.09719910  0.61189257  0.21935445  0.61176704 

---

    Code
      extract.modmed.mlm(fit, type = "recov")
    Output
                    Sm          Sy         SmX          SyM          SyX
      Sm   0.677715701  0.05617778  0.01854419  0.009296541 -0.006039702
      Sy   0.056177784  0.27135577  0.01227578 -0.004325420 -0.017470816
      SmX  0.018544187  0.01227578  0.12009954  0.099910030 -0.022761504
      SyM  0.009296541 -0.00432542  0.09991003  0.110315717  0.007020152
      SyX -0.006039702 -0.01747082 -0.02276150  0.007020152  0.030512998

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
           re.SmSm      re.SySm     re.SmXSm     re.SyMSm     re.SyXSm      re.SmSy 
       0.677715701  0.056177784  0.018544187  0.009296541 -0.006039702  0.056177784 
           re.SySy     re.SmXSy     re.SyMSy     re.SyXSy     re.SmSmX     re.SySmX 
       0.271355771  0.012275780 -0.004325420 -0.017470816  0.018544187  0.012275780 
         re.SmXSmX    re.SyMSmX    re.SyXSmX     re.SmSyM     re.SySyM    re.SmXSyM 
       0.120099536  0.099910030 -0.022761504  0.009296541 -0.004325420  0.099910030 
         re.SyMSyM    re.SyXSyM     re.SmSyX     re.SySyX    re.SmXSyX    re.SyMSyX 
       0.110315717  0.007020152 -0.006039702 -0.017470816 -0.022761504  0.007020152 
         re.SyXSyX 
       0.030512998 

# BPG rand a

    Code
      extract.modmed.mlm(fit)
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.094696300 -0.122614524  0.611216774  0.284493927  0.685198460  0.708110637 
           re.SySm     re.SmXSm      re.SmSy      re.SySy     re.SmXSy     re.SmSmX 
       0.049456221  0.007724591  0.049456221  0.417117056 -0.007026517  0.007724591 
          re.SySmX    re.SmXSmX 
      -0.007026517  0.115590548 

---

    Code
      extract.modmed.mlm(fit, type = "indirect")
    Output
      [1] 0.4188048

---

    Code
      extract.modmed.mlm(fit, type = "a")
    Output
      [1] 0.6112168

---

    Code
      extract.modmed.mlm(fit, type = "b")
    Output
      [1] 0.6851985

---

    Code
      extract.modmed.mlm(fit, type = "cprime")
    Output
      [1] 0.2844939

---

    Code
      extract.modmed.mlm(fit, type = "fixef")
    Output
              Sm         Sy        SmX        SyX        SyM 
       0.0946963 -0.1226145  0.6112168  0.2844939  0.6851985 

---

    Code
      extract.modmed.mlm(fit, type = "recov")
    Output
                   Sm           Sy          SmX
      Sm  0.708110637  0.049456221  0.007724591
      Sy  0.049456221  0.417117056 -0.007026517
      SmX 0.007724591 -0.007026517  0.115590548

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
           re.SmSm      re.SySm     re.SmXSm      re.SmSy      re.SySy     re.SmXSy 
       0.708110637  0.049456221  0.007724591  0.049456221  0.417117056 -0.007026517 
          re.SmSmX     re.SySmX    re.SmXSmX 
       0.007724591 -0.007026517  0.115590548 

# BPG rand b

    Code
      extract.modmed.mlm(fit)
    Output
               Sm          Sy         SmX         SyX         SyM     re.SmSm 
       0.07089517 -0.08971696  0.62127749  0.26163168  0.60777759  0.80731950 
          re.SySm    re.SyMSm     re.SmSy     re.SySy    re.SyMSy    re.SmSyM 
       0.04480842 -0.02928484  0.04480842  0.27556662 -0.01768606 -0.02928484 
         re.SySyM   re.SyMSyM 
      -0.01768606  0.13410969 

---

    Code
      extract.modmed.mlm(fit, type = "indirect")
    Output
      [1] 0.3775985

---

    Code
      extract.modmed.mlm(fit, type = "a")
    Output
      [1] 0.6212775

---

    Code
      extract.modmed.mlm(fit, type = "b")
    Output
      [1] 0.6077776

---

    Code
      extract.modmed.mlm(fit, type = "cprime")
    Output
      [1] 0.2616317

---

    Code
      extract.modmed.mlm(fit, type = "fixef")
    Output
               Sm          Sy         SmX         SyX         SyM 
       0.07089517 -0.08971696  0.62127749  0.26163168  0.60777759 

---

    Code
      extract.modmed.mlm(fit, type = "recov")
    Output
                   Sm          Sy         SyM
      Sm   0.80731950  0.04480842 -0.02928484
      Sy   0.04480842  0.27556662 -0.01768606
      SyM -0.02928484 -0.01768606  0.13410969

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
          re.SmSm     re.SySm    re.SyMSm     re.SmSy     re.SySy    re.SyMSy 
       0.80731950  0.04480842 -0.02928484  0.04480842  0.27556662 -0.01768606 
         re.SmSyM    re.SySyM   re.SyMSyM 
      -0.02928484 -0.01768606  0.13410969 

