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

# moderated a

    Code
      extract.modmed.mlm(fitmoda, "indirect", modval1 = 0)
    Output
      [1] 0.206372

---

    Code
      extract.modmed.mlm(fitmoda, "indirect", modval1 = 1)
    Output
      [1] 0.2479163

---

    Code
      extract.modmed.mlm(fitmoda, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.04154434

---

    Code
      extract.modmed.mlm(fitmoda, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmoda, "indirect", modval1 = 1)
    Output
      [1] -0.04154434

---

    Code
      extract.modmed.mlm(fitmoda, "a", modval1 = 0)
    Output
      [1] 0.2824568

---

    Code
      extract.modmed.mlm(fitmoda, "a", modval1 = 1)
    Output
      [1] 0.3988152

---

    Code
      extract.modmed.mlm(fitmoda, "a.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.1163584

---

    Code
      extract.modmed.mlm(fitmoda, "a", modval1 = 0) - extract.modmed.mlm(fitmoda, "a",
        modval1 = 1)
    Output
      [1] -0.1163584

---

    Code
      extract.modmed.mlm(fitmoda, "b", modval1 = 0)
    Output
      [1] 0.3570378

---

    Code
      extract.modmed.mlm(fitmoda, "b", modval1 = 1)
    Output
      [1] 0.3570378

---

    Code
      extract.modmed.mlm(fitmoda, "b.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0

---

    Code
      extract.modmed.mlm(fitmoda, "b", modval1 = 0) - extract.modmed.mlm(fitmoda, "b",
        modval1 = 1)
    Output
      [1] 0

# moderated b

    Code
      extract.modmed.mlm(fitmodb, "indirect", modval1 = 0)
    Output
      [1] 0.3029877

---

    Code
      extract.modmed.mlm(fitmodb, "indirect", modval1 = 1)
    Output
      [1] 0.1880407

---

    Code
      extract.modmed.mlm(fitmodb, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.114947

---

    Code
      extract.modmed.mlm(fitmodb, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodb, "indirect", modval1 = 1)
    Output
      [1] 0.114947

---

    Code
      extract.modmed.mlm(fitmodb, "a", modval1 = 0)
    Output
      [1] 0.3466759

---

    Code
      extract.modmed.mlm(fitmodb, "a", modval1 = 1)
    Output
      [1] 0.3466759

---

    Code
      extract.modmed.mlm(fitmodb, "a.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0

---

    Code
      extract.modmed.mlm(fitmodb, "a", modval1 = 0) - extract.modmed.mlm(fitmodb, "a",
        modval1 = 1)
    Output
      [1] 0

---

    Code
      extract.modmed.mlm(fitmodb, "b", modval1 = 0)
    Output
      [1] 0.5390711

---

    Code
      extract.modmed.mlm(fitmodb, "b", modval1 = 1)
    Output
      [1] 0.207502

---

    Code
      extract.modmed.mlm(fitmodb, "b.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.3315691

---

    Code
      extract.modmed.mlm(fitmodb, "b", modval1 = 0) - extract.modmed.mlm(fitmodb, "b",
        modval1 = 1)
    Output
      [1] 0.3315691

# moderated a and b

    Code
      extract.modmed.mlm(fitmodab, "indirect", modval1 = 0)
    Output
      [1] 0.2704213

---

    Code
      extract.modmed.mlm(fitmodab, "indirect", modval1 = 1)
    Output
      [1] 0.2006622

---

    Code
      extract.modmed.mlm(fitmodab, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.06975914

---

    Code
      extract.modmed.mlm(fitmodab, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodab, "indirect", modval1 = 1)
    Output
      [1] 0.06975914

---

    Code
      extract.modmed.mlm(fitmodab, "a", modval1 = 0)
    Output
      [1] 0.2831149

---

    Code
      extract.modmed.mlm(fitmodab, "a", modval1 = 1)
    Output
      [1] 0.3998559

---

    Code
      extract.modmed.mlm(fitmodab, "a.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.1167409

---

    Code
      extract.modmed.mlm(fitmodab, "a", modval1 = 0) - extract.modmed.mlm(fitmodab,
        "a", modval1 = 1)
    Output
      [1] -0.1167409

---

    Code
      extract.modmed.mlm(fitmodab, "b", modval1 = 0)
    Output
      [1] 0.5384093

---

    Code
      extract.modmed.mlm(fitmodab, "b", modval1 = 1)
    Output
      [1] 0.2067559

---

    Code
      extract.modmed.mlm(fitmodab, "b.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.3316534

---

    Code
      extract.modmed.mlm(fitmodab, "b", modval1 = 0) - extract.modmed.mlm(fitmodab,
        "b", modval1 = 1)
    Output
      [1] 0.3316534

# moderated a and b rand interaction a

    Code
      extract.modmed.mlm(fitmodab2, "indirect", modval1 = 0)
    Output
      [1] 0.3089283

---

    Code
      extract.modmed.mlm(fitmodab2, "indirect", modval1 = 1)
    Output
      [1] 0.161422

---

    Code
      extract.modmed.mlm(fitmodab2, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.1475063

---

    Code
      extract.modmed.mlm(fitmodab2, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodab2, "indirect", modval1 = 1)
    Output
      [1] 0.1475063

---

    Code
      extract.modmed.mlm(fitmodab2, "a", modval1 = 0)
    Output
      [1] 0.286757

---

    Code
      extract.modmed.mlm(fitmodab2, "a", modval1 = 1)
    Output
      [1] 0.3806721

---

    Code
      extract.modmed.mlm(fitmodab2, "a.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.09391512

---

    Code
      extract.modmed.mlm(fitmodab2, "a", modval1 = 0) - extract.modmed.mlm(fitmodab2,
        "a", modval1 = 1)
    Output
      [1] -0.09391512

---

    Code
      extract.modmed.mlm(fitmodab2, "b", modval1 = 0)
    Output
      [1] 0.5325093

---

    Code
      extract.modmed.mlm(fitmodab2, "b", modval1 = 1)
    Output
      [1] 0.2099432

---

    Code
      extract.modmed.mlm(fitmodab2, "b.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.322566

---

    Code
      extract.modmed.mlm(fitmodab2, "b", modval1 = 0) - extract.modmed.mlm(fitmodab2,
        "b", modval1 = 1)
    Output
      [1] 0.322566

# moderated a and b rand interaction b

    Code
      extract.modmed.mlm(fitmodab3, "indirect", modval1 = 0)
    Output
      [1] 0.2929382

---

    Code
      extract.modmed.mlm(fitmodab3, "indirect", modval1 = 1)
    Output
      [1] 0.1902641

---

    Code
      extract.modmed.mlm(fitmodab3, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.1026741

---

    Code
      extract.modmed.mlm(fitmodab3, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodab3, "indirect", modval1 = 1)
    Output
      [1] 0.1026741

---

    Code
      extract.modmed.mlm(fitmodab3, "a", modval1 = 0)
    Output
      [1] 0.2874946

---

    Code
      extract.modmed.mlm(fitmodab3, "a", modval1 = 1)
    Output
      [1] 0.4079787

---

    Code
      extract.modmed.mlm(fitmodab3, "a.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.1204841

---

    Code
      extract.modmed.mlm(fitmodab3, "a", modval1 = 0) - extract.modmed.mlm(fitmodab3,
        "a", modval1 = 1)
    Output
      [1] -0.1204841

---

    Code
      extract.modmed.mlm(fitmodab3, "b", modval1 = 0)
    Output
      [1] 0.5946357

---

    Code
      extract.modmed.mlm(fitmodab3, "b", modval1 = 1)
    Output
      [1] 0.2143952

---

    Code
      extract.modmed.mlm(fitmodab3, "b.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.3802405

---

    Code
      extract.modmed.mlm(fitmodab3, "b", modval1 = 0) - extract.modmed.mlm(fitmodab3,
        "b", modval1 = 1)
    Output
      [1] 0.3802405

# moderated a and b rand interaction both

    Code
      extract.modmed.mlm(fitmodab4, "indirect", modval1 = 0)
    Output
      [1] 0.3091362

---

    Code
      extract.modmed.mlm(fitmodab4, "indirect", modval1 = 1)
    Output
      [1] 0.1511675

---

    Code
      extract.modmed.mlm(fitmodab4, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.1579687

---

    Code
      extract.modmed.mlm(fitmodab4, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodab4, "indirect", modval1 = 1)
    Output
      [1] 0.1579687

---

    Code
      extract.modmed.mlm(fitmodab4, "a", modval1 = 0)
    Output
      [1] 0.2909437

---

    Code
      extract.modmed.mlm(fitmodab4, "a", modval1 = 1)
    Output
      [1] 0.3874318

---

    Code
      extract.modmed.mlm(fitmodab4, "a.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.0964881

---

    Code
      extract.modmed.mlm(fitmodab4, "a", modval1 = 0) - extract.modmed.mlm(fitmodab4,
        "a", modval1 = 1)
    Output
      [1] -0.0964881

---

    Code
      extract.modmed.mlm(fitmodab4, "b", modval1 = 0)
    Output
      [1] 0.5846794

---

    Code
      extract.modmed.mlm(fitmodab4, "b", modval1 = 1)
    Output
      [1] 0.2282893

---

    Code
      extract.modmed.mlm(fitmodab4, "b.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.3563901

---

    Code
      extract.modmed.mlm(fitmodab4, "b", modval1 = 0) - extract.modmed.mlm(fitmodab4,
        "b", modval1 = 1)
    Output
      [1] 0.3563901

