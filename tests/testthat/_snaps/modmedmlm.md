# BPG rand a rand b

    Code
      extract.modmed.mlm(fit)
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.096021427 -0.093769548  0.612020474  0.244975872  0.588036977  0.698025213 
           re.SySm     re.SmXSm     re.SyMSm      re.SmSy      re.SySy     re.SmXSy 
       0.050824164  0.017477586  0.008356598  0.050824164  0.276503200  0.012162786 
          re.SyMSy     re.SmSmX     re.SySmX    re.SmXSmX    re.SyMSmX     re.SmSyM 
      -0.013229249  0.017477586  0.012162786  0.120764870  0.092540870  0.008356598 
          re.SySyM    re.SmXSyM    re.SyMSyM 
      -0.013229249  0.092540870  0.134696988 

---

    Code
      extract.modmed.mlm(fit, type = "indirect")
    Output
      [1] 0.4524315

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
      [1] 0.09254087

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
      Sm  0.698025213  0.05082416 0.01747759  0.008356598
      Sy  0.050824164  0.27650320 0.01216279 -0.013229249
      SmX 0.017477586  0.01216279 0.12076487  0.092540870
      SyM 0.008356598 -0.01322925 0.09254087  0.134696988

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
           re.SmSm      re.SySm     re.SmXSm     re.SyMSm      re.SmSy      re.SySy 
       0.698025213  0.050824164  0.017477586  0.008356598  0.050824164  0.276503200 
          re.SmXSy     re.SyMSy     re.SmSmX     re.SySmX    re.SmXSmX    re.SyMSmX 
       0.012162786 -0.013229249  0.017477586  0.012162786  0.120764870  0.092540870 
          re.SmSyM     re.SySyM    re.SmXSyM    re.SyMSyM 
       0.008356598 -0.013229249  0.092540870  0.134696988 

# BPG rand a, rand b, rand c

    Code
      extract.modmed.mlm(fit)
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.092678686 -0.097199099  0.611892573  0.219354446  0.611767037  0.677715690 
           re.SySm     re.SmXSm     re.SyMSm     re.SyXSm      re.SmSy      re.SySy 
       0.056226677  0.018588010  0.009326880 -0.005992049  0.056226677  0.271355806 
          re.SmXSy     re.SyMSy     re.SyXSy     re.SmSmX     re.SySmX    re.SmXSmX 
       0.012226367 -0.004331304 -0.017459285  0.018588010  0.012226367  0.120099569 
         re.SyMSmX    re.SyXSmX     re.SmSyM     re.SySyM    re.SmXSyM    re.SyMSyM 
       0.099872856 -0.022767536  0.009326880 -0.004331304  0.099872856  0.110315721 
         re.SyXSyM     re.SmSyX     re.SySyX    re.SmXSyX    re.SyMSyX    re.SyXSyX 
       0.007043619 -0.005992049 -0.017459285 -0.022767536  0.007043619  0.030513005 

---

    Code
      extract.modmed.mlm(fit, type = "indirect")
    Output
      [1] 0.4742086

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
      [1] 0.09987286

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
                    Sm           Sy         SmX          SyM          SyX
      Sm   0.677715690  0.056226677  0.01858801  0.009326880 -0.005992049
      Sy   0.056226677  0.271355806  0.01222637 -0.004331304 -0.017459285
      SmX  0.018588010  0.012226367  0.12009957  0.099872856 -0.022767536
      SyM  0.009326880 -0.004331304  0.09987286  0.110315721  0.007043619
      SyX -0.005992049 -0.017459285 -0.02276754  0.007043619  0.030513005

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
           re.SmSm      re.SySm     re.SmXSm     re.SyMSm     re.SyXSm      re.SmSy 
       0.677715690  0.056226677  0.018588010  0.009326880 -0.005992049  0.056226677 
           re.SySy     re.SmXSy     re.SyMSy     re.SyXSy     re.SmSmX     re.SySmX 
       0.271355806  0.012226367 -0.004331304 -0.017459285  0.018588010  0.012226367 
         re.SmXSmX    re.SyMSmX    re.SyXSmX     re.SmSyM     re.SySyM    re.SmXSyM 
       0.120099569  0.099872856 -0.022767536  0.009326880 -0.004331304  0.099872856 
         re.SyMSyM    re.SyXSyM     re.SmSyX     re.SySyX    re.SmXSyX    re.SyMSyX 
       0.110315721  0.007043619 -0.005992049 -0.017459285 -0.022767536  0.007043619 
         re.SyXSyX 
       0.030513005 

# BPG rand a

    Code
      extract.modmed.mlm(fit)
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.094696300 -0.122614524  0.611216774  0.284493927  0.685198460  0.708110602 
           re.SySm     re.SmXSm      re.SmSy      re.SySy     re.SmXSy     re.SmSmX 
       0.049549372  0.007702749  0.049549372  0.417117063 -0.007003034  0.007702749 
          re.SySmX    re.SmXSmX 
      -0.007003034  0.115590562 

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
      Sm  0.708110602  0.049549372  0.007702749
      Sy  0.049549372  0.417117063 -0.007003034
      SmX 0.007702749 -0.007003034  0.115590562

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
           re.SmSm      re.SySm     re.SmXSm      re.SmSy      re.SySy     re.SmXSy 
       0.708110602  0.049549372  0.007702749  0.049549372  0.417117063 -0.007003034 
          re.SmSmX     re.SySmX    re.SmXSmX 
       0.007702749 -0.007003034  0.115590562 

# BPG rand b

    Code
      extract.modmed.mlm(fit)
    Output
               Sm          Sy         SmX         SyX         SyM     re.SmSm 
       0.07089517 -0.08971696  0.62127749  0.26163168  0.60777759  0.80731951 
          re.SySm    re.SyMSm     re.SmSy     re.SySy    re.SyMSy    re.SmSyM 
       0.04491563 -0.02923564  0.04491563  0.27556666 -0.01777719 -0.02923564 
         re.SySyM   re.SyMSyM 
      -0.01777719  0.13410971 

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
      Sm   0.80731951  0.04491563 -0.02923564
      Sy   0.04491563  0.27556666 -0.01777719
      SyM -0.02923564 -0.01777719  0.13410971

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
          re.SmSm     re.SySm    re.SyMSm     re.SmSy     re.SySy    re.SyMSy 
       0.80731951  0.04491563 -0.02923564  0.04491563  0.27556666 -0.01777719 
         re.SmSyM    re.SySyM   re.SyMSyM 
      -0.02923564 -0.01777719  0.13410971 

# moderated a

    Code
      extract.modmed.mlm(fitmoda, "indirect", modval1 = 0)
    Output
      [1] 0.2064083

---

    Code
      extract.modmed.mlm(fitmoda, "indirect", modval1 = 1)
    Output
      [1] 0.2479526

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
      [1] 0.3029876

---

    Code
      extract.modmed.mlm(fitmodb, "indirect", modval1 = 1)
    Output
      [1] 0.1880406

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
      [1] 0.2705027

---

    Code
      extract.modmed.mlm(fitmodab, "indirect", modval1 = 1)
    Output
      [1] 0.2007436

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
      [1] 0.3088488

---

    Code
      extract.modmed.mlm(fitmodab2, "indirect", modval1 = 1)
    Output
      [1] 0.1614119

---

    Code
      extract.modmed.mlm(fitmodab2, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.1474369

---

    Code
      extract.modmed.mlm(fitmodab2, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodab2, "indirect", modval1 = 1)
    Output
      [1] 0.1474369

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
      [1] 0.2928725

---

    Code
      extract.modmed.mlm(fitmodab3, "indirect", modval1 = 1)
    Output
      [1] 0.1901692

---

    Code
      extract.modmed.mlm(fitmodab3, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.1027033

---

    Code
      extract.modmed.mlm(fitmodab3, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodab3, "indirect", modval1 = 1)
    Output
      [1] 0.1027033

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
      [1] 0.3091185

---

    Code
      extract.modmed.mlm(fitmodab4, "indirect", modval1 = 1)
    Output
      [1] 0.1511969

---

    Code
      extract.modmed.mlm(fitmodab4, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.1579216

---

    Code
      extract.modmed.mlm(fitmodab4, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodab4, "indirect", modval1 = 1)
    Output
      [1] 0.1579216

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

