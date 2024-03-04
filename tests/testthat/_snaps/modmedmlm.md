# BPG rand a rand b

    Code
      extract.modmed.mlm(fit)
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.096021954 -0.093772127  0.612020371  0.244975365  0.588036915  0.698044627 
           re.SySm     re.SmXSm     re.SyMSm      re.SmSy      re.SySy     re.SmXSy 
       0.050825005  0.017486251  0.008367895  0.050825005  0.276507737  0.012162696 
          re.SyMSy     re.SmSmX     re.SySmX    re.SmXSmX    re.SyMSmX     re.SmSyM 
      -0.013223201  0.017486251  0.012162696  0.120767200  0.092542236  0.008367895 
          re.SySyM    re.SmXSyM    re.SyMSyM 
      -0.013223201  0.092542236  0.134700854 

---

    Code
      extract.modmed.mlm(fit, type = "indirect")
    Output
      [1] 0.4524328

---

    Code
      extract.modmed.mlm(fit, type = "a")
    Output
      [1] 0.6120204

---

    Code
      extract.modmed.mlm(fit, type = "b")
    Output
      [1] 0.5880369

---

    Code
      extract.modmed.mlm(fit, type = "covab")
    Output
      [1] 0.09254224

---

    Code
      extract.modmed.mlm(fit, type = "cprime")
    Output
      [1] 0.2449754

---

    Code
      extract.modmed.mlm(fit, type = "fixef")
    Output
               Sm          Sy         SmX         SyX         SyM 
       0.09602195 -0.09377213  0.61202037  0.24497536  0.58803691 

---

    Code
      extract.modmed.mlm(fit, type = "recov")
    Output
                   Sm         Sy        SmX          SyM
      Sm  0.698044627  0.0508250 0.01748625  0.008367895
      Sy  0.050825005  0.2765077 0.01216270 -0.013223201
      SmX 0.017486251  0.0121627 0.12076720  0.092542236
      SyM 0.008367895 -0.0132232 0.09254224  0.134700854

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
           re.SmSm      re.SySm     re.SmXSm     re.SyMSm      re.SmSy      re.SySy 
       0.698044627  0.050825005  0.017486251  0.008367895  0.050825005  0.276507737 
          re.SmXSy     re.SyMSy     re.SmSmX     re.SySmX    re.SmXSmX    re.SyMSmX 
       0.012162696 -0.013223201  0.017486251  0.012162696  0.120767200  0.092542236 
          re.SmSyM     re.SySyM    re.SmXSyM    re.SyMSyM 
       0.008367895 -0.013223201  0.092542236  0.134700854 

# BPG rand a, rand b, rand c

    Code
      extract.modmed.mlm(fit)
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.092678783 -0.097198953  0.611892535  0.219355102  0.611766783  0.677715547 
           re.SySm     re.SmXSm     re.SyMSm     re.SyXSm      re.SmSy      re.SySy 
       0.056226656  0.018587977  0.009326583 -0.005992485  0.056226656  0.271354193 
          re.SmXSy     re.SyMSy     re.SyXSy     re.SmSmX     re.SySmX    re.SmXSmX 
       0.012226000 -0.004331612 -0.017460059  0.018587977  0.012226000  0.120100414 
         re.SyMSmX    re.SyXSmX     re.SmSyM     re.SySyM    re.SmXSyM    re.SyMSyM 
       0.099872640 -0.022766909  0.009326583 -0.004331612  0.099872640  0.110314811 
         re.SyXSyM     re.SmSyX     re.SySyX    re.SmXSyX    re.SyMSyX    re.SyXSyX 
       0.007044345 -0.005992485 -0.017460059 -0.022766909  0.007044345  0.030512781 

---

    Code
      extract.modmed.mlm(fit, type = "indirect")
    Output
      [1] 0.4742082

---

    Code
      extract.modmed.mlm(fit, type = "a")
    Output
      [1] 0.6118925

---

    Code
      extract.modmed.mlm(fit, type = "b")
    Output
      [1] 0.6117668

---

    Code
      extract.modmed.mlm(fit, type = "covab")
    Output
      [1] 0.09987264

---

    Code
      extract.modmed.mlm(fit, type = "cprime")
    Output
      [1] 0.2193551

---

    Code
      extract.modmed.mlm(fit, type = "fixef")
    Output
               Sm          Sy         SmX         SyX         SyM 
       0.09267878 -0.09719895  0.61189254  0.21935510  0.61176678 

---

    Code
      extract.modmed.mlm(fit, type = "recov")
    Output
                    Sm           Sy         SmX          SyM          SyX
      Sm   0.677715547  0.056226656  0.01858798  0.009326583 -0.005992485
      Sy   0.056226656  0.271354193  0.01222600 -0.004331612 -0.017460059
      SmX  0.018587977  0.012226000  0.12010041  0.099872640 -0.022766909
      SyM  0.009326583 -0.004331612  0.09987264  0.110314811  0.007044345
      SyX -0.005992485 -0.017460059 -0.02276691  0.007044345  0.030512781

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
           re.SmSm      re.SySm     re.SmXSm     re.SyMSm     re.SyXSm      re.SmSy 
       0.677715547  0.056226656  0.018587977  0.009326583 -0.005992485  0.056226656 
           re.SySy     re.SmXSy     re.SyMSy     re.SyXSy     re.SmSmX     re.SySmX 
       0.271354193  0.012226000 -0.004331612 -0.017460059  0.018587977  0.012226000 
         re.SmXSmX    re.SyMSmX    re.SyXSmX     re.SmSyM     re.SySyM    re.SmXSyM 
       0.120100414  0.099872640 -0.022766909  0.009326583 -0.004331612  0.099872640 
         re.SyMSyM    re.SyXSyM     re.SmSyX     re.SySyX    re.SmXSyX    re.SyMSyX 
       0.110314811  0.007044345 -0.005992485 -0.017460059 -0.022766909  0.007044345 
         re.SyXSyX 
       0.030512781 

# BPG rand a

    Code
      extract.modmed.mlm(fit)
    Output
                Sm           Sy          SmX          SyX          SyM      re.SmSm 
       0.094696305 -0.122614503  0.611216790  0.284494102  0.685198220  0.708110737 
           re.SySm     re.SmXSm      re.SmSy      re.SySy     re.SmXSy     re.SmSmX 
       0.049550703  0.007702409  0.049550703  0.417115234 -0.007002409  0.007702409 
          re.SySmX    re.SmXSmX 
      -0.007002409  0.115591134 

---

    Code
      extract.modmed.mlm(fit, type = "indirect")
    Output
      [1] 0.4188047

---

    Code
      extract.modmed.mlm(fit, type = "a")
    Output
      [1] 0.6112168

---

    Code
      extract.modmed.mlm(fit, type = "b")
    Output
      [1] 0.6851982

---

    Code
      extract.modmed.mlm(fit, type = "cprime")
    Output
      [1] 0.2844941

---

    Code
      extract.modmed.mlm(fit, type = "fixef")
    Output
              Sm         Sy        SmX        SyX        SyM 
       0.0946963 -0.1226145  0.6112168  0.2844941  0.6851982 

---

    Code
      extract.modmed.mlm(fit, type = "recov")
    Output
                   Sm           Sy          SmX
      Sm  0.708110737  0.049550703  0.007702409
      Sy  0.049550703  0.417115234 -0.007002409
      SmX 0.007702409 -0.007002409  0.115591134

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
           re.SmSm      re.SySm     re.SmXSm      re.SmSy      re.SySy     re.SmXSy 
       0.708110737  0.049550703  0.007702409  0.049550703  0.417115234 -0.007002409 
          re.SmSmX     re.SySmX    re.SmXSmX 
       0.007702409 -0.007002409  0.115591134 

# BPG rand b

    Code
      extract.modmed.mlm(fit)
    Output
               Sm          Sy         SmX         SyX         SyM     re.SmSm 
       0.07089517 -0.08971709  0.62127750  0.26163174  0.60777762  0.80731942 
          re.SySm    re.SyMSm     re.SmSy     re.SySy    re.SyMSy    re.SmSyM 
       0.04491574 -0.02923504  0.04491574  0.27556755 -0.01777642 -0.02923504 
         re.SySyM   re.SyMSyM 
      -0.01777642  0.13410918 

---

    Code
      extract.modmed.mlm(fit, type = "indirect")
    Output
      [1] 0.3775986

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
       0.07089517 -0.08971709  0.62127750  0.26163174  0.60777762 

---

    Code
      extract.modmed.mlm(fit, type = "recov")
    Output
                   Sm          Sy         SyM
      Sm   0.80731942  0.04491574 -0.02923504
      Sy   0.04491574  0.27556755 -0.01777642
      SyM -0.02923504 -0.01777642  0.13410918

---

    Code
      extract.modmed.mlm(fit, type = "recov.vec")
    Output
          re.SmSm     re.SySm    re.SyMSm     re.SmSy     re.SySy    re.SyMSy 
       0.80731942  0.04491574 -0.02923504  0.04491574  0.27556755 -0.01777642 
         re.SmSyM    re.SySyM   re.SyMSyM 
      -0.02923504 -0.01777642  0.13410918 

# moderated a

    Code
      extract.modmed.mlm(fitmoda, "indirect", modval1 = 0)
    Output
      [1] 0.2064048

---

    Code
      extract.modmed.mlm(fitmoda, "indirect", modval1 = 1)
    Output
      [1] 0.247949

---

    Code
      extract.modmed.mlm(fitmoda, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.04154419

---

    Code
      extract.modmed.mlm(fitmoda, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmoda, "indirect", modval1 = 1)
    Output
      [1] -0.04154419

---

    Code
      extract.modmed.mlm(fitmoda, "a", modval1 = 0)
    Output
      [1] 0.2824572

---

    Code
      extract.modmed.mlm(fitmoda, "a", modval1 = 1)
    Output
      [1] 0.3988154

---

    Code
      extract.modmed.mlm(fitmoda, "a.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.1163582

---

    Code
      extract.modmed.mlm(fitmoda, "a", modval1 = 0) - extract.modmed.mlm(fitmoda, "a",
        modval1 = 1)
    Output
      [1] -0.1163582

---

    Code
      extract.modmed.mlm(fitmoda, "b", modval1 = 0)
    Output
      [1] 0.3570369

---

    Code
      extract.modmed.mlm(fitmoda, "b", modval1 = 1)
    Output
      [1] 0.3570369

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
      [1] 0.3029866

---

    Code
      extract.modmed.mlm(fitmodb, "indirect", modval1 = 1)
    Output
      [1] 0.1880396

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
      [1] 0.3466758

---

    Code
      extract.modmed.mlm(fitmodb, "a", modval1 = 1)
    Output
      [1] 0.3466758

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
      [1] 0.5390697

---

    Code
      extract.modmed.mlm(fitmodb, "b", modval1 = 1)
    Output
      [1] 0.2075004

---

    Code
      extract.modmed.mlm(fitmodb, "b.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.3315693

---

    Code
      extract.modmed.mlm(fitmodb, "b", modval1 = 0) - extract.modmed.mlm(fitmodb, "b",
        modval1 = 1)
    Output
      [1] 0.3315693

# moderated a and b

    Code
      extract.modmed.mlm(fitmodab, "indirect", modval1 = 0)
    Output
      [1] 0.2704902

---

    Code
      extract.modmed.mlm(fitmodab, "indirect", modval1 = 1)
    Output
      [1] 0.2007316

---

    Code
      extract.modmed.mlm(fitmodab, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.06975858

---

    Code
      extract.modmed.mlm(fitmodab, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodab, "indirect", modval1 = 1)
    Output
      [1] 0.06975858

---

    Code
      extract.modmed.mlm(fitmodab, "a", modval1 = 0)
    Output
      [1] 0.283115

---

    Code
      extract.modmed.mlm(fitmodab, "a", modval1 = 1)
    Output
      [1] 0.3998554

---

    Code
      extract.modmed.mlm(fitmodab, "a.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.1167403

---

    Code
      extract.modmed.mlm(fitmodab, "a", modval1 = 0) - extract.modmed.mlm(fitmodab,
        "a", modval1 = 1)
    Output
      [1] -0.1167403

---

    Code
      extract.modmed.mlm(fitmodab, "b", modval1 = 0)
    Output
      [1] 0.5384096

---

    Code
      extract.modmed.mlm(fitmodab, "b", modval1 = 1)
    Output
      [1] 0.206758

---

    Code
      extract.modmed.mlm(fitmodab, "b.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.3316517

---

    Code
      extract.modmed.mlm(fitmodab, "b", modval1 = 0) - extract.modmed.mlm(fitmodab,
        "b", modval1 = 1)
    Output
      [1] 0.3316517

# moderated a and b rand interaction a

    Code
      extract.modmed.mlm(fitmodab2, "indirect", modval1 = 0)
    Output
      [1] 0.3088432

---

    Code
      extract.modmed.mlm(fitmodab2, "indirect", modval1 = 1)
    Output
      [1] 0.1614151

---

    Code
      extract.modmed.mlm(fitmodab2, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.1474281

---

    Code
      extract.modmed.mlm(fitmodab2, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodab2, "indirect", modval1 = 1)
    Output
      [1] 0.1474281

---

    Code
      extract.modmed.mlm(fitmodab2, "a", modval1 = 0)
    Output
      [1] 0.2867552

---

    Code
      extract.modmed.mlm(fitmodab2, "a", modval1 = 1)
    Output
      [1] 0.3806712

---

    Code
      extract.modmed.mlm(fitmodab2, "a.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.09391599

---

    Code
      extract.modmed.mlm(fitmodab2, "a", modval1 = 0) - extract.modmed.mlm(fitmodab2,
        "a", modval1 = 1)
    Output
      [1] -0.09391599

---

    Code
      extract.modmed.mlm(fitmodab2, "b", modval1 = 0)
    Output
      [1] 0.5325046

---

    Code
      extract.modmed.mlm(fitmodab2, "b", modval1 = 1)
    Output
      [1] 0.2099373

---

    Code
      extract.modmed.mlm(fitmodab2, "b.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.3225673

---

    Code
      extract.modmed.mlm(fitmodab2, "b", modval1 = 0) - extract.modmed.mlm(fitmodab2,
        "b", modval1 = 1)
    Output
      [1] 0.3225673

# moderated a and b rand interaction b

    Code
      extract.modmed.mlm(fitmodab3, "indirect", modval1 = 0)
    Output
      [1] 0.2928716

---

    Code
      extract.modmed.mlm(fitmodab3, "indirect", modval1 = 1)
    Output
      [1] 0.1901704

---

    Code
      extract.modmed.mlm(fitmodab3, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.1027011

---

    Code
      extract.modmed.mlm(fitmodab3, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodab3, "indirect", modval1 = 1)
    Output
      [1] 0.1027011

---

    Code
      extract.modmed.mlm(fitmodab3, "a", modval1 = 0)
    Output
      [1] 0.2874954

---

    Code
      extract.modmed.mlm(fitmodab3, "a", modval1 = 1)
    Output
      [1] 0.4079777

---

    Code
      extract.modmed.mlm(fitmodab3, "a.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.1204823

---

    Code
      extract.modmed.mlm(fitmodab3, "a", modval1 = 0) - extract.modmed.mlm(fitmodab3,
        "a", modval1 = 1)
    Output
      [1] -0.1204823

---

    Code
      extract.modmed.mlm(fitmodab3, "b", modval1 = 0)
    Output
      [1] 0.5946465

---

    Code
      extract.modmed.mlm(fitmodab3, "b", modval1 = 1)
    Output
      [1] 0.214396

---

    Code
      extract.modmed.mlm(fitmodab3, "b.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.3802505

---

    Code
      extract.modmed.mlm(fitmodab3, "b", modval1 = 0) - extract.modmed.mlm(fitmodab3,
        "b", modval1 = 1)
    Output
      [1] 0.3802505

# moderated a and b rand interaction both

    Code
      extract.modmed.mlm(fitmodab4, "indirect", modval1 = 0)
    Output
      [1] 0.3091309

---

    Code
      extract.modmed.mlm(fitmodab4, "indirect", modval1 = 1)
    Output
      [1] 0.1511918

---

    Code
      extract.modmed.mlm(fitmodab4, "indirect.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.1579391

---

    Code
      extract.modmed.mlm(fitmodab4, "indirect", modval1 = 0) - extract.modmed.mlm(
        fitmodab4, "indirect", modval1 = 1)
    Output
      [1] 0.1579391

---

    Code
      extract.modmed.mlm(fitmodab4, "a", modval1 = 0)
    Output
      [1] 0.2909317

---

    Code
      extract.modmed.mlm(fitmodab4, "a", modval1 = 1)
    Output
      [1] 0.3874295

---

    Code
      extract.modmed.mlm(fitmodab4, "a.diff", modval1 = 0, modval2 = 1)
    Output
      [1] -0.09649783

---

    Code
      extract.modmed.mlm(fitmodab4, "a", modval1 = 0) - extract.modmed.mlm(fitmodab4,
        "a", modval1 = 1)
    Output
      [1] -0.09649783

---

    Code
      extract.modmed.mlm(fitmodab4, "b", modval1 = 0)
    Output
      [1] 0.5847626

---

    Code
      extract.modmed.mlm(fitmodab4, "b", modval1 = 1)
    Output
      [1] 0.2282696

---

    Code
      extract.modmed.mlm(fitmodab4, "b.diff", modval1 = 0, modval2 = 1)
    Output
      [1] 0.356493

---

    Code
      extract.modmed.mlm(fitmodab4, "b", modval1 = 0) - extract.modmed.mlm(fitmodab4,
        "b", modval1 = 1)
    Output
      [1] 0.356493

