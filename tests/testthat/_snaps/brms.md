# random a

    Code
      extract.modmed.mlm.brms(fit.randa, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.419  0.419 0.0387 0.0397 0.346 0.495  1.00    1973.    2589.

# random b

    Code
      extract.modmed.mlm.brms(fit.randb, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.379  0.378 0.0370 0.0367 0.311 0.454 1.000    1851.    2792.

# random a and b

    Code
      extract.modmed.mlm.brms(fit.randboth, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.452  0.449 0.0527 0.0519 0.355 0.562 1.000    3377.    3165.

# all random

    Code
      extract.modmed.mlm.brms(fit.randall, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.459  0.456 0.0530 0.0512 0.363 0.569  1.00    3325.    4020.

# moderation of a

    Code
      extract.modmed.mlm.brms(fitmoda, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.207  0.201 0.0577 0.0548 0.110 0.336  1.00    1801.    2817.

---

    Code
      extract.modmed.mlm.brms(fitmoda, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.248  0.242 0.0626 0.0585 0.142 0.388  1.00    1861.    2828.

---

    Code
      extract.modmed.mlm.brms(fitmoda, "indirect", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.207  0.201 0.0577 0.0548 0.110 0.336  1.00    1801.    2817.

# moderation of b

    Code
      extract.modmed.mlm.brms(fitmodb, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.305  0.299 0.0693 0.0659 0.188 0.459  1.00    1510.    2425.

---

    Code
      extract.modmed.mlm.brms(fitmodb, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.189  0.184 0.0551 0.0516 0.0961 0.313  1.00    1711.    2386.

---

    Code
      extract.modmed.mlm.brms(fitmodb, "indirect", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.305  0.299 0.0693 0.0659 0.188 0.459  1.00    1510.    2425.

# moderation of a and b

    Code
      extract.modmed.mlm.brms(fitmodab, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.268  0.263 0.0674 0.0663 0.152 0.416  1.00    1647.    2245.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.199  0.193 0.0573 0.0558 0.104 0.328  1.00    1783.    1904.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable       mean median     sd    mad    q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>         <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.di~ 0.0690 0.0682 0.0318 0.0316 0.00995 0.133  1.00    2954.    2983.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.282  0.283 0.0731 0.0738 0.140 0.425  1.00    1825.    2516.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.399  0.399 0.0721 0.0717 0.259 0.541  1.00    2056.    2301.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable   mean median     sd    mad   q2.5   q97.5  rhat ess_bulk ess_tail
        <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.116 -0.117 0.0479 0.0469 -0.210 -0.0211  1.00    8092.    3016.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.539  0.538 0.0784 0.0773 0.382 0.696  1.00    1841.    2617.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.209  0.208 0.0775 0.0754 0.0531 0.362  1.00    1976.    2239.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.330  0.329 0.0430 0.0417 0.245 0.416  1.00    6962.    3386.

# moderation of a and b, re for a int

    Code
      extract.modmed.mlm.brms(fitmodab2, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.291  0.285 0.0689 0.0657 0.174 0.449  1.00    1277.    1565.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.169  0.163 0.0653 0.0626 0.0547 0.314  1.00    1681.    2280.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable       mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>         <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.diff 0.122  0.120 0.0462 0.0452 0.0370 0.216  1.00    2779.    3058.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.291  0.291 0.0697 0.0669 0.153 0.430  1.00    1549.    2054.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.396  0.395 0.0893 0.0865 0.216 0.574  1.00    1708.    1981.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable   mean median     sd    mad   q2.5  q97.5  rhat ess_bulk ess_tail
        <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.105 -0.107 0.0728 0.0695 -0.250 0.0384  1.00    2626.    3056.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.539  0.538 0.0811 0.0799 0.379 0.699  1.00    1495.    1998.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.216  0.216 0.0799 0.0779 0.0563 0.372  1.00    1521.    2026.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.323  0.323 0.0417 0.0422 0.242 0.405  1.00    5838.    3037.

# moderation of a and b, re for b int

    Code
      extract.modmed.mlm.brms(fitmodab3, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.279  0.273 0.0690 0.0669 0.163 0.433  1.00     958.    1928.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.195  0.190 0.0671 0.0627 0.0812 0.343  1.00    1307.    1743.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable       mean median     sd    mad    q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>         <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.di~ 0.0842 0.0831 0.0547 0.0520 -0.0155 0.198  1.00    1409.    2061.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.284  0.284 0.0755 0.0754 0.129 0.431  1.00    1151.    2035.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.403  0.403 0.0739 0.0755 0.260 0.547  1.00    1224.    2577.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable   mean median     sd    mad   q2.5   q97.5  rhat ess_bulk ess_tail
        <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.119 -0.119 0.0469 0.0460 -0.212 -0.0262  1.00    5903.    3006.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.575  0.577 0.0747 0.0744 0.426 0.720  1.00    1224.    2187.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.231  0.231 0.0915 0.0884 0.0461 0.409  1.00    1621.    2258.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.344  0.343 0.0785 0.0774 0.192 0.501  1.00    1891.    2511.

# moderation of a and b, re for both

    Code
      extract.modmed.mlm.brms(fitmodab4, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.282  0.276 0.0623 0.0596 0.176 0.424  1.00    1265.    1791.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.161  0.157 0.0699 0.0646 0.0393 0.312  1.00    1822.    2323.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable       mean median     sd    mad    q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>         <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.diff 0.120  0.120 0.0617 0.0617 0.00258 0.243  1.00    2184.    2813.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.288  0.288 0.0679 0.0676 0.155 0.423  1.00    1242.    1930.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.390  0.388 0.0861 0.0875 0.222 0.561  1.00    1868.    2267.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable   mean median     sd    mad   q2.5  q97.5  rhat ess_bulk ess_tail
        <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.106 -0.106 0.0728 0.0720 -0.253 0.0359 1.000    2485.    2941.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.566  0.566 0.0744 0.0741 0.424 0.716  1.00    1526.    2334.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.235  0.232 0.0928 0.0898 0.0594 0.422  1.00    1737.    2186.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.332  0.333 0.0777 0.0771 0.177 0.484 1.000    2020.    2693.

