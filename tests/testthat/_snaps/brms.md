# random a

    Code
      extract.modmed.mlm.brms(fit.randa, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.420  0.419 0.0397 0.0386 0.343 0.499  1.00     443.     643.

# random b

    Code
      extract.modmed.mlm.brms(fit.randb, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.377  0.378 0.0362 0.0372 0.306 0.447  1.00     525.     555.

# random a and b

    Code
      extract.modmed.mlm.brms(fit.randboth, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.454  0.450 0.0558 0.0512 0.357 0.581  1.00     570.     746.

# all random

    Code
      extract.modmed.mlm.brms(fit.randall, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.460  0.457 0.0547 0.0525 0.365 0.582  1.01     459.     609.

# moderation of a

    Code
      extract.modmed.mlm.brms(fitmoda, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.204  0.199 0.0578 0.0555 0.102 0.331  1.01     241.     635.

---

    Code
      extract.modmed.mlm.brms(fitmoda, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.246  0.240 0.0636 0.0606 0.134 0.380  1.01     260.     598.

---

    Code
      extract.modmed.mlm.brms(fitmoda, "indirect", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.204  0.199 0.0578 0.0555 0.102 0.331  1.01     241.     635.

# moderation of b

    Code
      extract.modmed.mlm.brms(fitmodb, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.305  0.298 0.0682 0.0669 0.197 0.460  1.00     408.     520.

---

    Code
      extract.modmed.mlm.brms(fitmodb, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.189  0.182 0.0539 0.0522 0.104 0.309  1.00     463.     784.

---

    Code
      extract.modmed.mlm.brms(fitmodb, "indirect", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.305  0.298 0.0682 0.0669 0.197 0.460  1.00     408.     520.

# moderation of a and b

    Code
      extract.modmed.mlm.brms(fitmodab, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.275  0.269 0.0682 0.0667 0.157 0.428  1.00     347.     501.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.205  0.200 0.0587 0.0543 0.106 0.338  1.00     382.     622.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable       mean median     sd    mad    q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>         <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.di~ 0.0706 0.0698 0.0321 0.0311 0.00988 0.139  1.00     610.     701.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.288  0.291 0.0769 0.0754 0.140 0.444  1.01     344.     585.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.403  0.403 0.0774 0.0793 0.249 0.554  1.01     434.     785.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable   mean median     sd    mad   q2.5   q97.5  rhat ess_bulk ess_tail
        <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.115 -0.115 0.0488 0.0497 -0.213 -0.0227  1.01    2103.     875.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.540  0.540 0.0862 0.0852 0.376 0.708  1.01     419.     700.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.210  0.210 0.0845 0.0851 0.0446 0.366  1.00     381.     480.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.330  0.329 0.0420 0.0451 0.253 0.410  1.00    1666.     898.

# moderation of a and b, re for a int

    Code
      extract.modmed.mlm.brms(fitmodab2, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.290  0.284 0.0666 0.0647 0.179 0.434  1.00    1469.    2066.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.169  0.165 0.0635 0.0600 0.0582 0.309  1.00    1645.    2368.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable       mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>         <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.diff 0.121  0.120 0.0476 0.0465 0.0338 0.218  1.00    2967.    3517.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.291  0.292 0.0676 0.0660 0.158 0.423  1.00    1626.    2415.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.392  0.390 0.0861 0.0853 0.220 0.562  1.00    1983.    2495.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable   mean median     sd    mad   q2.5  q97.5  rhat ess_bulk ess_tail
        <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.101 -0.102 0.0723 0.0702 -0.242 0.0471  1.00    3123.    2558.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.542  0.542 0.0801 0.0798 0.384 0.697  1.00    1466.    2257.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.218  0.218 0.0789 0.0775 0.0624 0.372  1.00    1512.    2469.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.324  0.324 0.0433 0.0446 0.239 0.408  1.00    6402.    2646.

# moderation of a and b, re for b int

    Code
      extract.modmed.mlm.brms(fitmodab3, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.279  0.273 0.0674 0.0646 0.165 0.430  1.00    1285.    2036.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.196  0.190 0.0666 0.0636 0.0799 0.340  1.00    1351.    2453.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable       mean median     sd    mad    q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>         <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.di~ 0.0831 0.0823 0.0541 0.0522 -0.0213 0.197  1.00    1753.    2153.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.284  0.284 0.0741 0.0704 0.137 0.430  1.00    1267.    2096.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.404  0.404 0.0735 0.0727 0.258 0.546  1.00    1307.    2059.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable   mean median     sd    mad   q2.5   q97.5  rhat ess_bulk ess_tail
        <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.119 -0.119 0.0485 0.0474 -0.214 -0.0250  1.00    5321.    2783.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.571  0.573 0.0767 0.0766 0.420 0.715  1.00    1484.    2380.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.227  0.226 0.0943 0.0920 0.0376 0.411  1.00    1661.    2561.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.344  0.344 0.0796 0.0786 0.185 0.503  1.00    1997.    2855.

# moderation of a and b, re for both

    Code
      extract.modmed.mlm.brms(fitmodab4, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.281  0.277 0.0643 0.0634 0.172 0.419  1.00    1364.    2194.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.162  0.157 0.0704 0.0673 0.0318 0.315  1.00    1971.    2547.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable      mean median     sd    mad     q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>        <dbl>  <dbl>  <dbl>  <dbl>    <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.di~ 0.120  0.119 0.0626 0.0599 -0.00473 0.247  1.00    2440.    2838.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.286  0.285 0.0694 0.0695 0.148 0.418  1.00    1516.    2693.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.391  0.392 0.0861 0.0841 0.218 0.560  1.00    2006.    2733.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable   mean median     sd    mad   q2.5  q97.5  rhat ess_bulk ess_tail
        <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.105 -0.107 0.0730 0.0721 -0.247 0.0404  1.00    2881.    2929.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.566  0.567 0.0751 0.0753 0.416 0.709  1.00    1511.    2220.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.235  0.235 0.0909 0.0898 0.0496 0.411  1.00    1824.    2251.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.331  0.329 0.0767 0.0765 0.187 0.485  1.00    2387.    2923.

