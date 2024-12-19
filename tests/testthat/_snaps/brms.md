# random a

    Code
      extract.modmed.mlm.brms(fit.randa, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.417  0.416 0.0396 0.0394 0.343 0.496  1.00    1904.    2675.

# random b

    Code
      extract.modmed.mlm.brms(fit.randb, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.379  0.378 0.0370 0.0367 0.311 0.454  1.00    1851.    2792.

# random a and b

    Code
      extract.modmed.mlm.brms(fit.randboth, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.452  0.449 0.0527 0.0519 0.355 0.562  1.00    3377.    3165.

# all random

    Code
      extract.modmed.mlm.brms(fit.randall, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.460  0.457 0.0545 0.0540 0.362 0.574  1.00    3595.    4071.

# moderation of a

    Code
      extract.modmed.mlm.brms(fitmoda, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.207  0.202 0.0595 0.0561 0.105 0.338  1.00    1869.    2495.

---

    Code
      extract.modmed.mlm.brms(fitmoda, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.249  0.245 0.0644 0.0622 0.140 0.392  1.00    1831.    2450.

---

    Code
      extract.modmed.mlm.brms(fitmoda, "indirect", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.207  0.202 0.0595 0.0561 0.105 0.338  1.00    1869.    2495.

# moderation of b

    Code
      extract.modmed.mlm.brms(fitmodb, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.305  0.297 0.0722 0.0676 0.183 0.466  1.00    1376.    2114.

---

    Code
      extract.modmed.mlm.brms(fitmodb, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.189  0.183 0.0569 0.0531 0.0940 0.315  1.00    1445.    2277.

---

    Code
      extract.modmed.mlm.brms(fitmodb, "indirect", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.305  0.297 0.0722 0.0676 0.183 0.466  1.00    1376.    2114.

# moderation of a and b

    Code
      extract.modmed.mlm.brms(fitmodab, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.270  0.264 0.0673 0.0653 0.153 0.414  1.00    1755.    2173.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.200  0.195 0.0573 0.0571 0.101 0.324  1.00    1841.    2783.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable       mean median     sd    mad    q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>         <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.di~ 0.0699 0.0692 0.0319 0.0316 0.00913 0.134  1.00    3093.    2994.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.285  0.287 0.0746 0.0762 0.138 0.425  1.00    1872.    2625.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.400  0.401 0.0719 0.0719 0.255 0.539  1.00    1856.    2509.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable   mean median     sd    mad   q2.5   q97.5  rhat ess_bulk ess_tail
        <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.115 -0.116 0.0485 0.0483 -0.209 -0.0225  1.00    9128.    3107.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.540  0.540 0.0818 0.0816 0.378 0.700  1.00    2026.    2436.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.210  0.208 0.0801 0.0778 0.0541 0.372  1.00    2147.    2347.

---

    Code
      extract.modmed.mlm.brms(fitmodab, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.330  0.329 0.0415 0.0410 0.249 0.412  1.00    7588.    3054.

# moderation of a and b, re for a int

    Code
      extract.modmed.mlm.brms(fitmodab2, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.289  0.282 0.0651 0.0632 0.175 0.429  1.00    1575.    2408.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.167  0.163 0.0608 0.0595 0.0624 0.295  1.00    1952.    2414.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable       mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>         <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.diff 0.121  0.119 0.0450 0.0427 0.0371 0.215  1.00    2731.    2848.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.289  0.289 0.0690 0.0692 0.152 0.418  1.00    1533.    2508.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.389  0.391 0.0846 0.0839 0.223 0.550  1.00    1741.    2579.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable    mean  median     sd    mad   q2.5  q97.5  rhat ess_bulk ess_tail
        <chr>      <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.0996 -0.0991 0.0714 0.0701 -0.237 0.0428  1.00    2846.    3004.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.538  0.537 0.0795 0.0782 0.380 0.697  1.00    1560.    2215.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.215  0.216 0.0789 0.0775 0.0594 0.368  1.00    1645.    2359.

---

    Code
      extract.modmed.mlm.brms(fitmodab2, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.323  0.322 0.0418 0.0430 0.241 0.403  1.00    6088.    3413.

# moderation of a and b, re for b int

    Code
      extract.modmed.mlm.brms(fitmodab3, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.277  0.269 0.0708 0.0677 0.159 0.433  1.00    1188.    1855.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.193  0.188 0.0658 0.0610 0.0762 0.343  1.00    1536.    2218.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable       mean median     sd    mad    q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>         <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.di~ 0.0842 0.0813 0.0557 0.0527 -0.0171 0.202  1.00    1828.    2718.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.282  0.283 0.0780 0.0786 0.128 0.434  1.00    1212.    1985.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.402  0.404 0.0746 0.0729 0.252 0.549  1.00    1244.    2255.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable   mean median     sd    mad   q2.5   q97.5  rhat ess_bulk ess_tail
        <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.120 -0.120 0.0487 0.0480 -0.215 -0.0231  1.00    4625.    2999.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.572  0.572 0.0754 0.0736 0.421 0.718  1.00    1293.    1840.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.226  0.226 0.0909 0.0894 0.0464 0.400  1.00    1610.    2264.

---

    Code
      extract.modmed.mlm.brms(fitmodab3, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.346  0.346 0.0781 0.0782 0.194 0.500  1.00    2089.    2632.

# moderation of a and b, re for both

    Code
      extract.modmed.mlm.brms(fitmodab4, "indirect")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.283  0.277 0.0638 0.0612 0.178 0.426  1.00    1217.    2245.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "indirect", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect 0.164  0.159 0.0716 0.0686 0.0377 0.314  1.00    1562.    2527.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "indirect.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable      mean median     sd    mad     q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>        <dbl>  <dbl>  <dbl>  <dbl>    <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 indirect.di~ 0.119  0.118 0.0627 0.0593 -6.82e-4 0.246  1.00    1965.    2727.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "a")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.287  0.288 0.0693 0.0676 0.150 0.425  1.00    1298.    2323.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "a", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 a        0.394  0.393 0.0867 0.0854 0.221 0.561  1.00    1623.    2346.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "a.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable   mean median     sd    mad   q2.5  q97.5  rhat ess_bulk ess_tail
        <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>    <dbl>    <dbl>
      1 a.diff   -0.106 -0.106 0.0728 0.0720 -0.253 0.0359  1.00    2485.    2941.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "b")$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.568  0.568 0.0755 0.0748 0.425 0.717  1.00    1655.    2542.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "b", modval1 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad   q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b        0.236  0.235 0.0910 0.0901 0.0590 0.414  1.00    1644.    2662.

---

    Code
      extract.modmed.mlm.brms(fitmodab4, "b.diff", modval1 = 0, modval2 = 1)$CI
    Output
      # A tibble: 1 x 10
        variable  mean median     sd    mad  q2.5 q97.5  rhat ess_bulk ess_tail
        <chr>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 b.diff   0.332  0.333 0.0777 0.0771 0.177 0.484  1.00    2020.    2693.

