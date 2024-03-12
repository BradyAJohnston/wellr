# Reading WL from biotek .csv

    Code
      head(dat)
    Output
      # A tibble: 6 x 4
        id    wavelength well  value
        <chr>      <dbl> <chr> <dbl>
      1 1            300 A01   0.28 
      2 1            300 A02   0.284
      3 1            300 A03   0.285
      4 1            300 A04   0.285
      5 1            300 A05   0.285
      6 1            300 A06   0.284

---

    Code
      tail(dat)
    Output
      # A tibble: 6 x 4
        id    wavelength well  value
        <chr>      <dbl> <chr> <dbl>
      1 6            700 H07   0.042
      2 6            700 H08   0.042
      3 6            700 H09   0.042
      4 6            700 H10   0.042
      5 6            700 H11   0.042
      6 6            700 H12   0.042

