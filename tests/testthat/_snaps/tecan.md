# reading tecan csv

    Code
      plate_read_tecan(file)
    Output
      # A tibble: 11,520 x 5
         cycle_nr time_s well   od600  lumi
            <dbl>  <dbl> <chr>  <dbl> <dbl>
       1        1      0 A01   0.0459    -4
       2        1      0 A02   0.0455    -2
       3        1      0 A03   0.0456    -5
       4        1      0 A04   0.0448     3
       5        1      0 A05   0.0444   110
       6        1      0 A06   0.0452     6
       7        1      0 A07   0.0442   127
       8        1      0 A08   0.0454    -2
       9        1      0 A09   0.0458    -2
      10        1      0 A10   0.046     -3
      # i 11,510 more rows

# reading tecan xslsx

    Code
      plate_read_tecan(file)
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"chunk"` instead of `.data$chunk`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"data"` instead of `.data$data`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"cycle_nr"` instead of `.data$cycle_nr`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"value"` instead of `.data$value`
    Output
      # A tibble: 11,520 x 5
         cycle_nr time_s well   od600  lumi
            <dbl>  <dbl> <chr>  <dbl> <dbl>
       1        1      0 A01   0.0459    -4
       2        1      0 A02   0.0455    -2
       3        1      0 A03   0.0456    -5
       4        1      0 A04   0.0448     3
       5        1      0 A05   0.0444   110
       6        1      0 A06   0.0452     6
       7        1      0 A07   0.0442   127
       8        1      0 A08   0.0454    -2
       9        1      0 A09   0.0458    -2
      10        1      0 A10   0.0460    -3
      # i 11,510 more rows

