# Test the Reading of Biotek Files

    Code
      head(dat)
    Output
      # A tibble: 6 x 8
         time well    lum od600 strain  concentration promoter rbs  
        <dbl> <chr> <dbl> <dbl> <chr>           <dbl> <chr>    <chr>
      1  358. A01       3 0.09  <NA>               NA <NA>     <NA> 
      2  358. A02      27 0.097 pRW0041             0 PJ23100  wk2  
      3  358. A03       4 0.091 pRW0044             0 PJ23107  wk2  
      4  358. A04       3 0.09  pRW0046             0 PJ23109  wk2  
      5  358. A05      32 0.096 pRW0047             0 PJ23110  wk2  
      6  358. A06       3 0.097 pRW0048             0 PJ23111  wk2  

# Read single wavelength

    Code
      head(dat)
    Output
      # A tibble: 6 x 7
         time well  od600 gfp_485 vio_575 vio_585 vio_595
        <dbl> <chr> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
      1  469. A01   0.041     260   0.042   0.041   0.041
      2  469. A02   0.043     258   0.044   0.044   0.043
      3  469. A03   0.043     258   0.044   0.044   0.044
      4  469. A04   0.044     254   0.044   0.044   0.044
      5  469. A05   0.044     256   0.044   0.044   0.044
      6  469. A06   0.043     255   0.044   0.044   0.044

# Read second wavelength

    Code
      head(dat)
    Output
      # A tibble: 6 x 7
         time well  od600 gfp_485_530 vio_575 vio_585 vio_595
        <dbl> <chr> <dbl>       <dbl>   <dbl>   <dbl>   <dbl>
      1  469. A01   0.041         260   0.042   0.041   0.041
      2  469. A02   0.043         258   0.044   0.044   0.043
      3  469. A03   0.043         258   0.044   0.044   0.044
      4  469. A04   0.044         254   0.044   0.044   0.044
      5  469. A05   0.044         256   0.044   0.044   0.044
      6  469. A06   0.043         255   0.044   0.044   0.044

