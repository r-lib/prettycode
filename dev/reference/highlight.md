# Syntax highlight R code

Syntax highlight R code

## Usage

``` r
highlight(code, style = default_style())
```

## Arguments

- code:

  Character vector, each element is one line of code.

- style:

  Style functions, see
  [`default_style()`](https://r-lib.github.io/prettycode/dev/reference/default_style.md).

## Value

Character vector, the highlighted code.

## Examples

``` r
highlight(deparse(ls))
#>  [1] "\033[31mfunction\033[39m \033[33m(\033[39mname, pos \033[32m=\033[39m \033[32m-\033[39m\033[34m1L\033[39m, envir \033[32m=\033[39m \033[36mas.environment\033[39m\033[34m(\033[39mpos\033[34m)\033[39m, all.names \033[32m=\033[39m \033[34mFALSE\033[39m, "                                                       
#>  [2] "    pattern, sorted \033[32m=\033[39m \033[34mTRUE\033[39m\033[33m)\033[39m "                                                                                                                                                                                                                                      
#>  [3] "\033[33m{\033[39m"                                                                                                                                                                                                                                                                                                 
#>  [4] "    \033[31mif\033[39m \033[34m(\033[39m\033[32m!\033[39m\033[36mmissing\033[39m\033[36m(\033[39mname\033[36m)\033[39m\033[34m)\033[39m \033[34m{\033[39m"                                                                                                                                                         
#>  [5] "        pos \033[32m<-\033[39m \033[36mtryCatch\033[39m\033[36m(\033[39mname, error \033[32m=\033[39m \033[31mfunction\033[39m\033[33m(\033[39me\033[33m)\033[39m e\033[36m)\033[39m"                                                                                                                              
#>  [6] "        \033[31mif\033[39m \033[36m(\033[39m\033[36minherits\033[39m\033[33m(\033[39mpos, \033[33m\"error\"\033[39m\033[33m)\033[39m\033[36m)\033[39m \033[36m{\033[39m"                                                                                                                                           
#>  [7] "            name \033[32m<-\033[39m \033[36msubstitute\033[39m\033[33m(\033[39mname\033[33m)\033[39m"                                                                                                                                                                                                              
#>  [8] "            \033[31mif\033[39m \033[33m(\033[39m\033[32m!\033[39m\033[36mis.character\033[39m\033[34m(\033[39mname\033[34m)\033[39m\033[33m)\033[39m "                                                                                                                                                             
#>  [9] "                name \033[32m<-\033[39m \033[36mdeparse\033[39m\033[33m(\033[39mname\033[33m)\033[39m"                                                                                                                                                                                                             
#> [10] "            \033[36mwarning\033[39m\033[33m(\033[39m\033[36mgettextf\033[39m\033[34m(\033[39m\033[33m\"%s converted to character string\"\033[39m, "                                                                                                                                                               
#> [11] "                \033[36msQuote\033[39m\033[36m(\033[39mname\033[36m)\033[39m\033[34m)\033[39m, domain \033[32m=\033[39m \033[34mNA\033[39m\033[33m)\033[39m"                                                                                                                                                       
#> [12] "            pos \033[32m<-\033[39m name"                                                                                                                                                                                                                                                                           
#> [13] "        \033[36m}\033[39m"                                                                                                                                                                                                                                                                                         
#> [14] "    \033[34m}\033[39m"                                                                                                                                                                                                                                                                                             
#> [15] "    all.names \033[32m<-\033[39m \033[36m.Internal\033[39m\033[34m(\033[39m\033[36mls\033[39m\033[36m(\033[39menvir, all.names, sorted\033[36m)\033[39m\033[34m)\033[39m"                                                                                                                                          
#> [16] "    \033[31mif\033[39m \033[34m(\033[39m\033[32m!\033[39m\033[36mmissing\033[39m\033[36m(\033[39mpattern\033[36m)\033[39m\033[34m)\033[39m \033[34m{\033[39m"                                                                                                                                                      
#> [17] "        \033[31mif\033[39m \033[36m(\033[39m\033[33m(\033[39mll \033[32m<-\033[39m \033[36mlength\033[39m\033[34m(\033[39m\033[36mgrep\033[39m\033[36m(\033[39m\033[33m\"[\"\033[39m, pattern, fixed \033[32m=\033[39m \033[34mTRUE\033[39m\033[36m)\033[39m\033[34m)\033[39m\033[33m)\033[39m \033[32m&&\033[39m "
#> [18] "            ll != \033[36mlength\033[39m\033[33m(\033[39m\033[36mgrep\033[39m\033[34m(\033[39m\033[33m\"]\"\033[39m, pattern, fixed \033[32m=\033[39m \033[34mTRUE\033[39m\033[34m)\033[39m\033[33m)\033[39m\033[36m)\033[39m \033[36m{\033[39m"                                                                   
#> [19] "            \033[31mif\033[39m \033[33m(\033[39mpattern \033[32m==\033[39m \033[33m\"[\"\033[39m\033[33m)\033[39m \033[33m{\033[39m"                                                                                                                                                                               
#> [20] "                pattern \033[32m<-\033[39m \033[33m\"\\\\[\"\033[39m"                                                                                                                                                                                                                                              
#> [21] "                \033[36mwarning\033[39m\033[34m(\033[39m\033[33m\"replaced regular expression pattern '[' by  '\\\\\\\\['\"\033[39m\033[34m)\033[39m"                                                                                                                                                              
#> [22] "            \033[33m}\033[39m"                                                                                                                                                                                                                                                                                     
#> [23] "            \033[31melse\033[39m \033[31mif\033[39m \033[33m(\033[39m\033[36mlength\033[39m\033[34m(\033[39m\033[36mgrep\033[39m\033[36m(\033[39m\033[33m\"[^\\\\\\\\]\\\\[<-\"\033[39m, pattern\033[36m)\033[39m\033[34m)\033[39m\033[33m)\033[39m \033[33m{\033[39m"                                             
#> [24] "                pattern \033[32m<-\033[39m \033[36msub\033[39m\033[34m(\033[39m\033[33m\"\\\\[<-\"\033[39m, \033[33m\"\\\\\\\\\\\\[<-\"\033[39m, pattern\033[34m)\033[39m"                                                                                                                                         
#> [25] "                \033[36mwarning\033[39m\033[34m(\033[39m\033[33m\"replaced '[<-' by '\\\\\\\\[<-' in regular expression pattern\"\033[39m\033[34m)\033[39m"                                                                                                                                                        
#> [26] "            \033[33m}\033[39m"                                                                                                                                                                                                                                                                                     
#> [27] "        \033[36m}\033[39m"                                                                                                                                                                                                                                                                                         
#> [28] "        \033[36mgrep\033[39m\033[36m(\033[39mpattern, all.names, value \033[32m=\033[39m \033[34mTRUE\033[39m\033[36m)\033[39m"                                                                                                                                                                                    
#> [29] "    \033[34m}\033[39m"                                                                                                                                                                                                                                                                                             
#> [30] "    \033[31melse\033[39m all.names"                                                                                                                                                                                                                                                                                
#> [31] "\033[33m}\033[39m"                                                                                                                                                                                                                                                                                                 
cat(highlight(deparse(ls)), sep = "\n")
#> function (name, pos = -1L, envir = as.environment(pos), all.names = FALSE, 
#>     pattern, sorted = TRUE) 
#> {
#>     if (!missing(name)) {
#>         pos <- tryCatch(name, error = function(e) e)
#>         if (inherits(pos, "error")) {
#>             name <- substitute(name)
#>             if (!is.character(name)) 
#>                 name <- deparse(name)
#>             warning(gettextf("%s converted to character string", 
#>                 sQuote(name)), domain = NA)
#>             pos <- name
#>         }
#>     }
#>     all.names <- .Internal(ls(envir, all.names, sorted))
#>     if (!missing(pattern)) {
#>         if ((ll <- length(grep("[", pattern, fixed = TRUE))) && 
#>             ll != length(grep("]", pattern, fixed = TRUE))) {
#>             if (pattern == "[") {
#>                 pattern <- "\\["
#>                 warning("replaced regular expression pattern '[' by  '\\\\['")
#>             }
#>             else if (length(grep("[^\\\\]\\[<-", pattern))) {
#>                 pattern <- sub("\\[<-", "\\\\\\[<-", pattern)
#>                 warning("replaced '[<-' by '\\\\[<-' in regular expression pattern")
#>             }
#>         }
#>         grep(pattern, all.names, value = TRUE)
#>     }
#>     else all.names
#> }
```
