<!-- README.md is generated from README.Rmd. Please edit that file -->



```
       __ _       _      _
 _ __ / _(_) __ _| | ___| |_
| '__| |_| |/ _` | |/ _ \ __|
| |  |  _| | (_| | |  __/ |_
|_|  |_| |_|\__, |_|\___|\__|
            |___/
```

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R build status](https://github.com/richfitz/rfiglet/workflows/R-CMD-check/badge.svg)](https://github.com/richfitz/rfiglet/actions)
[![codecov.io](https://codecov.io/github/richfitz/rfiglet/coverage.svg?branch=master)](https://codecov.io/github/richfitz/rfiglet?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/richfitz/rfiglet/badge)](https://www.codefactor.io/repository/github/richfitz/rfiglet)
![works?](https://img.shields.io/badge/works-on%20my%20machine-pink)
<!-- badges: end -->


rfiglet is a pure-R implementation of [FIGlet](https://en.wikipedia.org/wiki/FIGlet) (Frank, Ian and Glenn's letters) a classic system for creating text banners in many fonts.

```
                                  ___
  .--.,                         ,--.'|_
,--.'  \   ,---.        ,---,   |  | :,'
|  | /\/  '   ,'\   ,-+-. /  |  :  : ' :  .--.--.
:  : :   /   /   | ,--.'|'   |.;__,'  /  /  /    '
:  | |-,.   ; ,. :|   |  ,"' ||  |   |  |  :  /`./
|  : :/|'   | |: :|   | /  | |:__,'| :  |  :  ;_
|  |  .''   | .; :|   | |  | |  '  : |__ \  \    `.
'  : '  |   :    ||   | |  |/   |  | '.'| `----.   \
|  | |   \   \  / |   | |--'    ;  :    ;/  /`--'  /
|  : \    `----'  |   |/        |  ,   /'--'.     /
|  |,'            '---'          ---`-'   `--'---'
`--'
```

There are many FIGlet compatible fonts; to keep things small, this package only includes the base set included by FIGlet


```
#>  * 3d_diagonal
#>  * banner
#>  * big
#>  * block
#>  * bubble
#>  * digital
#>  * ivrit
#>  * lean
#>  * mini
#>  * mnemonic
#>  * script
#>  * shadow
#>  * slant
#>  * small
#>  * smscript
#>  * smshadow
#>  * smslant
#>  * standard
#>  * term
```

However, several hundred extra fonts can be installed using

```r
rfiglet::figlet_download_fonts()
```

```
::::::.    :::. .::::::.:::::::::::::::.      :::      :::
;;;`;;;;,  `;;;;;;`    `;;;;;;;;'''';;`;;     ;;;      ;;;
[[[  [[[[[. '[['[==/[[[[,    [[    ,[[ '[[,   [[[      [[[
$$$  $$$ "Y$c$$  '''    $    $$   c$$$cc$$$c  $$'      $$'
888  888    Y88 88b    dP    88,   888   888,o88oo,.__o88oo,.__
MMM  MMM     YM  "YMmMY"     MMM   YMM   ""` """"YUMMM""""YUMMM
```

```r
remotes::install_github("richfitz/rfiglet", upgrade = FALSE)
```

## License

MIT © Richard G. FitzJohn
