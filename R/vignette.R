################################################################################
##  vignette.R
##  Copyright (C) Andrew Redd, Alun Thomas
##  
##  This file is part of the transmission R package.
##  
##  transmission is free software: you can redistribute it and/or modify it
##  under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 2 of the License, or
##  (at your option) any later version.
##  
##  transmission is distributed in the hope that it will be useful, but
##  WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with transmission.  If not, see <http://www.gnu.org/licenses/>.# 
## 
##  DESCRIPTION
##  -----------
##  Provides functions for creating PDF & HTML versions of the vignettes.
##  
################################################################################

pandoc_vignette <- function(file, ...){
    require(pander)
    Pandoc.convert(file, format='pdf', open=FALSE, footer=FALSE)
    
}


vweave = vtangle = function(file, driver, syntax, encoding = '', quiet = FALSE, ...) {
  opts_knit$set(stop_on_error = 2L)  # should not hide errors
  (if (grepl('\\.[Rr]md$', file)) knit2html else if (grepl('\\.[Rr]rst$', file)) knit2pdf else knit)(
    file, encoding = encoding, quiet = quiet, envir = globalenv()
  )
}
vweave = vtangle = function(file, driver, syntax, encoding = '', quiet = FALSE, ...) {
  opts_knit$set(stop_on_error = 2L)  # should not hide errors
  (if (grepl('\\.[Rr]md$', file)) knit2html else if (grepl('\\.[Rr]rst$', file)) knit2pdf else knit)(
    file, encoding = encoding, quiet = quiet, envir = globalenv()
  )
}

body(vtangle)[3L] = expression(purl(file, encoding = encoding, quiet = quiet))                      




