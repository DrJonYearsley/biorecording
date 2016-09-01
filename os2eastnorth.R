os2eastnorth <- function(GR, hectad=FALSE) {
  # A function to convert from a grid reference (letter format) to eastings and northings
  # The function works with GB and Irish grid references
  # Grid refs in the Channel Islands are not included
  #
  # To reduce the computational time for large lists, all unique grid references in the 
  # GR array are found and converted. The output then gives an index (index) that points
  # to the relevant output for each element of the input array (see example below)
  #
  # Input arguments:
  #    GR      an array of grid references, formatted as character strings (e.g. 'V234834')
  #    hectad  = TRUE  the eastings and northings will be rounded to the nearest 10km and 
  #                    given in units of 10km
  #            = FALSE (default) eastings and northings given to the accuracy of the 
  #                    original grid reference in units of metres
  #
  # Output: 
  #    index      array index pointing to the relevant output in 'names', 'en' and 'country' for each element of GR
  #    gridref    all unique grid references
  #    en         eastings (1st column) and northings (2nd column) for all the grid references in 'names'
  #    system     either 'OSI' =  Ireland or 'OSGB'=Great Britain
  #    resolution the resolution of the original grid reference (1m, 10m, 100m, 1km, 2km, 10km)
  #    units      ='metres' when hectad=FALSE, ='hectad' when hectad=TRUE
  #
  # Example: Convert a set of 7 grid references of varying accuracy (4 Irish and 3 GB).
  # Grid reference 'O2645' is repeated twice (its output index is 3)
  #
  # gridrefs = c('V2353','SH356194', 'O2645','N7639','O2645', 'NX35V', 'HY2351278945')
  # out = os2eastnorth(gridrefs, hectad=F)
  # 
  # Sample output:
  # out$index
  # [1] 1 2 3 4 3 5 6
  # 
  # out$gridref
  # [1] "V2353"        "SH356194"     "O2645"        "N7639"        "NX35V"        "HY2351278945"
  # 
  # out$en
  #      eastings northings
  # [1,]    23000     53000
  # [2,]   235600    319400
  # [3,]   326000    245000
  # [4,]   276000    239000
  # [5,]   238000    550000
  # [6,]   323512   1078945
  # 
  # out$system
  # [1] "OSI"  "OSGB" "OSI"  "OSI"  "OSGB" "OSGB"
  # 
  # out$units
  # [1] "metres"
  # 
  # out$resolution
  # [1] "1km"          "100m"         "1km"          "1km"          "2km (tetrad)" "1m"          
  # 
  # 
  # Written by Jon Yearsley (jon.yearsley@ucd.ie)
  #
  # Last modified: Aug 2016
  
  # Define the OS grid letters for Ireland
  OS = t(array(c('A','B','C','D','E','F','G','H','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'),dim=c(5,5)))
  # define default scale
  scale = 'metres'
  
  ostmp = cbind(
    rbind(array(paste('H', OS, sep=''),dim=c(5,5)),
          array(paste('N', OS, sep=''),dim=c(5,5)),
          array(paste('S', OS, sep=''),dim=c(5,5))), 
    rbind(array(paste('J', OS, sep=''),dim=c(5,5)),
          array(paste('O', OS, sep=''),dim=c(5,5)),
          array(paste('T', OS, sep=''),dim=c(5,5))))
  
  OSUK = ostmp[15:3,1:7] # Flip so that the SV square has indices 0,0
  OSIR = OS[5:1,1:5] # Flip so that the V square has indices 0,0
  
  tetrad = array(c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','P','Q','R','S','T','U','V','W','X','Y','Z'),dim=c(5,5))
  
  tmp = as.character(GR) # Ensure grid refs in data are of type character
  
  gr.list = unique(tmp)  # Create a list of all unique grid references
  gr.ref = match(tmp,gr.list) # Link each grid reference to a unique value

  system = rep(NA, times=length(gr.list))
  system[grepl('^[[:alpha:]]{2}[[:digit:]]{2}',gr.list)] = 'OSGB'   # Find UK grid references
  system[grepl('^[[:alpha:]]{1}[[:digit:]]{2}',gr.list)] = 'OSI'   # Find Irish grid references
  
  tetrad.GR = rep(NA, times=length(gr.list))
  tetrad.GR[grepl('[[:digit:]]{2}[[:alpha:]]{1}$',gr.list)] = TRUE
  tetrad.GR[grepl('[[:digit:]]{2}$',gr.list)] = FALSE
  
  # Count number of digits in the grid reference
  n.digits = nchar(gr.list)
  n.digits[tetrad.GR] = n.digits[tetrad.GR] - 1
  n.digits[system=='OSGB'] = n.digits[system=='OSGB'] - 2
  n.digits[system=='OSI'] = n.digits[system=='OSI'] - 1
  
  if (any(n.digits%%2!=0)) {
    # Check that all grid references have an even number of digits
    warning('Some grid references have an odd number of digits')
  }
  
  valid.ref = rep(TRUE, times=length(gr.list))
  valid.ref[is.na(system)] = FALSE
  valid.ref[is.na(tetrad.GR)] = FALSE

  if (any(!valid.ref)) {
    warning('Some grid references were not recognised as valid Irish or GB grid references')
  }
  
  letter.code = rep(NA, times=length(gr.list))
  letter.code[system=='OSGB'] = strtrim(gr.list[system=='OSGB'], width=2)
  letter.code[system=='OSI'] = strtrim(gr.list[system=='OSI'], width=1)

  if (any(!(letter.code%in%OSGB | letter.code%in%OSIR)) ) {
    warning('Some letter codes do not give valid grid references')
  }
    
  # Extract the numbers from the grid reference and the tetrad letter
  digits = rep(NA, times=length(gr.list))
  digits[system=='OSGB'] = substring(gr.list[system=='OSGB'], first=3)
  digits[system=='OSI'] = substring(gr.list[system=='OSI'], first=2)

  tetrad.code = rep(NA, times=length(gr.list))
  tetrad.code[tetrad.GR] = substring(digits[tetrad.GR], first=n.digits[tetrad.GR]+1)
  digits[tetrad.GR] = strtrim(digits[tetrad.GR], width=n.digits[tetrad.GR])
  
  if (any(n.digits!=2 & tetrad.GR)) {
    warning('Some tetrad grid references with too many digits')
  }
  
  # Calculate Eastings and Northings in meters
  fac = 10^(5-n.digits/2)
  east  = as.numeric(substring(digits, first=1, last=n.digits/2))*fac
  north  = as.numeric(substring(digits, first=n.digits/2+1))*fac
  
  # Add on coordinates for the 100km grid letter
  pref = array(NA, dim=c(length(gr.list),2))
  tmp = match(letter.code[system=='OSGB'],OSUK) # Find letter code position in the matrix OSUK (UK)
  pref[system=='OSGB', ] = c(floor((tmp-1)/nrow(OSUK)), (tmp-1)%%nrow(OSUK))

  tmp = match(letter.code[system=='OSI'],OSIR) # Find letter code position in the matrix OSIR (Ireland)
  pref[system=='OSI',] = c(floor((tmp-1)/nrow(OSIR)), (tmp-1)%%nrow(OSIR))
  
  # Add on tetrad if applicable
  tetrad.add = array(0, dim=c(length(gr.list),2))
  tmp = match(tetrad.code[tetrad.GR], tetrad) # Find letter code position in the matrix tetrad
  tetrad.add[tetrad.GR, ] = c(floor((tmp-1)/nrow(tetrad)), (tmp-1)%%nrow(tetrad)) * 2e3
  
  # Find Eatsings and Northings (units meters)
  eastnorth = array(NA,dim=c(length(gr.list),2))  # Define an array to hold eastings and northings
  eastnorth[,1] = 10^5*pref[,1] + east + tetrad.add[,1]
  eastnorth[,2] = 10^5*pref[,2] + north + tetrad.add[,2]
  
  # Name the columns of eastnorth
  colnames(eastnorth) <- c('eastings','northings')
  
  if (hectad) {
    # Round down to nearest hectad and express in units of 10km
    eastnorth = floor(eastnorth/10^4)
    scale='hectad (10km)'
  }
  
  resolution = rep(NA, times=length(gr.list))
  resolution[tetrad.GR] = '2km (tetrad)'
  resolution[!tetrad.GR & n.digits==10] = '1m'
  resolution[!tetrad.GR & n.digits==8] = '10m'
  resolution[!tetrad.GR & n.digits==6] = '100m'
  resolution[!tetrad.GR & n.digits==4] = '1km'
  resolution[!tetrad.GR & n.digits==2] = '10km (hectad)'
  
  return(list(index=gr.ref, gridref=gr.list, en=eastnorth, system=system, units=scale, resolution=resolution))  
}
