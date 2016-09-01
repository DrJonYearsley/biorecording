eastnorth2os <- function(EN, system='OSGB', format='grid') {
  # A function to convert from eastings and northings to a grid reference (units of metres)
  # The function works with UK and Irish grid references
  # Grid refs in the Channel Islands are not included
  #
  # Input arguments:
  #    EN      an array of eastings and northings (two columns, column 1=eastings, column 2 = northings)
  #    system  the grid reference system to use: 'OSGB' (default) for the GB grid, 'OSI' for the Irish grid
  #    format  the output format to use: 'grid' for a standard grid reference, 'hectad' for the relevant hectad, 
  #           'tetrad' for the relevant tetrad
  #
  # Output: 
  #    GR      all unique grid references
  #    system  the grid reference system
  #
  # Example: Convert a set of eatsings and northings (in variable input_en) to grid references
  #
  # input_en
  #        eastings northings
  #  [1,]    23000     53000
  #  [2,]   235600    319400
  #  [3,]   326000    245000
  #  [4,]   276000    239000
  #
  # These correspond to a mix of GB and Irish grid references
  # system = c('OSI','OSGB','OSI','OSI')
  #
  # out = eastnorth2os(input_en, system)
  # 
  # Sample output:
  # out$GR
  # [1] "V2300053000"  "SH3560019400" "O2600045000"  "N7600039000" 
  # 
  # out$system
  # [1] "OSI"  "OSGB" "OSI"  "OSI" 
  #
  # Output these grid references as tetrads
  #
  # out = eastnorth2os(input_en, system, format='tetrad')
  # 
  # Sample output:
  # out$GR
  # [1] "V25G"  "SH31P" "O24S"  "N73U" 
  # 
  # out$system
  # [1] "OSI"  "OSGB" "OSI"  "OSI" 
  #
  #
  # Written by Jon Yearsley (jon.yearsley@ucd.ie)
  #
  # Last modified: Aug 2016
  
  # Number of digits for the grid reference
  n.digits = 5   # 5 digits is to nearest m
  
  # Define the OS grid letters for Ireland
  OS = t(array(c('A','B','C','D','E','F','G','H','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'),dim=c(5,5)))

  ostmp = cbind(
    rbind(array(paste('H', OS, sep=''),dim=c(5,5)),
          array(paste('N', OS, sep=''),dim=c(5,5)),
          array(paste('S', OS, sep=''),dim=c(5,5))), 
    rbind(array(paste('J', OS, sep=''),dim=c(5,5)),
          array(paste('O', OS, sep=''),dim=c(5,5)),
          array(paste('T', OS, sep=''),dim=c(5,5))))
  
  OSGB = ostmp[15:3,1:7] # Flip so that the SV square has indices 0,0
  OSIR = OS[5:1,1:5] # Flip so that the V square has indices 0,0
  
  tetrad = array(c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','P','Q','R','S','T','U','V','W','X','Y','Z'),dim=c(5,5))

  GB.grid = grepl('OSGB',system, ignore.case=T) # True if using OSGB grid reference
    
  # Ensure eastings and northings correspond to grid system information
  # Round all eastings and northings to the nearest 1m
  EN = round(EN)
  if (length(EN)==2) {
    eastings = EN[1]
    northings = EN[2]
    
    if (length(GB.grid)!=1) {warning('The argument \'system\' has more values than number of coordinates')}
    
  } else {
    eastings = EN[,1]
    northings = EN[,2]
    if (length(GB.grid)==1) {GB.grid = rep(GB.grid, times=nrow(EN))}
    if (length(GB.grid)!=nrow(EN)) {
      warning('The argument \'system\' is not the same length as the number of coordinates. Using only the first value of \'system\'')
      GB.grid = rep(GB.grid[1],times=nrow(EN))
      }
  }

  letter.prefix = rep(NA,times=length(GB.grid))
  # Use GB letter prefix if appropriate
  letter.prefix[GB.grid] = OSGB[floor(northings[GB.grid]/10^5)+1+ nrow(OSGB)*floor(eastings[GB.grid]/10^5)]
  # Otherwise use irish letter prefix
  letter.prefix[!GB.grid] = OSIR[floor(northings[!GB.grid]/10^5)+1+ nrow(OSIR)*floor(eastings[!GB.grid]/10^5)]

  # Pad grid reference with zeros
  GR.part1 = gsub(' ','0',format(eastings-floor(eastings/10^5)*10^5, width=n.digits))
  GR.part2 = gsub(' ','0',format(northings-floor(northings/10^5)*10^5, width=n.digits))
  
  # Give output at a specified resolution (default = m, which is 5 digits per coordinate)
  width = 5
  pref=''
  if (grepl('(tetrad|2km|2 km)', format, ignore.case=T)) {
    width = 1
    ind = ceiling(as.numeric(substr(GR.part2,start=2,stop=2))/2) + nrow(tetrad)*floor(as.numeric(substr(GR.part1,start=2,stop=2))/2)
    pref = tetrad[ind] 
  } 
  if (grepl('(hectad|10km|10 km)',format, ignore.case=T)) { 
    width = 1
    }
  if (grepl('(1km|1 km)', format,ignore.case=T)) { 
    width = 2
  }
  if (grepl('(100m|100 m)',format, ignore.case=T)) { 
    width = 3
  }
  if (grepl('(10m|10 m)',format, ignore.case=T)) { 
    width = 4
  }
  if (grepl('(grid|1m|1 m)',format, ignore.case=T)) { 
    width = 5
  }
  GR.part1 = strtrim(GR.part1,width)
  GR.part2 = strtrim(GR.part2,width)
  GR = paste(letter.prefix, GR.part1, GR.part2,pref,sep='')


  return(list(GR=GR, system=system))  
}
