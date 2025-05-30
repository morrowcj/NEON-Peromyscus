# Filename: ibutton_download_launch_combined.R
# 
# Author: Luke Miller  Jul 11, 2012 (as modified by Ryan Knowles Oct 04, 2012)
###############################################################################
# This script will allow the user to download iButtons, giving a unique name 
# to each file. It will parse the raw data file and extract the useful bits
# (to me at least): date/time, temperature (Celsius), user's ID for this 
# iButton, and the serial number for this iButton, and place all of these in a
# comma-separated-value file that can be easily opened in Excel or R. Each 
# downloaded iButton data file has the current date and time included in the 
# filename to avoid overwriting older data files. Raw data files are stored in a 
# directory with today's date on it, while the parsed csv files go in a 2nd 
# directory.

# The thermodl.exe file was originally downloaded as part of the Maxim iButton 
# 1-Wire Public Domain Kit. 
# There are several versions of the Kit available, including
# versions with pre-compiled binaries (executables) for Windows/Linux/OSX.
# http://www.maxim-ic.com/products/ibutton/software/1wire/wirekit.cfm
# On my Windows 7 x64 computer using the DS9490B USB ibutton adapter, I used the
# precompiled binary build for Win64 USB (DS9490 + WinUSB) Preliminary Version 
# 3.11 Beta 2,
# filename: winusb64vc311Beta2_r2.zip, downloaded 2012-03-15
# Unzip this file and find the .exe files thermoms.exe and thermodl.exe in the
# builds\winusb64vc\release folder. Copy these to your R working directory.
# The drivers for the DS9490 USB iButton adapter must also be downloaded and 
# installed: 
# http://www.maxim-ic.com/products/ibutton/software/tmex/
# I downloaded and installed the file "install_1_wire_drivers_x64_v403.msi"
# The Java OneWireViewer app can also be downloaded and used to verify that your
# drivers work and that you can communicate with iButtons successfully through 
# the USB adapter. You can download this app here: 
# http://www.maxim-ic.com/products/ibutton/software/1wire/OneWireViewer.cfm


# Notes - Alli Brehm (7/7/22) - the above link for downloading drivers
# no longer works. Instead I found them at the following :
# https://www.maximintegrated.com/en/products/ibutton-one-wire/one-wire/software-tools/drivers/download-1-wire-ibutton-drivers-for-windows.html



# NOTE: the Maxim program "thermodl.exe" must be present in the current R 
# working directory. Uncomment the setwd() line below to change the R working 
# directory. Enter your working directory location inside the quotes.
setwd('/Users/brehm3/Desktop') 

cur.date = Sys.Date() # Get current date
# Assemble a directory name to store raw downloaded data into
#dir.name = "/Users/brehm3/Desktop/iButton"
dir.name = paste('.\\',as.character(cur.date), '_dump',sep = '')

# Assemble a directory name to store the parsed csv output files in
#dir.name2 = "C:/Users/John Orrock/R/parsed data"
dir.name2 = paste('.\\',as.character(cur.date), '_parsed',sep = '')
# Check to see if that folder already exists, if not then create it.
if (is.na(file.info(dir.name)$isdir)) {
  dir.create(dir.name)	
}
# Check to see if the other folder already exists, if not then create it.
if (is.na(file.info(dir.name2)$isdir)) {
  dir.create(dir.name2)	
}

#######################################
# This main while loop will repeat continuously to download data from
# multiple iButtons.

Counter = 0
loop = TRUE
while(loop) {
  Counter = Counter + 1
  # Get current time to insert in filename so we don't overwrite old data
  currTime = strftime(Sys.time(), format = "%Y%m%d_%H%M%S")
  # Assemble filename
#  fname = paste(dir.name,'/',currTime,'.dat', sep = '')
  fname = paste(dir.name,'\\',currTime,'.dat', sep = '')
  # Call the thermodl.exe program provided by Maxim. This must be in the 
  # current R working directory. Start by assembling a command to be issued
  sys.com = paste('thermodl.exe ds2490-0 ', fname, sep = '')
  # Now issue the command to the system using the system() function
  temp = system(sys.com, wait = TRUE, intern = TRUE)
  # The raw downloaded data should now be in the "raw_downloads" directory 
  # created at the start of the script.
 
   # Output the status updates from thermodl.exe 
  cat(temp[7], '\n')
  cat(temp[9],'\n')
  cat(temp[18], '\n')
  cat(temp[20], '\n')
  # Open the data file created by thermodl.exe
  x <- readLines(fname) # read data into a character vector
  # Extract serial number from line 4 of file
  serialnum <- substr(x[4], 26, 43)
  
  # Parse the iButton data file to extract the relevant temp/time data
  log.start <- grep('Log Data', x) # Find what line the log data starts below
  log.start <- log.start + 3 # change value to first line of log data
  log.end <- grep('Debug Dump', x) # Find where the log data ends (roughly)
  log.end <- log.end - 2 # change value to last line of log data
  # Check if there were any logged temperature data
  if (!(log.end - log.start < 0)) {
    temps <- x[log.start:log.end] # extract log data, still as characters
    
    # Convert time stamps to POSIX object
    times <- as.POSIXct(strptime(substr(temps,1,17), 
                                 format = '%m/%d/%Y %H:%M'))
    temp.data <- data.frame(Time = times) # stick into a new data frame
    # convert temps to numeric and insert into temp.data
    temp.data$TempC <- as.numeric(substr(temps,20,26))
    # Insert column with iButton's unique serial number
    temp.data$Serial.number <- serialnum
    # Output temperature data to console
    
    if (Counter == 1) { # Copy times and temps for first ibutton
      # The times and number of readings of the first ibutton
      # will dictate how much is read from each subsequent button.
      # For example, if the first ibutton has 100 readings and the
      # second has 120 readings, only the first 100 readings of the
      # second ibutton will be saved in the combined csv file.
      # The remaining data will still be available in the .dat file.
      combdatah <- as.data.frame(temp.data$Time)
      names(combdatah)[1] <- "Time"
      temprow <- nrow(combdatah)
    } # End of first button IF
    # Only copy temps for all subsequent ibuttons (add *'s to ibuttons with
    # times that don't line up with the first ibutton)
    Colcounter <- Counter + 1
    Checkdiff = temp.data[1,1] # Extract time
    if(Checkdiff != combdatah[1,1]) {
      serialnum<-paste(serialnum,"*",sep="") # Add * to serialnum
    } 
    # Add temperature data to combined data frame
    combdatah[Colcounter] <- temp.data$TempC[1:temprow]
    # Rename new temp data with serial num.
    names(combdatah)[Colcounter] <- serialnum 
    
    cat('Temperature data downloaded \n')
  } else { # Continue logged data check IF
    cat('\n\n*****No temperature data*****\a\n\n')
    cat('Would you still like to launch this iButton?\n')
    cat('Doing so will erase any data that has not been downloaded.\n')
    cat('Enter y to continue.\n')
    cat('Leave blank to skip launch and (re)download ibutton.\n')
    # Ask user to continue
    cont.launch = scan(file = '', what = character(), n = 1) 
    if (length(cont.launch) > 0) { # Allows user to not type anything
      if (cont.launch == 'y') launch = TRUE
    } else {launch = FALSE}
    # Cancel launch if entry is anything but 'y'
  } # End of logged data check IF
    
  cat('\a\n---------------------\n')
  cat('Swap in next iButton and hit enter. Enter q to quit.\n')
  user.input = scan(file = '', what = character(), n = 1)
  if (length(user.input) > 0) { # Allows user to not type anything
    if (user.input == 'q') loop = FALSE # quit out of while loop
  } else {
    loop = TRUE # return to start of while loop to download next iButton
  } # End of "q" IF
  
} # End of main loop



# Write the contents of combdatah to a comma-separated-values file.
write.csv(combdatah, "horizontal.csv", row.names = FALSE)

cat('Finished\n')

