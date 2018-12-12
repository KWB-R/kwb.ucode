# ucConf(): Configure UCODE run ------------------------------------------------
ucConf <- function # Configure UCODE run
### Configure UCODE run
(
  general = ucConfGeneral(), 
  ### configuration of control parameters as retrieved by \code{\link{ucConfGeneral}}.
  parameters = ucDefaultConfParameters(),
  ### data.frame with each row representing a parameter configuration as 
  ### e.g. retrieved by \code{\link{ucConfParameter}}. For possible parameter
  ### names see \code{\link{ucSetPlaceholder}}
  weights = ucDefaultConfWeights()
  ### Matrix of weights as e.g. retrieved by \code{\link{ucDefaultConfWeights}}. 
  ### Matix of weights is used when calculating overall fitness. 
  ### Row names = names of fitness indicators out of the set of c("ME", "MAE", 
  ### "MSE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE",
  ### "d", "md", "rd", "cp", "r", "R2", "bR2", "KGE, "VE"). Column names = well
  ### names ("PW" = pumping well, observation wells named as in parameter 
  ### setting). See example.
)
{
  ##seealso<< \code{\link{ucPrepCalib}}  
  list(general = general, parameters = parameters, weights = weights)
}

# ucConfGeneral() --------------------------------------------------------------
ucConfGeneral <- function # general settings in UCODE configuration
### general settings in UCODE configuration
(
  gof.digits = 5,
  ### Number of digits to be used in goodness of fit values. Default: 5
  maxIter = 100
  ### Maximum number of parameter-estimation iterations allowed before stopping  
)
{
  ##seealso<< \code{\link{ucConf}}    
  list(gof.digits = gof.digits, maxIter = maxIter)
}

# ucConfParameter() ------------------------------------------------------------------
ucConfParameter <- structure(function # Create parameter for UCODE configuration
### Create parameter for UCODE configuration
(
  paramname,
  ### Parameter name (up to 12 characters; not case sensitive) - a character 
  ### string that is used in a template file or in an equation of a derived 
  ### parameter in the Derived_Parameters input block. Each parameter name 
  ### needs to be unique and can not be the same as any parameter name defined 
  ### in the Derived_Parameter input block. For possible parameter
  ### names see \code{\link{ucSetPlaceholder}}
  startvalue = 1e38,
  ### Starting parameter value. Default=A huge real number. The huge real number
  ### is obtained for the computer being used and commonly is about 1e38.
  lowerValue = -1e38,
  ### Smallest reasonable value for this parameter. Default= -(Huge real
  ### number). In absolute value, commonly about -1e38.
  upperValue = 1e38,
  ### Largest reasonable value for this parameter. Default= +(Huge real number).
  ### Commonly about +1e38.
  lowerConstraint = 0,
  ### Lower limit of considered parameter values. Default (HS): 0
  upperConstraint = 1,
  ### Upper limit of considered parameter values. Default (HS): 1
  constrain = "yes",
  ### yes: constrain parameter values using LowerConstraint and UpperConstraint.
  ### Default (HS): yes.
  adjustable = "yes",
  ### yes: change this value as needed depending on the purpose of the
  ### UCODE_2005 run defined in the UCODE_Control_Data input file. no: leave the
  ### value of this parameter unchanged. Default (HS): "yes".
  maxChange = 2.0,
  ### Maximum fractional parameter change allowed between parameter iterations.
  ### Default=2.0.
  perturbAmt = 0.5
  ### Fractional amount of parameter value to perturb to calculate sensitivity.
  ### Commonly 0.01 to 0.10. Default (HS): 0.5. See discussion in Chapter 3.  
)
{
  ##seealso<< \code{\link{ucConf}, \link{ucDefaultConfParameters}}    
  data.frame(
    paramname = paramname,
    startvalue = startvalue,
    lowerValue = lowerValue,
    upperValue = upperValue,
    lowerConstraint = lowerConstraint,
    upperConstraint = upperConstraint,
    constrain = constrain,
    adjustable = adjustable,
    maxChange = maxChange,
    perturbAmt = perturbAmt, stringsAsFactors = FALSE)
  
  ### data.frame containing all argument values with column names equalling 
  ### function argument names
}, ex = function()
{
  # Use rbind to create a data.frame containing parameter information as
  # required by ucConf:
  cpara <- rbind(
    ucConfParameter(paramname = "HKR", startvalue = 2e-5, 
                    lowerConstraint = 1e-5, upperConstraint = 1e-1,
                    maxChange = 1, perturbAmt = 0.5, 
                    constrain = "yes", adjustable = "yes"),
    ucConfParameter(paramname = "HKZ", startvalue = 2e-7, 
                    lowerConstraint = 1e-7, upperConstraint = 1e-3,
                    maxChange = 1, perturbAmt = 0.5,
                    constrain = "yes", adjustable = "yes"))  
  
  # set this parameter setting in default UCODE configuration
  ucConf(parameters = cpara)
})

# ucDefaultConfParameters() ---------------------------------------------------
ucDefaultConfParameters <- function
### default parameter definition for UCODE configuration
(
) 
{
  ##seealso<< \code{\link{ucConfParameter}, \link{ucConf}}
  rbind(ucConfParameter(paramname = "HKR", 
                        startvalue = 2e-5, 
                        lowerConstraint = 1e-5, 
                        upperConstraint = 1e-1,
                        constrain = "yes", 
                        adjustable = "yes", 
                        maxChange = 1, 
                        perturbAmt = 0.5),
        ucConfParameter(paramname = "HKZ", 
                        startvalue = 2e-7, 
                        lowerConstraint = 1e-7, 
                        upperConstraint = 1e-3,
                        constrain = "yes",
                        adjustable = "yes",
                        maxChange = 1,
                        perturbAmt = 0.5))

  ### data.frame with each row representing a model parameter
}

# ucDefaultConfWeights() ------------------------------------------------------------
ucDefaultConfWeights <- function
### default weight matrix. Weights are given for each pair of well name 
### (pumping well \emph{PW} or observation wells) and name of goodness of fit 
### (gof)-function. 
(
  wells = wtConfiguredWellnames(wtDefaultConfiguration()),
  gofs  = c("RMSE", "NSE", "mNSE")
)  
{
  ##seealso<< \code{\link{ucConf}}  
  matrix(rep(1, times = length(wells) * length(gofs)),
         nrow = length(gofs), 
         dimnames = list(gofs, wells))
  ### Matix of weights used when calculating overall fitness. 
  ### Row names = names of fitness indicators out of the set of c("ME", "MAE", 
  ### "MSE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE",
  ### "d", "md", "rd", "cp", "r", "R2", "bR2", "KGE, "VE"). Column names = well
  ### names ("PW" = pumping well, observation wells named as in parameter 
  ### setting). 
}

# ucTable() --------------------------------------------------------------------
ucTable <- function
### creates lines to be put in TABLE-data block of UCODE input file representing
### data in given data frame
(
  dat
  ### data frame
) 
{
  # helper function to generate one line with elements in flist, considering
  # column widths given in cwidth  
  .datline <- function(flist, cwidth) {
    flist <- as.list(flist)
    cells <- c()
    for (i in 1:length(flist)) {
      fmt <- sprintf("%%%ds", cwidth[i])
      # convert to character as list elements may be factors!
      cells <- c(cells, sprintf(fmt, as.character(flist[[i]])))
    }
    paste(cells, collapse = " ")
  }  
  
  cwidth <- numeric()
  cnames <- names(dat)
  for (i in 1:ncol(dat)) {
    cwidth[i] <- max(nchar(cnames[i]), nchar(as.character(dat[[i]])))
  }
  datlines <- .datline(cnames, cwidth)
  for (i in 1:nrow(dat)) {
    datlines <- c(datlines, .datline(dat[i, ], cwidth))
  }
  
  c(sprintf("nrow=%d ncol=%d columnlabels", nrow(dat), ncol(dat)),
    datlines)
}

# ucConf_UCODE_Control_Data() -------------------------------------------------------
ucConf_UCODE_Control_Data <- function
### lines for input block \emph{UCODE_Control_Data}
(
) 
{
  c("Begin UCODE_Control_Data KEYWORDS",
    "Optimize=yes",
    "End UCODE_Control_Data")
}

# ucConf_Reg_GN_Controls() ----------------------------------------------------------
ucConf_Reg_GN_Controls <- function
### lines for input block \emph{UCODE_Control_Data}
(
  maxIter
  ### Maximum number of parameter-estimation iterations allowed before stopping
)   
{
  c("Begin Reg_GN_Controls KEYWORDS",
    sprintf("MaxIter=%d", maxIter), 
    "End Reg_GN_Controls")
}

# ucConf_Model_Command_Lines() ------------------------------------------------------
ucConf_Model_Command_Lines <- function
### lines for input block \emph{Model_Command_Lines}
(
  bfile
  ### name of batch file to be invoked by UCODE
) 
{
  c("Begin Model_Command_Lines KEYWORDS",
    sprintf("Command = '%s'", bfile),
    "Purpose = forward",
    "CommandID = WTAQ_GOF",
    "End Model_Command_Lines")
} 

# ucConf_Parameter_Data() -----------------------------------------------------------
ucConf_Parameter_Data <- function
### lines for input block \emph{Parameter_Data}
(
  confParameters
  ### data frame representing table as required in Parameter_Data block of
  ### UCODE input file. , for example see \code{\link{ucDefaultConfParameters}}
) 
{
  c("Begin Parameter_Data TABLE",
    ucTable(confParameters),
    "End Parameter_Data")  
}

# ucConf_Observation_Data() ---------------------------------------------------------
ucConf_Observation_Data <- function
### lines for input block \emph{Observation_Data}
(
  weights 
  ### Matix of weights used when calculating overall fitness. 
  ### Row names = names of fitness indicators out of the set of c("ME", "MAE", 
  ### "MSE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE",
  ### "d", "md", "rd", "cp", "r", "R2", "bR2", "KGE, "VE"). Column names = well
  ### names ("PW" = pumping well, observation wells named as in parameter 
  ### setting). For an example see \code{\link{ucConf}}
)
{    
  dat <- NULL
  wells <- colnames(weights)
  gofs  <- rownames(weights)
  #for (i in 1:ncol(weight)) {
  for (well in wells) {
    for (gof in gofs) {
      # print only rows with weight > 0
      if (weights[gof, well] > 0) {
        drow <- data.frame(
          obsname   = sprintf("%s_%s", well, gof),
          obsvalue  = uwGofTargetValue(gof),
          statistic = weights[gof, well], 
          statflag  = "WT", stringsAsFactors = FALSE)
        dat <- rbind(dat, drow)        
      }
    }
  }
  c("Begin Observation_Data TABLE",
    ucTable(dat),
    "End Observation_Data")  
}

# ucConf_Model_Input_Files() --------------------------------------------------------
ucConf_Model_Input_Files <- function
### lines for input block \emph{Model_Input_Files}
(
) 
{
  c("Begin Model_Input_Files KEYWORDS",
    "modinfile=wtaq.inp templatefile=wtaq.tpl",
    "End Model_Input_Files")
}

# ucConf_Model_Output_Files() ---------------------------------------------------------
ucConf_Model_Output_Files <- function
### lines for input block \emph{Model_Output_Files}
(
)
{
  c("Begin Model_Output_Files KEYWORDS",
  "modoutfile=wtaq.gof instructionfile=wtaqGof.ins category=obs",
  "End Model_Output_Files")
}

# ucInpFileLines() ---------------------------------------------------------
ucInpFileLines <- function
### lines for UCODE main input file
(
  uconf,
  ### UCODE configuration as retrieved by \code{\link{ucConf}}
  command
  ### name of batch file that is invoked by UCODE with each iteration  
) 
{
  c(ucConf_UCODE_Control_Data(), "",
    ucConf_Reg_GN_Controls(uconf$general$maxIter), "",
    ucConf_Model_Command_Lines(bfile = command), "",
    ucConf_Parameter_Data(uconf$parameters), "",
    ucConf_Observation_Data(uconf$weight), "",
    ucConf_Model_Input_Files(), "",
    ucConf_Model_Output_Files())
}

# ucWriteArgs() ------------------------------------------------------------
ucWriteArgs <- function
### write WTAQ arguments file
(
  afile
) 
{
  cat("*** Writing WTAQ arguments file", afile, "...\n")
  write(sprintf("wtaq.%s", c("inp", "out", "plot")), afile)  
}

# ucWriteEvalGofR() --------------------------------------------------------
ucWriteEvalGofR <- function
### write R script evaluating goodness of fit from WTAQ output file
(
  rfile, 
  uconf
) 
{
  cat("*** Writing GOF evaluation script", rfile, "...\n")
  goflist <- paste("\"", rownames(uconf$weight), "\"", sep="", collapse=", ")
  rows <- c("library(kwb.wtaq)",
            "args <- commandArgs(TRUE)",
            "if (length(args) == 0) stop(\"Usage: uwGofEval.R plot-file\")",
            "pfile <- args[1]",
            "if (!file.exists(pfile)) stop(\"Could not find plot-file: \", pfile)",
            sprintf("print(uwGofEval(pfile, digits = %d, gofs=c(%s)))", 
                    uconf$general$gof.digits, goflist))
  write(rows, rfile)
}

# ucWriteBatchWtaq() -----------------------------------------------------------
ucWriteBatchWtaq <- function
### write batch file invoked by UCODE, running first WTAQ and second the R 
### script doing the goodness of fit evaluation
(
  bfile, 
  wtaq, 
  verbose,
  waitForInput = FALSE
) 
{
  cat("*** Writing batch file", bfile, "...\n")  
  blines <- c("@ECHO OFF",
              "REM Paths to RCmd and WTAQ executables",
              sprintf("SET RSCRIPT=\"%s\"", 
                      gsub("/", "\\\\", file.path(R.home("bin"), "Rscript.exe"))),
              sprintf("SET WTAQ=\"%s\"", gsub("/", "\\\\", wtaq)),
              "echo *** Running WTAQ...",
              sprintf("%%WTAQ%% < wtaq.args %s", 
                      ifelse(verbose, "", "> nul")),
              "echo *** Evaluating...",
              sprintf("%%RSCRIPT%% serialWtaqGof.R wtaq.plot > wtaq.gof %s",
                      ifelse(verbose, "", "2> nul")))
  if (waitForInput) {
    blines <- c(blines, "pause")
  }
  write(blines, bfile)  
}

# ucWriteBatchWtaqParallel() -----------------------------------------------------------
ucWriteBatchWtaqParallel <- function
### write batch file invoked by UCODE, which is just waiting until WTAQ has
### finished and goodness of fit is available
(
  bfile, 
  recheckAfterMs = 10
) 
{
  cat("*** Writing batch file", bfile, "...\n")  
  blines <- c("@ECHO OFF",
              "echo > .\\_INP_AVAILABLE_",
              "echo *** Wait until WTAQ has finished and goodness of fit is available...",
              sprintf(".\\WaitForFile.exe _GOF_AVAILABLE_ %d", recheckAfterMs),
              "del _GOF_AVAILABLE_")
  write(blines, bfile)  
}

# ucWriteBatchUcode() -----------------------------------------------------------
ucWriteBatchUcode <- function
### write batch file invoking UCODE
(
  bfile, 
  ### full path to batch file
  ucode = system.file("extdata", "ucode_2005.exe", package = "kwb.wtaq"),
  ### full path to UCODE executable file
  waitForInput = TRUE,
  inpfile = "ucode.in"
  ### name of ucode input file
) 
{
  cat("*** Writing batch file", bfile, "...\n")  
  blines <- c("@ECHO OFF",
              "REM Path to UCODE executable",
              sprintf("SET UCODE=\"%s\"", 
                      gsub("/", "\\\\", ucode)),
              "echo.",
              "echo *** Running calibration with UCODE...",
              "echo.",
              ifelse(waitForInput, "pause", ""),
              sprintf("%%UCODE%% %s cal", inpfile),
              ifelse(waitForInput, "pause", ""))
  write(blines, bfile)  
}

# ucTestBatchWtaq(): Test batch file with parameter setting ------------------------
ucTestBatchWtaq <- function
### test run of batch file
(
  bfile, 
  wtaqConfiguration
) 
{
  cat("*** Writing test input file ...\n")
  tdir <- dirname(bfile)
  write(wtInputFileLines(wtaqConfiguration), file.path(tdir, "wtaq.inp"))
  
  cat("*** Running test ...\n")
  odir <- getwd()
  setwd(tdir)
  shell(bfile)  
  setwd(odir)  
}

# ucWriteInstruction() -----------------------------------------------------
ucWriteInstruction <- function
### Write UCODE instruction file, describing how to read goodness of fit values
### from wtaq.gof
(
  ifile, 
  wtaqConfiguration,
  uconf
) 
{
  wells <- colnames(uconf$weight)
  gofs  <- rownames(uconf$weight)
  
  cat("*** Writing instruction file", ifile, "...\n")
  txtrows <- c("jif @", "l1") # l1: skip first line (header line)
  for (g in gofs) {
    #txtrow <- paste("l1 w", paste(sprintf("!%s_%s!", wells, g), collapse = " w "))
    txtrow <- "l1"
    for (w in wtConfiguredWellnames(wtaqConfiguration)) {
      txtrow <- paste(txtrow, "w")
      if (w %in% wells && uconf$weight[g, w] > 0) {
        txtrow <- paste(txtrow, sprintf("!%s_%s!", w, g))
      }
    }
    txtrows <- c(txtrows, txtrow)
  }
  write(txtrows, ifile)  
}

# ucWriteMainInput() -------------------------------------------------------
ucWriteMainInput <- function
### write UCODE main input file
(
  ufile, 
  uconf,
  command
  ### name of batch file that is invoked by UCODE with each iteration  
) 
{  
  cat("*** Writing UCODE input file", ufile, "...\n")
  lines <- ucInpFileLines(uconf, command)
  write(lines, ufile)  
}

# ucWriteTemplate() --------------------------------------------------------
ucWriteTemplate <- function
### write WTAQ input template file
(
  tfile, 
  wtaqConfiguration, 
  uconf,
  pchar = "@"
) 
{
  cat("*** Writing WTAQ template file", tfile, "...\n")
  
  # Set placeholders in parameter setting
  for (pname in tolower(uconf$parameters$paramname)) {
    wtaqConfiguration <- ucSetPlaceholder(pname, wtaqConfiguration, pchar)
  }
  
  # Write parameter setting to input file (including first line needed by UCODE)
  write(c(paste("jtf", pchar), wtInputFileLines(wtaqConfiguration)), tfile) 
}

# ucPrepCalib() ----------------------------------------------------------------
ucPrepCalib <- structure(function
### prepare calibration of WTAQ-model by creation of needed input and control 
### files
(
  wtaqConfiguration = wtDefaultConfiguration(),
  ### WTAQ configuration as retrieved by \code{\link{wtConfigure}}
  uconf = ucConf(),
  ### UCode configuration as retrieved by \code{\link{ucConf}}
  tdir = tempdir(),
  ### target directory in which all input and control files are to be created.
  ### Default: temporary directory of current R Session. Attention! This
  ### temporary directory will be emptied after the R session has finished!
  open.tdir = TRUE,
  ### if TRUE, the target directory will be opened in the Windows Explorer in
  ### order to allow inspecting the created files  
  wtaq = system.file("extdata", "wtaq.2.1.exe", package = "kwb.wtaq"),
  ### full path to WTAQ executable
  ucode = system.file("extdata", "ucode_2005.exe", package = "kwb.wtaq")
  ### full path to UCODE executable
) 
{  
  ##seealso<< \code{\link{ucConf}, \link{ucRunParallel}}
  
  ##details<< All input files needed to run a UCODE calibration of a WTAQ
  ## model are created in the directory \emph{tdir}. By default 
  ## (\emph{open.tdir = TRUE}) this directory is opened in the Windows explorer.
  ## You can use the created batch file \code{runParallelUCode.bat} to start the
  ## UCODE calibration.
  
  # Write wtaq.args
  ucWriteArgs(file.path(tdir, "wtaq.args"))
  
  # Write uwGofEval.R
  ucWriteEvalGofR(file.path(tdir, "serialWtaqGof.R"), uconf)
  
  # Write runSerialWtaq.bat
  ucWriteBatchWtaq(file.path(tdir, "runSerialWtaq.bat"), wtaq, verbose = FALSE, 
                   waitForInput = FALSE)
  ucWriteBatchWtaq(file.path(tdir, "runSerialWtaqDbg.bat"), wtaq, verbose = TRUE, 
                   waitForInput = TRUE)
  
  # Write runParallelWtaq.bat
  ucWriteBatchWtaqParallel(file.path(tdir, "runParallelWtaq.bat"))
  
  # Test batch file with parameter setting
  ucTestBatchWtaq(file.path(tdir, "runSerialWtaq.bat"), wtaqConfiguration)
  
  # Write WTAQ input template file
  ucWriteTemplate(file.path(tdir, "wtaq.tpl"), wtaqConfiguration, uconf)
  
  # Write UCODE instruction file  
  ucWriteInstruction(file.path(tdir, "wtaqGof.ins"), wtaqConfiguration, uconf)
  
  # Write UCODE input file
  upin <- "ucodeParallel.in"
  usin <- "ucodeSerial.in"
  ucWriteMainInput(file.path(tdir, upin), uconf, command = "runParallelWtaq.bat")
  ucWriteMainInput(file.path(tdir, usin), uconf, command = "runSerialWtaq.bat")
  
  # Write batch file running UCODE: runParallelUCode.bat  
  ucWriteBatchUcode(file.path(tdir, "runParallelUCode.bat"), ucode, inpfile = upin)
  ucWriteBatchUcode(file.path(tdir, "runSerialUCode.bat"), ucode, inpfile = usin)
  
  # Copy WaitForFile.exe into target folder
  file.copy(system.file("extdata", "WaitForFile.exe", package = "kwb.wtaq"), tdir)
  
  # open window explorer to show created files
  if (open.tdir) {
    cmd <- sprintf("explorer /e,/root,\"%s\"", tdir)
    cat(cmd, "\n")
    shell(cmd)    
  }
  
  # Return target directory
  tdir
}, ex = function() {
  # Define WTAQ configuration...
  # Here: just load the configuration of WTAQ's Sample problem 2
  wtaqConfiguration <- wtConfigurationExample2()
  
  # Define parameters
  p <- rbind(
    ucConfParameter("HKR", startvalue = 2e-5, 
                    lowerConstraint = 1e-5, upperConstraint = 1e-1,
                    maxChange = 1, perturbAmt = 0.5),
    ucConfParameter("HKZ", startvalue = 2e-7, 
                    lowerConstraint = 1e-7, upperConstraint = 1e-3,
                    maxChange = 1, perturbAmt = 0.5))  
  
  # Define weights
  #
  #   The following weight matrix w gives a weight of 1 to the root mean square 
  #   error (RMSE) and the Nash-Sutcliffe-Efficiency (NSE) between simulated 
  #   and observed values at the pumping well ("PW") and to the NSE between 
  #   simulated and observed values at the observation well "PS1". 
  #
  #     well -> PW PS1
  w <- matrix(c( 1,  1,  # RMSE  <- GOF
                 1,  1), # NSE
              nrow = 2, byrow = TRUE,
              dimnames = list(c("RMSE", "NSE"), c("PW", "PS1")))
  
  # Create UCODE configuration
  uconf <- ucConf(general = ucConfGeneral(maxIter = 100), 
                  parameters = p, weights = w)
  
  # Create all input files needed to run the UCODE calibration of WTAQ 
  # sample problem 2  
  tdir <- ucPrepCalib(wtaqConfiguration = wtaqConfiguration, uconf = uconf)
  
  # Run loop waiting for input files provided by UCODE (remove the comment
  # character in the following line!)
  #ucRunParallel(tdir, uconf = uconf)
  
  # Now, run runParallelUCode.bat in the target directory...
})

# ucSetPlaceholder() ---------------------------------------------------------
ucSetPlaceholder <- function # set placeholder in WTAQ parameter setting
### set placeholder for parameter value in WTAQ parameter setting
(
  parname,
  ### Parameter name. Must be one of the aquifer parameters "bb", "hkr", "hkz",
  ### "ss", "sy" (for description see \code{\link{wtConfigureAquifer}}) or one of the
  ### drainage parameters "acc", "akk", "amm", "axmm" (for description see 
  ### \code{\link{wtConfigureDrainage}}; parameter "alpha" currently not supported) or
  ### one of the pumpwell parameters "qq", "rw", "rc", "zpd", "zpl", "sw"
  ### (for description see \code{\link{wtConfigurePumpwell}}) or one of the observation
  ### well parameters "r_<i>", "z1_<i>", "z2_<i>", "zp_<i>", "rp_<i>", "xll_<i>"
  ### (for description see \code{\link{wtConfigureObservationWell}}) where <i> is the number of 
  ### the observation well according to the list index in the "obswell" section
  ### of the parameter setting.
  wtaqConfiguration,
  ### WTAQ parameter setting as created by means of \code{\link{wtConfigure}}
  pchar = "@"
  ### character to be used as the very first and very last character of the
  ### placeholder. Default: \dQuote{@}
)
{
  ##seealso<< \code{\link{ucConfParameter}, \link{ucConf}}
  ph <- sprintf("%s%-10s%s", pchar, parname, pchar)
  if (parname %in% c("bb", "hkr", "hkz", "ss", "sy")) {
    wtaqConfiguration$aquifer[[parname]] <- ph
  } 
  else if (parname %in% c("acc", "akk", "amm", "axmm")) {
    wtaqConfiguration$drainage[[parname]] <- ph
  }
  else if (parname %in% c("qq", "rw", "rc", "zpd", "zpl", "sw")) {
    wtaqConfiguration$pumpwell[[parname]] <- ph
  }
  else if (length(grep("^(r|z1|z2|zp|rp|xll)_\\d$", parname)) > 0) {
    parts <- strsplit(parname, "_")[[1]]
    i <- as.integer(parts[2])
    if (i > length(wtaqConfiguration$obswells)) {
      stop("No observation well with index ", i, " available in wtaqConfiguration")
    }
    wtaqConfiguration$obswell[[i]][[parts[1]]] <- ph
  } 
  else {
    stop("Unknown WTAQ model parameter: ", parname)
  }
  
  wtaqConfiguration  
  # Parameter setting in which a parameter value is replaced by an appropriate
  # placeholder
}

# uwGofEval(): Evaluate goodness of fit for WTAQ result ------------------------
uwGofEval <- function # Evaluate goodness of fit for WTAQ result
### Evaluate goodness of fit for WTAQ result
(
  res, 
  ### either character string representing full path to WTAQ result plot file or
  ### data.frame containing the content of the WTAQ result plot file.
  logtimes = FALSE,
  ### if TRUE, time steps are supposed to be logarithmic, i.e. it is assumed 
  ### that the result plot file is in \dQuote{matrix form}.
  gofs = names(uwGofTargetValue()),
  ### vector of names of fitness functions, evaluated by hydroGOF::gof.
  ### Default: \code{c("ME", "MAE", "MSE", "RMSE", "NRMSE", "PBIAS", "RSR", 
  ### "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", "r", "R2", "bR2", 
  ### "KGE", "VE")}
  ...
  ### arguments passed to gof() function, e.g. digits: decimal places used for 
  ### rounding the goodness-of-fit indexes.
) 
{  
  if (! class(res) %in% c("character", "data.frame")) {
    stop("res must be of class character (then representing the path to the ",
         "result file) or data.frame (then representing the result data ",
         "already being read from the result file).")
  }
  
  if (class(res) == "character") {
    if (! file.exists(res)) {
      stop("Result file given in argument res not found: ", res)
    }
    
    # Read result data from plot file
#     if (logtimes) { # log-cycle time steps -> matrix form
#       res <- wtReadPlotFileMatrixView(res)
#     } else {        # user-specified times -> list form
#       res <- wtReadPlotFileListView(res)
#     }
    res <- wtReadPlotFile(res, logtimes=logtimes)    
  }
  
  wells <- unique(res$WELL)
  evalmtx <- NULL
  for (well in wells) {
    sel <- res$WELL == well
    gofres <- gof(sim=res$CALCDD[sel], obs=res$MEASDD[sel], ...)
    # Erase " %" in rownames
    rownames(gofres) <- sub(" %$", "", rownames(gofres))
    # Filter for desired fitness functions
    gofres <- gofres[rownames(gofres) %in% gofs, ]
    evalmtx <- cbind(evalmtx, gofres)
  }
  colnames(evalmtx) <- wells
  #rownames(evalmtx) <- gsub(" ", "", rownames(evalmtx))
  
  # Return evaluation matrix
  evalmtx
}

# uwGofNames() -----------------------------------------------------------------
uwGofNames <- function()# Names of available \emph{GOF} functions
### Names of available \emph{Goodness of fit (GOF)} functions
{
  sub(" %$", "", rownames(gof(1:2,1:2)))
}

# uwGofTargetValue() -----------------------------------------------------------
uwGofTargetValue <- function # target values for different GOF functions
### target values for different GOF functions, i.e. values that GOF functions
### return if observed and calculated timeseries are identical
(
  gofs = uwGofNames()
  ### names of GOF functions of which target values are to be returned
)
{
  tv <- data.frame(
    ME    = 0, # Mean Error
    MAE   = 0, # Mean Absolute Error
    MSE   = 0, # Mean Squared Error
    RMSE  = 0, # Root Mean Square Error
    NRMSE = 0, # Normalized Root Mean Square Error ( -100% <= nrms <= 100% )
    PBIAS = 0, # Percent Bias
    RSR   = 0, # Ratio of RMSE to the Standard Deviation of the Observations, RSR = rms / sd(obs). ( 0 <= RSR <= +Inf)
    rSD   = 1, # Ratio of Standard Deviations, rSD = sd(sim) / sd(obs)
    NSE   = 1, # Nash-Sutcliffe Efficiency ( -Inf <= NSE <= 1 )
    mNSE  = 1, # Modified Nash-Sutcliffe Efficiency
    rNSE  = 1, # Relative Nash-Sutcliffe Efficiency
    d     = 1, # Index of Agreement ( 0 <= d <= 1 )
    md    = 1, # Modified Index of Agreement
    rd    = 1, # Relative Index of Agreement
    cp    = 1, # Persistence Index ( 0 <= PI <= 1 )
    r     = 1, # Pearson Correlation coefficient ( -1 <= r <= 1 )
    R2    = 1, # Coefficient of Determination ( 0 <= R2 <= 1 ). Gives the proportion of the variance of one variable that is predictable from the other variable
    bR2   = 1, # R2 multiplied by the coefficient of the regression line between sim and obs ( 0 <= bR2 <= 1 )
    KGE   = 1, # Kling-Gupta efficiency between sim and obs ( 0 <= KGE <= 1 )
    VE    = 1) # Volumetric efficiency between sim and obs ( -Inf <= VE <= 1)  

  as.matrix(tv[, gofs])[1, ]
}

# .terminated() ----------------------------------------------------------------
.terminated <- function() 
{
  # Normal termination of ucode?
  ufile <- "cal.#uout"
  termtag <- "Normal termination of UCODE"
  term <- file.exists(ufile) && length(grep(termtag, readLines(ufile))) > 0
  if (term) {
    cat("\n*****\n*\n* Yippieh! Normal termination of UCODE!!!\n*\n*****\n")
  }
  term
}

# ucPlotPng() ------------------------------------------------------------------
ucPlotPng <- function
### plot WTAQ results to png file
(
  fname, 
  wtaqResult, 
  i
) 
{
  png(fname)
  wtPlotResult(wtaqResult, sprintf("Iteration: %d", i))
#  if (file.exists("cal._summary")) {
#    ucPlotSummary("cal._summary")
#  }
  dev.off()
}

# htmlForCalibrationPage() -------------------------------------------------------------------
htmlForCalibrationPage <- function # main HTML for calibration progress visualisation
### main HTML for calibration progress visualisation
(
  htmlHist = "ucHistory.html", 
  htmlCurr = "ucCurrent.html"
)
{
  c('<html>',
    '<head>',
    '</head>', 
    '<frameset cols="200,*">',
    sprintf('<frame src="%s" name="history"></frame>', htmlHist),
    sprintf('<frame src="%s" name="content"></frame>', htmlCurr),
    '<noframes>',
    '<body>',
    '<h1>Alternativ-Inhalt</h1>',
    '<p>Sorry, no frames supported.</p>',
    '</body>',
    '</noframes>',
    '</frameset>',
    '</html>')
}

# htmlForCalibrationState() ----------------------------------------------------------------
htmlForCalibrationState <- function # Content HTML for calibration progress visualisation
### Content HTML for calibration progress visualisation
(
  refresh=1
  ### refresh time in seconds
)
{
  c('<html>',
    sprintf('<head><meta http-equiv="refresh" content="%d"></head>', refresh),
    '<body>',
    '<h1>Current status of WTAQ calibration with UCODE</h1>',
    '<img src="plots/plotCurr.png"></img>',
    '</body>',
    '</html>')
}

# htmlForCalibrationHistory() ----------------------------------------------------------------
htmlForCalibrationHistory <- function # History HTML for calibration progress visualisation
### History HTML for calibration progress visualisation
(
  n, 
  current = "ucCurrent.html"
) 
{
  links <- sprintf('<p><a href="%s" target="content">Current</a></p>',
                   current)
  for (i in seq(n, 1, by = -1)) {
    links <- c(links, 
               sprintf(
                 '<p><a href="plots/plot%04d.png" target="content">plot%04d</a></p>', 
                 i, i))
  }
  c('<html>',
    '<head><meta http-equiv="refresh" content="1"></head>',
    '<body>', '<h1>History</h1>', links, '</body>',
    '</html>')
}

# ucRunParallel ----------------------------------------------------------------
ucRunParallel <- function # run main calibration loop
### run main calibration loop 
(
  uconf = ucConf(),
  ### UCode configuration as retrieved by \code{\link{ucConf}}
  tdir = tempdir(),
  wtaq = system.file("extdata", "wtaq.2.1.exe", package = "kwb.wtaq"),
  FUN = NULL
  ### function that is invoked each time before WTAQ is run
  #,autorun = FALSE
) 
{
  ##seealso<< \code{\link{ucPrepCalib}}
  
  # Set current directory to target directory
  cat("Target directory:", tdir, "\n")
  cdir <- getwd()
  setwd(tdir)
  on.exit(setwd(cdir))
  
  # Delete files from previous runs
  dfiles <- c("wtaq.inp", "wtaq.out", "wtaq.plot", "wtaq.log", "wtaq.gof",
              "_GOF_AVAILABLE_", "INP_AVAILABLE", 
              dir(getwd(), "^cal"),
              dir(getwd(), "\\.png$"))
  cat("Deleting", paste(dfiles, collapse = ", "), "...")
  unlink(dfiles)
  cat("ok.\n")

  dir.create("plots")
  ucCurrent <- "ucCurrent.html"
  ucHistory <- "ucHistory.html"
  writeLines(htmlForCalibrationState(1), ucCurrent)
  writeLines(htmlForCalibrationPage(), "ucMain.html")
    
  cat("*\n* RUN runParallelUCode.bat IN THE TARGET DIRECTORY",
      "IN ORDER TO START THE CALIBRATION!\n*\n")
#  if (autorun) {
#    shell("echo x | runParallelUCode.bat ", wait = FALSE)  
#  }
  
  terminated <- FALSE
  
  i <- 1
  while (!terminated) {
    
    # Wait for file "_INP_AVAILABLE_"
    cat("*** waiting for _INP_AVAILABLE_... ")
    while (!file.exists("_INP_AVAILABLE_") && !(terminated <- .terminated())) {}
    cat("ok.\n")
    
    if (! terminated) {
      
      cat("i =", i, "\n")
      
      # Delete file "_INP_AVAILABLE_" (use shell instead of unlink in order 
      # to wait until the file is deleted)
      shell("del _INP_AVAILABLE_")
      
      if (! is.null(FUN)) {
        FUN()
      }
      
      # Run WTAQ
      cat("*** Running WTAQ...\n")
      cmd <- sprintf('"%s" < wtaq.args > nul', gsub("/", "\\\\", wtaq))
      shell(cmd, intern=FALSE)
      
      # plot simulated values versus measured values
      pdat <- wtReadPlotFile("wtaq.plot")
      pfile <- sprintf("plots/plot%04d.png", i)
      ucPlotPng(pfile, pdat, i)
      file.copy(pfile, "plots/plotCurr.png", overwrite=TRUE)
      
      # Update history html
      writeLines(htmlForCalibrationHistory(i, ucCurrent), file.path(tdir, ucHistory))                
      
      # Evaluate GOF from WTAQ result wtaq.plot and write wtaq.gof
      gofres <- uwGofEval("wtaq.plot", digits = uconf$general$gof.digits, 
                          gofs = rownames(uconf$weights))
      write.table(gofres, "wtaq.gof")
 
      # Write file _GOF_AVAILABLE_
      write("", "_GOF_AVAILABLE_")
      #while (!file.exists("_GOF_AVAILABLE_") && !(terminated <- .terminated())) {}      
    }
    i <- i + 1
  } 
}

# ucReadSummary() --------------------------------------------------------------
ucReadSummary <- function # Read UCODE summary file
### Read UCODE summary file
(sfile) {
  txt <- readLines(sfile)
  
  h1 <- grep("SELECTED STATISTICS FROM MODIFIED GAUSS-NEWTON ITERATIONS", txt)
  h2 <- grep("PARAMETER VALUES FOR EACH ITERATION", txt)
  h3 <- grep("SUMS OF SQUARED WEIGHTED RESIDUALS FOR EACH ITERATION", txt)
  nr <- length(txt)
  
  b1 <- sub("^\\s+", "", txt[(h1+5):(h2-3)])
  b2 <- sub("^\\s+", "", txt[(h2+5):(h3-2)])
  b3 <- sub("^\\s+", "", txt[(h3+4):nr])
  
  # 1st block
  d1 <- as.data.frame(t(sapply(strsplit(b1, "\\s+"), FUN=c)))
  n1 <- c("ITER", "NUM.PARS.ESTIM", "PARNAM", "MAX.CALC.CHANGE", 
          "MAX.CHANGE.ALLOWED", "DAMPING.PAR")
  names(d1) <- n1
  
  # 2nd block
  dtmp <- t(sapply(strsplit(b2, "\\s+"), FUN=c))
  d2 <- as.data.frame(dtmp[-1, ])
  n2 <- dtmp[1, ]
  names(d2) <- n2
  
  # 3rd block
  d3 <- as.data.frame(t(sapply(strsplit(b3, "\\s+"), FUN=c))[, -6])
  n3 <- c("ITER", "OBSERVS", "PRIOR.INFO", "TOTAL", "NUM.INCL.OBS", "NUM.TOT.OBS")
  names(d3) <- n3
  
  d <- merge(merge(d1, d2), d3)
  
  # convert types
  for (cname in c(n1[-3], n2, n3)) {
    d[[cname]] <- as.numeric(as.character(d[[cname]]))
  }
  
  # return data frame, ordered by ITER
  d[order(d$ITER), ]  
}

# ucPlotSummary() --------------------------------------------------------------
ucPlotSummary <- function # Plot UCODE summary file
### Plot UCODE summary file
(sfile) {
  dat <- ucReadSummary(sfile)
  tr1 <- xyplot(HKR + HKZ ~ ITER, data = dat, type = "b", 
                auto.key = list(columns = 2), xlab = "Iteration number")
  tr2 <- xyplot(TOTAL ~ ITER, data = dat, type = "b", 
                auto.key = list(columns = 2), xlab = "Iteration number",
                ylab = "Total sum of squared weighted residuals", 
                scales = list(y = list(log=TRUE)))  
  print(tr1, split = c(1, 1, 1, 2))
  print(tr2, split = c(1, 2, 1, 2), newpage = FALSE)  
}
