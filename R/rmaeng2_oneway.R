#' rmaeng2_oneway
#'
#' one way anova test
#'
#' @param infile Path to the input files
#'
#' @return analysis of the infile
#'
#' @importFrom
#' tcltk tkframe
#' tcltk ttkentry
#' tcltk ttkcheckbutton
#'
#' @export

.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  putRcmdr("slider.env", new.env())
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }
  }
}

rmaeng2_oneway <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.pairwise = 0, initial.welch=0)
  dialog.values <- getDialog("oneWayAnova", defaults)
  initializeDialog(title = gettextRcmdr("One-Way Analysis of Variance"))
  UpdateModelNumber()
  modelName <- tcltk::tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tcltk::tkframe(top)
  model <- tcltk::ttkentry(modelFrame, width = "20", textvariable = modelName)
  dataFrame <- tcltk::tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), title = gettextRcmdr("Groups (pick one)"),
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))
  optionsFrame <- tcltk::tkframe(top)
  pairwiseVariable <- tcltk::tclVar(dialog.values$initial.pairwise)
  pairwiseCheckBox <- tcltk::ttkcheckbutton(optionsFrame, variable = pairwiseVariable)
  welchVariable <- tcltk::tclVar(dialog.values$initial.welch)
  welchCheckBox <- tcltk::ttkcheckbutton(optionsFrame, variable = welchVariable)
  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)) {
      UpdateModelNumber(-1)
      errorCondition(recall = oneWayAnova, message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),
                                                             modelValue))
      return()
    }
    if (is.element(modelValue, listAOVModels())) {
      if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        UpdateModelNumber(-1)
        tkdestroy(top)
        oneWayAnova()
        return()
      }
    }
    group <- getSelection(groupBox)
    response <- getSelection(responseBox)
    closeDialog()
    if (length(group) == 0) {
      errorCondition(recall = oneWayAnova, message = gettextRcmdr("You must select a groups factor."))
      return()
    }
    if (length(response) == 0) {
      errorCondition(recall = oneWayAnova, message = gettextRcmdr("You must select a response variable."))
      return()
    }
    .activeDataSet <- ActiveDataSet()
    command <- paste(modelValue, " <- aov(", response, " ~ ",
                     group, ", data=", .activeDataSet, ")", sep = "")
    justDoIt(command)
    logger(command)
    doItAndPrint(paste("summary(", modelValue, ")", sep = ""))
    doItAndPrint(paste("with(", .activeDataSet, ", numSummary(",
                       response, ", groups=", group,
                       ", statistics=c(\"mean\", \"sd\")))", sep = ""))
    activeModel(modelValue)
    putRcmdr("modelWithSubset", FALSE)
    pairwise <- tclvalue(pairwiseVariable)
    welch <- tclvalue(welchVariable)
    putDialog ("oneWayAnova", list (initial.group = group, initial.response = response, initial.pairwise = pairwise,
                                    initial.welch=welch))
    if (pairwise == 1) {
      if (eval(parse(text = paste("length(levels(", .activeDataSet,
                                  "$", group, ")) < 3"))))
        Message(message = gettextRcmdr("Factor has fewer than 3 levels; pairwise comparisons omitted."),
                type = "warning")
      else {
        commands <- character(7)
        commands[1] <- paste("local({\n  .Pairs <- glht(", modelValue,
                             ", linfct = mcp(", group, " = \"Tukey\"))",
                             sep = "")
        commands[2] <- "  print(summary(.Pairs)) # pairwise tests"
        commands[3] <- "  print(confint(.Pairs)) # confidence intervals"
        commands[4] <- "  print(cld(.Pairs)) # compact letter display"
        commands[5] <- "  old.oma <- par(oma=c(0,5,0,0))"
        commands[6] <- "  plot(confint(.Pairs))"
        commands[7] <- "  par(old.oma)\n})"
        doItAndPrint(paste(commands, collapse="\n"))
      }
    }
    if (welch == 1){
      command <- paste("oneway.test(", response, " ~ ",
                       group, ", data=", .activeDataSet, ") # Welch test", sep = "")
      doItAndPrint(command)
    }
    tcltk::tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "anova", model = TRUE, reset = "oneWayAnova", apply = "oneWayAnova")
  tcltk::tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tcltk::tkgrid(modelFrame, sticky = "w", columnspan = 2)
  tcltk::tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tcltk::tkgrid(dataFrame, sticky="w")
  tcltk::tkgrid(pairwiseCheckBox, labelRcmdr(optionsFrame, text = gettextRcmdr("Pairwise comparisons of means")),
         sticky = "w")
  tcltk::tkgrid(welchCheckBox, labelRcmdr(optionsFrame, text = gettextRcmdr("Welch F-test not assuming equal variances")),
         sticky = "w")
  tcltk::tkgrid(optionsFrame, sticky = "w")
  tcltk::tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix()
}
