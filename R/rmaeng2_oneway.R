#' rmaeng2_oneway
#'
#' one way anova test
#'
#' @param
#'
#' @return anova
#'
#' @examples
#'
#' @export
#'
#' @importFrom
#' tcltk tkframe
#' tcltk ttkentry
#' tcltk ttkcheckbutton

rmaeng2_oneway <- function () {
  Library("multcomp")
  Library("abind")
  Library("tcltk")
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.pairwise = 0, initial.welch=0)
  dialog.values <- getDialog("oneWayAnova", defaults)
  initializeDialog(title = gettextRcmdr("One-Way Analysis of Variance"))
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), title = gettextRcmdr("Groups (pick one)"),
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))
  optionsFrame <- tkframe(top)
  pairwiseVariable <- tclVar(dialog.values$initial.pairwise)
  pairwiseCheckBox <- ttkcheckbutton(optionsFrame, variable = pairwiseVariable)
  welchVariable <- tclVar(dialog.values$initial.welch)
  welchCheckBox <- ttkcheckbutton(optionsFrame, variable = welchVariable)
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
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "anova", model = TRUE, reset = "oneWayAnova", apply = "oneWayAnova")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w", columnspan = 2)
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(pairwiseCheckBox, labelRcmdr(optionsFrame, text = gettextRcmdr("Pairwise comparisons of means")),
         sticky = "w")
  tkgrid(welchCheckBox, labelRcmdr(optionsFrame, text = gettextRcmdr("Welch F-test not assuming equal variances")),
         sticky = "w")
  tkgrid(optionsFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix()
}
