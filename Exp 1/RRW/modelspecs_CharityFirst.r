###############################
#### MODEL SPECIFICATIONS #####
###############################



#in this model: positive coefficients for the "s" parameter indicate a bias toward the "keep" boundary and negative coefficients indicate a bias toward the "donate" boundary
grpVars <- c("leftItem")

#fixedNSD
#Add NSD as a fixed parameter (NSD = 1)
#Here I group by the three variables but add not effects for them.  This will split the data properly
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- grpVars
  parameter <- "nSD"
  ParameterName <- "nSD"
  parameterBounds <- c(1, 1, 0.25)

  fixedNSD  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#freeNSD
#Add NSD as a fixed parameter (NSD = 1)
#Here I group by the three variables but add not effects for them.  This will split the data properly
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- grpVars
  parameter <- "nSD"
  ParameterName <- "nSD"
  parameterBounds <- c(7, 1, 0.25)

  freeNSD  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeDB
  #Add DB as a free parameter
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "db"
  ParameterName <- "db"
  parameterBounds <- c(0.5, 0, 0.001)

  freeDB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeB
  #Add B as a free parameter
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "b"
  ParameterName <- "b"
  parameterBounds <- c(100, 5, 0.01)

  freeB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeValue
  #Add v as a fixed parameter (v = -0.07)
  #add a column for the fixed parameter.  This is needed because we will be adding another vc effect later
    x1 <- "default"
    #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
    v1 <- 1

    columnName <- "VCconstantColumn"
    df.code <- data.frame(logic = c(x1), value = c(v1))
    GroupByVariables <- grpVars
    parameter <- "vc"
    ParameterName <- "vConstant"
    parameterBounds <- c(0.35, -0.35, 0.001)

  freeVC <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #overallSE
  #now assess whether there is a bias induced by the left (or right) item with "leftItem" by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
  #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
    x1 <- "leftItem == 'leftLVO'"
    #when the leftItem is LVO then bias towards the upperBound (choose HVO: bias = right Item)
    v1 <- 1
    x2 <- "leftItem == 'leftHVO'"
    #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
    v2 <- -1
    x3 <- "default"
    v3 <- 0

    #this is the columnName of the dummy/effect variable
    columnName <- "sidebiasSEColumn"
    #this dataframe contains the coding inforamtion of the dummy/effect variable
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
    GroupByVariables <- grpVars
    #this is the name given to the parameter that will measure the effect of this dummy/effect variable
    ParameterName <- "sSidebias"
    #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
    parameter <- "s"
    #These are the bounds of the parameter values: c(high, low, interval)
    parameterBounds <- c(0.8, -0.8, 0.001)

    #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
    sidebiasSE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


##### Fixed effects from Exp 1 ###

#fixedDB
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- grpVars
  parameter <- "db"
  ParameterName <- "db"
  parameterBounds <- c(0.22, 0.22, 0.001)

  fixedDB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #fixedValue
  #Add v as a fixed parameter (v = -0.11)
  #add a column for the fixed parameter.  This is needed because we will be adding another vc effect later
    x1 <- "default"
    #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
    v1 <- 1

    columnName <- "VCconstantColumn"
    df.code <- data.frame(logic = c(x1), value = c(v1))
    GroupByVariables <- grpVars
    parameter <- "vc"
    ParameterName <- "vConstant"
    parameterBounds <- c(-0.11, -0.11, 0.001)

    fixedVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #fixedB
    columnName <- NULL
    df.code <- NULL
    GroupByVariables <- grpVars
    parameter <- "b"
    ParameterName <- "b"
    parameterBounds <- c(66.1, 66.1, 0.01)

    fixedB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#########################
### build models
#########################

### first build with nSD == 1 ##

simpleModelList.nSD1 <- NULL
simpleModelList.nSD1 <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSD1, c(fixedNSD, freeDB, freeB))

#side start bias
sidebiasSEModelList.nSD1 <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSD1, c(sidebiasSE))

#add value correction;
constantVCModelList.nSD1 <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSD1, c(freeVC))

#side start bias + constantVC
sidebiasSEconstantVCModelList.nSD1 <- rrwAddParameterEffectListToRRWModel(sidebiasSEModelList.nSD1, c(freeVC))

### second build with free nSD  ##

simpleModelList.nSDfree <- NULL
simpleModelList.nSDfree <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSDfree, c(freeNSD, freeDB, freeB))

#side start bias
sidebiasSEModelList.nSDfree <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSDfree, c(sidebiasSE))

#add value correction;
constantVCModelList.nSDfree <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSDfree, c(freeVC))

#side start bias + constantVC
sidebiasSEconstantVCModelList.nSDfree <- rrwAddParameterEffectListToRRWModel(sidebiasSEModelList.nSDfree, c(freeVC))


# allModels <- list(simpleModelList.nSD1= simpleModelList.nSD1,
#   sidebiasSEModelList.nSD1 = sidebiasSEModelList.nSD1,
#   constantVCModelList.nSD1=constantVCModelList.nSD1,
#   sidebiasSEconstantVCModelList.nSD1=sidebiasSEconstantVCModelList.nSD1,
#   simpleModelList.nSDfree= simpleModelList.nSDfree,
#   sidebiasSEModelList.nSDfree = sidebiasSEModelList.nSDfree,
#   constantVCModelList.nSDfree=constantVCModelList.nSDfree,
#   sidebiasSEconstantVCModelList.nSDfree=sidebiasSEconstantVCModelList.nSDfree)

  allModels <- list(simpleModelList.nSDfree= simpleModelList.nSDfree,
    sidebiasSEModelList.nSDfree = sidebiasSEModelList.nSDfree,
    constantVCModelList.nSDfree=constantVCModelList.nSDfree,
    sidebiasSEconstantVCModelList.nSDfree=sidebiasSEconstantVCModelList.nSDfree)

allFixedModels <- NULL
