###############################
#### MODEL SPECIFICATIONS #####
###############################



#in this model: positive coefficients for the "s" parameter indicate a bias toward the "keep" boundary and negative coefficients indicate a bias toward the "donate" boundary
grpVars <- c("leftItem", "inGroupConsistency")

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


    #InGroupOverallSE
    #add title SE effect to overallSE effect model
      x1 <- "inGroupConsistency == 'HVOin-LVOout'"
      v1 <- 1
      x2 <- "inGroupConsistency == 'HVOout-LVOin'"
      v2 <- -1
      x3 <- "default"
      v3 <- 0

      columnName <- "inGroupSEColumn"
      df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
      GroupByVariables <- grpVars
      ParameterName <- "sInGroup"
      parameter <- "s"
      parameterBounds <- c(0.8, -0.8, 0.001)

    ingroupSE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

    #inGroup
    x1 <- "inGroupConsistency == 'HVOin-LVOout'"
    v1 <- 1
    x2 <- "inGroupConsistency == 'HVOout-LVOin'"
    v2 <- -1
    x3 <- "default"
    v3 <- 0

    columnName <- "inGroupVCColumn"
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    GroupByVariables <- grpVars
    ParameterName <- "vInGroup"
    parameter <- "vc"
    parameterBounds <- c(0.25, -0.25, 0.001)

    ingroupVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#########################
### build models
#########################

### first build with nSD == 1 ##

simpleModelList.nSDfree <- NULL
simpleModelList.nSDfree <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSDfree, c(freeNSD, freeDB, freeB, freeVC))

#side ingroup SE bias
ingroupSEModelList.nSDfree <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSDfree, c(ingroupSE))

#add value correction;
ingroupVCModelList.nSDfree <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSDfree, c(ingroupVC))

#side start bias + constantVC
ingroupSEingroupVCModelList.nSDfree <- rrwAddParameterEffectListToRRWModel(ingroupSEModelList.nSDfree, c(ingroupVC))

#add side bias
simpleModelList.SB.nSDfree <- rrwAddParameterEffectListToRRWModel(simpleModelList.nSDfree, c(sidebiasSE))

#side ingroup SE bias
ingroupSEModelList.SB.nSDfree <- rrwAddParameterEffectListToRRWModel(simpleModelList.SB.nSDfree, c(ingroupSE))

#add value correction;
ingroupVCModelList.SB.nSDfree <- rrwAddParameterEffectListToRRWModel(simpleModelList.SB.nSDfree, c(ingroupVC))

#side start bias + constantVC
ingroupSEingroupVCModelList.SB.nSDfree <- rrwAddParameterEffectListToRRWModel(ingroupSEModelList.SB.nSDfree, c(ingroupVC))



allModels <- list(simpleModelList.nSDfree= simpleModelList.nSDfree,
  ingroupSEModelList.nSDfree = ingroupSEModelList.nSDfree,
  ingroupVCModelList.nSDfree=ingroupVCModelList.nSDfree,
  ingroupSEingroupVCModelList.nSDfree=ingroupSEingroupVCModelList.nSDfree,
  simpleModelList.SB.nSDfree = simpleModelList.SB.nSDfree,
  ingroupSEModelList.SB.nSDfree = ingroupSEModelList.SB.nSDfree,
  ingroupVCModelList.SB.nSDfree = ingroupVCModelList.SB.nSDfree,
  ingroupSEingroupVCModelList.SB.nSDfree = ingroupSEingroupVCModelList.SB.nSDfree)

allFixedModels <- NULL
