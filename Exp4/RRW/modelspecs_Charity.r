###############################
#### MODEL SPECIFICATIONS #####
###############################



#in this model: positive coefficients for the "s" parameter indicate a bias toward the "keep" boundary and negative coefficients indicate a bias toward the "donate" boundary
grpVars <- c("refValue", "typeOfScen")

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
  parameterBounds <- c(200, 5, 0.01)

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
    parameterBounds <- c(0.65, -0.65, 0.001)

  constantVC <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#keepbiasSE
#now assess whether there is a bias to keep vs donate by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
#Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
#we expect a positive coefficient because we expect a bias toward the "keep" boundary
  x1 <- "refValue == 'refHVO'"
  #when the leftItem is LVO then bias towards the upperBound (choose HVO: bias = right Item)
  v1 <- 1
  x2 <- "refValue == 'refLVO'"
  #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
  v2 <- -1
  x3 <- "default"
  v3 <- 0

  #this is the columnName of the dummy/effect variable
  columnName <- "keepbiasSEColumn"
  #this dataframe contains the coding inforamtion of the dummy/effect variable
  df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
  #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
  GroupByVariables <- grpVars
  #this is the name given to the parameter that will measure the effect of this dummy/effect variable
  ParameterName <- "sKeepbias"
  #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
  parameter <- "s"
  #These are the bounds of the parameter values: c(high, low, interval)
  parameterBounds <- c(0.9, -0.9, 0.001)

  #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
  keepbiasSE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


  #YouValue Change Correction
  #now assess whether there is a bias to keep vs donate induced by the $10 donation by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
  #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
  #we expect a negative coefficient because we expect a bias toward the "donate" boundary
    x1 <- "refValue == 'refHVO'"
    #when the leftItem is LVO then bias towards the upperBound (choose HVO: bias = right Item)
    v1 <- 1
    x2 <- "refValue == 'refLVO'"
    #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
    v2 <- -1
    x3 <- "default"
    v3 <- 0

    #this is the columnName of the dummy/effect variable
    columnName <- "refVCshiftColumn"
    #this dataframe contains the coding inforamtion of the dummy/effect variable
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
    GroupByVariables <- grpVars
    #this is the name given to the parameter that will measure the effect of this dummy/effect variable
    ParameterName <- "vcYoubias"
    #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
    parameter <- "vc"
    #These are the bounds of the parameter values: c(high, low, interval)
    parameterBounds <- c(0.65, -0.65, 0.001)


    refShiftVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)



  #10EffectbiasSE
  #now assess whether there is a bias to keep vs donate induced by the $10 donation by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
  #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
  #we expect a negative coefficient because we expect a bias toward the "donate" boundary
    x1 <- "refValue == 'refHVO' & typeOfScen == 'ten'"
    #when the leftItem is LVO then bias towards the upperBound (choose HVO: bias = right Item)
    v1 <- 1
    x2 <- "refValue == 'refLVO' & typeOfScen == 'ten'"
    #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
    v2 <- -1
    x3 <- "default"
    v3 <- 0

    #this is the columnName of the dummy/effect variable
    columnName <- "tenbiasSEColumn"
    #this dataframe contains the coding inforamtion of the dummy/effect variable
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
    GroupByVariables <- grpVars
    #this is the name given to the parameter that will measure the effect of this dummy/effect variable
    ParameterName <- "sTenbias"
    #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
    parameter <- "s"
    #These are the bounds of the parameter values: c(high, low, interval)
    parameterBounds <- c(0.45, -0.45, 0.001)

    #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
    tenKeepbiasSE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

    #100EffectbiasSE
    #now assess whether there is a bias to keep vs donate induced by the $10 donation by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
    #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
    #we expect a negative coefficient because we expect a bias toward the "donate" boundary
      x1 <- "refValue == 'refHVO' & typeOfScen == 'onehundred'"
      #when the leftItem is LVO then bias towards the upperBound (choose HVO: bias = right Item)
      v1 <- 1
      x2 <- "refValue == 'refLVO' & typeOfScen == 'onehundred'"
      #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
      v2 <- -1
      x3 <- "default"
      v3 <- 0

      #this is the columnName of the dummy/effect variable
      columnName <- "onehundredbiasSEColumn"
      #this dataframe contains the coding inforamtion of the dummy/effect variable
      df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
      #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
      GroupByVariables <- grpVars
      #this is the name given to the parameter that will measure the effect of this dummy/effect variable
      ParameterName <- "sOnehundredbias"
      #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
      parameter <- "s"
      #These are the bounds of the parameter values: c(high, low, interval)
      parameterBounds <- c(0.45, -0.45, 0.001)

      #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
      onehundredKeepbiasSE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

      #10ValueChange
      #now assess whether there is a bias to keep vs donate induced by the $10 donation by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
      #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
      #we expect a negative coefficient because we expect a bias toward the "donate" boundary
        x1 <- "refValue == 'refHVO' & typeOfScen == 'ten'"
        #when the leftItem is LVO then bias towards the upperBound (choose HVO: bias = right Item)
        v1 <- 1
        x2 <- "refValue == 'refLVO' & typeOfScen == 'ten'"
        #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
        v2 <- -1
        x3 <- "default"
        v3 <- 0

        #this is the columnName of the dummy/effect variable
        columnName <- "tenVCColumn"
        #this dataframe contains the coding inforamtion of the dummy/effect variable
        df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
        #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
        GroupByVariables <- grpVars
        #this is the name given to the parameter that will measure the effect of this dummy/effect variable
        ParameterName <- "vcTen"
        #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
        parameter <- "vc"
        #These are the bounds of the parameter values: c(high, low, interval)
        parameterBounds <- c(0.75, -0.75, 0.001)

        #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
        tenVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


      #100ValueChangeSE
      #now assess whether there is a bias to keep vs donate induced by the $10 donation by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
      #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
      #we expect a negative coefficient because we expect a bias toward the "donate" boundary
        x1 <- "refValue == 'refHVO' & typeOfScen == 'onehundred'"
        #when the leftItem is LVO then bias towards the upperBound (choose HVO: bias = right Item)
        v1 <- 1
        x2 <- "refValue == 'refLVO' & typeOfScen == 'onehundred'"
        #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
        v2 <- -1
        x3 <- "default"
        v3 <- 0

        #this is the columnName of the dummy/effect variable
        columnName <- "onehundredVCColumn"
        #this dataframe contains the coding inforamtion of the dummy/effect variable
        df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
        #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
        GroupByVariables <- grpVars
        #this is the name given to the parameter that will measure the effect of this dummy/effect variable
        ParameterName <- "vcOnehundred"
        #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
        parameter <- "vc"
        #These are the bounds of the parameter values: c(high, low, interval)
        parameterBounds <- c(0.75, -0.75, 0.001)

        #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
        onehundredVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


        simpleModelList <- NULL
        simpleModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(freeNSD, freeDB, freeB, refShiftVC))

        #overall start bias
        keepbiasSEModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(keepbiasSE))

        #dollar effects are start bias
        dollarKeepbiasSEModelList <- rrwAddParameterEffectListToRRWModel(keepbiasSEModelList, c(tenKeepbiasSE,onehundredKeepbiasSE))

        # #dollar effects are start bias
        # VCdollarKeepbiasSEModelList <- rrwAddParameterEffectListToRRWModel(dollarKeepbiasSEModelList, c(constantVC))

        #dollar effects are value
        dollarVCModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(tenVC, onehundredVC))

        #biased toward keep: dollar effects are value
        dollarVCkeepbiasSEModelList <- rrwAddParameterEffectListToRRWModel(keepbiasSEModelList, c(tenVC, onehundredVC))

        #biased toward keep - separately for each dollar amout: 10 and 100 dollar effects for value
        dollarVCdollarKeepbiasSEModelList <- rrwAddParameterEffectListToRRWModel(dollarKeepbiasSEModelList, c(tenVC, onehundredVC))

        # #dollar effects are value
        # VCdollarVCdollarKeepbiasSEModelList <- rrwAddParameterEffectListToRRWModel(VCdollarKeepbiasSEModelList, c(tenVC, onehundredVC))

        # #ref value correction;
        # refVCModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(refShiftVC))
        #
        # #ref value correction; overall start bias
        # refVCkeepbiasSEModelList <- rrwAddParameterEffectListToRRWModel(keepbiasSEModelList, c(refShiftVC))
        #
        # #ref value correction; dollar effects are start bias
        # YouVCdollarKeepbiasSEModelList <- rrwAddParameterEffectListToRRWModel(refVCkeepbiasSEModelList, c(tenKeepbiasSE,onehundredKeepbiasSE))
        #
        # #dollar effects are value; You VC effect
        # dollarYouVCModelList <- rrwAddParameterEffectListToRRWModel(refVCModelList, c(tenVC, onehundredVC))
        #
        # #biased toward keep: dollar effects are value: You VC
        # dollarYouVCkeepbiasSEModelList <- rrwAddParameterEffectListToRRWModel(refVCkeepbiasSEModelList, c(tenVC, onehundredVC))
        #
        # #biased toward keep: dollar effects are value and a start bias; ref VC
        # dollarYouVCdollarKeepbiasSEModelList <- rrwAddParameterEffectListToRRWModel(dollarYouVCkeepbiasSEModelList, c(tenKeepbiasSE,onehundredKeepbiasSE))



allModels <- list(simpleModelList= simpleModelList,
  keepbiasSEModelList = keepbiasSEModelList,
  dollarKeepbiasSEModelList=dollarKeepbiasSEModelList,
  dollarVCModelList = dollarVCModelList,
  dollarVCkeepbiasSEModelList = dollarVCkeepbiasSEModelList,
  dollarVCdollarKeepbiasSEModelList = dollarVCdollarKeepbiasSEModelList)


allFixedModels <- NULL
