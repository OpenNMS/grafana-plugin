export const updateFlowQuery = (source: any) => {
  let target: any = {
    ...source,
    functions: convertFunctions(source),
    functionParameters: convertFunctionParameters(source),
    parameterOptions: convertParameterOptions(source)
  }

  return target
}

const convertFunctions = (target: any) => {
  const functions = target.functions?.map(f => {
    return {
      label: f.name
    }
  })

  return functions || []
}

const convertFunctionParameters = (target: any) => {
  const params = target.functions?.map(f => {
    return f.parameters ? f.parameters[0] : null
  })

  return params || []
}

const convertParameterOptions = (target: any) => {
  // TODO
  const options = target.functions?.map(f => {
    return {}
  })

  return options || []
}
