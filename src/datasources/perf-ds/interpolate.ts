import _ from 'lodash';

interface PerfVariable {
  name: string;
  value: any;
}

interface PerfVariables extends Array<PerfVariable> {};

function cartesianProductOfArrays(arrays) {
  // Based on the code from http://stackoverflow.com/questions/15298912/
  // javascript-generating-combinations-from-n-arrays-with-m-elements
  let r = [] as any[], max = arrays.length - 1;

  function helper(arr, i) {
    for (let j = 0, l = arrays[i].length; j < l; j++) {
      let a = arr.slice(0); // clone arr
      a.push(arrays[i][j]);
      if (i === max) {
        r.push(a);
      } else {
        helper(a, i + 1);
      }
    }
  }

  helper([], 0);
  return r;
}

function cartesianProductOfVariables(variables: PerfVariables) {
  // Collect the values from all of the variables
  const allValues = [] as any[];
  _.each(variables, function (variable) {
    allValues.push(variable.value);
  });

  // Generate the cartesian product
  const productOfAllValues = cartesianProductOfArrays(allValues);

  // Rebuild the variables
  const productOfAllVariables = [] as PerfVariables[];
  _.each(productOfAllValues, function (rowOfValues) {
    const rowOfVariables = [] as PerfVariable[];
    for (let i = 0, l = variables.length; i < l; i++) {
      // Deep clone
      const variable = JSON.parse(JSON.stringify(variables[i]));
      variable.value = rowOfValues[i];
      rowOfVariables.push(variable);
    }
    productOfAllVariables.push(rowOfVariables);
  });

  return productOfAllVariables;
}

function defaultContainsVariable(value: any | undefined, variableName: string) {
  if (_.isNull(value) || _.isEmpty(value)) {
    return false;
  }
  return value.indexOf("$" + variableName) >= 0;
}

function defaultReplace(value: any, variables: PerfVariables) {
  if (_.isNull(value) || _.isEmpty(value)) {
    return value;
  }
  let interpolatedValue = value;
  _.each(variables, function (variable) {
    const regexVarName = "\\$" + variable.name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    interpolatedValue = interpolatedValue.replace(new RegExp(regexVarName, 'g'), variable.value);
  });
  return interpolatedValue;
}

/**
 * Replaces the object's attributes with the values of the referenced variables.
 *
 * If a referenced variable contains multiple values or if there are multiple referenced variables
 * then we generate copies of the object with all of the possible permutations.
 *
 * See interpolate_spec.js for examples.
 *
 * @param object
 *    the object to interpolate
 * @param attributes
 *    a list of attributes on a given object that should be checked for variables
 * @param variables
 *    a list of variables of the form [{name: 'varname', value: ['value1', 'value2']}, ...]
 * @param callback
 *    an optional callback made with the object after variable substitution has been performed
 * @param containsVariable
 *    optionally override the function used to determine if a string contains a reference to the named variable
 * @param replace
 *    optionally override the function used to substitute the variable reference in a string with the variables's value
 * @returns an array of objects, if no substitutions were performed, the array will contain the original object
 */
export function interpolate(object: any, attributes: any[], variables: PerfVariables, callback: (value: any) => void = () => {}, containsVariable: (value: any|undefined, variableName: string) => boolean = defaultContainsVariable, replace: (value: any, variables: PerfVariables) => any = defaultReplace): any {
  // Add the index variable with a single value
  const variablesWithIndex = _.clone(variables);
  variablesWithIndex.push({name: 'index', value: [0]});

  // Collect the list of variables that are referenced by one or more of the keys
  const referencedVariables = [] as PerfVariables;
  _.each(variablesWithIndex, function (variable) {
    const isVariableReferenced = _.find(attributes, function (attribute) {
      return containsVariable(object[attribute], variable.name);
    });

    if (isVariableReferenced) {
      referencedVariables.push(variable);
    }
  });

  if (referencedVariables.length < 1) {
    // No variables are referenced, nothing to substitute
    callback(object);
    return [object];
  }

  // Generate all possible permutations of the referenced variable's values
  const productOfAllVariables = cartesianProductOfVariables(referencedVariables);

  // Perform the required variable substitution
  const objects = [] as any[];
  let index = 0;
  _.each(productOfAllVariables, function (rowOfReferencedVariables) {
    // Update the value of the index variable to reflect the index of the row
    _.each(rowOfReferencedVariables, function (variable) {
      if (variable.name === 'index') {
        variable.value = 'idx' + index;
        index += 1;
      }
    });

    let o = _.clone(object);
    _.each(attributes, function (attribute) {
      o[attribute] = replace(o[attribute], rowOfReferencedVariables);
    });

    callback(o);

    objects.push(o);
  });

  return objects;
}
