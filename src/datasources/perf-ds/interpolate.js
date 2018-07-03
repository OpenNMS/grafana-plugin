import _ from 'lodash';

function cartesianProductOfArrays(arrays) {
  // Based on the code from http://stackoverflow.com/questions/15298912/
  // javascript-generating-combinations-from-n-arrays-with-m-elements
  var r = [], max = arrays.length - 1;

  function helper(arr, i) {
    for (var j = 0, l = arrays[i].length; j < l; j++) {
      var a = arr.slice(0); // clone arr
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

function cartesianProductOfVariables(variables) {
  // Collect the values from all of the variables
  var allValues = [];
  _.each(variables, function (variable) {
    allValues.push(variable.value);
  });

  // Generate the cartesian product
  var productOfAllValues = cartesianProductOfArrays(allValues);

  // Rebuild the variables
  var productOfAllVariables = [];
  _.each(productOfAllValues, function (rowOfValues) {
    var rowOfVariables = [];
    for (var i = 0, l = variables.length; i < l; i++) {
      // Deep clone
      var variable = JSON.parse(JSON.stringify(variables[i]));
      variable.value = rowOfValues[i];
      rowOfVariables.push(variable);
    }
    productOfAllVariables.push(rowOfVariables);
  });

  return productOfAllVariables;
}

function defaultContainsVariable(value, variableName) {
  if (_.isNull(value) || _.isEmpty(value)) {
    return false;
  }
  return value.indexOf("$" + variableName) >= 0;
}

function defaultReplace(value, variables) {
  if (_.isNull(value) || _.isEmpty(value)) {
    return value;
  }
  var interpolatedValue = value;
  _.each(variables, function (variable) {
    var regexVarName = "\\$" + variable.name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    interpolatedValue = interpolatedValue.replace(new RegExp(regexVarName, "g"), variable.value);
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
export function interpolate(object, attributes, variables, callback, containsVariable, replace) {
  // Use default for the functions when undefined
  if (callback === undefined) {
    callback = () => {};
  }
  if (containsVariable === undefined) {
    containsVariable = defaultContainsVariable;
  }
  if (replace === undefined) {
    replace = defaultReplace;
  }

  // Add the index variable with a single value
  var variablesWithIndex = _.clone(variables);
  variablesWithIndex.push({name: 'index', value: [0]});

  // Collect the list of variables that are referenced by one or more of the keys
  var referencedVariables = [];
  _.each(variablesWithIndex, function (variable) {
    var isVariableReferenced = _.find(attributes, function (attribute) {
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
  var productOfAllVariables = cartesianProductOfVariables(referencedVariables);

  // Perform the required variable substitution
  var objects = [];
  var index = 0;
  _.each(productOfAllVariables, function (rowOfReferencedVariables) {
    // Update the value of the index variable to reflect the index of the row
    _.each(rowOfReferencedVariables, function (variable) {
      if (variable.name === 'index') {
        variable.value = 'idx' + index;
        index += 1;
      }
    });

    var o = _.clone(object);
    _.each(attributes, function (attribute) {
      o[attribute] = replace(o[attribute], rowOfReferencedVariables);
    });

    callback(o);

    objects.push(o);
  });

  return objects;
}
