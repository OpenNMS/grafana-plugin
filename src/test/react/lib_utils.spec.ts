import {
  getNumberOrDefault,
  isInteger,
  isUndefined,
  processSelectionVariables
} from '../../lib/utils'

describe('Utils :: processSelectionVariables', () => {
  it('should return a proper string array for either dscp, applications, conversations and hosts responses', () => {
    let array = processSelectionVariables(["all"])
    expect(array.length).toEqual(0)
    array = processSelectionVariables([])
    expect(array.length).toEqual(0)

    array = processSelectionVariables(['{["Default",0,"0.0.0.0","0.0.0.0","app4"],["Default",0,"0.0.0.0","0.0.0.0","app8"],["Default",0,"0.0.0.0","0.0.0.1","app0"],["Default",0,"0.0.0.0","0.0.0.0","app2"]}']) 
    expect(array.length).toEqual(4)

    array = processSelectionVariables(["{app0,app1,app2}"])
    expect(array.length).toEqual(3)

    array = processSelectionVariables(["app0,app1,app2"])
    expect(array.length).toEqual(3)

    array = processSelectionVariables(['["Default",0,"0.0.0.0","0.0.0.0","app1"],["Default",0,"0.0.0.0","0.0.0.0","app2"],["Default",0,"0.0.0.0","0.0.0.1","app3"],["Default",0,"0.0.0.0","0.0.0.0","app4"]'])
    expect(array.length).toEqual(4)
  })
})

describe('Utils :: getNumberOrDefault', () => {
  it('should return a number coerced to an integer or else a default value if value is not a number', () => {

    expect(getNumberOrDefault(1, 99)).toBe(1)
    expect(getNumberOrDefault(10, 99)).toBe(10)
    expect(getNumberOrDefault(123, 99)).toBe(123)
    expect(getNumberOrDefault(1000000, 99)).toBe(1000000)
    expect(getNumberOrDefault(-1, 99)).toBe(-1)

    expect(getNumberOrDefault(undefined, 99)).toBe(99)
    expect(getNumberOrDefault(null, 99)).toBe(99)
    expect(getNumberOrDefault(1.123, 99)).toBe(1)
    expect(getNumberOrDefault('a', 99)).toBe(99)
  })
})

describe('Utils :: isInteger', () => {
  it('should return a whether item is an integer or not', () => {

    expect(isInteger(1)).toBeTruthy()
    expect(isInteger(10)).toBeTruthy()
    expect(isInteger(123)).toBeTruthy()
    expect(isInteger(1000000)).toBeTruthy()
    expect(isInteger(-1)).toBeTruthy()

    expect(isInteger(undefined)).toBeFalsy()
    expect(isInteger(null)).toBeFalsy()
    expect(isInteger(1.123)).toBeFalsy()
    expect(isInteger('a')).toBeFalsy()
  })
})

describe('Utils :: isUndefined', () => {
  it('should return a whether item is undefined or not', () => {

    expect(isUndefined(undefined)).toBeTruthy()

    expect(isUndefined(null)).toBeFalsy()
    expect(isUndefined('')).toBeFalsy()
    expect(isUndefined(1)).toBeFalsy()
    expect(isUndefined('abc')).toBeFalsy()
    expect(isUndefined(1.123)).toBeFalsy()
    expect(isUndefined({})).toBeFalsy()

    const item: any = {
      a: 1,
      b: 'text',
      c: null
    }

    expect(isUndefined(item)).toBeFalsy()
    expect(isUndefined(item.a)).toBeFalsy()
    expect(isUndefined(item.b)).toBeFalsy()
    expect(isUndefined(item.c)).toBeFalsy()
    expect(isUndefined(item.d)).toBeTruthy()
  })
})
