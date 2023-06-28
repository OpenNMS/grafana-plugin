import { interpolate } from '../../datasources/perf-ds/queries/interpolate'

describe('OpenNMSPerformanceDatasource :: interpolate', () => {
  const query = { resource: '$node', metric: '$x.$y' }
  const queryWithBraces = { resource: '${node}', metric: '$x.$y' }
  const queryWithBracesAndFormat = { resource: '${node:csv}', metric: '$x.$y' }

  it('should return the same object when the list of attributes is empty', () => {
    expect(interpolate(query, [], [])).toStrictEqual([query])
  })

  it('should return the same object when the list of variables is empty', () => {
    expect(interpolate(query, ['resource'], [])).toStrictEqual([query])
  })

  it('should return the same object when no matching variables are referenced', () => {
    expect(interpolate(query, ['resource'], [{ name: '!node', value: ['1'] }])).toStrictEqual([query])
  })

  it('should be able to interpolate a single variable in a single attribute', () => {
    const interpolated = interpolate(query, ['resource'], [{ name: 'node', value: ['1', '2'] }])

    expect(interpolated).toStrictEqual([
      { resource: '1', metric: '$x.$y' },
      { resource: '2', metric: '$x.$y' }
    ])
  })

  it('should be able to interpolate multiple variables in a single attribute', () => {
    const interpolated = interpolate(query, ['metric'], [
      { name: 'x', value: ['x1', 'x2'] },
      { name: 'y', value: ['y1', 'y2'] }
    ])

    expect(interpolated).toStrictEqual([
      { resource: '$node', metric: 'x1.y1' },
      { resource: '$node', metric: 'x1.y2' },
      { resource: '$node', metric: 'x2.y1' },
      { resource: '$node', metric: 'x2.y2' }
    ])
  })

  it('should be able to interpolate multiple variables in multiple attributes', () => {
    const interpolated = interpolate(query, ['resource', 'metric'], [
      { name: 'node', value: ['1', '2'] },
      { name: 'x', value: ['x1', 'x2'] },
      { name: 'y', value: ['y1', 'y2'] }
    ])

    expect(interpolated).toStrictEqual([
      { resource: '1', metric: 'x1.y1' },
      { resource: '1', metric: 'x1.y2' },
      { resource: '1', metric: 'x2.y1' },
      { resource: '1', metric: 'x2.y2' },
      { resource: '2', metric: 'x1.y1' },
      { resource: '2', metric: 'x1.y2' },
      { resource: '2', metric: 'x2.y1' },
      { resource: '2', metric: 'x2.y2' }
    ])
  })

  it('should support interpolating a special variable named $index which is unique for every row', () => {
    const queryWithIndex = {'resource': 'node', 'metric': '$x.$y', 'label': '$index'}

    const interpolated = interpolate(queryWithIndex, ['resource', 'metric', 'label'], [
      { name: 'x', value: ['x1', 'x2'] },
      { name: 'y', value: ['y1', 'y2'] }
    ])

    expect(interpolated).toStrictEqual([
      { resource: 'node', metric: 'x1.y1', label: 'idx0' },
      { resource: 'node', metric: 'x1.y2', label: 'idx1' },
      { resource: 'node', metric: 'x2.y1', label: 'idx2' },
      { resource: 'node', metric: 'x2.y2', label: 'idx3' }
    ])
  })

  it('should be able to interpolate multiple variables with the same name in a single attribute', () => {
    const queryWithMultipleVariables = {'resource': '$node', 'metric': '$x-var + $x-var'}
    const interpolated = interpolate(queryWithMultipleVariables, ['resource', 'metric'], [
      { name: 'x-var', value: ['x1', 'x2'] }
    ])

    expect(interpolated).toStrictEqual([
      { resource: '$node', metric: 'x1 + x1' },
      { resource: '$node', metric: 'x2 + x2' }
    ])
  })

  it('should be able to interpolate a single variable-with-braces in a single attribute', () => {
    const interpolated = interpolate(queryWithBraces, ['resource'], [{ name: 'node', value: ['1', '2'] }])

    expect(interpolated).toStrictEqual([
      { resource: '1', metric: '$x.$y' },
      { resource: '2', metric: '$x.$y' }
    ])
  })

  it('should be able to interpolate a single variable-with-braces-and-format in a single attribute', () => {
    const interpolated = interpolate(queryWithBracesAndFormat, ['resource'], [{ name: 'node', value: ['1', '2'] }])

    expect(interpolated).toStrictEqual([
      { resource: '1', metric: '$x.$y' },
      { resource: '2', metric: '$x.$y' }
    ])
  })
})
