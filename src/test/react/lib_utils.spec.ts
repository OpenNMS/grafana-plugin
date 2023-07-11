import { processSelectionVariables } from '../../lib/utils';

describe('Utils :: processSelectionVariables', () => {
  it('should return a proper string array for either dscp, applications, conversations and hosts responses', () => {
    let array = processSelectionVariables(["all"]);
    expect(array.length).toEqual(0);
    array = processSelectionVariables([]);
    expect(array.length).toEqual(0);

    array = processSelectionVariables(['{["Default",0,"0.0.0.0","0.0.0.0","app4"],["Default",0,"0.0.0.0","0.0.0.0","app8"],["Default",0,"0.0.0.0","0.0.0.1","app0"],["Default",0,"0.0.0.0","0.0.0.0","app2"]}']); 
    expect(array.length).toEqual(4);

    array = processSelectionVariables(["{app0,app1,app2}"]);
    expect(array.length).toEqual(3);

    array = processSelectionVariables(["app0,app1,app2"]);
    expect(array.length).toEqual(3);

    array = processSelectionVariables(['["Default",0,"0.0.0.0","0.0.0.0","app1"],["Default",0,"0.0.0.0","0.0.0.0","app2"],["Default",0,"0.0.0.0","0.0.0.1","app3"],["Default",0,"0.0.0.0","0.0.0.0","app4"]']);
    expect(array.length).toEqual(4);
  });
});
