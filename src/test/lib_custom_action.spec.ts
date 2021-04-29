import { Model } from 'opennms';
import { CustomAction, getMatch } from '../lib/custom_action';

describe('CustomAction :: getMatch', () => {
  it('should find an exact match', () => {
    const match = getMatch('$foo', 'foo');
    expect(match).toBeDefined();
    // @ts-expect-error
    expect(match.token).toEqual('$foo');
    // @ts-expect-error
    expect(match.variable).toEqual('foo');
    // @ts-expect-error
    expect(match.index).toBeUndefined();
  });

  it('should find a substring match', () => {
    const match = getMatch('I like $foo bar!', 'foo');
    expect(match).toBeDefined();
    // @ts-expect-error
    expect(match.token).toEqual('$foo');
    // @ts-expect-error
    expect(match.variable).toEqual('foo');
    // @ts-expect-error
    expect(match.index).toBeUndefined();
  });

  it('should not find a substring match without a word boundary', () => {
    expect(getMatch('I like $foobar!', 'foo')).toBeNull();
  });

  it('should find a match with a numerical index inside brackets', () => {
    const match = getMatch('I like $foo[1]', 'foo');
    expect(match).toBeDefined();
    // @ts-expect-error
    expect(match.token).toEqual('$foo[1]');
    // @ts-expect-error
    expect(match.variable).toEqual('foo');
    // @ts-expect-error
    expect(match.index).toEqual(1);
  });

  it('should find a match with a string index inside brackets', () => {
    const match = getMatch('I like $foo[blah]', 'foo');
    expect(match).toBeDefined();
    // @ts-expect-error
    expect(match.token).toEqual('$foo[blah]');
    // @ts-expect-error
    expect(match.variable).toEqual('foo');
    // @ts-expect-error
    expect(match.index).toEqual('blah');
  });

  it('should find a match with empty brackets', () => {
    const match = getMatch('I like $foo[]', 'foo');
    expect(match).toBeDefined();
    // @ts-expect-error
    expect(match.token).toEqual('$foo[]');
    // @ts-expect-error
    expect(match.variable).toEqual('foo');
    // @ts-expect-error
    expect(match.index).toBeUndefined();
  });
});

describe('CustomAction', function() {
  /*
  let currentSelection;
  let mgr;
  */

  beforeEach(function() {
    /*
    currentSelection = [];
    mgr = new SelectionMgr(
      (from, to) => _.range(from, to + 1),
      () => {currentSelection = mgr.getSelectedRows();}
      );
    expect(mgr.getSelectedRows()).to.have.length(0);
    */
  });

  describe('constructor', function() {
    it('should fail if no label or url is specified', () => {
      // @ts-expect-error
      expect(() => { new CustomAction(); }).toThrow();
    });
    it('should fail if label is specified without url (2-arg)', () => {
      expect(() => { new CustomAction('foo'); }).toThrow();
    });
    it('should fail if label is specified without url (object)', () => {
      // @ts-expect-error
      expect(() => { new CustomAction({label:'foo'}); }).toThrow();
    });
    it('should fail if url is specified without label (2-arg)', () => {
      // @ts-expect-error
      expect(() => { new CustomAction(undefined, 'foo'); }).toThrow();
    });
    it('should fail if url is specified without label (object)', () => {
      // @ts-expect-error
      expect(() => { new CustomAction({url:'foo'}); }).toThrow();
    });
    it('should have readable url and label properties (2-arg)', () => {
      const ca = new CustomAction('foo', 'bar');
      expect(ca.label).toEqual('foo');
      expect(ca.url).toEqual('bar');
    });
    it('should have readable url and label properties (object)', () => {
      const ca = new CustomAction({label:'foo',url:'bar'});
      expect(ca.label).toEqual('foo');
      expect(ca.url).toEqual('bar');
    });
    it('should be read-only', () => {
      const ca = new CustomAction({label:'foo',url:'bar'});
      expect(() => {
        ca.label = 'baz';
      }).toThrow();
      expect(() => {
        ca.url = 'baz';
      }).toThrow();
    });
    it('should validate a url with no variables', () => {
      const ca = new CustomAction('foo', 'http://bar/');
      expect(ca.validate()).toBeTruthy();
    });
    it('should interpolate a url with no variables', () => {
      const ca = new CustomAction('foo', 'http://bar/');
      expect(ca.interpolate()).toEqual('http://bar/');
    });
    it('should validate a url with a nodeLabel variable', () => {
      const ca = new CustomAction('foo', 'http://bar/$nodeLabel');
      const alarm = new Model.OnmsAlarm();
      alarm.nodeId = 5;
      alarm.nodeLabel = 'theLabel';
      expect(ca.validate(alarm)).toBeTruthy();
    });
    it('should interpolate a url with a nodeLabel variable', () => {
      const ca = new CustomAction('foo', 'http://bar/$nodeLabel');
      const alarm = new Model.OnmsAlarm();
      alarm.nodeId = 5;
      alarm.nodeLabel = 'theLabel';
      expect(ca.interpolate(alarm)).toEqual('http://bar/theLabel');
    });
    it('should validate a variable that refers to an object', () => {
      const ca = new CustomAction('foo', 'http://bar/$severity');
      const alarm = new Model.OnmsAlarm();
      alarm.severity = Model.Severities.NORMAL;
      expect(ca.validate(alarm)).toBeTruthy();
    });
    it('should interpolate a variable that refers to an object', () => {
      const ca = new CustomAction('foo', 'http://bar/$severity');
      const alarm = new Model.OnmsAlarm();
      alarm.severity = Model.Severities.NORMAL;
      expect(ca.interpolate(alarm)).toEqual('http://bar/NORMAL');
    });
    it('should validate a variable with a number index', () => {
      const ca = new CustomAction('foo', 'http://bar/$parameters[0]');
      const alarm = new Model.OnmsAlarm();
      alarm.parameters = [new Model.OnmsParm('blah', 'string', 'yo')];
      expect(ca.validate(alarm)).toBeTruthy();
    });
    it('should interpolate a variable with a number index', () => {
      const ca = new CustomAction('foo', 'http://bar/$parameters[0]');
      const alarm = new Model.OnmsAlarm();
      alarm.parameters = [new Model.OnmsParm('blah', 'string', 'yo')];
      expect(ca.interpolate(alarm)).toEqual('http://bar/yo');
    });
    it('should validate a variable with a string index', () => {
      const ca = new CustomAction('foo', 'http://bar/$blah[monkey]');
      const obj = {
        blah: {
          monkey: 'see'
        }
      };
      expect(ca.validate(obj)).toBeTruthy();
    });
    it('should interpolate a variable with a string index', () => {
      const ca = new CustomAction('foo', 'http://bar/$blah[monkey]');
      const obj = {
        blah: {
          monkey: 'see'
        }
      };
      expect(ca.interpolate(obj)).toEqual('http://bar/see');
    });
    it('should validate a variable with a named event parm', () => {
      const ca = new CustomAction('foo', 'http://bar/$parameters[monkey]');
      const obj = new Model.OnmsAlarm();
      obj.parameters = [
        new Model.OnmsParm('monkey', 'Int32', '6')
      ];
      expect(ca.validate(obj)).toBeTruthy();
    });
    it('should interpolate a variable with a named event parm', () => {
      const ca = new CustomAction('foo', 'http://bar/$parameters[monkey]');
      const obj = new Model.OnmsAlarm();
      obj.parameters = [
        new Model.OnmsParm('monkey', 'Int32', '6')
      ];
      expect(ca.interpolate(obj)).toEqual('http://bar/6');
    });
  });

});
