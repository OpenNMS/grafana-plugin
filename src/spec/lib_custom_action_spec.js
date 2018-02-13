import _ from 'lodash';

import {Model} from '../opennms';
import {CustomAction, getMatch} from '../lib/custom_action';

describe('CustomAction :: getMatch', () => {
  it('should find an exact match', () => {
    const match = getMatch('$foo', 'foo');
    expect(match).to.be.defined;
    expect(match.token).to.equal('$foo');
    expect(match.variable).to.equal('foo');
    expect(match.index).to.be.undefined;
  });

  it('should find a substring match', () => {
    const match = getMatch('I like $foo bar!', 'foo');
    expect(match).to.be.defined;
    expect(match.token).to.equal('$foo');
    expect(match.variable).to.equal('foo');
    expect(match.index).to.be.undefined;
  });

  it('should not find a substring match without a word boundary', () => {
    expect(getMatch('I like $foobar!', 'foo')).to.be.null;
  });

  it('should find a match with a numerical index inside brackets', () => {
    const match = getMatch('I like $foo[1]', 'foo');
    expect(match).to.be.defined;
    expect(match.token).to.equal('$foo[1]');
    expect(match.variable).to.equal('foo');
    expect(match.index).to.equal(1);
  });

  it('should find a match with a string index inside brackets', () => {
    const match = getMatch('I like $foo[blah]', 'foo');
    expect(match).to.be.defined;
    expect(match.token).to.equal('$foo[blah]');
    expect(match.variable).to.equal('foo');
    expect(match.index).to.equal('blah');
  });

  it('should find a match with empty brackets', () => {
    const match = getMatch('I like $foo[]', 'foo');
    expect(match).to.be.defined;
    expect(match.token).to.equal('$foo[]');
    expect(match.variable).to.equal('foo');
    expect(match.index).to.be.undefined;
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
      expect(() => { new CustomAction(); }).to.throw();
    });
    it('should fail if label is specified without url (2-arg)', () => {
      expect(() => { new CustomAction('foo'); }).to.throw();
    });
    it('should fail if label is specified without url (object)', () => {
      expect(() => { new CustomAction({label:'foo'}); }).to.throw();
    });
    it('should fail if url is specified without label (2-arg)', () => {
      expect(() => { new CustomAction(undefined, 'foo'); }).to.throw();
    });
    it('should fail if url is specified without label (object)', () => {
      expect(() => { new CustomAction({url:'foo'}); }).to.throw();
    });
    it('should have readable url and label properties (2-arg)', () => {
      const ca = new CustomAction('foo', 'bar');
      expect(ca.label).to.equal('foo');
      expect(ca.url).to.equal('bar');
    });
    it('should have readable url and label properties (object)', () => {
      const ca = new CustomAction({label:'foo',url:'bar'});
      expect(ca.label).to.equal('foo');
      expect(ca.url).to.equal('bar');
    });
    it('should be read-only', () => {
      const ca = new CustomAction({label:'foo',url:'bar'});
      expect(() => {
        ca.label = 'baz';
      }).to.throw();
      expect(() => {
        ca.url = 'baz';
      }).to.throw();
    });
    it('should validate a url with no variables', () => {
      const ca = new CustomAction('foo', 'http://bar/');
      expect(ca.validate()).to.be.true;
    });
    it('should interpolate a url with no variables', () => {
      const ca = new CustomAction('foo', 'http://bar/');
      expect(ca.interpolate()).to.equal('http://bar/');
    });
    it('should validate a url with a nodeLabel variable', () => {
      const ca = new CustomAction('foo', 'http://bar/$nodeLabel');
      const alarm = new Model.OnmsAlarm();
      alarm.nodeId = 5;
      alarm.nodeLabel = 'theLabel';
      expect(ca.validate(alarm)).to.be.true;
    });
    it('should interpolate a url with a nodeLabel variable', () => {
      const ca = new CustomAction('foo', 'http://bar/$nodeLabel');
      const alarm = new Model.OnmsAlarm();
      alarm.nodeId = 5;
      alarm.nodeLabel = 'theLabel';
      expect(ca.interpolate(alarm)).to.equal('http://bar/theLabel');
    });
    it('should validate a variable that refers to an object', () => {
      const ca = new CustomAction('foo', 'http://bar/$severity');
      const alarm = new Model.OnmsAlarm();
      alarm.severity = Model.Severities.NORMAL;
      expect(ca.validate(alarm)).to.be.true;
    });
    it('should interpolate a variable that refers to an object', () => {
      const ca = new CustomAction('foo', 'http://bar/$severity');
      const alarm = new Model.OnmsAlarm();
      alarm.severity = Model.Severities.NORMAL;
      expect(ca.interpolate(alarm)).to.equal('http://bar/NORMAL');
    });
    it('should validate a variable with a number index', () => {
      const ca = new CustomAction('foo', 'http://bar/$parameters[0]');
      const alarm = new Model.OnmsAlarm();
      alarm.parameters = [new Model.OnmsParm('blah', 'string', 'yo')];
      expect(ca.validate(alarm)).to.be.true;
    });
    it('should interpolate a variable with a number index', () => {
      const ca = new CustomAction('foo', 'http://bar/$parameters[0]');
      const alarm = new Model.OnmsAlarm();
      alarm.parameters = [new Model.OnmsParm('blah', 'string', 'yo')];
      expect(ca.interpolate(alarm)).to.equal('http://bar/yo');
    });
    it('should validate a variable with a string index', () => {
      const ca = new CustomAction('foo', 'http://bar/$blah[monkey]');
      const obj = {
        blah: {
          monkey: 'see'
        }
      };
      expect(ca.validate(obj)).to.be.true;
    });
    it('should interpolate a variable with a string index', () => {
      const ca = new CustomAction('foo', 'http://bar/$blah[monkey]');
      const obj = {
        blah: {
          monkey: 'see'
        }
      };
      expect(ca.interpolate(obj)).to.equal('http://bar/see');
    });
    it('should validate a variable with a named event parm', () => {
      const ca = new CustomAction('foo', 'http://bar/$parameters[monkey]');
      const obj = new Model.OnmsAlarm();
      obj.parameters = [
        new Model.OnmsParm('monkey', 'Int32', '6')
      ];
      expect(ca.validate(obj)).to.be.true;
    });
    it('should interpolate a variable with a named event parm', () => {
      const ca = new CustomAction('foo', 'http://bar/$parameters[monkey]');
      const obj = new Model.OnmsAlarm();
      obj.parameters = [
        new Model.OnmsParm('monkey', 'Int32', '6')
      ];
      expect(ca.interpolate(obj)).to.equal('http://bar/6');
    });
  });

});
